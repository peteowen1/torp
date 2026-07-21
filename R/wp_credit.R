#' Create Win Probability Credit Data
#'
#' Splits Win Probability Added (WPA) between disposers and receivers for each
#' play, then aggregates per player per game. Also tracks each player's highest
#' impact play by absolute WPA.
#'
#' @param pbp_data Play-by-play data with WPA already computed (via
#'   \code{add_wp_vars()}). Must contain columns: \code{wpa}, \code{player_id},
#'   \code{player_name}, \code{lead_player_id}, \code{pos_team},
#'   \code{display_order}, \code{match_id}, \code{team}, \code{season},
#'   \code{round_number}, \code{utc_start_time}. If NULL, loads and prepares
#'   data via \code{load_pbp()}.
#' @param disp_share Fraction of WPA credited to the disposer (0-1). Default is
#'   \code{WP_CREDIT_DISP_SHARE} (0.5). The receiver gets \code{1 - disp_share}.
#'
#' @return A data.table with one row per player per match, containing:
#'   \describe{
#'     \item{player_id, player_name, match_id, team, season, round, utc_start_time}{Identifiers}
#'     \item{wp_credit}{Total WPA credit (disposal + reception)}
#'     \item{wp_disp_credit}{WPA credit from disposals only}
#'     \item{wp_recv_credit}{WPA credit from receptions only}
#'     \item{n_disposals}{Number of disposals}
#'     \item{n_receptions}{Number of receptions}
#'     \item{max_play_wpa}{WPA of highest-impact play (by absolute value)}
#'     \item{max_play_display_order}{display_order of that play (joinable to PBP)}
#'     \item{max_play_role}{"disposer" or "receiver"}
#'   }
#'
#' @export
#' @importFrom data.table data.table setDT setnames setkey fcoalesce
#' @importFrom data.table fifelse rbindlist
create_wp_credit <- function(pbp_data = NULL,
                             disp_share = WP_CREDIT_DISP_SHARE) {

  if (is.null(pbp_data)) {
    pbp_data <- load_pbp(TRUE)
  }

  required_cols <- c("wpa", "player_id", "player_name", "lead_player_id",
                     "pos_team", "display_order", "match_id", "team",
                     "utc_start_time", "round_number", "description")
  missing <- setdiff(required_cols, names(pbp_data))
  if (length(missing) > 0) {
    cli::cli_abort("Missing required columns: {.val {missing}}")
  }

  dt <- data.table::setDT(data.table::copy(pbp_data))

  # Defensive: descriptive scoring rows ("Goal" / "Behind" / "Rushed") would
  # over-credit the scorer if they reached this aggregation, but the standard
  # pipeline strips them upstream in clean_model_data_epv() via the
  # EPV_RELEVANT_DESCRIPTIONS whitelist. This guard exists for direct callers
  # that bypass that step. See project_wpa_reconciliation_b4 in memory for the
  # full investigation -- the chain-sum vs parquet WPA gap is a WP-model
  # divergence (worker's chain-aware features produce larger per-row deltas),
  # not a filter or attribution issue.
  dt <- dt[!is.na(player_id) & !is.na(wpa) &
           !(description %in% c("Goal", "Behind", "Rushed"))]

  # Determine if receiver exists and is different from disposer
  # No receiver when: lead_player_id is NA, equals player_id, or is a shot/OOB
  dt[, has_receiver := !is.na(lead_player_id) & lead_player_id != player_id]

  # --- Disposal credit ---
  # Disposer gets disp_share * wpa, or 100% when no receiver
  dt[, disp_wpa := data.table::fifelse(has_receiver, disp_share * wpa, wpa)]

  # --- Reception credit ---
  # Receiver gets (1 - disp_share) * wpa * pos_team (sign flip for turnovers)
  dt[, recv_wpa := data.table::fifelse(has_receiver, (1 - disp_share) * wpa * pos_team, 0)]

  # --- Aggregate disposals per player-game ---
  disp_agg <- dt[, .(
    wp_disp_credit = sum(disp_wpa, na.rm = TRUE),
    n_disposals    = .N,
    player_name    = player_name[1L],
    team           = team[1L],
    season         = as.integer(format(as.Date(utc_start_time[1L]), "%Y")),
    round          = round_number[1L],
    utc_start_time = utc_start_time[1L],
    # Peak disposal play
    disp_peak_wpa  = disp_wpa[which.max(abs(disp_wpa))],
    disp_peak_do   = display_order[which.max(abs(disp_wpa))]
  ), by = .(player_id, match_id)]

  # --- Aggregate receptions per player-game ---
  recv_dt <- dt[has_receiver == TRUE]
  recv_agg <- recv_dt[, .(
    wp_recv_credit = sum(recv_wpa, na.rm = TRUE),
    n_receptions   = .N,
    # Peak reception play
    recv_peak_wpa  = recv_wpa[which.max(abs(recv_wpa))],
    recv_peak_do   = display_order[which.max(abs(recv_wpa))]
  ), by = .(lead_player_id, match_id)]

  # --- Join disposal + reception ---
  result <- merge(
    disp_agg, recv_agg,
    by.x = c("player_id", "match_id"),
    by.y = c("lead_player_id", "match_id"),
    all.x = TRUE
  )

  # Fill NA receptions with 0
  result[, wp_recv_credit := data.table::fcoalesce(wp_recv_credit, 0)]
  result[, n_receptions := data.table::fcoalesce(n_receptions, 0L)]

  # Total credit
  result[, wp_credit := wp_disp_credit + wp_recv_credit]

  # --- Biggest impact play (compare disp peak vs recv peak) ---
  result[, recv_peak_wpa := data.table::fcoalesce(recv_peak_wpa, 0)]
  result[, recv_peak_do := data.table::fcoalesce(recv_peak_do, 0L)]

  result[, c("max_play_wpa", "max_play_display_order", "max_play_role") := {
    disp_abs <- abs(disp_peak_wpa)
    recv_abs <- abs(recv_peak_wpa)
    is_disp <- disp_abs >= recv_abs
    list(
      data.table::fifelse(is_disp, disp_peak_wpa, recv_peak_wpa),
      data.table::fifelse(is_disp, disp_peak_do, recv_peak_do),
      data.table::fifelse(is_disp, "disposer", "receiver")
    )
  }]

  # Drop working columns
  result[, c("disp_peak_wpa", "disp_peak_do", "recv_peak_wpa", "recv_peak_do") := NULL]

  # Final column order
  data.table::setcolorder(result, c(
    "player_id", "player_name", "match_id", "team", "season", "round",
    "utc_start_time", "wp_credit", "wp_disp_credit", "wp_recv_credit",
    "n_disposals", "n_receptions",
    "max_play_wpa", "max_play_display_order", "max_play_role"
  ))

  data.table::setkey(result, match_id, player_id)
  return(result)
}

#' Attach Per-Row Win Probability Credit Split
#'
#' Companion to \code{create_wp_credit()}: instead of aggregating the 50/50
#' disposer/receiver WPA split to per-player totals, this attaches the split
#' as two new columns (\code{wpa_disp}, \code{wpa_recv}) on the original
#' per-row play-by-play data. Written for the AFL chain parquet (blog
#' per-quarter WPA), so downstream consumers can aggregate
#' \code{sum(wpa_disp) by player_id} + \code{sum(wpa_recv) by lead_player_id}
#' over any row subset (e.g. filtered to a single quarter) and get the exact
#' same numbers \code{create_wp_credit()} would produce over the full match.
#'
#' \code{wpa_recv} is emitted on the disposer's row (not duplicated onto a
#' separate receiver row); the receiving player's identity is already on
#' that row via \code{lead_player_id}, mirroring \code{create_wp_credit()}'s
#' join structure (\code{disp_agg} keyed by \code{player_id}, \code{recv_agg}
#' keyed by \code{lead_player_id}).
#'
#' @param pbp_data Play-by-play data with WPA already computed (via
#'   \code{add_wp_vars()}). Must contain columns: \code{wpa}, \code{player_id},
#'   \code{lead_player_id}, \code{pos_team}, \code{description}.
#' @param disp_share Fraction of WPA credited to the disposer (0-1). Default is
#'   \code{WP_CREDIT_DISP_SHARE} (0.5). The receiver gets \code{1 - disp_share}.
#'
#' @return \code{pbp_data} (as a data.table) with two additional columns:
#'   \describe{
#'     \item{wpa_disp}{Disposer's share of \code{wpa} for this row}
#'     \item{wpa_recv}{Receiver's share of \code{wpa} for this row (sign-flipped
#'       via \code{pos_team} on turnovers), or \code{0} when there is no
#'       receiver. \code{NA} on rows excluded from crediting (see below).}
#'   }
#'   Rows with \code{NA} \code{player_id}/\code{wpa}, or a descriptive scoring
#'   \code{description} (\code{"Goal"}/\code{"Behind"}/\code{"Rushed"}), get
#'   \code{NA} for both columns -- the same defensive exclusion
#'   \code{create_wp_credit()} applies before aggregating, so
#'   \code{sum(..., na.rm = TRUE)} over the per-row columns reproduces
#'   \code{create_wp_credit()}'s totals exactly.
#'
#' @export
#' @importFrom data.table setDT copy fifelse
attach_per_row_wpa_split <- function(pbp_data,
                                      disp_share = WP_CREDIT_DISP_SHARE) {

  required_cols <- c("wpa", "player_id", "lead_player_id", "pos_team", "description")
  missing <- setdiff(required_cols, names(pbp_data))
  if (length(missing) > 0) {
    cli::cli_abort("Missing required columns: {.val {missing}}")
  }

  dt <- data.table::setDT(data.table::copy(pbp_data))

  # Same defensive exclusion as create_wp_credit(): rows with NA player_id/wpa,
  # or descriptive scoring rows, don't earn disposer/receiver credit. Mirrored
  # here (rather than dropped) so the per-row split can be exported alongside
  # every chain row while still reproducing create_wp_credit()'s per-player
  # totals via sum(..., na.rm = TRUE).
  excluded <- is.na(dt$player_id) | is.na(dt$wpa) |
    dt$description %in% c("Goal", "Behind", "Rushed")

  dt[, has_receiver := !is.na(lead_player_id) & lead_player_id != player_id]

  dt[, wpa_disp := data.table::fifelse(has_receiver, disp_share * wpa, wpa)]
  dt[, wpa_recv := data.table::fifelse(has_receiver, (1 - disp_share) * wpa * pos_team, 0)]

  dt[excluded, wpa_disp := NA_real_]
  dt[excluded, wpa_recv := NA_real_]

  dt[, has_receiver := NULL]

  return(dt[])
}
