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
                     "utc_start_time", "round_number")
  missing <- setdiff(required_cols, names(pbp_data))
  if (length(missing) > 0) {
    cli::cli_abort("Missing required columns: {.val {missing}}")
  }

  dt <- data.table::setDT(data.table::copy(pbp_data))

  # Filter to rows with a valid player and non-NA wpa

  dt <- dt[!is.na(player_id) & !is.na(wpa)]

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
    round          = as.numeric(sprintf("%02d", round_number[1L])),
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
