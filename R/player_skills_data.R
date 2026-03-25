# Player Stat Rating Data Preparation
# ====================================
# Position mapping, data joining/cleaning, and denominator computation
# for the stat rating estimation pipeline.
#
# Rate stats use Gamma-Poisson conjugate model (counts per game).
# Efficiency stats use Beta-Binomial conjugate model (proportions).
#
# Output columns use `_rating` suffix (e.g. `goals_rating`).
# These are individual stat-level predictions, distinct from composite
# ratings like PSR/EPR.


# ============================================================================
# Position helpers
# ============================================================================

#' Map AFL positions to simplified position groups
#'
#' @param pos Character vector of AFL position strings.
#' @return Character vector of position groups (DEF/MID/FWD/RUCK), NA for unknown.
#' @keywords internal
.map_position_group <- function(pos) {
  pm <- stat_rating_position_map()
  # Build reverse lookup: position -> group
  lookup <- character(0)
  for (grp in names(pm)) {
    for (p in pm[[grp]]) {
      lookup[p] <- grp
    }
  }
  unname(lookup[pos])
}


#' Resolve position groups, filling NAs with each player's modal position
#'
#' @param dt A data.table with player_id and position columns.
#' @return The data.table with added `pos_group` column.
#' @keywords internal
.resolve_stat_rating_positions <- function(dt) {
  # Use 'listed_position' column (listed position: KEY_DEFENDER, MIDFIELDER, etc.)
  # Fallback to 'position' if 'listed_position' not available
  pos_col <- if ("listed_position" %in% names(dt)) "listed_position" else "position"
  dt[, pos_group := .map_position_group(get(pos_col))]

  na_idx <- which(is.na(dt$pos_group))
  if (length(na_idx) > 0) {
    # Compute modal position per player from non-NA rows
    valid <- dt[!is.na(pos_group)]
    if (nrow(valid) > 0) {
      modal_pos <- valid[, {
        tt <- table(pos_group)
        list(modal_pos = if (length(tt) > 0) names(tt)[which.max(tt)] else NA_character_)
      }, by = player_id]

      dt[modal_pos, on = "player_id", modal_pos := i.modal_pos]
      dt[is.na(pos_group) & !is.na(modal_pos), pos_group := modal_pos]
      dt[, modal_pos := NULL]
    }
  }

  dt
}


# ============================================================================
# Data preparation
# ============================================================================

#' Prepare data for stat rating estimation
#'
#' Joins player game data with player stats to produce a single table with
#' all required columns for the stat rating estimation pipeline.
#'
#' @param player_game_data Player game data from \code{load_player_game_data(TRUE)}.
#' @param player_stats Player stats from \code{load_player_stats(TRUE)}.
#' @param rosters Optional roster data. If NULL, loads from torpdata.
#' @param fixtures Optional fixture data. If NULL, loads from torpdata.
#'
#' @return A data.table with one row per player-match containing:
#'   identifiers (player_id, match_id, player_name, season, round, team),
#'   match_date_rating (Date), tog (time on ground as fraction), position,
#'   and all stat columns referenced by \code{stat_rating_definitions()}.
#'
#' @importFrom data.table as.data.table
#' @keywords internal
.prepare_stat_rating_data <- function(player_game_data, player_stats, rosters = NULL,
                               fixtures = NULL) {
  pgd <- data.table::as.data.table(player_game_data)
  ps <- data.table::as.data.table(player_stats)

  # Compute match_date from utc_start_time
  pgd[, match_date_rating := as.Date(utc_start_time)]

  # Compute TOG as fraction and constant denominator for Beta-Binomial model
  pgd[, tog := time_on_ground_percentage / 100]
  pgd[, tog_denominator := 1]
  pgd[, played := 1L]

  # player_stats columns are normalised by load_player_stats()
  if (!all(c("player_id", "match_id") %in% names(ps))) {
    cli::cli_warn(c(
      "Cannot find player_id/match_id columns in player_stats.",
      "i" = "Some skills will rely entirely on the prior."
    ))
    pgd[, disposal_efficiency_pct_x_disposals := NA_real_]
  } else {
    pid_col <- "player_id"
    mid_col <- "match_id"

    # Find all stat columns needed by skill definitions but missing from pgd
    stat_defs_merge <- stat_rating_definitions()
    needed_cols <- unique(c(
      stats::na.omit(stat_defs_merge$source_col),
      stats::na.omit(stat_defs_merge$success_col),
      stats::na.omit(stat_defs_merge$attempts_col)
    ))
    needed_cols <- unique(unlist(strsplit(needed_cols, "\\+")))
    missing_from_pgd <- setdiff(needed_cols, names(pgd))
    # Also always bring in disposal_efficiency for the derived column
    missing_from_pgd <- unique(c(missing_from_pgd, "disposal_efficiency"))
    available_in_ps <- intersect(missing_from_pgd, names(ps))

    if (length(available_in_ps) > 0) {
      ps_slim <- ps[, c(pid_col, mid_col, available_in_ps), with = FALSE]
      data.table::setnames(ps_slim, c(pid_col, mid_col), c("player_id", "match_id"), skip_absent = TRUE)
      ps_slim <- unique(ps_slim, by = c("player_id", "match_id"))

      n_before <- nrow(pgd)
      pgd <- merge(pgd, ps_slim, by = c("player_id", "match_id"), all.x = TRUE, suffixes = c("", "_ps"))
      if (nrow(pgd) != n_before) {
        cli::cli_warn("Merge with player_stats changed row count from {n_before} to {nrow(pgd)}")
      }

      # Handle suffixed columns from name conflicts
      for (col in available_in_ps) {
        ps_col <- paste0(col, "_ps")
        if (ps_col %in% names(pgd)) {
          if (!col %in% names(pgd)) {
            data.table::setnames(pgd, ps_col, col)
          } else {
            pgd[, (ps_col) := NULL]
          }
        }
      }
    }

    # Compute derived disposal_efficiency column for Beta-Binomial model
    if ("disposal_efficiency" %in% names(pgd)) {
      pgd[, disposal_efficiency_pct_x_disposals :=
        data.table::fifelse(is.na(disposal_efficiency), 0, as.numeric(disposal_efficiency)) / 100 * disposals]
    } else {
      cli::cli_warn("disposal_efficiency column not found; skill will rely on prior")
      pgd[, disposal_efficiency_pct_x_disposals := NA_real_]
    }
  }

  # Map positions to groups
  pgd <- .resolve_stat_rating_positions(pgd)

  # Select essential columns + all stat source columns
  stat_defs <- stat_rating_definitions()
  rate_cols <- stats::na.omit(unique(stat_defs$source_col))
  # For efficiency stats, parse columns from success_col and attempts_col
  eff_cols <- unique(c(
    stats::na.omit(stat_defs$success_col),
    stats::na.omit(stat_defs$attempts_col)
  ))
  # Handle "col1+col2" specs
  eff_cols <- unlist(strsplit(eff_cols, "\\+"))

  all_stat_cols <- unique(c(rate_cols, eff_cols))
  keep_cols <- unique(c(
    "player_id", "match_id", "player_name", "season", "round",
    "match_date_rating", "tog", "pos_group", "position",
    intersect(all_stat_cols, names(pgd)),
    "disposal_efficiency_pct_x_disposals"
  ))

  # Only keep columns that exist
  keep_cols <- intersect(keep_cols, names(pgd))
  out <- pgd[, ..keep_cols]

  # Add team column if available
  if ("team" %in% names(pgd) && !"team" %in% keep_cols) {
    out[, team := pgd$team]
  }

  # Expand with zero-TOG rows for rostered players who didn't play.
  # This lets the Beta-Binomial TOG model account for selection probability:
  # missed rounds contribute (tog=0, tog_denominator=1) to the denominator,
  # pulling estimates down for players who miss games.
  # Other stats are unaffected: rate stats use tog as exposure (0 * w = 0),
  # efficiency stats have 0 successes and 0 attempts (0 * w = 0).
  out[, avail_only := FALSE]

  round_cal <- out[, .(match_date_rating = min(match_date_rating)),
                   by = .(season, round)]

  if (!is.null(rosters)) {
    # Roster-based expansion: every rostered player × every round their team played
    roster_dt <- data.table::as.data.table(rosters)

    if (!is.null(fixtures)) {
      # Build team-round calendar from fixtures (handles byes + finals correctly)
      fix_dt <- data.table::as.data.table(fixtures)
      home <- fix_dt[, .(season, round = round_number,
                         team = home_team_name)]
      away <- fix_dt[, .(season, round = round_number,
                         team = away_team_name)]
      team_rounds <- unique(data.table::rbindlist(list(home, away)))
      team_rounds <- merge(team_rounds, round_cal, by = c("season", "round"))

      # Join roster players (with team) to team-round calendar
      roster_players <- unique(roster_dt[, .(player_id, season, team)])
      all_combos <- merge(roster_players, team_rounds,
                          by = c("season", "team"), allow.cartesian = TRUE)
      all_combos[, team := NULL]
    } else {
      # Fallback without fixtures: every round in their season (old behavior)
      roster_players <- unique(roster_dt[, .(player_id, season)])
      all_combos <- merge(roster_players, round_cal, by = "season",
                          allow.cartesian = TRUE)
    }
  } else {
    # Fallback: expand from each player's first game to the latest round
    player_first <- out[, .(first_season = min(season),
                            first_round = min(round[season == min(season)])),
                        by = player_id]
    all_combos <- data.table::CJ(player_id = player_first$player_id,
                                  round_idx = seq_len(nrow(round_cal)))
    all_combos[, c("season", "round", "match_date_rating") :=
                 round_cal[round_idx, .(season, round, match_date_rating)]]
    all_combos[, round_idx := NULL]
    all_combos <- merge(all_combos, player_first, by = "player_id")
    all_combos <- all_combos[season > first_season |
                             (season == first_season & round >= first_round)]
    all_combos[, c("first_season", "first_round") := NULL]
  }

  played <- unique(out[, .(player_id, season, round)])
  played[, .played := TRUE]
  all_combos <- merge(all_combos, played,
                      by = c("player_id", "season", "round"), all.x = TRUE)
  missed <- all_combos[is.na(.played)]

  if (nrow(missed) > 0) {
    zero_rows <- missed[, .(
      player_id, season, round, match_date_rating,
      match_id = paste0("AVAIL_", season, "_", sprintf("%02d", round)),
      tog = 0, tog_denominator = 1, played = 0L, avail_only = TRUE
    )]

    # Assign pos_group: use modal position from games if available,
    # otherwise use roster position for never-played players
    player_pos <- out[avail_only == FALSE & !is.na(pos_group),
                      .(pos_group = names(which.max(table(pos_group)))), by = player_id]
    if (!is.null(rosters)) {
      roster_pos <- unique(roster_dt[, .(player_id, position)])
      roster_pos[, roster_pos_group := .map_position_group(position)]
      roster_pos <- roster_pos[!is.na(roster_pos_group), .(player_id, roster_pos_group)]
      roster_pos <- unique(roster_pos, by = "player_id")
      # Merge: prefer game-based pos, fallback to roster pos
      player_pos <- merge(roster_pos, player_pos, by = "player_id", all.x = TRUE)
      player_pos[is.na(pos_group), pos_group := roster_pos_group]
      player_pos[, roster_pos_group := NULL]
    }
    zero_rows <- merge(zero_rows, player_pos, by = "player_id", all.x = TRUE)

    out <- data.table::rbindlist(list(out, zero_rows), fill = TRUE)
  }

  out
}


# ============================================================================
# Denominator helper for efficiency stats
# ============================================================================

#' Compute denominator vector from a column spec string
#'
#' Supports "col1+col2" for summing multiple columns.
#'
#' @param dt A data.table.
#' @param denom_spec A string like "col" or "col1+col2".
#' @return Numeric vector of denominators.
#' @keywords internal
.compute_stat_rating_denominator <- function(dt, denom_spec) {
  if (grepl("\\+", denom_spec)) {
    parts <- strsplit(denom_spec, "\\+")[[1]]
    result <- rep(0, nrow(dt))
    for (p in parts) {
      if (p %in% names(dt)) {
        v <- as.numeric(dt[[p]])
        v[is.na(v)] <- 0
        result <- result + v
      }
    }
    return(result)
  }
  if (denom_spec %in% names(dt)) {
    v <- as.numeric(dt[[denom_spec]])
    v[is.na(v)] <- 0
    return(v)
  }
  rep(0, nrow(dt))
}


# Backward compatibility aliases
#' @rdname .prepare_stat_rating_data
#' @keywords internal
.prepare_skill_data <- .prepare_stat_rating_data

#' @rdname .resolve_stat_rating_positions
#' @keywords internal
.resolve_skill_positions <- .resolve_stat_rating_positions

#' @rdname .compute_stat_rating_denominator
#' @keywords internal
.compute_skill_denominator <- .compute_stat_rating_denominator
