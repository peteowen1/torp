# AFL Injury Data: Schedule & Save
# ================================
# Build per-team return schedules (torp boost by return round) for the
# season simulation, and save the combined injury list to torpdata releases.


#' Build Injury Return Schedule for Simulation
#'
#' Takes an injury data frame (with `return_round` already parsed) and
#' player-level TORP ratings, and computes per-team TORP boosts at each
#' round when injured players return.
#'
#' @param injuries_df A data.frame with at least `player_norm` and
#'   `return_round` columns. Typically from [get_all_injuries()] after
#'   calling [parse_return_round()].
#' @param player_ratings_dt A data.table of player ratings with `player_name`,
#'   `team`, `torp`, and optionally `pred_tog` columns.
#' @return A data.table with columns `team`, `torp_boost`, `return_round`.
#'   One row per (team, return_round) combination.
#' @export
build_injury_schedule <- function(injuries_df, player_ratings_dt) {
  if (is.null(injuries_df) || nrow(injuries_df) == 0) {
    return(data.table::data.table(
      team = character(), torp_boost = numeric(), return_round = numeric()
    ))
  }

  inj_dt <- data.table::as.data.table(injuries_df)
  pr_dt <- data.table::as.data.table(player_ratings_dt)

  # Only players who will actually return this season (finite return_round)
  inj_dt <- inj_dt[is.finite(return_round) & !is.na(return_round)]
  if (nrow(inj_dt) == 0) {
    return(data.table::data.table(
      team = character(), torp_boost = numeric(), return_round = numeric()
    ))
  }

  # Normalise player and team names for joining
  if (!"player_norm" %in% names(inj_dt)) {
    inj_dt[, player_norm := norm_name(player)]
  }
  pr_dt[, player_norm := norm_name(player_name)]
  if ("team" %in% names(inj_dt)) inj_dt[, team := torp_replace_teams(team)]
  pr_dt[, team := torp_replace_teams(team)]

  # Resolve the rating column name (torp or epr depending on data source)
  torp_col <- if ("torp" %in% names(pr_dt)) "torp" else if ("epr" %in% names(pr_dt)) "epr" else NULL
  if (is.null(torp_col)) {
    cli::cli_warn("Player ratings have no {.field torp} or {.field epr} column")
    return(data.table::data.table(
      team = character(), torp_boost = numeric(), return_round = numeric()
    ))
  }
  if (torp_col != "torp") data.table::setnames(pr_dt, torp_col, "torp")

  # Join injured players to their ratings
  # Use (player_norm, team) when available to distinguish same-name players
  pr_cols <- c("player_norm", "team", "torp")
  if ("pred_tog" %in% names(pr_dt)) pr_cols <- c(pr_cols, "pred_tog")
  inj_merge_cols <- c("player_norm", "player", "return_round")
  inj_merge_cols <- intersect(inj_merge_cols, names(inj_dt))

  has_team <- "team" %in% names(inj_dt) && !all(is.na(inj_dt$team))
  if (has_team) {
    inj_merge_cols <- union(inj_merge_cols, "team")
    join_by <- c("player_norm", "team")
  } else {
    join_by <- "player_norm"
  }
  matched <- merge(inj_dt[, ..inj_merge_cols],
                   pr_dt[, ..pr_cols],
                   by = join_by, all.x = TRUE)

  # Drop unmatched (players not in ratings)
  n_unmatched <- sum(is.na(matched$torp))
  if (n_unmatched > 0) {
    unmatched_names <- if ("player" %in% names(matched)) {
      matched[is.na(torp), player]
    } else {
      matched[is.na(torp), player_norm]
    }
    cli::cli_warn("Dropped {n_unmatched} injured player{?s} not found in ratings: {paste(utils::head(unmatched_names, 5), collapse = ', ')}")
  }
  matched <- matched[!is.na(torp)]

  if (nrow(matched) == 0) {
    return(data.table::data.table(
      team = character(), torp_boost = numeric(), return_round = numeric()
    ))
  }

  # Compute TOG-weighted TORP contribution (same logic as prepare_sim_data)
  # Team names already normalised via torp_replace_teams() before the join

  # Get team-level TOG sums from full player ratings for proper weighting
  team_tog <- pr_dt[, .(team_tog_sum = sum(pred_tog, na.rm = TRUE),
                         n_players = .N), by = .(team)]

  matched <- merge(matched, team_tog, by = "team", all.x = TRUE)

  if ("pred_tog" %in% names(matched) && !all(is.na(matched$pred_tog))) {
    matched[, tog_wt := data.table::fifelse(
      team_tog_sum > 0 & !is.na(pred_tog),
      pred_tog * AFL_TEAM_SIZE / team_tog_sum,
      AFL_TEAM_SIZE / n_players
    )]
    matched[, player_boost := torp * tog_wt]
  } else {
    matched[, player_boost := torp]
  }

  # Apply the same discount that was used when removing them
  matched[, player_boost := player_boost * INJURY_KNOWN_DISCOUNT]

  # Aggregate by (team, return_round)
  schedule <- matched[, .(torp_boost = sum(player_boost, na.rm = TRUE)),
                       by = .(team, return_round)]

  schedule[]
}


#' Save Injury Data to GitHub Release
#'
#' Appends the current scrape to the season's injury history on torpdata,
#' deduping to one row per state change. Weekly-source rows are collapsed
#' on (player_norm, team, injury, estimated_return, updated) so you get
#' one row each time a player's status changes (e.g. Test -> Out). Preseason
#' rows are collapsed on (player_norm, team).
#'
#' @param injuries_df A data.frame of injuries (e.g., from [get_all_injuries()]).
#' @param season Numeric season year (e.g. 2026).
#' @return Invisible NULL. Called for side effects (upload).
#' @keywords internal
save_injury_data <- function(injuries_df, season) {
  if (is.null(injuries_df) || nrow(injuries_df) == 0) {
    cli::cli_inform("No injuries to save for {season}")
    return(invisible(NULL))
  }

  now <- Sys.time()
  injuries_df$scraped_at   <- now
  injuries_df$scraped_date <- as.Date(now)

  existing <- tryCatch(load_injury_data(season), error = function(e) NULL)

  if (!is.null(existing) && nrow(existing) > 0) {
    # Backfill scraped_at on pre-upgrade rows that only have scraped_date
    if (!"scraped_at" %in% names(existing)) {
      existing$scraped_at <- as.POSIXct(
        paste(existing$scraped_date, "00:00:00"), tz = "UTC"
      )
    }
    combined <- dplyr::bind_rows(existing, injuries_df)
  } else {
    combined <- injuries_df
  }

  combined <- dplyr::arrange(combined, scraped_at)

  weekly <- combined |>
    dplyr::filter(source == "weekly") |>
    dplyr::distinct(player_norm, team, injury, estimated_return, updated,
                    .keep_all = TRUE)

  preseason <- combined |>
    dplyr::filter(source == "preseason") |>
    dplyr::distinct(player_norm, team, .keep_all = TRUE)

  combined <- dplyr::bind_rows(weekly, preseason) |>
    dplyr::arrange(player_norm, scraped_at)

  n_prev <- if (is.null(existing)) 0L else nrow(existing)
  n_new  <- nrow(combined) - n_prev
  cli::cli_inform("Injury history: {nrow(combined)} rows ({n_new} new state change{?s} this scrape)")

  save_to_release(combined, paste0("injury_list_", season), "injury-data")
  invisible(NULL)
}

