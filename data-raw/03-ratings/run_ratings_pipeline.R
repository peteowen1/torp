# TORP Ratings Pipeline
#
# End-to-end script for computing TORP ratings:
#   Stage 1: Refresh upstream data (player_stats, teams) from fitzRoy
#   Stage 2: Build player game data from PBP + player_stats + teams
#   Stage 3: Compute TORP ratings per season/round and release
#
# Usage:
#   Rscript data-raw/03-ratings/run_ratings_pipeline.R
#   Or: source("data-raw/03-ratings/run_ratings_pipeline.R")
#
# CI Usage (from GitHub Actions):
#   Set config variables before sourcing:
#     SEASONS <- NULL; REBUILD_PLAYER_GAME <- TRUE
#   Then source this file - it will skip setting defaults if they already exist.

# Setup ----

library(dplyr)
library(cli)
library(tictoc)
library(piggyback)

devtools::load_all()

# Source daily_release.R for update_player_stats() and update_teams()
source(here::here("data-raw/01-data/daily_release.R"))

# Configuration ----
# These defaults are only set if not already defined (allows CI to override)

# Which seasons to process:
#   NULL          = current season only
#   numeric vector = specific seasons (e.g. 2024:2025)
#   TRUE          = all seasons 2021+
if (!exists("SEASONS", envir = .GlobalEnv)) SEASONS <- TRUE

# Whether to re-fetch player_stats + teams from fitzRoy
if (!exists("REFRESH_UPSTREAM", envir = .GlobalEnv)) REFRESH_UPSTREAM <- FALSE

# Whether to rebuild player game tables from PBP
if (!exists("REBUILD_PLAYER_GAME", envir = .GlobalEnv)) REBUILD_PLAYER_GAME <- TRUE

# Full rebuild vs incremental (only configured seasons)
if (!exists("REBUILD_ALL_RATINGS", envir = .GlobalEnv)) REBUILD_ALL_RATINGS <- TRUE

# Resolve seasons ----

resolve_seasons <- function(seasons) {
  if (is.null(seasons)) return(get_afl_season())
  if (isTRUE(seasons)) return(2021:get_afl_season())
  seasons
}

seasons <- resolve_seasons(SEASONS)

cli::cli_h1("TORP Ratings Pipeline")
cli::cli_inform("Seasons: {paste(seasons, collapse = ', ')}")
cli::cli_inform("Refresh upstream: {REFRESH_UPSTREAM}")
cli::cli_inform("Rebuild player game: {REBUILD_PLAYER_GAME}")
cli::cli_inform("Rebuild all ratings: {REBUILD_ALL_RATINGS}")

# Stage 1: Refresh Upstream Data ----

if (REFRESH_UPSTREAM) {
  cli::cli_h2("Stage 1: Refresh Upstream Data")
  tictoc::tic("stage_1_upstream")

  for (s in seasons) {
    tryCatch({
      cli::cli_progress_step("Refreshing player_stats and teams for {s}")
      update_player_stats(s)
      update_teams(s)
    }, error = function(e) {
      cli::cli_warn("Failed to refresh upstream for {s}: {conditionMessage(e)}")
    })
  }

  tictoc::toc(log = TRUE)
} else {
  cli::cli_alert_info("Skipping Stage 1 (REFRESH_UPSTREAM = FALSE)")
}

# Stage 2: Build Player Game Data + Release ----

stage2_failed_seasons <- character()

if (REBUILD_PLAYER_GAME) {
  cli::cli_h2("Stage 2: Build Player Game Data")
  tictoc::tic("stage_2_player_game")

  for (s in seasons) {
    tryCatch({
      cli::cli_progress_step("Building player game data for {s}")

      pbp <- load_pbp(s, rounds = TRUE)
      pstats <- load_player_stats(s)
      teams_data <- load_teams(s)

      cli::cli_inform("  PBP: {nrow(pbp)} rows | player_stats: {nrow(pstats)} rows | teams: {nrow(teams_data)} rows")
      if (nrow(pbp) == 0) {
        cli::cli_warn("No PBP data for {s} - skipping")
        stage2_failed_seasons <- c(stage2_failed_seasons, as.character(s))
        next
      }

      pgd <- create_player_game_data(pbp, pstats, teams_data)
      cli::cli_inform("  Player game data: {nrow(pgd)} rows")

      file_name <- paste0("player_game_", s)
      save_to_release(pgd, file_name, "player_game-data")
      cli::cli_alert_success("Released {file_name} ({nrow(pgd)} rows)")
    }, error = function(e) {
      cli::cli_warn("Failed to build player game data for {s}: {conditionMessage(e)}")
      stage2_failed_seasons <<- c(stage2_failed_seasons, as.character(s))
    })
  }

  if (length(stage2_failed_seasons) > 0) {
    cli::cli_warn("Stage 2 failed for seasons: {paste(stage2_failed_seasons, collapse = ', ')} - Stage 3 will use stale player game data for these")
  }

  tictoc::toc(log = TRUE)
} else {
  cli::cli_alert_info("Skipping Stage 2 (REBUILD_PLAYER_GAME = FALSE)")
}

# Stage 3: Compute TORP Ratings + Release ----

cli::cli_h2("Stage 3: Compute TORP Ratings")
tictoc::tic("stage_3_ratings")

cli::cli_progress_step("Loading all player game data")
all_pgd <- load_player_game_data(TRUE)
cli::cli_inform("Player game data loaded: {nrow(all_pgd)} rows")

# Convert to keyed data.table once — avoids full copy on every round call
data.table::setDT(all_pgd)
data.table::setkey(all_pgd, match_id)

# Pre-load shared data once — avoids ~145 redundant loads per full rebuild
cli::cli_progress_step("Pre-loading shared reference data")
shared_skills <- tryCatch(get_player_skills(current = FALSE), error = function(e) {
  cli::cli_warn("Could not load skills: {conditionMessage(e)}")
  NULL
})
shared_fixtures <- load_fixtures(TRUE)

get_torp_df <- function(year, rounds, pgd, skills, fixtures) {
  plyr_tm_df <- load_player_details(year)
  if (nrow(plyr_tm_df) == 0 || !"season" %in% names(plyr_tm_df)) {
    plyr_tm_df <- load_player_details(year - 1)
  }

  results <- purrr::map(rounds, ~ {
    tryCatch(
      calculate_torp_ratings(year, .x,
        player_game_data = pgd,
        plyr_tm_df = plyr_tm_df,
        fixtures = fixtures,
        skills = skills),
      error = function(e) {
        cli::cli_warn("Failed for {year} R{.x}: {conditionMessage(e)}")
        NULL
      }
    )
  }, .progress = TRUE)

  n_failed <- sum(vapply(results, is.null, logical(1)))
  if (n_failed == length(rounds) && length(rounds) > 1) {
    cli::cli_abort("All {length(rounds)} rounds failed for {year} -- likely a systemic data issue")
  }

  results |>
    dplyr::bind_rows() |>
    (\(df) {
      if (nrow(df) == 0) return(df)
      if (!"player_id" %in% names(df)) {
        cli::cli_warn("player_id column missing from ratings output for {year} - row_id cannot be computed")
        return(df)
      }
      dplyr::mutate(df, row_id = paste0(player_id, season, sprintf("%02d", round)))
    })()
}

torp_season_list <- list()
failed_seasons <- character()
empty_seasons <- character()

for (s in seasons) {
  tryCatch({
    start_round <- if (s >= 2024) 0 else 1
    max_round <- if (s == get_afl_season()) {
      get_afl_week(type = "next")
    } else {
      28
    }

    cli::cli_h3("Computing ratings for {s} (rounds {start_round}-{max_round})")
    tictoc::tic(paste0("ratings_", s))

    torp_df <- get_torp_df(s, start_round:max_round, all_pgd, shared_skills, shared_fixtures)
    cli::cli_inform("  {s}: {nrow(torp_df)} rating rows")

    if (nrow(torp_df) == 0) {
      empty_seasons <<- c(empty_seasons, as.character(s))
    }

    torp_season_list[[as.character(s)]] <- torp_df

    tictoc::toc(log = TRUE)
  }, error = function(e) {
    cli::cli_warn("Failed to compute ratings for {s}: {conditionMessage(e)}")
    failed_seasons <<- c(failed_seasons, as.character(s))
  })
}

torp_new <- dplyr::bind_rows(torp_season_list)
cli::cli_inform("New ratings computed: {nrow(torp_new)} rows")

if (length(failed_seasons) > 0) {
  cli::cli_warn("Failed seasons: {paste(failed_seasons, collapse = ', ')}")
}
if (length(empty_seasons) > 0) {
  cli::cli_alert_info("Empty seasons (no data available yet): {paste(empty_seasons, collapse = ', ')}")
}
if (length(failed_seasons) == length(seasons)) {
  cli::cli_abort("All seasons failed to compute - aborting pipeline")
}
if (nrow(torp_new) == 0 && length(failed_seasons) == 0) {
  cli::cli_alert_info("No new ratings computed (pre-season or no games played yet) - exiting gracefully")
}

if (nrow(torp_new) > 0) {
  if (!REBUILD_ALL_RATINGS) {
    cli::cli_progress_step("Incremental update: loading existing ratings")
    existing <- tryCatch(
      load_torp_ratings(),
      error = function(e) {
        cli::cli_warn("No existing ratings found, doing full build: {conditionMessage(e)}")
        NULL
      }
    )

    if (!is.null(existing) && nrow(existing) > 0) {
      torp_df_total <- existing |>
        dplyr::rows_upsert(torp_new, by = "row_id")
      if (nrow(torp_df_total) < nrow(existing)) {
        cli::cli_abort("Row count decreased after upsert ({nrow(existing)} -> {nrow(torp_df_total)}) - possible data corruption")
      }
      cli::cli_inform("Upserted into existing: {nrow(torp_df_total)} total rows (was {nrow(existing)})")
    } else {
      torp_df_total <- torp_new
    }
  } else {
    torp_df_total <- torp_new
  }

  save_to_release(torp_df_total, "torp_ratings", "ratings-data")

  uploaded <- tryCatch(load_torp_ratings(), error = function(e) NULL)
  if (is.null(uploaded) || nrow(uploaded) != nrow(torp_df_total)) {
    cli::cli_warn("Upload verification failed - piggyback cache delay may be the cause")
  }
  cli::cli_alert_success("Released torp_ratings ({nrow(torp_df_total)} rows)")
}

tictoc::toc(log = TRUE)

# Stage 4: Compute Team Ratings ----

cli::cli_h2("Stage 4: Compute Team Ratings")
tictoc::tic("stage_4_team_ratings")

tryCatch({
  # Use the just-released torp_ratings (or the one we built above)
  ratings_for_teams <- if (exists("torp_df_total") && nrow(torp_df_total) > 0) {
    torp_df_total
  } else {
    load_torp_ratings()
  }

  cli::cli_inform("Building team ratings from {nrow(ratings_for_teams)} player rating rows")

  team_ratings <- ratings_for_teams |>
    dplyr::filter(!is.na(.data$torp)) |>
    dplyr::group_by(.data$season, .data$round, .data$team) |>
    dplyr::mutate(
      # Scale pred_tog to sum to 18 per team (18 full-game equivalents)
      team_tog_sum = sum(.data$pred_tog, na.rm = TRUE),
      tog_wt = dplyr::if_else(.data$team_tog_sum > 0,
                               .data$pred_tog * 18 / .data$team_tog_sum, 0)
    ) |>
    dplyr::summarise(
      team_torp    = round(sum(.data$torp * .data$tog_wt, na.rm = TRUE), 2),
      team_recv    = round(sum(.data$torp_recv * .data$tog_wt, na.rm = TRUE), 2),
      team_disp    = round(sum(.data$torp_disp * .data$tog_wt, na.rm = TRUE), 2),
      team_spoil   = round(sum(.data$torp_spoil * .data$tog_wt, na.rm = TRUE), 2),
      team_hitout  = round(sum(.data$torp_hitout * .data$tog_wt, na.rm = TRUE), 2),
      top_player   = .data$player_name[which.max(.data$torp)],
      top_torp     = round(max(.data$torp, na.rm = TRUE), 2),
      n_players    = sum(.data$pred_tog > 0),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$season, .data$round, -.data$team_torp)

  save_to_release(team_ratings, "team_ratings", "team_ratings-data")
  cli::cli_alert_success("Released team_ratings ({nrow(team_ratings)} rows)")
}, error = function(e) {
  cli::cli_warn("Failed to compute team ratings: {conditionMessage(e)}")
})

tictoc::toc(log = TRUE)

# Summary ----

cli::cli_h2("Pipeline Complete")
timings <- tictoc::tic.log(format = TRUE)
for (t in timings) {
  cli::cli_inform(t)
}
tictoc::tic.clearlog()
