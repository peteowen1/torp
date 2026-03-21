# EPR Ratings Pipeline
#
# End-to-end script for computing EPR ratings:
#   Stage 1: Refresh upstream data (player_stats, teams) from AFL API
#   Stage 2: Build player game data from PBP + player_stats + teams
#   Stage 3: Compute EPR ratings per season/round and release
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

# Whether to re-fetch player_stats + teams from AFL API
if (!exists("REFRESH_UPSTREAM", envir = .GlobalEnv)) REFRESH_UPSTREAM <- TRUE

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

cli::cli_h1("EPR Ratings Pipeline")
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
      cli::cli_alert_danger("Failed to refresh upstream for {s}: {conditionMessage(e)}")
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

  # Batch load all seasons at once (parallel download via curl::multi_download)
  cli::cli_progress_step("Batch loading PBP, player_stats, teams for {length(seasons)} seasons")
  all_pbp <- load_pbp(seasons, rounds = TRUE)
  all_pstats <- load_player_stats(seasons)
  all_teams <- load_teams(seasons)
  cli::cli_inform("  Loaded: PBP {nrow(all_pbp)} | player_stats {nrow(all_pstats)} | teams {nrow(all_teams)}")

  for (s in seasons) {
    tryCatch({
      cli::cli_progress_step("Building player game data for {s}")

      pbp <- all_pbp[all_pbp$season == s, ]
      pstats <- all_pstats[all_pstats$season == s, ]
      teams_data <- all_teams[all_teams$season == s, ]

      cli::cli_inform("  PBP: {nrow(pbp)} rows | player_stats: {nrow(pstats)} rows | teams: {nrow(teams_data)} rows")
      if (nrow(pbp) == 0) {
        cli::cli_alert_danger("No PBP data for {s} - skipping")
        stage2_failed_seasons <- c(stage2_failed_seasons, as.character(s))
        next
      }

      pgd <- create_player_game_data(pbp, pstats, teams_data)
      cli::cli_inform("  Player game data: {nrow(pgd)} rows")

      file_name <- paste0("player_game_", s)
      save_to_release(pgd, file_name, "player_game-data")
      cli::cli_alert_success("Released {file_name} ({nrow(pgd)} rows)")
    }, error = function(e) {
      cli::cli_alert_danger("Failed to build player game data for {s}: {conditionMessage(e)}")
      stage2_failed_seasons <<- c(stage2_failed_seasons, as.character(s))
    })
  }

  if (length(stage2_failed_seasons) > 0) {
    cli::cli_alert_danger("Stage 2 failed for seasons: {paste(stage2_failed_seasons, collapse = ', ')} - Stage 3 will use stale player game data for these")
  }

  tictoc::toc(log = TRUE)
} else {
  cli::cli_alert_info("Skipping Stage 2 (REBUILD_PLAYER_GAME = FALSE)")
}

# Stage 3: Compute EPR Ratings + Release ----

cli::cli_h2("Stage 3: Compute EPR Ratings")
tictoc::tic("stage_3_ratings")

cli::cli_progress_step("Loading all player game data")
all_pgd <- load_player_game_data(TRUE)
cli::cli_inform("Player game data loaded: {nrow(all_pgd)} rows")

# Convert to keyed data.table once — avoids full copy on every round call
data.table::setDT(all_pgd)
data.table::setkey(all_pgd, match_id)

# Pre-load shared data once — avoids ~145 redundant loads per full rebuild
cli::cli_progress_step("Pre-loading shared reference data")
shared_stat_ratings <- tryCatch(get_player_stat_ratings(current = FALSE), error = function(e) {
  cli::cli_alert_danger("Could not load stat ratings: {conditionMessage(e)}")
  NULL
})
# Backwards compat: rename time_on_ground_rating → cond_tog_rating if needed
if (!is.null(shared_stat_ratings)) {
  if (!"cond_tog_rating" %in% names(shared_stat_ratings) && "time_on_ground_rating" %in% names(shared_stat_ratings)) {
    shared_stat_ratings$cond_tog_rating <- shared_stat_ratings$time_on_ground_rating
  }
  # Also handle old _skill column names during transition
  if (!"cond_tog_rating" %in% names(shared_stat_ratings) && "cond_tog_skill" %in% names(shared_stat_ratings)) {
    shared_stat_ratings$cond_tog_rating <- shared_stat_ratings$cond_tog_skill
  }
  if (!"squad_selection_rating" %in% names(shared_stat_ratings)) {
    if ("squad_selection_skill" %in% names(shared_stat_ratings)) {
      shared_stat_ratings$squad_selection_rating <- shared_stat_ratings$squad_selection_skill
    } else {
      cli::cli_alert_danger("Stat ratings missing squad_selection_rating; using cond_tog_rating alone as pred_tog")
      shared_stat_ratings$squad_selection_rating <- 1
    }
  }
}
shared_fixtures <- load_fixtures(TRUE)

get_epr_df <- function(year, rounds, pgd, stat_ratings, fixtures) {
  plyr_tm_df <- load_player_details(year)
  if (nrow(plyr_tm_df) == 0 || !"season" %in% names(plyr_tm_df)) {
    plyr_tm_df <- load_player_details(year - 1)
  }

  # Build round_info with dates from fixtures
  fix_dt <- data.table::as.data.table(fixtures)
  fix_dates <- fix_dt[
    season == year & round_number %in% rounds,
    .(date_val = lubridate::as_date(min(utc_start_time))),
    by = .(round_val = round_number)
  ]

  round_info <- data.table::data.table(
    round_val = rounds,
    match_ref = paste0("CD_M", year, "014", sprintf("%02d", rounds))
  )
  round_info <- round_info[fix_dates, on = "round_val", nomatch = NULL]

  if (nrow(round_info) == 0) {
    cli::cli_alert_danger("No fixtures found for {year}")
    return(data.frame())
  }

  # Batch compute all rounds' player stats in one data.table pass
  batch_stats <- calculate_epr_stats_batch(pgd, round_info)

  # Attach decomposed TOG from stat ratings
  batch_stats[, pred_tog := NA_real_]
  batch_stats[, pred_selection := NA_real_]
  batch_stats[, pred_cond_tog := NA_real_]
  if (!is.null(stat_ratings)) {
    stat_ratings_dt <- data.table::as.data.table(stat_ratings)
    batch_stats[stat_ratings_dt, `:=`(
      pred_selection = i.squad_selection_rating,
      pred_cond_tog = i.cond_tog_rating
    ), on = "player_id"]
    batch_stats[is.na(pred_selection), pred_selection := 0]
    batch_stats[is.na(pred_cond_tog), pred_cond_tog := 0]
    batch_stats[, pred_tog := pred_selection * pred_cond_tog]
  }

  # Pre-compute fixtures summary once (avoids re-summarising 6K rows per round)
  fix_summary <- fixtures |>
    dplyr::group_by(season = .data$season, round = .data$round_number) |>
    dplyr::summarise(ref_date = lubridate::as_date(min(.data$utc_start_time)), .groups = "drop")

  # Per-round: roster join + TOG centering (lightweight ~700 rows per round)
  results <- lapply(round_info$round_val, function(rv) {
    round_dt <- batch_stats[round_val == rv]
    final_df <- .prepare_final_dataframe(plyr_tm_df, round_dt, year, rv, fixtures, fix_summary = fix_summary)

    if (!is.null(stat_ratings) && nrow(final_df) > 0) {
      final_df$pred_tog[is.na(final_df$pred_tog)] <- 0
      tot_tog <- sum(final_df$pred_tog)
      if (tot_tog > 0) {
        n_teams <- length(unique(final_df$team))
        target_tog <- n_teams * 18L
        final_df$pred_tog <- final_df$pred_tog * (target_tog / tot_tog)
        comps <- c("recv_epr", "disp_epr", "spoil_epr", "hitout_epr")
        for (comp in comps) {
          avg_val <- sum(final_df[[comp]] * final_df$pred_tog, na.rm = TRUE) / sum(final_df$pred_tog)
          final_df[[comp]] <- final_df[[comp]] - avg_val
        }
        final_df$epr <- round(final_df$recv_epr + final_df$disp_epr + final_df$spoil_epr + final_df$hitout_epr, 2)
        for (comp in comps) {
          final_df[[comp]] <- round(final_df[[comp]], 2)
        }
      }
    }

    final_df
  })

  n_empty <- sum(vapply(results, function(x) nrow(x) == 0, logical(1)))
  if (n_empty == length(round_info$round_val) && length(round_info$round_val) > 1) {
    cli::cli_abort("All {length(round_info$round_val)} rounds empty for {year}")
  }

  results |>
    dplyr::bind_rows() |>
    (\(df) {
      if (nrow(df) == 0) return(df)
      if (!"player_id" %in% names(df)) {
        cli::cli_alert_danger("player_id column missing from ratings output for {year} - row_id cannot be computed")
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

    torp_df <- get_epr_df(s, start_round:max_round, all_pgd, shared_stat_ratings, shared_fixtures)
    cli::cli_inform("  {s}: {nrow(torp_df)} rating rows")

    if (nrow(torp_df) == 0) {
      empty_seasons <<- c(empty_seasons, as.character(s))
    }

    torp_season_list[[as.character(s)]] <- torp_df

    tictoc::toc(log = TRUE)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to compute ratings for {s}: {conditionMessage(e)}")
    failed_seasons <<- c(failed_seasons, as.character(s))
  })
}

torp_new <- dplyr::bind_rows(torp_season_list)
cli::cli_inform("New ratings computed: {nrow(torp_new)} rows")

if (length(failed_seasons) > 0) {
  cli::cli_alert_danger("Failed seasons: {paste(failed_seasons, collapse = ', ')}")
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
        cli::cli_alert_danger("No existing ratings found, doing full build: {conditionMessage(e)}")
        NULL
      }
    )

    if (!is.null(existing) && nrow(existing) > 0) {
      # Deduplicate both sides by row_id (keeps first occurrence)
      n_dup_existing <- sum(duplicated(existing$row_id))
      n_dup_new <- sum(duplicated(torp_new$row_id))
      if (n_dup_existing > 0) {
        cli::cli_alert_danger("Existing ratings had {n_dup_existing} duplicate row_id{?s} (keeping first)")
      }
      if (n_dup_new > 0) {
        cli::cli_alert_danger("New ratings had {n_dup_new} duplicate row_id{?s} (keeping first)")
      }
      existing <- dplyr::distinct(existing, row_id, .keep_all = TRUE)
      torp_new <- dplyr::distinct(torp_new, row_id, .keep_all = TRUE)
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

  # Blend PSR into ratings so the release has torp/psr/osr/dsr columns
  psr_df <- tryCatch({
    stat_ratings <- load_player_stat_ratings()
    .compute_psr_from_stat_ratings(stat_ratings)
  }, error = function(e) {
    cli::cli_warn("Could not compute PSR for release: {e$message}")
    NULL
  })
  if (!is.null(psr_df) && nrow(psr_df) > 0 && "psr" %in% names(psr_df)) {
    torp_df_total <- calculate_torp(torp_df_total, psr_df)
    cli::cli_alert_success("Blended PSR into ratings ({sum(!is.na(torp_df_total$torp))} rows with torp)")
  }

  save_to_release(torp_df_total, "torp_ratings", "ratings-data")

  uploaded <- tryCatch(load_torp_ratings(), error = function(e) NULL)
  if (is.null(uploaded) || nrow(uploaded) != nrow(torp_df_total)) {
    cli::cli_alert_danger("Upload verification failed - piggyback cache delay may be the cause")
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
    dplyr::filter(!is.na(.data$epr)) |>
    dplyr::group_by(.data$season, .data$round, .data$team) |>
    dplyr::mutate(
      # Scale pred_tog to sum to 18 per team (18 full-game equivalents)
      team_tog_sum = sum(.data$pred_tog, na.rm = TRUE),
      tog_wt = dplyr::if_else(.data$team_tog_sum > 0,
                               .data$pred_tog * 18 / .data$team_tog_sum, 0)
    ) |>
    dplyr::summarise(
      team_epr     = round(sum(.data$epr * .data$tog_wt, na.rm = TRUE), 2),
      team_recv    = round(sum(.data$recv_epr * .data$tog_wt, na.rm = TRUE), 2),
      team_disp    = round(sum(.data$disp_epr * .data$tog_wt, na.rm = TRUE), 2),
      team_spoil   = round(sum(.data$spoil_epr * .data$tog_wt, na.rm = TRUE), 2),
      team_hitout  = round(sum(.data$hitout_epr * .data$tog_wt, na.rm = TRUE), 2),
      top_player   = .data$player_name[which.max(.data$epr)],
      top_epr      = round(max(.data$epr, na.rm = TRUE), 2),
      n_players    = sum(.data$pred_tog > 0),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$season, .data$round, -.data$team_epr)

  save_to_release(team_ratings, "team_ratings", "team_ratings-data")
  cli::cli_alert_success("Released team_ratings ({nrow(team_ratings)} rows)")
}, error = function(e) {
  cli::cli_alert_danger("Failed to compute team ratings: {conditionMessage(e)}")
})

tictoc::toc(log = TRUE)

# Stage 5: Compute Player Game & Season Ratings ----

cli::cli_h2("Stage 5: Player Game & Season Ratings")
tictoc::tic("stage_5_derived_ratings")

for (s in seasons) {
  tryCatch({
    start_round <- get_start_round(s)
    max_round <- get_max_round(s)

    pgd <- all_pgd[all_pgd$season == s, ]
    if (nrow(pgd) == 0) next

    # Player game ratings (EPV-based)
    pgr <- .compute_player_game_ratings(pgd, s, start_round:max_round)

    # Add PSV columns from box-score stats
    pstats_season <- data.table::as.data.table(all_pstats[all_pstats$season == s, ])
    if (nrow(pstats_season) > 0) {
      # Ensure tog column exists (PSV expects fraction 0-1)
      if (!"tog" %in% names(pstats_season) && "time_on_ground_percentage" %in% names(pstats_season)) {
        pstats_season[, tog := pmax(time_on_ground_percentage / 100, 0.1)]
      }
      psv_result <- tryCatch({
        .compute_psv(pstats_season)
      }, error = function(e) {
        cli::cli_warn("PSV computation failed for {s}: {conditionMessage(e)}")
        NULL
      })
      if (!is.null(psv_result)) {
        psv_cols <- intersect(c("psv", "osv", "dsv"), names(psv_result))
        if (length(psv_cols) > 0 && "player_id" %in% names(psv_result) &&
            "match_id" %in% names(psv_result)) {
          psv_slim <- psv_result[, c("player_id", "match_id", psv_cols), with = FALSE]
          pgr <- merge(pgr, psv_slim, by = c("player_id", "match_id"), all.x = TRUE)
          cli::cli_inform("  Added PSV columns to game ratings ({sum(!is.na(pgr$psv))} matched)")
        }
      }
    }

    # Compute per-game TORP value: 50% EPV + 50% PSV (parallels career TORP = 50% EPR + 50% PSR)
    if (all(c("epv", "psv") %in% names(pgr))) {
      pgr$torp_value <- round(
        TORP_EPR_WEIGHT * pgr$epv + (1 - TORP_EPR_WEIGHT) * pgr$psv,
        1
      )
      pgr$torp_value_p80 <- round(pgr$torp_value / pgr$tog, 1)
    }

    file_name <- paste0("player_game_ratings_", s)
    save_to_release(pgr, file_name, "player_game_ratings-data")
    cli::cli_alert_success("Released {file_name} ({nrow(pgr)} rows)")

    # Player season ratings
    psr <- .compute_player_season_ratings(pgr)
    file_name <- paste0("player_season_ratings_", s)
    save_to_release(psr, file_name, "player_season_ratings-data")
    cli::cli_alert_success("Released {file_name} ({nrow(psr)} rows)")
  }, error = function(e) {
    cli::cli_alert_danger("Failed derived ratings for {s}: {conditionMessage(e)}")
  })
}

tictoc::toc(log = TRUE)

# Stage 6: Compute & Release PSR ----

cli::cli_h2("Stage 6: Compute & Release PSR")
tictoc::tic("stage_6_psr")

tryCatch({
  stat_ratings <- load_player_stat_ratings(TRUE)
  psr_coef_path <- file.path("data-raw", "cache-skills", "psr_v2_coefficients.csv")
  if (!file.exists(psr_coef_path)) {
    psr_coef_path <- system.file("extdata", "psr_v2_coefficients.csv", package = "torp")
  }
  if (file.exists(psr_coef_path) && nchar(psr_coef_path) > 0) {
    coef_df <- utils::read.csv(psr_coef_path)
    psr_all <- calculate_psr(stat_ratings, coef_df)
    cli::cli_inform("PSR computed for {nrow(psr_all)} player-rounds across {length(unique(psr_all$season))} seasons")

    for (s in sort(unique(psr_all$season))) {
      psr_season <- psr_all[psr_all$season == s, ]
      file_name <- paste0("psr_", s)
      save_to_release(psr_season, file_name, "psr-data")
      cli::cli_alert_success("Released {file_name} ({nrow(psr_season)} rows)")
    }
  } else {
    cli::cli_warn("PSR coefficient file not found - skipping PSR release")
  }
}, error = function(e) {
  cli::cli_alert_danger("Failed to compute/release PSR: {conditionMessage(e)}")
})

tictoc::toc(log = TRUE)

# Summary ----

cli::cli_h2("Pipeline Complete")
timings <- tictoc::tic.log(format = TRUE)
for (t in timings) {
  cli::cli_inform(t)
}
tictoc::tic.clearlog()
