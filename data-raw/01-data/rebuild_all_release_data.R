# Rebuild All GitHub Release Data
#
# Full rebuild of all 12 torpdata release data types across seasons 2021-2026.
# Refetches from AFL API, reprocesses through the updated pipeline, and
# re-uploads to ensure consistent snake_case column names everywhere.
#
# Usage:
#   # Full rebuild (all seasons, all phases)
#   Rscript "torp/data-raw/01-data/rebuild_all_release_data.R"
#
#   # Single season
#   Rscript "torp/data-raw/01-data/rebuild_all_release_data.R" 2026 2026
#
#   # Phase flags (can combine with season args)
#   Rscript "torp/data-raw/01-data/rebuild_all_release_data.R" --api-only
#   Rscript "torp/data-raw/01-data/rebuild_all_release_data.R" --chains-only
#   Rscript "torp/data-raw/01-data/rebuild_all_release_data.R" --derived-only
#   Rscript "torp/data-raw/01-data/rebuild_all_release_data.R" --ratings-only
#   Rscript "torp/data-raw/01-data/rebuild_all_release_data.R" --skip-api --skip-chains
#   Rscript "torp/data-raw/01-data/rebuild_all_release_data.R" --skip-api 2024 2026

# Setup ----

library(dplyr)
library(data.table)
library(mgcv) # required for shot model predictions (bam/gam)
library(tictoc)

# Find the torp package root (works whether run from torpverse/ or torp/)
torp_root <- if (file.exists("DESCRIPTION")) "." else if (file.exists("torp/DESCRIPTION")) "torp" else stop("Cannot find torp package")
devtools::load_all(torp_root)

# Parse Args ----

args <- commandArgs(trailingOnly = TRUE)

flag_args <- args[grepl("^--", args)]
pos_args <- args[!grepl("^--", args)]

# Phase flags
run_api     <- !("--skip-api" %in% flag_args)
run_chains  <- !("--skip-chains" %in% flag_args)
run_derived <- TRUE
run_ratings <- TRUE

if ("--api-only" %in% flag_args) {
  run_chains <- FALSE; run_derived <- FALSE; run_ratings <- FALSE
}
if ("--chains-only" %in% flag_args) {
  run_api <- FALSE; run_derived <- FALSE; run_ratings <- FALSE
}
if ("--derived-only" %in% flag_args) {
  run_api <- FALSE; run_chains <- FALSE; run_ratings <- FALSE
}
if ("--ratings-only" %in% flag_args) {
  run_api <- FALSE; run_chains <- FALSE; run_derived <- FALSE
}

# Season range
if (length(pos_args) >= 2) {
  season_start <- as.integer(pos_args[1])
  season_end   <- as.integer(pos_args[2])
} else if (length(pos_args) == 1) {
  season_start <- as.integer(pos_args[1])
  season_end   <- season_start
} else {
  season_start <- 2021L
  season_end   <- as.integer(get_afl_season())
}

seasons <- season_start:season_end

cli::cli_h1("Rebuild All Release Data")
cli::cli_inform("Seasons: {paste(seasons, collapse = ', ')}")
cli::cli_inform("Phases: API={run_api}, Chains/PBP={run_chains}, Derived={run_derived}, Ratings={run_ratings}")

# Helpers ----

get_start_round <- function(season) {
  if (season >= 2024) 0L else 1L
}

get_max_round <- function(season) {
  if (season == get_afl_season()) get_afl_week() else 28L
}

# Track failures across the run
failures <- data.frame(label = character(), error = character(), time = character(),
                       stringsAsFactors = FALSE)

safe_run <- function(label, expr) {
  tryCatch({
    expr
    cli::cli_alert_success("{label}")
  }, error = function(e) {
    failures <<- rbind(failures, data.frame(
      label = label,
      error = conditionMessage(e),
      time  = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    ))
    cli::cli_alert_danger("{label}: {conditionMessage(e)}")
  })
}

failure_log_path <- file.path(torp_root, "data-raw", "01-data", "rebuild_failures.csv")

# Phase 1: API Data ----

if (run_api) {
  cli::cli_h1("Phase 1: API Data")
  tic("phase_1_api")

  for (season in seasons) {
    cli::cli_h2("Season {season}")

    # Fixtures
    safe_run(paste0("fixtures_", season), {
      fixtures <- get_afl_fixtures(season)
      # Drop list-columns (nested structs) that arrow can't serialize
      list_cols <- names(fixtures)[vapply(fixtures, is.list, logical(1))]
      if (length(list_cols) > 0) {
        fixtures <- fixtures[, !names(fixtures) %in% list_cols, drop = FALSE]
      }
      save_to_release(fixtures, paste0("fixtures_", season), "fixtures-data")
      cli::cli_inform("  fixtures_{season}: {nrow(fixtures)} rows")
    })

    # Results
    safe_run(paste0("results_", season), {
      results <- get_afl_results(season)
      save_to_release(results, paste0("results_", season), "results-data")
      cli::cli_inform("  results_{season}: {nrow(results)} rows")
    })

    # Player Stats
    safe_run(paste0("player_stats_", season), {
      pstats <- get_afl_player_stats(season) |>
        dplyr::select(where(~ dplyr::n_distinct(.) > 1)) |>
        janitor::clean_names()
      pstats <- .normalise_player_stats_columns(pstats)
      if (!"season" %in% names(pstats)) pstats$season <- as.integer(season)
      save_to_release(pstats, paste0("player_stats_", season), "player_stats-data")
      cli::cli_inform("  player_stats_{season}: {nrow(pstats)} rows")
    })

    # Teams / Lineups
    safe_run(paste0("teams_", season), {
      teams <- get_afl_lineups(season)
      save_to_release(teams, paste0("teams_", season), "teams-data")
      cli::cli_inform("  teams_{season}: {nrow(teams)} rows")
    })

    # Player Details
    safe_run(paste0("player_details_", season), {
      pd <- get_afl_player_details(season)
      save_to_release(pd, paste0("player_details_", season), "player_details-data")
      cli::cli_inform("  player_details_{season}: {nrow(pd)} rows")
    })
  }

  toc(log = TRUE)
}

# Phase 2: Chains + PBP ----

if (run_chains) {
  cli::cli_h1("Phase 2: Chains + PBP")
  tic("phase_2_chains_pbp")

  for (season in seasons) {
    cli::cli_h2("Season {season}")
    start_rd <- get_start_round(season)
    end_rd   <- get_max_round(season)
    rounds   <- start_rd:end_rd

    # Fetch all chains for season
    cli::cli_inform("Fetching chains for {season}, rounds {start_rd}-{end_rd}...")
    tic(paste0("chains_", season))

    all_chains <- purrr::map(rounds, function(rd) {
      tryCatch(get_week_chains(season, rd), error = function(e) {
        cli::cli_warn("  Failed to fetch chains {season} R{rd}: {conditionMessage(e)}")
        NULL
      })
    })

    chains <- data.table::rbindlist(purrr::compact(all_chains), use.names = TRUE, fill = TRUE)

    if (nrow(chains) == 0) {
      cli::cli_warn("No chains data for {season} - skipping")
      toc(log = TRUE)
      next
    }

    # Save chains
    safe_run(paste0("chains_data_", season, "_all"), {
      save_to_release(chains, paste0("chains_data_", season, "_all"), "chains-data")
      cli::cli_inform("  chains_{season}: {nrow(chains)} rows")
    })

    # Process PBP from the same chains data (no re-fetch)
    safe_run(paste0("pbp_data_", season, "_all"), {
      pbp <- chains |>
        clean_pbp() |>
        clean_model_data_epv() |>
        clean_shots_data() |>
        add_shot_vars() |>
        add_epv_vars() |>
        clean_model_data_wp() |>
        add_wp_vars()

      save_to_release(pbp, paste0("pbp_data_", season, "_all"), "pbp-data")
      cli::cli_inform("  pbp_{season}: {nrow(pbp)} rows")
    })

    toc(log = TRUE)

    # Free memory between seasons
    rm(all_chains, chains)
    gc()
  }

  toc(log = TRUE)
}

# Phase 3: Derived Data ----

if (run_derived) {
  cli::cli_h1("Phase 3: Derived Data")
  tic("phase_3_derived")

  for (season in seasons) {
    cli::cli_h2("Season {season}")
    start_rd <- get_start_round(season)
    end_rd   <- get_max_round(season)

    # XG Data
    safe_run(paste0("xg_data_", season), {
      xg_df <- calculate_match_xgs(season, start_rd:end_rd)
      save_to_release(xg_df, paste0("xg_data_", season), "xg-data")
      cli::cli_inform("  xg_{season}: {nrow(xg_df)} rows")
    })

    # Player Game Data
    safe_run(paste0("player_game_", season), {
      pbp <- load_pbp(season, rounds = TRUE)
      pstats <- load_player_stats(season)
      teams_data <- load_teams(season)
      pgd <- create_player_game_data(pbp, pstats, teams_data)
      save_to_release(pgd, paste0("player_game_", season), "player_game-data")
      cli::cli_inform("  player_game_{season}: {nrow(pgd)} rows")
    })

    # EP/WP Chart Data
    safe_run(paste0("ep_wp_chart_", season, "_all"), {
      pbp <- load_pbp(season, rounds = TRUE)
      chart_cols <- c(
        "match_id", "season", "round_number", "period", "period_seconds",
        "total_seconds", "display_order",
        "home_team_name", "away_team_name", "team", "home",
        "pos_team_points", "opp_team_points", "points_diff",
        "home_points", "away_points",
        "exp_pts", "delta_epv", "wp", "wpa",
        "description", "player_name", "play_type",
        "shot_row", "points_shot"
      )
      available_cols <- intersect(chart_cols, names(pbp))
      chart_data <- pbp[, available_cols, drop = FALSE]
      save_to_release(chart_data, paste0("ep_wp_chart_", season, "_all"), "ep_wp_chart-data")
      cli::cli_inform("  ep_wp_chart_{season}: {nrow(chart_data)} rows, {ncol(chart_data)} cols")
    })

    gc()
  }

  toc(log = TRUE)
}

# Phase 4: Ratings ----

if (run_ratings) {
  cli::cli_h1("Phase 4: Ratings")
  tic("phase_4_ratings")

  for (season in seasons) {
    cli::cli_h2("Season {season}")
    start_rd <- get_start_round(season)
    end_rd   <- get_max_round(season)

    # Player Game Ratings
    safe_run(paste0("player_game_ratings_", season), {
      pgd <- load_player_game_data(season)
      ratings <- .compute_player_game_ratings(pgd, season, start_rd:end_rd)
      save_to_release(ratings, paste0("player_game_ratings_", season), "player_game_ratings-data")
      cli::cli_inform("  player_game_ratings_{season}: {nrow(ratings)} rows")
    })

    # Player Season Ratings
    safe_run(paste0("player_season_ratings_", season), {
      pgd <- load_player_game_data(season)
      pgr <- .compute_player_game_ratings(pgd, season, start_rd:end_rd)
      ratings <- .compute_player_season_ratings(pgr)
      save_to_release(ratings, paste0("player_season_ratings_", season), "player_season_ratings-data")
      cli::cli_inform("  player_season_ratings_{season}: {nrow(ratings)} rows")
    })

    # PSR (Player Skill Ratings)
    safe_run(paste0("psr_", season), {
      skills <- load_player_skills(season)
      psr_coef_path <- system.file("extdata", "psr_v2_coefficients.csv", package = "torp")
      if (psr_coef_path == "") {
        psr_coef_path <- file.path("data-raw", "cache-skills", "psr_v2_coefficients.csv")
      }
      if (file.exists(psr_coef_path)) {
        coef_df <- utils::read.csv(psr_coef_path)
        psr_data <- calculate_psr(skills, coef_df)
        save_to_release(psr_data, paste0("psr_", season), "psr-data")
        cli::cli_inform("  psr_{season}: {nrow(psr_data)} rows")
      } else {
        cli::cli_warn("  PSR coefficient file not found - skipping PSR for {season}")
      }
    })
  }

  toc(log = TRUE)
}

# Summary ----

cli::cli_h1("Rebuild Complete")

if (nrow(failures) > 0) {
  cli::cli_alert_warning("{nrow(failures)} failure{?s}:")
  for (i in seq_len(nrow(failures))) {
    cli::cli_alert_danger("  {failures$label[i]}: {failures$error[i]}")
  }
  write.csv(failures, failure_log_path, row.names = FALSE)
  cli::cli_inform("Failure log written to {failure_log_path}")
} else {
  cli::cli_alert_success("All data types rebuilt successfully!")
  # Clean up any old failure log
  if (file.exists(failure_log_path)) file.remove(failure_log_path)
}

cli::cli_h2("Timing")
tic.log(format = TRUE) |> unlist() |> cat(sep = "\n")
tic.clearlog()
