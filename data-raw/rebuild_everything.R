# Full Torpverse Rebuild
#
# Nuclear option: rescrape everything, retrain all models, rebuild all derived
# data, recompute ratings, regenerate predictions, re-simulate season.
#
# Usage:
#   powershell.exe -Command 'Rscript "torp/data-raw/rebuild_everything.R"'
#
#   # Season range
#   Rscript rebuild_everything.R 2021 2025        # specific range
#   Rscript rebuild_everything.R 2024 2026        # recent seasons only
#
#   # Phase flags (skip)
#   --skip-api        Skip Phase 1 (API scraping)
#   --skip-chains     Skip Phase 2 (chains scraping)
#   --skip-pbp        Skip Phase 3 (build PBP from chains)
#   --skip-models     Skip Phase 4 (model training)
#   --skip-skills     Skip Phase 7 (stat ratings pipeline)
#   --skip-sim        Skip Phase 10 (simulation)
#
#   # Phase flags (only)
#   --models-only     Only Phase 4 (train + upload models)
#   --ratings-only    Only Phases 8-9 (ratings + predictions)
#   --predictions-only Only Phase 9 (match predictions)
#
#   # Common combos
#   --skip-api --skip-chains              # Start from PBP build (Phase 3)
#   --skip-api --skip-chains --skip-pbp   # Start from model training (Phase 4)
#
#   # Model training season override
#   --training-seasons 2021 2025   # override training range (default 2021:current-1)
#
#   Plain invocation (no flags) = full nuclear rebuild, all phases from 1.

# Phase 0: Setup ----

library(tictoc)
library(dplyr)
library(data.table)
library(mgcv)
library(xgboost)
library(cli)
library(piggyback)

torp_root <- if (file.exists("DESCRIPTION")) {
  "."
} else if (file.exists("torp/DESCRIPTION")) {
  "torp"
} else {
  stop("Cannot find torp package. Run from torpverse/ or torp/")
}

torpmodels_root <- if (file.exists("torpmodels/DESCRIPTION")) {
  "torpmodels"
} else if (file.exists("../torpmodels/DESCRIPTION")) {
  "../torpmodels"
} else {
  NULL
}

devtools::load_all(torp_root)
if (!is.null(torpmodels_root)) devtools::load_all(torpmodels_root)

# Interactive start-from prompt ----
# When sourcing in RStudio, lets you pick which phase to start from.
# Set START_FROM in the global env to skip the prompt, e.g. START_FROM <- 3

phase_menu <- c(
  "1 - API Data",
  "2 - Scrape Chains",
  "3 - Build PBP",
  "4 - Train Models",
  "5 - Rebuild PBP (new models)",
  "6 - Derived Data",
  "7 - Skills Pipeline",
  "8 - TORP Ratings",
  "9 - Match Predictions",
  "10 - Season Simulation"
)

start_from <- 1L

if (interactive()) {
  if (exists("START_FROM", envir = .GlobalEnv)) {
    start_from <- as.integer(get("START_FROM", envir = .GlobalEnv))
    cli::cli_alert_info("Using START_FROM = {start_from} from global env")
  } else {
    cli::cli_h2("Start from which phase?")
    cat(paste(phase_menu, collapse = "\n"), "\n")
    answer <- readline("Start from phase [1]: ")
    if (nzchar(answer)) start_from <- as.integer(answer)
  }
}

# Parse CLI Args ----

args <- commandArgs(trailingOnly = TRUE)
flag_args <- args[grepl("^--", args)]
pos_args <- args[!grepl("^--", args)]

# Phase flags
skip_api    <- "--skip-api" %in% flag_args
skip_chains <- "--skip-chains" %in% flag_args
skip_pbp    <- "--skip-pbp" %in% flag_args
skip_models <- "--skip-models" %in% flag_args
skip_skills <- "--skip-skills" %in% flag_args
skip_sim    <- "--skip-sim" %in% flag_args

models_only      <- "--models-only" %in% flag_args
ratings_only     <- "--ratings-only" %in% flag_args
predictions_only <- "--predictions-only" %in% flag_args
data_only        <- "--data-only" %in% flag_args  # Phases 1-3 only: re-scrape + PBP, then stop
skip_model_upload <- "--skip-model-upload" %in% flag_args  # train + saveRDS locally, HOLD the pb_upload

# "only" flags disable everything else
if (models_only) {
  # Pin start_from = 4 so Phase 4 (models) runs non-interactively (default is 6,
  # and start_from > 4 would force skip_models). Mirrors the --data-only fix.
  start_from <- 4L
  skip_api <- TRUE; skip_chains <- TRUE; skip_pbp <- TRUE
  skip_skills <- TRUE; skip_sim <- TRUE
}
if (ratings_only) {
  skip_api <- TRUE; skip_chains <- TRUE; skip_pbp <- TRUE
  skip_models <- TRUE; skip_skills <- TRUE; skip_sim <- TRUE
}
if (predictions_only) {
  skip_api <- TRUE; skip_chains <- TRUE; skip_pbp <- TRUE
  skip_models <- TRUE; skip_skills <- TRUE; skip_sim <- TRUE
}
if (data_only) {
  # Re-scrape + rebuild PBP only (Phases 1-3), publish chains/pbp, then stop —
  # deliberately NOT recomputing models/derived/ratings/predictions, so a
  # coordinate re-scrape can be verified before any retrain (see BACKFILL-PLAN.md).
  # Pin start_from = 1 so Phases 1-3 actually run non-interactively (the default
  # is 6, and start_from is otherwise only lowerable via the interactive prompt).
  start_from <- 1L
  skip_models <- TRUE; skip_skills <- TRUE; skip_sim <- TRUE
}

# Apply start_from — skip all phases before it
if (start_from > 1)  skip_api    <- TRUE
if (start_from > 2)  skip_chains <- TRUE
if (start_from > 3)  skip_pbp    <- TRUE
if (start_from > 4)  skip_models <- TRUE
# Phase 5 (rebuild PBP) is auto-gated by run_pbp && run_models
if (start_from > 7)  skip_skills <- TRUE  # Phase 7 maps to stat ratings
if (start_from > 10) skip_sim    <- TRUE

# Derive phase booleans
run_api         <- !skip_api && !models_only && !ratings_only && !predictions_only
run_chains      <- !skip_chains && !models_only && !ratings_only && !predictions_only
run_pbp         <- !skip_pbp && !models_only && !ratings_only && !predictions_only
run_models      <- !skip_models && !ratings_only && !predictions_only
run_derived     <- !models_only && !predictions_only && !data_only && start_from <= 6
run_skills      <- !skip_skills && !models_only && !predictions_only
run_ratings     <- !models_only && !predictions_only && !data_only && start_from <= 8
run_predictions <- !models_only && !data_only && start_from <= 9
run_sim         <- !skip_sim && !models_only && !ratings_only && !predictions_only

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

# Training season range (for model training only)
train_flag_idx <- which(flag_args == "--training-seasons")
if (length(train_flag_idx) > 0) {
  # Next two positional args after the flag
  remaining <- args[(which(args == "--training-seasons") + 1):length(args)]
  remaining <- remaining[!grepl("^--", remaining)]
  training_start <- as.integer(remaining[1])
  training_end   <- as.integer(remaining[2])
} else {
  training_start <- 2021L
  training_end   <- as.integer(get_afl_season()) - 1L
}
training_seasons <- training_start:training_end

cli::cli_h1("Full Torpverse Rebuild")
cli::cli_inform("Data seasons: {paste(range(seasons), collapse = '-')}")
cli::cli_inform("Training seasons: {paste(range(training_seasons), collapse = '-')}")
cli::cli_inform(paste0(
  "Phases: API=", run_api, " Chains=", run_chains, " PBP=", run_pbp,
  " Models=", run_models, " Derived=", run_derived, " StatRatings=", run_skills,
  " Ratings=", run_ratings, " Predictions=", run_predictions, " Sim=", run_sim
))

# Helpers ----

get_start_round <- function(season) {
  if (season >= 2024) 0L else 1L
}

get_max_round <- function(season) {
  if (season == get_afl_season()) get_afl_week() else 28L
}

failures <- data.frame(label = character(), error = character(),
                       time = character(), stringsAsFactors = FALSE)

safe_run <- function(label, expr) {
  tryCatch({
    expr
    cli::cli_alert_success("{label}")
  }, error = function(e) {
    failures <<- rbind(failures, data.frame(
      label = label, error = conditionMessage(e),
      time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    ))
    cli::cli_alert_danger("{label}: {conditionMessage(e)}")
  })
}

failure_log_path <- file.path(torp_root, "data-raw", "rebuild_failures.csv")

# Phase 1: API Data ----

if (run_api) {
  cli::cli_h1("Phase 1: API Data")
  tic("phase_1_api")

  for (season in seasons) {
    cli::cli_h2("Season {season}")

    safe_run(paste0("fixtures_", season), {
      fixtures <- get_afl_fixtures(season)
      list_cols <- names(fixtures)[vapply(fixtures, is.list, logical(1))]
      if (length(list_cols) > 0) {
        fixtures <- fixtures[, !names(fixtures) %in% list_cols, drop = FALSE]
      }
      save_to_release(fixtures, paste0("fixtures_", season), "fixtures-data")
      cli::cli_inform("  fixtures_{season}: {nrow(fixtures)} rows")
    })

    safe_run(paste0("results_", season), {
      results <- get_afl_results(season)
      save_to_release(results, paste0("results_", season), "results-data")
      cli::cli_inform("  results_{season}: {nrow(results)} rows")
    })

    safe_run(paste0("player_stats_", season), {
      pstats <- get_afl_player_stats(season) |>
        dplyr::select(where(~ dplyr::n_distinct(.) > 1)) |>
        janitor::clean_names()
      pstats <- .normalise_player_stats_columns(pstats)
      if (!"season" %in% names(pstats)) pstats$season <- as.integer(season)
      save_to_release(pstats, paste0("player_stats_", season), "player_stats-data")
      cli::cli_inform("  player_stats_{season}: {nrow(pstats)} rows")
    })

    safe_run(paste0("teams_", season), {
      teams <- get_afl_lineups(season)
      save_to_release(teams, paste0("teams_", season), "teams-data")
      cli::cli_inform("  teams_{season}: {nrow(teams)} rows")
    })

    safe_run(paste0("player_details_", season), {
      pd <- get_afl_player_details(season)
      save_to_release(pd, paste0("player_details_", season), "player_details-data")
      cli::cli_inform("  player_details_{season}: {nrow(pd)} rows")
    })
  }

  toc(log = TRUE)
}

# Phase 2: Scrape Chains ----

if (run_chains) {
  cli::cli_h1("Phase 2: Scrape Chains")
  tic("phase_2_chains")

  for (season in seasons) {
    cli::cli_h2("Season {season}")
    start_rd <- get_start_round(season)
    end_rd   <- get_max_round(season)
    rounds   <- start_rd:end_rd

    cli::cli_inform("Fetching chains for {season}, rounds {start_rd}-{end_rd}...")
    tic(paste0("chains_", season))

    all_chains <- purrr::map(rounds, function(rd) {
      tryCatch(get_match_chains(season, rd), error = function(e) {
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

    safe_run(paste0("chains_data_", season, "_all"), {
      save_to_release(chains, paste0("chains_data_", season, "_all"), "chains-data")
      cli::cli_inform("  chains_{season}: {nrow(chains)} rows")
    })

    toc(log = TRUE)
    rm(all_chains, chains)
    gc()
  }

  toc(log = TRUE)
}

# Phase 3: Build PBP ----

if (run_pbp) {
  cli::cli_h1("Phase 3: Build PBP")
  tic("phase_3_pbp")

  for (season in seasons) {
    cli::cli_h2("Season {season}")

    safe_run(paste0("pbp_data_", season, "_all"), {
      chains <- load_chains(seasons = season, rounds = TRUE)

      if (nrow(chains) == 0) {
        cli::cli_warn("No chains for {season} - skipping PBP")
        return(invisible(NULL))
      }

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

      rm(chains, pbp)
      gc()
    })
  }

  toc(log = TRUE)
}

# Phase 4: Train Models ----

if (run_models) {
  cli::cli_h1("Phase 4: Train Models")

  if (is.null(torpmodels_root)) {
    cli::cli_abort(paste(
      "torpmodels not found (checked torpmodels/DESCRIPTION, ../torpmodels/DESCRIPTION).",
      "Training without torpmodels/data-raw/lib/train_lib.R is no longer supported.",
      "Run from torpverse/, or pass --skip-models."
    ))
  }

  model_output_dir <- file.path(torpmodels_root, "inst", "models", "core")
  if (!dir.exists(model_output_dir)) dir.create(model_output_dir, recursive = TRUE)

  # Delegates to the SAME lib/train_lib.R the manual `train_models.R` CLI
  # uses -- one canonical trainer, no hand-inlined hyperparameters/constraints
  # to drift out of sync (this was F1: a 15-entry constraint string against
  # 18 WP features, eta 0.1 vs canonical 0.025). Accepted cost: WP training
  # here now runs full CV-EP (5 extra fold-EP trainings) instead of reusing
  # the in-memory EP model from 4a -- slower, correct, identical to the
  # manual path.
  cli::cli_h2("Phase 4: Core Model Training (EP/WP/Shot)")
  tic("phase_4_core_models")

  source(file.path(torpmodels_root, "data-raw", "lib", "train_lib.R"))

  safe_run("core_model_training", {
    train_core_models(
      c("ep", "wp", "shot"),
      seasons = training_seasons,
      upload = !skip_model_upload,
      output_dir = model_output_dir
    )
  })

  toc(log = TRUE)

  # Match GAMs are NOT trained here. run_predictions_pipeline() (Phase 9) is
  # the sole match-GAM publisher -- the former Phase 4d was dead on arrival
  # (it injected HOLDOUT_SEASON into train_match_models.R, which no longer
  # reads that variable at all) and redundant besides.

  # Clear model cache so subsequent phases pick up fresh models
  clear_model_cache()
  cli::cli_alert_info("Model cache cleared")
}

# Phase 5: Rebuild PBP with New Models ----

if (run_pbp && run_models) {
  cli::cli_h1("Phase 5: Rebuild PBP with New Models")
  tic("phase_5_pbp_rebuild")

  for (season in seasons) {
    cli::cli_h2("Season {season} (re-processing PBP)")

    safe_run(paste0("pbp_rebuild_", season), {
      chains <- load_chains(seasons = season, rounds = TRUE)

      if (nrow(chains) == 0) {
        cli::cli_warn("No chains for {season} - skipping rebuild")
        return(invisible(NULL))
      }

      pbp <- chains |>
        clean_pbp() |>
        clean_model_data_epv() |>
        clean_shots_data() |>
        add_shot_vars() |>
        add_epv_vars() |>
        clean_model_data_wp() |>
        add_wp_vars()

      save_to_release(pbp, paste0("pbp_data_", season, "_all"), "pbp-data")
      cli::cli_inform("  pbp_{season} (rebuilt): {nrow(pbp)} rows")

      rm(chains, pbp)
      gc()
    })
  }

  toc(log = TRUE)
} else if (run_pbp && !run_models) {
  cli::cli_alert_info("Skipping Phase 5 (PBP rebuild) - no new models trained")
}

# Phase 6: Derived Data ----

if (run_derived) {
  cli::cli_h1("Phase 6: Derived Data")
  tic("phase_6_derived")

  for (season in seasons) {
    cli::cli_h2("Season {season}")
    start_rd <- get_start_round(season)
    end_rd   <- get_max_round(season)

    safe_run(paste0("xg_data_", season), {
      xg_df <- calculate_match_xgs(season, start_rd:end_rd)
      save_to_release(xg_df, paste0("xg_data_", season), "xg-data")
      cli::cli_inform("  xg_{season}: {nrow(xg_df)} rows")
    })

    safe_run(paste0("player_game_", season), {
      pbp <- load_pbp(season, rounds = TRUE)
      chains <- load_chains(season, rounds = TRUE)
      pstats <- load_player_stats(season)
      teams_data <- load_teams(season)
      pgd <- create_player_game_data(pbp, pstats, teams_data, chains = chains)
      save_to_release(pgd, paste0("player_game_", season), "player_game-data")
      cli::cli_inform("  player_game_{season}: {nrow(pgd)} rows")
    })

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
      cli::cli_inform("  ep_wp_chart_{season}: {nrow(chart_data)} rows")
    })

    gc()
  }

  toc(log = TRUE)
}

# Phase 7: Stat Ratings Pipeline ----

if (run_skills) {
  cli::cli_h1("Phase 7: Stat Ratings Pipeline")
  tic("phase_7_stat_ratings")

  stat_ratings_dir <- file.path(torp_root, "data-raw", "06-stat-ratings")

  stat_ratings_scripts <- c(
    "01_compute_match_stats.R",
    "02_optimize_stat_rating_params.R",
    "03_estimate_stat_ratings.R",
    "04_export_stat_ratings.R",
    "05_compare_psr_models.R",
    "06_train_psr_model.R"
  )

  for (script in stat_ratings_scripts) {
    script_path <- file.path(stat_ratings_dir, script)
    if (!file.exists(script_path)) {
      cli::cli_warn("Stat ratings script not found: {script}")
      next
    }
    safe_run(paste0("stat_ratings_", script), {
      cli::cli_inform("Running {script}...")
      source(script_path, local = new.env(parent = globalenv()))
    })
  }

  toc(log = TRUE)
}

# Phase 8: TORP Ratings ----

if (run_ratings) {
  cli::cli_h1("Phase 8: TORP Ratings")
  tic("phase_8_ratings")

  # Set config in global env (run_ratings_pipeline.R checks .GlobalEnv)
  SEASONS <<- TRUE
  REFRESH_UPSTREAM <<- FALSE  # Phase 1 already handled this
  REBUILD_PLAYER_GAME <<- TRUE  # Phase 6 might have already handled this but set as TRUE to be safe
  REBUILD_ALL_RATINGS <<- TRUE

  ratings_script <- file.path(torp_root, "data-raw", "03-ratings", "run_ratings_pipeline.R")
  if (file.exists(ratings_script)) {
    safe_run("torp_ratings_pipeline", {
      source(ratings_script)
    })
  } else {
    cli::cli_alert_danger("Ratings pipeline script not found at {ratings_script}")
  }

  toc(log = TRUE)
}

# Phase 9: Match Predictions ----

if (run_predictions) {
  cli::cli_h1("Phase 9: Match Predictions")
  tic("phase_9_predictions")

  safe_run("match_predictions", {
    # Reload torp to pick up any new ratings data
    devtools::load_all(torp_root)

    result <- run_predictions_pipeline(weeks = "all")
    cli::cli_inform("Predictions: {nrow(result$predictions)} rows")
  })

  toc(log = TRUE)
}

# Phase 10: Season Simulation ----

if (run_sim) {
  cli::cli_h1("Phase 10: Season Simulation")
  tic("phase_10_simulation")

  safe_run("season_simulation", {
    devtools::load_all(torp_root)

    current_season <- as.integer(get_afl_season())
    n_cores <- pmax(parallel::detectCores(logical = FALSE) - 2, 1)

    injuries <- tryCatch(
      get_all_injuries(current_season, scrape = FALSE),
      error = function(e) {
        cli::cli_warn("Could not load injuries: {conditionMessage(e)}")
        NULL
      }
    )

    sim_result <- simulate_afl_season(
      current_season, n_sims = 3000, seed = 420,
      verbose = TRUE, n_cores = n_cores, injuries = injuries
    )

    summary_sim <- summarise_simulations(sim_result)
    cli::cli_inform("Simulation complete: {nrow(summary_sim)} teams")
    print(summary_sim[, .(team, avg_wins, top_4_pct, top_8_pct, won_gf_pct)])
  })

  toc(log = TRUE)
}

# Phase 11: Summary ----

cli::cli_h1("Rebuild Complete")

if (nrow(failures) > 0) {
  cli::cli_alert_warning("{nrow(failures)} failure{?s}:")
  for (i in seq_len(nrow(failures))) {
    cli::cli_alert_danger("  {failures$label[i]}: {failures$error[i]}")
  }
  write.csv(failures, failure_log_path, row.names = FALSE)
  cli::cli_inform("Failure log: {failure_log_path}")
} else {
  cli::cli_alert_success("All phases completed successfully!")
  if (file.exists(failure_log_path)) file.remove(failure_log_path)
}

cli::cli_h2("Timing")
tic.log(format = TRUE) |> unlist() |> cat(sep = "\n")
tic.clearlog()
