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
#   --skip-chains     Skip Phase 2+4 (chains/PBP)
#   --skip-models     Skip Phase 3 (model training)
#   --skip-skills     Skip Phase 6 (skills pipeline)
#   --skip-sim        Skip Phase 9 (simulation)
#
#   # Phase flags (only)
#   --models-only     Only Phase 3 (train + upload models)
#   --ratings-only    Only Phases 7-8 (ratings + predictions)
#   --predictions-only Only Phase 8 (match predictions)
#
#   # Model training season override
#   --training-seasons 2021 2025   # override training range (default 2021:current-1)

# Phase 0: Setup ----

library(dplyr)
library(data.table)
library(mgcv)
library(xgboost)
library(tictoc)
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

# Parse CLI Args ----

args <- commandArgs(trailingOnly = TRUE)
flag_args <- args[grepl("^--", args)]
pos_args <- args[!grepl("^--", args)]

# Phase flags
skip_api    <- "--skip-api" %in% flag_args
skip_chains <- "--skip-chains" %in% flag_args
skip_models <- "--skip-models" %in% flag_args
skip_skills <- "--skip-skills" %in% flag_args
skip_sim    <- "--skip-sim" %in% flag_args

models_only      <- "--models-only" %in% flag_args
ratings_only     <- "--ratings-only" %in% flag_args
predictions_only <- "--predictions-only" %in% flag_args

# "only" flags disable everything else
if (models_only) {
  skip_api <- TRUE; skip_chains <- TRUE; skip_skills <- TRUE; skip_sim <- TRUE
}
if (ratings_only) {
  skip_api <- TRUE; skip_chains <- TRUE; skip_models <- TRUE; skip_skills <- TRUE; skip_sim <- TRUE
}
if (predictions_only) {
  skip_api <- TRUE; skip_chains <- TRUE; skip_models <- TRUE
  skip_skills <- TRUE; skip_sim <- TRUE
}

# Derive phase booleans
run_api         <- !skip_api && !models_only && !ratings_only && !predictions_only
run_chains      <- !skip_chains && !models_only && !ratings_only && !predictions_only
run_models      <- !skip_models && !ratings_only && !predictions_only
run_derived     <- !models_only && !predictions_only
run_skills      <- !skip_skills && !models_only && !predictions_only
run_ratings     <- !models_only && !predictions_only
run_predictions <- !models_only
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
  "Phases: API=", run_api, " Chains=", run_chains, " Models=", run_models,
  " Derived=", run_derived, " Skills=", run_skills, " Ratings=", run_ratings,
  " Predictions=", run_predictions, " Sim=", run_sim
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

# Phase 2: Chains + Initial PBP ----

if (run_chains) {
  cli::cli_h1("Phase 2: Chains + Initial PBP")
  tic("phase_2_chains_pbp")

  for (season in seasons) {
    cli::cli_h2("Season {season}")
    start_rd <- get_start_round(season)
    end_rd   <- get_max_round(season)
    rounds   <- start_rd:end_rd

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

    safe_run(paste0("chains_data_", season, "_all"), {
      save_to_release(chains, paste0("chains_data_", season, "_all"), "chains-data")
      cli::cli_inform("  chains_{season}: {nrow(chains)} rows")
    })

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
    rm(all_chains, chains)
    gc()
  }

  toc(log = TRUE)
}

# Phase 3: Train Models ----

if (run_models) {
  cli::cli_h1("Phase 3: Train Models")

  model_output_dir <- if (!is.null(torpmodels_root)) {
    file.path(torpmodels_root, "inst", "models", "core")
  } else {
    file.path(torp_root, "data-raw", "models")
  }
  if (!dir.exists(model_output_dir)) dir.create(model_output_dir, recursive = TRUE)

  # --- 3a: EP Model ---
  cli::cli_h2("Phase 3a: EP Model")
  tic("phase_3a_ep")

  safe_run("ep_model_training", {
    cli::cli_inform("Loading chains {training_start}-{training_end}...")
    chains_train <- load_chains(seasons = training_seasons, rounds = TRUE)
    cli::cli_inform("Chains: {nrow(chains_train)} rows")

    pbp_train <- clean_pbp(chains_train)
    model_data_epv <- clean_model_data_epv(pbp_train)
    cli::cli_inform("EP model data: {nrow(model_data_epv)} rows")

    params_ep <- list(
      booster = "gbtree", objective = "multi:softprob",
      eval_metric = "mlogloss", tree_method = "hist",
      num_class = 5, eta = 0.1, gamma = 0,
      subsample = 0.85, colsample_bytree = 0.85,
      max_depth = 6, min_child_weight = 25
    )

    epv_vars <- model_data_epv |> select_epv_model_vars()
    X_train <- stats::model.matrix(~ . + 0, data = epv_vars, na.action = na.pass)
    y_train <- model_data_epv$label_ep
    full_train <- xgboost::xgb.DMatrix(data = X_train, label = y_train)

    # Match-grouped CV folds
    match_ids <- unique(model_data_epv$torp_match_id)
    set.seed(1234)
    match_folds <- sample(rep(1:5, length.out = length(match_ids)))
    names(match_folds) <- match_ids
    row_folds <- match_folds[model_data_epv$torp_match_id]
    folds <- lapply(1:5, function(k) which(row_folds == k))

    cli::cli_inform("Running 5-fold CV...")
    set.seed(1234)
    cv_result <- xgboost::xgb.cv(
      params = params_ep, data = full_train, nrounds = 500,
      folds = folds, early_stopping_rounds = 20,
      print_every_n = 20, verbose = 1
    )

    optimal_rounds_ep <- cv_result$best_iteration
    if (is.null(optimal_rounds_ep) || length(optimal_rounds_ep) == 0) {
      optimal_rounds_ep <- which.min(cv_result$evaluation_log$test_mlogloss_mean)
    }
    cli::cli_inform("EP optimal rounds: {optimal_rounds_ep}")

    set.seed(1234)
    ep_model <- xgboost::xgb.train(
      params = params_ep, data = full_train,
      nrounds = optimal_rounds_ep, print_every_n = 10
    )

    ep_path <- file.path(model_output_dir, "ep_model.rds")
    saveRDS(ep_model, ep_path)
    piggyback::pb_upload(ep_path, repo = "peteowen1/torpmodels", tag = "core-models")

    # Keep model_data_epv and ep_model in memory — WP training needs them
    rm(full_train, epv_vars, X_train, y_train, cv_result, chains_train, pbp_train)
    gc()
  })

  toc(log = TRUE)

  # --- 3b: WP Model ---
  cli::cli_h2("Phase 3b: WP Model")
  tic("phase_3b_wp")

  safe_run("wp_model_training", {
    # Custom EP predictor using in-memory model
    add_epv_custom <- function(df, ep_mdl) {
      epv_vars <- df |> select_epv_model_vars()
      epv_matrix <- stats::model.matrix(~ . + 0, data = epv_vars)
      preds_raw <- predict(ep_mdl, xgboost::xgb.DMatrix(data = epv_matrix))

      if (is.matrix(preds_raw)) {
        preds_matrix <- preds_raw
      } else {
        preds_matrix <- matrix(preds_raw, ncol = 5, byrow = TRUE)
      }

      epv_lookup <- c(-6, -1, 1, 6, 0)
      df$exp_pts <- as.vector(preds_matrix %*% epv_lookup)
      df$opp_goal <- preds_matrix[, 1]
      df$opp_behind <- preds_matrix[, 2]
      df$behind <- preds_matrix[, 3]
      df$goal <- preds_matrix[, 4]
      df$no_score <- preds_matrix[, 5]
      df
    }

    model_data_wp <- model_data_epv |>
      clean_shots_data() |>
      add_shot_vars() |>
      add_epv_custom(ep_model) |>
      clean_model_data_wp()
    cli::cli_inform("WP model data: {nrow(model_data_wp)} rows")

    params_wp <- list(
      booster = "gbtree", objective = "binary:logistic",
      eval_metric = "logloss", tree_method = "hist",
      eta = 0.1, gamma = 0, subsample = 0.85,
      colsample_bytree = 0.85, max_depth = 6, min_child_weight = 1,
      monotone_constraints = "(0,0,0,1,1,1,0,1,0,0,0,0,0,0,0)"
    )

    wp_vars <- model_data_wp |> select_wp_model_vars()
    X_train_wp <- stats::model.matrix(~ . + 0, data = wp_vars, na.action = na.pass)
    y_train_wp <- model_data_wp$label_wp
    full_train_wp <- xgboost::xgb.DMatrix(data = X_train_wp, label = y_train_wp)

    # Match-grouped CV
    match_ids_wp <- unique(model_data_wp$torp_match_id)
    set.seed(1234)
    match_folds_wp <- sample(rep(1:5, length.out = length(match_ids_wp)))
    names(match_folds_wp) <- match_ids_wp
    row_folds_wp <- match_folds_wp[model_data_wp$torp_match_id]
    folds_wp <- lapply(1:5, function(k) which(row_folds_wp == k))

    cli::cli_inform("Running 5-fold CV...")
    set.seed(1234)
    cv_result_wp <- xgboost::xgb.cv(
      params = params_wp, data = full_train_wp, nrounds = 500,
      folds = folds_wp, early_stopping_rounds = 20,
      print_every_n = 20, verbose = 1
    )

    optimal_rounds_wp <- cv_result_wp$best_iteration
    if (is.null(optimal_rounds_wp) || length(optimal_rounds_wp) == 0) {
      optimal_rounds_wp <- which.min(cv_result_wp$evaluation_log$test_logloss_mean)
    }
    cli::cli_inform("WP optimal rounds: {optimal_rounds_wp}")

    set.seed(1234)
    wp_model <- xgboost::xgb.train(
      params = params_wp, data = full_train_wp,
      nrounds = optimal_rounds_wp, print_every_n = 10
    )

    wp_path <- file.path(model_output_dir, "wp_model.rds")
    saveRDS(wp_model, wp_path)
    piggyback::pb_upload(wp_path, repo = "peteowen1/torpmodels", tag = "core-models")

    rm(full_train_wp, wp_vars, X_train_wp, y_train_wp, cv_result_wp,
       model_data_epv, model_data_wp)
    gc()
  })

  toc(log = TRUE)

  # --- 3c: Shot Model ---
  cli::cli_h2("Phase 3c: Shot Model")
  tic("phase_3c_shot")

  safe_run("shot_model_training", {
    shots_prep <- load_pbp(seasons = training_seasons, rounds = TRUE)

    shots <- shots_prep |>
      dplyr::filter(!is.na(shot_at_goal), x > 0, goal_x < 65, abs_y < 45) |>
      dplyr::mutate(
        scored_shot = ifelse(!is.na(points_shot), 1, 0),
        shot_cat = dplyr::case_when(
          is.na(points_shot) ~ 1,
          points_shot == 1 ~ 2,
          points_shot == 6 ~ 3
        )
      )

    shots$player_id_shot <- forcats::fct_lump_min(shots$player_id, 10, other_level = "Other")

    player_name_mapping <- shots |>
      dplyr::group_by(player_id_shot = player_id) |>
      dplyr::summarise(player_name_shot = dplyr::last(player_name))

    shot_player_df <- tibble::tibble(
      player_id_shot = levels(shots$player_id_shot)
    ) |>
      dplyr::left_join(player_name_mapping)

    cli::cli_inform("Training shot model ({nrow(shots)} shots)...")
    shot_ocat_mdl <- mgcv::bam(
      shot_cat ~
        ti(goal_x, abs_y, by = phase_of_play, bs = "ts")
        + ti(goal_x, abs_y, bs = "ts")
        + s(goal_x, bs = "ts")
        + s(abs_y, bs = "ts")
        + ti(lag_goal_x, lag_y)
        + s(lag_goal_x, bs = "ts")
        + s(lag_y, bs = "ts")
        + s(play_type, bs = "re")
        + s(phase_of_play, bs = "re")
        + s(player_position_fac, bs = "re")
        + s(player_id_shot, bs = "re"),
      data = shots,
      family = ocat(R = 3),
      nthreads = 4,
      select = TRUE,
      discrete = TRUE,
      drop.unused.levels = FALSE
    )

    shot_path <- file.path(model_output_dir, "shot_ocat_mdl.rds")
    saveRDS(shot_ocat_mdl, shot_path)
    piggyback::pb_upload(shot_path, repo = "peteowen1/torpmodels", tag = "core-models")

    player_path <- file.path(model_output_dir, "shot_player_df.rds")
    saveRDS(shot_player_df, player_path)
    piggyback::pb_upload(player_path, repo = "peteowen1/torpmodels", tag = "core-models")

    rm(shots_prep, shots, shot_ocat_mdl, shot_player_df)
    gc()
  })

  toc(log = TRUE)

  # --- 3d: Match GAMs ---
  cli::cli_h2("Phase 3d: Match GAMs")
  tic("phase_3d_match_gams")

  match_model_script <- if (!is.null(torpmodels_root)) {
    file.path(torpmodels_root, "data-raw", "04-match-model", "train_match_models.R")
  } else {
    NULL
  }

  if (!is.null(match_model_script) && file.exists(match_model_script)) {
    safe_run("match_gam_training", {
      # Source with production settings (no holdout, upload to GitHub)
      match_env <- new.env(parent = globalenv())
      match_env$HOLDOUT_SEASON <- Inf
      match_env$UPLOAD_TO_GITHUB <- TRUE
      source(match_model_script, local = match_env)
    })
  } else {
    cli::cli_alert_info("Match model script not found - match GAMs will train in Phase 8 via run_predictions_pipeline()")
  }

  toc(log = TRUE)

  # Clear model cache so subsequent phases pick up fresh models
  clear_model_cache()
  cli::cli_alert_info("Model cache cleared")
}

# Phase 4: Rebuild PBP with New Models ----

if (run_chains && run_models) {
  cli::cli_h1("Phase 4: Rebuild PBP with New Models")
  tic("phase_4_pbp_rebuild")

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
} else if (run_chains && !run_models) {
  cli::cli_alert_info("Skipping Phase 4 (PBP rebuild) - no new models trained")
}

# Phase 5: Derived Data ----

if (run_derived) {
  cli::cli_h1("Phase 5: Derived Data")
  tic("phase_5_derived")

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
      pstats <- load_player_stats(season)
      teams_data <- load_teams(season)
      pgd <- create_player_game_data(pbp, pstats, teams_data)
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

# Phase 6: Skills Pipeline ----

if (run_skills) {
  cli::cli_h1("Phase 6: Skills Pipeline")
  tic("phase_6_skills")

  skills_dir <- file.path(torp_root, "data-raw", "06-skills")

  skills_scripts <- c(
    "01_compute_match_stats.R",
    "02_optimize_skill_params.R",
    "03_estimate_skills.R",
    "04_export_skills.R",
    "05_train_psr_model.R",
    "06_train_psr_v2.R"
  )

  for (script in skills_scripts) {
    script_path <- file.path(skills_dir, script)
    if (!file.exists(script_path)) {
      cli::cli_warn("Skills script not found: {script}")
      next
    }
    safe_run(paste0("skills_", script), {
      cli::cli_inform("Running {script}...")
      source(script_path, local = new.env(parent = globalenv()))
    })
  }

  toc(log = TRUE)
}

# Phase 7: TORP Ratings ----

if (run_ratings) {
  cli::cli_h1("Phase 7: TORP Ratings")
  tic("phase_7_ratings")

  # Set config in global env (run_ratings_pipeline.R checks .GlobalEnv)
  SEASONS <<- TRUE
  REFRESH_UPSTREAM <<- FALSE  # Phase 1 already handled this
  REBUILD_PLAYER_GAME <<- FALSE  # Phase 5 already handled this
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

# Phase 8: Match Predictions ----

if (run_predictions) {
  cli::cli_h1("Phase 8: Match Predictions")
  tic("phase_8_predictions")

  safe_run("match_predictions", {
    # Reload torp to pick up any new ratings data
    devtools::load_all(torp_root)

    result <- run_predictions_pipeline(weeks = "all")
    cli::cli_inform("Predictions: {nrow(result$predictions)} rows")
  })

  toc(log = TRUE)
}

# Phase 9: Season Simulation ----

if (run_sim) {
  cli::cli_h1("Phase 9: Season Simulation")
  tic("phase_9_simulation")

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

# Phase 10: Summary ----

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
