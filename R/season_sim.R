# AFL Season Monte Carlo Simulation
# =================================
# End-to-end driver for simulating complete AFL seasons:
#   prepare_sim_data()       — load fixtures + ratings + (optional) injuries
#   .extract_team_residuals() — pull team-quality residuals from the match GAM
#   simulate_afl_season()    — main Monte Carlo entry point
#   process_games_dt()       — per-round scoring/rating update
#   summarise_simulations()  — aggregate ladder + finals across runs
#   print.torp_sim_results   — pretty print method
#
# Ladder calculation lives in R/ladder.R; finals bracket in R/finals_sim.R.

# --------------------------------------------------------------------------
# Data preparation
# --------------------------------------------------------------------------

#' Prepare simulation data for a season
#'
#' Loads fixtures, team ratings, and (optionally) predictions, then separates
#' played from unplayed games and formats everything for the simulation loop.
#'
#' @param season Numeric season year (e.g. 2026).
#' @param team_ratings Optional data.table/data.frame with columns `team` and
#'   `torp`. If NULL, loads via [load_team_ratings()].
#' @param fixtures Optional fixture data.frame (AFL API format). If NULL, loads
#'   via [load_fixtures()].
#' @param predictions Optional predictions data.frame with `pred_xtotal`. If
#'   NULL, attempts to load via [load_predictions()].
#' @param injuries Optional injury data.frame from [get_all_injuries()]. When
#'   provided, team ratings are built from player-level TORP with injured
#'   players excluded and a lighter discount ([INJURY_KNOWN_DISCOUNT]) applied.
#' @param team_residuals Optional data.frame with columns `team`,
#'   `residual_mean`, `residual_se`. Team-level quality residuals from the
#'   match GAM random effects, capturing systematic over/under-performance
#'   not explained by player TORP. If `"auto"`, attempts to extract from the
#'   match GAM model.
#' @param models Optional named list of GAM models from
#'   `run_predictions_pipeline()$models`. Passed to [.extract_team_residuals()]
#'   to use fresh models instead of the stored torpmodels version.
#' @return A list with elements `sim_teams`, `sim_games`, `played_games`.
#' @keywords internal
prepare_sim_data <- function(season, team_ratings = NULL, fixtures = NULL,
                             predictions = NULL, injuries = NULL,
                             team_residuals = NULL, models = NULL) {

 # --- Fixtures ---
  if (is.null(fixtures)) {
    fixtures <- load_fixtures(seasons = season)
  }
  fix_dt <- data.table::as.data.table(fixtures)

  # Filter to target season (normalised fixtures use canonical `season` column)
  if ("season" %in% names(fix_dt)) {
    fix_dt <- fix_dt[get("season") == season]
  }

  # Build sim_games from canonical fixture columns
  # Fixtures are normalised at load time via .normalise_fixture_columns(),
  # but keep minimal fallbacks for directly-passed user data
  rnd_col  <- .resolve_col(fix_dt, c("round_number", "roundnum"))
  ht_col   <- .resolve_col(fix_dt, c("home_team_name", "home_team"))
  at_col   <- .resolve_col(fix_dt, c("away_team_name", "away_team"))
  hs_col   <- .resolve_col(fix_dt, c("home_score", "home_points"))
  as_col   <- .resolve_col(fix_dt, c("away_score", "away_points"))

  if (is.null(rnd_col) || is.null(ht_col) || is.null(at_col)) {
    cli::cli_abort("Could not find required fixture columns (round_number, home_team_name, away_team_name).")
  }

  sim_games <- data.table::data.table(
    roundnum  = as.integer(fix_dt[[rnd_col]]),
    home_team = as.character(fix_dt[[ht_col]]),
    away_team = as.character(fix_dt[[at_col]]),
    home_score = if (!is.null(hs_col)) as.integer(fix_dt[[hs_col]]) else NA_integer_,
    away_score = if (!is.null(as_col)) as.integer(fix_dt[[as_col]]) else NA_integer_
  )

  # Keep only regular season rounds (exclude finals)
  max_round <- AFL_REGULAR_SEASON_ROUNDS[as.character(season)]
  if (is.na(max_round)) max_round <- AFL_MAX_REGULAR_ROUNDS
  sim_games <- sim_games[roundnum <= max_round]

  # Determine played vs unplayed
  sim_games[, result := data.table::fifelse(
    !is.na(home_score) & !is.na(away_score),
    home_score - away_score,
    NA_integer_
  )]

  # Add columns expected by simulate_season()
  sim_games[, `:=`(
    torp_home_round = NA_real_,
    torp_away_round = NA_real_
  )]

  played_games <- sim_games[!is.na(result)]

  # --- Team Ratings ---
  # When injuries are provided, build from player-level ratings so we can

  # exclude specific injured players. Otherwise use pre-computed team ratings.
  use_injury_aware <- !is.null(injuries) && nrow(injuries) > 0

  if (is.null(team_ratings)) {
    sim_teams <- NULL

    # When injuries provided, skip pre-computed team ratings and go straight
    # to player-level ratings so injured players can be excluded
    if (!use_injury_aware) {
      # Try pre-computed team ratings first
      tr <- tryCatch(load_team_ratings(), error = function(e) NULL)
      if (!is.null(tr)) {
        tr_dt <- data.table::as.data.table(tr)
        if ("season" %in% names(tr_dt) && "round" %in% names(tr_dt)) {
          target_season <- season
          tr_dt <- tr_dt[season == target_season]
          if (nrow(tr_dt) > 0) {
            max_round <- max(tr_dt$round)
            tr_dt <- tr_dt[round == max_round]
            torp_col <- if ("team_torp" %in% names(tr_dt)) "team_torp" else "torp"
            team_col <- if ("team" %in% names(tr_dt)) "team" else "team_name"
            sim_teams <- data.table::data.table(
              team = as.character(tr_dt[[team_col]]),
              torp = as.numeric(tr_dt[[torp_col]])
            )
            sim_teams <- sim_teams[, .(torp = mean(torp)), by = team]
          }
        }
      }
    }

    # Build from player ratings (top N per team, with or without injury exclusion)
    if (is.null(sim_teams) || nrow(sim_teams) == 0) {
      discount <- if (use_injury_aware) INJURY_KNOWN_DISCOUNT else SIM_INJURY_DISCOUNT
      label <- if (use_injury_aware) "injury-aware" else "standard"
      cli::cli_alert_info("Building {label} team ratings from player TORP (top {SIM_TOP_N_PLAYERS} per team)")

      # Injury-aware path: use torp_ratings() for live computation with full
      # TORP blend (epr + psr). Standard path: download pre-computed release.
      if (use_injury_aware) {
        pr <- tryCatch(torp_ratings(season, injuries = injuries), error = function(e) NULL)
      } else {
        pr <- tryCatch(load_torp_ratings(), error = function(e) NULL)
      }
      if (is.null(pr)) {
        cli::cli_abort("Could not load team or player ratings. Provide them via the {.arg team_ratings} argument.")
      }
      pr_dt <- data.table::as.data.table(pr)
      # Take latest available ratings
      if ("season" %in% names(pr_dt) && "round" %in% names(pr_dt)) {
        max_season <- max(pr_dt$season)
        pr_dt <- pr_dt[season == max_season]
        max_round <- max(pr_dt$round)
        pr_dt <- pr_dt[round == max_round]
      }

      # Normalise rating column name: epr -> torp for consistency
      if ("epr" %in% names(pr_dt) && !"torp" %in% names(pr_dt)) {
        data.table::setnames(pr_dt, "epr", "torp")
      }

      # Exclude injured players when injury data is provided
      # Keep a copy of full ratings before exclusion for injury schedule
      pr_dt_full <- if (use_injury_aware) data.table::copy(pr_dt) else NULL

      if (use_injury_aware && "player_name" %in% names(pr_dt)) {
        pr_dt[, player_norm := norm_name(player_name)]
        injured_norms <- injuries$player_norm
        n_before <- nrow(pr_dt)
        pr_dt <- pr_dt[!player_norm %in% injured_norms]
        n_excluded <- n_before - nrow(pr_dt)
        if (n_excluded > 0) {
          cli::cli_alert_info("Excluded {n_excluded} injured player{?s} from team ratings")
        }
        pr_dt[, player_norm := NULL]
      }

      # pred_tog-weighted aggregation: torp is now per-80 (centered around 0),
      # so negative values are intentional (below-average players). No pmax guard
      # needed — TOG weighting handles contribution scaling.
      if ("pred_tog" %in% names(pr_dt)) {
        pr_dt[, tog_wt := {
          team_sum <- sum(pred_tog, na.rm = TRUE)
          n_pl <- .N
          if (team_sum > 0) pred_tog * 18 / team_sum else rep(18 / n_pl, n_pl)
        }, by = team]
        sim_teams <- pr_dt[, .(
          torp = sum(torp * tog_wt, na.rm = TRUE) * discount
        ), by = team]
      } else {
        pr_dt[, tm_rnk := rank(-torp), by = team]
        sim_teams <- pr_dt[tm_rnk <= SIM_TOP_N_PLAYERS, .(
          torp = sum(pmax(torp, 0), na.rm = TRUE) * discount
        ), by = team]
      }
    }
  } else {
    sim_teams <- data.table::as.data.table(team_ratings)[, .(team, torp)]
  }

  # --- Injury Return Schedule ---
  # Build a schedule of TORP boosts for when injured players return
  injury_schedule <- NULL
  if (use_injury_aware && exists("pr_dt_full") && !is.null(pr_dt_full)) {
    # Determine current round from played games
    current_round <- if (nrow(played_games) > 0) max(played_games$roundnum) else 1L
    inj_with_round <- injuries
    inj_with_round$return_round <- parse_return_round(
      injuries$estimated_return, season, current_round
    )
    injury_schedule <- build_injury_schedule(inj_with_round, pr_dt_full)
    if (nrow(injury_schedule) > 0) {
      n_returning <- nrow(injury_schedule)
      cli::cli_alert_info("Injury schedule: {n_returning} team-round return boost{?s} computed")
    }
    injuries <- inj_with_round
  }

  # --- Predictions (optional) ---
  if (is.null(predictions)) {
    predictions <- tryCatch(
      load_predictions(seasons = season, rounds = TRUE),
      error = function(e) {
        cli::cli_warn("Could not load predictions: {conditionMessage(e)} -- simulations will use team ratings only")
        NULL
      }
    )
  }

  if (!is.null(predictions)) {
    pred_dt <- data.table::as.data.table(predictions)
    # Find matching columns for join
    pred_rnd <- .resolve_col(pred_dt, c("round_number", "roundnum", "round", "week"))
    pred_ht  <- .resolve_col(pred_dt, c("home_team", "home_team_name"))

    if (!is.null(pred_rnd) && !is.null(pred_ht) && "pred_xtotal" %in% names(pred_dt)) {
      sim_games[pred_dt,
        pred_xtotal := i.pred_xtotal,
        on = stats::setNames(c(pred_rnd, pred_ht), c("roundnum", "home_team"))
      ]
    }
  }

  # --- GF Venue Familiarity ---
  # Compute each team's proportion of games at the GF venue (MCG)
  venue_col <- .resolve_col(fix_dt, c("venue_name", "venue"))
  if (!is.null(venue_col)) {
    venue_dt <- data.table::data.table(
      home_team = as.character(fix_dt[[ht_col]]),
      away_team = as.character(fix_dt[[at_col]]),
      venue     = torp_replace_venues(as.character(fix_dt[[venue_col]]))
    )
    # Pivot to team-level rows
    home_rows <- venue_dt[, .(team = home_team, venue)]
    away_rows <- venue_dt[, .(team = away_team, venue)]
    all_rows  <- data.table::rbindlist(list(home_rows, away_rows))

    gf_venue <- SIM_GF_VENUE
    gf_familiarity <- all_rows[, .(
      gf_familiarity = sum(venue == gf_venue) / .N
    ), by = team]
  } else {
    gf_familiarity <- data.table::data.table(
      team = sim_teams$team,
      gf_familiarity = 0
    )
  }

  # --- Team Residuals (GAM random effects) ---
  if (identical(team_residuals, "auto")) {
    team_residuals <- .extract_team_residuals(models = models, season = season)
  }
  if (!is.null(team_residuals)) {
    tr_dt <- data.table::as.data.table(team_residuals)[, .(team, residual_mean, residual_se)]
    sim_teams <- merge(sim_teams, tr_dt, by = "team", all.x = TRUE)
    # Teams not in GAM (expansion teams etc.) get 0 residual with league-average SE
    avg_se <- mean(tr_dt$residual_se, na.rm = TRUE)
    sim_teams[is.na(residual_mean), `:=`(residual_mean = 0, residual_se = avg_se)]
  } else {
    sim_teams[, `:=`(residual_mean = 0, residual_se = 0)]
  }

  list(
    sim_teams       = sim_teams,
    sim_games       = sim_games,
    played_games    = played_games,
    gf_familiarity  = gf_familiarity,
    injury_schedule = injury_schedule,
    injuries        = injuries
  )
}


#' Extract team-level quality residuals from match GAM
#'
#' Extracts both cross-season `team_name` and within-season `team_name_season`
#' random effects from the `xscore_diff` GAM and combines them. These capture
#' systematic team over/under-performance not explained by player TORP ratings
#' (e.g. coaching, team system, season-specific form).
#' Team names are standardised via [torp_replace_teams()] to ensure
#' consistent merging with simulation team names.
#'
#' @param models Optional named list of GAM models (as returned by
#'   `run_predictions_pipeline()$models`). When provided, uses the
#'   `xscore_diff` model directly. Otherwise loads from torpmodels.
#' @param season Integer season year used to filter `team_name_season` effects
#'   to the relevant season. Defaults to the current year.
#' @return A data.table with columns `team`, `residual_mean`, `residual_se`,
#'   or NULL if extraction fails.
#' @keywords internal
.extract_team_residuals <- function(models = NULL, season = NULL) {
  if (is.null(season)) season <- as.integer(format(Sys.Date(), "%Y"))

  if (!is.null(models) && !is.null(models[["xscore_diff"]])) {
    xsd <- models[["xscore_diff"]]
  } else {
    match_gams <- tryCatch(
      torpmodels::load_torp_model("match_gams"),
      error = function(e) {
        cli::cli_warn("Could not load match GAMs: {conditionMessage(e)}")
        NULL
      }
    )
    if (is.null(match_gams) || is.null(match_gams[["xscore_diff"]])) return(NULL)
    xsd <- match_gams[["xscore_diff"]]
  }

  # Cross-season team random effects
  # GAM uses team_name.x (home) and team_name.y (away) from merged data.
  # .x captures the team's own effect on score diff; .y is the opponent's
  # (opposite sign), so we only need .x. Fall back to unsuffixed for
  # models fitted with a single team_name column.
  re_team <- tryCatch(
    extract_gam_random_effects(xsd, "^team_name\\.x$"),
    error = function(e) NULL
  )
  if (is.null(re_team)) {
    re_team <- tryCatch(
      extract_gam_random_effects(xsd, "^team_name$"),
      error = function(e) {
        cli::cli_warn("Could not extract team random effects: {conditionMessage(e)}")
        NULL
      }
    )
  }
  if (is.null(re_team)) return(NULL)

  result <- re_team[, .(team = level, residual_mean = coefficient, residual_se = se)]
  result[, team := torp_replace_teams(team)]

  # Within-season team random effects (same .x suffix logic)
  re_season <- tryCatch(
    extract_gam_random_effects(xsd, "^team_name_season\\.x$"),
    error = function(e) NULL
  )
  if (is.null(re_season)) {
    re_season <- tryCatch(
      extract_gam_random_effects(xsd, "team_name_season"),
      error = function(e) NULL
    )
  }

  if (!is.null(re_season)) {
    season_suffix <- paste0(" ", season, "$")
    re_cur <- re_season[grepl(season_suffix, level)]

    if (nrow(re_cur) > 0) {
      re_cur[, team := sub(paste0(" ", season, "$"), "", level)]
      re_cur[, team := torp_replace_teams(team)]
      re_cur <- re_cur[, .(season_coef = coefficient, season_se = se), by = team]

      result <- merge(result, re_cur, by = "team", all.x = TRUE)
      result[!is.na(season_coef), `:=`(
        residual_mean = residual_mean + season_coef,
        residual_se   = sqrt(residual_se^2 + season_se^2)
      )]
      result[, c("season_coef", "season_se") := NULL]
    }
  }

  # Inflate SE to widen season-long team uncertainty (GAM random effects are
  # shrunk toward the league mean and tend to under-state real uncertainty).
  result[, residual_se := residual_se * SIM_RESIDUAL_SE_MULT]

  result
}


# --------------------------------------------------------------------------
# Main entry point
# --------------------------------------------------------------------------

#' Simulate a full AFL season
#'
#' Runs multiple Monte Carlo simulations of the AFL regular season and finals
#' to estimate ladder positions, finals probabilities, and premiership odds.
#'
#' @param season Numeric season year (e.g. 2026).
#' @param n_sims Number of simulations to run (default [SIM_DEFAULT_N]).
#' @param team_ratings Optional data.table with `team` and `torp` columns. If
#'   NULL, loads from torpdata.
#' @param fixtures Optional fixture data. If NULL, loads from torpdata.
#' @param predictions Optional predictions data with `pred_xtotal`.
#' @param injuries Injury data.frame from [get_all_injuries()]. By default,
#'   fetched automatically from the AFL injury list. Pass `FALSE` to disable
#'   injury-aware simulation, or supply your own data.frame.
#' @param team_residuals Team quality residuals from the match GAM random
#'   effects. `"auto"` (default) extracts from the xscore_diff GAM via
#'   [.extract_team_residuals()]. Pass a data.frame with `team`,
#'   `residual_mean`, `residual_se` to supply custom values, or `NULL` to
#'   disable. Residuals are sampled from `N(mean, se)` once per simulation,
#'   capturing coaching/system quality not explained by player TORP.
#' @param models Optional named list of GAM models from
#'   `run_predictions_pipeline()$models`. When provided, team residuals are
#'   extracted from the fresh `xscore_diff` model instead of the stored
#'   torpmodels version. Ignored when `team_residuals` is not `"auto"`.
#' @param seed Optional random seed for reproducibility.
#' @param verbose Logical; if TRUE, shows a progress bar.
#' @param keep_games Logical; if TRUE, stores per-sim game results (memory
#'   intensive). Default FALSE.
#' @param n_cores Number of cores for parallel execution. Defaults to
#'   `parallel::detectCores() - 1` (leaving one core free for the OS/session).
#'   Set to `1L` for sequential execution.
#'   Values > 1 use [parallel::parLapply()] with reproducible L'Ecuyer-CMRG
#'   random streams. Progress bars are not shown in parallel mode.
#' @return An S3 object of class `"torp_sim_results"` containing `season`,
#'   `n_sims`, `ladders`, `finals`, `games` (if requested), and
#'   `original_ratings`.
#' @export
#' @examples
#' \dontrun{
#' try({
#'   results <- simulate_afl_season(2025, n_sims = 10, seed = 42)
#'   print(results)
#' })
#' }
simulate_afl_season <- function(season,
                                n_sims = SIM_DEFAULT_N,
                                team_ratings = NULL,
                                fixtures = NULL,
                                predictions = NULL,
                                injuries = NULL,
                                team_residuals = "auto",
                                models = NULL,
                                seed = NULL,
                                verbose = TRUE,
                                keep_games = FALSE,
                                n_cores = max(1L, parallel::detectCores() - 1L, na.rm = TRUE)) {

  .pipeline_start <- proc.time()

  if (!is.null(seed)) withr::local_seed(seed)

  # Default: fetch current injury list from AFL. Pass FALSE to skip.
  if (isFALSE(injuries)) {
    injuries <- NULL
  } else if (is.null(injuries)) {
    injuries <- tryCatch(get_all_injuries(season), error = function(e) {
      cli::cli_alert_warning("Could not fetch injuries: {conditionMessage(e)}")
      NULL
    })
  }

  # When injuries are provided, use reduced per-round noise
  sim_injury_sd <- if (!is.null(injuries) && nrow(injuries) > 0) {
    SIM_INJURY_SD_KNOWN
  } else {
    SIM_INJURY_SD
  }

  # Prepare data once
  prep <- prepare_sim_data(
    season         = season,
    team_ratings   = team_ratings,
    fixtures       = fixtures,
    predictions    = predictions,
    injuries       = injuries,
    team_residuals = team_residuals,
    models         = models
  )

  base_teams <- prep$sim_teams
  base_games <- prep$sim_games
  played_games <- prep$played_games
  gf_familiarity <- prep$gf_familiarity
  injury_schedule <- prep$injury_schedule
  prep <- NULL  # free memory before parallel serialization

  prep_elapsed <- (proc.time() - .pipeline_start)[["elapsed"]]
  if (verbose) {
    cli::cli_alert_success("Data preparation [{round(prep_elapsed, 1)}s]")
  }

  # Report residuals if present
  if (verbose && any(base_teams$residual_se > 0, na.rm = TRUE)) {
    n_teams <- sum(base_teams$residual_se > 0, na.rm = TRUE)
    cli::cli_alert_info("Team quality residuals: {n_teams} teams with GAM random effects (sampled per-sim)")
  }

  # Ensure column types once (so simulate_season doesn't need to per-sim)
  if ("result" %in% names(base_games)) {
    base_games[, result := as.integer(result)]
  }
  if ("torp_home_round" %in% names(base_games)) {
    base_games[, torp_home_round := as.numeric(torp_home_round)]
  }
  if ("torp_away_round" %in% names(base_games)) {
    base_games[, torp_away_round := as.numeric(torp_away_round)]
  }

  # --- Worker function: run a BATCH of sims and return combined results ---
  # progress_fn is called per sim in sequential mode; no-op in parallel
  run_sim_batch <- function(sim_ids, progress_fn = NULL) {
    n_batch <- length(sim_ids)
    ladders <- vector("list", n_batch)
    finals_list <- vector("list", n_batch)
    games_list <- if (keep_games) vector("list", n_batch) else NULL

    for (j in seq_along(sim_ids)) {
      sim_id <- sim_ids[j]
      sim_result <- simulate_season(base_teams, base_games, return_teams = TRUE,
                                    injury_sd = sim_injury_sd,
                                    injury_schedule = injury_schedule)

      all_games <- data.table::rbindlist(
        list(played_games, sim_result$games),
        use.names = TRUE, fill = TRUE
      )

      ladder <- calculate_ladder(all_games)
      ladder[, sim_id := sim_id]
      ladders[[j]] <- ladder

      finals <- simulate_finals(ladder, sim_result$teams, gf_familiarity)
      finals[, sim_id := sim_id]
      finals_list[[j]] <- finals

      if (keep_games) {
        all_games[, sim_id := sim_id]
        games_list[[j]] <- all_games
      }

      if (!is.null(progress_fn)) progress_fn()
    }

    list(
      ladder = data.table::rbindlist(ladders),
      finals = data.table::rbindlist(finals_list),
      games  = if (keep_games) data.table::rbindlist(games_list) else NULL
    )
  }

  # --- Execute: parallel or sequential ---
  n_cores <- as.integer(n_cores)
  # On Windows, PSOCK cluster startup (~5s) only pays off for large n_sims.
  # Sequential with optimised vector-based simulate_season runs ~9ms/sim,

  # so parallel break-even is roughly n_sims > 1000.
  if (.Platform$OS.type == "windows" && n_cores > 1L && n_sims <= 1000L) {
    n_cores <- 1L
  }
  if (verbose) {
    cli::cli_inform("Running {n_sims} simulations on {n_cores} core{?s}.")
  }
  if (n_cores > 1L) {
    # Aggressive cleanup before spawning workers: leaked file connections from
    # pipeline downloads can collide with PSOCK sockets on Windows and surface
    # as "Error in serialize(data, node$con)" during clusterExport. Close all
    # non-stdio connections unconditionally — safer than the selective pass.
    tryCatch(closeAllConnections(), error = function(e) NULL)
    gc()

    # Wrap the entire parallel pipeline (cluster creation, worker setup,
    # exports, parLapply) in one tryCatch. Any failure — socket death, missing
    # dep, OOM — cleanly falls through to the sequential branch below.
    cl <- NULL
    chunk_results <- tryCatch({
      cl <- parallel::makeCluster(n_cores)

      # Load torp in each worker to avoid serializing the full namespace.
      # devtools::load_all sessions need load_all on workers too (installed
      # version may be stale). Detect via pkgload::is_dev_package().
      pkg_src <- getNamespaceInfo("torp", "path")
      is_dev <- isTRUE(tryCatch(
        pkgload::is_dev_package("torp"),
        error = function(e) FALSE
      ))

      parallel::clusterEvalQ(cl, library(data.table))

      torp_loaded <- FALSE
      if (is_dev) {
        # devtools session: workers must load_all from source to get current code
        parallel::clusterExport(cl, "pkg_src", envir = environment())
        torp_loaded <- tryCatch({
          parallel::clusterEvalQ(cl,
            suppressMessages(devtools::load_all(pkg_src, quiet = TRUE)))
          TRUE
        }, error = function(e) FALSE)
      }
      if (!torp_loaded) {
        # Installed package or load_all failed: try library()
        torp_loaded <- tryCatch({
          parallel::clusterEvalQ(cl, library(torp))
          TRUE
        }, error = function(e) FALSE)
      }
      if (!torp_loaded) {
        # Last resort: export functions directly (slower due to env serialization)
        parallel::clusterExport(cl, c(
          "simulate_season", "calculate_ladder",
          "simulate_finals", "simulate_match",
          "finals_home_advantage", "gf_home_advantage",
          "SIM_HOME_ADVANTAGE", "SIM_NOISE_SD", "SIM_WP_SCALING_FACTOR",
          "SIM_AVG_TOTAL", "SIM_TOTAL_SD", "SIM_GF_FAMILIARITY_SCALE",
          "SIM_GF_VENUE", "SIM_VICTORIAN_TEAMS", "SIM_RATING_SHIFT",
          "SIM_INJURY_SD", "SIM_INJURY_SD_KNOWN", "SIM_MEAN_REVERSION"
        ), envir = environment())
      }

      # Export only data objects (small, no environment baggage)
      parallel::clusterExport(cl, c(
        "base_teams", "base_games", "played_games", "keep_games",
        "sim_injury_sd", "gf_familiarity", "injury_schedule"
      ), envir = environment())

      # Reproducible parallel RNG
      if (!is.null(seed)) parallel::clusterSetRNGStream(cl, seed)

      # Split sims into one chunk per core — one round-trip per worker
      chunks <- split(seq_len(n_sims), rep(seq_len(n_cores), length.out = n_sims))

      parallel::parLapply(cl, chunks, run_sim_batch)
    },
    error = function(e) {
      cli::cli_warn("Parallel cluster failed ({conditionMessage(e)}), falling back to sequential")
      NULL
    })

    # Always stop the cluster, even if setup or parLapply failed. Explicit
    # stopCluster is cleaner than on.exit (which wouldn't fire until the whole
    # simulate_afl_season() call returns, leaving workers hanging during the
    # sequential fallback).
    if (!is.null(cl)) {
      tryCatch(parallel::stopCluster(cl), error = function(e) NULL)
    }

    # Defensive chunk validation: a worker returning NULL or a malformed list
    # (e.g. silent recovery from a data.table issue, a future refactor in
    # run_sim_batch that silently short-circuits) would otherwise pass
    # through rbindlist and yield ladder results from fewer sims than
    # requested. Cheap to check, and the sequential fallback is already
    # wired up below.
    if (!is.null(chunk_results)) {
      required <- c("ladder", "finals", if (keep_games) "games")
      malformed <- vapply(chunk_results, function(x) {
        is.null(x) || !is.list(x) || !all(required %in% names(x))
      }, logical(1))
      if (any(malformed)) {
        cli::cli_warn(
          "Parallel run produced {sum(malformed)}/{length(malformed)} malformed chunk{?s}; falling back to sequential"
        )
        chunk_results <- NULL
      }
    }

    if (!is.null(chunk_results)) {
      all_ladders <- data.table::rbindlist(lapply(chunk_results, `[[`, "ladder"))
      all_finals  <- data.table::rbindlist(lapply(chunk_results, `[[`, "finals"))
      all_games   <- if (keep_games) {
        data.table::rbindlist(lapply(chunk_results, `[[`, "games"))
      } else {
        NULL
      }

      # Second-order sanity check: total ladder rows should be n_sims * n_teams.
      # A silently-dropped chunk that still looked well-formed (empty frames)
      # would slip past the malformed check above; this catches that.
      expected_ladder_rows <- n_sims * nrow(base_teams)
      if (nrow(all_ladders) != expected_ladder_rows) {
        cli::cli_warn(c(
          "Parallel run produced {nrow(all_ladders)} ladder rows, expected {expected_ladder_rows}",
          "i" = "Falling back to sequential"
        ))
        chunk_results <- NULL
      }
    }

    if (is.null(chunk_results)) {
      # Sequential fallback — show progress so the run isn't silent when
      # verbose = TRUE (a 3000-sim fallback is ~30 s, long enough to look hung).
      if (verbose) {
        pb_id <- cli::cli_progress_bar(
          "Simulating seasons (sequential fallback)", total = n_sims)
        progress_fn <- function() cli::cli_progress_update(id = pb_id)
      } else {
        pb_id <- NULL
        progress_fn <- NULL
      }
      fallback <- run_sim_batch(seq_len(n_sims), progress_fn = progress_fn)
      all_ladders <- fallback$ladder
      all_finals  <- fallback$finals
      all_games   <- fallback$games
      if (verbose) cli::cli_progress_done(id = pb_id)
    }
  } else {
    # Sequential: run all sims in a single batch with inline progress
    if (verbose) {
      pb_id <- cli::cli_progress_bar("Simulating seasons", total = n_sims)
      progress_fn <- function() cli::cli_progress_update(id = pb_id)
    } else {
      pb_id <- NULL
      progress_fn <- NULL
    }

    batch_result <- run_sim_batch(seq_len(n_sims), progress_fn = progress_fn)
    all_ladders <- batch_result$ladder
    all_finals  <- batch_result$finals
    all_games   <- batch_result$games

    if (verbose) cli::cli_progress_done(id = pb_id)
  }

  if (verbose) {
    sim_elapsed <- (proc.time() - .pipeline_start)[["elapsed"]] - prep_elapsed
    cli::cli_alert_success("Simulation [{round(sim_elapsed, 1)}s]")
  }

  # --- Collect results ---
  result <- list(
    season           = season,
    n_sims           = n_sims,
    ladders          = all_ladders,
    finals           = all_finals,
    games            = all_games,
    original_ratings = base_teams,
    played_games     = played_games,
    injury_schedule  = injury_schedule,
    injuries         = injuries
  )
  class(result) <- "torp_sim_results"

  if (verbose) {
    total_elapsed <- (proc.time() - .pipeline_start)[["elapsed"]]
    cli::cli_alert_success("Total [{round(total_elapsed, 1)}s]")
  }

  result
}


# --------------------------------------------------------------------------
# Summary and print methods
# --------------------------------------------------------------------------

#' Summarise simulation results
#'
#' Aggregates ladder and finals results across all simulations into a single
#' team-level summary table.
#'
#' @param sim_results An object of class `"torp_sim_results"`.
#' @return A data.table with one row per team, ordered by average wins descending.
#' @export
summarise_simulations <- function(sim_results) {
  n <- sim_results$n_sims

  # Ladder summary
  # w10 / w90 are 10th and 90th percentile of wins across sims (downside/upside
  # bounds for a team's likely season total, roughly a "bad year / good year").
  # Top-6 and Top-10 added for the expanded 2026 finals structure.
  ladder_sum <- sim_results$ladders[, .(
    avg_wins       = mean(wins),
    avg_losses     = mean(losses),
    avg_draws      = mean(draws),
    avg_percentage = mean(percentage),
    avg_pf_pg      = mean(points_for / played),
    avg_pa_pg      = mean(points_against / played),
    avg_rank       = mean(rank),
    w10            = stats::quantile(wins, 0.1, names = FALSE),
    w90            = stats::quantile(wins, 0.9, names = FALSE),
    top_10_pct     = mean(rank <= 10),
    top_8_pct      = mean(rank <= 8),
    top_6_pct      = mean(rank <= 6),
    top_4_pct      = mean(rank <= 4),
    top_2_pct      = mean(rank <= 2),
    top_1_pct      = mean(rank == 1),
    last_pct       = mean(rank == max(rank))
  ), by = team]

  # Finals summary
  if (nrow(sim_results$finals) > 0) {
    finals_sum <- sim_results$finals[, .(
      made_finals_pct = .N / n,
      avg_finals_wins = mean(finals_wins),
      made_gf_pct     = sum(made_gf) / n,
      won_gf_pct      = sum(won_gf) / n
    ), by = team]

    out <- merge(ladder_sum, finals_sum, by = "team", all.x = TRUE)

    # Teams that never made finals
    out[is.na(made_finals_pct), `:=`(
      made_finals_pct = 0,
      avg_finals_wins = 0,
      made_gf_pct     = 0,
      won_gf_pct      = 0
    )]
  } else {
    out <- ladder_sum
    out[, `:=`(made_finals_pct = 0, avg_finals_wins = 0,
               made_gf_pct = 0, won_gf_pct = 0)]
  }

  # Join original TORP ratings and team residuals
  if (!is.null(sim_results$original_ratings)) {
    rat_dt <- data.table::as.data.table(sim_results$original_ratings)
    rat_cols <- intersect(c("team", "torp", "residual_mean"), names(rat_dt))
    out <- merge(out, rat_dt[, ..rat_cols], by = "team", all.x = TRUE)
  }

  # Compute current W-L from played games
  if (!is.null(sim_results$played_games) && nrow(sim_results$played_games) > 0) {
    pg <- sim_results$played_games
    home <- pg[, .(team = home_team, margin = result)]
    away <- pg[, .(team = away_team, margin = -result)]
    records <- data.table::rbindlist(list(home, away))
    current_record <- records[, .(
      current_w = sum(margin > 0),
      current_l = sum(margin < 0),
      current_d = sum(margin == 0)
    ), by = team]
    out <- merge(out, current_record, by = "team", all.x = TRUE)
    out[is.na(current_w), `:=`(current_w = 0L, current_l = 0L, current_d = 0L)]
  }

  data.table::setorder(out, -avg_wins)
  out[]
}


#' Print simulation results
#'
#' @param x A `torp_sim_results` object.
#' @param ... Additional arguments (ignored).
#' @export
print.torp_sim_results <- function(x, ...) {
  summary <- summarise_simulations(x)

  cli::cli_h2("AFL Season Simulation: {x$season}")
  cli::cli_text("{x$n_sims} simulations")

  # Show team residuals summary if present
  ratings <- x$original_ratings
  if (!is.null(ratings) && "residual_mean" %in% names(ratings) &&
      any(ratings$residual_se > 0, na.rm = TRUE)) {
    cli::cli_text("Team quality residuals from match GAM (sampled per-sim)")
  }

  # Show injury schedule summary if present
  inj_sched <- x$injury_schedule
  if (!is.null(inj_sched) && nrow(inj_sched) > 0) {
    # Net boost per team (sum of all returning players' contributions)
    net_boost <- inj_sched[, .(net_boost = sum(torp_boost)), by = team]
    data.table::setorder(net_boost, -net_boost)
    top_gains <- utils::head(net_boost[net_boost > 0], 3)
    if (nrow(top_gains) > 0) {
      boost_str <- paste(
        sprintf("%s (+%.1f)", top_gains$team, top_gains$net_boost),
        collapse = ", "
      )
      cli::cli_text("Injury-aware | biggest return boosts: {boost_str}")
    }
  }
  cli::cli_text("")

  # Format compact table
  has_residual <- "residual_mean" %in% names(summary) &&
    any(summary$residual_mean != 0, na.rm = TRUE)
  display <- summary[, .(
    Team    = team,
    TORP    = if ("torp" %in% names(summary)) sprintf("%.1f", torp) else NA_character_,
    Resid   = if (has_residual) sprintf("%+.1f", residual_mean) else NA_character_,
    Record  = if ("current_w" %in% names(summary)) {
      sprintf("%d-%d%s", current_w, current_l,
              ifelse(current_d > 0, sprintf("-%d", current_d), ""))
    } else NA_character_,
    W       = sprintf("%.1f", avg_wins),
    W10     = sprintf("%.1f", w10),
    W90     = sprintf("%.1f", w90),
    PF      = sprintf("%.1f", avg_pf_pg),
    PA      = sprintf("%.1f", avg_pa_pg),
    Pct     = sprintf("%.1f", avg_percentage),
    `1st`   = sprintf("%.1f%%", top_1_pct * 100),
    Top4    = sprintf("%.0f%%", top_4_pct * 100),
    Top6    = sprintf("%.0f%%", top_6_pct * 100),
    Top8    = sprintf("%.0f%%", top_8_pct * 100),
    Top10   = sprintf("%.0f%%", top_10_pct * 100),
    GF      = sprintf("%.0f%%", made_gf_pct * 100),
    Premier = sprintf("%.1f%%", won_gf_pct * 100),
    Last    = sprintf("%.1f%%", last_pct * 100)
  )]

  # Drop columns that are all NA (e.g. no played games yet)
  na_cols <- names(display)[vapply(display, function(x) all(is.na(x)), logical(1))]
  if (length(na_cols) > 0) display[, (na_cols) := NULL]

  print(display, nrows = 18)
  invisible(x)
}
