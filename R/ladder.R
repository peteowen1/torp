#' AFL Season Simulation System
#'
#' Functions for simulating complete AFL seasons including regular season,
#' ladder calculation with AFL tiebreak rules, and finals bracket simulation.
#'
#' @name ladder
NULL


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
  resolve_col <- function(dt, candidates) {
    for (cand in candidates) {
      if (cand %in% names(dt)) return(cand)
    }
    NULL
  }

  rnd_col  <- resolve_col(fix_dt, c("round_number", "roundnum"))
  ht_col   <- resolve_col(fix_dt, c("home_team_name", "home_team"))
  at_col   <- resolve_col(fix_dt, c("away_team_name", "away_team"))
  hs_col   <- resolve_col(fix_dt, c("home_score", "home_points"))
  as_col   <- resolve_col(fix_dt, c("away_score", "away_points"))

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
  if (is.na(max_round)) max_round <- 24L
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
    pred_rnd <- resolve_col(pred_dt, c("round_number", "roundnum", "round", "week"))
    pred_ht  <- resolve_col(pred_dt, c("home_team", "home_team_name"))

    if (!is.null(pred_rnd) && !is.null(pred_ht) && "pred_xtotal" %in% names(pred_dt)) {
      sim_games[pred_dt,
        pred_xtotal := i.pred_xtotal,
        on = stats::setNames(c(pred_rnd, pred_ht), c("roundnum", "home_team"))
      ]
    }
  }

  # --- GF Venue Familiarity ---
  # Compute each team's proportion of games at the GF venue (MCG)
  venue_col <- resolve_col(fix_dt, c("venue_name", "venue"))
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
    team_residuals <- .extract_team_residuals(models = models)
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
#' Extracts the cross-season `team_name.x` random effects from the
#' `xscore_diff` GAM. These capture systematic team over/under-performance
#' not explained by player TORP ratings (e.g. coaching, team system).
#' Team names are standardised via [torp_replace_teams()] to ensure
#' consistent merging with simulation team names.
#'
#' @param models Optional named list of GAM models (as returned by
#'   `run_predictions_pipeline()$models`). When provided, uses the
#'   `xscore_diff` model directly. Otherwise loads from torpmodels.
#' @return A data.table with columns `team`, `residual_mean`, `residual_se`,
#'   or NULL if extraction fails.
#' @keywords internal
.extract_team_residuals <- function(models = NULL) {
  if (!requireNamespace("mixedup", quietly = TRUE)) {
    cli::cli_warn("Package {.pkg mixedup} is needed to extract team residuals. Install with {.code install.packages('mixedup')}.")
    return(NULL)
  }

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

  re <- tryCatch(
    mixedup::extract_ranef(xsd),
    error = function(e) {
      cli::cli_warn("Could not extract random effects: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(re)) return(NULL)

  team_re <- re[re$group_var == "team_name.x", c("group", "value", "se")]
  if (nrow(team_re) == 0) return(NULL)

  names(team_re) <- c("team", "residual_mean", "residual_se")
  result <- data.table::as.data.table(team_re)

  # Standardise team names to canonical form for reliable merging
  result[, team := torp_replace_teams(team)]

  result
}


# --------------------------------------------------------------------------
# Ladder calculation
# --------------------------------------------------------------------------

#' Calculate AFL ladder from game results
#'
#' Pivots game-level results to team-perspective rows and aggregates to produce
#' a full AFL ladder sorted by ladder points then percentage.
#'
#' @param games_dt A data.table with columns `home_team`, `away_team`,
#'   `home_score`, `away_score`, and `result` (home margin).
#' @return A data.table with one row per team, sorted by ladder position.
#' @export
calculate_ladder <- function(games_dt) {
  if (!data.table::is.data.table(games_dt)) {
    games_dt <- data.table::as.data.table(games_dt)
  }

  # Pivot home + away in one filter pass
  played <- games_dt[!is.na(result)]
  team_rows <- data.table::data.table(
    team          = c(played$home_team, played$away_team),
    score_for     = c(played$home_score, played$away_score),
    score_against = c(played$away_score, played$home_score),
    margin        = c(played$result, -played$result)
  )

  ladder <- team_rows[, .(
    played     = .N,
    wins       = sum(margin > 0L),
    draws      = sum(margin == 0L),
    losses     = sum(margin < 0L),
    points_for = sum(score_for),
    points_against = sum(score_against)
  ), by = team]

  ladder[, `:=`(
    percentage    = data.table::fifelse(points_against > 0,
                                        points_for / points_against * 100,
                                        0),
    ladder_points = wins * 4L + draws * 2L
  )]

  data.table::setorder(ladder, -ladder_points, -percentage)
  ladder[, rank := seq_len(.N)]

  ladder[]
}


#' Calculate expected final ladder from actual results + predictions
#'
#' Combines played game results with model predictions for remaining games
#' to produce a single deterministic expected end-of-season ladder. Uses
#' fractional wins (pred_win) for unplayed games, giving a smooth expected
#' ladder that properly reflects match uncertainty.
#'
#' When `injuries` is provided, predicted margins for unplayed games are
#' adjusted to reflect injured players returning mid-season. The injury
#' schedule is built using [torp_ratings()], [parse_return_round()], and
#' [build_injury_schedule()], and cumulative TORP boosts are applied per
#' round to shift `pred_margin` and recalculate `pred_win`.
#'
#' @param season Numeric season year (e.g. 2026).
#' @param fixtures Optional fixture data. If NULL, loads via [load_fixtures()].
#' @param predictions Optional predictions data. If NULL, loads via
#'   [load_predictions()].
#' @param injuries Optional injury data.frame from [get_all_injuries()]. When
#'   provided, team predictions are adjusted for injured players returning
#'   during the season.
#' @return A data.table with one row per team, sorted by expected ladder
#'   position. Columns: `team`, `played`, `wins`, `draws`, `losses`,
#'   `expected_wins`, `expected_losses`, `points_for`, `points_against`,
#'   `percentage`, `ladder_points`, `rank`.
#' @export
calculate_final_ladder <- function(season = get_afl_season(),
                                   fixtures = NULL,
                                   predictions = NULL,
                                   injuries = NULL) {

  # --- Load fixtures ---
  if (is.null(fixtures)) {
    fixtures <- load_fixtures(seasons = season)
  }
  fix_dt <- data.table::as.data.table(fixtures)
  if ("season" %in% names(fix_dt)) {
    fix_dt <- fix_dt[get("season") == season]
  }

  # Resolve columns (same helper logic as prepare_sim_data)
  resolve_col <- function(dt, candidates) {
    for (cand in candidates) {
      if (cand %in% names(dt)) return(cand)
    }
    NULL
  }

  rnd_col <- resolve_col(fix_dt, c("round_number", "roundnum"))
  ht_col  <- resolve_col(fix_dt, c("home_team_name", "home_team"))
  at_col  <- resolve_col(fix_dt, c("away_team_name", "away_team"))
  hs_col  <- resolve_col(fix_dt, c("home_score", "home_points"))
  as_col  <- resolve_col(fix_dt, c("away_score", "away_points"))

  if (is.null(rnd_col) || is.null(ht_col) || is.null(at_col)) {
    cli::cli_abort("Could not find required fixture columns.")
  }

  mid_col <- resolve_col(fix_dt, c("match_id", "providerId"))

  games <- data.table::data.table(
    match_id   = if (!is.null(mid_col)) as.character(fix_dt[[mid_col]]) else NA_character_,
    roundnum   = as.integer(fix_dt[[rnd_col]]),
    home_team  = as.character(fix_dt[[ht_col]]),
    away_team  = as.character(fix_dt[[at_col]]),
    home_score = if (!is.null(hs_col)) as.numeric(fix_dt[[hs_col]]) else NA_real_,
    away_score = if (!is.null(as_col)) as.numeric(fix_dt[[as_col]]) else NA_real_
  )

  # Keep only regular season
  max_round <- AFL_REGULAR_SEASON_ROUNDS[as.character(season)]
  if (is.na(max_round)) max_round <- 24L
  games <- games[roundnum <= max_round]

  games[, played := !is.na(home_score) & !is.na(away_score)]

  # --- Load predictions ---
  if (is.null(predictions)) {
    predictions <- tryCatch(
      load_predictions(seasons = season, rounds = TRUE),
      error = function(e) {
        cli::cli_warn("Could not load predictions: {conditionMessage(e)} -- unplayed games will use 50/50 toss-up estimates")
        NULL
      }
    )
  }

  if (!is.null(predictions)) {
    pred_dt <- data.table::as.data.table(predictions)

    # Resolve prediction team columns for join matching
    pred_ht <- resolve_col(pred_dt, c("home_team", "home_team_name"))
    pred_at <- resolve_col(pred_dt, c("away_team", "away_team_name"))

    # Join on match_id (predictions may have home/away swapped vs fixtures)
    pred_mid <- resolve_col(pred_dt, c("match_id", "providerId"))

    if (!is.null(pred_mid) && !is.null(mid_col)) {
      # Build a lookup with the prediction's home_team for flip detection
      pred_lookup <- pred_dt[, c(pred_mid, "pred_win", "pred_margin",
                                  "pred_xtotal", pred_ht), with = FALSE]
      data.table::setnames(pred_lookup, pred_ht, "pred_home_team")
      data.table::setnames(pred_lookup, pred_mid, "match_id")

      games[pred_lookup, `:=`(
        pred_win    = i.pred_win,
        pred_margin = i.pred_margin,
        pred_xtotal = i.pred_xtotal,
        pred_home   = i.pred_home_team
      ), on = "match_id"]

      # Flip pred_margin and pred_win when prediction home != fixture home
      flipped <- !is.na(games$pred_home) & games$pred_home != games$home_team
      if (any(flipped)) {
        games[flipped, `:=`(
          pred_margin = -pred_margin,
          pred_win    = 1 - pred_win
        )]
      }
      games[, pred_home := NULL]
    }
  }

  # --- Injury adjustment ---
  # When injuries are provided, adjust pred_margin for unplayed games to

  # reflect injured players returning mid-season
  if (!is.null(injuries) && nrow(injuries) > 0) {
    # Determine current round from played games
    current_round <- if (any(games$played)) max(games[played == TRUE, roundnum]) else 1L

    # Build injury schedule from player-level ratings
    pr <- tryCatch(torp_ratings(season), error = function(e) {
      cli::cli_warn("Could not load player ratings for injury adjustment: {conditionMessage(e)} -- returning ladder without injury adjustments")
      NULL
    })
    if (!is.null(pr)) {
      pr_dt <- data.table::as.data.table(pr)
      if ("season" %in% names(pr_dt) && "round" %in% names(pr_dt)) {
        max_s <- max(pr_dt$season)
        pr_dt <- pr_dt[season == max_s]
        max_r <- max(pr_dt$round)
        pr_dt <- pr_dt[round == max_r]
      }

      inj_with_round <- data.table::as.data.table(injuries)
      inj_with_round[, return_round := parse_return_round(
        estimated_return, season, current_round
      )]

      injury_schedule <- build_injury_schedule(inj_with_round, pr_dt)

      if (nrow(injury_schedule) > 0) {
        # Compute cumulative torp_boost per team up to each round
        # (returning players stay returned for all subsequent rounds)
        all_rounds <- sort(unique(games[played == FALSE, roundnum]))
        all_teams <- unique(injury_schedule$team)

        # Build a lookup: for each (team, round), sum boosts from all
        # return_rounds <= that round
        cum_boost <- data.table::CJ(team = all_teams, roundnum = all_rounds)
        cum_boost[, boost := vapply(seq_len(.N), function(i) {
          sum(injury_schedule[team == cum_boost$team[i] &
                              return_round <= cum_boost$roundnum[i],
                              torp_boost])
        }, numeric(1))]
        cum_boost <- cum_boost[boost != 0]

        if (nrow(cum_boost) > 0) {
          # Join home and away boosts to unplayed games
          games[cum_boost, home_boost := i.boost,
                on = .(home_team = team, roundnum)]
          games[cum_boost, away_boost := i.boost,
                on = .(away_team = team, roundnum)]
          games[is.na(home_boost), home_boost := 0]
          games[is.na(away_boost), away_boost := 0]

          # Adjust pred_margin then recalculate pred_win from the new margin
          unplayed_adj <- games$played == FALSE &
                          (games$home_boost != 0 | games$away_boost != 0)
          games[unplayed_adj,
                pred_margin := pred_margin + (home_boost - away_boost)]
          games[unplayed_adj,
                pred_win := 1 / (10^(-pred_margin / SIM_WP_SCALING_FACTOR) + 1)]

          games[, c("home_boost", "away_boost") := NULL]
        }
      }
    }
  }

  # For unplayed games, derive expected scores from predictions
  # Games with full predictions: use pred_xtotal + pred_margin
  games[played == FALSE & !is.na(pred_xtotal) & !is.na(pred_margin), `:=`(
    home_score = (pred_xtotal + pred_margin) / 2,
    away_score = (pred_xtotal - pred_margin) / 2
  )]

  # Games with pred_margin but no pred_xtotal: use average total
  games[played == FALSE & is.na(pred_xtotal) & !is.na(pred_margin), `:=`(
    home_score = (SIM_AVG_TOTAL + pred_margin) / 2,
    away_score = (SIM_AVG_TOTAL - pred_margin) / 2
  )]

  # Games with no predictions at all: 50/50 toss-up, average scores
  games[played == FALSE & is.na(pred_win), pred_win := 0.5]
  games[played == FALSE & is.na(home_score), `:=`(
    home_score = SIM_AVG_TOTAL / 2,
    away_score = SIM_AVG_TOTAL / 2
  )]

  # --- Pivot to team rows ---
  # Home perspective
  home <- games[, .(
    team          = home_team,
    score_for     = home_score,
    score_against = away_score,
    is_played     = played,
    win_prob      = data.table::fifelse(played,
      data.table::fifelse(home_score > away_score, 1, data.table::fifelse(home_score == away_score, 0.5, 0)),
      pred_win)
  )]

  # Away perspective
  away <- games[, .(
    team          = away_team,
    score_for     = away_score,
    score_against = home_score,
    is_played     = played,
    win_prob      = data.table::fifelse(played,
      data.table::fifelse(away_score > home_score, 1, data.table::fifelse(away_score == home_score, 0.5, 0)),
      1 - pred_win)
  )]

  team_rows <- data.table::rbindlist(list(home, away))

  # Drop rows with no score info at all
  team_rows <- team_rows[!is.na(score_for)]

  # --- Aggregate ---
  ladder <- team_rows[, .(
    played         = sum(is_played),
    wins           = sum(win_prob[is_played == TRUE] == 1),
    draws          = sum(win_prob[is_played == TRUE] == 0.5),
    losses         = sum(win_prob[is_played == TRUE] == 0),
    expected_wins  = sum(win_prob),
    expected_losses = sum(1 - win_prob),
    points_for     = round(sum(score_for, na.rm = TRUE)),
    points_against = round(sum(score_against, na.rm = TRUE))
  ), by = team]

  ladder[, `:=`(
    percentage    = data.table::fifelse(points_against > 0,
                                        points_for / points_against * 100, 0),
    ladder_points = round(expected_wins * 4)
  )]

  data.table::setorder(ladder, -ladder_points, -percentage)
  ladder[, rank := seq_len(.N)]

  ladder[]
}


# --------------------------------------------------------------------------
# Finals venue advantage
# --------------------------------------------------------------------------

#' Compute finals home advantage based on venue familiarity
#'
#' AFL finals venue rules: the higher-seeded team hosts in their home state.
#' Victorian teams play at the MCG, where familiarity varies by team.
#' Interstate teams host at their home ground (standard home advantage).
#' The Grand Final is always at the MCG regardless of who is playing.
#'
#' @param home Character. Home (higher-seeded) team name.
#' @param away Character. Away team name.
#' @param fam_lookup Named numeric vector of MCG familiarity per team,
#'   or NULL (falls back to standard home advantage).
#' @param gf Logical. TRUE for the Grand Final (always MCG).
#' @return Numeric points advantage for the home team.
#' @keywords internal
finals_home_advantage <- function(home, away, fam_lookup, gf = FALSE) {
  is_mcg <- gf || (home %in% SIM_VICTORIAN_TEAMS)

  if (is_mcg && !is.null(fam_lookup)) {
    fam_home <- if (home %in% names(fam_lookup)) fam_lookup[[home]] else 0
    fam_away <- if (away %in% names(fam_lookup)) fam_lookup[[away]] else 0
    (fam_home - fam_away) * SIM_GF_FAMILIARITY_SCALE
  } else {
    SIM_HOME_ADVANTAGE
  }
}

#' @rdname finals_home_advantage
#' @keywords internal
gf_home_advantage <- function(home, away, fam_lookup) {
  finals_home_advantage(home, away, fam_lookup, gf = TRUE)
}


# --------------------------------------------------------------------------
# Finals simulation
# --------------------------------------------------------------------------

#' Simulate a single match (internal helper)
#'
#' Uses the same formula as the regular season simulation. For finals,
#' `allow_draw = FALSE` re-draws until a decisive result.
#'
#' @param home_torp Numeric home team TORP rating.
#' @param away_torp Numeric away team TORP rating.
#' @param home_advantage Numeric home advantage in points.
#' @param allow_draw Logical; if FALSE, re-simulates ties.
#' @return A list with `result` (margin from home perspective), `home_score`,
#'   `away_score`, `estimate`.
#' @keywords internal
simulate_match <- function(home_torp, away_torp,
                           home_advantage = SIM_HOME_ADVANTAGE,
                           allow_draw = TRUE) {
  estimate <- home_advantage + (home_torp - away_torp)

  repeat {
    result <- as.integer(round(
      stats::rnorm(1, estimate, SIM_NOISE_SD + abs(estimate) / 3)
    ))
    if (allow_draw || result != 0L) break
  }

  total <- pmax(stats::rnorm(1, SIM_AVG_TOTAL, SIM_TOTAL_SD), 40)
  home_score <- as.integer(pmax(round((total + result) / 2), 0))
  away_score <- as.integer(pmax(round((total - result) / 2), 0))

  list(result = result, home_score = home_score, away_score = away_score,
       estimate = estimate)
}


#' Simulate AFL finals series
#'
#' Implements the full AFL top-8 finals bracket: qualifying finals, elimination
#' finals, semi-finals, preliminary finals, and grand final.
#'
#' @param ladder_dt A data.table from [calculate_ladder()] with `team` and
#'   `rank` columns.
#' @param sim_teams_dt A data.table with `team` and `torp` (hot ratings after
#'   regular season).
#' @param gf_familiarity Optional data.table with `team` and `gf_familiarity`
#'   columns (proportion of games played at GF venue). When provided, the Grand
#'   Final home advantage is based on familiarity difference between teams.
#' @return A data.table with columns: `team`, `finals_finish` (week eliminated
#'   or 5 for premier), `finals_wins`, `made_gf`, `won_gf`.
#' @keywords internal
simulate_finals <- function(ladder_dt, sim_teams_dt, gf_familiarity = NULL) {
  # Build lookup: team -> torp rating
  ratings <- stats::setNames(sim_teams_dt$torp, sim_teams_dt$team)
  # Original ladder positions for home advantage
  ladder_pos <- stats::setNames(ladder_dt$rank, ladder_dt$team)

  # GF venue familiarity lookup
  fam_lookup <- if (!is.null(gf_familiarity)) {
    stats::setNames(gf_familiarity$gf_familiarity, gf_familiarity$team)
  } else {
    NULL
  }

  top8 <- ladder_dt[rank <= 8, team]
  if (length(top8) < 8) {
    cli::cli_abort("Need at least 8 teams for finals simulation, got {length(top8)}.")
  }

  # Helper: play a finals match, update ratings, return winner/loser
  play_final <- function(home, away, gf = FALSE) {
    ha <- finals_home_advantage(home, away, fam_lookup, gf = gf)
    res <- simulate_match(ratings[home], ratings[away],
                          home_advantage = ha, allow_draw = FALSE)

    # Hot rating adjustment
    shift <- SIM_RATING_SHIFT * (res$result - res$estimate)
    ratings[home] <<- ratings[home] + shift
    ratings[away] <<- ratings[away] - shift

    if (res$result > 0) list(winner = home, loser = away)
    else list(winner = away, loser = home)
  }

  # Higher-ranked team (lower rank number) is home
  home_team <- function(a, b) {
    if (ladder_pos[a] < ladder_pos[b]) a else b
  }
  away_team <- function(a, b) {
    if (ladder_pos[a] < ladder_pos[b]) b else a
  }

  # Track results
  finals_wins  <- stats::setNames(rep(0L, 8), top8)
  finals_finish <- stats::setNames(rep(0L, 8), top8)

  # --- Week 1 ---
  # QF1: 1 v 4
  qf1 <- play_final(top8[1], top8[4])
  finals_wins[qf1$winner] <- finals_wins[qf1$winner] + 1L

  # QF2: 2 v 3
  qf2 <- play_final(top8[2], top8[3])
  finals_wins[qf2$winner] <- finals_wins[qf2$winner] + 1L

  # EF1: 5 v 8
  ef1 <- play_final(top8[5], top8[8])
  finals_wins[ef1$winner] <- finals_wins[ef1$winner] + 1L
  finals_finish[ef1$loser] <- 1L  # eliminated week 1

  # EF2: 6 v 7
  ef2 <- play_final(top8[6], top8[7])
  finals_wins[ef2$winner] <- finals_wins[ef2$winner] + 1L
  finals_finish[ef2$loser] <- 1L  # eliminated week 1

  # --- Week 2 ---
  # SF1: Loser QF1 v Winner EF1
  sf1_home <- home_team(qf1$loser, ef1$winner)
  sf1_away <- away_team(qf1$loser, ef1$winner)
  sf1 <- play_final(sf1_home, sf1_away)
  finals_wins[sf1$winner] <- finals_wins[sf1$winner] + 1L
  finals_finish[sf1$loser] <- 2L  # eliminated week 2

  # SF2: Loser QF2 v Winner EF2
  sf2_home <- home_team(qf2$loser, ef2$winner)
  sf2_away <- away_team(qf2$loser, ef2$winner)
  sf2 <- play_final(sf2_home, sf2_away)
  finals_wins[sf2$winner] <- finals_wins[sf2$winner] + 1L
  finals_finish[sf2$loser] <- 2L  # eliminated week 2

  # --- Week 3 ---
  # PF1: Winner QF1 v Winner SF1
  pf1_home <- home_team(qf1$winner, sf1$winner)
  pf1_away <- away_team(qf1$winner, sf1$winner)
  pf1 <- play_final(pf1_home, pf1_away)
  finals_wins[pf1$winner] <- finals_wins[pf1$winner] + 1L
  finals_finish[pf1$loser] <- 3L  # eliminated week 3

  # PF2: Winner QF2 v Winner SF2
  pf2_home <- home_team(qf2$winner, sf2$winner)
  pf2_away <- away_team(qf2$winner, sf2$winner)
  pf2 <- play_final(pf2_home, pf2_away)
  finals_wins[pf2$winner] <- finals_wins[pf2$winner] + 1L
  finals_finish[pf2$loser] <- 3L  # eliminated week 3

  # --- Week 4: Grand Final ---
  gf_home <- home_team(pf1$winner, pf2$winner)
  gf_away <- away_team(pf1$winner, pf2$winner)
  gf <- play_final(gf_home, gf_away, gf = TRUE)
  finals_wins[gf$winner] <- finals_wins[gf$winner] + 1L
  finals_finish[gf$winner] <- 5L  # premier
  finals_finish[gf$loser]  <- 4L  # runner-up

  data.table::data.table(
    team          = names(finals_wins),
    finals_finish = as.integer(finals_finish[names(finals_wins)]),
    finals_wins   = as.integer(finals_wins),
    made_gf       = as.integer(names(finals_wins) %in% c(gf$winner, gf$loser)),
    won_gf        = as.integer(names(finals_wins) == gf$winner)
  )
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

  if (!is.null(seed)) set.seed(seed)

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
    # Clean up leaked file connections from pipeline downloads to avoid
    # collisions with parallel PSOCK cluster connections (Windows)
    open_cons <- rownames(showConnections(all = FALSE))
    for (con_id in open_cons) {
      con_num <- as.integer(con_id)
      if (con_num > 2L) {  # skip stdin/stdout/stderr
        tryCatch(close(getConnection(con_num)), error = function(e) NULL)
      }
    }
    gc()

    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)

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

    chunk_results <- tryCatch(
      parallel::parLapply(cl, chunks, run_sim_batch),
      error = function(e) {
        cli::cli_warn("Parallel cluster failed ({conditionMessage(e)}), falling back to sequential")
        NULL
      }
    )

    if (!is.null(chunk_results)) {
      all_ladders <- data.table::rbindlist(lapply(chunk_results, `[[`, "ladder"))
      all_finals  <- data.table::rbindlist(lapply(chunk_results, `[[`, "finals"))
      all_games   <- if (keep_games) {
        data.table::rbindlist(lapply(chunk_results, `[[`, "games"))
      } else {
        NULL
      }
    } else {
      # Sequential fallback
      if (verbose) cli::cli_progress_bar("Simulating seasons", total = n_sims)
      fallback <- run_sim_batch(seq_len(n_sims))
      all_ladders <- fallback$ladder
      all_finals  <- fallback$finals
      all_games   <- fallback$games
      if (verbose) cli::cli_progress_done()
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
  ladder_sum <- sim_results$ladders[, .(
    avg_wins       = mean(wins),
    avg_losses     = mean(losses),
    avg_draws      = mean(draws),
    avg_percentage = mean(percentage),
    avg_pf_pg      = mean(points_for / played),
    avg_pa_pg      = mean(points_against / played),
    avg_rank       = mean(rank),
    top_8_pct      = mean(rank <= 8),
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
    PF      = sprintf("%.1f", avg_pf_pg),
    PA      = sprintf("%.1f", avg_pa_pg),
    Pct     = sprintf("%.1f", avg_percentage),
    `1st`   = sprintf("%.1f%%", top_1_pct * 100),
    Top4    = sprintf("%.0f%%", top_4_pct * 100),
    Top8    = sprintf("%.0f%%", top_8_pct * 100),
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
