# AFL Ladder Calculation
# ======================
# Pure ladder construction from game results: pivots fixtures into per-team
# rows and applies AFL tiebreak rules (points, percentage). Companion
# simulation files: R/season_sim.R drives Monte Carlo seasons that consume
# this; R/finals_sim.R simulates the top-8 bracket on top of the ladder.

#' AFL Ladder Calculation
#'
#' Functions for constructing the AFL ladder from game results, applying
#' standard tiebreak rules (ladder points, then percentage). Used as the
#' input to finals simulation (R/finals_sim.R) and the season Monte Carlo
#' (R/season_sim.R).
#'
#' @name ladder
NULL


# Resolve first matching column name from a vector of candidates
# Used by prepare_sim_data() and calculate_final_ladder()
.resolve_col <- function(dt, candidates) {
  for (cand in candidates) {
    if (cand %in% names(dt)) return(cand)
  }
  NULL
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

  rnd_col <- .resolve_col(fix_dt, c("round_number", "roundnum"))
  ht_col  <- .resolve_col(fix_dt, c("home_team_name", "home_team"))
  at_col  <- .resolve_col(fix_dt, c("away_team_name", "away_team"))
  hs_col  <- .resolve_col(fix_dt, c("home_score", "home_points"))
  as_col  <- .resolve_col(fix_dt, c("away_score", "away_points"))

  if (is.null(rnd_col) || is.null(ht_col) || is.null(at_col)) {
    cli::cli_abort("Could not find required fixture columns.")
  }

  mid_col <- .resolve_col(fix_dt, c("match_id", "providerId"))

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
  if (is.na(max_round)) max_round <- AFL_MAX_REGULAR_ROUNDS
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
    pred_ht <- .resolve_col(pred_dt, c("home_team", "home_team_name"))
    pred_at <- .resolve_col(pred_dt, c("away_team", "away_team_name"))

    # Join on match_id (predictions may have home/away swapped vs fixtures)
    pred_mid <- .resolve_col(pred_dt, c("match_id", "providerId"))

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


