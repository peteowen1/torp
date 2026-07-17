# AFL Ladder Calculation
# ======================
# Pure ladder construction from game results: pivots fixtures into per-team
# rows and applies the full Reg 2.5(c) tiebreak chain (points -> percentage
# -> H2H points -> H2H percentage -> lot, docs/reference/afl-season-rules.md
# S1.3, inthegame-blog repo). Companion simulation files: R/season_sim.R
# drives Monte Carlo seasons that consume this; R/finals_sim.R simulates the
# Final Ten System bracket on top of the ladder.

#' AFL Ladder Calculation
#'
#' Functions for constructing the AFL ladder from game results, applying
#' the official tiebreak chain (ladder points, then percentage, then head-
#' to-head points/percentage between tied clubs, then lot -- Reg 2.5(c)).
#' Used as the input to finals simulation (R/finals_sim.R) and the season
#' Monte Carlo (R/season_sim.R).
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
# Ladder tiebreak chain (Reg 2.5(c), docs/reference/afl-season-rules.md S1.3)
# --------------------------------------------------------------------------

#' Break ties within an AFL ladder using the official Reg 2.5(c) chain
#'
#' `ladder` must already be sorted by `-ladder_points, -percentage`. Any
#' maximal block of 2+ consecutive teams tied on BOTH is re-ordered by: (A)
#' head-to-head premiership points from just the H&A meetings between the
#' tied clubs; (B) if still tied, head-to-head percentage from those same
#' meetings; (C) if still tied, drawn by lot. AFL is not round-robin (each
#' club plays 23 of 17 possible opponents, some twice, some once, some
#' zero), so a pair of tied clubs may have 0, 1, or 2 meetings -- whichever
#' actually happened are summed. Ported from the same rule
#' `inthegame-blog/afl/season-sim.js`'s `rankLadder()` implements (torp#106);
#' here as a one-pass `setorder()` (h2h_pts, h2h_pct, lot) rather than that
#' function's recursive mini-table, since a single per-team random `lot`
#' draw up front is sufficient to fully resolve a tied block in one sort.
#'
#' "Drawn by lot" (Reg 2.5(c)(iv)(C)) is implemented as a `stats::runif()`
#' draw per team in the residual tied block -- callers that need
#' reproducible output across repeated calls with identical `games_dt`
#' (e.g. the season Monte Carlo, which already wraps its own call in
#' `withr::local_seed()`/`set.seed()`) should seed beforehand. A genuine
#' full tie through percentage is vanishingly rare in real results but not
#' negligible in simulation (exact-tie odds are higher over integer-sampled
#' scores) -- see the rules doc's severity note.
#'
#' @param ladder A data.table with `team`, `ladder_points`, `percentage`,
#'   already sorted by `-ladder_points, -percentage`.
#' @param played_games A data.table of completed games with `home_team`,
#'   `away_team`, `home_score`, `away_score`.
#' @return `ladder`, re-ordered so any tied block satisfies the full
#'   tiebreak chain.
#' @keywords internal
.resolve_ladder_ties <- function(ladder, played_games) {
  n <- nrow(ladder)
  if (n < 2L) return(ladder)

  tied_next <- c(
    ladder$ladder_points[-n] == ladder$ladder_points[-1] &
      ladder$percentage[-n]   == ladder$percentage[-1],
    FALSE
  )
  if (!any(tied_next)) return(ladder)

  # Team-perspective rows of every completed game, for intra-block H2H sums.
  h2h_rows <- data.table::rbindlist(list(
    played_games[, .(team = home_team, opp = away_team,
                     gf = home_score, ga = away_score)],
    played_games[, .(team = away_team, opp = home_team,
                     gf = away_score, ga = home_score)]
  ))

  out_order <- integer(0)
  i <- 1L
  while (i <= n) {
    j <- i
    while (j < n && tied_next[j]) j <- j + 1L
    if (j == i) {
      out_order <- c(out_order, i)
    } else {
      block_teams <- ladder$team[i:j]
      out_order <- c(out_order, (i - 1L) + .rank_tied_block(block_teams, h2h_rows))
    }
    i <- j + 1L
  }

  ladder[out_order]
}

# Order (as an index into `block_teams`) one tied block via H2H points ->
# H2H percentage -> lot. `h2h_rows` is the full team-perspective game log;
# filtered here to just meetings BETWEEN block members.
.rank_tied_block <- function(block_teams, h2h_rows) {
  bl <- length(block_teams)
  sub <- h2h_rows[team %in% block_teams & opp %in% block_teams]

  agg <- sub[, .(
    h2h_pts = sum(data.table::fifelse(gf > ga, 4L,
                  data.table::fifelse(gf == ga, 2L, 0L))),
    h2h_pf  = sum(gf),
    h2h_pa  = sum(ga)
  ), by = team]

  full <- merge(data.table::data.table(team = block_teams), agg,
                by = "team", all.x = TRUE)
  full[is.na(h2h_pts), `:=`(h2h_pts = 0L, h2h_pf = 0, h2h_pa = 0)]
  full[, h2h_pct := data.table::fifelse(h2h_pa > 0, h2h_pf / h2h_pa * 100,
                                        data.table::fifelse(h2h_pf > 0, Inf, 0))]
  full[, lot := stats::runif(.N)]

  data.table::setorder(full, -h2h_pts, -h2h_pct, -lot)
  match(full$team, block_teams)
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
                                        data.table::fifelse(points_for > 0, Inf, 0)),
    ladder_points = wins * 4L + draws * 2L
  )]

  # Tiebreak order: ladder points -> percentage -> H2H points -> H2H
  # percentage -> lot (Reg 2.5(c), docs/reference/afl-season-rules.md S1.3).
  # Points-for is NOT an official tiebreaker anywhere in the regulation.
  data.table::setorder(ladder, -ladder_points, -percentage)
  ladder <- .resolve_ladder_ties(ladder, played)
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
                                        points_for / points_against * 100,
                                        data.table::fifelse(points_for > 0, Inf, 0)),
    ladder_points = round(expected_wins * 4)
  )]

  # Tiebreak: ladder points -> percentage. Points-for was dropped (it is not
  # an official Reg 2.5(c) criterion -- see calculate_ladder()); the full
  # H2H tiebreak chain is NOT applied here (disclosed simplification) since
  # this ladder blends real results with fractional predicted-win scores
  # for unplayed games, and a meaningful H2H-points tiebreak needs
  # deterministic W/L, not a win probability. A residual tie here (points
  # AND percentage both equal) falls back to insertion order.
  data.table::setorder(ladder, -ladder_points, -percentage)
  ladder[, rank := seq_len(.N)]

  ladder[]
}


