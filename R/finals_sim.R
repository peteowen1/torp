# AFL Finals Simulation
# =====================
# Top-8 bracket simulation on top of a ladder (qualifying / elimination /
# semi-finals / preliminary finals / Grand Final). Match-level helper
# `simulate_match()` is shared with the season simulation.


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
  # Mutable state held in an environment so play_final() can update hot ratings
  # between matches without using <<- (project convention: prefer environments
  # over scope-climbing assignment, see CLAUDE.md "Package State").
  state <- new.env(parent = emptyenv())
  state$ratings <- stats::setNames(sim_teams_dt$torp, sim_teams_dt$team)

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

  # Helper: play a finals match, update ratings in `state`, return winner/loser
  play_final <- function(home, away, gf = FALSE) {
    ha <- finals_home_advantage(home, away, fam_lookup, gf = gf)
    res <- simulate_match(state$ratings[home], state$ratings[away],
                          home_advantage = ha, allow_draw = FALSE)

    # Hot rating adjustment — env mutation, not <<-
    shift <- SIM_RATING_SHIFT * (res$result - res$estimate)
    state$ratings[home] <- state$ratings[home] + shift
    state$ratings[away] <- state$ratings[away] - shift

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
