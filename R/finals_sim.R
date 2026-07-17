# AFL Finals Simulation
# =====================
# Final Ten System bracket simulation on top of a ladder: a sudden-death
# Wildcard Round (7th v 10th, 8th v 9th) reseeds its two winners into the
# Elimination Final 7th/8th slots, then qualifying / elimination / semi /
# preliminary finals / Grand Final proceed as before. Match-level helper
# `simulate_match()` is shared with the season simulation.
#
# GROUND TRUTH: docs/reference/afl-season-rules.md (inthegame-blog repo) --
# Reg 2.6(a) (top 10 qualify), Annexure 1 (bracket + reseeding + PF
# crossover), Reg 2.7 (hosting), Reg 2.9 (original-H&A-rank GF hosting).
# A JS port of the same rules is harness-validated in
# inthegame-blog/afl/season-sim.js's finals block (torp#106) -- this file
# is the idiomatic-R port of that same bracket topology.


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
#' Implements the 2026 Final Ten System finals bracket (AFL Regulations,
#' Reg 2.6(a) + Annexure 1): a sudden-death Wildcard Round (7th v 10th,
#' 8th v 9th), whose two winners are reseeded into the Elimination Final
#' 7th/8th slots (higher-ORIGINAL-rank winner takes 7th), then qualifying
#' finals, elimination finals, semi-finals, a crossed preliminary final pair
#' (QF1 winner v SF2 winner; QF2 winner v SF1 winner — avoids a Qualifying
#' Final rematch, Annexure 1 footnote), and the grand final. Grand Final
#' home-club status and all other hosting decisions use each team's
#' ORIGINAL end-of-H&A ladder rank throughout (Reg 2.9(a)-(b)) — the
#' Wildcard reseed only changes bracket *placement*, never `ladder_pos`.
#'
#' @param ladder_dt A data.table from [calculate_ladder()] with `team` and
#'   `rank` columns.
#' @param sim_teams_dt A data.table with `team` and `torp` (hot ratings after
#'   regular season).
#' @param gf_familiarity Optional data.table with `team` and `gf_familiarity`
#'   columns (proportion of games played at GF venue). When provided, the Grand
#'   Final home advantage is based on familiarity difference between teams.
#' @return A data.table with columns: `team`, `finals_finish` (0 = lost
#'   Wildcard, 1 = lost Elimination Final, 2 = lost Semi Final, 3 = lost
#'   Preliminary Final, 4 = runner-up, 5 = premier), `finals_wins`,
#'   `made_gf`, `won_gf`.
#' @keywords internal
simulate_finals <- function(ladder_dt, sim_teams_dt, gf_familiarity = NULL) {
  # Mutable state held in an environment so play_final() can update hot ratings
  # between matches without using <<- (project convention: prefer environments
  # over scope-climbing assignment, see CLAUDE.md "Package State").
  state <- new.env(parent = emptyenv())
  state$ratings <- stats::setNames(sim_teams_dt$torp, sim_teams_dt$team)

  # Original H&A ladder positions — NEVER mutated by the Wildcard reseed, so
  # every home/away and GF-hosting decision below (Reg 2.9(a)-(b)) reads each
  # team's real end-of-H&A finish, not its reseeded finals bracket slot.
  ladder_pos <- stats::setNames(ladder_dt$rank, ladder_dt$team)

  # GF venue familiarity lookup
  fam_lookup <- if (!is.null(gf_familiarity)) {
    stats::setNames(gf_familiarity$gf_familiarity, gf_familiarity$team)
  } else {
    NULL
  }

  top10 <- ladder_dt[rank <= 10, team]
  if (length(top10) < 10) {
    cli::cli_abort("Need at least 10 teams for finals simulation (Final Ten System), got {length(top10)}.")
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

  # Higher-ranked team (lower rank number, by ORIGINAL H&A rank) is home
  home_team <- function(a, b) {
    if (ladder_pos[a] < ladder_pos[b]) a else b
  }
  away_team <- function(a, b) {
    if (ladder_pos[a] < ladder_pos[b]) b else a
  }

  # Track results
  finals_wins  <- stats::setNames(rep(0L, 10), top10)
  finals_finish <- stats::setNames(rep(0L, 10), top10)

  # --- Week 0: Wildcard Finals (sudden death) ---
  # WF1: 7th v 10th, hosted by 7th (Reg 2.7(b))
  wf1 <- play_final(top10[7], top10[10])
  finals_wins[wf1$winner] <- finals_wins[wf1$winner] + 1L
  finals_finish[wf1$loser] <- 0L  # eliminated week 0 (Wildcard)

  # WF2: 8th v 9th, hosted by 8th (Reg 2.7(b))
  wf2 <- play_final(top10[8], top10[9])
  finals_wins[wf2$winner] <- finals_wins[wf2$winner] + 1L
  finals_finish[wf2$loser] <- 0L  # eliminated week 0 (Wildcard)

  # Wildcard reseeding (Annexure 1 footnote): the higher-ranked winner (by
  # ORIGINAL H&A rank) takes the 7th Elimination-Final seed; the lower-ranked
  # winner takes the 8th seed. `ladder_pos` (not bracket slot) drives this.
  seed7 <- if (ladder_pos[wf1$winner] < ladder_pos[wf2$winner]) wf1$winner else wf2$winner
  seed8 <- if (identical(seed7, wf1$winner)) wf2$winner else wf1$winner

  # --- Week 1: Qualifying + Elimination Finals ---
  # QF1: 1 v 4 (unaffected by the Wildcard Round — 1st-6th bye straight here)
  qf1 <- play_final(top10[1], top10[4])
  finals_wins[qf1$winner] <- finals_wins[qf1$winner] + 1L

  # QF2: 2 v 3
  qf2 <- play_final(top10[2], top10[3])
  finals_wins[qf2$winner] <- finals_wins[qf2$winner] + 1L

  # EF1: 5 v seed-8 (the reseeded Wildcard survivor, not the raw 8th-place
  # finisher). 5th always outranks a 7th-10th team, so it hosts (Reg 2.7(c)).
  ef1 <- play_final(top10[5], seed8)
  finals_wins[ef1$winner] <- finals_wins[ef1$winner] + 1L
  finals_finish[ef1$loser] <- 1L  # eliminated week 1 (Elimination Final)

  # EF2: 6 v seed-7
  ef2 <- play_final(top10[6], seed7)
  finals_wins[ef2$winner] <- finals_wins[ef2$winner] + 1L
  finals_finish[ef2$loser] <- 1L  # eliminated week 1 (Elimination Final)

  # --- Week 2: Semi Finals (unaffected by the Wildcard change) ---
  # SF1: Loser QF1 v Winner EF1
  sf1_home <- home_team(qf1$loser, ef1$winner)
  sf1_away <- away_team(qf1$loser, ef1$winner)
  sf1 <- play_final(sf1_home, sf1_away)
  finals_wins[sf1$winner] <- finals_wins[sf1$winner] + 1L
  finals_finish[sf1$loser] <- 2L  # eliminated week 2 (Semi Final)

  # SF2: Loser QF2 v Winner EF2
  sf2_home <- home_team(qf2$loser, ef2$winner)
  sf2_away <- away_team(qf2$loser, ef2$winner)
  sf2 <- play_final(sf2_home, sf2_away)
  finals_wins[sf2$winner] <- finals_wins[sf2$winner] + 1L
  finals_finish[sf2$loser] <- 2L  # eliminated week 2 (Semi Final)

  # --- Week 3: Preliminary Finals, CROSSED (Annexure 1 footnote) ---
  # PF1: Winner QF1 v Winner SF2 (not SF1 — the crossover avoids a
  # Qualifying Final rematch; predates 2026, previously a bug here)
  pf1_home <- home_team(qf1$winner, sf2$winner)
  pf1_away <- away_team(qf1$winner, sf2$winner)
  pf1 <- play_final(pf1_home, pf1_away)
  finals_wins[pf1$winner] <- finals_wins[pf1$winner] + 1L
  finals_finish[pf1$loser] <- 3L  # eliminated week 3 (Preliminary Final)

  # PF2: Winner QF2 v Winner SF1 (not SF2)
  pf2_home <- home_team(qf2$winner, sf1$winner)
  pf2_away <- away_team(qf2$winner, sf1$winner)
  pf2 <- play_final(pf2_home, pf2_away)
  finals_wins[pf2$winner] <- finals_wins[pf2$winner] + 1L
  finals_finish[pf2$loser] <- 3L  # eliminated week 3 (Preliminary Final)

  # --- Week 4: Grand Final ---
  # Home club = higher ORIGINAL H&A rank among the two Prelim winners
  # (Reg 2.9(a)-(b)) — home_team()/away_team() resolve this correctly since
  # `ladder_pos` was never touched by the Wildcard reseed above.
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
