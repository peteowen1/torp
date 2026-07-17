test_that("calculate_ladder produces correct results for known fixture", {
  # 4-team round-robin: A beats B by 30, C beats D by 10,
  # A beats C by 20, B beats D by 5, A beats D by 50, B loses to C by 15
  games <- data.table::data.table(
    home_team  = c("A", "C", "A", "B", "A", "C"),
    away_team  = c("B", "D", "C", "D", "D", "B"),
    home_score = c(100L, 80L, 90L, 60L, 120L, 85L),
    away_score = c( 70L, 70L, 70L, 55L,  70L, 70L),
    result     = c( 30L, 10L, 20L,  5L,  50L, 15L)
  )

  ladder <- calculate_ladder(games)

  expect_s3_class(ladder, "data.table")
  expect_equal(nrow(ladder), 4)
  expect_true(all(c("team", "wins", "losses", "percentage", "rank") %in% names(ladder)))

  # A won all 3 games
 team_a <- ladder[team == "A"]
  expect_equal(team_a$wins, 3)
  expect_equal(team_a$losses, 0)
  expect_equal(team_a$ladder_points, 12)
  expect_equal(team_a$rank, 1)

  # Percentage = points_for / points_against * 100
  expect_equal(team_a$percentage, team_a$points_for / team_a$points_against * 100)

  # All teams played 3 games
  expect_true(all(ladder$played == 3))

  # Ranks are 1-4
  expect_equal(sort(ladder$rank), 1:4)
})

test_that("calculate_ladder resolves points/percentage ties via H2H, not points-for", {
  # A and B finish tied on BOTH ladder_points (4) and percentage (91.667%),
  # but B has the higher points_for (220 vs 110) -- the pre-fix (illegitimate,
  # not an actual Reg 2.5(c) criterion) 3rd tiebreak would have ranked B
  # above A. A beat B 70-20 head-to-head, so the correct chain (H2H points
  # first) must rank A above B instead.
  games <- data.table::data.table(
    home_team  = c("A", "A", "B"),
    away_team  = c("B", "C", "D"),
    home_score = c(70L, 40L, 200L),
    away_score = c(20L, 100L, 170L),
    result     = c(50L, -60L, 30L)
  )

  set.seed(1)
  ladder <- calculate_ladder(games)

  team_a <- ladder[team == "A"]
  team_b <- ladder[team == "B"]

  expect_equal(team_a$ladder_points, team_b$ladder_points)
  expect_equal(team_a$percentage, team_b$percentage)
  expect_gt(team_b$points_for, team_a$points_for)
  expect_lt(team_a$rank, team_b$rank)
})

test_that("calculate_ladder falls back to lot (reproducibly) when tied teams never met", {
  # E/F are tied on ladder_points (4) and percentage (200%) but never played
  # each other (E only played G, F only played H) -- no H2H meetings to
  # resolve the tie, so it falls through to "drawn by lot". G/H form a
  # second, independent tied block (0 pts, 50% each), also with no mutual
  # meeting -- exercises two simultaneous tied blocks in one ladder.
  games <- data.table::data.table(
    home_team  = c("E", "F"),
    away_team  = c("G", "H"),
    home_score = c(100L, 100L),
    away_score = c(50L, 50L),
    result     = c(50L, 50L)
  )

  set.seed(2)
  ladder1 <- calculate_ladder(games)
  set.seed(2)
  ladder2 <- calculate_ladder(games)

  expect_equal(nrow(ladder1), 4)
  expect_setequal(ladder1$team, c("E", "F", "G", "H"))
  # Same seed -> same "drawn by lot" resolution (reproducible)
  expect_equal(ladder1$team, ladder2$team)
})

test_that("calculate_ladder handles draws correctly", {
  games <- data.table::data.table(
    home_team  = c("A", "A"),
    away_team  = c("B", "C"),
    home_score = c(80L, 80L),
    away_score = c(80L, 60L),
    result     = c(0L, 20L)
  )

  ladder <- calculate_ladder(games)

  team_a <- ladder[team == "A"]
  expect_equal(team_a$wins, 1)
  expect_equal(team_a$draws, 1)
  expect_equal(team_a$ladder_points, 6) # 1 win (4) + 1 draw (2)
})

test_that("simulate_finals produces valid bracket results (Final Ten System)", {
  set.seed(123)

  teams <- paste0("Team", 1:18)
  ladder <- data.table::data.table(
    team = teams,
    rank = 1:18,
    wins = 18:1,
    percentage = seq(130, 95, length.out = 18)
  )

  sim_teams <- data.table::data.table(
    team = teams,
    torp = seq(80, 10, length.out = 18)
  )

  finals <- simulate_finals(ladder, sim_teams)

  expect_s3_class(finals, "data.table")
  expect_equal(nrow(finals), 10) # Top 10 in finals (Final Ten System, Reg 2.6(a))
  expect_true(all(finals$team %in% teams[1:10]))

  # Exactly 1 premier (finals_finish == 5)
  expect_equal(sum(finals$finals_finish == 5), 1)

  # Exactly 1 runner-up (finals_finish == 4)
  expect_equal(sum(finals$finals_finish == 4), 1)

  # 2 teams made the GF
  expect_equal(sum(finals$made_gf), 2)

  # 1 team won the GF
  expect_equal(sum(finals$won_gf), 1)

  # All finals_finish values are valid (0-5): 0=lost Wildcard, 1=lost EF,
  # 2=lost Semi, 3=lost Prelim, 4=runner-up, 5=premier
  expect_true(all(finals$finals_finish %in% 0:5))

  # Wildcard losers finish at week 0
  expect_equal(sum(finals$finals_finish == 0), 2)
  # EF losers finish at week 1
  expect_equal(sum(finals$finals_finish == 1), 2)
  # SF losers finish at week 2
  expect_equal(sum(finals$finals_finish == 2), 2)
  # PF losers finish at week 3
  expect_equal(sum(finals$finals_finish == 3), 2)
})

test_that("simulate_finals aborts with fewer than 10 teams", {
  teams <- paste0("Team", 1:8)
  ladder <- data.table::data.table(
    team = teams, rank = 1:8,
    wins = 8:1, percentage = seq(120, 95, length.out = 8)
  )
  sim_teams <- data.table::data.table(team = teams, torp = seq(50, 10, length.out = 8))

  expect_error(simulate_finals(ladder, sim_teams), "at least 10 teams")
})

test_that("9th/10th ranked teams can win the premiership via the Wildcard Round", {
  set.seed(7)

  teams <- paste0("Team", 1:18)
  ladder <- data.table::data.table(
    team = teams, rank = 1:18,
    wins = 18:1, percentage = seq(130, 95, length.out = 18)
  )
  # Modest, realistic-scale spread (not the huge separations used in the
  # deterministic bracket-topology tests below) so upsets are common enough
  # for 9th/10th to have a real, observable shot at the flag.
  sim_teams <- data.table::data.table(
    team = teams, torp = seq(20, -20, length.out = 18)
  )

  n_sims <- 2000
  premiers <- vapply(seq_len(n_sims), function(i) {
    finals <- simulate_finals(ladder, sim_teams)
    finals[finals_finish == 5, team]
  }, character(1))

  rank_9_10 <- teams[9:10]
  expect_gt(sum(premiers %in% rank_9_10), 0)
})

# --------------------------------------------------------------------------
# Final Ten System bracket-topology tests (torp#106) — deterministic via a
# mocked simulate_match() that always sends the higher `home_torp` value to
# victory, regardless of home/away label or the actual home_advantage arg.
# This decouples the bracket TOPOLOGY (who plays whom) from match-simulation
# randomness, so pairings can be traced exactly across a whole bracket.
# --------------------------------------------------------------------------

test_that("Wildcard reseeding places the higher-original-rank WF winner into the 7th EF seed", {
  # WF1 (7th v 10th): 10th UPSETS 7th (an original rank-10 team wins).
  # WF2 (8th v 9th): 8th wins normally.
  # Correct reseed: seed7 = better ORIGINAL rank of {Team10, Team8} = Team8
  # (feeds EF2 v Team6); seed8 = Team10 (feeds EF1 v Team5) -- NOT simply
  # "WF1's own winner keeps the 7 seed" (which would wrongly give Team10 the
  # 7 seed here, despite its original rank being worse than Team8's).
  teams <- paste0("Team", 1:10)
  ladder <- data.table::data.table(team = teams, rank = 1:10)
  sim_teams <- data.table::data.table(team = teams, torp = rep(0, 10))

  power <- c(Team1 = 20, Team2 = 100, Team3 = 80, Team4 = 90, Team5 = 50,
             Team6 = 60, Team7 = 5, Team8 = 100, Team9 = 20, Team10 = 10)

  testthat::local_mocked_bindings(
    simulate_match = function(home_torp, away_torp,
                              home_advantage = SIM_HOME_ADVANTAGE, allow_draw = TRUE) {
      list(result = if (home_torp >= away_torp) 1L else -1L,
           home_score = 100L, away_score = 90L, estimate = 0)
    }
  )
  sim_teams[, torp := power[team]]

  finals <- simulate_finals(ladder, sim_teams)

  # Team7/Team9 lost their Wildcard tie.
  expect_equal(finals[team == "Team7", finals_finish], 0L)
  expect_equal(finals[team == "Team9", finals_finish], 0L)

  # Correct reseed: Team8 (seed7, paired v Team6) upsets Team6 and advances;
  # Team10 (seed8, paired v Team5) loses to Team5. If reseeding instead used
  # the raw Wildcard bracket slot (Team10 -> "7 seed" v Team6, Team8 ->
  # "8 seed" v Team5), Team5 would be the one eliminated here instead.
  expect_equal(finals[team == "Team10", finals_finish], 1L)   # lost EF (v Team5)
  expect_equal(finals[team == "Team6",  finals_finish], 1L)   # lost EF (upset by reseeded Team8)
  expect_gt(finals[team == "Team5", finals_finish], 1L)        # survived EF1
  expect_gt(finals[team == "Team8", finals_finish], 1L)        # survived EF2 (the reseeded upset winner)
})

test_that("Preliminary Final crossover pairs QF1-winner x SF2-winner and QF2-winner x SF1-winner", {
  # Wildcard Round resolved "normally" (7th/8th both win) so reseeding is
  # trivial here and cannot confound the PF-crossover signal below.
  teams <- paste0("Team", 1:10)
  ladder <- data.table::data.table(team = teams, rank = 1:10)
  sim_teams <- data.table::data.table(team = teams, torp = rep(0, 10))

  # Power order engineered so PF1's winner DIFFERS depending on whether its
  # opponent is the SF2 winner (Team3, correct/crossed) or the SF1 winner
  # (Team5, the pre-2026 uncrossed bug): Team4 > Team3 but Team5 > Team4.
  power <- c(Team1 = 20, Team2 = 100, Team3 = 80, Team4 = 90, Team5 = 95,
             Team6 = 70, Team7 = 45, Team8 = 60, Team9 = 50, Team10 = 40)

  testthat::local_mocked_bindings(
    simulate_match = function(home_torp, away_torp,
                              home_advantage = SIM_HOME_ADVANTAGE, allow_draw = TRUE) {
      list(result = if (home_torp >= away_torp) 1L else -1L,
           home_score = 100L, away_score = 90L, estimate = 0)
    }
  )
  sim_teams[, torp := power[team]]

  finals <- simulate_finals(ladder, sim_teams)

  # Correct crossed bracket: PF1 = Team4 (QF1 winner) v Team3 (SF2 winner)
  # -> Team4 wins; PF2 = Team2 (QF2 winner) v Team5 (SF1 winner) -> Team2
  # wins. GF participants are exactly {Team4, Team2}. The pre-2026 uncrossed
  # bug (PF1 = QF1 x SF1, PF2 = QF2 x SF2) would instead pit Team4 v Team5
  # in PF1 -- Team5 (95) beats Team4 (90) -- swapping Team5 into the GF in
  # Team4's place.
  expect_setequal(finals[made_gf == 1, team], c("Team4", "Team2"))
  expect_equal(finals[team == "Team4", finals_finish], 4L)  # runner-up
  expect_equal(finals[team == "Team2", finals_finish], 5L)  # premier
  expect_equal(finals[team == "Team5", finals_finish], 3L)  # lost PF2 (crossed opponent)
  expect_equal(finals[team == "Team3", finals_finish], 3L)  # lost PF1 (crossed opponent)
})

# --------------------------------------------------------------------------
# Finals venue familiarity tests
# --------------------------------------------------------------------------

test_that("finals_home_advantage falls back to standard HA when fam_lookup is NULL", {
  ha <- finals_home_advantage("Collingwood", "Sydney Swans", NULL, gf = TRUE)
  expect_equal(ha, SIM_HOME_ADVANTAGE)
})

test_that("gf_home_advantage returns positive for MCG tenant vs interstate", {
  fam <- c("Collingwood" = 0.50, "West Coast Eagles" = 0.05)
  ha <- gf_home_advantage("Collingwood", "West Coast Eagles", fam)
  expect_gt(ha, 0)
  expect_equal(ha, (0.50 - 0.05) * SIM_GF_FAMILIARITY_SCALE)
})

test_that("gf_home_advantage returns near-zero for two MCG tenants", {
  fam <- c("Collingwood" = 0.50, "Melbourne" = 0.48)
  ha <- gf_home_advantage("Collingwood", "Melbourne", fam)
  expect_lt(abs(ha), 0.5)
})

test_that("finals_home_advantage uses MCG familiarity for Victorian home teams", {
  fam <- c("Collingwood" = 0.50, "Sydney Swans" = 0.08)
  ha <- finals_home_advantage("Collingwood", "Sydney Swans", fam, gf = FALSE)
  expect_equal(ha, (0.50 - 0.08) * SIM_GF_FAMILIARITY_SCALE)
})

test_that("finals_home_advantage uses standard HA for interstate home teams", {
  fam <- c("Brisbane Lions" = 0.08, "Collingwood" = 0.50)
  ha <- finals_home_advantage("Brisbane Lions", "Collingwood", fam, gf = FALSE)
  expect_equal(ha, SIM_HOME_ADVANTAGE)
})

test_that("finals_home_advantage always uses MCG for GF regardless of teams", {
  fam <- c("Brisbane Lions" = 0.08, "Sydney Swans" = 0.06)
  ha <- finals_home_advantage("Brisbane Lions", "Sydney Swans", fam, gf = TRUE)
  # GF always at MCG, so familiarity applies even for interstate teams
  expect_equal(ha, (0.08 - 0.06) * SIM_GF_FAMILIARITY_SCALE)
})

test_that("gf_home_advantage is negative when away team has higher familiarity", {
  fam <- c("Fremantle" = 0.05, "Richmond" = 0.55)
  ha <- gf_home_advantage("Fremantle", "Richmond", fam)
  expect_lt(ha, 0)
})

test_that("gf_home_advantage defaults to 0 for unknown teams", {
  fam <- c("Collingwood" = 0.50)
  ha <- gf_home_advantage("Collingwood", "UnknownFC", fam)
  expect_equal(ha, 0.50 * SIM_GF_FAMILIARITY_SCALE)
})

test_that("simulate_finals accepts gf_familiarity parameter", {
  set.seed(123)
  teams <- paste0("Team", 1:18)
  ladder <- data.table::data.table(
    team = teams, rank = 1:18,
    wins = 18:1, percentage = seq(130, 95, length.out = 18)
  )
  sim_teams <- data.table::data.table(
    team = teams, torp = seq(80, 10, length.out = 18)
  )
  gf_fam <- data.table::data.table(
    team = teams,
    gf_familiarity = c(0.50, 0.45, 0.08, 0.05, rep(0.10, 14))
  )

  finals <- simulate_finals(ladder, sim_teams, gf_fam)
  expect_s3_class(finals, "data.table")
  expect_equal(sum(finals$won_gf), 1)
  expect_equal(sum(finals$made_gf), 2)
})

test_that("simulate_finals works without gf_familiarity (backward compat)", {
  set.seed(123)
  teams <- paste0("Team", 1:18)
  ladder <- data.table::data.table(
    team = teams, rank = 1:18,
    wins = 18:1, percentage = seq(130, 95, length.out = 18)
  )
  sim_teams <- data.table::data.table(
    team = teams, torp = seq(80, 10, length.out = 18)
  )

  finals <- simulate_finals(ladder, sim_teams)
  expect_s3_class(finals, "data.table")
  expect_equal(sum(finals$won_gf), 1)
})


test_that("simulate_afl_season returns valid structure", {
  set.seed(42)

  teams <- create_test_sim_teams(18)
  games <- create_test_sim_games(n_rounds = 23, teams = teams$team)

  results <- simulate_afl_season(
    season       = 2025,
    n_sims       = 5,
    n_cores      = 1L,
    team_ratings = teams,
    fixtures     = data.table::data.table(
      roundnum  = games$roundnum,
      home_team = games$home_team,
      away_team = games$away_team
    ),
    verbose = FALSE
  )

  expect_s3_class(results, "torp_sim_results")
  expect_equal(results$season, 2025)
  expect_equal(results$n_sims, 5)

  # Ladders: 18 teams per sim, 5 sims
  expect_equal(nrow(results$ladders), 18 * 5)
  expect_true(all(c("team", "wins", "rank", "sim_id") %in% names(results$ladders)))

  # Finals: 10 teams per sim, 5 sims (Final Ten System, Reg 2.6(a))
  expect_equal(nrow(results$finals), 10 * 5)

  # Exactly 1 premier per sim
  premiers_per_sim <- results$finals[won_gf == 1, .N, by = sim_id]
  expect_true(all(premiers_per_sim$N == 1))
})

test_that("summarise_simulations produces valid summary", {
  set.seed(42)

  teams <- create_test_sim_teams(18)
  games <- create_test_sim_games(n_rounds = 23, teams = teams$team)

  results <- simulate_afl_season(
    season       = 2025,
    n_sims       = 10,
    n_cores      = 1L,
    team_ratings = teams,
    fixtures     = data.table::data.table(
      roundnum  = games$roundnum,
      home_team = games$home_team,
      away_team = games$away_team
    ),
    verbose = FALSE
  )

  summary <- summarise_simulations(results)

  expect_s3_class(summary, "data.table")
  expect_equal(nrow(summary), 18)

  # top_N_pct columns should sum to N across teams (N teams finish top-N per sim)
  expect_equal(sum(summary$top_4_pct),  4, tolerance = 0.01)
  expect_equal(sum(summary$top_6_pct),  6, tolerance = 0.01)
  expect_equal(sum(summary$top_8_pct),  8, tolerance = 0.01)
  expect_equal(sum(summary$top_10_pct), 10, tolerance = 0.01)

  # won_gf_pct should sum to 1 (exactly 1 premier per sim)
  expect_equal(sum(summary$won_gf_pct), 1, tolerance = 0.01)

  # made_gf_pct should sum to 2 (exactly 2 GF participants per sim)
  expect_equal(sum(summary$made_gf_pct), 2, tolerance = 0.01)

  # Per-stage finals-loss pcts (Final Ten System, docs/reference/
  # afl-season-rules.md): exactly 2 teams lose at each of the Wildcard,
  # Elimination, Semi, and Preliminary Final stages every sim.
  expect_equal(sum(summary$lose_wildcard_pct), 2, tolerance = 0.01)
  expect_equal(sum(summary$lose_elim_pct),     2, tolerance = 0.01)
  expect_equal(sum(summary$lose_semi_pct),     2, tolerance = 0.01)
  expect_equal(sum(summary$lose_prelim_pct),   2, tolerance = 0.01)

  # Monotonic nesting: top_1 ⊆ top_4 ⊆ top_6 ⊆ top_8 ⊆ top_10
  expect_true(all(summary$top_1_pct <= summary$top_4_pct))
  expect_true(all(summary$top_4_pct <= summary$top_6_pct))
  expect_true(all(summary$top_6_pct <= summary$top_8_pct))
  expect_true(all(summary$top_8_pct <= summary$top_10_pct))

  # W10/W90 bounds: 10th pct <= mean <= 90th pct for every team
  expect_true(all(summary$w10 <= summary$avg_wins + 1e-8))
  expect_true(all(summary$avg_wins <= summary$w90 + 1e-8))
})

test_that("simulate_season backward compat: returns data.table without return_teams", {
  teams <- create_test_sim_teams(18)
  games <- create_test_sim_games(n_rounds = 3, teams = teams$team)

  result <- simulate_season(teams, games)

  # Should return a data.table, not a list
  expect_s3_class(result, "data.table")
})

test_that("simulate_season with return_teams returns list", {
  teams <- create_test_sim_teams(18)
  games <- create_test_sim_games(n_rounds = 3, teams = teams$team)

  result <- simulate_season(teams, games, return_teams = TRUE)

  expect_type(result, "list")
  expect_true(all(c("games", "teams") %in% names(result)))
  expect_s3_class(result$games, "data.table")
  expect_s3_class(result$teams, "data.table")
})

test_that("score generation produces non-negative plausible scores", {
  set.seed(99)

  teams <- create_test_sim_teams(18)
  games <- create_test_sim_games(n_rounds = 5, teams = teams$team)

  result <- simulate_season(teams, games)

  # home_score and away_score should exist and be non-negative
  expect_true("home_score" %in% names(result))
  expect_true("away_score" %in% names(result))
  expect_true(all(result$home_score >= 0))
  expect_true(all(result$away_score >= 0))

  # Scores should be plausible (roughly 30-200 range for AFL)
  expect_true(all(result$home_score < 300))
  expect_true(all(result$away_score < 300))

  # Margin should equal home_score - away_score (approximately, due to rounding)
  score_margin <- result$home_score - result$away_score
  expect_true(all(abs(score_margin - result$result) <= 1))
})
