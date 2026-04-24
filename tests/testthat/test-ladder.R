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

test_that("simulate_finals produces valid bracket results", {
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
  expect_equal(nrow(finals), 8) # Only top 8 in finals
  expect_true(all(finals$team %in% teams[1:8]))

  # Exactly 1 premier (finals_finish == 5)
  expect_equal(sum(finals$finals_finish == 5), 1)

  # Exactly 1 runner-up (finals_finish == 4)
  expect_equal(sum(finals$finals_finish == 4), 1)

  # 2 teams made the GF
  expect_equal(sum(finals$made_gf), 2)

  # 1 team won the GF
  expect_equal(sum(finals$won_gf), 1)

  # All finals_finish values are valid (1-5)
  expect_true(all(finals$finals_finish %in% 1:5))

  # EF losers finish at week 1
  expect_equal(sum(finals$finals_finish == 1), 2)
  # SF losers finish at week 2
  expect_equal(sum(finals$finals_finish == 2), 2)
  # PF losers finish at week 3
  expect_equal(sum(finals$finals_finish == 3), 2)
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

  # Finals: 8 teams per sim, 5 sims
  expect_equal(nrow(results$finals), 8 * 5)

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
