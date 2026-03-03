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

test_that("simulate_afl_season returns valid structure", {
  set.seed(42)

  teams <- create_test_sim_teams(18)
  games <- create_test_sim_games(n_rounds = 23, teams = teams$team)

  results <- simulate_afl_season(
    season       = 2025,
    n_sims       = 5,
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

  # top_8_pct should sum to approximately 8/18 * 18 = 8
  expect_equal(sum(summary$top_8_pct), 8, tolerance = 0.01)

  # won_gf_pct should sum to 1 (exactly 1 premier per sim)
  expect_equal(sum(summary$won_gf_pct), 1, tolerance = 0.01)

  # Position distribution: each team's top_1 + ... sums correctly
  # (top_1 is already a subset of top_4 which is subset of top_8)
  expect_true(all(summary$top_1_pct <= summary$top_4_pct))
  expect_true(all(summary$top_4_pct <= summary$top_8_pct))
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
