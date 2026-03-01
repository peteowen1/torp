# Test helpers for mock skill data
create_mock_skill_data <- function(n_players = 10, games_per_player = 8) {
  set.seed(42)
  positions <- c("KEY_DEFENDER", "MIDFIELDER", "KEY_FORWARD", "RUCK")
  pos_groups <- c("DEF", "MID", "FWD", "RUCK")

  rows <- list()
  for (p in seq_len(n_players)) {
    pos_idx <- ((p - 1) %% 4) + 1
    for (g in seq_len(games_per_player)) {
      rows[[length(rows) + 1]] <- data.frame(
        player_id = as.character(p),
        match_id = paste0("M_", p, "_", g),
        player_name = paste0("Player_", p),
        season = 2024,
        round = g,
        match_date_skill = as.Date("2024-01-01") + (g - 1) * 14,
        tog = runif(1, 0.6, 1.0),
        pos_group = pos_groups[pos_idx],
        position = positions[pos_idx],
        goals = rpois(1, if (pos_idx == 3) 2 else 0.5),
        behinds = rpois(1, 0.5),
        shots_at_goal = rpois(1, if (pos_idx == 3) 3 else 1),
        disposals = rpois(1, 20),
        kicks = rpois(1, 10),
        handballs = rpois(1, 10),
        marks = rpois(1, 5),
        tackles = rpois(1, 4),
        contested_possessions = rpois(1, 8),
        uncontested_possessions = rpois(1, 10),
        inside50s = rpois(1, 3),
        hitouts = rpois(1, if (pos_idx == 4) 25 else 0),
        extended_stats_hitouts_to_advantage = rpois(1, if (pos_idx == 4) 8 else 0),
        clearances_total_clearances = rpois(1, 3),
        contested_marks = rpois(1, 1),
        extended_stats_ground_ball_gets = rpois(1, 3),
        marks_inside50 = rpois(1, 1),
        rebound50s = rpois(1, if (pos_idx == 1) 3 else 1),
        metres_gained = rpois(1, 300),
        extended_stats_spoils = rpois(1, if (pos_idx == 1) 3 else 0),
        intercepts = rpois(1, if (pos_idx == 1) 4 else 1),
        one_percenters = rpois(1, 2),
        extended_stats_pressure_acts = rpois(1, 10),
        frees_for = rpois(1, 1),
        frees_against = rpois(1, 1),
        clangers = rpois(1, 3),
        turnovers = rpois(1, 3),
        score_involvements = rpois(1, 3),
        goal_assists = rpois(1, 1),
        disposal_efficiency_pct_x_disposals = NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }

  dt <- data.table::rbindlist(rows)

  # Set disposal efficiency: ~70% of disposals are effective
  dt[, disposal_efficiency_pct_x_disposals := round(disposals * runif(.N, 0.55, 0.85))]

  dt
}


test_that("estimate_player_skills returns correct structure", {
  skill_data <- create_mock_skill_data()
  params <- default_skill_params()

  result <- estimate_player_skills(skill_data, params = params)

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)

  # Must have core metadata columns
  expect_true(all(c("player_id", "player_name", "pos_group",
                     "n_games", "wt_games", "ref_date") %in% names(result)))

  # Should have skill columns for rate stats
  expect_true("goals_skill" %in% names(result))
  expect_true("goals_lower" %in% names(result))
  expect_true("goals_upper" %in% names(result))
  expect_true("goals_raw" %in% names(result))
  expect_true("disposals_skill" %in% names(result))
  expect_true("disposals_raw" %in% names(result))

  # Should have efficiency stats with attempts
  expect_true("goal_accuracy_skill" %in% names(result))
  expect_true("goal_accuracy_raw" %in% names(result))
  expect_true("goal_accuracy_attempts" %in% names(result))
  expect_true("goal_accuracy_wt_attempts" %in% names(result))

  # Rate stats should NOT have attempts columns
  expect_false("goals_attempts" %in% names(result))
  expect_false("goals_wt_attempts" %in% names(result))
})

test_that("player with many games has estimate close to empirical mean", {
  # Create player with many games and known stat value
  set.seed(123)
  n_games <- 50
  dt <- data.table::data.table(
    player_id = rep("P1", n_games),
    match_id = paste0("M_", seq_len(n_games)),
    player_name = rep("Test Player", n_games),
    season = rep(2024, n_games),
    round = seq_len(n_games),
    match_date_skill = as.Date("2024-01-01") + seq_len(n_games) * 7,
    tog = rep(1.0, n_games),
    pos_group = rep("MID", n_games),
    position = rep("MIDFIELDER", n_games),
    goals = rep(2, n_games),  # Exactly 2 goals per game
    behinds = rep(0, n_games),
    shots_at_goal = rep(3, n_games),
    disposals = rep(25, n_games),
    kicks = rep(12, n_games),
    handballs = rep(13, n_games),
    marks = rep(6, n_games),
    tackles = rep(5, n_games),
    contested_possessions = rep(10, n_games),
    uncontested_possessions = rep(12, n_games),
    inside50s = rep(4, n_games),
    hitouts = rep(0, n_games),
    extended_stats_hitouts_to_advantage = rep(0, n_games),
    clearances_total_clearances = rep(4, n_games),
    contested_marks = rep(1, n_games),
    extended_stats_ground_ball_gets = rep(3, n_games),
    marks_inside50 = rep(1, n_games),
    rebound50s = rep(2, n_games),
    metres_gained = rep(350, n_games),
    extended_stats_spoils = rep(1, n_games),
    intercepts = rep(2, n_games),
    one_percenters = rep(2, n_games),
    extended_stats_pressure_acts = rep(12, n_games),
    frees_for = rep(1, n_games),
    frees_against = rep(1, n_games),
    clangers = rep(3, n_games),
    turnovers = rep(3, n_games),
    score_involvements = rep(4, n_games),
    goal_assists = rep(1, n_games),
    disposal_efficiency_pct_x_disposals = rep(18, n_games)
  )

  params <- default_skill_params()
  params$min_games <- 1

  result <- estimate_player_skills(dt, params = params)

  # With 50 games at exactly 2 goals, estimate should be close to 2
  expect_equal(result$goals_skill[1], 2, tolerance = 0.3)

  # Goal accuracy: 2/3 shots -> ~0.667
  expect_equal(result$goal_accuracy_skill[1], 2/3, tolerance = 0.1)

  # Raw averages should be exact (no smoothing)
  # goals_raw = sum(goals) / sum(tog) = 2 / 1 = 2.0
  expect_equal(result$goals_raw[1], 2.0)

  # goal_accuracy_raw = sum(goals) / sum(shots) = 100 / 150 = 2/3
  expect_equal(result$goal_accuracy_raw[1], 2/3)

  # goal_accuracy_attempts = total shots = 50 * 3 = 150
  expect_equal(result$goal_accuracy_attempts[1], 150)

  # wt_attempts should be < raw attempts due to time decay
  expect_true(result$goal_accuracy_wt_attempts[1] < 150)
  expect_true(result$goal_accuracy_wt_attempts[1] > 0)
})

test_that("raw averages are per-player, not league-wide", {
  n <- 10
  dt <- data.table::data.table(
    player_id = rep(c("A", "B"), each = n),
    match_id = paste0("M_", seq_len(2 * n)),
    player_name = rep(c("Alice", "Bob"), each = n),
    season = 2024, round = rep(seq_len(n), 2),
    match_date_skill = as.Date("2024-01-01") + seq_len(2 * n) * 7,
    tog = 1.0,
    pos_group = "MID", position = "MIDFIELDER",
    goals = rep(c(1, 3), each = n),
    behinds = 0, shots_at_goal = rep(c(2, 4), each = n),
    disposals = 20, kicks = 10, handballs = 10,
    marks = 5, tackles = 4, contested_possessions = 8,
    uncontested_possessions = 10, inside50s = 3,
    hitouts = 0, extended_stats_hitouts_to_advantage = 0,
    clearances_total_clearances = 3, contested_marks = 1,
    extended_stats_ground_ball_gets = 3, marks_inside50 = 1,
    rebound50s = 2, metres_gained = 300, extended_stats_spoils = 1,
    intercepts = 2, one_percenters = 2, extended_stats_pressure_acts = 10,
    frees_for = 1, frees_against = 1, clangers = 3, turnovers = 3,
    score_involvements = 3, goal_assists = 1,
    disposal_efficiency_pct_x_disposals = 14
  )
  params <- default_skill_params()
  params$min_games <- 1
  result <- estimate_player_skills(dt, params = params)

  alice <- result[player_id == "A"]
  bob   <- result[player_id == "B"]
  expect_equal(alice$goals_raw, 1.0)
  expect_equal(bob$goals_raw, 3.0)
  expect_equal(alice$goal_accuracy_raw, 1/2)
  expect_equal(bob$goal_accuracy_raw, 3/4)
})

test_that("high prior_strength pulls estimates toward prior", {
  skill_data <- create_mock_skill_data(n_players = 5, games_per_player = 3)

  # Default params
  params_low <- default_skill_params()
  params_low$prior_games <- 1
  params_low$min_games <- 0

  # Very high prior
  params_high <- default_skill_params()
  params_high$prior_games <- 100
  params_high$min_games <- 0

  result_low <- estimate_player_skills(skill_data, params = params_low)
  result_high <- estimate_player_skills(skill_data, params = params_high)

  # With high prior, all players' estimates should be more similar
  # (closer to position mean)
  sd_low <- sd(result_low$goals_skill, na.rm = TRUE)
  sd_high <- sd(result_high$goals_skill, na.rm = TRUE)

  expect_lt(sd_high, sd_low)
})

test_that("efficiency stats are bounded between 0 and 1", {
  skill_data <- create_mock_skill_data()
  params <- default_skill_params()

  result <- estimate_player_skills(skill_data, params = params)

  eff_cols <- grep("_skill$", names(result), value = TRUE)
  eff_defs <- skill_stat_definitions()
  eff_stat_names <- eff_defs$stat_name[eff_defs$type == "efficiency"]
  eff_skill_cols <- paste0(eff_stat_names, "_skill")
  eff_skill_cols <- intersect(eff_skill_cols, names(result))

  for (col in eff_skill_cols) {
    vals <- result[[col]]
    vals <- vals[!is.na(vals)]
    if (length(vals) > 0) {
      expect_true(all(vals >= 0 & vals <= 1),
                  info = paste(col, "should be in [0,1]"))
    }
  }
})

test_that("credible intervals contain point estimate", {
  skill_data <- create_mock_skill_data()
  params <- default_skill_params()

  result <- estimate_player_skills(skill_data, params = params)

  stat_defs <- skill_stat_definitions()
  for (stat_nm in stat_defs$stat_name) {
    skill_col <- paste0(stat_nm, "_skill")
    lower_col <- paste0(stat_nm, "_lower")
    upper_col <- paste0(stat_nm, "_upper")

    if (all(c(skill_col, lower_col, upper_col) %in% names(result))) {
      est <- result[[skill_col]]
      lo <- result[[lower_col]]
      hi <- result[[upper_col]]

      valid <- !is.na(est) & !is.na(lo) & !is.na(hi)
      if (any(valid)) {
        expect_true(all(lo[valid] <= est[valid] + 1e-10),
                    info = paste(stat_nm, "lower should be <= estimate"))
        expect_true(all(est[valid] <= hi[valid] + 1e-10),
                    info = paste(stat_nm, "estimate should be <= upper"))
      }
    }
  }
})

test_that("min_games filter works correctly", {
  skill_data <- create_mock_skill_data(n_players = 5, games_per_player = 2)

  # With min_games = 10, all players should be filtered out (only 2 games each)
  params <- default_skill_params()
  params$min_games <- 10

  result <- estimate_player_skills(skill_data, params = params)
  expect_equal(nrow(result), 0)

  # With min_games = 1, all should be included
  params$min_games <- 1
  result <- estimate_player_skills(skill_data, params = params)
  expect_equal(nrow(result), 5)
})

test_that("ref_date filters correctly", {
  skill_data <- create_mock_skill_data(n_players = 3, games_per_player = 5)

  params <- default_skill_params()
  params$min_games <- 1

  # Use all data
  result_all <- estimate_player_skills(skill_data, params = params)

  # Use only first 2 games (dates: 2024-01-15, 2024-01-29)
  result_early <- estimate_player_skills(
    skill_data,
    ref_date = as.Date("2024-02-15"),
    params = params
  )

  # Early result should have fewer weighted games
  expect_true(all(result_early$wt_games <= result_all$wt_games))
})

test_that("aggregate_team_skills produces correct output", {
  skill_data <- create_mock_skill_data(n_players = 10, games_per_player = 5)
  params <- default_skill_params()
  params$min_games <- 1

  skills <- estimate_player_skills(skill_data, params = params)

  # Create mock lineups: 2 matches, 5 players per team
  lineups <- data.table::data.table(
    match_id = rep(c("MATCH1", "MATCH2"), each = 10),
    team = rep(c(rep("TeamA", 5), rep("TeamB", 5)), 2),
    player_id = rep(as.character(1:10), 2)
  )

  result <- aggregate_team_skills(skills, lineups)

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
  expect_true("match_id" %in% names(result))
  expect_true("team" %in% names(result))
  expect_true("n_players" %in% names(result))

  # Should have team sum/mean columns
  sum_cols <- grep("_team_sum$", names(result), value = TRUE)
  mean_cols <- grep("_team_mean$", names(result), value = TRUE)
  expect_true(length(sum_cols) > 0)
  expect_true(length(mean_cols) > 0)
})

test_that("player_skill_profile produces correct output structure", {
  skill_data <- create_mock_skill_data(n_players = 5, games_per_player = 8)
  params <- default_skill_params()
  params$min_games <- 1
  skills <- estimate_player_skills(skill_data, params = params)

  # Mock resolve_player to return Player_1's info
  local_mocked_bindings(
    resolve_player = function(name, ...) {
      list(player_id = "1", player_name = "Player_1",
           team = "MockTeam", position = "KEY_DEFENDER")
    }
  )

  profile <- player_skill_profile("Player_1", skills = skills)

  expect_s3_class(profile, "torp_skill_profile")
  expect_equal(profile$player_info$name, "Player_1")

  sk <- profile$skills
  expected_cols <- c("category", "stat", "type", "skill", "raw_avg",
                     "league_avg", "league_pct", "pos_avg", "pos_pct",
                     "n_games", "wt_games", "attempts", "wt_attempts",
                     "lower", "upper")
  expect_true(all(expected_cols %in% names(sk)))

  # Percentiles should be 0-100
  expect_true(all(sk$pos_pct >= 0 & sk$pos_pct <= 100, na.rm = TRUE))
  expect_true(all(sk$league_pct >= 0 & sk$league_pct <= 100, na.rm = TRUE))

  # Rate stats should have NA attempts, efficiency should have numeric
  rate_rows <- sk$type == "rate"
  eff_rows <- sk$type == "efficiency"
  expect_true(all(is.na(sk$attempts[rate_rows])))
  if (any(eff_rows)) {
    expect_true(all(!is.na(sk$attempts[eff_rows])))
  }

  # Print should not error
  expect_no_error(capture.output(print(profile)))
})

test_that(".map_position_group maps correctly", {
  expect_equal(.map_position_group("KEY_DEFENDER"), "DEF")
  expect_equal(.map_position_group("MEDIUM_DEFENDER"), "DEF")
  expect_equal(.map_position_group("MIDFIELDER"), "MID")
  expect_equal(.map_position_group("MIDFIELDER_FORWARD"), "MID")
  expect_equal(.map_position_group("KEY_FORWARD"), "FWD")
  expect_equal(.map_position_group("MEDIUM_FORWARD"), "FWD")
  expect_equal(.map_position_group("RUCK"), "RUCK")
  expect_true(is.na(.map_position_group("UNKNOWN")))
})
