# Tests for create_wp_credit()

# Helper to create minimal PBP data for WP credit testing
make_wp_pbp <- function(n = 10) {
  data.table::data.table(
    wpa = rnorm(n, 0, 0.05),
    player_id = rep(c("P1", "P2"), length.out = n),
    player_name = rep(c("Alice", "Bob"), length.out = n),
    lead_player_id = rep(c("P2", "P1"), length.out = n),
    pos_team = rep(c(1L, -1L), length.out = n),
    display_order = seq_len(n),
    match_id = "M001",
    team = "TeamA",
    utc_start_time = "2025-04-01T14:00",
    round_number = 1L
  )
}


test_that("create_wp_credit returns expected structure", {
  pbp <- make_wp_pbp(20)
  result <- create_wp_credit(pbp)

  expect_s3_class(result, "data.table")
  expected_cols <- c("player_id", "player_name", "match_id", "team", "season",
                     "round", "utc_start_time", "wp_credit", "wp_disp_credit",
                     "wp_recv_credit", "n_disposals", "n_receptions",
                     "max_play_wpa", "max_play_display_order", "max_play_role")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("create_wp_credit splits credit by disp_share", {
  pbp <- data.table::data.table(
    wpa = c(0.10),
    player_id = "P1",
    player_name = "Alice",
    lead_player_id = "P2",
    pos_team = 1L,
    display_order = 1L,
    match_id = "M001",
    team = "TeamA",
    utc_start_time = "2025-04-01T14:00",
    round_number = 1L
  )

  result <- create_wp_credit(pbp, disp_share = 0.6)

  # Disposer P1 gets 60% of 0.10 = 0.06
  p1 <- result[player_id == "P1"]
  expect_equal(p1$wp_disp_credit, 0.06)
  expect_equal(p1$n_disposals, 1L)
})

test_that("create_wp_credit gives 100% to disposer when no receiver", {
  pbp <- data.table::data.table(
    wpa = c(0.10),
    player_id = "P1",
    player_name = "Alice",
    lead_player_id = NA_character_,
    pos_team = 1L,
    display_order = 1L,
    match_id = "M001",
    team = "TeamA",
    utc_start_time = "2025-04-01T14:00",
    round_number = 1L
  )

  result <- create_wp_credit(pbp, disp_share = 0.5)

  p1 <- result[player_id == "P1"]
  expect_equal(p1$wp_disp_credit, 0.10)
  expect_equal(p1$wp_recv_credit, 0)
})

test_that("create_wp_credit flips receiver credit on turnovers", {
  # pos_team = -1 means turnover; need both players as disposers to appear in output
  pbp <- data.table::data.table(
    wpa = c(0.10, 0.02),
    player_id = c("P1", "P2"),
    player_name = c("Alice", "Bob"),
    lead_player_id = c("P2", NA_character_),
    pos_team = c(-1L, 1L),
    display_order = c(1L, 2L),
    match_id = "M001",
    team = "TeamA",
    utc_start_time = "2025-04-01T14:00",
    round_number = 1L
  )

  result <- create_wp_credit(pbp, disp_share = 0.5)

  # Receiver P2 gets (1-0.5) * 0.10 * (-1) = -0.05
  p2 <- result[player_id == "P2"]
  expect_equal(p2$wp_recv_credit, -0.05)
})

test_that("create_wp_credit errors on missing columns", {
  bad_pbp <- data.frame(x = 1)
  expect_error(create_wp_credit(bad_pbp), "Missing required columns")
})

test_that("create_wp_credit handles NA wpa rows", {
  pbp <- make_wp_pbp(10)
  pbp$wpa[1:3] <- NA_real_

  result <- create_wp_credit(pbp)

  # Should have fewer total disposals since NA rows are dropped
  total_disps <- sum(result$n_disposals)
  expect_equal(total_disps, 7L)
})

test_that("create_wp_credit identifies peak play correctly", {
  pbp <- data.table::data.table(
    wpa = c(0.01, 0.50, -0.02),
    player_id = c("P1", "P1", "P1"),
    player_name = "Alice",
    lead_player_id = c(NA_character_, NA_character_, NA_character_),
    pos_team = c(1L, 1L, 1L),
    display_order = c(10L, 20L, 30L),
    match_id = "M001",
    team = "TeamA",
    utc_start_time = "2025-04-01T14:00",
    round_number = 1L
  )

  result <- create_wp_credit(pbp)

  expect_equal(result$max_play_display_order, 20L)
  expect_equal(result$max_play_wpa, 0.50)
  expect_equal(result$max_play_role, "disposer")
})

test_that("create_wp_credit aggregates across multiple games", {
  pbp <- data.table::data.table(
    wpa = c(0.10, 0.20),
    player_id = c("P1", "P1"),
    player_name = "Alice",
    lead_player_id = c(NA_character_, NA_character_),
    pos_team = c(1L, 1L),
    display_order = c(1L, 1L),
    match_id = c("M001", "M002"),
    team = "TeamA",
    utc_start_time = c("2025-04-01T14:00", "2025-04-08T14:00"),
    round_number = c(1L, 2L)
  )

  result <- create_wp_credit(pbp)

  expect_equal(nrow(result), 2L)
  expect_equal(result[match_id == "M001"]$wp_credit, 0.10)
  expect_equal(result[match_id == "M002"]$wp_credit, 0.20)
})
