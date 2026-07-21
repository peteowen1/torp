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
    round_number = 1L,
    description = "Kick"
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
    round_number = 1L,
    description = "Handball"
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
    round_number = 1L,
    description = "Kick"
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
    round_number = 1L,
    description = "Kick"
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
    round_number = 1L,
    description = "Kick"
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
    round_number = c(1L, 2L),
    description = "Kick"
  )

  result <- create_wp_credit(pbp)

  expect_equal(nrow(result), 2L)
  expect_equal(result[match_id == "M001"]$wp_credit, 0.10)
  expect_equal(result[match_id == "M002"]$wp_credit, 0.20)
})

test_that("create_wp_credit excludes descriptive Goal/Behind/Rushed rows", {
  # B4 defensive filter: descriptive scoring rows should not contribute to
  # credit. The standard pipeline strips them upstream via
  # EPV_RELEVANT_DESCRIPTIONS, but this guard handles direct callers.
  pbp <- data.table::data.table(
    wpa = c(0.10, 0.05, 0.07),
    player_id = c("P1", "P1", "P1"),
    player_name = "Alice",
    lead_player_id = c(NA_character_, NA_character_, NA_character_),
    pos_team = c(1L, 1L, 1L),
    display_order = c(1L, 2L, 3L),
    match_id = "M001",
    team = "TeamA",
    utc_start_time = "2025-04-01T14:00",
    round_number = 1L,
    description = c("Kick", "Goal", "Behind")
  )

  result <- create_wp_credit(pbp)

  # Only the Kick row should be credited; Goal + Behind rows excluded.
  expect_equal(result$wp_credit, 0.10)
  expect_equal(result$n_disposals, 1L)
})

test_that("create_wp_credit errors when description column missing", {
  pbp <- data.table::data.table(
    wpa = 0.10,
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

  expect_error(create_wp_credit(pbp), "description")
})

# -----------------------------------------------------------------------------
# Tests for attach_per_row_wpa_split()
# -----------------------------------------------------------------------------

test_that("attach_per_row_wpa_split returns expected structure", {
  pbp <- make_wp_pbp(20)
  result <- attach_per_row_wpa_split(pbp)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), nrow(pbp))
  expect_true(all(c("wpa_disp", "wpa_recv") %in% names(result)))
  # Original columns preserved
  expect_true(all(names(pbp) %in% names(result)))
})

test_that("attach_per_row_wpa_split splits credit by disp_share", {
  pbp <- data.table::data.table(
    wpa = c(0.10),
    player_id = "P1",
    lead_player_id = "P2",
    pos_team = 1L,
    display_order = 1L,
    match_id = "M001",
    description = "Handball"
  )

  result <- attach_per_row_wpa_split(pbp, disp_share = 0.6)

  # Disposer P1 gets 60% of 0.10 = 0.06; receiver gets 40% * 1 = 0.04
  expect_equal(result$wpa_disp, 0.06)
  expect_equal(result$wpa_recv, 0.04)
})

test_that("attach_per_row_wpa_split gives 100% to disposer when no receiver", {
  pbp <- data.table::data.table(
    wpa = c(0.10),
    player_id = "P1",
    lead_player_id = NA_character_,
    pos_team = 1L,
    display_order = 1L,
    match_id = "M001",
    description = "Kick"
  )

  result <- attach_per_row_wpa_split(pbp, disp_share = 0.5)

  expect_equal(result$wpa_disp, 0.10)
  expect_equal(result$wpa_recv, 0)
})

test_that("attach_per_row_wpa_split flips receiver credit on turnovers", {
  pbp <- data.table::data.table(
    wpa = c(0.10),
    player_id = "P1",
    lead_player_id = "P2",
    pos_team = -1L,
    display_order = 1L,
    match_id = "M001",
    description = "Kick"
  )

  result <- attach_per_row_wpa_split(pbp, disp_share = 0.5)

  # Receiver P2 gets (1-0.5) * 0.10 * (-1) = -0.05, credited via lead_player_id
  expect_equal(result$wpa_recv, -0.05)
})

test_that("attach_per_row_wpa_split marks excluded rows as NA in both columns", {
  pbp <- data.table::data.table(
    wpa = c(0.10, NA_real_, 0.05, 0.07),
    player_id = c("P1", "P1", NA_character_, "P1"),
    lead_player_id = c(NA_character_, NA_character_, NA_character_, NA_character_),
    pos_team = c(1L, 1L, 1L, 1L),
    display_order = 1:4,
    match_id = "M001",
    description = c("Kick", "Kick", "Kick", "Goal")
  )

  result <- attach_per_row_wpa_split(pbp)

  expect_false(is.na(result$wpa_disp[1]))
  expect_true(is.na(result$wpa_disp[2]))   # NA wpa
  expect_true(is.na(result$wpa_disp[3]))   # NA player_id
  expect_true(is.na(result$wpa_disp[4]))   # descriptive "Goal" row
  expect_true(all(is.na(result$wpa_recv[2:4])))
})

test_that("attach_per_row_wpa_split errors on missing columns", {
  bad_pbp <- data.frame(x = 1)
  expect_error(attach_per_row_wpa_split(bad_pbp), "Missing required columns")
})

test_that("attach_per_row_wpa_split reproduces create_wp_credit totals exactly", {
  # Acceptance gate from docs/plans/AFL_CHAIN_PARQUET_PLAN.md Stage 1:
  # sum(wpa_disp) by player_id + sum(wpa_recv) by lead_player_id, joined,
  # must reproduce create_wp_credit()'s per-player totals exactly.
  set.seed(42)

  n <- 400
  players <- paste0("P", 1:12)
  matches <- c("M001", "M002", "M003")

  pbp <- data.table::data.table(
    wpa = round(rnorm(n, 0, 0.04), 5),
    player_id = sample(players, n, replace = TRUE),
    player_name = "Name",
    lead_player_id = sample(c(players, NA_character_), n, replace = TRUE,
                             prob = c(rep(0.85 / length(players), length(players)), 0.15)),
    pos_team = sample(c(1L, -1L), n, replace = TRUE, prob = c(0.85, 0.15)),
    display_order = seq_len(n),
    match_id = sample(matches, n, replace = TRUE),
    team = "TeamA",
    utc_start_time = "2025-04-01T14:00",
    round_number = 1L,
    description = sample(
      c("Kick", "Handball", "Mark", "Goal", "Behind", "Rushed"),
      n, replace = TRUE, prob = c(0.35, 0.35, 0.15, 0.05, 0.05, 0.05)
    )
  )
  # Sprinkle in some NA wpa / NA player_id rows, like real defensive edge cases
  na_idx <- sample(n, 15)
  pbp$wpa[na_idx[1:8]] <- NA_real_
  pbp$player_id[na_idx[9:15]] <- NA_character_

  official <- create_wp_credit(pbp)

  split <- attach_per_row_wpa_split(pbp)

  disp_totals <- split[, .(wpa_disp_sum = sum(wpa_disp, na.rm = TRUE)),
                        by = .(player_id, match_id)]
  recv_totals <- split[!is.na(lead_player_id),
                        .(wpa_recv_sum = sum(wpa_recv, na.rm = TRUE)),
                        by = .(lead_player_id, match_id)]

  rebuilt <- merge(
    disp_totals, recv_totals,
    by.x = c("player_id", "match_id"), by.y = c("lead_player_id", "match_id"),
    all.x = TRUE
  )
  rebuilt[, wpa_recv_sum := data.table::fcoalesce(wpa_recv_sum, 0)]
  rebuilt[, wpa_total := wpa_disp_sum + wpa_recv_sum]
  data.table::setkey(rebuilt, match_id, player_id)

  cmp <- merge(
    official[, .(player_id, match_id, wp_disp_credit, wp_recv_credit, wp_credit)],
    rebuilt,
    by = c("player_id", "match_id")
  )

  expect_equal(nrow(cmp), nrow(official))
  expect_equal(cmp$wpa_disp_sum, cmp$wp_disp_credit, tolerance = 1e-8)
  expect_equal(cmp$wpa_recv_sum, cmp$wp_recv_credit, tolerance = 1e-8)
  expect_equal(cmp$wpa_total, cmp$wp_credit, tolerance = 1e-8)
})
