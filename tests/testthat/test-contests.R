# Tests for contest extraction functions
# =======================================

# Helper: Create mock chains data with known contest patterns
# Uses camelCase column names matching raw chains data from load_chains()
create_mock_contest_chains <- function() {
  data.table::data.table(
    matchId = rep("CD_M20240101", 22),
    season = rep(2024L, 22),
    round_number = rep(1L, 22),
    displayOrder = 1:22,
    period = rep(1L, 22),
    periodSeconds = as.integer(seq(100, 2200, length.out = 22)),
    x = c(
      -62L, -21L, -21L, -25L,  # rows 1-4: kick, contest target, spoil, knock on
       30L,  30L,               # rows 5-6: contest target (T1) -> contested mark (T2) = INTERCEPT MARK
       15L,  15L,               # rows 7-8: hard ball get, loose ball get (same xy, ground ball)
       40L,  50L,               # rows 9-10: kick, gather (different xy, NOT contest)
       10L,  10L,               # rows 11-12: contest target (T1) -> uncontested mark (T2) = INTERCEPT MARK
       -5L,  -5L,               # rows 13-14: kick inside 50 result -> spoil (same xy)
       60L,  60L,               # rows 15-16: contest target (T1) -> mark on lead (T2) = INTERCEPT MARK
       20L,  20L,               # rows 17-18: hard ball get, hard ball get (same team, NOT contest)
       35L,  35L,               # rows 19-20: kick inside 50 result (T1) -> contested mark (T2) = INTERCEPT MARK
       45L,  45L                # rows 21-22: contest target (T1) -> contested mark (T1) = MARK (same team!)
    ),
    y = c(
      -17L, -35L, -35L, -35L,  # rows 1-4
       10L,  10L,               # rows 5-6
        5L,   5L,               # rows 7-8
       20L,  25L,               # rows 9-10 (different y)
       -8L,  -8L,               # rows 11-12
       12L,  12L,               # rows 13-14
      -20L, -20L,               # rows 15-16
        0L,   0L,               # rows 17-18
       15L,  15L,               # rows 19-20
       30L,  30L                # rows 21-22
    ),
    teamId = c(
      "T001", "T001", "T002", "T002",  # 1-4: kick -> contest target (T1) -> spoil (T2)
      "T001", "T002",                    # 5-6: contest target (T1) -> contested mark (T2) = INTERCEPT MARK
      "T001", "T002",                    # 7-8: hard ball (T1) -> loose ball (T2) = GROUND BALL
      "T001", "T001",                    # 9-10: same team, NOT a contest
      "T001", "T002",                    # 11-12: contest target (T1) -> uncontested mark (T2) = INTERCEPT MARK
      "T001", "T002",                    # 13-14: kick inside 50 result (T1) -> spoil (T2) = SPOIL
      "T001", "T002",                    # 15-16: contest target (T1) -> mark on lead (T2) = INTERCEPT MARK
      "T001", "T001",                    # 17-18: same team ground ball, NOT a contest
      "T001", "T002",                    # 19-20: kick inside 50 result (T1) -> contested mark (T2) = INTERCEPT MARK
      "T001", "T001"                     # 21-22: contest target (T1) -> contested mark (T1) = MARK (same team wins)
    ),
    description = c(
      "Kick", "Contest Target", "Spoil", "Contested Knock On",       # 1-4 (spoil at same xy)
      "Contest Target", "Contested Mark",                              # 5-6 INTERCEPT MARK
      "Hard Ball Get", "Loose Ball Get",                               # 7-8 GROUND BALL
      "Kick", "Gather",                                                # 9-10 (not a contest)
      "Contest Target", "Uncontested Mark",                            # 11-12 INTERCEPT MARK
      "Kick Inside 50 Result", "Spoil",                                # 13-14 SPOIL
      "Contest Target", "Mark On Lead",                                # 15-16 INTERCEPT MARK
      "Hard Ball Get", "Hard Ball Get",                                # 17-18 (same team)
      "Kick Inside 50 Result", "Contested Mark",                       # 19-20 INTERCEPT MARK
      "Contest Target", "Contested Mark"                               # 21-22 MARK (same team)
    ),
    playerId = c(
      "P001", "P002", "P003", "P004",
      "P005", "P006",
      "P007", "P008",
      "P009", "P010",
      "P011", "P012",
      "P013", "P014",
      "P015", "P016",
      "P017", "P018",
      "P019", "P020",
      "P021", "P022"
    ),
    disposal = rep(NA_character_, 22),
    shotAtGoal = rep(NA, 22),
    behindInfo = rep(NA_character_, 22),
    venueWidth = rep(136L, 22),
    venueLength = rep(155L, 22)
  )
}


# --- extract_contests() tests ---

test_that("extract_contests finds spoil outcomes via same x,y", {
  chains <- create_mock_contest_chains()
  result <- extract_contests(chains = chains, type = "aerial")

  spoils <- result[outcome == "spoil"]
  # Row 2->3: Contest Target (T1) -> Spoil (T2) at (-21,-35)
  # Row 13->14: Kick Inside 50 Result (T1) -> Spoil (T2) at (-5,12)
  expect_equal(nrow(spoils), 2)
  expect_true(all(spoils$contest_type == "aerial"))
  expect_true(all(spoils$winner == "player2"))

  # Check first spoil
  first <- spoils[1]
  expect_equal(first$player1_id, "P002")
  expect_equal(first$player2_id, "P003")
  expect_equal(first$player1_desc, "Contest Target")
  expect_equal(first$player2_desc, "Spoil")
})

test_that("extract_contests finds intercept_mark outcomes via same x,y", {
  chains <- create_mock_contest_chains()
  result <- extract_contests(chains = chains, type = "aerial")

  intercepts <- result[outcome == "intercept_mark"]
  # Row 5->6: Contest Target -> Contested Mark (opp team) at (30,10)
  # Row 11->12: Contest Target -> Uncontested Mark (opp team) at (10,-8)
  # Row 15->16: Contest Target -> Mark On Lead (opp team) at (60,-20)
  # Row 19->20: Kick Inside 50 Result -> Contested Mark (opp team) at (35,15)
  expect_equal(nrow(intercepts), 4)
  expect_true(all(intercepts$contest_type == "aerial"))
  expect_true(all(intercepts$winner == "player2"))
})

test_that("extract_contests finds mark outcome (same team) via same x,y", {
  chains <- create_mock_contest_chains()
  result <- extract_contests(chains = chains, type = "aerial")

  marks <- result[outcome == "mark"]
  # Row 21->22: Contest Target (T1) -> Contested Mark (T1) at (45,30)
  expect_equal(nrow(marks), 1)
  expect_equal(marks$contest_type, "aerial")
  expect_equal(marks$winner, "player1")  # attacker's team won
  expect_equal(marks$player1_id, "P021")
  expect_equal(marks$player2_id, "P022")
  expect_equal(marks$team1_id, marks$team2_id)  # same team
})

test_that("extract_contests finds ground ball contests via same x,y", {
  chains <- create_mock_contest_chains()
  result <- extract_contests(chains = chains, type = "ground_ball")

  # Row 7->8: Hard Ball Get (T1) -> Loose Ball Get (T2) at (15,5)
  # Row 17->18: Same team (T1->T1), should NOT match
  expect_equal(nrow(result), 1)
  expect_equal(result$contest_type, "ground_ball")
  expect_true(is.na(result$outcome))
  expect_equal(result$player1_id, "P007")
  expect_equal(result$player2_id, "P008")
})

test_that("extract_contests type='all' returns all contest types", {
  chains <- create_mock_contest_chains()
  result <- extract_contests(chains = chains, type = "all")

  # 2 spoils + 4 intercept marks + 1 mark + 1 ground ball = 8
  expect_equal(nrow(result), 8)
  expect_setequal(unique(result$contest_type), c("aerial", "ground_ball"))
  expect_setequal(
    unique(result$outcome[!is.na(result$outcome)]),
    c("spoil", "intercept_mark", "mark")
  )
})

test_that("extract_contests ignores same-team ground ball rows at same x,y", {
  chains <- create_mock_contest_chains()
  result <- extract_contests(chains = chains, type = "all")

  # Rows 17-18 are same team (T1) ground ball at same xy -- should NOT appear
  same_team_gb <- result[player1_id == "P017" & player2_id == "P018"]
  expect_equal(nrow(same_team_gb), 0)
})

test_that("extract_contests ignores different x,y adjacent rows", {
  chains <- create_mock_contest_chains()
  result <- extract_contests(chains = chains, type = "all")

  # Rows 9-10 are different teams but different x,y -- should NOT appear
  diff_xy <- result[player1_id == "P009" & player2_id == "P010"]
  expect_equal(nrow(diff_xy), 0)
})

test_that("extract_contests rejects invalid type", {
  chains <- create_mock_contest_chains()
  expect_error(extract_contests(chains = chains, type = "invalid"), "must be one of")
})

test_that("extract_contests returns correct schema when empty", {
  empty_chains <- data.table::data.table(
    matchId = character(), season = integer(), round_number = integer(),
    displayOrder = integer(), period = integer(), periodSeconds = integer(),
    x = integer(), y = integer(),
    teamId = character(), description = character(),
    playerId = character(),
    venueWidth = integer(), venueLength = integer()
  )
  result <- extract_contests(chains = empty_chains)
  expect_equal(nrow(result), 0)
  expect_true("contest_type" %in% names(result))
  expect_true("outcome" %in% names(result))
  expect_true("winner" %in% names(result))
})

test_that("extract_contests does not modify input chains by reference", {
  chains <- create_mock_contest_chains()
  orig_cols <- copy(names(chains))
  extract_contests(chains = chains, type = "all")
  expect_equal(names(chains), orig_cols)
})

test_that("extract_contests works with snake_case columns (PBP-style)", {
  chains <- create_mock_contest_chains()
  # Rename to snake_case
  data.table::setnames(chains, c(
    "matchId", "displayOrder", "periodSeconds", "teamId", "playerId"
  ), c(
    "match_id", "display_order", "period_seconds", "team_id", "player_id"
  ))

  result <- extract_contests(chains = chains, type = "all")
  expect_equal(nrow(result), 8)
})


# --- print.torp_head_to_head tests ---

test_that("print.torp_head_to_head works with contests", {
  h2h <- list(
    player1 = list(player_name = "Alice", team = "Team A"),
    player2 = list(player_name = "Bob", team = "Team B"),
    contests = data.table::data.table(
      season = c(2024L, 2024L),
      contest_type = c("aerial", "ground_ball"),
      outcome = c("spoil", NA_character_),
      winner = c("player1", "player2")
    ),
    summary = data.table::data.table(
      contest_type = c("aerial", "ground_ball"),
      outcome = c("spoil", NA_character_),
      total = c(1L, 1L),
      Alice_wins = c(1L, 0L),
      Bob_wins = c(0L, 1L),
      Alice_win_pct = c(100, 0)
    )
  )
  class(h2h) <- "torp_head_to_head"
  out <- capture.output(print.torp_head_to_head(h2h))
  expect_true(any(grepl("Alice vs Bob", out)))
  expect_true(any(grepl("Total contests: 2", out)))
})

test_that("print.torp_head_to_head handles empty contests", {
  h2h <- list(
    player1 = list(player_name = "Alice", team = "Team A"),
    player2 = list(player_name = "Bob", team = "Team B"),
    contests = data.table::data.table(
      season = integer(), contest_type = character(), winner = character()
    ),
    summary = data.table::data.table(
      contest_type = character(), total = integer()
    )
  )
  class(h2h) <- "torp_head_to_head"
  out <- capture.output(print.torp_head_to_head(h2h))
  expect_true(any(grepl("No contests found", out)))
})


# --- build_h2h_summary tests ---

test_that("build_h2h_summary computes correct win counts", {
  contests <- data.table::data.table(
    player1_id = c("P1", "P2", "P1"),
    player2_id = c("P2", "P1", "P2"),
    contest_type = c("aerial", "aerial", "ground_ball"),
    outcome = c("spoil", "spoil", NA_character_),
    winner = c("player2", "player2", "player1")
  )
  p1 <- list(player_id = "P1", player_name = "Alice")
  p2 <- list(player_id = "P2", player_name = "Bob")

  result <- build_h2h_summary(contests, p1, p2)

  # Row 1: P1 target, P2 spoiled (P2 wins) -> Bob wins
  # Row 2: P2 target, P1 spoiled (P1 wins via player2 position) -> Alice wins
  # Row 3: P1 ground ball, P1 wins -> Alice wins
  spoil_row <- result[outcome == "spoil"]
  expect_equal(spoil_row$Alice_wins, 1L)
  expect_equal(spoil_row$Bob_wins, 1L)
  expect_equal(spoil_row$Alice_win_pct, 50)

  gb_row <- result[contest_type == "ground_ball"]
  expect_equal(gb_row$Alice_wins, 1L)
  expect_equal(gb_row$Bob_wins, 0L)
})

test_that("build_h2h_summary handles empty contests", {
  contests <- data.table::data.table(
    player1_id = character(), player2_id = character(),
    contest_type = character(), outcome = character(),
    winner = character()
  )
  p1 <- list(player_id = "P1", player_name = "Alice")
  p2 <- list(player_id = "P2", player_name = "Bob")

  result <- build_h2h_summary(contests, p1, p2)
  expect_equal(nrow(result), 0)
})
