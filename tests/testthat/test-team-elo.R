# torpverse/docs/plans/FABLE-MATCH-MAE-PLAN.md WS2/WS5 -- team Elo feature.
# Network-free: build_team_elo() operates purely on a synthetic matches
# data.frame, no loaders involved.

.synthetic_matches <- function() {
  data.frame(
    match_id    = c("m1", "m2", "m3", "m4"),
    date        = as.Date(c("2024-01-01", "2024-01-08", "2024-01-15", "2025-01-01")),
    season      = c(2024L, 2024L, 2024L, 2025L),
    round       = c(1L, 2L, 3L, 1L),
    home_team   = c("A", "B", "A", "A"),
    away_team   = c("B", "A", "C", "B"),
    home_margin = c(20, -10, 5, 0),
    stringsAsFactors = FALSE
  )
}

test_that("build_team_elo: elo_pre for match N reflects only strictly-prior matches", {
  m <- .synthetic_matches()
  res <- build_team_elo(m, k = 20, hga = 0, carryover = 1, mov_mult = FALSE)
  by_match <- res$by_match

  # m1 is the first match either team appears in -- both start at 1500
  expect_equal(by_match$elo_pre[by_match$match_id == "m1" & by_match$team_name == "A"], 1500)
  expect_equal(by_match$elo_pre[by_match$match_id == "m1" & by_match$team_name == "B"], 1500)

  # A won m1 (home_margin=20>0), so A's elo_pre going into m2 must be > 1500
  # and B's must be < 1500 (B lost m1)
  a_pre_m2 <- by_match$elo_pre[by_match$match_id == "m2" & by_match$team_name == "A"]
  b_pre_m2 <- by_match$elo_pre[by_match$match_id == "m2" & by_match$team_name == "B"]
  expect_gt(a_pre_m2, 1500)
  expect_lt(b_pre_m2, 1500)

  # C has never played before m3 -- starts at 1500 regardless of what
  # happened in m1/m2 (leak-safety: C's rating is untouched by matches it
  # wasn't in)
  c_pre_m3 <- by_match$elo_pre[by_match$match_id == "m3" & by_match$team_name == "C"]
  expect_equal(c_pre_m3, 1500)
})

test_that("build_team_elo: carryover < 1 regresses ratings toward 1500 at a season boundary", {
  m <- .synthetic_matches()
  res_full <- build_team_elo(m, k = 20, hga = 0, carryover = 1, mov_mult = FALSE)
  res_half <- build_team_elo(m, k = 20, hga = 0, carryover = 0.5, mov_mult = FALSE)

  # A's rating going into its 2025 match (m4) should be closer to 1500 under
  # carryover=0.5 than under carryover=1 (both started from the same place)
  a_2024_final <- res_full$current$elo_current[res_full$current$team_name == "A"]
  a_pre_m4_full <- res_full$by_match$elo_pre[res_full$by_match$match_id == "m4" & res_full$by_match$team_name == "A"]
  a_pre_m4_half <- res_half$by_match$elo_pre[res_half$by_match$match_id == "m4" & res_half$by_match$team_name == "A"]

  expect_lt(abs(a_pre_m4_half - 1500), abs(a_pre_m4_full - 1500))
})

test_that("join_elo_diff_to_team_mdl_df: matches with historical elo_pre use it; unmatched rows fall back to current rating, not 0", {
  m <- .synthetic_matches()
  res <- build_team_elo(m, k = 20, hga = 0, carryover = 1, mov_mult = FALSE)

  team_mdl_df <- data.frame(
    match_id = c("m1", "m1", "future1", "future1"),
    team_name.x = factor(c("A", "B", "A", "B")),
    team_name.y = factor(c("B", "A", "B", "A"))
  )

  out <- join_elo_diff_to_team_mdl_df(team_mdl_df, res)

  # m1 rows: historical elo_pre lookup (both teams at 1500 pre-match-1 -> diff 0)
  expect_equal(out$elo_diff[out$match_id == "m1"], c(0, 0))

  # future1 rows: match_id not in res$by_match at all -- must fall back to
  # each team's CURRENT rating (post-series), not a neutral 0, since A and B
  # are NOT tied after m1/m2/m3
  a_current <- res$current$elo_current[res$current$team_name == "A"]
  b_current <- res$current$elo_current[res$current$team_name == "B"]
  expect_false(isTRUE(all.equal(a_current, b_current)))
  future_row_a_home <- out[out$match_id == "future1" & out$team_name.x == "A", ]
  expect_equal(future_row_a_home$elo_diff, a_current - b_current)
})

test_that("join_elo_diff_to_team_mdl_df: a team with zero history anywhere gets the neutral 1500 fallback", {
  res <- list(
    by_match = data.table::data.table(match_id = character(0), team_name = character(0), elo_pre = numeric(0)),
    current  = data.table::data.table(team_name = character(0), elo_current = numeric(0))
  )
  team_mdl_df <- data.frame(
    match_id = "brand_new",
    team_name.x = factor("Z"),
    team_name.y = factor("Y")
  )
  out <- join_elo_diff_to_team_mdl_df(team_mdl_df, res)
  expect_equal(out$elo_diff, 0)
})
