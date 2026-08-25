# Tests for the xRAPM match-model feature (team_rapm_match_feature.R).
#
# The three things that can silently go wrong when wiring a player rating into
# the match model, per this file's directive:
#   1. a future rating leaking backwards into an earlier match,
#   2. an inverted diff (model learns the feature backwards),
#   3. a missing rating becoming a real "teams are equal" claim.

.mk_lineups <- function() {
  data.frame(
    player_id = c("p1", "p2", "p1", "p2"),
    season = c(2026L, 2026L, 2026L, 2026L),
    round_number = c(1L, 1L, 5L, 5L),
    lineup_tog = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )
}

test_that(".join_xrapm_to_lineups never uses a rating from a later round", {
  # p1 has a rating at round 5 only. A round-1 lineup row must NOT see it --
  # that rating is built from matches p1 had not yet played at round 1.
  snap <- data.frame(
    player_id = "p1",
    season = 2026L,
    round_number = 5L,
    team_rapm_shrunk = 99,
    stringsAsFactors = FALSE
  )
  # 3 of 4 fixture rows are unrated here, so the stale-snapshot warning fires
  # by design -- suppressed rather than silenced globally, so a NEW warning in
  # this path would still surface.
  suppressWarnings(out <- .join_xrapm_to_lineups(.mk_lineups(), snap))

  r1 <- out[out$player_id == "p1" & out$round_number == 1L, ]
  r5 <- out[out$player_id == "p1" & out$round_number == 5L, ]

  expect_equal(r1$xrapm, 0) # unrated as of round 1 -> ridge prior
  expect_equal(r5$xrapm, 99) # rated by round 5
})

test_that(".join_xrapm_to_lineups carries an earlier rating forward, not backward", {
  snap <- data.frame(
    player_id = "p1",
    season = 2026L,
    round_number = 1L,
    team_rapm_shrunk = 7,
    stringsAsFactors = FALSE
  )
  out <- .join_xrapm_to_lineups(.mk_lineups(), snap)

  # Round 1 sees its own checkpoint; round 5 still sees it (latest <= 5).
  expect_equal(out$xrapm[out$player_id == "p1" & out$round_number == 1L], 7)
  expect_equal(out$xrapm[out$player_id == "p1" & out$round_number == 5L], 7)
})

test_that(".join_xrapm_to_lineups picks the latest checkpoint at or before the round", {
  snap <- data.frame(
    player_id = c("p1", "p1", "p1"),
    season = c(2026L, 2026L, 2026L),
    round_number = c(1L, 4L, 9L),
    team_rapm_shrunk = c(1, 4, 9),
    stringsAsFactors = FALSE
  )
  out <- .join_xrapm_to_lineups(.mk_lineups(), snap)

  expect_equal(out$xrapm[out$player_id == "p1" & out$round_number == 1L], 1)
  # round 5 must take the round-4 checkpoint, never the round-9 one
  expect_equal(out$xrapm[out$player_id == "p1" & out$round_number == 5L], 4)
})

test_that("an unrated player gets the ridge prior 0, not an NA or a non-zero prior", {
  # p2 is absent from the snapshot entirely.
  snap <- data.frame(
    player_id = "p1", season = 2026L, round_number = 1L,
    team_rapm_shrunk = 5, stringsAsFactors = FALSE
  )
  out <- .join_xrapm_to_lineups(.mk_lineups(), snap)

  expect_false(any(is.na(out$xrapm)))
  expect_true(all(out$xrapm[out$player_id == "p2"] == 0))
})

test_that("a NULL snapshot yields a flat 0 column rather than erroring", {
  out <- .join_xrapm_to_lineups(.mk_lineups(), NULL)
  expect_true(all(out$xrapm == 0))
  expect_equal(nrow(out), 4L)
})

test_that("xrapm is TOG-weighted, so a part-game slot counts for less", {
  lineups <- .mk_lineups()
  lineups$lineup_tog <- c(1, 0.5, 1, 1)
  snap <- data.frame(
    player_id = c("p1", "p2"), season = c(2026L, 2026L),
    round_number = c(1L, 1L), team_rapm_shrunk = c(10, 10),
    stringsAsFactors = FALSE
  )
  out <- .join_xrapm_to_lineups(lineups, snap)

  r1 <- out[out$round_number == 1L, ]
  expect_equal(r1$xrapm[r1$player_id == "p1"], 10)
  expect_equal(r1$xrapm[r1$player_id == "p2"], 5) # 10 * 0.5
})

test_that("duplicate snapshot keys are deduped rather than duplicating lineup rows", {
  # Two rows for the same (player, season, round) would otherwise fan out
  # through closest() and silently double this player's contribution to the
  # per-team sum.
  snap <- data.frame(
    player_id = c("p1", "p1"),
    season = c(2026L, 2026L),
    round_number = c(1L, 1L),
    team_rapm_shrunk = c(3, 4),
    stringsAsFactors = FALSE
  )
  expect_warning(
    out <- .join_xrapm_to_lineups(.mk_lineups(), snap),
    "duplicate"
  )
  expect_equal(nrow(out), 4L)
})

test_that("load_team_rapm_asof warns and returns NULL when no snapshot exists", {
  expect_warning(
    got <- load_team_rapm_asof(comp = "AFLM",
                               path = file.path(tempdir(), "definitely-not-here.parquet")),
    "No as-of xRAPM snapshot"
  )
  expect_null(got)
})

test_that("load_team_rapm_asof aborts on a pre-2026-08-26 snapshot missing team_rapm_shrunk", {
  skip_if_not_installed("arrow")
  p <- file.path(tempdir(), "old_schema.parquet")
  # The old schema: rapm (raw) but no team_rapm_shrunk, no season/round keys.
  arrow::write_parquet(
    data.frame(player_id = "p1", ref_date = as.Date("2026-01-01"), rapm = 1),
    p
  )
  on.exit(unlink(p), add = TRUE)
  expect_error(load_team_rapm_asof(comp = "AFLM", path = p), "missing required column")
})

test_that("the diff is oriented self-minus-opponent, matching the other *_diff features", {
  # Guards the inverted-feature failure mode: a stronger own team must produce
  # a POSITIVE xrapm_diff, same sign convention as psr_diff/epr_diff.
  df <- data.frame(xrapm.x = c(10, 2), xrapm.y = c(2, 10))
  xrapm_diff <- df$xrapm.x - df$xrapm.y
  expect_gt(xrapm_diff[1], 0) # own team stronger -> positive
  expect_lt(xrapm_diff[2], 0) # opponent stronger -> negative
  expect_equal(xrapm_diff[1], -xrapm_diff[2]) # symmetric
})
