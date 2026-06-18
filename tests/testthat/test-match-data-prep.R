# Tests for .build_team_ratings_df PSR rolling-join semantics
# ============================================================
# Regression tests for the PSR leakage fix. Previously the join used
# `slice_tail(n=1)` per player which applied each player's LATEST PSR
# (which could be a season-end snapshot) to every historical lineup row,
# leaking future skill information into past games used for training.
#
# The current implementation uses join_by(closest(.lineup_key >= psr_key))
# which picks the most recent PSR row available *as of* the lineup row's
# (season, round_number). PSR(s, r) is computed using stat ratings filtered
# to match_date_rating < ref_date (where ref_date = first utc_start_time of
# round r), so PSR(s, r) is itself snapshot-as-of-start-of-round-r and is
# safe to use for predicting round r.

# -----------------------------------------------------------------------------
# Fixture builders
# -----------------------------------------------------------------------------

.tdp_make_teams <- function() {
  # 1 player per team, 2 teams (so we get a valid match), playing 2 rounds.
  # Position "C" (centre) -- POSITION_AVG_TOG["C"] = 0.80, in MATCH_INDIVIDUAL_POS.
  expand.grid(
    player_id = c("P1", "P2"),
    round_number = c(0L, 1L),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  ) |>
    transform(
      season = 2026L,
      lineup_position = "C",
      position_group = "MID",
      team_name = ifelse(player_id == "P1", "Adelaide Crows", "Brisbane Lions"),
      team_id = ifelse(player_id == "P1", 10L, 20L),
      team_type = ifelse(player_id == "P1", "home", "away"),
      match_id = paste0("M-2026-R", round_number)
    )
}

.tdp_make_torp <- function() {
  # EPR per (player, season, round). Use constant EPR so we can reason about
  # it. round_number on lineups joins to `round` here.
  data.frame(
    player_id = rep(c("P1", "P2"), each = 2),
    season = 2026L,
    round = c(0L, 1L, 0L, 1L),
    epr = 10,
    epr_recv = 2.5,
    epr_disp = 2.5,
    epr_spoil = 2.5,
    epr_hitout = 2.5,
    stringsAsFactors = FALSE
  )
}

.tdp_make_psr <- function() {
  # PSR computed AT each round. Distinct values per round per player so we
  # can verify the rolling join picks the right one.
  data.frame(
    player_id = c("P1", "P1", "P2", "P2"),
    season = 2026L,
    round = c(0L, 1L, 0L, 1L),
    psr = c(100, 200, 300, 400),
    osr = c(50, 100, 150, 200),
    dsr = c(50, 100, 150, 200),
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# Tests
# -----------------------------------------------------------------------------

test_that("round 0 picks PSR(s,0) when present (same-round non-strict match)", {
  teams  <- .tdp_make_teams()
  torp_df <- .tdp_make_torp()
  psr_df  <- .tdp_make_psr()

  result <- torp:::.build_team_ratings_df(teams, torp_df, psr_df)

  tog_c <- POSITION_AVG_TOG[["C"]]  # 0.80

  # round 0 lineup picks PSR(s, 0): P1 = 100, P2 = 300.
  # PSR(s, 0) is computed at start of round 0 using prior-season data, so it
  # is safe to use when predicting round 0 -- not leakage.
  r0 <- result[result$round_number == 0L, ]
  expect_equal(nrow(r0), 2L)
  r0_p1 <- r0[r0$team_id == 10L, ]
  r0_p2 <- r0[r0$team_id == 20L, ]
  expect_equal(r0_p1$psr, 100 * tog_c)
  expect_equal(r0_p2$psr, 300 * tog_c)
})

test_that("round N lineup picks PSR(s, N), not PSR from the global tail", {
  teams  <- .tdp_make_teams()
  torp_df <- .tdp_make_torp()
  psr_df  <- .tdp_make_psr()

  result <- torp:::.build_team_ratings_df(teams, torp_df, psr_df)

  tog_c <- POSITION_AVG_TOG[["C"]]  # 0.80

  # round 1 lineup picks PSR(s, 1): P1 = 200, P2 = 400.
  # The bug under regression-test: slice_tail(n=1) would have used the global
  # latest (also 200 / 400 here, but in real data could be a future-season
  # snapshot). What we are pinning is that the value comes from THIS round's
  # PSR row, not from any later one.
  r1 <- result[result$round_number == 1L, ]
  expect_equal(nrow(r1), 2L)
  r1_p1 <- r1[r1$team_id == 10L, ]
  r1_p2 <- r1[r1$team_id == 20L, ]
  expect_equal(r1_p1$psr, 200 * tog_c)
  expect_equal(r1_p2$psr, 400 * tog_c)
})

test_that("future round (no PSR yet) picks the latest available prior PSR", {
  # Add a round-2 lineup row but no round-2 PSR. The join should pick the
  # round-1 PSR (the latest with psr_key <= lineup_key). This mimics the
  # production prediction case where we predict round R using PSR through
  # R-1, and matches the previous slice_tail(n=1) behaviour at prediction
  # time.
  teams_extra <- rbind(
    .tdp_make_teams(),
    data.frame(
      player_id = c("P1", "P2"),
      round_number = 2L,
      season = 2026L,
      lineup_position = "C",
      position_group = "MID",
      team_name = c("Adelaide Crows", "Brisbane Lions"),
      team_id = c(10L, 20L),
      team_type = c("home", "away"),
      match_id = "M-2026-R2",
      stringsAsFactors = FALSE
    )
  )
  torp_extra <- rbind(
    .tdp_make_torp(),
    data.frame(player_id = c("P1", "P2"), season = 2026L, round = 2L,
               epr = 10, epr_recv = 2.5, epr_disp = 2.5,
               epr_spoil = 2.5, epr_hitout = 2.5,
               stringsAsFactors = FALSE)
  )
  psr_df  <- .tdp_make_psr()  # only rounds 0 and 1

  result <- torp:::.build_team_ratings_df(teams_extra, torp_extra, psr_df)

  tog_c <- POSITION_AVG_TOG[["C"]]
  r2 <- result[result$round_number == 2L, ]
  r2_p1 <- r2[r2$team_id == 10L, ]
  r2_p2 <- r2[r2$team_id == 20L, ]
  expect_equal(r2_p1$psr, 200 * tog_c)
  expect_equal(r2_p2$psr, 400 * tog_c)
})

test_that("missing PSR for a player falls back to PSR_PRIOR_RATE", {
  # P1 has PSR rows; P2 has none. P2 should fall back to PSR_PRIOR_RATE,
  # P1 should still get its rolling-join value. Verifies the per-player
  # partition of closest() (a misplaced group_by would silently leak
  # P1's PSR onto P2 or vice versa).
  teams  <- .tdp_make_teams()
  torp_df <- .tdp_make_torp()
  psr_df  <- .tdp_make_psr()
  psr_df  <- psr_df[psr_df$player_id == "P1", ]

  # P2 has no PSR rows: 50% of lineup rows have NA PSR, which trips the >25%
  # telemetry warning. We consume it explicitly so the test isn't noisy.
  expect_warning(
    result <- suppressMessages(
      torp:::.build_team_ratings_df(teams, torp_df, psr_df)
    ),
    "High proportion of missing PSR"
  )

  tog_c <- POSITION_AVG_TOG[["C"]]

  r1_p1 <- result[result$round_number == 1L & result$team_id == 10L, ]
  r1_p2 <- result[result$round_number == 1L & result$team_id == 20L, ]
  expect_equal(r1_p1$psr, 200 * tog_c)
  expect_equal(r1_p2$psr, PSR_PRIOR_RATE * tog_c)
})

test_that("duplicate (player, season, round) PSR rows are deduped (warn)", {
  # If psr_df accidentally contains duplicate rows at the same
  # (player_id, season, round), the rolling join with
  # default `multiple = \"all\"` would duplicate lineup rows and silently
  # inflate the team-level sum aggregate. We dedup with a warning instead.
  teams  <- .tdp_make_teams()
  torp_df <- .tdp_make_torp()
  psr_df  <- .tdp_make_psr()
  # Inject a duplicate of P1's round-1 PSR with a different value:
  psr_df  <- rbind(
    psr_df,
    data.frame(player_id = "P1", season = 2026L, round = 1L,
               psr = 999, osr = 999, dsr = 999,
               stringsAsFactors = FALSE)
  )

  expect_warning(
    result <- torp:::.build_team_ratings_df(teams, torp_df, psr_df),
    "duplicate"
  )

  tog_c <- POSITION_AVG_TOG[["C"]]
  # P1 round 1 has 1 lineup row (no row duplication from join).
  r1_p1 <- result[result$round_number == 1L & result$team_id == 10L, ]
  expect_equal(nrow(r1_p1), 1L)
  # The first matching PSR row is kept by distinct(); the value is whichever
  # of {200, 999} appeared first in psr_for_join row order. Either way the
  # test pins the no-duplication invariant.
  expect_true(r1_p1$psr %in% c(200 * tog_c, 999 * tog_c))
})

test_that("NA in season / round_number aborts rather than silently fallback", {
  teams  <- .tdp_make_teams()
  torp_df <- .tdp_make_torp()
  psr_df  <- .tdp_make_psr()
  teams$round_number[1] <- NA_integer_

  expect_error(
    torp:::.build_team_ratings_df(teams, torp_df, psr_df),
    "Cannot build PSR join key"
  )
})

test_that("PSR join works without osr/dsr (back-compat)", {
  # Old psr_df without osr/dsr columns should still work; only `psr` populated.
  teams  <- .tdp_make_teams()
  torp_df <- .tdp_make_torp()
  psr_df  <- .tdp_make_psr()[, c("player_id", "season", "round", "psr")]

  result <- torp:::.build_team_ratings_df(teams, torp_df, psr_df)

  tog_c <- POSITION_AVG_TOG[["C"]]
  r1_p1 <- result[result$round_number == 1L & result$team_id == 10L, ]
  expect_equal(r1_p1$psr, 200 * tog_c)
  # osr/dsr should not be in output
  expect_false("osr" %in% names(result))
  expect_false("dsr" %in% names(result))
})

test_that("PSR fallback works when psr_df is NULL", {
  # When psr_df is NULL the join is bypassed entirely: psr is set directly to
  # PSR_PRIOR_RATE and -- distinct from the populated branch -- is NOT
  # multiplied by lineup_tog. This asymmetry pre-dates the leakage fix; the
  # test pins the existing behaviour rather than codifying a recommendation.
  teams  <- .tdp_make_teams()
  torp_df <- .tdp_make_torp()

  result <- torp:::.build_team_ratings_df(teams, torp_df, psr_df = NULL)

  expect_true(all(result$psr == PSR_PRIOR_RATE))
})
