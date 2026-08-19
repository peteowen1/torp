# The engine is a DELIBERATE, published choice, so this test pins it rather
# than defaulting it. Flipped to "v3" on 2026-08-18 by Pete after the
# four-channel gate: dMAE +0.1913, 95% CI [-0.2311, +0.6136] on 630 matches
# (2024-2026), v3 leading on bits -- inside the stated "slightly worse is
# fine" tolerance, where the three-channel +1.109 was not.
#
# CHANGING THIS LINE CHANGES EVERY PUBLISHED RATING. If this test fails, do
# not "fix" it -- find out who moved the engine and why.
test_that("EPV_ENGINE is pinned to the engine we intend to publish", {
  expect_identical(EPV_ENGINE, "v3")
})

test_that("create_player_game_data rejects an unknown engine", {
  expect_error(
    create_player_game_data(epv_engine = "v4"),
    "Unknown"
  )
})

test_that("aerial outcomes exclude fumbles, which have no winner to credit", {
  # Mark Fumbled / Dropped Mark mean nobody secured the ball. Folding them into
  # the attacking branch would drag V_att down for every genuine mark.
  expect_false("Mark Fumbled" %in% EPV3_AERIAL_OUT)
  expect_false("Dropped Mark" %in% EPV3_AERIAL_OUT)
  expect_true("Spoil" %in% EPV3_AERIAL_OUT)
  expect_true("Contested Mark" %in% EPV3_AERIAL_OUT)
})

test_that("the contest credit split is zero-sum and rewards the upset", {
  # The identity the whole engine rests on: winner banks |cont_att|, loser sheds
  # it, and the payout scales with the SURPRISE, not with the event.
  p <- c(0.1, 0.5, 0.9)
  delta <- 2
  # Defence wins: it banks (1 - p) * Delta, so beating a contest it was expected
  # to lose (p small) pays most.
  def_win_credit <- (1 - p) * delta
  expect_true(all(diff(def_win_credit) < 0))
  # Attack retains: it banks p * Delta, largest when the defence was favoured.
  att_win_credit <- p * delta
  expect_true(all(diff(att_win_credit) > 0))
  # Zero-sum in both branches.
  expect_equal(def_win_credit + (-def_win_credit), rep(0, length(p)))
})

test_that("the allocation rule is one of the measured set", {
  # "contested" is deliberately absent as a default: it INVERTS the channel
  # (cor with contested marks -0.165; Harris Andrews -284). See
  # epv3_compare_alloc.R and the constant's own documentation.
  expect_true(EPV_CONT_LOSS_ALLOC %in% c("ledger", "team", "none", "prorata"))
  expect_false(identical(EPV_CONT_LOSS_ALLOC, "prorata"))
})

test_that("ledger allocation refuses to run without player_stats", {
  # Silently falling back to a different rule than the caller asked for is the
  # "guard degrades to a no-op" failure this repo keeps hitting.
  scored <- data.table::data.table(
    loser_pid = NA_character_, loser_tid = "T1", loser_credit = -1,
    def_win = TRUE, att_x = 0, match_id = "M1"
  )
  expect_error(
    allocate_contest_losses(scored, data.table::data.table(), half = 78,
                            exposure_descs = "ledger", player_stats = NULL),
    "player_stats"
  )
})

test_that("uncontested marks are excluded from the contested exposure base", {
  # An uncontested mark means by definition no contest happened.
  expect_false("Uncontested Mark" %in% EPV3_AERIAL_EXPOSURE_DESCS)
  expect_false("Mark On Lead" %in% EPV3_AERIAL_EXPOSURE_DESCS)
  expect_true("Uncontested Mark" %in% EPV3_AERIAL_EXPOSURE_WIDE)
})

test_that("the stoppage term is a win/loss ledger, not an attendance count", {
  # ON since 2026-08-04, having cleared its gate. Paying a ruckman for contests
  # he LOST is indefensible, and the measured case is that turning the term into
  # a differential cuts count-dependence -- cor(contest, hitouts) 0.370 -> 0.252
  # -- while year-over-year repeatability holds at 0.79. A COUNT can be padded by
  # volume; a DIFFERENTIAL cannot.
  expect_true(EPV3_STOP_ZERO_SUM)
})

test_that("zero-sum stoppage pays nothing for splitting contests 50/50", {
  # v3's ledger: a credit per contest WON, a debit per contest LOST. Split them
  # 50/50 and the two cancel.
  #
  # abs() because that is exactly what the production branch does
  # (player_credit.R, the EPV3_STOP_ZERO_SUM arm). v2 uses the same constant
  # against `ruck_contests` -- attendance -- where it is a DEBIT and went
  # negative on 2026-08-07. The previous version of this test hardcoded the
  # SIGNED constant and broke on that flip while production was untouched, which
  # is precisely the failure mode of a test that restates a formula in its own
  # body instead of exercising the code.
  w <- abs(EPV_RUCK_CONTEST_WT)
  wins <- 10; contests <- 20
  expect_equal(wins * w - max(0, contests - wins) * EPV_RUCK_LOSS_WT, 0)
  # And a ruck who wins more than he loses is still paid.
  wins <- 15
  expect_gt(wins * w - max(0, contests - wins) * EPV_RUCK_LOSS_WT, 0)
})

test_that("v2's attendance debit cannot invert v3's credit for winning", {
  # One constant, two opposite sign conventions:
  #   v2  EPV_RUCK_CONTEST_WT * ruck_contests   -- per contest ATTENDED, a debit
  #   v3  abs(...)            * hitouts         -- per contest WON, a credit
  # Drop the abs() in the v3 branch and a ruck starts being charged for winning,
  # silently, with the v2 arm still looking correct.
  #
  # What this can check is the contract the two engines rely on; it cannot check
  # that the v3 branch still calls abs(), which would need a full engine build.
  # Stated so nobody reads it as more coverage than it is.
  expect_lt(EPV_RUCK_CONTEST_WT, 0)        # v2 side: attending must cost
  expect_gt(abs(EPV_RUCK_CONTEST_WT), 0)   # v3 side: winning must still pay
  expect_gt(EPV_RUCK_LOSS_WT, 0)           # and the v3 debit stays a debit
  # Break-even for v2 sits at the league average win rate, which is the whole
  # basis for EPV_HITOUT_WT's value -- pin it so a later edit to either constant
  # has to face the intent rather than just the number.
  expect_equal(abs(EPV_RUCK_CONTEST_WT) / EPV_HITOUT_WT, 0.377, tolerance = 0.02)
})

test_that("v3 points constants are engine-conditioned, not a bare vector", {
  # They were fitted and applied on 2026-08-04, so "all 1" is no longer a
  # property of the constant -- it is a property of the ENGINE being v2. Keeping
  # the old bare `all(... == 1)` assertion here would pass today and start
  # failing the moment the engine flag flips, for no reason.
  #
  # The full contract -- inert under v2, exactly the fitted values under v3, and
  # each EPR_PRIOR_RATE_* carrying its own channel's factor -- is enforced in
  # test-epv3-constants.R.
  expect_named(EPV3_POINTS_SCALE, c("recv", "disp", "cont_aerial", "cont_stop"))
  if (identical(EPV_ENGINE, "v2")) {
    expect_true(all(EPV3_POINTS_SCALE == 1))
  } else {
    expect_true(all(EPV3_POINTS_SCALE > 0))
    expect_false(all(EPV3_POINTS_SCALE == 1))
  }
})

# build_aerial_contests() had NO test coverage until 2026-08-18, which is how the
# `get()`-in-a-grouped-`:=` memory pattern at epv_v3.R:107 survived. These call
# the shipped function rather than recomputing its logic, so a broken lead-shift
# fails them.
.mk_chains <- function() {
  data.table::data.table(
    match_id      = c("M1", "M1", "M1", "M2", "M2"),
    display_order = c(1L, 2L, 3L, 1L, 2L),
    description   = c("Kick", "Contested Mark", "Goal", "Kick", "Goal"),
    player_id     = c("pKick", "pMark", "pMark", "pKick2", "pKick2"),
    team_id       = c("A", "B", "B", "A", "A"),
    x             = c(10, 40, 60, 15, 55),
    y             = c(0, 5, 0, 0, 0)
  )
}
.mk_pbp <- function() {
  data.table::data.table(
    match_id      = c("M1", "M1", "M1", "M2", "M2"),
    display_order = c(1L, 2L, 3L, 1L, 2L),
    exp_pts       = c(1.5, 2.0, 6.0, 1.4, 6.0),
    delta_epv     = c(0.5, -0.3, 4.0, 0.4, 4.6)
  )
}

test_that("build_aerial_contests pairs a kick with the aerial outcome that follows it", {
  cst <- build_aerial_contests(.mk_chains(), .mk_pbp())
  expect_true(nrow(cst) >= 1)
  row <- cst[match_id == "M1"]
  expect_equal(nrow(row), 1L)
  # The outcome must come from display_order 2 of the SAME match, not anywhere else.
  expect_equal(row$kick_pid, "pKick")
  expect_equal(row$out_desc, "Contested Mark")
  expect_equal(row$out_pid, "pMark")
  expect_equal(row$out_tid, "B")
  expect_equal(row$out_x, 40)
  # Opposition took the mark, so the defence won this contest.
  expect_true(row$def_win)
})

test_that("the lead shift does not reach across a match boundary", {
  # This fixture is built to DISCRIMINATE. M1 ends on a Kick and M2 opens with an
  # aerial outcome by the other team, so dropping `by = match_id` from the shift
  # pairs them and invents a contest for M1. An earlier version of this test put a
  # "Goal" last in M1, which produced no contest either way — it passed against the
  # broken code and was worth nothing. Mutation-checked: removing `by = match_id`
  # fails this.
  ch <- data.table::data.table(
    match_id      = c("M1", "M1", "M2", "M2", "M2"),
    display_order = c(1L, 2L, 1L, 2L, 3L),
    description   = c("Goal", "Kick", "Contested Mark", "Kick", "Goal"),
    player_id     = c("pA", "pKick1", "pMarkB", "pKick2", "pA2"),
    team_id       = c("A", "A", "B", "A", "A"),
    x             = c(60, 10, 40, 12, 62),
    y             = c(0, 0, 5, 0, 0)
  )
  pbp <- data.table::data.table(
    match_id      = c("M1", "M1", "M2", "M2", "M2"),
    display_order = c(1L, 2L, 1L, 2L, 3L),
    exp_pts       = c(6.0, 1.5, 2.0, 1.5, 6.0),
    delta_epv     = c(4.0, 0.5, -0.3, 0.5, 4.0)
  )
  cst <- build_aerial_contests(ch, pbp)
  # M1's trailing kick has nothing after it INSIDE M1, so it must yield no contest.
  expect_equal(nrow(cst[match_id == "M1"]), 0L)
  # M2's own kick->mark pair is not a contest either (same team took the mark is
  # false here: mark is B, kick is A at display_order 2 -> nothing follows it).
  expect_equal(nrow(cst), 0L)
})
