# The engine is a DELIBERATE, published choice, so this test pins it rather
# than defaulting it. Flipped to "v3" on 2026-08-18 by Pete after the
# four-channel gate: dMAE +0.1913, 95% CI [-0.2311, +0.6136] on 630 matches
# (2024-2026), v3 leading on bits -- inside the stated "slightly worse is
# fine" tolerance, where the three-channel +1.109 was not.
#
# CHANGING THIS LINE CHANGES EVERY PUBLISHED RATING. If this test fails, do
# not "fix" it -- find out who moved the engine and why.
test_that("EPV_ENGINE is pinned to the engine we intend to publish", {
  # Flipped to "v4" (the Net Points ledger) on 2026-09-06 by Pete after the
  # six-season fast gate: v4 won every out-of-sample row (MAE 26.77 vs 27.18,
  # bits 0.163 vs 0.139, defence coefficient -1.45 vs -0.57 with club and
  # season fixed effects) on 1,227 matches. docs/plans/EPV-V4-CREDIT-RULES.md s8.
  expect_identical(EPV_ENGINE, "v4")
})

test_that("the v4 units factor is the measured within-team coefficient, and the shrink is linear", {
  expect_equal(EPR_UNITS_SCALE_V4, 2.40)
  # scaling the finished rating cannot reorder anyone; scaling the data term
  # (loading) changes the shrink -- the two must not be confused
  r <- torp:::.bayesian_shrink(c(10, 20), c(5, 5), 1, 4, -0.7)
  expect_equal(r[2] - r[1], (20 - 10) / (5 + 4))
  # v4 is in points already: the prior rates carry no per-channel scale
  expect_equal(c(EPR_PRIOR_RATE_RECV, EPR_PRIOR_RATE_DISP, EPR_PRIOR_RATE_SPOIL),
               c(-0.7, -0.7, -0.3))
})

test_that("create_player_game_data rejects an unknown engine", {
  expect_error(
    create_player_game_data(epv_engine = "v9"),
    "Unknown"
  )
})

test_that("the removed v2 and v3 engines fail loudly rather than running v4", {
  # Removed 2026-11-16. An old script asking for them must not silently get v4
  # numbers under a v2/v3 label.
  expect_error(create_player_game_data(epv_engine = "v2"), "removed")
  expect_error(create_player_game_data(epv_engine = "v3"), "removed")
})

test_that("aerial outcomes exclude fumbles, which have no winner to credit", {
  # Mark Fumbled / Dropped Mark mean nobody secured the ball. Folding them into
  # the attacking branch would drag V_att down for every genuine mark.
  expect_false("Mark Fumbled" %in% EPV3_AERIAL_OUT)
  expect_false("Dropped Mark" %in% EPV3_AERIAL_OUT)
  expect_true("Spoil" %in% EPV3_AERIAL_OUT)
  expect_true("Contested Mark" %in% EPV3_AERIAL_OUT)
})

# Stub model for exercising score_contests() without fitting a GAM. Defined at
# file level so S3 dispatch finds it; predict() is all score_contests() asks of
# these objects.
epv3_stub_model <- function(v) structure(list(v = v), class = "epv3_stub")
predict.epv3_stub <- function(object, newdata, ...) rep_len(object$v, nrow(newdata))
# score_contests() calls stats::predict() from inside the package namespace, so
# a method defined only in this file's environment is not found by UseMethod().
# Register it explicitly.
registerS3method("predict", "epv3_stub", predict.epv3_stub)

test_that("the contest credit split is zero-sum and rewards the upset", {
  # The identity the whole engine rests on: winner banks |cont_att|, loser sheds
  # it, and the payout scales with the SURPRISE, not with the event.
  #
  # This test used to restate that formula in its own body and assert on the
  # restatement -- its final check was `x + (-x) == 0`, true for any x, so it
  # exercised no production code at all. It now calls score_contests() for real.
  #
  # What it actually catches, mutation-tested 2026-09-05: swapping p and (1 - p)
  # in score_contests()'s cont_att line fails 4 assertions here. What it does
  # NOT catch, checked rather than assumed: inverting the sign for defence wins
  # passes, because winner_credit/loser_credit go through abs() -- the current
  # design is immune to that by construction, which is precisely why the
  # production comment prefers it to the old branching version.
  #
  # Delta = V_att - V_def = 2; p is the defence-win probability.
  cst <- data.table::data.table(
    def_win    = c(TRUE, FALSE),
    exp_pts    = c(0, 0),
    out_pid    = c("W1", "W2"), out_tid    = c("T1", "T2"),
    target_pid = c("L1", "L2"), target_tid = c("T2", "T1"),
    kick_tid   = c("T2", "T1")
  )
  mk <- function(p) list(p = epv3_stub_model(p), att = epv3_stub_model(3),
                         def = epv3_stub_model(1))
  s <- score_contests(cst, mk(0.1))

  # Zero-sum, on the REAL output: what the winner banks the loser sheds.
  expect_equal(s$winner_credit + s$loser_credit, c(0, 0))
  expect_true(all(s$winner_credit >= 0))
  expect_true(all(s$loser_credit <= 0))

  # Rewards the upset: at p = 0.1 the defence is a heavy underdog, so a defence
  # win pays (1 - p) * Delta = 1.8 while the attack merely retaining pays 0.2.
  expect_equal(s$winner_credit[s$def_win], 1.8)
  expect_equal(s$winner_credit[!s$def_win], 0.2)

  # Scales with surprise, not with the event: raise p and the defence-win credit
  # must FALL while the attack-retain credit RISES.
  s2 <- score_contests(cst, mk(0.9))
  expect_lt(s2$winner_credit[s2$def_win], s$winner_credit[s$def_win])
  expect_gt(s2$winner_credit[!s2$def_win], s$winner_credit[!s$def_win])
})

test_that("the box-score ruck weights keep their break-even at the league win rate", {
  # These weights no longer reach any published rating (v4 overwrites the
  # box-score epv_hitout with zero), but player_epv_breakdown() still prices
  # with them. Attending must cost, and break-even sits at the league average
  # win rate, which is the whole basis for EPV_HITOUT_WT's value.
  expect_lt(EPV_RUCK_CONTEST_WT, 0)
  expect_equal(abs(EPV_RUCK_CONTEST_WT) / EPV_HITOUT_WT, 0.377, tolerance = 0.02)
})

test_that("the per-channel points scale is all 1 under v4", {
  # v3's fitted values went with v3 (2026-11-16). v4 is already in margin
  # points, so nothing is scaled per channel.
  expect_named(EPV3_POINTS_SCALE, c("recv", "disp", "cont_aerial", "cont_stop"))
  expect_true(all(EPV3_POINTS_SCALE == 1))
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

test_that("an attack-win contest debit lands on the defending team, not the winner", {
  cst <- data.table::data.table(
    def_win = c(TRUE, FALSE), exp_pts = c(0, 0),
    out_pid = c("W1", "W2"), out_tid = c("T2", "T1"),
    target_pid = c("L1", NA), kick_tid = c("T1", "T1"),
    opp_tid = c("T2", "T2"))
  mk <- list(p = epv3_stub_model(0.3), att = epv3_stub_model(3),
             def = epv3_stub_model(1))
  s <- score_contests(cst, mk)
  expect_equal(s$loser_tid, c("T1", "T2"))
  expect_true(all(s$loser_tid != s$winner_tid))
})

test_that("build_aerial_contests names the non-kicking team as opp_tid", {
  cst <- build_aerial_contests(.mk_chains(), .mk_pbp())
  expect_true("opp_tid" %in% names(cst))
  expect_false(any(cst$opp_tid == cst$kick_tid, na.rm = TRUE))
})

test_that("epv3_aerial_out rejects non-character population", {
  expect_error(epv3_aerial_out(1))
  expect_error(epv3_aerial_out("bogus"))
})
