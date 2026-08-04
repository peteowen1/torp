test_that("EPV_ENGINE defaults to v2 so published ratings are unchanged", {
  expect_identical(EPV_ENGINE, "v2")
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
  # The defect it fixes: EPV_RUCK_CONTEST_WT is positive, so v2 pays a ruckman
  # for every contest he ATTENDS, including the ones he loses.
  wins <- 10; contests <- 20
  attendance_term <- wins * EPV_RUCK_CONTEST_WT -
    max(0, contests - wins) * EPV_RUCK_LOSS_WT
  expect_equal(attendance_term, 0)
  # And a ruck who wins more than he loses is still paid.
  wins <- 15
  expect_gt(wins * EPV_RUCK_CONTEST_WT - max(0, contests - wins) * EPV_RUCK_LOSS_WT, 0)
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
