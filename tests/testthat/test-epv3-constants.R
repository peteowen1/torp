# v3's constants are engine-conditioned. Two things have to stay true and
# neither is obvious from reading any one of them:
#
#   1. While EPV_ENGINE is "v2", every v3 constant is INERT -- production ratings
#      must not move because v3 constants were fitted and committed. This is the
#      whole basis on which v3 work has been landing on dev for two sessions.
#
#   2. The constants file is order-sensitive. The engine-conditioned constants
#      read EPV_ENGINE, R sources a package file top to bottom, and their
#      natural home is ABOVE where EPV_ENGINE is defined. Getting that wrong
#      fails at BUILD time with "object 'EPV_ENGINE' not found" -- it happened
#      on 2026-08-04. The build/load itself is now the guard for that (this
#      test no longer re-sources the file, since the file is not on disk under
#      R CMD check).

test_that("v3 constants are inert while the engine is v2", {
  skip_if_not(identical(EPV_ENGINE, "v2"),
              "engine is not v2 -- production-inertness is not the live question")

  # Shrinkage: production has always run 3.0 on every channel. The v3 values
  # were measured on v3's channels and are meaningless for v2's.
  expect_equal(EPR_PRIOR_GAMES_RECV, 3)
  expect_equal(EPR_PRIOR_GAMES_DISP, 3)
  expect_equal(EPR_PRIOR_GAMES_SPOIL, 3)
  expect_equal(EPR_PRIOR_GAMES_HITOUT, 3)

  # Prior rates carry the single global points scale under v2, per channel
  # under v3. .bayesian_shrink() adds prior_games * prior_rate AFTER the value,
  # so a mismatch here leaves EPR a blend of scaled and unscaled parts.
  expect_equal(unname(EPR_PRIOR_RATE_RECV), -0.7 * EPV_POINTS_SCALE)
  expect_equal(unname(EPR_PRIOR_RATE_DISP), -0.7 * EPV_POINTS_SCALE)
  expect_equal(unname(EPR_PRIOR_RATE_SPOIL), -0.3 * EPV_POINTS_SCALE)

  # The per-channel scale must be the identity so centre_epv_by_position()
  # cannot rescale a v2 frame.
  expect_equal(unname(EPV3_POINTS_SCALE), c(1, 1, 1, 1))
  expect_equal(unname(EPV3_SUB_SCALE), c(1, 1))

  # v2's spoil channel is a genuine all-position quantity (spoils, tackles,
  # pressure acts) and keeps being standardised. Only v3's merged contest
  # channel is excluded.
  expect_equal(EPV_STANDARDISE_CHANNELS, c("recv", "disp", "spoil"))
})

test_that("flipping the engine to v3 changes exactly the intended constants", {
  # Ask the engine-conditioned constants' own functions what they return under
  # v3, rather than asserting on the live values or re-reading the source file.
  # That is the only way to check the v3 branch without shipping it, and it runs
  # identically under devtools::load_all() and R CMD check (the R/ source tree
  # does not exist beside the tests under check, which is why the earlier
  # version of this test, which sourced the file, skipped on CI).
  expect_equal(.epr_prior_games("recv",  "v3"), 14.38)
  expect_equal(.epr_prior_games("disp",  "v3"), 24.33)
  expect_equal(.epr_prior_games("spoil", "v3"), 11.09)
  expect_equal(.epv_standardise_channels("v3"), c("recv", "disp"))

  pts <- .epv3_points_scale("v3")
  # FOUR-channel constants, from ws30_epv3_4ch_gate.R -- the gate that scored
  # them. cont_stop is no longer 1: under 4 channels the ruck slot is live.
  # the four-channel constants the gate actually scored (torp abd70131,
  # 2026-08-18); the 3.7557 / 2.5196 / 0.3870 / 1.8557 pin was the earlier fit
  expect_equal(unname(pts), c(1.650438, 1.421199, 0.337485, 1.659480))

  # Each prior rate must carry its OWN channel's factor. This is the check that
  # would have caught the 0.919 fallback bug from the other direction.
  expect_equal(-0.7 * .epr_prior_points_scale("recv",        "v3"), -0.7 * pts[["recv"]])
  expect_equal(-0.7 * .epr_prior_points_scale("disp",        "v3"), -0.7 * pts[["disp"]])
  expect_equal(-0.3 * .epr_prior_points_scale("cont_aerial", "v3"), -0.3 * pts[["cont_aerial"]])
  # The hitout slot holds nothing under 3 channels, so it must shrink toward
  # exactly zero rather than toward a prior for a channel that does not exist.
  expect_equal(.epr_prior_rate_hitout("v3", channels = 3L), 0)
  # Under FOUR channels the ruck slot holds a real channel, so its prior must
  # carry its own factor. Zero was correct only while the 3-channel merge
  # emptied the slot; leaving it at zero now is the "blend of scaled and
  # unscaled parts" bug this file exists to catch.
  expect_equal(.epr_prior_rate_hitout("v3", channels = 4L),
               -0.3 * pts[["cont_stop"]] * EPV_RUCK_SWING_SCALE)
})

test_that("the live constants are exactly what their functions return for the live engine", {
  # The constants are defined as calls to these functions, so this pins that the
  # wiring stays that way: a constant edited back to a literal would let the v3
  # test above check a value production no longer reads.
  expect_identical(EPV3_POINTS_SCALE, .epv3_points_scale(EPV_ENGINE))
  expect_identical(EPR_PRIOR_GAMES_RECV,  .epr_prior_games("recv",  EPV_ENGINE))
  expect_identical(EPR_PRIOR_GAMES_DISP,  .epr_prior_games("disp",  EPV_ENGINE))
  expect_identical(EPR_PRIOR_GAMES_SPOIL, .epr_prior_games("spoil", EPV_ENGINE))
  expect_identical(EPV_STANDARDISE_CHANNELS, .epv_standardise_channels(EPV_ENGINE))
  expect_identical(EPR_PRIOR_RATE_HITOUT, .epr_prior_rate_hitout(EPV_ENGINE))
})
