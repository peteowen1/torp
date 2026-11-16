# The engine-keyed EPR constants, pinned to the values v4 publishes.
#
# Until 2026-11-16 these were computed by per-engine helpers
# (.epr_prior_games(), .epr_prior_rate(), .epr_prior_rate_hitout(),
# .epv3_points_scale(), .epv_standardise_channels()), and this file checked the
# v2 and v3 branches of them. Those engines and helpers were removed; the
# constants are literals now, and every one of them is in
# .rating_defining_constants(), so a change here also trips the nightly vintage
# guard. This test says what the values ARE, so an accidental edit fails here
# first, with a readable message, rather than at check_vintage_alignment().

test_that("the v4 EPR shrinkage constants hold the values v4 was gated and published with", {
  expect_identical(EPV_ENGINE, "v4")
  expect_identical(c(EPR_PRIOR_GAMES_RECV, EPR_PRIOR_GAMES_DISP, EPR_PRIOR_GAMES_SPOIL),
                   c(14.38, 24.33, 11.09))
  expect_identical(c(EPR_PRIOR_RATE_RECV, EPR_PRIOR_RATE_DISP, EPR_PRIOR_RATE_SPOIL),
                   c(-0.7, -0.7, -0.3))
  # the hitout slot is identically zero under v4, so its prior is exactly zero
  expect_identical(EPR_PRIOR_RATE_HITOUT, 0)
  expect_identical(EPV3_POINTS_SCALE, c(recv = 1, disp = 1, cont_aerial = 1, cont_stop = 1))
  expect_identical(EPV_STANDARDISE_CHANNELS, c("recv", "disp"))
})
