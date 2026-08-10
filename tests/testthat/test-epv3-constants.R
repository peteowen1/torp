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
#      on 2026-08-04 and this test is why it was caught before commit.

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
  # Re-source the constants with the flag flipped, rather than asserting on the
  # live values -- this is the only way to check the v3 branch without shipping
  # it, and it also proves the file still LOADS with the flag on, which is the
  # ordering trap above.
  f <- system.file("R", "constants_ratings.R", package = "torp")
  if (!nzchar(f) || !file.exists(f)) {
    f <- testthat::test_path("..", "..", "R", "constants_ratings.R")
  }
  skip_if_not(file.exists(f), "constants_ratings.R not locatable from the test tree")

  txt <- sub('^EPV_ENGINE <- "v2"$', 'EPV_ENGINE <- "v3"', readLines(f))
  expect_true(any(grepl('^EPV_ENGINE <- "v3"$', txt)),
              info = "the EPV_ENGINE assignment no longer matches the pattern this test rewrites")

  tf <- tempfile(fileext = ".R")
  writeLines(txt, tf)
  e <- new.env()
  # If any engine-conditioned constant is defined ABOVE EPV_ENGINE this throws.
  expect_no_error(sys.source(tf, envir = e))

  expect_equal(get("EPR_PRIOR_GAMES_RECV", e), 14.38)
  expect_equal(get("EPR_PRIOR_GAMES_DISP", e), 24.33)
  expect_equal(get("EPR_PRIOR_GAMES_SPOIL", e), 11.09)
  expect_equal(get("EPV_STANDARDISE_CHANNELS", e), c("recv", "disp"))

  pts <- get("EPV3_POINTS_SCALE", e)
  expect_equal(unname(pts), c(3.3413, 2.6078, 0.5226, 1))

  # Each prior rate must carry its OWN channel's factor. This is the check that
  # would have caught the 0.919 fallback bug from the other direction.
  expect_equal(unname(get("EPR_PRIOR_RATE_RECV", e)),  -0.7 * pts[["recv"]])
  expect_equal(unname(get("EPR_PRIOR_RATE_DISP", e)),  -0.7 * pts[["disp"]])
  expect_equal(unname(get("EPR_PRIOR_RATE_SPOIL", e)), -0.3 * pts[["cont_aerial"]])
  # The hitout slot holds nothing under 3 channels, so it must shrink toward
  # exactly zero rather than toward a prior for a channel that does not exist.
  expect_equal(unname(get("EPR_PRIOR_RATE_HITOUT", e)), 0)
})
