# The three centring flags ship ON as of 2026-08-06, and until now nothing in
# the suite touched either helper behind them. These tests exist because the
# behaviour is now production, not because the functions are complicated.
#
# What each one is actually pinning is written on it. A test that recomputes the
# formula in its own body would pass while the function was broken -- that trap
# is already in this repo's test suite twice, so these call the real functions.

# ---- .remap_bench_role -----------------------------------------------------

test_that(".remap_bench_role resolves a bench start to the player's season role", {
  # One player, four games: three on the ground at RK, one starting on the
  # interchange. The INT game should come back RK, because that is the job he
  # does -- which is the entire point of the flag.
  slot   <- c("RK", "RK", "INT", "RK")
  pid    <- rep(1L, 4)
  season <- rep(2026L, 4)
  listed <- rep("RUCK", 4)
  out <- torp:::.remap_bench_role(slot, pid, season, listed)

  expect_equal(out[3], "RK")
  # The three non-bench games must be untouched -- the remap is for bench rows only.
  expect_equal(out[c(1, 2, 4)], c("RK", "RK", "RK"))
})

test_that(".remap_bench_role falls back to CAREER role when the season has none", {
  # 2025: on the ground at RK. 2026: bench every time. The 2026 rows have no
  # season role to use, so tier 2 (career) has to fire or they stay in the INT
  # cell -- which is the bug this whole thing exists to fix.
  slot   <- c("RK", "INT", "INT")
  pid    <- rep(7L, 3)
  season <- c(2025L, 2026L, 2026L)
  listed <- rep("RUCK", 3)
  out <- torp:::.remap_bench_role(slot, pid, season, listed)

  expect_equal(out[2:3], c("RK", "RK"))
})

test_that(".remap_bench_role falls back to the LISTED position when nothing else exists", {
  # A player who has never once started on the ground. Tiers 1 and 2 both miss,
  # so this lands on ROLE_FALLBACK_SLOT. Verified against the constant rather
  # than a hardcoded "RK", so the test tracks the mapping if it is ever changed.
  slot   <- c("INT", "INT")
  pid    <- rep(99L, 2)
  season <- rep(2026L, 2)
  listed <- rep("RUCK", 2)
  out <- torp:::.remap_bench_role(slot, pid, season, listed)

  expect_equal(unname(out), rep(unname(ROLE_FALLBACK_SLOT[["RUCK"]]), 2))
})

test_that(".remap_bench_role leaves a non-bench slot alone even with a mixed history", {
  slot   <- c("FB", "INT", "CHB")
  pid    <- rep(3L, 3)
  season <- rep(2026L, 3)
  listed <- rep("KEY_DEFENDER", 3)
  out <- torp:::.remap_bench_role(slot, pid, season, listed)

  expect_equal(out[1], "FB")
  expect_equal(out[3], "CHB")
  # And the bench row resolves to one of the two roles he actually played.
  expect_true(out[2] %in% c("FB", "CHB"))
})

test_that(".remap_bench_role errors loudly rather than silently mis-keying on a NULL input", {
  # `season` genuinely was NULL at the call site once, because the column is
  # created ~40 lines later in create_player_game_data(). A silent wrong answer
  # there would have centred everyone against the wrong cell.
  expect_error(
    torp:::.remap_bench_role(c("RK", "INT"), c(1L, 1L), NULL, c("RUCK", "RUCK"))
  )
})

# ---- .blend_adjust ---------------------------------------------------------

test_that(".blend_adjust removes the threshold cliff", {
  # Two players either side of the ramp with the SAME per-80 value. Under a hard
  # cell they would be centred against different means and their adjusted values
  # would jump; under the blend the difference must be small and continuous.
  lo <- EPV_RUCK_INVOLVEMENT_MIN - EPV_RUCK_BLEND_WIDTH / 2
  hi <- EPV_RUCK_INVOLVEMENT_MIN + EPV_RUCK_BLEND_WIDTH / 2

  set.seed(42)
  n <- 400
  involvement <- seq(0, 30, length.out = n)
  # Rucks genuinely have more hitout value; the blend must not erase that.
  p80 <- 0.2 * involvement + rnorm(n, 0, 0.5)
  tog <- rep(0.8, n)

  out <- torp:::.blend_adjust(p80, tog, involvement, lo, hi,
                              pooled_sd = stats::sd(p80), standardise = FALSE)

  # Adjacent players must not jump: the biggest step across the ramp should be
  # of the same order as the biggest step anywhere else, not a discontinuity.
  in_ramp <- which(involvement >= lo & involvement <= hi)
  step_ramp <- max(abs(diff(out[in_ramp])))
  step_all  <- max(abs(diff(out)))
  expect_lt(step_ramp, step_all * 1.5)
})

test_that(".blend_adjust preserves ordering within the ruck group", {
  # The channel must still say a better ruck is better. A centring scheme that
  # scrambles within-group order is measuring the cell, not the player.
  involvement <- rep(25, 50)          # all clearly rucks
  p80 <- seq(1, 10, length.out = 50)
  tog <- rep(0.8, 50)
  out <- torp:::.blend_adjust(p80, tog, involvement, 5, 15,
                              pooled_sd = 2, standardise = FALSE)
  expect_equal(order(out), order(p80))
})

test_that(".blend_adjust does not silently drop standardisation when one side is empty", {
  # `0 * NA` is NA in R, so an empty group on one side of the ramp used to make
  # the blended SD NA for EVERY row -- including rows that give that side no
  # weight -- and standardisation fell back for the whole vector with no warning.
  set.seed(1)
  n <- 200
  p80 <- rnorm(n, 5, 2); tog <- runif(n, 0.5, 1)
  involvement <- rep(0, n)            # nobody crosses the ramp: hi group empty

  out <- torp:::.blend_adjust(p80, tog, involvement, 5, 15,
                              pooled_sd = 2, standardise = TRUE)

  expect_false(anyNA(out))
  # And it must actually still be standardised, not quietly centred-only.
  centred_only <- (p80 - stats::weighted.mean(p80, tog)) * tog
  expect_false(isTRUE(all.equal(as.numeric(out), as.numeric(centred_only))))
})

test_that(".blend_adjust weights the reference continuously across the ramp", {
  # "A partial ruck gets a partial cell" is the claim the flag rests on, so pin
  # it directly: hold a player's OWN value fixed, vary only his involvement, and
  # the reference he is judged against must slide from the low cell to the high
  # one rather than switch.
  #
  # This needs a population where the two cells genuinely differ. An earlier
  # version of this test passed a constant p80, which makes both cell means
  # equal and correctly yields zero everywhere -- it was testing nothing.
  set.seed(11)
  pop_inv <- c(rep(0, 100), rep(30, 100))
  pop_p80 <- c(rnorm(100, 1, 0.3), rnorm(100, 5, 0.3))

  # Probes spread ACROSS the ramp interior, plus one either side of it. The
  # weight is clamped to [0, 1], so 0 and 5 contests both weigh 0 and 15 and 30
  # both weigh 1 -- outside the ramp the reference is meant to be flat, and only
  # between `lo` and `hi` does it slide. Testing for a slide outside the ramp
  # would be testing for a bug.
  probe_inv <- c(0, 5, 7.5, 10, 12.5, 15, 30)
  probe_p80 <- rep(3, 7)                       # identical player, seven roles
  n <- length(pop_inv) + length(probe_inv)

  out <- torp:::.blend_adjust(
    c(pop_p80, probe_p80), rep(1, n), c(pop_inv, probe_inv),
    lo = 5, hi = 15, pooled_sd = 1, standardise = FALSE)
  probes <- out[(length(pop_inv) + 1):n]
  names(probes) <- probe_inv

  # Inside the ramp: strictly decreasing, because the reference is sliding from
  # the part-timer cell up to the ruck cell.
  in_ramp <- probes[as.character(c(5, 7.5, 10, 12.5, 15))]
  expect_true(all(diff(in_ramp) < 0))

  # Outside the ramp: flat, because the weight is clamped.
  expect_equal(unname(probes[["0"]]), unname(probes[["5"]]))
  expect_equal(unname(probes[["30"]]), unname(probes[["15"]]))

  # And it must cross over: judged against part-timers he is above the mark,
  # against rucks he is below it.
  expect_gt(probes[["0"]], 0)
  expect_lt(probes[["30"]], 0)
})
