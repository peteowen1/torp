# Characterisation tests for the stat-rating opponent adjustment.
#
# `.compute_rolling_stat_profiles()` and `.compute_team_defensive_profiles()`
# had ZERO test coverage as of 2026-08-11. Their EPV twin
# (`.compute_rolling_epv_profiles()`) nominally had five tests, but on reading
# them they are all about the 2026-07-27 stale-vintage / all-NA guard, not
# about shrinkage arithmetic, decay direction or causality. So these are the
# FIRST arithmetic tests for the whole family, not a catch-up to a
# well-covered sibling -- an earlier version of this header said otherwise and
# was wrong.
#
# All three implement the same decay-weighted, shrunk-to-league-average team
# profile over strictly-prior games -- the shrinkage line is duplicated
# verbatim between `opponent_adjustment.R:333` and
# `epv_opponent_adjustment.R:94`.
#
# These tests exist BEFORE any attempt to share that core. Refactoring an
# untested function that feeds published stat ratings is the dangerous order;
# pinning its behaviour first is the safe one. Expected values below are hand
# computed, not produced by re-running the code, so they would survive the
# implementation being replaced.
#
# The fixture uses lambda_decay = 0 deliberately: with every decay weight equal
# to 1 the arithmetic is exact and checkable by hand. Decay itself is then
# tested separately, where it is the only moving part.

.opp_fixture <- function(target_date = as.Date("2026-01-11")) {
  # Two prior matches, same day, four teams, one stat.
  #   M1: A produces 10 against B  -> B allowed 10
  #       B produces 30 against A  -> A allowed 30
  #   M2: C produces 20 against D  -> D allowed 20
  #       D produces 40 against C  -> C allowed 40
  # League average of what is allowed = mean(10, 30, 20, 40) = 25.
  d0 <- as.Date("2026-01-01")
  data.table::data.table(
    match_id = c("M1", "M1", "M2", "M2", "M3", "M3"),
    team     = c("A", "B", "C", "D", "A", "C"),
    opponent = c("B", "A", "D", "C", "C", "A"),
    match_date_rating = c(d0, d0, d0, d0, target_date, target_date),
    disposals = c(10, 30, 20, 40, 999, 999)  # M3's own values must not matter
  )
}

.rate_sources <- c(disposals = "disposals")
.cap <- c(0.7, 1.4)

# Hand-computed, prior_games = 5, league average 25:
#   B: shrunk = (1*10 + 5*25)/6 = 22.5      factor = 25/22.5      = 1.1111111
#   A: shrunk = (1*30 + 5*25)/6 = 25.83333  factor = 25/25.833333 = 0.9677419
#   D: shrunk = (1*20 + 5*25)/6 = 24.16667  factor = 25/24.166667 = 1.0344828
#   C: shrunk = (1*40 + 5*25)/6 = 27.5      factor = 25/27.5      = 0.9090909
.expected <- c(A = 25 / (155 / 6), B = 25 / (135 / 6),
               C = 25 / (165 / 6), D = 25 / (145 / 6))

# --- .compute_team_defensive_profiles ----------------------------------------

test_that("defensive profiles shrink each team toward the league average", {
  dt <- .opp_fixture()
  prior <- dt[match_date_rating < as.Date("2026-01-11")]

  res <- .compute_team_defensive_profiles(
    prior, ref_date = as.Date("2026-01-11"), lambda_decay = 0,
    rate_sources = .rate_sources, cap = .cap
  )

  expect_setequal(res$team, c("A", "B", "C", "D"))
  got <- stats::setNames(res$disposals_adj_factor, res$team)
  expect_equal(got[c("A", "B", "C", "D")], .expected[c("A", "B", "C", "D")],
               tolerance = 1e-10)
})

test_that("a team that concedes MORE than average deflates, less than average boosts", {
  # The direction of the factor is the thing most likely to be silently
  # inverted by a refactor, and it is not self-evident from the code.
  dt <- .opp_fixture()
  prior <- dt[match_date_rating < as.Date("2026-01-11")]
  res <- .compute_team_defensive_profiles(
    prior, ref_date = as.Date("2026-01-11"), lambda_decay = 0,
    rate_sources = .rate_sources, cap = .cap
  )
  got <- stats::setNames(res$disposals_adj_factor, res$team)

  # C allowed 40 (worst defence) -> players who faced C get deflated.
  expect_lt(got[["C"]], 1)
  # B allowed 10 (best defence) -> players who faced B get boosted.
  expect_gt(got[["B"]], 1)
})

test_that("the factor cap clamps both ends", {
  d0 <- as.Date("2026-01-01")
  dt <- data.table::data.table(
    match_id = c("M1", "M1"),
    team = c("A", "B"), opponent = c("B", "A"),
    match_date_rating = c(d0, d0),
    # A monstrous spread so the raw factor blows through the cap.
    disposals = c(1, 10000)
  )
  # prior_games = 0 is required to actually reach the cap: with the default 5,
  # shrinkage toward the league mean pulls even a 1-vs-10000 spread back inside
  # c(0.7, 1.4) on its own. Worth knowing -- the cap is a backstop behind the
  # shrinkage, not the primary control.
  res <- .compute_team_defensive_profiles(
    dt, ref_date = as.Date("2026-01-11"), lambda_decay = 0,
    rate_sources = .rate_sources, cap = c(0.7, 1.4), prior_games = 0
  )
  expect_true(all(res$disposals_adj_factor >= 0.7))
  expect_true(all(res$disposals_adj_factor <= 1.4))
  # Both ends actually hit, not merely respected.
  expect_setequal(res$disposals_adj_factor, c(0.7, 1.4))
})

test_that("shrinkage alone keeps an extreme spread inside the cap", {
  # The complement of the test above, stated as its own fact: at the default
  # prior_games the cap never fires on a 1-vs-10000 fixture.
  d0 <- as.Date("2026-01-01")
  dt <- data.table::data.table(
    match_id = c("M1", "M1"), team = c("A", "B"), opponent = c("B", "A"),
    match_date_rating = c(d0, d0), disposals = c(1, 10000)
  )
  res <- .compute_team_defensive_profiles(
    dt, ref_date = as.Date("2026-01-11"), lambda_decay = 0,
    rate_sources = .rate_sources, cap = c(0.7, 1.4), prior_games = 5
  )
  expect_true(all(res$disposals_adj_factor > 0.7))
  expect_true(all(res$disposals_adj_factor < 1.4))
})

test_that("decay makes an older match count for less", {
  # Isolated: one team, two prior matches, identical except age.
  mk <- function(gap_days) {
    ref <- as.Date("2026-03-01")
    data.table::data.table(
      match_id = c("M1", "M1", "M2", "M2"),
      team = c("A", "B", "A", "B"), opponent = c("B", "A", "B", "A"),
      match_date_rating = c(ref - gap_days, ref - gap_days, ref - 1, ref - 1),
      disposals = c(10, 50, 50, 10)
    )
  }
  near <- .compute_team_defensive_profiles(
    mk(2), ref_date = as.Date("2026-03-01"), lambda_decay = 0.05,
    rate_sources = .rate_sources, cap = .cap)
  far <- .compute_team_defensive_profiles(
    mk(200), ref_date = as.Date("2026-03-01"), lambda_decay = 0.05,
    rate_sources = .rate_sources, cap = .cap)

  nf <- stats::setNames(near$disposals_adj_factor, near$team)
  ff <- stats::setNames(far$disposals_adj_factor, far$team)

  # DIRECTION, not merely difference. An earlier version of this test asserted
  # only that near != far, which an inverted decay sign (exp(+lambda*days),
  # older games counting for MORE) would have passed while validating exactly
  # the wrong behaviour.
  #
  # A allowed 50 in the old match and 10 in the recent one. Ageing the old
  # match out must therefore pull A's allowance DOWN toward 10 -- a stingier
  # defence -- which RAISES its factor, since factor = league / allowed.
  expect_gt(ff[["A"]], nf[["A"]])
  # B is the mirror image and must move the other way.
  expect_lt(ff[["B"]], nf[["B"]])
})

# --- .compute_rolling_stat_profiles ------------------------------------------

test_that("rolling profiles use only STRICTLY prior matches", {
  # The causality guarantee. If this ever loosens to <=, a match would be
  # adjusted using its own result and every backtest silently gains foresight.
  res <- .compute_rolling_stat_profiles(
    .opp_fixture(), lambda_decay = 0, rate_sources = .rate_sources,
    cap = .cap, prior_games = 5
  )
  m3 <- res[match_id == "M3"]
  got <- stats::setNames(m3$disposals_adj_factor, m3$opponent)

  expect_setequal(m3$opponent, c("A", "B", "C", "D"))
  # Exactly the same numbers as the as-of snapshot: M3's own inflated values
  # (999) contributed nothing.
  expect_equal(got[c("A", "B", "C", "D")], .expected[c("A", "B", "C", "D")],
               tolerance = 1e-10)
})

test_that("a match with no prior history gets a neutral factor of exactly 1", {
  res <- .compute_rolling_stat_profiles(
    .opp_fixture(), lambda_decay = 0, rate_sources = .rate_sources,
    cap = .cap, prior_games = 5
  )
  m1 <- res[match_id == "M1"]
  expect_gt(nrow(m1), 0)
  expect_true(all(m1$disposals_adj_factor == 1.0))
})

test_that("same-day matches do not inform each other", {
  # M1 and M2 are on the same date. Neither may contribute to the other, or
  # the adjustment leaks within-round information.
  res <- .compute_rolling_stat_profiles(
    .opp_fixture(), lambda_decay = 0, rate_sources = .rate_sources,
    cap = .cap, prior_games = 5
  )
  expect_true(all(res[match_id %in% c("M1", "M2")]$disposals_adj_factor == 1.0))
})

test_that("prior_games controls how hard the profile is pulled to the league mean", {
  # prior_games = 0 means no shrinkage: the factor is league / own mean.
  # A wide cap here so the unshrunk value is visible -- under the production
  # cap c(0.7, 1.4) both of these clamp, which would hide what is being tested.
  none <- .compute_rolling_stat_profiles(
    .opp_fixture(), lambda_decay = 0, rate_sources = .rate_sources,
    cap = c(0.01, 100), prior_games = 0
  )
  g <- stats::setNames(none[match_id == "M3"]$disposals_adj_factor,
                       none[match_id == "M3"]$opponent)
  expect_equal(g[["C"]], 25 / 40, tolerance = 1e-10)   # unshrunk
  expect_equal(g[["B"]], 25 / 10, tolerance = 1e-10)   # unshrunk

  # A huge prior drags every team to ~1.
  heavy <- .compute_rolling_stat_profiles(
    .opp_fixture(), lambda_decay = 0, rate_sources = .rate_sources,
    cap = .cap, prior_games = 10000
  )
  h <- heavy[match_id == "M3"]$disposals_adj_factor
  expect_true(all(abs(h - 1) < 0.01))
})

test_that("the rolling path clamps to the cap too, not just the as-of path", {
  # The clamp is written out separately in each function. Testing it only on
  # .compute_team_defensive_profiles() would let a refactor fumble it in the
  # rolling path and still pass everything.
  d0 <- as.Date("2026-01-01")
  dt <- data.table::data.table(
    match_id = c("M1", "M1", "M2", "M2"),
    team = c("A", "B", "A", "B"), opponent = c("B", "A", "B", "A"),
    match_date_rating = c(d0, d0, d0 + 10, d0 + 10),
    disposals = c(1, 10000, 5, 5)
  )
  res <- .compute_rolling_stat_profiles(
    dt, lambda_decay = 0, rate_sources = .rate_sources,
    cap = c(0.7, 1.4), prior_games = 0
  )
  m2 <- res[match_id == "M2"]
  expect_gt(nrow(m2), 0)
  expect_true(all(m2$disposals_adj_factor >= 0.7))
  expect_true(all(m2$disposals_adj_factor <= 1.4))
  expect_setequal(m2$disposals_adj_factor, c(0.7, 1.4))
})

test_that("the rolling and as-of implementations agree on the same prior window", {
  # They are separate code paths -- one loops per match, one takes a single
  # ref_date -- computing the same quantity. Nothing asserted that before.
  dt <- .opp_fixture()
  rolling <- .compute_rolling_stat_profiles(
    dt, lambda_decay = 0, rate_sources = .rate_sources,
    cap = .cap, prior_games = 5)
  as_of <- .compute_team_defensive_profiles(
    dt[match_date_rating < as.Date("2026-01-11")],
    ref_date = as.Date("2026-01-11"), lambda_decay = 0,
    rate_sources = .rate_sources, cap = .cap)

  r <- rolling[match_id == "M3"]
  a <- stats::setNames(as_of$disposals_adj_factor, as_of$team)
  expect_equal(
    stats::setNames(r$disposals_adj_factor, r$opponent)[names(a)],
    a, tolerance = 1e-10
  )
})
