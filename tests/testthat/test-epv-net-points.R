# Net points is the one metric here whose correctness can be ASSERTED rather
# than eyeballed: the margin is a known total and the allocation either sums to
# it or it does not. So these tests check the identity exactly (to floating
# point), not to a tolerance chosen to make them pass.
#
# What these tests CANNOT see, stated up front so nobody reads a green run as
# more than it is: conservation holds no matter WHICH players are paid. A rule
# that credited the margin entirely to the first player alphabetically would
# pass every test in this file. Whether the right people are credited is a
# football question, checked in data-raw/04-analysis/, not here.

# ---- fixture ---------------------------------------------------------------
# Two matches, hand-built so every expected number can be worked out by hand.
np_fixture <- function() {
  pbp <- data.table::data.table(
    match_id = c(rep("M1", 8), rep("M2", 4)),
    display_order = c(1:8, 1:4),
    description = c("Kick", "Handball", "Kick", "Uncontested Mark",
                    "Kick", "Centre Bounce", "Handball", "Kick",
                    "Kick", "Handball", "Kick", "Handball"),
    team = c("Home FC", "Home FC", "Home FC", "Away FC",
             "Away FC", "Home FC", "Away FC", "Away FC",
             "Home FC", "Away FC", "Home FC", "Home FC"),
    home_away = c("Home", "Home", "Home", "Away",
                  "Away", "Home", "Away", "Away",
                  "Home", "Away", "Home", "Home"),
    player_id = c("p1", "p2", "p1", "p3", "p3", "p2", "p4", "p3",
                  "p1", "p3", "p2", "p1"),
    delta_epv = c(1.0, -2.0, 3.0, 0.5, -1.5, 99.0, 2.0, -0.5,
                  4.0, -1.0, 2.5, -3.0)
  )
  stats <- data.table::data.table(
    match_id = c(rep("M1", 4), rep("M2", 4)),
    player_id = rep(c("p1", "p2", "p3", "p4"), 2),
    position = c("FF", "C", "FB", "WL", "FF", "C", "FB", "WL"),
    time_on_ground_percentage = c(90, 80, 100, 70, 90, 80, 100, 70),
    tackles = c(1, 2, 3, 4, 1, 2, 3, 4),
    pressure_acts = c(5, 6, 7, 8, 5, 6, 7, 8),
    spoils = c(0, 1, 2, 3, 0, 1, 2, 3),
    intercepts = c(1, 1, 1, 1, 1, 1, 1, 1),
    one_percenters = c(0, 0, 1, 1, 0, 0, 1, 1)
  )
  results <- data.table::data.table(
    match_id = c("M1", "M2"),
    home_team_name = "Home FC", away_team_name = "Away FC",
    home_score = c(100, 60), away_score = c(80, 75)
  )
  list(pbp = pbp, stats = stats, results = results)
}

# ---- the identity ----------------------------------------------------------
test_that("allocation sums to the margin exactly, in every match", {
  f <- np_fixture()
  np <- suppressMessages(build_net_points(f$pbp, f$stats, f$results))
  chk <- np[, .(alloc = sum(net_points_hm), margin = data.table::first(margin)),
            by = match_id]
  expect_equal(chk$alloc, chk$margin, tolerance = 1e-10)
  expect_silent(suppressMessages(check_net_points_conservation(np)))
})

test_that("check_net_points_conservation aborts on a broken ledger", {
  f <- np_fixture()
  np <- suppressMessages(build_net_points(f$pbp, f$stats, f$results))
  np[1, net_points_hm := net_points_hm + 10]
  expect_error(check_net_points_conservation(np), "does not conserve")
})

test_that("the team difference IS the margin, in own-team frames", {
  f <- np_fixture()
  np <- suppressMessages(build_net_points(f$pbp, f$stats, f$results))
  d <- np[, .(v = sum(net_points)), by = .(match_id, home_away)]
  h <- d[home_away == "Home"]; a <- d[home_away == "Away"]
  got <- merge(h[, .(match_id, h = v)], a[, .(match_id, a = v)], by = "match_id")
  got <- merge(got, unique(np[, .(match_id, margin)]), by = "match_id")
  expect_equal(got$h - got$a, got$margin, tolerance = 1e-10)
})

test_that("components sum to the total", {
  f <- np_fixture()
  np <- suppressMessages(build_net_points(f$pbp, f$stats, f$results))
  expect_equal(np$np_direct + np$np_defensive + np$np_defensive_won +
                 np$np_ceded + np$np_residual,
               np$net_points, tolerance = 1e-10)
})

# ---- the test that is NOT satisfied by the reconciler ----------------------
# Everything above runs with reconcile = TRUE, which FORCES the match total to
# the margin. So those tests would stay green on a ledger with a sign error, a
# leaking spread rule, or a dropped act type -- the reconciler would quietly
# absorb all of it into np_residual. Confirmed by mutation: flipping the away
# sign in .np_build_ledger() leaves every reconciled test passing.
#
# This is the one that actually constrains the allocation: with the reconciler
# off, the allocated total must equal the LEDGER total, because allocation may
# move value and may never create or destroy it.
# The expected totals are worked out BY HAND from the fixture below, not read
# back from .np_build_ledger(). Deriving them from the function under test makes
# the check self-referential: an away-sign mutation moves both sides together
# and the test stays green. That mutant passed all 48 tests on 2026-09-05 before
# these numbers were hard-coded.
#
#   M1, Centre Bounce (+99) excluded, away rows negated:
#     +1.0 -2.0 +3.0  -0.5 +1.5  -2.0 +0.5  =  1.5
#   M2:
#     +4.0 +1.0 +2.5 -3.0                    =  4.5
NP_FIXTURE_LEDGER_HM <- c(M1 = 1.5, M2 = 4.5)

test_that("the ledger orients away acts against the home margin", {
  f <- np_fixture()
  led <- suppressMessages(torp:::.np_build_ledger(f$pbp))
  away <- led[home_away == "Away"]
  src <- f$pbp[home_away == "Away" & description != "Centre Bounce"]
  expect_equal(sum(away$hm), -sum(src$delta_epv), tolerance = 1e-10)
  got <- led[, .(hm = sum(hm)), by = match_id]
  expect_equal(got$hm, unname(NP_FIXTURE_LEDGER_HM[got$match_id]),
               tolerance = 1e-10)
})

test_that("raw allocation neither creates nor destroys ledger value", {
  f <- np_fixture()
  for (sp in c("matchup", "defensive_acts", "tog")) {
    for (phi in c(0, 0.3, 0.9)) {
      np <- suppressMessages(build_net_points(
        f$pbp, f$stats, f$results, defensive_share = phi,
        spread = sp, reconcile = FALSE))
      got <- np[, .(v = sum(net_points_hm)), by = match_id]
      expect_equal(got$v, unname(NP_FIXTURE_LEDGER_HM[got$match_id]),
                   tolerance = 1e-10, info = paste(sp, phi))
    }
  }
})

test_that("residual is zero when the ledger already lands on the margin", {
  # A reconciler that always fires cannot be distinguished from one that fires
  # only when needed, unless a case exists where it must do nothing.
  f <- np_fixture()
  res <- data.table::copy(f$results)
  res[, home_score := 100]
  res[, away_score := 100 - unname(NP_FIXTURE_LEDGER_HM[match_id])]
  np <- suppressMessages(build_net_points(f$pbp, f$stats, res, level = "sum"))
  expect_lt(max(abs(np$np_residual)), 1e-9)
})

# ---- invariance: a reallocation may never change a total -------------------
# This is the property that makes the spread rule safe to change later, so it is
# worth pinning rather than assuming.
test_that("conservation holds for every spread rule and every share", {
  f <- np_fixture()
  for (sp in c("matchup", "defensive_acts", "tog")) {
    for (phi in c(0, 0.3, 1)) {
      for (alpha in c(0, 0.5)) {
        np <- suppressMessages(build_net_points(
          f$pbp, f$stats, f$results, defensive_share = phi,
          receiver_share = alpha, spread = sp))
        chk <- np[, .(a = sum(net_points_hm), m = data.table::first(margin)),
                  by = match_id]
        expect_equal(chk$a, chk$m, tolerance = 1e-10,
                     info = paste(sp, phi, alpha))
      }
    }
  }
})

test_that("the receiver split moves value without changing any team total", {
  f <- np_fixture()
  a0 <- suppressMessages(build_net_points(f$pbp, f$stats, f$results,
                                          receiver_share = 0, reconcile = FALSE))
  a5 <- suppressMessages(build_net_points(f$pbp, f$stats, f$results,
                                          receiver_share = 0.5, reconcile = FALSE))
  t0 <- a0[, .(v = sum(net_points_hm)), by = .(match_id, team)]
  t5 <- a5[, .(v = sum(net_points_hm)), by = .(match_id, team)]
  tt <- merge(t0, t5, by = c("match_id", "team"))
  expect_equal(tt$v.x, tt$v.y, tolerance = 1e-10)
  # ...and it must actually move something, or the test above is vacuous.
  p0 <- a0[, .(match_id, player_id, v = net_points_hm)]
  p5 <- a5[, .(match_id, player_id, v = net_points_hm)]
  pp <- merge(p0, p5, by = c("match_id", "player_id"))
  expect_true(max(abs(pp$v.x - pp$v.y)) > 1e-6)
})

test_that("the defensive share moves value BETWEEN teams but not the margin", {
  f <- np_fixture()
  d0 <- suppressMessages(build_net_points(f$pbp, f$stats, f$results,
                                          defensive_share = 0, reconcile = FALSE))
  d9 <- suppressMessages(build_net_points(f$pbp, f$stats, f$results,
                                          defensive_share = 0.9, reconcile = FALSE))
  # the match total is untouched...
  m0 <- d0[, .(v = sum(net_points_hm)), by = match_id]
  m9 <- d9[, .(v = sum(net_points_hm)), by = match_id]
  expect_equal(m0$v, merge(m0, m9, by = "match_id")$v.y, tolerance = 1e-10)
  # ...but a team total IS, which is the whole point of a cross-team transfer.
  t0 <- d0[, .(v = sum(net_points_hm)), by = .(match_id, team)]
  t9 <- d9[, .(v = sum(net_points_hm)), by = .(match_id, team)]
  tt <- merge(t0, t9, by = c("match_id", "team"))
  expect_true(max(abs(tt$v.x - tt$v.y)) > 1e-6)
})

# ---- the exclusion rule ----------------------------------------------------
test_that("centre-bounce phantom value is excluded, not merely dropped", {
  f <- np_fixture()
  # The fixture puts a deliberately absurd +99 on a Centre Bounce row with a
  # named player and a real team, so only an explicit description rule can
  # remove it -- the is.na(team) filter cannot.
  np <- suppressMessages(build_net_points(f$pbp, f$stats, f$results,
                                          reconcile = FALSE))
  expect_lt(max(abs(np$net_points_hm)), 50)
  expect_true("Centre Bounce" %in% NP_EXCLUDED_DESCS)
})

# ---- the mirror map --------------------------------------------------------
test_that("the positional mirror is symmetric and closed", {
  m <- NP_POSITION_MIRROR
  expect_true(all(m %in% names(m)))
  expect_equal(unname(m[unname(m)]), unname(names(m)))
})

test_that("mirror lookup warns rather than silently dropping an unknown slot", {
  expect_warning(out <- .np_mirror_of(c("FF", "NOT_A_SLOT")), "no mirror")
  expect_equal(out, c("FB", "NOT_A_SLOT"))
})

# ---- guards ----------------------------------------------------------------
test_that("the observed ball-winner is paid, and paying him conserves", {
  f <- np_fixture()
  b0 <- suppressMessages(build_net_points(f$pbp, f$stats, f$results,
                                          ball_winner_share = 0, reconcile = FALSE))
  b1 <- suppressMessages(build_net_points(f$pbp, f$stats, f$results,
                                          ball_winner_share = 1, reconcile = FALSE))
  expect_equal(sum(b0$np_defensive_won), 0)
  expect_true(sum(abs(b1$np_defensive_won)) > 1e-6)
  # routing the pool differently must not change any match total
  for (b in list(b0, b1)) {
    got <- b[, .(v = sum(net_points_hm)), by = match_id]
    expect_equal(got$v, unname(NP_FIXTURE_LEDGER_HM[got$match_id]),
                 tolerance = 1e-10)
  }
  # ...and it must actually move who is paid, or the check above is vacuous
  m <- merge(b0[, .(match_id, player_id, v = net_points_hm)],
             b1[, .(match_id, player_id, v = net_points_hm)],
             by = c("match_id", "player_id"))
  expect_true(max(abs(m$v.x - m$v.y)) > 1e-6)
})

test_that("out-of-range shares are refused", {
  f <- np_fixture()
  expect_error(suppressMessages(build_net_points(
    f$pbp, f$stats, f$results, defensive_share = 1.5)), "must be one number")
  expect_error(suppressMessages(build_net_points(
    f$pbp, f$stats, f$results, receiver_share = -1)), "must be one number")
})

test_that("missing pbp columns abort with the column named", {
  f <- np_fixture()
  bad <- data.table::copy(f$pbp)[, delta_epv := NULL]
  expect_error(suppressMessages(build_net_points(bad, f$stats, f$results)),
               "delta_epv")
})

test_that("matchup spread refuses to degrade silently when positions are absent", {
  f <- np_fixture()
  bad <- data.table::copy(f$stats)[, position := NA_character_]
  expect_error(
    suppressMessages(build_net_points(f$pbp, bad, f$results, spread = "matchup")),
    "flat spread")
})

test_that("level = half_margin pins each team to half the margin", {
  f <- np_fixture()
  np <- suppressMessages(build_net_points(f$pbp, f$stats, f$results,
                                          level = "half_margin"))
  d <- np[, .(v = sum(net_points_hm)), by = .(match_id, team)]
  d <- merge(d, unique(np[, .(match_id, margin)]), by = "match_id")
  expect_equal(d$v, d$margin / 2, tolerance = 1e-10)
})
