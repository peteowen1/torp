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
                  4.0, -1.0, 2.5, -3.0),
    # running score, booked on the row AFTER the scoring act as PBP does;
    # constant here so no row is terminal by score. The scoring test below
    # moves it.
    home_points = 0L, away_points = 0L,
    # frame, location and state for the stoppage rule; inert under "exclude"
    home = c(1L, 1L, 1L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 1L),
    x = 0, exp_pts = 0
  )
  stats <- data.table::data.table(
    match_id = c(rep("M1", 4), rep("M2", 4)),
    player_id = rep(c("p1", "p2", "p3", "p4"), 2),
    position = c("FF", "C", "FB", "WL", "FF", "C", "FB", "WL"),
    time_on_ground_percentage = c(90, 80, 100, 70, 90, 80, 100, 70),
    hitouts_to_advantage = c(2, 0, 3, 0, 2, 0, 3, 0),
    ruck_contests = c(5, 0, 6, 0, 5, 0, 6, 0),
    hitouts = c(2, 0, 3, 0, 2, 0, 3, 0),
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

# ---- adjacency: the defect conservation could never see --------------------
# Filtering rows out and THEN taking the next row steps over the gap, so the
# "next actor" becomes whoever followed it. On 2026 that made 19.2% of detected
# turnovers actually restarts, and classified 2,586 goals -- half of all goal
# kicks -- as turnovers, firing the defensive pool and paying the opposition for
# conceding. Every conservation test stayed green throughout: it is a pure
# attribution error and the totals never moved.
test_that("adjacency is taken on the unfiltered sequence", {
  f <- np_fixture()
  # M1 row 5 is a Kick by Away FC, immediately followed by the excluded Centre
  # Bounce (row 6). Its next actor must be NOBODY -- a restart is chain-terminal.
  # Filter-then-shift instead sees row 7 (Handball, Away FC) and calls it a
  # retained disposal.
  adj <- torp:::.np_adjacency(f$pbp)
  r5 <- adj[match_id == "M1" & display_order == 5]
  expect_true(is.na(r5$next_team))
  expect_true(is.na(r5$next_player))
  # ...and a genuine same-team pass still resolves.
  r1 <- adj[match_id == "M1" & display_order == 1]
  expect_equal(r1$next_team, "Home FC")
  expect_equal(r1$next_player, "p2")
})

test_that("a disposal into a restart is neither retained nor a turnover", {
  f <- np_fixture()
  led <- suppressMessages(torp:::.np_build_ledger(f$pbp))
  r5 <- led[match_id == "M1" & display_order == 5]
  expect_true(is.na(r5$next_team))
  # With no next team it cannot be a turnover, so no defensive pool fires for it
  # and no receiver share is paid. Verify by making the receiver share total:
  # p3's row-5 value must stay entirely with p3.
  a <- suppressMessages(build_net_points(f$pbp, f$stats, f$results,
                                         receiver_share = 1, defensive_share = 1,
                                         reconcile = FALSE))
  expect_equal(sum(a$net_points_hm), unname(sum(NP_FIXTURE_LEDGER_HM)),
               tolerance = 1e-10)
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

test_that("the default level is sum, and it barely disturbs the raw allocation", {
  # half_margin was the default until it was measured: its residual runs a median
  # 2.64 points against a median |net_points| of about the same size (102%), and
  # it reorders players at Spearman 0.7425 because it spreads by TOG and TOG
  # varies. "sum" gets the SAME margin identity for a median residual of 0.10.
  f <- np_fixture()
  d <- suppressMessages(build_net_points(f$pbp, f$stats, f$results))
  expect_equal(attr(d, "np_params")$level, "sum")

  hm <- suppressMessages(build_net_points(f$pbp, f$stats, f$results,
                                          level = "half_margin"))
  # The STRUCTURAL difference, which is exact and fixture-independent: under
  # "sum" both teams absorb the same total correction, (margin - total)/2 each,
  # so it is a pure shift. Under "half_margin" each team absorbs its own
  # distance from margin/2, which differs between them whenever the levels are
  # asymmetric -- and on real data they are, by a median of 61.4 points.
  #
  # The magnitude claim (median residual 0.10 vs 2.64, Spearman 0.9993 vs
  # 0.7425) is a real-data result and is recorded in docs/plans/EPV-NET-POINTS.md
  # rather than asserted here: this fixture's two teams happen to be symmetric,
  # so the two modes coincide on it exactly. Asserting it here would have been a
  # test that passes for the wrong reason.
  sides <- d[, .(r = sum(np_residual)), by = .(match_id, home_away)]
  h <- sides[home_away == "Home"][order(match_id)]
  a <- sides[home_away == "Away"][order(match_id)]
  expect_equal(h$match_id, a$match_id)
  # Equal and OPPOSITE in own-team frames: in the home-margin frame both sides
  # take the same +(margin - total)/2, and the away side's sign then flips on
  # output. Measured here as 9.2 / -9.2 -- if these ever came out equal with the
  # same sign, the final frame flip would have stopped being applied.
  expect_equal(h$r, -a$r, tolerance = 1e-10)

  for (x in list(d, hm)) {
    chk <- x[, .(a = sum(net_points_hm), m = data.table::first(margin)),
             by = match_id]
    expect_equal(chk$a, chk$m, tolerance = 1e-10)
  }
})

test_that("level = half_margin pins each team to half the margin", {
  f <- np_fixture()
  np <- suppressMessages(build_net_points(f$pbp, f$stats, f$results,
                                          level = "half_margin"))
  d <- np[, .(v = sum(net_points_hm)), by = .(match_id, team)]
  d <- merge(d, unique(np[, .(match_id, margin)]), by = "match_id")
  expect_equal(d$v, d$margin / 2, tolerance = 1e-10)
})

# ---- chains-aware sequence ---------------------------------------------------
# PBP is a subset of chains on (match_id, display_order). With chains supplied
# the VALUE must not move at all -- every point still comes from a PBP row --
# while each disposal learns what it resolved into (a spoil, a goal), which PBP
# never shows. The fixture spaces display_order by 10 so chains-only rows can
# sit between PBP rows.
np_chains_fixture <- function() {
  f <- np_fixture()
  pbp <- data.table::copy(f$pbp)
  pbp[, display_order := display_order * 10L]
  pbp[, team_id := data.table::fifelse(home_away == "Home", "H", "A")]
  extra <- data.table::data.table(
    match_id      = c("M1", "M1", "M1", "M2"),
    display_order = c(15L, 33L, 36L, 35L),
    description   = c("Kick Into F50", "Contest Target", "Spoil", "Goal"),
    team_id       = c("H", "H", "A", "H"),
    player_id     = c("p1", "p2", "p3", "p2")
  )
  chains <- data.table::rbindlist(list(
    pbp[, .(match_id, display_order, description, team_id, player_id)], extra),
    use.names = TRUE)
  data.table::setorder(chains, match_id, display_order)
  list(pbp = pbp, chains = chains, stats = f$stats, results = f$results)
}

test_that("chains change nothing about the allocation", {
  f <- np_chains_fixture()
  for (rs in c(0, 0.3)) {
    a <- suppressMessages(build_net_points(f$pbp, f$stats, f$results,
                                           receiver_share = rs))
    b <- suppressMessages(build_net_points(f$pbp, f$stats, f$results,
                                           chains = f$chains, receiver_share = rs))
    data.table::setorder(a, match_id, player_id)
    data.table::setorder(b, match_id, player_id)
    # the params attribute records that chains were supplied; the DATA must
    # be bit-identical
    expect_identical(as.data.frame(a), as.data.frame(b), ignore_attr = TRUE)
    expect_identical(a$net_points, b$net_points)
  }
  expect_true(attr(b, "np_params")$chains)
})

test_that("the sequence keeps adjacency on PBP rows and names the resolution", {
  f <- np_chains_fixture()
  l0 <- suppressMessages(torp:::.np_build_ledger(f$pbp))
  l1 <- suppressMessages(torp:::.np_build_ledger(f$pbp, f$chains))
  # same ledger rows, same value, same next actor: chains-only rows are not
  # states and must not become "who acted next"
  expect_equal(nrow(l1), nrow(l0))
  expect_identical(l1[, .(match_id, display_order, hm, next_team, next_player)],
                   l0[, .(match_id, display_order, hm, next_team, next_player)])
  expect_true(all(is.na(l0$resolve_desc)))
  # M1 row 30: Kick by p1, then Contest Target (in flight) then Spoil by p3.
  # Adjacency still says p3 (the next PBP row); resolution names the spoil.
  r30 <- l1[match_id == "M1" & display_order == 30]
  expect_equal(r30$next_player, "p3")
  expect_equal(r30$resolve_desc, "Spoil")
  expect_equal(r30$resolve_player, "p3")
  expect_equal(r30$resolve_team, "Away FC")
  expect_equal(r30$resolve_lag, 2L)
  # M1 row 10: Kick, then an in-flight annotation, then the Handball. The
  # annotation is skipped.
  r10 <- l1[match_id == "M1" & display_order == 10]
  expect_equal(r10$resolve_desc, "Handball")
  expect_equal(r10$resolve_player, "p2")
  expect_equal(r10$resolve_lag, 2L)
  # M2 row 30: a Kick resolving to a chains-only Goal row by the kicker.
  r30b <- l1[match_id == "M2" & display_order == 30]
  expect_equal(r30b$resolve_desc, "Goal")
  expect_equal(r30b$resolve_player, "p2")
  # a chains-only row is oriented by PBP's own team map
  s <- suppressMessages(torp:::.np_sequence(f$pbp, f$chains))
  sp <- s[match_id == "M1" & display_order == 36]
  expect_false(sp$in_pbp)
  expect_equal(sp$team, "Away FC")
  expect_equal(sp$home_away, "Away")
  expect_true(is.na(sp$delta_epv))
})

test_that("a sequence with holes or duplicates is refused", {
  f <- np_chains_fixture()
  holed <- f$chains[!(match_id == "M1" & display_order == 20)]
  expect_error(suppressMessages(torp:::.np_sequence(f$pbp, holed)),
               "not in")
  duped <- data.table::rbindlist(list(f$chains, f$chains[1]))
  expect_error(suppressMessages(torp:::.np_sequence(f$pbp, duped)),
               "duplicated")
  # same key, different act: not the same data
  wrong <- data.table::copy(f$chains)
  wrong[match_id == "M1" & display_order == 20, description := "Kick"]
  expect_error(suppressMessages(torp:::.np_sequence(f$pbp, wrong)),
               "disagree on 1 description")
  # same key and act, different player: would silently move credit (review
  # finding, 2026-09-06), so it must abort too
  wrong_p <- data.table::copy(f$chains)
  wrong_p[match_id == "M1" & display_order == 30, player_id := "pX"]
  expect_error(suppressMessages(torp:::.np_sequence(f$pbp, wrong_p)),
               "1 player")
  wrong_t <- data.table::copy(f$chains)
  wrong_t[match_id == "M1" & display_order == 30, team_id := "A"]
  expect_error(suppressMessages(torp:::.np_sequence(f$pbp, wrong_t)),
               "1 team")
})

# ---- scoring acts are terminal ----------------------------------------------
test_that("a behind is followed by a restart, not a turnover", {
  f <- np_fixture()
  # M1 row 3 is a Kick by Home p1 followed by an Away Uncontested Mark (row 4):
  # a turnover as the fixture stands. Book one home point from row 4 onward and
  # it becomes a behind followed by the kick-in.
  base <- suppressMessages(build_net_points(f$pbp, f$stats, f$results,
                                            defensive_share = 1, reconcile = FALSE))
  pbp <- data.table::copy(f$pbp)
  pbp[match_id == "M1" & display_order >= 4, home_points := 1L]
  adj <- torp:::.np_adjacency(pbp)
  r3 <- adj[match_id == "M1" & display_order == 3]
  expect_true(is.na(r3$next_team))
  expect_true(is.na(r3$next_player))
  # the row before it is untouched
  expect_equal(adj[match_id == "M1" & display_order == 2]$next_player, "p1")
  led <- suppressMessages(torp:::.np_build_ledger(pbp))
  expect_true(is.na(led[match_id == "M1" & display_order == 3]$next_team))
  # no defensive pool fires for it, so with defensive_share = 1 the kicker keeps
  # the whole row where before the whole row went to the opposition
  np <- suppressMessages(build_net_points(pbp, f$stats, f$results,
                                          defensive_share = 1, reconcile = FALSE))
  p1_before <- base[match_id == "M1" & player_id == "p1"]$net_points_hm
  p1_after  <- np[match_id == "M1" & player_id == "p1"]$net_points_hm
  expect_equal(p1_after - p1_before, 3.0, tolerance = 1e-10)
  # and the ledger total has not moved
  expect_equal(sum(np$net_points_hm), unname(sum(NP_FIXTURE_LEDGER_HM)),
               tolerance = 1e-10)
})

# ---- difficulty credit ------------------------------------------------------
# The terms are injected by hand so the arithmetic is checked, not the GAMs.
np_terms_fixture <- function() {
  data.table::data.table(
    match_id      = c("M1", "M1"),
    display_order = c(1L, 3L),
    # row 1: Kick by Home p1, retained by p2, delta 1.0 = 0.3 + 0.7
    # row 3: Kick by Home p1, turned over to Away p3, delta 3.0 = -0.5 + 3.5
    p_hat    = c(0.4, 0.5),
    decision = c(0.3, -0.5),
    surprise = c(0.7, 3.5)
  )
}

test_that("flat credit leaves np_team at zero and the totals where they were", {
  f <- np_fixture()
  np <- suppressMessages(build_net_points(f$pbp, f$stats, f$results, reconcile = FALSE))
  expect_true(all(np$np_team == 0))
  expect_equal(sum(np$net_points_hm), unname(sum(NP_FIXTURE_LEDGER_HM)), tolerance = 1e-10)
})

test_that("difficulty credit pays decision, split surprise, blame and the pools", {
  f <- np_fixture()
  np <- suppressMessages(build_net_points(
    f$pbp, f$stats, f$results, credit = "difficulty",
    difficulty_terms = np_terms_fixture(), reconcile = FALSE, spread = "tog",
    receiver_share = 0.3, defensive_share = 0.3, ball_winner_share = 1,
    blame_share = 0.3, offence_pool_share = 0.1))
  m1 <- np[match_id == "M1"]
  p1 <- m1[player_id == "p1"]; p2 <- m1[player_id == "p2"]; p3 <- m1[player_id == "p3"]
  # p1, row 1 retained: keeps 0.9 * (0.3 + 0.4 * 0.7) = 0.522; p2 receives
  # 0.9 * 0.6 * 0.7 = 0.378; the Home pool takes 0.1.
  # p1, row 3 turnover: keeps -0.5 + 0.3 * 3.5 = 0.55, cedes 0.7 * 3.5 = 2.45;
  # np_direct reports the row at face value (3.0) and np_ceded the transfer.
  # p1 also receives the flat share of p2's unscored handball (row 2, -2.0):
  # 0.3 * -2.0 = -0.6.
  expect_equal(p1$np_direct, 0.522 + 3.0 - 0.6, tolerance = 1e-9)
  expect_equal(p1$np_ceded, -2.45, tolerance = 1e-9)
  # p3 is away: he won the ball with an Uncontested Mark, so under difficulty
  # credit the by-act table (0.80, NP_BALL_WINNER_SHARE_BY_ACT) applies, not
  # the flat ball_winner_share; reported in his own frame
  expect_equal(p3$np_defensive_won, -0.8 * 2.45, tolerance = 1e-9)
  # the offence pool (0.1 from row 1) spreads by TOG over Home FC's p1 (90)
  # and p2 (80)
  expect_equal(p1$np_team, 0.1 * 90 / 170, tolerance = 1e-9)
  expect_equal(p2$np_team, 0.1 * 80 / 170, tolerance = 1e-9)
  # p2's direct: his own unscored handball under the flat rule (keeps 70% of
  # -2.0) plus the surprise he received on row 1
  expect_equal(p2$np_direct, -1.4 + 0.378, tolerance = 1e-9)
  # and nothing was created or destroyed
  expect_equal(sum(np$net_points_hm), unname(sum(NP_FIXTURE_LEDGER_HM)), tolerance = 1e-10)
  expect_equal(attr(np, "np_params")$credit, "difficulty")
})

test_that("difficulty terms that do not rebuild the row are refused", {
  f <- np_fixture()
  bad <- np_terms_fixture()
  bad[1, surprise := 0.9]  # 0.3 + 0.9 != 1.0
  expect_error(
    suppressMessages(build_net_points(f$pbp, f$stats, f$results, credit = "difficulty",
                                      difficulty_terms = bad)),
    "do not rebuild")
})

test_that("difficulty credit without chains or terms is refused", {
  f <- np_fixture()
  expect_error(
    suppressMessages(build_net_points(f$pbp, f$stats, f$results, credit = "difficulty")),
    "needs")
})

# ---- contested kicks: three terms, three recipients (D8) ----------------------
test_that("a contest the defence won pays the winner, the pool and the ground ball", {
  f <- np_fixture()
  # M1 row 3 (Kick, Home p1, 3.0, turned over to Away p3) becomes a spoil by p3:
  # decision -0.5, contest surprise -1.5, ground surprise 5.0 (sums to 3.0).
  terms <- np_terms_fixture()
  terms[display_order == 3, `:=`(contested = TRUE, cont_desc = "Spoil",
                                 cont_surprise = -1.5, ground_surprise = 5.0,
                                 def_win = TRUE, winner_pid = "p3")]
  np <- suppressMessages(build_net_points(
    f$pbp, f$stats, f$results, credit = "difficulty", difficulty_terms = terms,
    reconcile = FALSE, spread = "tog", receiver_share = 0.3, defensive_share = 0.3,
    ball_winner_share = 1, blame_share = 0.3, offence_pool_share = 0.1))
  m1 <- np[match_id == "M1"]
  p1 <- m1[player_id == "p1"]; p3 <- m1[player_id == "p3"]; p4 <- m1[player_id == "p4"]
  # p1 keeps decision + 0.3 of both losses: -0.5 - 0.45 + 1.5 = 0.55; cedes
  # 0.7 * -1.5 = -1.05 at the contest and 0.7 * 5.0 = 3.5 on the ground ball.
  # Face value is unchanged (0.55 - 1.05 + 3.5 = 3.0), so np_direct is as before.
  expect_equal(p1$np_direct, 0.522 + 3.0 - 0.6, tolerance = 1e-9)
  expect_equal(p1$np_ceded, -(3.5 - 1.05), tolerance = 1e-9)
  # p3 (away): 80% of the ground-ball cession (he won it with an Uncontested
  # Mark, by-act table) and half the contest cession as the spoiler
  # (NP_CONTEST_WINNER_SHARE["Spoil"] = 0.5), both in his own frame
  expect_equal(p3$np_defensive_won, -0.8 * 3.5, tolerance = 1e-9)
  expect_equal(p3$np_contest_won, 0.525, tolerance = 1e-9)
  # the pool is the other half of the contest cession (-0.525 in the home
  # frame) plus the unpaid 20% of the ground-ball cession (+0.7): +0.175, spread
  # by TOG over Away FC (p3 100, p4 70) and read in their own frame
  expect_equal(p3$np_defensive, -0.175 * 100 / 170, tolerance = 1e-9)
  expect_equal(p4$np_defensive, -0.175 * 70 / 170, tolerance = 1e-9)
  expect_equal(sum(np$net_points_hm), unname(sum(NP_FIXTURE_LEDGER_HM)), tolerance = 1e-10)
})

test_that("a contest the attack won pays the same-team winner directly", {
  f <- np_fixture()
  # M2 row 3 (Kick, Home p2, 2.5, retained by p1): a contested mark by p1.
  terms <- data.table::data.table(
    match_id = "M2", display_order = 3L, p_hat = 0.5, decision = 0.5, surprise = 2.0,
    contested = TRUE, cont_desc = "Contested Mark", cont_surprise = 1.5,
    ground_surprise = 0.5, def_win = FALSE, winner_pid = "p1")
  np <- suppressMessages(build_net_points(
    f$pbp, f$stats, f$results, credit = "difficulty", difficulty_terms = terms,
    reconcile = FALSE, spread = "tog", receiver_share = 0.3, defensive_share = 0.3,
    ball_winner_share = 1, blame_share = 0.3, offence_pool_share = 0.1))
  m2 <- np[match_id == "M2"]
  p1 <- m2[player_id == "p1"]; p2 <- m2[player_id == "p2"]
  # p2 keeps 0.9 * decision; p1 gets 0.9 * (1.5 + 0.5) as winner and receiver;
  # the Home pool takes 0.1 * 2.5.
  expect_equal(p2$np_direct, 0.9 * 0.5, tolerance = 1e-9)
  # p1's other rows: row 1 Kick (4.0, unscored turnover, face value) and row 4
  # Handball (-3.0, terminal)
  expect_equal(p1$np_direct, 4.0 - 3.0 + 0.9 * 2.0, tolerance = 1e-9)
  expect_equal(p1$np_team, 0.25 * 90 / 170, tolerance = 1e-9)
  expect_equal(p2$np_team, 0.25 * 80 / 170, tolerance = 1e-9)
  expect_true(all(m2$np_contest_won == 0))
  expect_equal(sum(np$net_points_hm), unname(sum(NP_FIXTURE_LEDGER_HM)), tolerance = 1e-10)
})

# ---- routing by act and the context spread (D11, D12) -------------------------
test_that("under difficulty the ball-winner's share follows what he did", {
  f <- np_fixture()
  # M1 row 3: p1 turns it over to p3, whose next act is an Uncontested Mark
  # (0.80 by act) -- the flat rule would pay ball_winner_share.
  np <- suppressMessages(build_net_points(
    f$pbp, f$stats, f$results, credit = "difficulty",
    difficulty_terms = np_terms_fixture(), reconcile = FALSE, spread = "tog",
    ball_winner_share = 0.6, blame_share = 0.3, offence_pool_share = 0))
  p3 <- np[match_id == "M1" & player_id == "p3"]
  # ceded on row 3 = 0.7 * 3.5 = 2.45; an intercept mark takes 80% of it
  expect_equal(p3$np_defensive_won, -0.8 * 2.45, tolerance = 1e-9)
  expect_equal(sum(np$net_points_hm), unname(sum(NP_FIXTURE_LEDGER_HM)), tolerance = 1e-10)
})

test_that("the context spread conserves and honours an observed pairing", {
  f <- np_fixture()
  # Away FC in M1 is p3 (FB, 100% TOG, 3 tackles ...) and p4 (WL, 70%).
  # A pairing says p4 was the one contesting p1's kicks.
  pairs <- data.table::data.table(match_id = "M1", att = "p1", def = "p4", n = 3L)
  base <- suppressMessages(build_net_points(
    f$pbp, f$stats, f$results, reconcile = FALSE, spread = "context",
    ball_winner_share = 0, defensive_share = 1))
  paired <- suppressMessages(build_net_points(
    f$pbp, f$stats, f$results, reconcile = FALSE, spread = "context",
    ball_winner_share = 0, defensive_share = 1, contest_pairs = pairs))
  for (np in list(base, paired)) {
    expect_equal(sum(np$net_points_hm), unname(sum(NP_FIXTURE_LEDGER_HM)), tolerance = 1e-10)
  }
  # p1's turnovers in M1 pool to Away FC; with the pairing, p4's share of
  # those pools rises and p3's falls (compared in size: away players read the
  # pool in their own frame), and the team sum is unchanged
  b <- base[match_id == "M1"]; q <- paired[match_id == "M1"]
  expect_gt(abs(q[player_id == "p4"]$np_defensive), abs(b[player_id == "p4"]$np_defensive))
  expect_lt(abs(q[player_id == "p3"]$np_defensive), abs(b[player_id == "p3"]$np_defensive))
  expect_equal(q[team == "Away FC", sum(np_defensive)], b[team == "Away FC", sum(np_defensive)],
               tolerance = 1e-10)
  # And the exact weights, from the constants, so a mislabelled component would
  # fail (review, 2026-09-06). The only Away pool in M1 is p1's row-3 turnover
  # (3.0, all of it pooled). p3 is FB (p1 is FF, so p3 is the mirror), TOG 100,
  # defensive acts 3+7+2+1+1 = 14; p4 is WL, TOG 70, acts 4+8+3+1+1 = 17.
  cw <- NP_CONTEXT_WEIGHTS
  w3 <- cw[["acts"]] * 14 / 31 + cw[["mirror"]] * 1 + cw[["tog"]] * 100 / 170
  w4 <- cw[["acts"]] * 17 / 31 + cw[["mirror"]] * 0 + cw[["tog"]] * 70 / 170
  expect_equal(b[player_id == "p4"]$np_defensive, -3.0 * w4 / (w3 + w4), tolerance = 1e-9)
  expect_equal(b[player_id == "p3"]$np_defensive, -3.0 * w3 / (w3 + w4), tolerance = 1e-9)
  w4p <- w4 + cw[["pair"]] * 1
  expect_equal(q[player_id == "p4"]$np_defensive, -3.0 * w4p / (w3 + w4p), tolerance = 1e-9)
  # a pairing table with a repeated key is summed, not truncated to its last
  # row: p4 seen 2 + 1 times and p3 once must equal p4 3, p3 1 (pair shares
  # 0.75 / 0.25), where truncation would give 1 / 1 (0.5 / 0.5)
  dup <- data.table::data.table(match_id = "M1", att = "p1", def = c("p4", "p4", "p3"), n = c(2L, 1L, 1L))
  agg <- data.table::data.table(match_id = "M1", att = "p1", def = c("p4", "p3"), n = c(3L, 1L))
  run <- function(pr) suppressMessages(build_net_points(
    f$pbp, f$stats, f$results, reconcile = FALSE, spread = "context",
    ball_winner_share = 0, defensive_share = 1, contest_pairs = pr))[match_id == "M1"]
  expect_equal(run(dup)[player_id == "p4"]$np_defensive, run(agg)[player_id == "p4"]$np_defensive,
               tolerance = 1e-10)
  w4d <- w4 + cw[["pair"]] * 0.75; w3d <- w3 + cw[["pair"]] * 0.25
  expect_equal(run(dup)[player_id == "p4"]$np_defensive, -3.0 * w4d / (w3d + w4d), tolerance = 1e-9)
})

# ---- the opposition is the other roster, never the resolution row -------------
test_that("a contest the defence won but the attack regathered pays the opposition", {
  f <- np_fixture()
  # M1 row 1: Kick by Home p1, retained by p2 (delta 1.0). Terms say the
  # defence (p3) spoiled it and Home regathered: decision 0.3, contest -0.5,
  # ground 1.2. No chains are supplied, so the ledger's resolve_team is NA and
  # the opposition must be derived from the roster.
  terms <- data.table::data.table(
    match_id = "M1", display_order = 1L, p_hat = 0.4, decision = 0.3, surprise = 0.7,
    contested = TRUE, cont_desc = "Spoil", cont_surprise = -0.5, ground_surprise = 1.2,
    def_win = TRUE, winner_pid = "p3")
  np <- suppressMessages(build_net_points(
    f$pbp, f$stats, f$results, credit = "difficulty", difficulty_terms = terms,
    reconcile = FALSE, spread = "tog", blame_share = 0.3, offence_pool_share = 0.1))
  expect_false(anyNA(np$team))
  expect_equal(nrow(np[, .N, by = .(match_id, player_id)][N > 1]), 0)
  p3 <- np[match_id == "M1" & player_id == "p3"]
  expect_equal(p3$team, "Away FC")
  # spoiler takes half of the ceded 0.7 * -0.5 = -0.35 (home frame), own frame +
  expect_equal(p3$np_contest_won, 0.5 * 0.35, tolerance = 1e-9)
  # p2 receives 0.9 * 1.2 as the regatherer
  expect_equal(np[match_id == "M1" & player_id == "p2"]$np_direct, -1.4 + 0.9 * 1.2, tolerance = 1e-9)
  expect_equal(sum(np$net_points_hm), unname(sum(NP_FIXTURE_LEDGER_HM)), tolerance = 1e-10)
  # a winner who is not on the opposition's roster: share to the pool, no
  # phantom row, total unchanged
  terms[, winner_pid := "pZ"]
  np2 <- suppressWarnings(suppressMessages(build_net_points(
    f$pbp, f$stats, f$results, credit = "difficulty", difficulty_terms = terms,
    reconcile = FALSE, spread = "tog", blame_share = 0.3, offence_pool_share = 0.1)))
  expect_false("pZ" %in% np2$player_id)
  expect_true(all(np2$np_contest_won == 0))
  expect_equal(np2[match_id == "M1" & team == "Away FC", sum(np_defensive)],
               np[match_id == "M1" & team == "Away FC", sum(np_defensive + np_contest_won)],
               tolerance = 1e-10)
  expect_equal(sum(np2$net_points_hm), unname(sum(NP_FIXTURE_LEDGER_HM)), tolerance = 1e-10)
})

# ---- the payment table rebuilds the ledger ------------------------------------
test_that("return_payments gives one row per act and recipient that sums to the ledger", {
  f <- np_fixture()
  terms <- np_terms_fixture()
  terms[display_order == 3, `:=`(contested = TRUE, cont_desc = "Spoil",
                                 cont_surprise = -1.5, ground_surprise = 5.0,
                                 def_win = TRUE, winner_pid = "p3")]
  np <- suppressMessages(build_net_points(
    f$pbp, f$stats, f$results, credit = "difficulty", difficulty_terms = terms,
    reconcile = FALSE, spread = "tog", return_payments = TRUE))
  pay <- attr(np, "np_payments")
  expect_true(data.table::is.data.table(pay))
  expect_setequal(unique(pay$role), c("actor", "receiver", "contest_winner", "ball_winner",
                                      "attack_pool", "defence_pool"))
  # the table is the ledger, re-expressed
  expect_equal(sum(pay$hm), unname(sum(NP_FIXTURE_LEDGER_HM)), tolerance = 1e-10)
  # and it names the spoiler on row 3 at his share
  r3 <- pay[match_id == "M1" & display_order == 3]
  expect_equal(r3[role == "contest_winner"]$player_id, "p3")
  expect_equal(r3[role == "contest_winner"]$team, "Away FC")
  expect_equal(r3[role == "contest_winner"]$hm, 0.7 * -1.5 * 0.5, tolerance = 1e-9)
  # per row, named payments + pools = the row's value
  led <- suppressMessages(torp:::.np_build_ledger(f$pbp))
  chk <- merge(pay[, .(paid = sum(hm)), by = .(match_id, display_order)],
               led[, .(match_id, display_order, hm)], by = c("match_id", "display_order"))
  expect_equal(chk$paid, chk$hm, tolerance = 1e-9)
})

# ---- stoppages: baseline, repricing, rucks (D15) -------------------------------
np_stoppage_fixture <- function() {
  f <- np_fixture()
  pbp <- data.table::copy(f$pbp)
  pbp[, display_order := display_order * 10L]
  # M2 between row 20 (Handball, Away p3, -1.0) and row 30 (Kick, Home p2, 2.5):
  # a ball-up whose state is filled from the eventual winner (home), then a
  # Gather From Hitout by Home p2 -- the ruck (p1 has the hitouts) tapped it.
  stop <- data.table::data.table(
    match_id = "M2", display_order = 25L, description = "Ball Up Call",
    team = NA_character_, home_away = NA_character_, player_id = NA_character_,
    delta_epv = 0.6, home_points = 0L, away_points = 0L, home = 1L, x = 10, exp_pts = 0.4)
  # the first possession becomes a Gather From Hitout (was a Kick) with the
  # same value, so the ledger total moves only by the stoppage row itself
  pbp[match_id == "M2" & display_order == 30L, description := "Gather From Hitout"]
  # M1's deliberately absurd +99 centre bounce is a stoppage too; it belongs to
  # the exclusion test, not to this one
  pbp <- pbp[!(match_id == "M1" & display_order == 60L)]
  pbp <- data.table::rbindlist(list(pbp, stop), use.names = TRUE)
  data.table::setorder(pbp, match_id, display_order)
  list(pbp = pbp, stats = f$stats, results = f$results)
}

test_that("stoppage rows are excluded by default and allocated on request", {
  f <- np_stoppage_fixture()
  # baseline injected: this ball-up is worth 0.1 to the home side at neutral
  bl <- data.table::data.table(description = "Ball Up Call", band = 0, baseline = 0.1, n = 1L)
  ex <- suppressMessages(build_net_points(f$pbp, f$stats, f$results, reconcile = FALSE))
  expect_true(all(ex$np_stoppage == 0))
  # the same difficulty build with and without the stoppage, so pool sums can
  # be compared net of the turnover pools that exist either way
  ex_d <- suppressMessages(build_net_points(
    f$pbp, f$stats, f$results, credit = "difficulty", difficulty_terms = np_terms_fixture(),
    reconcile = FALSE, spread = "tog", offence_pool_share = 0))
  al <- suppressMessages(build_net_points(
    f$pbp, f$stats, f$results, credit = "difficulty", difficulty_terms = np_terms_fixture(),
    reconcile = FALSE, spread = "tog", offence_pool_share = 0, stoppages = "allocate",
    stoppage_baseline = bl, stoppage_loser_share = 0.5))
  # the ledger now carries the stoppage row's raw value, and only that: the
  # repricing below is a transfer between two rows of the same match and can
  # never move a total, so this line says nothing about shares or direction
  # (those are asserted row by row further down)
  expect_equal(sum(al$net_points_hm) - sum(ex$net_points_hm), 0.6, tolerance = 1e-10)
  # repricing: adj = baseline - exp_pts * sgn = 0.1 - 0.4 = -0.3. The row before
  # (Away p3's handball, delta -1.0, so +1.0 in the home frame) is paid +adj =
  # 0.7; the stoppage swing is 0.6 - adj = 0.9: the home side won the ball at
  # 0.9 above neutral.
  led <- suppressMessages(torp:::.np_build_ledger(f$pbp, stoppages = "allocate",
                                                  stoppage_baseline = bl))
  expect_equal(led[match_id == "M2" & display_order == 20L]$hm, 0.7, tolerance = 1e-10)
  expect_equal(led[match_id == "M2" & display_order == 25L]$hm, 0.9, tolerance = 1e-10)
  expect_true(led[match_id == "M2" & display_order == 25L]$is_stoppage)
  # split: winner half 0.45 (hitout: ruck 0.5 / player 0.3 / pool 0.2), loser
  # half 0.45 (rucks 0.5 by contests lost, pool 0.5). Home rucks: p1 has the
  # hitouts_to_advantage (2), p2 none. Away rucks by ruck_contests - hitouts:
  # p3 (6 - 3 = 3), p4 (0).
  m2 <- al[match_id == "M2"]
  expect_equal(m2[player_id == "p1"]$np_stoppage, 0.45 * 0.5, tolerance = 1e-9)   # ruck credit
  expect_equal(m2[player_id == "p2"]$np_stoppage, 0.45 * 0.3, tolerance = 1e-9)   # gatherer
  # p3 is away: the losing ruck wears 0.45 * 0.5 in the home frame = -0.225 own
  expect_equal(m2[player_id == "p3"]$np_stoppage, -0.45 * 0.5, tolerance = 1e-9)
  # p4 has no act in M2 and no stoppage payment, so no row at all
  expect_equal(nrow(m2[player_id == "p4"]), 0)
  # pools, net of the turnover pools both builds share: home 0.45 * 0.2 = 0.09,
  # away 0.45 * 0.5 = 0.225 (home frame, so negative in Away's own frame)
  e2 <- ex_d[match_id == "M2"]
  expect_equal(m2[team == "Home FC", sum(np_defensive)] - e2[team == "Home FC", sum(np_defensive)],
               0.09, tolerance = 1e-9)
  expect_equal(m2[team == "Away FC", sum(np_defensive)] - e2[team == "Away FC", sum(np_defensive)],
               -0.225, tolerance = 1e-9)
  # and the payment table rebuilds the ledger, stoppage rows included
  al2 <- suppressMessages(build_net_points(
    f$pbp, f$stats, f$results, credit = "difficulty", difficulty_terms = np_terms_fixture(),
    reconcile = FALSE, spread = "tog", offence_pool_share = 0, stoppages = "allocate",
    stoppage_baseline = bl, return_payments = TRUE))
  pay <- attr(al2, "np_payments")
  expect_equal(sum(pay$hm), sum(al2$net_points_hm), tolerance = 1e-9)
  expect_setequal(pay[display_order == 25L]$role, c("stoppage_ruck", "stoppage_player", "stoppage_pool"))
})

test_that("allocating stoppages under the flat rule is refused", {
  f <- np_stoppage_fixture()
  expect_error(suppressMessages(build_net_points(f$pbp, f$stats, f$results, stoppages = "allocate")),
               "difficulty")
})

test_that("consecutive stoppages telescope, and a stoppage that opens a match keeps its swing", {
  f <- np_stoppage_fixture()
  pbp <- data.table::copy(f$pbp)
  # a second ball-up straight after the first (no possession between)
  second <- pbp[match_id == "M2" & display_order == 25L]
  second[, `:=`(display_order = 27L, delta_epv = 0.2, exp_pts = 0.3)]
  # and a centre bounce as the very first row of M1 (home wins it: +0.8)
  opener <- data.table::copy(second)[, `:=`(match_id = "M1", display_order = 5L,
                                            description = "Centre Bounce", delta_epv = 0.8,
                                            exp_pts = 0, x = 0)]
  pbp <- data.table::rbindlist(list(pbp, second, opener), use.names = TRUE)
  data.table::setorder(pbp, match_id, display_order)
  bl <- data.table::data.table(description = c("Ball Up Call", "Centre Bounce"),
                               band = 0, baseline = c(0.1, 0.05), n = 1L)
  led <- suppressMessages(torp:::.np_build_ledger(pbp, stoppages = "allocate", stoppage_baseline = bl))
  raw <- suppressMessages(torp:::.np_build_ledger(pbp, stoppages = "exclude"))
  m2 <- led[match_id == "M2"]
  # adj25 = 0.1 - 0.4 = -0.3; adj27 = 0.1 - 0.3 = -0.2. Row 20 takes +adj25,
  # row 25 takes -adj25 + adj27, row 27 takes -adj27: one payer and one
  # receiver per adj, and the three rows still sum to the raw 1.0 + 0.6 + 0.2.
  expect_equal(m2[display_order == 20L]$hm, 1.0 - 0.3, tolerance = 1e-10)
  expect_equal(m2[display_order == 25L]$hm, 0.6 + 0.3 - 0.2, tolerance = 1e-10)
  expect_equal(m2[display_order == 27L]$hm, 0.2 + 0.2, tolerance = 1e-10)
  expect_equal(m2[display_order %in% c(20L, 25L, 27L), sum(hm)], 1.8, tolerance = 1e-10)
  # the opener has no row before it, so it keeps its raw home-frame value and
  # nothing is paid to a row that does not exist
  expect_equal(led[match_id == "M1" & display_order == 5L]$hm, 0.8, tolerance = 1e-10)
  # and the match totals are exactly the excluded totals plus the raw stoppage values
  expect_equal(led[, sum(hm)], raw[, sum(hm)] + 0.6 + 0.2 + 0.8, tolerance = 1e-10)
})

# ---- the v4 engine's channel mapping ----------------------------------------
test_that("the v4 channels are own / won / pools and nothing else", {
  np <- data.table::data.table(
    player_id = c("a", "b"), match_id = "M",
    np_direct = c(5, -1), np_ceded = c(-2, 0.5), np_defensive_won = c(1, 0),
    np_contest_won = c(0.5, 0), np_stoppage = c(0, 2), np_defensive = c(0.3, 0.2),
    np_team = c(0.1, 0.1), np_residual = c(0.05, -0.05))
  np[, net_points := np_direct + np_ceded + np_defensive_won + np_contest_won + np_stoppage +
                     np_defensive + np_team + np_residual]
  ch <- torp:::.np_v4_channels(np)
  expect_equal(ch[player_id == "a"]$np_own, 3)          # 5 - 2: own acts net of what he ceded
  expect_equal(ch[player_id == "a"]$np_won, 1.5)        # turnover + contest won
  expect_equal(ch[player_id == "a"]$np_pool, 0.45)      # pools + residual
  expect_equal(ch[player_id == "b"]$np_won, 2)          # a ruck's stoppage credit is "won"
  expect_equal(ch$np_own + ch$np_won + ch$np_pool, ch$net_points, tolerance = 1e-12)
  # a frame whose parts do not add up is refused
  np[1, np_residual := 9]
  expect_error(torp:::.np_v4_channels(np), "do not sum")
})
