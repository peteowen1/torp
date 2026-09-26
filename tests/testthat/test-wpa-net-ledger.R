# The WPA ledger's correctness, like net points', can be ASSERTED: every team
# sums to its result minus its pre-match chance or it does not. These tests
# check that identity exactly. As in test-epv-net-points.R, they cannot see
# whether the RIGHT players are paid; that is checked on real matches with
# Pete (docs/plans/WPA-NET-LEDGER.md section 4).

wpa_fixture <- function() {
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
    # win probability in the acting team's frame, and its change
    wp  = c(0.60, 0.62, 0.65, 0.33, 0.30, 0.70, 0.28, 0.25,
            0.50, 0.48, 0.55, 0.52),
    wpa = c(0.02, 0.03, -0.02, -0.03, 0.02, 0.00, -0.03, 0.01,
            0.02, -0.07, -0.03, 0.04),
    home_points = 0L, away_points = 0L,
    home_team_name = "Home FC", away_team_name = "Away FC",
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
  pre <- data.table::data.table(match_id = c("M1", "M2"), home_win_prob = c(0.60, 0.45))
  list(pbp = pbp, stats = stats, results = results, pre = pre)
}

run_wpa <- function(f = wpa_fixture(), pre = f$pre, results = f$results) {
  suppressMessages(build_wpa_ledger(f$pbp, f$stats, pre, results = results))
}

team_totals <- function(out, f = wpa_fixture()) {
  ha <- unique(f$pbp[, .(match_id, player_id, home_away)], by = c("match_id", "player_id"))
  merge(out, ha, by = c("match_id", "player_id"))[
    , .(total = sum(wpa_net)), by = .(match_id, home_away)]
}

test_that("each team sums to its result minus its pre-match chance, and the teams cancel", {
  out <- run_wpa()
  tt <- team_totals(out)
  # M1: home won at 0.60 -> +0.40 / -0.40. M2: home lost at 0.45 -> -0.45 / +0.45.
  want <- c(M1.Home = 0.40, M1.Away = -0.40, M2.Home = -0.45, M2.Away = 0.45)
  got <- stats::setNames(tt$total, paste(tt$match_id, tt$home_away, sep = "."))
  expect_equal(got[names(want)], want, tolerance = 1e-12)
  expect_equal(tt[, sum(total), by = match_id]$V1, c(0, 0), tolerance = 1e-12)
})

test_that("the parts add up to wpa_net", {
  out <- run_wpa()
  expect_equal(out$wpa_own + out$wpa_won + out$wpa_team, out$wpa_net, tolerance = 1e-12)
})

test_that("the neutral start pins every match to the home edge, whatever the forecast", {
  f <- wpa_fixture()
  out <- run_wpa(f, pre = .wpa_neutral_pre_match(f$pbp$match_id, home_prob = 0.57))
  tt <- team_totals(out)
  # M1 home won -> +0.43; M2 home lost -> -0.57
  expect_equal(tt[match_id == "M1" & home_away == "Home", total], 1 - 0.57, tolerance = 1e-12)
  expect_equal(tt[match_id == "M2" & home_away == "Home", total], 0 - 0.57, tolerance = 1e-12)
  expect_equal(unique(attr(out, "targets")$p0_source), "neutral")
})

test_that("a draw is half a win", {
  f <- wpa_fixture()
  res <- data.table::copy(f$results)[match_id == "M1", away_score := 100]
  tt <- team_totals(run_wpa(f, results = res))
  expect_equal(tt[match_id == "M1" & home_away == "Home", total], 0.5 - 0.60, tolerance = 1e-12)
})

test_that("a match with no forecast is left out, said so, and does not disturb the rest", {
  f <- wpa_fixture()
  expect_message(out <- build_wpa_ledger(f$pbp, f$stats, f$pre[match_id == "M1"], results = f$results),
                 "no pre-match forecast")
  expect_equal(attr(out, "skipped"), "M2")
  expect_false("M2" %in% out$match_id)
  tt <- team_totals(out)
  expect_equal(tt[match_id == "M1" & home_away == "Home", total], 0.40, tolerance = 1e-12)
})

test_that("no forecast for any match is refused", {
  f <- wpa_fixture()
  expect_error(suppressMessages(build_wpa_ledger(f$pbp, f$stats, f$pre[0], results = f$results)),
               "no match has a pre-match forecast")
})

test_that("the forecast's source is recorded per match", {
  f <- wpa_fixture()
  pre <- data.table::copy(f$pre)[, source := c("locked", "retrodiction")]
  tg <- attr(run_wpa(f, pre = pre), "targets")
  expect_equal(tg[order(match_id), p0_source], c("locked", "retrodiction"))
})

test_that("dropped players' value re-spreads to team-mates and the team total holds", {
  f <- wpa_fixture()
  out <- run_wpa(f)
  keep <- out[!(match_id == "M1" & player_id == "p2"), .(player_id, match_id)]
  kept <- suppressMessages(.wpa_respread_lost(out, keep, f$pbp, f$stats))
  expect_false(any(kept$match_id == "M1" & kept$player_id == "p2"))
  expect_equal(team_totals(kept)[order(match_id, home_away), total],
               team_totals(out)[order(match_id, home_away), total], tolerance = 1e-12)
  # a team share, not the team-mates' own play
  m <- merge(kept, out, by = c("match_id", "player_id"), suffixes = c("", ".before"))
  expect_equal(m$wpa_own, m$wpa_own.before)
  expect_equal(m$wpa_net - m$wpa_net.before, m$wpa_team - m$wpa_team.before, tolerance = 1e-12)
})

test_that("bad inputs are refused with the problem named", {
  f <- wpa_fixture()
  expect_error(build_wpa_ledger(f$pbp[, !"wpa"], f$stats, f$pre, results = f$results), "wpa")
  expect_error(build_wpa_ledger(f$pbp, f$stats, f$pre[, .(match_id, p = home_win_prob)],
                                results = f$results), "home_win_prob")
  bad <- data.table::copy(f$pre)[1, home_win_prob := 1.2]
  expect_error(build_wpa_ledger(f$pbp, f$stats, bad, results = f$results), "outside 0 to 1")
  expect_error(build_wpa_ledger(f$pbp, f$stats, f$pre, results = f$results, scale = 0), "scale")
})

test_that("the scale is only units: output is the same whatever the engine ran in", {
  f <- wpa_fixture()
  a <- suppressMessages(build_wpa_ledger(f$pbp, f$stats, f$pre, results = f$results, scale = 100))
  b <- suppressMessages(build_wpa_ledger(f$pbp, f$stats, f$pre, results = f$results, scale = 1000))
  data.table::setkey(a, match_id, player_id); data.table::setkey(b, match_id, player_id)
  expect_equal(a$wpa_net, b$wpa_net, tolerance = 1e-9)
})
