# The merged "Contest" display channel = epr_spoil + epr_hitout.
#
# The claim that makes this safe to show is that the merge is EXACT: the four-way
# split and the three-way split reconcile to epr identically. If that ever stops
# holding, a displayed Contest number would silently disagree with the EPR beside
# it, which is worse than showing two small columns.

test_that("plot_team_ratings() accepts contest and still accepts its two halves", {
  # The formal is the literal call `c("epr", "recv", ...)`; as.character() on it
  # yields "c" followed by the options, so drop the first. Avoids eval().
  opts <- as.character(formals(plot_team_ratings)$metric)[-1]
  expect_true("contest" %in% opts)
  # The halves stay reachable -- this is a display merge, not a removal.
  expect_true(all(c("spoil", "hitout") %in% opts))
  # And epr stays the default.
  expect_identical(opts[1], "epr")
})

test_that("contest is the exact sum, so the three-way split reconciles like the four-way", {
  tr <- data.frame(
    season = rep(2026L, 3), round = rep(23L, 3),
    team = c("A", "B", "C"),
    team_epr       = c(10.0, -4.0, 0.5),
    team_epr_recv  = c(6.0, -2.0, 0.2),
    team_epr_disp  = c(3.0, -1.5, 0.1),
    team_epr_spoil = c(0.7, -0.4, 0.15),
    team_epr_hitout = c(0.3, -0.1, 0.05)
  )
  contest <- tr$team_epr_spoil + tr$team_epr_hitout

  four_way  <- tr$team_epr_recv + tr$team_epr_disp + tr$team_epr_spoil + tr$team_epr_hitout
  three_way <- tr$team_epr_recv + tr$team_epr_disp + contest

  # expect_equal, NOT expect_identical, and the reason is worth recording so
  # nobody tightens it again: merging reassociates the sum from
  # ((recv+disp)+spoil)+hitout to (recv+disp)+(spoil+hitout), and floating-point
  # addition is not associative, so the last bit can differ (measured:
  # 0.50000000000000000 vs 0.50000000000000011, i.e. ~1e-16). That is the only
  # error the merge introduces.
  #
  # Separately, the published TEAM columns already reconcile only to ~0.02,
  # because run_ratings_pipeline.R round()s each team_epr_* to 2dp
  # independently. The four-way split carries that too -- merging does not
  # widen it.
  expect_equal(three_way, four_way)
  # The plot builds the same column the caller would.
  p <- plot_team_ratings(team_ratings = tr, metric = "contest", season = 2026)
  expect_s3_class(p, "ggplot")
  expect_equal(sort(p$data$team_epr_contest), sort(contest))
})

test_that("contest aborts rather than guessing when a half is missing", {
  tr <- data.frame(
    season = 2026L, round = 23L, team = "A",
    team_epr = 10, team_epr_recv = 6, team_epr_disp = 3,
    team_epr_spoil = 0.7            # team_epr_hitout absent
  )
  # Silently plotting spoil-only under a "Contest" label would be a wrong
  # number wearing the right name.
  expect_error(
    plot_team_ratings(team_ratings = tr, metric = "contest", season = 2026),
    "team_epr_hitout"
  )
})

test_that("the existing metrics are unchanged by the addition", {
  tr <- data.frame(
    season = rep(2026L, 2), round = rep(23L, 2), team = c("A", "B"),
    team_epr = c(10, -4), team_epr_recv = c(6, -2), team_epr_disp = c(3, -1.5),
    team_epr_spoil = c(0.7, -0.4), team_epr_hitout = c(0.3, -0.1)
  )
  for (m in c("epr", "recv", "disp", "spoil", "hitout")) {
    p <- plot_team_ratings(team_ratings = tr, metric = m, season = 2026)
    expect_s3_class(p, "ggplot")
  }
  # Identity: adding the contest option must not have introduced a
  # team_epr_contest column on paths that never asked for one.
  p <- plot_team_ratings(team_ratings = tr, metric = "epr", season = 2026)
  expect_false("team_epr_contest" %in% names(p$data))
})
