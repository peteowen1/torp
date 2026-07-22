# Tests for matchup_table.R (torp#108 -- directed matchup table)
# ==================================================================
# Deterministic small-input tests for the frozen-state extraction and
# newdata fabrication (.extract_frozen_teams() / .build_matchup_newdata()),
# plus shape/schema checks. A full end-to-end build_matchup_table() run
# needs a real GAM+XGBoost train (network + several minutes) and is
# exercised manually against 2026 data (torp#108), not here.

# -----------------------------------------------------------------------------
# Mock fixtures: 3 teams, 2 real grounds + the MCG
# -----------------------------------------------------------------------------
# Team A: 3 historical games at "Ground A" -> home_venue = Ground A,
#   familiarity(Ground A) = 1.0
# Team B: 2 at "Ground B", 1 at "M.C.G." -> home_venue = Ground B,
#   familiarity(Ground B) = 2/3, familiarity(M.C.G.) = 1/3
# Team C: 3 at "M.C.G." -> home_venue = M.C.G. (edge case: a team whose
#   detected "home ground" IS the MCG), familiarity(M.C.G.) = 1.0

.mt_make_team_rt_fix_df <- function() {
  history <- data.frame(
    team_id = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
    team_name = c(rep("Team A", 3), rep("Team B", 3), rep("Team C", 3)),
    season = 2026L,
    round_number = rep(1:3, 3),
    venue = c(
      "Ground A", "Ground A", "Ground A",
      "Ground B", "Ground B", "M.C.G.",
      "M.C.G.", "M.C.G.", "M.C.G."
    ),
    epr = 10, epr_recv = 2.5, epr_disp = 2.5, epr_spoil = 2.5, epr_hitout = 2.5,
    psr = 5, count = 22L,
    stringsAsFactors = FALSE
  )
  target <- data.frame(
    team_id = c(1, 2, 3),
    team_name = c("Team A", "Team B", "Team C"),
    season = 2026L, round_number = 4L,
    venue = c("Ground A", "Ground B", "M.C.G."),
    epr = c(12, 8, 6), epr_recv = c(3, 2, 1.5), epr_disp = c(3, 2, 1.5),
    epr_spoil = c(3, 2, 1.5), epr_hitout = c(3, 2, 1.5),
    psr = c(6, 4, 2), count = 22L,
    stringsAsFactors = FALSE
  )
  rbind(history, target)
}

.mt_make_all_grounds <- function() {
  data.frame(
    venue = c("Ground A", "Ground B", "M.C.G."),
    Latitude = c(0, 10, 5),
    Longitude = c(0, 10, 5),
    stringsAsFactors = FALSE
  )
}

.mt_make_team_mdl_df <- function() {
  # Minimal frame satisfying .matches_from_team_mdl_df() (Elo) plus the
  # factor-level universe .build_matchup_newdata() validates against.
  # game_wday_fac.x = "6": .gf_anchor_date() always lands on a Saturday
  # (wday(week_start=1) == 6), for ANY season -- not season-dependent here.
  data.frame(
    match_id = c("M1", "M2"),
    team_type = c("home", "home"),
    team_type_fac = factor(c("home", "home"), levels = c("away", "home")),
    utc_start_time = c("2026-03-01T00:00:00.000+0000", "2026-03-08T00:00:00.000+0000"),
    season.x = 2026L, round_number.x = c(1L, 2L),
    team_name.x = c("Team A", "Team B"),
    team_name.y = c("Team B", "Team C"),
    score_diff = c(10, -5),
    win = c(1, 0),
    venue_fac = factor(c("Ground A", "Ground B"), levels = c("Ground A", "Ground B", "M.C.G.")),
    days_rest_diff_fac = factor(c("0", "0"), levels = as.character(-4:4)),
    game_wday_fac.x = factor(c("6", "6"), levels = as.character(1:7)),
    temp_avg = c(18, 20), wind_avg = c(10, 12), humidity_avg = c(50, 55),
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# .extract_frozen_teams()
# -----------------------------------------------------------------------------

test_that(".extract_frozen_teams reads the target-week snapshot + home ground + familiarity", {
  state <- list(team_rt_fix_df = .mt_make_team_rt_fix_df(), season = 2026L, week = 4L)
  frozen <- torp:::.extract_frozen_teams(state)

  expect_equal(nrow(frozen$snapshot), 3)
  expect_setequal(frozen$snapshot$team_name, c("Team A", "Team B", "Team C"))
  expect_equal(frozen$snapshot$epr[frozen$snapshot$team_name == "Team A"], 12)
  expect_equal(frozen$snapshot$epr[frozen$snapshot$team_name == "Team B"], 8)
  expect_equal(frozen$snapshot$epr[frozen$snapshot$team_name == "Team C"], 6)

  hv_by_id <- stats::setNames(frozen$home_venue$venue, frozen$home_venue$team_id)
  expect_equal(unname(hv_by_id["1"]), "Ground A")
  expect_equal(unname(hv_by_id["2"]), "Ground B")
  expect_equal(unname(hv_by_id["3"]), "M.C.G.")

  fam <- frozen$familiarity_now
  expect_equal(fam$familiarity[fam$team_id == 1 & fam$venue == "Ground A"], 1)
  expect_equal(fam$familiarity[fam$team_id == 2 & fam$venue == "Ground B"], 2 / 3)
  expect_equal(fam$familiarity[fam$team_id == 2 & fam$venue == "M.C.G."], 1 / 3)
  expect_equal(fam$familiarity[fam$team_id == 3 & fam$venue == "M.C.G."], 1)
})

test_that(".extract_frozen_teams aborts loudly on unresolved NA ratings", {
  bad <- .mt_make_team_rt_fix_df()
  bad$epr[bad$round_number == 4 & bad$team_name == "Team B"] <- NA_real_
  state <- list(team_rt_fix_df = bad, season = 2026L, week = 4L)
  expect_error(torp:::.extract_frozen_teams(state), "NA epr")
})

# -----------------------------------------------------------------------------
# .build_matchup_newdata()
# -----------------------------------------------------------------------------

test_that(".build_matchup_newdata fabricates n*(n-1)*2*2 rows with no NAs in core features", {
  state <- list(
    team_rt_fix_df = .mt_make_team_rt_fix_df(),
    team_mdl_df = .mt_make_team_mdl_df(),
    all_grounds = .mt_make_all_grounds(),
    season = 2026L, week = 4L
  )
  frozen <- torp:::.extract_frozen_teams(state)
  nd <- torp:::.build_matchup_newdata(state, frozen)

  # 3 teams -> 6 ordered pairs * 2 venue tiers * 2 perspectives = 24 rows
  expect_equal(nrow(nd), 3 * 2 * 2 * 2)
  expect_setequal(unique(nd$tier), c("home", "mcg"))

  core_cols <- c("epr_diff", "log_dist_diff", "familiarity_diff", "elo_diff",
                 "torp_diff", "psr_diff")
  for (col in core_cols) {
    expect_true(all(!is.na(nd[[col]])), info = col)
  }
})

test_that(".build_matchup_newdata recomputes log_dist/familiarity per venue tier (not pooled)", {
  state <- list(
    team_rt_fix_df = .mt_make_team_rt_fix_df(),
    team_mdl_df = .mt_make_team_mdl_df(),
    all_grounds = .mt_make_all_grounds(),
    season = 2026L, week = 4L
  )
  frozen <- torp:::.extract_frozen_teams(state)
  nd <- torp:::.build_matchup_newdata(state, frozen)

  # Team A hosting (any opponent) at its own ground: zero travel, full
  # familiarity -- the host-perspective row of every "home" tier matchup.
  a_host_home <- nd[nd$tier == "home" & nd$team_type_fac == "home" & nd$team_name.x == "Team A", ]
  expect_equal(nrow(a_host_home), 2)
  expect_true(all(abs(a_host_home$log_dist.x - log(torp:::MATCH_LOG_DIST_OFFSET)) < 1e-9))
  expect_true(all(a_host_home$familiarity.x == 1))

  # Team C's real home ground IS the MCG (edge case by construction): hosting
  # at the "mcg" tier gives it the SAME zero-travel/full-familiarity result
  # as Team A gets at its OWN "home" tier -- this is the venue effect working
  # as intended, not a bug (torp#108's documented MCG-tier decision).
  c_host_mcg <- nd[nd$tier == "mcg" & nd$team_type_fac == "home" & nd$team_name.x == "Team C", ]
  expect_equal(nrow(c_host_mcg), 2)
  expect_true(all(abs(c_host_mcg$log_dist.x - log(torp:::MATCH_LOG_DIST_OFFSET)) < 1e-9))
  expect_true(all(c_host_mcg$familiarity.x == 1))

  # Team A hosting at the MCG (a venue it has NEVER visited): nonzero travel,
  # zero familiarity.
  a_host_mcg <- nd[nd$tier == "mcg" & nd$team_type_fac == "home" & nd$team_name.x == "Team A", ]
  expect_true(all(a_host_mcg$log_dist.x > log(torp:::MATCH_LOG_DIST_OFFSET)))
  expect_true(all(a_host_mcg$familiarity.x == 0))
})

test_that(".build_matchup_newdata aborts loudly when a fabricated venue has no trained-model level", {
  state <- list(
    team_rt_fix_df = .mt_make_team_rt_fix_df(),
    team_mdl_df = .mt_make_team_mdl_df(),
    all_grounds = .mt_make_all_grounds(),
    season = 2026L, week = 4L
  )
  frozen <- torp:::.extract_frozen_teams(state)
  # Shrink venue_fac's levels so "M.C.G." (a real fabricated tier) is missing
  state$team_mdl_df$venue_fac <- factor(as.character(state$team_mdl_df$venue_fac),
                                         levels = c("Ground A", "Ground B"))
  expect_error(torp:::.build_matchup_newdata(state, frozen), "venue_fac")
})

# -----------------------------------------------------------------------------
# .gf_anchor_date()
# -----------------------------------------------------------------------------

test_that(".gf_anchor_date returns the last Saturday of September", {
  d <- torp:::.gf_anchor_date(2026)
  expect_equal(format(d, "%Y-%m"), "2026-09")
  expect_equal(lubridate::wday(d, week_start = 1), 6)  # Saturday
  expect_true(as.integer(format(d, "%d")) >= 24)  # last Saturday, so late in Sept

  # Deterministic across a few seasons (leap years, differing Sept-30 weekdays)
  for (yr in c(2024, 2025, 2027, 2028)) {
    dy <- torp:::.gf_anchor_date(yr)
    expect_equal(lubridate::wday(dy, week_start = 1), 6)
    expect_equal(format(dy, "%m"), "09")
  }
})

# -----------------------------------------------------------------------------
# Shape / schema
# -----------------------------------------------------------------------------

test_that("build_matchup_table is exported with the expected signature", {
  expect_true(exists("build_matchup_table"))
  expect_true("build_matchup_table" %in% getNamespaceExports("torp"))
  fn_args <- names(formals(build_matchup_table))
  expect_true(all(c("season", "week", "days_rest", "state") %in% fn_args))
})

test_that("MATCHUP_TABLE_DAYS_REST constant exists and is a sensible positive number", {
  expect_true(is.numeric(torp:::MATCHUP_TABLE_DAYS_REST))
  expect_gt(torp:::MATCHUP_TABLE_DAYS_REST, 0)
})
