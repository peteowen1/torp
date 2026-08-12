# prepare_sim_data()'s rating-load path had three ways to answer wrongly
# without saying so. All three are about the SAME thing: the function has two
# routes to a team rating, and only one of them was disciplined.
#
#   1. It filtered player ratings to max(season) in the file rather than the
#      season asked for. torp_ratings.parquet is full-history and upserted, and
#      load_torp_ratings() takes no season argument, so every historical
#      backtest silently got the CURRENT season's ratings. The team-ratings
#      route had always filtered correctly -- one function, two answers.
#   2. load_torp_ratings() returns a ZERO-ROW frame plus a warning on a failed
#      download, not an error, so the tryCatch around it never fired and
#      is.null() never caught it.
#   3. Nothing checked that sim_teams came out non-empty, so an empty rating
#      set propagated into simulate_season() instead of failing.

.mock_fixtures <- function(season = 2024) {
  data.frame(
    season = season, round_number = 1L, match_id = c("M1", "M2"),
    home_team_name = c("Geelong Cats", "Carlton"),
    away_team_name = c("Carlton", "Geelong Cats"),
    home_score = NA_integer_, away_score = NA_integer_,
    venue_name = "MCG",
    stringsAsFactors = FALSE
  )
}

# Full-history ratings. The two seasons differ in SIGN, not just magnitude:
# the aggregation multiplies by a TOG weight (~18) and a discount, so any
# threshold on the magnitude would be an arithmetic guess. Sign survives both.
.mock_full_history_ratings <- function() {
  data.frame(
    season = c(2024L, 2024L, 2026L, 2026L),
    round  = 1L,
    team   = c("Geelong Cats", "Carlton", "Geelong Cats", "Carlton"),
    torp   = c(5, 5, -100, -100),
    pred_tog = 80,
    stringsAsFactors = FALSE
  )
}

test_that("the team-ratings branch prefers team_torp over team_epr", {
  # TORP is the composed metric (0.5*EPR + 0.5*PSR) and beats both components
  # at predicting the next round's margin on every scale-free measure. When a
  # release carries both, team_torp must win.
  local_mocked_bindings(
    load_team_ratings = function(...) data.frame(
      season = 2024L, round = 1L,
      team = c("Geelong Cats", "Carlton"),
      team_torp = c(12, -4),
      team_epr  = c(99, 99),   # sentinel: picking this is a visible failure
      stringsAsFactors = FALSE
    ),
    load_torp_ratings = function(...) .mock_full_history_ratings(),
    load_predictions  = function(...) NULL,
    get_all_injuries  = function(...) NULL
  )

  prep <- prepare_sim_data(season = 2024, fixtures = .mock_fixtures(2024))
  # Verbatim -- no TOG re-weighting, no discount applied on this path.
  expect_equal(sort(prep$sim_teams$torp), c(-4, 12))
})

test_that("a pre-team_torp release falls back to team_epr and says to re-run the pipeline", {
  # This branch was dead from the metric-first rename (28a42a82) until
  # 2026-08-12: the lookup asked only for team_torp/torp, load_team_ratings()
  # does not rename columns, and as.numeric(NULL) yields a 0-row table rather
  # than erroring -- so a successful load was indistinguishable from no data.
  local_mocked_bindings(
    load_team_ratings = function(...) data.frame(
      season = 2024L, round = 1L,
      team = c("Geelong Cats", "Carlton"),
      team_epr = c(12, -4),
      stringsAsFactors = FALSE
    ),
    load_torp_ratings = function(...) .mock_full_history_ratings(),
    load_predictions  = function(...) NULL,
    get_all_injuries  = function(...) NULL
  )

  expect_message(
    prep <- prepare_sim_data(season = 2024, fixtures = .mock_fixtures(2024)),
    "no.*team_torp"
  )
  expect_equal(sort(prep$sim_teams$torp), c(-4, 12))
})

test_that("a team-ratings frame with no usable rating column says so and falls back", {
  local_mocked_bindings(
    load_team_ratings = function(...) data.frame(
      season = 2024L, round = 1L, team = c("Geelong Cats", "Carlton"),
      some_other_metric = c(1, 2), stringsAsFactors = FALSE
    ),
    load_torp_ratings = function(...) .mock_full_history_ratings(),
    load_predictions  = function(...) NULL,
    get_all_injuries  = function(...) NULL
  )

  expect_message(
    prepare_sim_data(season = 2024, fixtures = .mock_fixtures(2024)),
    "no usable rating"
  )
})

test_that("prepare_sim_data() uses the season asked for, not the newest in the file", {
  local_mocked_bindings(
    load_team_ratings   = function(...) stop("no team ratings release"),
    load_torp_ratings   = function(...) .mock_full_history_ratings(),
    load_predictions    = function(...) NULL,
    get_all_injuries    = function(...) NULL
  )

  prep <- prepare_sim_data(season = 2024, fixtures = .mock_fixtures(2024))

  # 2024's ratings are positive, 2026's negative. Reading max(season) -- the
  # old behaviour -- builds every team torp from the 2026 rows and flips the
  # sign. Assert on the value, not merely that the call ran.
  expect_equal(nrow(prep$sim_teams), 2L)
  expect_true(all(prep$sim_teams$torp > 0))
})

test_that("prepare_sim_data() aborts when the requested season is absent, rather than substituting another", {
  local_mocked_bindings(
    load_team_ratings   = function(...) stop("no team ratings release"),
    load_torp_ratings   = function(...) .mock_full_history_ratings(),
    load_predictions    = function(...) NULL,
    get_all_injuries    = function(...) NULL
  )

  # 2019 is not in the mocked release. Silently handing back 2026's ratings is
  # the defect; refusing is the fix.
  expect_error(
    prepare_sim_data(season = 2019, fixtures = .mock_fixtures(2019)),
    "No player ratings for 2019"
  )
})

test_that("injuries = FALSE is accepted rather than crashing on if (NA)", {
  # nrow() returns NULL for anything that is not a data.frame or matrix, so
  # `TRUE && logical(0)` made use_injury_aware NA and killed the function on
  # `if (NA)` -- with a message naming neither the argument nor injuries. FALSE
  # is the documented way to say "no injury adjustment"; only
  # simulate_afl_season() normalising it to NULL kept this off the front door.
  local_mocked_bindings(
    load_team_ratings = function(...) data.frame(
      season = 2024L, round = 1L, team = c("Geelong Cats", "Carlton"),
      team_torp = c(12, -4), stringsAsFactors = FALSE
    ),
    load_torp_ratings = function(...) .mock_full_history_ratings(),
    load_predictions  = function(...) NULL,
    get_all_injuries  = function(...) NULL
  )

  expect_no_error(
    prep <- prepare_sim_data(season = 2024, fixtures = .mock_fixtures(2024),
                             injuries = FALSE)
  )
  expect_equal(sort(prep$sim_teams$torp), c(-4, 12))
})

test_that("a season with no ratings yet falls back to the prior season loudly, not fatally", {
  # The pre-season window, and the reason the abort above must NOT be
  # unconditional. get_afl_season() is year(Sys.Date()), and
  # run_ratings_pipeline.R skips seasons with no PBP, so from January until
  # round 1 the CURRENT season has zero ratings rows -- for ~2.5 months every
  # year. Aborting there would kill the daily run and publish nothing, which
  # is worse than carrying last season's ratings forward with a warning.
  local_mocked_bindings(
    load_team_ratings = function(...) stop("no team ratings release"),
    load_torp_ratings = function(...) .mock_full_history_ratings(),
    load_predictions  = function(...) NULL,
    get_all_injuries  = function(...) NULL
  )

  # 2027 is not in the release; 2026 is the most recent prior season.
  expect_message(
    prep <- prepare_sim_data(season = 2027, fixtures = .mock_fixtures(2027)),
    "falling back to 2026"
  )
  # And it really used 2026 (negative torp in the mock), not 2024.
  expect_true(all(prep$sim_teams$torp < 0))
})

test_that("a zero-row ratings return is caught, not treated as a successful load", {
  # load_torp_ratings() warns and returns 0 rows on a failed download. The old
  # is.null() check passed it straight through to max() on an empty column.
  local_mocked_bindings(
    load_team_ratings = function(...) stop("no team ratings release"),
    load_torp_ratings = function(...) .mock_full_history_ratings()[0, ],
    load_predictions  = function(...) NULL,
    get_all_injuries  = function(...) NULL
  )

  expect_error(
    prepare_sim_data(season = 2024, fixtures = .mock_fixtures(2024)),
    "Could not load team or player ratings"
  )
})

test_that("a failed team-ratings load says the fallback is a different estimator", {
  # The fallback sums ~18 players' TORP and discounts it; the primary path
  # reads a team-level rating. Switching between them on a transient network
  # failure is fine -- doing it silently is not.
  local_mocked_bindings(
    load_team_ratings = function(...) stop("simulated transient failure"),
    load_torp_ratings = function(...) .mock_full_history_ratings(),
    load_predictions  = function(...) NULL,
    get_all_injuries  = function(...) NULL
  )

  expect_message(
    prepare_sim_data(season = 2024, fixtures = .mock_fixtures(2024)),
    "DIFFERENT estimator"
  )
})
