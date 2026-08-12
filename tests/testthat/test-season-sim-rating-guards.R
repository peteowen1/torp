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
