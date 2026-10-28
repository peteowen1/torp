# load_fixtures() accepts next season by name: the AFL publishes it in the
# off-season (the 2027 opening round was out in October 2026).

this_year <- as.integer(format(Sys.Date(), "%Y"))

test_that(".validate_fixture_seasons() accepts next year when named", {
  expect_equal(.validate_fixture_seasons(this_year + 1L), this_year + 1L)
  expect_equal(.validate_fixture_seasons(c(this_year, this_year + 1L)), c(this_year, this_year + 1L))
})

test_that(".validate_fixture_seasons() keeps TRUE at the current year", {
  # Pipelines read all fixtures; TRUE must not start including an unplayed season.
  out <- .validate_fixture_seasons(TRUE)
  expect_equal(max(out), this_year)
})

test_that(".validate_fixture_seasons() still rejects two years ahead and before the floor", {
  expect_error(.validate_fixture_seasons(this_year + 2L), "Invalid season")
  expect_error(.validate_fixture_seasons(c(2019, this_year + 1L)), "Invalid season")
})

test_that(".validate_fixture_seasons() keeps AFLW's 2018 floor", {
  expect_equal(.validate_fixture_seasons(c(2018, this_year + 1L), comp = "AFLW"), c(2018, this_year + 1L))
})
