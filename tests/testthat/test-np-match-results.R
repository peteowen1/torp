# .np_match_results(): final scores per match for the v4 engine, from the
# official results when available, else the play-by-play's running score.

np_pbp_fixture <- function() {
  data.frame(
    match_id = rep(c("CD_M20260140101", "CD_M20260140102"), each = 2),
    display_order = c(1, 2, 1, 2),
    home_team_name = rep(c("Carlton", "Geelong Cats"), each = 2),
    away_team_name = rep(c("Richmond", "Sydney Swans"), each = 2),
    home_points = c(6, 80, 0, 71),
    away_points = c(0, 75, 7, 90),
    stringsAsFactors = FALSE
  )
}

test_that("an empty results frame falls back to the running score instead of crashing", {
  pbp <- np_pbp_fixture()
  for (empty in list(data.frame(), data.frame(match_id = character(), home_score = numeric(),
                                              away_score = numeric()))) {
    testthat::local_mocked_bindings(load_results = function(...) empty)
    expect_warning(res <- .np_match_results(pbp), "came back empty")
    res <- data.table::as.data.table(res)
    expect_equal(nrow(res), 2L)
    expect_equal(res[match_id == "CD_M20260140101", home_score], 80)
    expect_equal(res[match_id == "CD_M20260140102", away_score], 90)
  }
})

test_that("access_api() and get_token() set a request timeout", {
  # A request with no timeout hung the live chains job for 18 minutes.
  expect_true(is.numeric(AFL_API_TIMEOUT_S) && AFL_API_TIMEOUT_S > 0)
  for (fn in list(access_api, get_token)) {
    src <- paste(deparse(fn), collapse = "\n")
    expect_match(src, "httr::timeout(AFL_API_TIMEOUT_S)", fixed = TRUE)
  }
})
