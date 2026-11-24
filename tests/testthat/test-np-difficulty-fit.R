# torp#258: a one-season frame's difficulty models are fitted on the season
# before, inside .np_engine_frame(), so no builder can forget it. These tests
# cover the routing only, with the engine mocked out. Whether the prior-season
# fit prices real matches correctly is measured on real data by
# data-raw/04-analysis/measure_np_difficulty_fit_258.R.

routing_case <- function(match_ids, np_fit = NULL, difficulty_terms = NULL,
                         return_fit = FALSE) {
  seen <- new.env()
  seen$for_season <- NULL
  seen$terms <- "unset"
  local_mocked_bindings(
    np_difficulty_terms_for_season = function(season, pbp_data, chains,
                                              return_models = FALSE, ...) {
      seen$for_season <- list(season = season, return_models = return_models)
      structure(data.table::data.table(sentinel = TRUE), np_models = "prior-fit")
    },
    .np_match_results = function(pbp_data, results) data.table::data.table(),
    build_net_points = function(..., difficulty_terms = NULL, leak_safe = NULL) {
      seen$terms <- difficulty_terms
      seen$leak_safe <- leak_safe
      data.table::data.table(x = 1)
    },
    .np_team_margin = function(np, ...) np,
    .env = asNamespace("torp")
  )
  .np_engine_frame(data.table::data.table(match_id = match_ids), NULL, NULL,
                   difficulty_terms = difficulty_terms, np_fit = np_fit,
                   return_fit = return_fit)
  as.list(seen)
}

test_that("a one-season frame is priced on the prior-season fit", {
  s <- routing_case(c("CD_M20260140101", "CD_M20260140102"), return_fit = TRUE)
  expect_identical(s$for_season$season, 2026L)
  expect_true(s$for_season$return_models)
  expect_true(s$terms$sentinel)
  # The fitted models ride on the terms, so the season fit bundle gets them.
  expect_identical(attr(s$terms, "np_models"), "prior-fit")
  expect_false(s$leak_safe)
})

test_that("a supplied season fit or precomputed terms skip the prior-season fit", {
  s <- routing_case("CD_M20260140101", np_fit = list(stoppage_baseline = NULL))
  expect_null(s$for_season)
  expect_null(s$terms)
  terms <- data.table::data.table(given = TRUE)
  s <- routing_case("CD_M20260140101", difficulty_terms = terms)
  expect_null(s$for_season)
  expect_identical(s$terms, terms)
})

test_that("a multi-season frame keeps the rolling within-data fit", {
  s <- routing_case(c("CD_M20250140101", "CD_M20260140101"))
  expect_null(s$for_season)
  expect_null(s$terms)
  expect_true(s$leak_safe)
})

test_that("NP_DIFFICULTY_FIT is registered with the vintage guard", {
  expect_identical(.rating_defining_constants()$NP_DIFFICULTY_FIT, NP_DIFFICULTY_FIT)
  expect_identical(NP_DIFFICULTY_FIT, "prior_season")
})
