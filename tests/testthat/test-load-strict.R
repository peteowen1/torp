# torp H2 / ECOSYSTEM-FIX-PLAN.md T7: .load_with_cache() must never collapse
# a per-season fetch failure into a silently-cached partial result, and must
# abort outright in strict mode. No network -- fetch_fn is a local closure.

test_that(".load_with_cache aborts when strict=TRUE and a season fails", {
  clear_data_cache()
  withr::defer(clear_data_cache())

  fetch_fn <- function(s) {
    if (s == 2022) stop("simulated fetch failure")
    data.frame(season = s, x = 1:3)
  }

  expect_error(
    torp:::.load_with_cache(
      "strict_test_prefix", seasons = c(2021, 2022), fetch_fn = fetch_fn,
      strict = TRUE
    ),
    class = "vb_error_transient"
  )

  info <- get_cache_info()
  expect_false(any(grepl("strict_test_prefix", info$cache_key)))
})

test_that(".load_with_cache returns partial data with a warning when strict=FALSE, but still never caches it", {
  clear_data_cache()
  withr::defer(clear_data_cache())

  fetch_fn <- function(s) {
    if (s == 2022) stop("simulated fetch failure")
    data.frame(season = s, x = 1:3)
  }

  # Two warnings fire here (per-season failure + partial-data summary) --
  # assert at least one matches, then capture the return value separately
  # rather than relying on expect_warning()'s single-warning capture.
  expect_warning(
    torp:::.load_with_cache(
      "lenient_test_prefix", seasons = c(2021, 2022), fetch_fn = fetch_fn,
      strict = FALSE
    ),
    "simulated fetch failure"
  )

  out <- suppressWarnings(
    torp:::.load_with_cache(
      "lenient_test_prefix", seasons = c(2021, 2022), fetch_fn = fetch_fn,
      strict = FALSE
    )
  )

  expect_equal(nrow(out), 3)  # only the 2021 season came back
  info <- get_cache_info()
  expect_false(any(grepl("lenient_test_prefix", info$cache_key)))
})

test_that(".load_with_cache caches a fully successful multi-season load", {
  clear_data_cache()
  withr::defer(clear_data_cache())

  fetch_fn <- function(s) data.frame(season = s, x = 1:2)

  out <- torp:::.load_with_cache(
    "ok_cache_test_prefix", seasons = c(2021, 2022), fetch_fn = fetch_fn,
    strict = TRUE
  )

  expect_equal(nrow(out), 4)
  info <- get_cache_info()
  expect_true(any(grepl("ok_cache_test_prefix", info$cache_key)))
})

test_that(".load_with_cache strict defaults to VERSEBUS_STRICT env var", {
  withr::local_envvar(c(VERSEBUS_STRICT = "1"))
  clear_data_cache()
  withr::defer(clear_data_cache())

  fetch_fn <- function(s) stop("simulated fetch failure")

  expect_error(
    torp:::.load_with_cache("envtest_prefix", seasons = 2021, fetch_fn = fetch_fn),
    class = "vb_error_transient"
  )
})
