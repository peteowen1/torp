# Disk-backed memoization for team_rapm_asof.R (team_rapm_asof_cache.R).
# Tests the CACHE MECHANISM in isolation -- mocks the expensive inner
# functions with call-counting stubs rather than re-running a real ridge fit,
# since the fit's own correctness is already covered by test-team-rapm-asof.R.
# A cache that returns a different answer than a fresh computation is a
# correctness bug, not a speed win -- the identical() checks below are the
# point, not the counters.

.mock_asof_results <- function() {
  tibble::tibble(
    match_id = paste0("m", 1:5), season = 2024L,
    home_team_id = "T1", away_team_id = "T2",
    home_score = c(60, 70, 55, 80, 65), away_score = c(50, 65, 60, 70, 72),
    utc_start_time = as.Date("2024-03-01") + (0:4) * 7L
  )
}

.with_temp_asof_cache <- function(code) {
  tmp <- tempfile("asof_cache_test_")
  dir.create(tmp)
  withr::local_envvar(TORP_ASOF_CACHE_DIR = tmp, TORP_ASOF_CACHE = "1")
  force(code)
}

# -----------------------------------------------------------------------------
# .team_rapm_asof_cache_key -- must vary with every argument that changes the
# result, and must NOT vary with argument order/formatting quirks.
# -----------------------------------------------------------------------------

test_that(".team_rapm_asof_cache_key differs when halflife_days differs", {
  local_mocked_bindings(load_results = function(seasons, comp = "AFLM") .mock_asof_results())
  k1 <- torp:::.team_rapm_asof_cache_key("rapm_asof", "AFLM", ref_date = "2024-05-01", halflife_days = 365)
  k2 <- torp:::.team_rapm_asof_cache_key("rapm_asof", "AFLM", ref_date = "2024-05-01", halflife_days = 1095)
  expect_false(identical(k1, k2))
})

test_that(".team_rapm_asof_cache_key is identical for identical arguments", {
  local_mocked_bindings(load_results = function(seasons, comp = "AFLM") .mock_asof_results())
  k1 <- torp:::.team_rapm_asof_cache_key("rapm_asof", "AFLM", ref_date = "2024-05-01", halflife_days = 365)
  k2 <- torp:::.team_rapm_asof_cache_key("rapm_asof", "AFLM", ref_date = "2024-05-01", halflife_days = 365)
  expect_identical(k1, k2)
})

test_that(".team_rapm_asof_cache_key differs between comps", {
  local_mocked_bindings(load_results = function(seasons, comp = "AFLM") .mock_asof_results())
  k1 <- torp:::.team_rapm_asof_cache_key("rapm_asof", "AFLM", ref_date = "2024-05-01", halflife_days = 365)
  k2 <- torp:::.team_rapm_asof_cache_key("rapm_asof", "AFLW", ref_date = "2024-05-01", halflife_days = 365)
  expect_false(identical(k1, k2))
})

# -----------------------------------------------------------------------------
# fit_team_rapm_asof_cached -- cache hit skips the underlying computation
# entirely, and returns a result identical to a fresh computation.
# -----------------------------------------------------------------------------

test_that("fit_team_rapm_asof_cached: a second identical call does not recompute, and returns the cached result unchanged", {
  .with_temp_asof_cache({
    n_build_calls <- 0L
    n_fit_calls <- 0L
    n_extract_calls <- 0L
    fake_design <- list(n_train_matches = 42L, p = 7L)
    fake_fit <- list(cv_r2 = 0.31)
    fake_ratings <- data.table::data.table(
      player_id = c("P1", "P2"), rating_type = c("individual", "individual"),
      rapm_offense = c(1.1, -0.4), rapm_defense = c(0.2, 0.9)
    )

    local_mocked_bindings(
      .team_rapm_match_dates = function(seasons, comp = "AFLM") {
        data.table::data.table(match_id = "m1", match_date = as.Date("2024-03-01"))
      },
      build_team_rapm_asof = function(...) { n_build_calls <<- n_build_calls + 1L; fake_design },
      fit_team_rapm_asof = function(...) { n_fit_calls <<- n_fit_calls + 1L; fake_fit },
      extract_team_rapm_ratings = function(...) { n_extract_calls <<- n_extract_calls + 1L; data.table::copy(fake_ratings) },
      .package = "torp"
    )

    r1 <- fit_team_rapm_asof_cached("2024-05-01", comp = "AFLM", halflife_days = 365)
    expect_equal(n_build_calls, 1L)
    expect_equal(n_fit_calls, 1L)
    expect_equal(n_extract_calls, 1L)

    r2 <- fit_team_rapm_asof_cached("2024-05-01", comp = "AFLM", halflife_days = 365)
    # Cache hit -- none of the expensive inner functions should run again.
    expect_equal(n_build_calls, 1L)
    expect_equal(n_fit_calls, 1L)
    expect_equal(n_extract_calls, 1L)

    expect_identical(as.data.frame(r1), as.data.frame(r2))
    expect_equal(r2$player_id, c("P1", "P2"))
  })
})

test_that("fit_team_rapm_asof_cached: a different halflife_days recomputes (cache miss, not a wrong hit)", {
  .with_temp_asof_cache({
    n_build_calls <- 0L
    fake_ratings <- data.table::data.table(
      player_id = "P1", rating_type = "individual", rapm_offense = 1.0, rapm_defense = 0.0
    )
    local_mocked_bindings(
      .team_rapm_match_dates = function(seasons, comp = "AFLM") {
        data.table::data.table(match_id = "m1", match_date = as.Date("2024-03-01"))
      },
      build_team_rapm_asof = function(...) { n_build_calls <<- n_build_calls + 1L; list(n_train_matches = 1L, p = 1L) },
      fit_team_rapm_asof = function(...) list(cv_r2 = 0.1),
      extract_team_rapm_ratings = function(...) data.table::copy(fake_ratings),
      .package = "torp"
    )
    fit_team_rapm_asof_cached("2024-05-01", comp = "AFLM", halflife_days = 365)
    fit_team_rapm_asof_cached("2024-05-01", comp = "AFLM", halflife_days = 1095)  # different key
    expect_equal(n_build_calls, 2L)
  })
})

test_that("fit_team_rapm_asof_cached: TORP_ASOF_CACHE=0 forces recomputation even on an identical key", {
  .with_temp_asof_cache({
    n_build_calls <- 0L
    fake_ratings <- data.table::data.table(
      player_id = "P1", rating_type = "individual", rapm_offense = 1.0, rapm_defense = 0.0
    )
    local_mocked_bindings(
      .team_rapm_match_dates = function(seasons, comp = "AFLM") {
        data.table::data.table(match_id = "m1", match_date = as.Date("2024-03-01"))
      },
      build_team_rapm_asof = function(...) { n_build_calls <<- n_build_calls + 1L; list(n_train_matches = 1L, p = 1L) },
      fit_team_rapm_asof = function(...) list(cv_r2 = 0.1),
      extract_team_rapm_ratings = function(...) data.table::copy(fake_ratings),
      .package = "torp"
    )
    fit_team_rapm_asof_cached("2024-05-01", comp = "AFLM", halflife_days = 365)
    withr::with_envvar(c(TORP_ASOF_CACHE = "0"), {
      fit_team_rapm_asof_cached("2024-05-01", comp = "AFLM", halflife_days = 365)
    })
    expect_equal(n_build_calls, 2L)
  })
})

test_that("fit_team_rapm_asof_cached: NULL from build_team_rapm_asof (too few matches) is passed through, not cached", {
  .with_temp_asof_cache({
    n_build_calls <- 0L
    local_mocked_bindings(
      .team_rapm_match_dates = function(seasons, comp = "AFLM") {
        data.table::data.table(match_id = "m1", match_date = as.Date("2024-03-01"))
      },
      build_team_rapm_asof = function(...) { n_build_calls <<- n_build_calls + 1L; NULL },
      .package = "torp"
    )
    expect_null(fit_team_rapm_asof_cached("2024-05-01", comp = "AFLM", halflife_days = 365))
    expect_null(fit_team_rapm_asof_cached("2024-05-01", comp = "AFLM", halflife_days = 365))
    # A NULL (skip) result must not be cached -- otherwise a later data
    # refresh that would legitimately produce enough matches stays stuck
    # returning NULL forever.
    expect_equal(n_build_calls, 2L)
  })
})

# -----------------------------------------------------------------------------
# fit_team_spm_asof_cached -- keyed partly on a content hash of the RAPM
# ratings it's given, not just ref_date/comp.
# -----------------------------------------------------------------------------

test_that("fit_team_spm_asof_cached: a second identical call does not recompute", {
  .with_temp_asof_cache({
    n_calls <- 0L
    fake_out <- data.table::data.table(player_id = "P1", spm_offense = 0.5)
    local_mocked_bindings(
      .team_rapm_match_dates = function(seasons, comp = "AFLM") {
        data.table::data.table(match_id = "m1", match_date = as.Date("2024-03-01"))
      },
      fit_team_spm_asof = function(...) { n_calls <<- n_calls + 1L; data.table::copy(fake_out) },
      .package = "torp"
    )
    ratings <- data.table::data.table(player_id = "P1", rapm_offense = 1.0, rapm_defense = 0.2)
    r1 <- fit_team_spm_asof_cached("2024-05-01", ratings, comp = "AFLM")
    r2 <- fit_team_spm_asof_cached("2024-05-01", ratings, comp = "AFLM")
    expect_equal(n_calls, 1L)
    expect_identical(as.data.frame(r1), as.data.frame(r2))
  })
})

test_that("fit_team_spm_asof_cached: different input ratings produce a cache miss, not a stale hit", {
  .with_temp_asof_cache({
    n_calls <- 0L
    local_mocked_bindings(
      .team_rapm_match_dates = function(seasons, comp = "AFLM") {
        data.table::data.table(match_id = "m1", match_date = as.Date("2024-03-01"))
      },
      fit_team_spm_asof = function(...) { n_calls <<- n_calls + 1L; data.table::data.table(player_id = "P1", spm_offense = 0.5) },
      .package = "torp"
    )
    ratings_a <- data.table::data.table(player_id = "P1", rapm_offense = 1.0, rapm_defense = 0.2)
    ratings_b <- data.table::data.table(player_id = "P1", rapm_offense = 1.5, rapm_defense = 0.2)  # different content
    fit_team_spm_asof_cached("2024-05-01", ratings_a, comp = "AFLM")
    fit_team_spm_asof_cached("2024-05-01", ratings_b, comp = "AFLM")
    expect_equal(n_calls, 2L)
  })
})
