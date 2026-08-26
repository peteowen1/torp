# docs/plans/AFLW-MIGRATION-PLAN.md Phase 3 -- AFLW PSR/PSV.
# calculate_psr()/calculate_psv() need zero changes (already comp-agnostic,
# see test-psr.R). These tests cover only the new comp-aware coefficient
# path resolution (.comp_coef_filename(), .find_psr_coef_path()) and the
# AFLW data-prep adapter (.prepare_aflw_stat_rating_data()).

test_that(".comp_coef_filename leaves AFLM filenames unchanged", {
  expect_equal(torp:::.comp_coef_filename("psr_coefficients.csv"), "psr_coefficients.csv")
  expect_equal(torp:::.comp_coef_filename("psr_coefficients.csv", comp = "AFLM"), "psr_coefficients.csv")
})

test_that(".comp_coef_filename suffixes non-AFLM filenames", {
  expect_equal(torp:::.comp_coef_filename("psr_coefficients.csv", comp = "AFLW"), "psr_coefficients_aflw.csv")
  expect_equal(torp:::.comp_coef_filename("osr_coefficients.csv", comp = "AFLW"), "osr_coefficients_aflw.csv")
  expect_equal(torp:::.comp_coef_filename("dsr_coefficients.csv", comp = "AFLW"), "dsr_coefficients_aflw.csv")
})

test_that(".find_psr_coef_path defaults to AFLM and resolves the men's filename unchanged", {
  # AFLM behaviour must be byte-identical to before this change -- resolves
  # via system.file()/data-raw fallback exactly as it always has.
  path_default <- torp:::.find_psr_coef_path()
  path_explicit_aflm <- torp:::.find_psr_coef_path(comp = "AFLM")
  expect_equal(path_default, path_explicit_aflm)
  if (nzchar(path_default)) {
    expect_true(grepl("psr_coefficients\\.csv$", path_default))
    expect_false(grepl("_aflw", path_default))
  }
})

test_that(".find_psr_coef_path resolves the AFLW-suffixed filename", {
  path_aflw <- torp:::.find_psr_coef_path(comp = "AFLW")
  # Either found (this session's aflw_run_pipeline.R wrote it to inst/extdata)
  # or "" if not present -- either way the filename it looked for must be
  # the AFLW one, not the AFLM one.
  if (nzchar(path_aflw)) {
    expect_true(grepl("psr_coefficients_aflw\\.csv$", path_aflw))
  } else {
    expect_equal(path_aflw, "")
  }
})

test_that(".find_psr_coef_path rejects an unknown comp", {
  expect_error(torp:::.find_psr_coef_path(comp = "BOGUS"), "Unknown comp")
})

test_that(".compute_psv and .compute_psr_from_stat_ratings default to comp='AFLM' and don't error on the extra arg", {
  # Regression guard for the Phase 1 mocked-binding failure mode: confirm the
  # new comp param doesn't break a call site that doesn't pass it.
  expect_true("comp" %in% names(formals(torp:::.compute_psv)))
  expect_equal(formals(torp:::.compute_psv)$comp, "AFLM")
  expect_true("comp" %in% names(formals(torp:::.compute_psr_from_stat_ratings)))
  expect_equal(formals(torp:::.compute_psr_from_stat_ratings)$comp, "AFLM")
})

test_that(".prepare_aflw_stat_rating_data shapes AFLW loaders into .prepare_stat_rating_data()'s expected input", {
  mock_ps <- tibble::tibble(
    player_id = c("P1", "P2"),
    match_id = c("CD_M2025264010", "CD_M2025264010"),
    player_name = c("A One", "B Two"),
    season = c(2025, 2025),
    round_number = c(1, 1),
    utc_start_time = c("2025-08-14T09:15:00.000+0000", "2025-08-14T09:15:00.000+0000"),
    home_team_name = c("Carlton", "Carlton"),
    away_team_name = c("Collingwood", "Collingwood"),
    team_status = c("home", "away"),
    time_on_ground_percentage = c(90, 80),
    position = c("MIDFIELDER", "DEFENDER"),
    goals = c(2, 0),
    kicks = c(10, 8)
  )
  mock_teams <- tibble::tibble(
    player_id = c("P1", "P2"),
    season = c(2025, 2025),
    round_number = c(1, 1),
    team_name = c("Carlton", "Collingwood"),
    lineup_position = c("C", "FB")
  )
  mock_fixtures <- tibble::tibble(
    match_id = "CD_M2025264010",
    season = 2025,
    round_number = 1,
    home_team_name = "Carlton",
    away_team_name = "Collingwood",
    utc_start_time = "2025-08-14T09:15:00.000+0000"
  )

  local_mocked_bindings(
    load_player_stats = function(seasons, comp = "AFLM") { expect_equal(comp, "AFLW"); mock_ps },
    load_teams = function(seasons, comp = "AFLM") { expect_equal(comp, "AFLW"); mock_teams },
    load_fixtures = function(seasons, comp = "AFLM") { expect_equal(comp, "AFLW"); mock_fixtures }
  )

  out <- torp:::.prepare_aflw_stat_rating_data(2025)
  expect_true(nrow(out) >= 2)
  expect_true(all(c("player_id", "match_id", "season", "round", "tog", "pos_group") %in% names(out)))
  # team/opponent derived from team_status + home/away names
  p1_row <- out[out$player_id == "P1" & out$avail_only == FALSE, ]
  expect_equal(p1_row$tog[1], 0.9)
})

test_that(".prepare_aflw_stat_rating_data aborts on empty AFLW player_stats rather than proceeding silently", {
  local_mocked_bindings(
    load_player_stats = function(seasons, comp = "AFLM") tibble::tibble()
  )
  expect_error(torp:::.prepare_aflw_stat_rating_data(2025), "No AFLW player_stats")
})

# --- aflw_psr-data release path (scoring + loader) ---------------------------
# These cover the SCORING half only. aflw_run_pipeline.R (training) is
# deliberately not on the pipeline cadence -- see .build_aflw_stat_ratings().

test_that(".validate_aflw_seasons accepts AFLW's pre-2021 history", {
  # The whole reason this helper exists: validate_seasons() floors at
  # AFL_MIN_SEASON (2021) and would abort on AFLW's first three seasons.
  expect_error(validate_seasons(2018), "Invalid season")
  expect_equal(torp:::.validate_aflw_seasons(2018), 2018)
  expect_equal(torp:::.validate_aflw_seasons(c(2018, 2019)), c(2018, 2019))
})

test_that(".validate_aflw_seasons(TRUE) spans from AFLW_MIN_SEASON", {
  got <- torp:::.validate_aflw_seasons(TRUE)
  expect_equal(min(got), AFLW_MIN_SEASON)
  expect_true(AFLW_MIN_SEASON < AFL_MIN_SEASON)
})

test_that(".validate_aflw_seasons rejects seasons before AFLW existed", {
  expect_error(torp:::.validate_aflw_seasons(2017), "Invalid AFLW season")
  expect_error(torp:::.validate_aflw_seasons("2018"), "must be numeric")
})

test_that("load_aflw_psr requests the aflw_psr-data release", {
  captured <- NULL
  testthat::local_mocked_bindings(
    generate_urls = function(data_type, file_prefix, seasons, ...) {
      captured <<- list(data_type = data_type, file_prefix = file_prefix, seasons = seasons)
      "http://example.invalid/x.parquet"
    },
    load_from_url = function(...) data.frame()
  )
  load_aflw_psr(2025)
  expect_equal(captured$data_type, "aflw_psr-data")
  expect_equal(captured$file_prefix, "aflw_psr")
  expect_equal(captured$seasons, 2025)
})

test_that("load_aflw_psr does not abort on AFLW's earliest seasons", {
  # Regression guard: routing this loader through validate_seasons() would
  # make 2018-2020 unreachable, which is precisely the history the release
  # exists to carry.
  testthat::local_mocked_bindings(
    generate_urls = function(...) "http://example.invalid/x.parquet",
    load_from_url = function(...) data.frame()
  )
  expect_no_error(load_aflw_psr(2018))
})

test_that(".compute_aflw_psr returns NULL when no stat ratings are available", {
  # Must degrade to NULL, not error: the pipeline stage treats NULL as
  # "skip the release", and an abort there would be caught by the tryCatch
  # but reported as a failure rather than an empty season.
  expect_null(torp:::.compute_aflw_psr(stat_ratings = data.frame()))
})

test_that(".build_aflw_stat_ratings returns NULL when no round was played", {
  # The played-rounds guard is what keeps phantom future rounds out of the
  # published release; with nothing played there is nothing to estimate.
  testthat::local_mocked_bindings(
    .prepare_aflw_stat_rating_data = function(seasons = TRUE) {
      data.table::data.table(player_id = "1", season = 2026L, round = 1L)
    },
    load_fixtures = function(...) {
      data.frame(
        season = 2026L, round_number = 1L,
        utc_start_time = "2099-01-01T00:00:00.000+0000",
        home_score = NA_real_, away_score = NA_real_
      )
    }
  )
  expect_warning(res <- torp:::.build_aflw_stat_ratings(TRUE), "no PLAYED rounds")
  expect_null(res)
})

test_that(".compute_psr_from_stat_ratings forwards comp to listed-position loading", {
  # Latent-but-real: .load_listed_positions() calls load_player_details(),
  # which defaults to the men's competition. Without the passthrough an AFLW
  # run would centre on men's listings and simply fail to join -- silently.
  seen_comp <- NULL
  testthat::local_mocked_bindings(
    .load_listed_positions = function(seasons, comp = "AFLM") {
      seen_comp <<- comp
      NULL
    },
    .find_psr_coef_path = function(...) ""
  )
  suppressWarnings(torp:::.compute_psr_from_stat_ratings(
    data.frame(season = 2025L), center = TRUE, centre_on_listed = TRUE, comp = "AFLW"
  ))
  expect_equal(seen_comp, "AFLW")
})

# --- comp-aware season floor (build/scoring side) ---------------------------

test_that(".validate_seasons_comp() floors AFLW at 2018 but leaves AFLM at 2021", {
  # validate_seasons() is NOT comp-aware: TRUE resolves to AFL_MIN_SEASON:current
  # (2021+), which silently drops AFLW's first three seasons, and an explicit
  # 2019 aborts outright. The build/scoring path must not inherit that floor.
  expect_equal(min(torp:::.validate_seasons_comp(TRUE, "AFLW")), torp:::AFLW_MIN_SEASON)
  expect_equal(min(torp:::.validate_seasons_comp(TRUE, "AFLM")), torp:::AFL_MIN_SEASON)
  # The specific case that aborted before: an explicit pre-2021 AFLW season.
  expect_equal(torp:::.validate_seasons_comp(2019, "AFLW"), 2019)
  expect_error(torp:::.validate_seasons_comp(2019, "AFLM"), "Invalid season years")
})

test_that("AFLM season validation is bit-identical through the comp dispatcher", {
  # Routing on comp must not perturb the men's path at all.
  expect_equal(torp:::.validate_seasons_comp(TRUE, "AFLM"), torp:::validate_seasons(TRUE))
  expect_equal(torp:::.validate_seasons_comp(2023:2025, "AFLM"), torp:::validate_seasons(2023:2025))
})

test_that("every comp-taking loader routes season validation through the comp dispatcher", {
  # Class-level guard, not an instance one. When the comp-aware floor first
  # landed it reached load_player_stats/load_fixtures/load_teams but MISSED
  # load_results() and load_player_details() -- both take `comp`, both kept the
  # non-comp-aware validate_seasons(), so load_results(2019, comp = "AFLW")
  # aborted with "Seasons must be between 2021 and 2026" even though the data
  # exists (verified live: load_fixtures(2019, comp="AFLW") returns 38 scored
  # matches). That gap blocked the first AFLW PSR publish, because the
  # published-artifact guard verifies against load_results() and could not see
  # 2018-2020 at all.
  #
  # This asserts the RULE rather than those two functions: if a loader accepts
  # `comp`, its season floor must depend on `comp`. A future comp-taking loader
  # that forgets fails here.
  loader_names <- c("load_player_stats", "load_fixtures", "load_teams",
                    "load_results", "load_player_details")
  for (nm in loader_names) {
    fn <- get(nm, envir = asNamespace("torp"))
    expect_true("comp" %in% names(formals(fn)),
                info = paste0(nm, " is expected to take a comp argument"))
    body_src <- paste(deparse(body(fn)), collapse = "\n")
    expect_true(
      grepl(".validate_seasons_comp", body_src, fixed = TRUE),
      info = paste0(nm, "() takes `comp` but does not route season validation ",
                    "through .validate_seasons_comp() -- pre-2021 AFLW seasons ",
                    "will abort or be silently dropped.")
    )
  }
})

test_that(".build_aflw_stat_ratings() asserts cross-feed coverage", {
  # The guard that must be present: rating data has a played round the
  # fixture-derived checkpoint map lacks (results feed lagging the stats feed).
  map <- data.table::data.table(season = 2026L, round = 1L, ref_date = as.Date("2026-08-01"))
  srd <- data.table::data.table(season = c(2026L, 2026L), round = c(1L, 3L))
  expect_error(torp:::.assert_ref_date_coverage(map, srd, label = "AFLW"), "2026 R3")
})
