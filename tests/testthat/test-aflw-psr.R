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
