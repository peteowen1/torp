# docs/plans/AFLW-MIGRATION-PLAN.md Phase 3 -- AFLW team Elo.
# build_team_elo() itself is untouched and already covered by
# test-team-elo.R; these tests cover only the AFLW-specific data-prep
# (.matches_from_aflw_results()) and the thin wrapper (build_aflw_team_elo()).

.mock_aflw_results <- function() {
  tibble::tibble(
    match_id       = c("CD_M2025264010" , "CD_M2025264020"),
    utc_start_time = c("2025-08-14T09:15:00.000+0000", "2025-08-21T09:15:00.000+0000"),
    season         = c(2025, 2025),
    home_team_name = c("Carlton", "Melbourne"),
    away_team_name = c("Collingwood", "Carlton"),
    home_score     = c(60, 45),
    away_score     = c(40, 55),
    status         = c("CONCLUDED", "CONCLUDED")
  )
}

test_that(".matches_from_aflw_results shapes load_results(comp='AFLW') into build_team_elo()'s expected frame", {
  local_mocked_bindings(
    load_results = function(seasons, comp = "AFLM") {
      expect_equal(comp, "AFLW")
      .mock_aflw_results()
    }
  )

  m <- torp:::.matches_from_aflw_results(2025)

  expect_true(all(c("match_id", "date", "season", "home_team", "away_team", "home_margin") %in% names(m)))
  expect_equal(nrow(m), 2)
  expect_equal(m$home_margin, c(20, -10))
  expect_equal(m$date, as.Date(c("2025-08-14", "2025-08-21")))
  # sorted by date then match_id
  expect_equal(m$date, sort(m$date))
})

test_that(".matches_from_aflw_results returns a zero-row frame with the right columns when there are no results", {
  local_mocked_bindings(
    load_results = function(seasons, comp = "AFLM") tibble::tibble()
  )

  m <- torp:::.matches_from_aflw_results(2025)
  expect_equal(nrow(m), 0)
  expect_true(all(c("match_id", "date", "season", "home_team", "away_team", "home_margin") %in% names(m)))
})

test_that("build_aflw_team_elo uses AFLW-specific constants by default, not men's ELO_K/HGA/CARRYOVER", {
  local_mocked_bindings(
    load_results = function(seasons, comp = "AFLM") .mock_aflw_results()
  )

  res_default <- torp:::build_aflw_team_elo(2025)
  res_explicit_aflw_const <- torp:::build_aflw_team_elo(
    2025, k = AFLW_ELO_K, hga = AFLW_ELO_HGA, carryover = AFLW_ELO_CARRYOVER
  )
  expect_equal(res_default, res_explicit_aflw_const)

  # Sanity: build_aflw_team_elo output has the same shape as men's build_team_elo
  expect_true(all(c("match_id", "team_name", "elo_pre") %in% names(res_default$by_match)))
  expect_true(all(c("team_name", "elo_current") %in% names(res_default$current)))
})
