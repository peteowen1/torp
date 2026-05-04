# --- format_predictions_blog() ---

.mock_preds <- function() {
  tibble::tibble(
    season = c(2026L, 2026L),
    round = c(5L, 5L),
    home_team = c("Carlton Blues", "Essendon Bombers"),
    away_team = c("Essendon Bombers", "Collingwood Magpies"),
    home_epr = c(10.0, 8.0),
    away_epr = c(8.0, 12.0),
    pred_margin = c(5.0, -3.0),
    home_win_prob = c(0.62, 0.38),
    pred_total = c(180.0, 175.0),
    actual_margin = c(NA_real_, NA_real_),
    start_time = as.POSIXct(c("2026-04-20 19:30:00", "2026-04-21 19:30:00"),
                             tz = "UTC"),
    venue = c("M.C.G.", "M.C.G.")
  )
}

test_that("format_predictions_blog outputs the canonical schema in order", {
  out <- format_predictions_blog(.mock_preds())
  expect_identical(names(out), PREDICTIONS_BLOG_COLS)
  expect_equal(nrow(out), 2)
})

test_that("format_predictions_blog aborts on missing core column", {
  bad <- .mock_preds() |> dplyr::select(-home_team)
  expect_error(
    format_predictions_blog(bad),
    regexp = "home_team"
  )
})

test_that("format_predictions_blog NA-fills xscore when xg is NULL", {
  out <- format_predictions_blog(.mock_preds(), xg = NULL)
  expect_true(all(is.na(out$xscore_home)))
  expect_true(all(is.na(out$xscore_away)))
  expect_type(out$xscore_home, "double")
})

test_that("format_predictions_blog warns when xg is non-NULL but empty", {
  empty_xg <- tibble::tibble(
    season = integer(), round = integer(),
    home_team = character(), away_team = character(),
    xscore_home = double(), xscore_away = double()
  )
  expect_warning(
    out <- format_predictions_blog(.mock_preds(), xg = empty_xg),
    regexp = "non-NULL but empty"
  )
  expect_true(all(is.na(out$xscore_home)))
})

test_that("format_predictions_blog joins xscore lookup correctly", {
  xg <- tibble::tibble(
    season = 2026L,
    round = 5L,
    home_team = "Carlton Blues",
    away_team = "Essendon Bombers",
    xscore_home = 91.5,
    xscore_away = 88.0
  )
  out <- format_predictions_blog(.mock_preds(), xg = xg)
  joined <- out[out$home_team == "Carlton Blues" &
                  out$away_team == "Essendon Bombers", ]
  expect_equal(joined$xscore_home, 91.5)
  expect_equal(joined$xscore_away, 88.0)
  # Second match had no xg row -> NA
  missing_match <- out[out$home_team == "Essendon Bombers", ]
  expect_true(is.na(missing_match$xscore_home))
})

test_that("format_predictions_blog warns when xg joins zero rows", {
  # Mismatched season -> zero matches join. Should warn rather than silently
  # ship all-NA xscores.
  xg <- tibble::tibble(
    season = 2025L,
    round = 5L,
    home_team = "Carlton Blues",
    away_team = "Essendon Bombers",
    xscore_home = 91.5,
    xscore_away = 88.0
  )
  expect_warning(
    format_predictions_blog(.mock_preds(), xg = xg),
    regexp = "[Zz]ero xg rows joined"
  )
})

test_that("format_predictions_blog fails on xg with missing columns", {
  bad_xg <- tibble::tibble(
    season = 2026L, round = 5L,
    home_team = "Carlton Blues", away_team = "Essendon Bombers",
    xscore_home = 91.5  # missing xscore_away
  )
  expect_error(
    format_predictions_blog(.mock_preds(), xg = bad_xg),
    regexp = "xscore_away"
  )
})


# --- xg_to_blog_lookup() ---

test_that("xg_to_blog_lookup reshapes xg into canonical lookup schema", {
  # Match ID layout: CD_M{YYYY}{CCC}{RR}{XX} — positions 12-13 encode round.
  # CD_M20260140501 = round 05, match 01 of 2026 AFL (comp 014).
  xg <- tibble::tibble(
    match_id = c("CD_M20260140501", "CD_M20260140601"),
    home_team = c("Carlton", "Essendon"),
    away_team = c("Essendon", "Collingwood"),
    home_xscore = c(91.56, 77.12),
    away_xscore = c(88.01, 105.44)
  )
  out <- xg_to_blog_lookup(xg, 2026)
  expect_identical(names(out),
                   c("season", "round", "home_team", "away_team",
                     "xscore_home", "xscore_away"))
  expect_equal(out$season, c(2026L, 2026L))
  expect_equal(out$round, c(5L, 6L))
  # Team names canonicalised via torp_replace_teams()
  expect_equal(out$home_team[1], "Carlton Blues")
  expect_equal(out$away_team[1], "Essendon Bombers")
  # xscore rounded to 1 dp
  expect_equal(out$xscore_home, c(91.6, 77.1))
  expect_equal(out$xscore_away, c(88.0, 105.4))
})

test_that("xg_to_blog_lookup returns NULL on NULL input", {
  expect_null(xg_to_blog_lookup(NULL, 2026))
})

test_that("xg_to_blog_lookup returns NULL on 0-row input (with info message)", {
  empty_xg <- tibble::tibble(
    match_id = character(), home_team = character(), away_team = character(),
    home_xscore = double(), away_xscore = double()
  )
  # cli_alert_info is diagnostic, not a warning -- just check NULL return.
  expect_null(xg_to_blog_lookup(empty_xg, 2026))
})

test_that("xg_to_blog_lookup aborts on missing column", {
  bad_xg <- tibble::tibble(
    match_id = "CD_M20260140501",
    home_team = "Carlton", away_team = "Essendon",
    home_xscore = 91.5  # missing away_xscore
  )
  expect_error(
    xg_to_blog_lookup(bad_xg, 2026),
    regexp = "away_xscore"
  )
})

test_that("xg_to_blog_lookup output joins cleanly against canonical preds", {
  # Short-form team names in xg (as from raw AFL API) should canonicalise so
  # the join against preds (already canonical) succeeds end-to-end.
  xg <- tibble::tibble(
    match_id = "CD_M20260140501",
    home_team = "Carlton",
    away_team = "Essendon",
    home_xscore = 91.5,
    away_xscore = 88.0
  )
  lookup <- xg_to_blog_lookup(xg, 2026)
  out <- format_predictions_blog(.mock_preds(), xg = lookup)
  joined <- out[out$home_team == "Carlton Blues", ]
  expect_equal(joined$xscore_home, 91.5)
})
