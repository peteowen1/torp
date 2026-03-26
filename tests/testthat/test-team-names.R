# Tests for Team Name Standardisation
# =====================================

# -----------------------------------------------------------------------------
# AFL_TEAMS lookup table
# -----------------------------------------------------------------------------

test_that("AFL_TEAMS has 18 rows with no NAs", {
  expect_equal(nrow(AFL_TEAMS), 18)
  expect_equal(ncol(AFL_TEAMS), 3)
  expect_true(all(c("name", "full", "abbr") %in% names(AFL_TEAMS)))
  expect_false(anyNA(AFL_TEAMS))
})

test_that("AFL_TEAMS has unique values in each column", {
  expect_equal(length(unique(AFL_TEAMS$name)), 18)
  expect_equal(length(unique(AFL_TEAMS$full)), 18)
  expect_equal(length(unique(AFL_TEAMS$abbr)), 18)
})

# -----------------------------------------------------------------------------
# AFL_TEAM_ALIASES
# -----------------------------------------------------------------------------

test_that("AFL_TEAM_ALIASES maps to valid canonical names", {
  expect_true(all(AFL_TEAM_ALIASES %in% AFL_TEAMS$name))
})

test_that("all 18 teams are reachable via at least one alias", {
  expect_equal(sort(unique(AFL_TEAM_ALIASES)), sort(AFL_TEAMS$name))
})

# -----------------------------------------------------------------------------
# torp_replace_teams()
# -----------------------------------------------------------------------------

test_that("canonical names pass through unchanged", {
  expect_equal(torp_replace_teams(AFL_TEAMS$name), AFL_TEAMS$name)
})

test_that("abbreviations resolve correctly", {
  expect_equal(torp_replace_teams("ADEL"), "Adelaide Crows")
  expect_equal(torp_replace_teams("BL"), "Brisbane Lions")
  expect_equal(torp_replace_teams("WB"), "Western Bulldogs")
  expect_equal(torp_replace_teams("NM"), "North Melbourne Kangaroos")
  expect_equal(torp_replace_teams("PA"), "Port Adelaide Power")
})

test_that("full names resolve correctly", {
  expect_equal(torp_replace_teams("Adelaide Crows"), "Adelaide Crows")
  expect_equal(torp_replace_teams("GWS Giants"), "GWS Giants")
  expect_equal(torp_replace_teams("Sydney Swans"), "Sydney Swans")
  expect_equal(torp_replace_teams("Western Bulldogs"), "Western Bulldogs")
  expect_equal(torp_replace_teams("Geelong Cats"), "Geelong Cats")
})

test_that("nicknames resolve correctly", {
  expect_equal(torp_replace_teams("Blues"), "Carlton Blues")
  expect_equal(torp_replace_teams("Magpies"), "Collingwood Magpies")
  expect_equal(torp_replace_teams("Swans"), "Sydney Swans")
  expect_equal(torp_replace_teams("Bulldogs"), "Western Bulldogs")
  expect_equal(torp_replace_teams("SUNS"), "Gold Coast Suns")
  expect_equal(torp_replace_teams("GIANTS"), "GWS Giants")
})

test_that("Indigenous round names resolve correctly", {
  expect_equal(torp_replace_teams("Narrm"), "Melbourne Demons")
  expect_equal(torp_replace_teams("Walyalup"), "Fremantle Dockers")
  expect_equal(torp_replace_teams("Kuwarna"), "Adelaide Crows")
  expect_equal(torp_replace_teams("Euro-Yroke"), "St Kilda Saints")
  expect_equal(torp_replace_teams("Yartapuulti"), "Port Adelaide Power")
  expect_equal(torp_replace_teams("Waalitj Marawar"), "West Coast Eagles")
  expect_equal(torp_replace_teams("Wallitj Marawar"), "West Coast Eagles")
})

test_that("Footscray resolves to Western Bulldogs", {
  expect_equal(torp_replace_teams("Footscray"), "Western Bulldogs")
  expect_equal(torp_replace_teams("Footscray Bulldogs"), "Western Bulldogs")
})

test_that("vectorized input works", {
  input <- c("Adelaide Crows", "Narrm", "WB", "Sydney", "Unknown FC")
  expected <- c("Adelaide Crows", "Melbourne Demons", "Western Bulldogs", "Sydney Swans", "Unknown FC")
  expect_equal(torp_replace_teams(input), expected)
})

test_that("unknown team names pass through unchanged", {
  expect_equal(torp_replace_teams("Unknown FC"), "Unknown FC")
  expect_equal(torp_replace_teams("Test Team"), "Test Team")
})

test_that("NA input returns NA", {
  expect_true(is.na(torp_replace_teams(NA_character_)))
})

test_that("zero-length input returns character(0)", {
  expect_identical(torp_replace_teams(character(0)), character(0))
  expect_identical(torp_team_abbr(character(0)), character(0))
  expect_identical(torp_team_full(character(0)), character(0))
})

test_that("historical name variants resolve correctly", {
  expect_equal(torp_replace_teams("Greater Western Sydney"), "GWS Giants")
  expect_equal(torp_replace_teams("Greater Western Sydney Giants"), "GWS Giants")
  expect_equal(torp_replace_teams("South Melbourne"), "Sydney Swans")
  expect_equal(torp_replace_teams("Brisbane Bears"), "Brisbane Lions")
})

test_that("round-trip: abbr and full names resolve back to canonical", {
  expect_equal(torp_replace_teams(AFL_TEAMS$abbr), AFL_TEAMS$name)
  expect_equal(torp_replace_teams(AFL_TEAMS$full), AFL_TEAMS$name)
})

# -----------------------------------------------------------------------------
# torp_team_abbr()
# -----------------------------------------------------------------------------

test_that("torp_team_abbr returns correct abbreviations", {
  expect_equal(torp_team_abbr("Adelaide Crows"), "ADEL")
  expect_equal(torp_team_abbr("Western Bulldogs"), "WB")
  expect_equal(torp_team_abbr("Footscray"), "WB")
  expect_equal(torp_team_abbr("GWS Giants"), "GWS")
})

test_that("torp_team_abbr works vectorized", {
  expect_equal(
    torp_team_abbr(c("Adelaide Crows", "Sydney Swans", "Narrm")),
    c("ADEL", "SYD", "MELB")
  )
})

test_that("torp_team_abbr returns NA for unknown or NA input", {
  expect_true(is.na(torp_team_abbr("Unknown FC")))
  expect_true(is.na(torp_team_abbr(NA_character_)))
})

# -----------------------------------------------------------------------------
# torp_team_full()
# -----------------------------------------------------------------------------

test_that("torp_team_full returns correct full names", {
  expect_equal(torp_team_full("Adelaide Crows"), "Adelaide Crows")
  expect_equal(torp_team_full("GWS Giants"), "GWS Giants")
  expect_equal(torp_team_full("Footscray"), "Western Bulldogs")
  expect_equal(torp_team_full("WB"), "Western Bulldogs")
})

test_that("torp_team_full works vectorized", {
  expect_equal(
    torp_team_full(c("Adelaide Crows", "GWS Giants", "Narrm")),
    c("Adelaide Crows", "GWS Giants", "Melbourne Demons")
  )
})

test_that("torp_team_full returns NA for unknown or NA input", {
  expect_true(is.na(torp_team_full("Unknown FC")))
  expect_true(is.na(torp_team_full(NA_character_)))
})
