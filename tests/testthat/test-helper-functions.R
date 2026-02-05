# Tests for helper_functions.R
# Tests for clean_pbp and related internal functions

test_that("clean_pbp_dt internal helper functions exist", {
  # Test that key internal data.table functions exist
  expect_true(exists("add_torp_ids_dt", envir = asNamespace("torp")))
  expect_true(exists("add_chain_vars_dt", envir = asNamespace("torp")))
  expect_true(exists("add_quarter_vars_dt", envir = asNamespace("torp")))
  expect_true(exists("add_game_vars_dt", envir = asNamespace("torp")))
  expect_true(exists("add_score_vars_dt", envir = asNamespace("torp")))
})

test_that("nafill_char works correctly", {
  nafill_char <- torp:::nafill_char

  # Test LOCF
  x <- c("a", NA, NA, "b", NA)
  expect_equal(nafill_char(x, type = "locf"), c("a", "a", "a", "b", "b"))

  # Test NOCB
  expect_equal(nafill_char(x, type = "nocb"), c("a", "b", "b", "b", NA))

  # Test with all NAs
  all_na <- c(NA_character_, NA_character_)
  expect_equal(nafill_char(all_na, type = "locf"), c(NA_character_, NA_character_))

  # Test invalid type
  expect_error(nafill_char(x, type = "invalid"))
})

test_that("clean_pbp function exists and is exported", {
  expect_true(exists("clean_pbp"))
  expect_true("clean_pbp" %in% getNamespaceExports("torp"))
})
