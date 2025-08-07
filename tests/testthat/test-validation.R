test_that("validate_seasons works correctly", {
  # Valid seasons
  expect_equal(torp:::validate_seasons(2021), 2021)
  expect_equal(torp:::validate_seasons(c(2021, 2022)), c(2021, 2022))
  expect_equal(torp:::validate_seasons(TRUE), 2021:2025)  # Assumes current season is 2025
  
  # Invalid seasons should error
  expect_error(torp:::validate_seasons("2021"), "must be numeric")
  expect_error(torp:::validate_seasons(2020), "Invalid season years")
  expect_error(torp:::validate_seasons(2030), "Invalid season years")
})

test_that("validate_rounds works correctly", {
  # Valid rounds
  expect_equal(torp:::validate_rounds(1), 1)
  expect_equal(torp:::validate_rounds(c(1, 2, 28)), c(1, 2, 28))
  expect_equal(torp:::validate_rounds(TRUE), 0:28)
  
  # Invalid rounds should error
  expect_error(torp:::validate_rounds("1"), "must be numeric")
  expect_error(torp:::validate_rounds(-1), "Invalid round numbers")
  expect_error(torp:::validate_rounds(29), "Invalid round numbers")
})

test_that("check_internet_connection works", {
  # This test might be environment dependent
  result <- torp:::check_internet_connection()
  expect_type(result, "logical")
  expect_length(result, 1)
})