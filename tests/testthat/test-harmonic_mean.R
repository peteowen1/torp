test_that("harmonic_mean computes row-wise harmonic means", {
  x <- c(1, 2, 3)
  y <- c(2, 3, 4)
  result <- harmonic_mean(x, y)
  expected <- c(1.333333, 2.4, 3.428571)
  expect_equal(result, expected, tolerance = 1e-6)

  result2 <- harmonic_mean(c(1, 0), c(2, 2))
  expect_true(is.na(result2[2]))
})
