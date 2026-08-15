# .predict_all_rows() — row alignment when features carry NA.
#
# The bug this covers is not "predictions are wrong", it is "predictions are
# attached to the wrong match and nothing says so". Placeholder finals fixtures
# (teams TBD) carry NA rating features every September, and model.matrix()'s
# default na.action drops those rows, so the returned vector is shorter than the
# frame. When the lengths happen to divide, R recycles in silence.
#
# These tests pin the property the fix depends on, and the guard that catches it
# if the property ever stops holding.

skip_if_not_installed("xgboost")

# Small deterministic booster; the numbers are irrelevant, only the row count is.
.fit_toy_booster <- function() {
  withr::local_seed(42)
  x <- matrix(stats::rnorm(400), ncol = 2, dimnames = list(NULL, c("a", "b")))
  y <- x[, 1] * 2 + stats::rnorm(200) * 0.1
  xgboost::xgb.train(
    params = list(objective = "reg:squarederror", nthread = 1L),
    data = xgboost::xgb.DMatrix(data = x, label = y),
    nrounds = 5L, verbose = 0
  )
}

test_that("model.matrix() alone drops NA rows -- the premise of the guard", {
  # If this ever fails, R changed and the fix can be simplified. It is here so
  # that a future reader does not "simplify" the model.frame() detour away.
  df <- data.frame(a = c(1, 2, NA, 4, 5), b = c(5, 4, 3, NA, 1))

  expect_lt(nrow(stats::model.matrix(~ . - 1, data = df)), nrow(df))

  # And the obvious one-line fix genuinely does NOT work -- this is the trap.
  expect_lt(
    nrow(stats::model.matrix(~ . - 1, data = df, na.action = stats::na.pass)),
    nrow(df)
  )

  # Only routing na.action through model.frame() preserves the rows.
  mf <- stats::model.frame(~ . - 1, data = df, na.action = stats::na.pass)
  expect_equal(nrow(stats::model.matrix(~ . - 1, data = mf)), nrow(df))
})

test_that(".predict_all_rows() returns one prediction per row when features are NA", {
  bst <- .fit_toy_booster()
  df <- data.frame(a = c(1, 2, NA, 4, 5), b = c(5, 4, 3, NA, 1))

  preds <- .predict_all_rows(bst, df, c("a", "b"))

  expect_length(preds, nrow(df))
  expect_true(all(is.finite(preds)))
})

test_that(".predict_all_rows() is unchanged on a frame with no NAs", {
  bst <- .fit_toy_booster()
  clean <- data.frame(a = c(1, 2, 3, 4, 5), b = c(5, 4, 3, 2, 1))

  # Identity behaviour when the mechanism shouldn't apply: the na.pass detour
  # must not perturb the ordinary path.
  direct <- predict(bst, xgboost::xgb.DMatrix(
    data = stats::model.matrix(~ . - 1, data = clean)))

  expect_equal(.predict_all_rows(bst, clean, c("a", "b")), direct)
})

test_that("an all-NA feature column still yields one prediction per row", {
  bst <- .fit_toy_booster()
  # The finals case in its extreme form: a whole feature unavailable because no
  # team is named yet.
  df <- data.frame(a = c(1, 2, 3, 4), b = rep(NA_real_, 4))

  preds <- .predict_all_rows(bst, df, c("a", "b"))
  expect_length(preds, nrow(df))
})

test_that("the recycling case the guard exists for cannot slip through", {
  bst <- .fit_toy_booster()
  # 4-row frame with 2 NA rows: model.matrix() would return 2 rows, and 2
  # divides 4, so a bare assignment would RECYCLE silently rather than error.
  df <- data.frame(a = c(1, NA, 3, NA), b = c(4, 3, 2, NA))

  preds <- .predict_all_rows(bst, df, c("a", "b"))
  expect_length(preds, nrow(df))

  # Demonstrate what the old code would have done, so the test documents the
  # failure rather than only the fix.
  dropped <- stats::model.matrix(~ . - 1, data = df[, c("a", "b")])
  expect_lt(nrow(dropped), nrow(df))
  expect_equal(nrow(df) %% nrow(dropped), 0)   # divides => silent recycle
})
