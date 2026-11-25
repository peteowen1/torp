# .strip_gam() / .reset_formula_envs(): a GAM fitted inside a function must not
# carry that function's frame into saveRDS() (match_gams.rds was 66 MB; 6 MB after).

test_that(".strip_gam drops a fit-time frame and keeps predictions identical", {
  skip_if_not_installed("mgcv")
  fit_in_frame <- function() {
    big <- runif(2e5)                       # stands in for the training frame
    set.seed(1)
    d <- data.frame(x = runif(300), z = factor(sample(letters[1:3], 300, TRUE)))
    d$y <- sin(3 * d$x) + as.integer(d$z) + rnorm(300, sd = 0.2)
    mgcv::bam(y ~ s(x) + s(z, bs = "re"), data = d, discrete = TRUE)
  }
  m <- fit_in_frame()
  s <- .strip_gam(m)
  nd <- data.frame(x = c(0.2, 0.7), z = factor(c("a", "c"), levels = letters[1:3]))
  expect_identical(predict(s, nd), predict(m, nd))
  expect_lt(length(serialize(s, NULL)), 1e6)          # the 1.6 MB `big` is gone
  expect_true(all(class(m) %in% class(s)))
})

test_that(".reset_formula_envs repoints formulas and model-frame terms", {
  f <- local({ big <- 1:10; y ~ x })
  expect_false(identical(environment(f), globalenv()))
  expect_identical(environment(.reset_formula_envs(f)), globalenv())
  mf <- local({ big <- 1:10; model.frame(y ~ x, data.frame(x = 1:3, y = 1:3)) })
  expect_identical(environment(attr(.reset_formula_envs(list(mf = mf))$mf, "terms")), globalenv())
})
