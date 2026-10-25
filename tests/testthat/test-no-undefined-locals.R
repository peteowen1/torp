# Guards the class of bug that broke production predictions on 2026-10-25:
# torp#270 moved PSR loading into .build_rated_match_features(), and
# build_prediction_state() kept using a psr_df it no longer defined.
# codetools lists every free variable a function reads. dplyr column names
# show up there too, so this checks the locals the #270 refactor moved into
# .build_rated_match_features(): none of them may be read without a binding.
moved_locals <- c("psr_df", "xrapm_df", "team_rt_df", "skills")
test_that("prediction-path functions don't read locals that moved into the shared helper", {
  skip_if_not_installed("codetools")
  ns <- asNamespace("torp")
  fns <- c("build_prediction_state", "build_team_mdl_df", "run_predictions_pipeline")
  for (fn in fns) {
    free <- codetools::findGlobals(get(fn, envir = ns), merge = FALSE)$variables
    expect_identical(intersect(free, moved_locals), character(0), info = fn)
  }
})

test_that(".build_rated_match_features hands back psr_df for the weekly summary", {
  body_txt <- paste(deparse(body(get(".build_rated_match_features", envir = asNamespace("torp")))), collapse = "\n")
  expect_match(body_txt, 'attr(out, "psr_df") <- psr_df', fixed = TRUE)
  bps <- paste(deparse(body(get("build_prediction_state", envir = asNamespace("torp")))), collapse = "\n")
  expect_match(bps, 'psr_df <- attr(team_rt_fix_df, "psr_df")', fixed = TRUE)
})
