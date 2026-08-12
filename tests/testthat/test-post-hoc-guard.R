# .warn_post_hoc_predictions() was dead from the day it was written until
# 2026-08-12: .format_match_preds() selected utc_start_time and then dropped it
# in the summarise() (only group vars and summarised vars survive), so the
# guard's first line found no such column and returned 0 on every single run.
# "No post-hoc rows" and "the check never ran" looked identical in the log.
#
# These tests pin the column through the aggregation and prove the guard fires
# on a frame of the shape production actually builds. A shape test is the point:
# the arithmetic was always right, the input never reached it.

.mock_team_mdl_df_preds <- function(utc = "2026-06-04T09:00:00.000+0000") {
  # team_mdl_df is long: one row per team per match, built by a self-join on
  # type_anti. So the away row is the SAME match from the away team's
  # perspective -- .x and .y swapped, and every signed prediction negated.
  # .format_match_preds() flips the away row back and averages the pair.
  # A mock that repeats the home row's .x/.y on both rows produces two
  # distinct group keys and silently tests nothing.
  row <- function(type, tx, ty, eprx, psrx, epry, psry, xdiff, sdiff, win, margin) {
    data.frame(
      match_id            = "CD_M20260142101",
      season.x            = 2026L,
      round_number.x      = 14L,
      count.x             = 23,
      team_type_fac.x     = type,
      # Factors in production (set for the GAM categorical predictors) -- keep
      # them factors here so the as.character() coercion is exercised too.
      team_name.x         = factor(tx, levels = c("Geelong Cats", "Carlton")),
      team_name.y         = factor(ty, levels = c("Geelong Cats", "Carlton")),
      epr.x = eprx, psr.x = psrx,
      epr.y = epry, psr.y = psry,
      pred_tot_xscore     = 168.0,
      pred_xscore_diff    = xdiff,
      pred_score_diff     = sdiff,
      pred_win            = win,
      bits                = 0.21,
      score_diff          = margin,
      local_start_time_str = "2026-06-04 19:00:00 AEST",
      utc_start_time      = utc,
      venue.x             = "MCG",
      stringsAsFactors    = FALSE
    )
  }
  rbind(
    row("home", "Geelong Cats", "Carlton", 12.5, 3.1, 8.2, 1.4,  14,  11, 0.72,  19),
    row("away", "Carlton", "Geelong Cats", 8.2, 1.4, 12.5, 3.1, -14, -11, 0.28, -19)
  )
}

test_that(".format_match_preds() carries utc_start_time into the prediction frame", {
  out <- .format_match_preds(.mock_team_mdl_df_preds())

  # The regression itself. Without this column .warn_post_hoc_predictions()
  # returns 0 unconditionally and the generated_utc stamp answers nothing.
  expect_true("utc_start_time" %in% names(out))
  expect_equal(out$utc_start_time, "2026-06-04T09:00:00.000+0000")
})

test_that("grouping on utc_start_time does not split a match into two rows", {
  # Both rows of a match_id share one fixture start time (the home/away
  # self-join only duplicates opp_cols, and utc_start_time is not one), so
  # adding it to the group_by must still average home + flipped-away into a
  # single match-level row. If this ever returns 2, every match in the
  # published predictions file has just been duplicated.
  out <- .format_match_preds(.mock_team_mdl_df_preds())
  expect_equal(nrow(out), 1L)
  expect_equal(out$players, 23)
})

test_that(".warn_post_hoc_predictions() fires on a frame of the shape production builds", {
  combined <- .format_match_preds(.mock_team_mdl_df_preds())
  names(combined)[names(combined) == "round"] <- "week"   # as run_predictions_pipeline() does
  # Stamped a day AFTER the 09:00 UTC bounce: a retrodiction in the locked file.
  combined$generated_utc <- "2026-06-05T09:00:00Z"

  expect_warning(
    n <- .warn_post_hoc_predictions(combined),
    "computed AFTER"
  )
  expect_equal(n, 1L)
})

test_that(".warn_post_hoc_predictions() stays quiet on a genuinely as-at prediction", {
  combined <- .format_match_preds(.mock_team_mdl_df_preds())
  names(combined)[names(combined) == "round"] <- "week"
  combined$generated_utc <- "2026-06-03T22:00:00Z"   # ~11h before the bounce

  expect_no_warning(n <- .warn_post_hoc_predictions(combined))
  expect_equal(n, 0L)
})

test_that(".warn_post_hoc_predictions() reports a skip rather than returning silently", {
  combined <- .format_match_preds(.mock_team_mdl_df_preds())
  names(combined)[names(combined) == "round"] <- "week"
  combined$generated_utc <- "2026-06-05T09:00:00Z"
  combined$utc_start_time <- NULL

  # The silent return is what hid the original defect for its whole lifetime.
  expect_message(
    n <- .warn_post_hoc_predictions(combined),
    "skipped"
  )
  expect_equal(n, 0L)
})
