test_that("WP monotone constraint string aligns with WP_MODEL_FEATURES", {
  vals <- as.integer(strsplit(gsub("[()]", "", wp_monotone_constraints()), ",")[[1]])
  expect_length(vals, length(WP_MODEL_FEATURES))            # 15-vs-18 can never recur
  expect_true(all(vals %in% 0:1))
  expect_identical(WP_MODEL_FEATURES[vals == 1L],
                   WP_MODEL_FEATURES[WP_MODEL_FEATURES %in% WP_MONOTONE_INCREASING])
  expect_setequal(WP_MODEL_FEATURES[vals == 1L], WP_MONOTONE_INCREASING)
})

test_that("constraint string matches the canonical literal (conscious-change pin)", {
  expect_identical(wp_monotone_constraints(),
                   "(0,0,0,0,1,1,1,0,1,1,0,0,0,0,0,0,0,0)")
  # Changing WP features/monotonicity is allowed — but you must update this
  # literal in the same commit, i.e. consciously.
})

test_that("selectors return the pinned feature sets in pinned order", {
  # snapshot pins: adding/reordering a feature forces this file to change too
  expect_identical(WP_MODEL_FEATURES, c(
    "est_match_elapsed", "est_match_remaining", "shot_row", "home", "points_diff",
    "xpoints_diff", "pos_lead_prob", "time_left_scaler", "diff_time_ratio", "score_urgency",
    "goal_x",
    "play_type_handball", "play_type_kick", "play_type_reception",
    "phase_of_play_handball_received", "phase_of_play_hard_ball",
    "phase_of_play_loose_ball", "phase_of_play_set_shot"
  ))
  expect_identical(EPV_MODEL_FEATURES, c(
    "goal_x", "y", "lag_goal_x", "lag_goal_x5", "lag_y",
    "period_seconds", "period", "play_type_handball", "play_type_kick",
    "play_type_reception", "phase_of_play_handball_received",
    "phase_of_play_hard_ball", "phase_of_play_loose_ball",
    "phase_of_play_set_shot", "shot_row", "speed5", "home",
    "est_qtr_remaining", "est_match_remaining"
  ))
  df <- as.data.frame(setNames(as.list(rep(1, length(WP_MODEL_FEATURES) + 2)),
                               c(rev(WP_MODEL_FEATURES), "junk1", "junk2")))  # scrambled input
  expect_identical(names(select_wp_model_vars(df)), WP_MODEL_FEATURES)
})

test_that("model.matrix preserves selector order (the serving contract)", {
  df <- as.data.frame(setNames(as.list(rep(1, length(WP_MODEL_FEATURES))), WP_MODEL_FEATURES))
  X <- stats::model.matrix(~ . + 0, data = select_wp_model_vars(df), na.action = na.pass)
  expect_identical(colnames(X), WP_MODEL_FEATURES)
})
