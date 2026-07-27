# The team-rating build keeps every named player bar emergencies
# (FABLE-DEFENDER-VALUE-PLAN.md §7.29-§7.30).

test_that("the two lineup filters are identical in source", {
  # .build_team_ratings_df() builds the TRAINING frame and match_model.R the
  # SERVING frame. If they diverge, the model trains on one player set and
  # predicts on another -- a silent train/serve skew with no error anywhere.
  # Compared as source text because the two functions are internal and need
  # heavy fixtures to exercise behaviourally; this catches the drift cheaply.
  grab <- function(f) {
    p <- file.path("..", "..", "R", f)
    if (!file.exists(p)) return(character(0))
    trimws(grep("dplyr::filter(lineup_position", readLines(p, warn = FALSE),
                value = TRUE, fixed = TRUE))
  }
  a <- grab("match_data_prep.R")
  b <- grab("match_model.R")
  skip_if(length(a) == 0 || length(b) == 0, "package source not available here")
  expect_equal(a, b)
  expect_false(any(grepl("SUB", c(a, b), fixed = TRUE)))
})

test_that("SUB carries a part-game weight and EMERG a near-zero one", {
  # SUB is live now that the sub is kept, so its value matters. It is set to
  # his measured time on ground rather than to a round number.
  expect_equal(unname(POSITION_AVG_TOG[["SUB"]]), 0.33)
  expect_lt(POSITION_AVG_TOG[["SUB"]], POSITION_AVG_TOG[["INT"]])
  expect_lt(POSITION_AVG_TOG[["EMERG"]], POSITION_AVG_TOG[["SUB"]])
})

test_that("every on-field lineup code has a TOG weight", {
  # A code missing from POSITION_AVG_TOG silently falls back to 0.75 with a
  # warning. Now that fewer rows are filtered out, more codes reach this map.
  onfield <- names(LINEUP_POSITION_GROUP_MAP)[!is.na(LINEUP_POSITION_GROUP_MAP)]
  expect_true(all(onfield %in% names(POSITION_AVG_TOG)))
  expect_true(all(c("INT", "SUB", "EMERG") %in% names(POSITION_AVG_TOG)))
})

test_that("TOG weights are plausible fractions", {
  expect_true(all(POSITION_AVG_TOG > 0 & POSITION_AVG_TOG <= 1))
  # key defensive posts play the most; that ordering is a sanity anchor
  expect_gt(POSITION_AVG_TOG[["FB"]], POSITION_AVG_TOG[["INT"]])
})
