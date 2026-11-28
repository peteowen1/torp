utc <- function(x) as.POSIXct(x, tz = "UTC")

handoff_state <- function(...) {
  s <- list(
    season = 2026L, target_weeks = 20L, is_backfill = FALSE,
    team_mdl_df = data.frame(season.x = 2026L, round_number.x = 20L, pred_score_diff = c(4.5, -4.5)),
    team_rt_fix_df = data.frame(team_name = c("A", "B")),
    all_grounds = data.frame(venue = "MCG"),
    gam_result = list(models = list(win = "gam"), data = data.frame(x = 1:3)),
    xgb_result = list(models = list(a = "xgb"), feature_names = c("f1", "f2"), data = 1),
    margin_calib = list(b = 1.1),
    validation_errors = character(0)
  )
  utils::modifyList(s, list(...))
}

saved_obj <- function(at = "2026-08-01 06:00:00", ...) {
  st <- handoff_state()
  out <- list(
    meta = list(season = 2026L, week = 20L, saved_at = utc(at), torp_version = "9.9.9", n_rows = 2L),
    state = st[c("team_mdl_df", "team_rt_fix_df", "all_grounds", "gam_result", "xgb_result",
                 "margin_calib", "validation_errors")]
  )
  utils::modifyList(out, list(...))
}
decide <- function(saved, season = 2026L, week = 20L, now = utc("2026-08-01 06:10:00"), v = "9.9.9") {
  .match_state_handoff_decision(saved, season, week, now, v)
}

test_that("a fresh state for the same season, round and version is reused", {
  d <- decide(saved_obj())
  expect_true(d$reuse)
  expect_match(d$reason, "2026 R20 saved 10 minutes ago")
})

test_that("a different round, season or torp version rebuilds", {
  expect_false(decide(saved_obj(), week = 21L)$reuse)
  expect_match(decide(saved_obj(), week = 21L)$reason, "is for 2026 R20, not 2026 R21")
  expect_false(decide(saved_obj(), season = 2027L)$reuse)
  d <- decide(saved_obj(), v = "9.9.10")
  expect_false(d$reuse)
  expect_match(d$reason, "saved by torp 9.9.9")
})

test_that("an old state, or one saved in the future, rebuilds", {
  expect_false(decide(saved_obj(at = "2026-08-01 04:00:00"))$reuse)
  expect_match(decide(saved_obj(at = "2026-08-01 04:00:00"))$reason, "130 minutes old")
  expect_false(decide(saved_obj(at = "2026-08-01 07:00:00"))$reuse)
})

test_that("anything malformed rebuilds", {
  expect_false(decide(NULL)$reuse)
  expect_false(decide(list(meta = 1, state = 2))$reuse)
  expect_false(decide(list(meta = list(), state = list()))$reuse)
  ok <- saved_obj()
  expect_false(decide(ok, now = as.POSIXct(NA))$reuse)
  expect_false(decide(ok, week = c(20L, 21L))$reuse)
  expect_false(decide(ok, week = NA_integer_)$reuse)
  for (drop in c("team_mdl_df", "team_rt_fix_df", "all_grounds", "gam_result")) {
    bad <- ok
    bad$state[[drop]] <- NULL
    expect_false(decide(bad)$reuse, info = drop)
  }
  empty <- ok
  empty$state$team_mdl_df <- empty$state$team_mdl_df[0, ]
  expect_false(decide(empty)$reuse)
  invalid <- ok
  invalid$state$validation_errors <- "NA pred_win"
  expect_match(decide(invalid)$reason, "failed validation")
  na_time <- ok
  na_time$meta$saved_at <- as.POSIXct(NA)
  expect_false(decide(na_time)$reuse)
})

test_that("a GAM-only state (no XGBoost, no calibration) is still reusable", {
  ok <- saved_obj()
  ok$state$xgb_result <- NULL
  ok$state$margin_calib <- NULL
  expect_true(decide(ok)$reuse)
})

test_that("save then load round-trips the state the matchup step reads", {
  path <- withr::local_tempfile(fileext = ".rds")
  st <- handoff_state()
  expect_true(suppressMessages(.save_match_state_handoff(st, path)))
  expect_false(file.exists(paste0(path, ".tmp")))
  h <- .load_match_state_handoff(2026L, 20L, path)
  expect_true(h$reuse)
  expect_identical(h$state$team_mdl_df, st$team_mdl_df)
  expect_identical(h$state$team_rt_fix_df, st$team_rt_fix_df)
  expect_identical(h$state$gam_result$models, st$gam_result$models)
  expect_identical(h$state$xgb_result$feature_names, st$xgb_result$feature_names)
  expect_identical(h$state$margin_calib, st$margin_calib)
  # The duplicate training frame is not carried.
  expect_null(h$state$gam_result$data)
  expect_false(.load_match_state_handoff(2026L, 21L, path)$reuse)
})

test_that("nothing is saved when off, for a backfill, several rounds or an invalid state", {
  path <- withr::local_tempfile(fileext = ".rds")
  expect_false(.save_match_state_handoff(handoff_state(), ""))
  expect_false(suppressMessages(.save_match_state_handoff(handoff_state(is_backfill = TRUE), path)))
  expect_false(suppressMessages(.save_match_state_handoff(handoff_state(target_weeks = 1:3), path)))
  expect_false(suppressMessages(.save_match_state_handoff(handoff_state(validation_errors = "bad"), path)))
  expect_false(suppressMessages(.save_match_state_handoff(NULL, path)))
  expect_false(file.exists(path))
})

test_that("a save that cannot write warns and returns FALSE instead of throwing", {
  bad <- file.path(withr::local_tempdir(), "no-such-dir", "s.rds")
  expect_warning(res <- .save_match_state_handoff(handoff_state(), bad), "save failed")
  expect_false(res)
})

test_that("load rebuilds when off, when the file is missing, and when it is not an RDS", {
  expect_match(.load_match_state_handoff(2026L, 20L, "")$reason, "hand-over is off")
  path <- withr::local_tempfile(fileext = ".rds")
  expect_match(.load_match_state_handoff(2026L, 20L, path)$reason, "no saved state")
  writeLines("not an rds", path)
  h <- .load_match_state_handoff(2026L, 20L, path)
  expect_false(h$reuse)
  expect_null(h$state)
  expect_match(h$reason, "unreadable")
})

test_that("the env variable is the default path", {
  withr::local_envvar(TORP_MATCH_STATE_HANDOFF = "")
  expect_identical(.match_state_handoff_path(), "")
  withr::local_envvar(TORP_MATCH_STATE_HANDOFF = "x/y.rds")
  expect_identical(.match_state_handoff_path(), "x/y.rds")
})
