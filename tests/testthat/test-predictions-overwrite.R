# torp C2 / ECOSYSTEM-FIX-PLAN.md T3: .build_locked_predictions() (the
# extracted overwrite guard from run_predictions_pipeline()) must never
# collapse a transient read failure into "no existing predictions", and must
# floor-guard any accumulating merge. No network -- file_reader/
# vb_confirm_absent are mocked.

.mock_team_mdl_df <- function() {
  data.frame(
    match_id = character(0), season.x = integer(0), round_number.x = integer(0),
    team_type_fac.x = character(0), utc_dt = as.POSIXct(character(0)),
    stringsAsFactors = FALSE
  )
}

.mock_week_gms <- function(n = 2, week = 5) {
  data.frame(
    week = week,
    match_id = paste0("M", seq_len(n)),
    margin = seq_len(n),
    stringsAsFactors = FALSE
  )
}

test_that(".build_locked_predictions aborts on a transient file_reader error (never treats it as absent)", {
  local_mocked_bindings(
    file_reader = function(file_name, release_tag) {
      stop(structure(
        class = c("vb_error_transient", "vb_error", "error", "condition"),
        list(message = "simulated 500", call = NULL)
      ))
    }
  )

  expect_error(
    .build_locked_predictions(
      "predictions_2026", 2026, .mock_week_gms(), .mock_team_mdl_df(),
      target_weeks = 5, completed_margins = data.frame(match_id = character(0), .actual_margin = numeric(0))
    ),
    "not a confirmed-absent"
  )
})

test_that(".build_locked_predictions proceeds fresh when file_reader confirms absence", {
  local_mocked_bindings(
    file_reader = function(file_name, release_tag) {
      stop(structure(
        class = c("vb_error_absent", "vb_error", "error", "condition"),
        list(message = "404 not found", call = NULL))
      )
    },
    vb_confirm_absent = function(repo, tag, name) TRUE
  )

  out <- .build_locked_predictions(
    "predictions_2026", 2026, .mock_week_gms(n = 3), .mock_team_mdl_df(),
    target_weeks = 5, completed_margins = data.frame(match_id = character(0), .actual_margin = numeric(0))
  )
  expect_equal(nrow(out), 3)
})

test_that(".build_locked_predictions refuses a fresh upload when the asset is not confirmed absent", {
  local_mocked_bindings(
    file_reader = function(file_name, release_tag) {
      stop(structure(
        class = c("vb_error_absent", "vb_error", "error", "condition"),
        list(message = "404 not found", call = NULL))
      )
    },
    vb_confirm_absent = function(repo, tag, name) FALSE
  )

  expect_error(
    .build_locked_predictions(
      "predictions_2026", 2026, .mock_week_gms(), .mock_team_mdl_df(),
      target_weeks = 5, completed_margins = data.frame(match_id = character(0), .actual_margin = numeric(0))
    ),
    "Refusing fresh upload"
  )
})

test_that(".build_locked_predictions floor-guards the merge -- a >10% shrink aborts before upload", {
  # Corrupted "existing" read: 950 duplicate rows all sharing match_id "E1"
  # plus 50 genuinely distinct rows. The merge algorithm removes EVERY
  # existing row whose match_id appears in this week's fresh data (a set
  # membership test, not a 1:1 replacement) -- so replacing "E1" with a
  # single fresh row collapses 950 duplicate rows down to 1, a >10% shrink
  # vb_guard_accumulate must catch before any upload.
  existing_dup <- data.frame(
    week = c(rep(1, 950), 2:51),
    match_id = c(rep("E1", 950), paste0("E", 2:51)),
    margin = seq_len(1000),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    file_reader = function(file_name, release_tag) existing_dup
  )

  team_mdl_df <- .mock_team_mdl_df()  # no started matches -> nothing dropped from week_gms
  week_gms <- data.frame(week = 1, match_id = "E1", margin = 999, stringsAsFactors = FALSE)

  expect_error(
    .build_locked_predictions(
      "predictions_2026", 2026, week_gms, team_mdl_df,
      target_weeks = 1, completed_margins = data.frame(match_id = character(0), .actual_margin = numeric(0))
    ),
    class = "vb_error_integrity"
  )
})

test_that(".build_locked_predictions merges without tripping the floor guard on a normal weekly update", {
  existing <- data.frame(
    week = rep(1:10, each = 9),
    match_id = paste0("E", seq_len(90)),
    margin = seq_len(90),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    file_reader = function(file_name, release_tag) existing
  )

  team_mdl_df <- .mock_team_mdl_df()

  out <- .build_locked_predictions(
    "predictions_2026", 2026, .mock_week_gms(n = 9, week = 11), team_mdl_df,
    target_weeks = 11, completed_margins = data.frame(match_id = character(0), .actual_margin = numeric(0))
  )

  expect_equal(nrow(out), 99)  # 90 existing + 9 new, no overlap
})
