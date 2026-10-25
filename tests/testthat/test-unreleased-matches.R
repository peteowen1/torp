# .unreleased_completed_matches() -- the decision behind has_new_games() in
# data-raw/01-data/daily_release.R. Before 2026-10-25 "any game in the round
# has started" made every off-season run a full release.

.fx <- function() {
  data.frame(
    match_id = c("CD_M1", "CD_M2", "CD_M3", "CD_M4"),
    round_number = c(29L, 28L, 28L, 28L),
    status = c("CONCLUDED", "CONCLUDED", "LIVE", "SCHEDULED"),
    stringsAsFactors = FALSE
  )
}

test_that("off-season: the Grand Final is released, so nothing is new", {
  expect_identical(.unreleased_completed_matches(.fx(), 29, c("CD_M1", "CD_M2")), character())
})

test_that("a concluded match missing from the released pbp is new", {
  expect_identical(.unreleased_completed_matches(.fx(), 29, "CD_M2"), "CD_M1")
  expect_identical(.unreleased_completed_matches(.fx(), 29, character()), "CD_M1")
})

test_that("started-but-unfinished matches and other rounds never count", {
  # CD_M3 is LIVE, CD_M4 not started: only CD_M2 in round 28 is complete.
  expect_identical(.unreleased_completed_matches(.fx(), 28, character()), "CD_M2")
  expect_identical(.unreleased_completed_matches(.fx(), 28, "CD_M2"), character())
  # NA status is not complete.
  fx <- .fx(); fx$status[1] <- NA
  expect_identical(.unreleased_completed_matches(fx, 29, character()), character())
})

test_that("empty fixtures give nothing; fixtures without status abort rather than guess", {
  expect_identical(.unreleased_completed_matches(.fx()[0, ], 29, character()), character())
  expect_error(.unreleased_completed_matches(.fx()[, c("match_id", "round_number")], 29, character()),
               "status")
})

test_that("a match in pbp but missing from a later release file is still new", {
  # The full release writes pbp first; player_stats failed (tryCatch only
  # warns), so the next run must redo the full release to repair it.
  released <- list(pbp = c("CD_M1", "CD_M2"), xg = c("CD_M1", "CD_M2"),
                   player_stats = "CD_M2")
  expect_identical(.unreleased_completed_matches(.fx(), 29, released), "CD_M1")
  # Once every file has it, it is released.
  released$player_stats <- c("CD_M1", "CD_M2")
  expect_identical(.unreleased_completed_matches(.fx(), 29, released), character())
  # A file never released at all leaves every completed match new.
  expect_identical(.unreleased_completed_matches(.fx(), 29, list(xg = "CD_M1", ps = character())), "CD_M1")
  expect_identical(.unreleased_completed_matches(.fx(), 29, list()), "CD_M1")
})
