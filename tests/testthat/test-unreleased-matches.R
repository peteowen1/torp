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

# --- .release_decision(): time-bounded repair (2026-10-25, round 3) ----------
# Kickoffs are set relative to `now`; a match "ends" RELEASE_MATCH_DURATION_HOURS
# after kickoff.
.fx_t <- function(now, ended_hours_ago) {
  ko <- now - (ended_hours_ago + RELEASE_MATCH_DURATION_HOURS) * 3600
  data.frame(
    match_id = paste0("CD_M", seq_along(ended_hours_ago)),
    round_number = 23L,
    status = "CONCLUDED",
    utc_start_time = format(ko, "%Y-%m-%dT%H:%M:%S.000+0000", tz = "UTC"),
    stringsAsFactors = FALSE
  )
}
.now <- as.POSIXct("2026-08-18T12:00:00", tz = "UTC")
.all_files <- function(ids) list(xg = ids, player_stats = ids, score_events = ids, player_game_ratings = ids)

test_that("a match inside the repair window triggers even when every file has it", {
  fx <- .fx_t(.now, c(2, 100))
  d <- .release_decision(fx, 23, .all_files(fx$match_id), now = .now)
  expect_true(d$trigger)
  expect_identical(d$reason, "repair_window")
  expect_identical(d$recent, "CD_M1")
  # And without reading any files at all.
  d0 <- .release_decision(fx, 23, NULL, now = .now)
  expect_true(d0$trigger)
  expect_false(d0$needs_files)
})

test_that("past the repair window, a fully released round does not trigger", {
  fx <- .fx_t(.now, c(13, 30))
  expect_true(.release_decision(fx, 23, NULL, now = .now)$needs_files)
  d <- .release_decision(fx, 23, .all_files(fx$match_id), now = .now)
  expect_false(d$trigger)
  expect_equal(nrow(d$abandoned), 0)
})

test_that("a match missing from one file triggers until 48 h after it ended", {
  fx <- .fx_t(.now, 20)
  rel <- .all_files("CD_M1"); rel$player_stats <- character()
  d <- .release_decision(fx, 23, rel, now = .now)
  expect_true(d$trigger)
  expect_identical(d$reason, "gap")
  expect_identical(d$gaps, "CD_M1")
})

test_that("a match that never gets chains stops triggering after 48 h and is reported", {
  # CD_M20240142309 (2024 R23): CONCLUDED, 0 pbp rows, so never in xg or
  # player_game_ratings. It must not re-release the last round forever.
  fx <- .fx_t(.now, c(47, 49))
  fx$match_id <- c("CD_M_OK", "CD_M20240142309")
  rel <- .all_files("CD_M_OK")
  rel$player_stats <- c("CD_M_OK", "CD_M20240142309")
  rel$score_events <- c("CD_M_OK", "CD_M20240142309")
  d <- .release_decision(fx, 23, rel, now = .now)
  expect_false(d$trigger)
  expect_setequal(d$abandoned$file, c("xg", "player_game_ratings"))
  expect_true(all(d$abandoned$match_id == "CD_M20240142309"))
  # At 47 h after it ended, the same gap still triggered.
  d47 <- .release_decision(fx, 23, rel, now = .now - 2 * 3600)
  expect_true(d47$trigger)
  expect_identical(d47$gaps, "CD_M20240142309")
})

test_that("no concluded matches: no trigger and nothing to read; NA kickoff is treated as old", {
  fx <- .fx_t(.now, 1); fx$status <- "SCHEDULED"
  d <- .release_decision(fx, 23, NULL, now = .now)
  expect_false(d$trigger); expect_false(d$needs_files)
  fx2 <- .fx_t(.now, 1); fx2$utc_start_time <- NA_character_
  d2 <- .release_decision(fx2, 23, list(xg = character()), now = .now)
  expect_false(d2$trigger)
  expect_equal(nrow(d2$abandoned), 1)
})
