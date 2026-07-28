# Missing-lineup warning on locked predictions.
#
# Motivation: rounds 19, 20 and 21 of 2026 were all locked with `players = NA`
# -- no AFL team lists published yet, so every player's EPR fell back to the
# position prior -- while rounds 13-18 carried 23. Nothing reported it. The
# damage (mean 8.73 points/game disagreement vs a correctly-fed model, ~3.2 MAE)
# was found only by a paired audit against Squiggle weeks later.
#
# The guard warns rather than aborts: a prior-based prediction still beats no
# prediction against a tipping deadline, and the started-game lock lets a later
# run replace any match that has not started. Silence was the bug, not publication.

.lineup_frame <- function(players, week = 21) {
  # rep() rather than relying on recycling: data.frame() will not recycle a
  # length-1 column against a zero-length one, which the empty-frame case needs.
  n <- length(players)
  data.frame(
    week = rep(week, n),
    # NOT paste0("M", seq_len(n)) -- paste0 treats a zero-length argument as ""
    # rather than propagating emptiness, so n = 0 yields "M" (length 1) and
    # data.frame() then rejects the mismatched columns.
    match_id = if (n == 0) character(0) else paste0("M", seq_len(n)),
    players = players,
    pred_margin = rep(10, n),
    stringsAsFactors = FALSE
  )
}

test_that("a full team list is not flagged", {
  expect_silent(n <- torp:::.warn_missing_lineups(.lineup_frame(c(23, 23, 23))))
  expect_equal(n, 0L)
})

test_that("a missing team list is flagged, naming the round", {
  df <- .lineup_frame(c(NA, NA), week = 21)
  expect_warning(n <- torp:::.warn_missing_lineups(df), "NO team list")
  expect_warning(torp:::.warn_missing_lineups(df), "21")
  expect_equal(suppressWarnings(torp:::.warn_missing_lineups(df)), 2L)
})

test_that("a partially-named round reports only the missing rows", {
  # The real shape when a round's Thursday game is named but the weekend is not.
  df <- .lineup_frame(c(23, NA, 23, NA))
  expect_equal(suppressWarnings(torp:::.warn_missing_lineups(df)), 2L)
})

test_that("multiple affected rounds are all named", {
  df <- rbind(.lineup_frame(NA, week = 19), .lineup_frame(NA, week = 20))
  w <- tryCatch(torp:::.warn_missing_lineups(df), warning = function(e) conditionMessage(e))
  expect_match(w, "19")
  expect_match(w, "20")
})

test_that("a partially-published team sheet is flagged, not just an absent one", {
  # The gap a NA-only check misses: `players` is a COUNT, so ins/outs published
  # ahead of the full side give a small non-NA number. Same degradation (most of
  # the side on priors), but is.na() is FALSE.
  df <- .lineup_frame(c(23, 4))
  expect_warning(n <- torp:::.warn_missing_lineups(df), "PART of a team list")
  expect_equal(suppressWarnings(torp:::.warn_missing_lineups(df)), 1L)
})

test_that("a full sheet just under the nominal 23 is NOT flagged", {
  # Guard against crying wolf: the threshold sits below a real named side so a
  # late withdrawal before the sheet is finalised stays quiet.
  expect_silent(torp:::.warn_missing_lineups(.lineup_frame(c(22, 21, 20))))
})

test_that("missing and partial sheets are reported separately and both counted", {
  df <- .lineup_frame(c(NA, 3, 23))
  expect_equal(suppressWarnings(torp:::.warn_missing_lineups(df)), 2L)
})

test_that("a frame with no players column at all reports that it skipped", {
  # Must not error inside a release path -- but must not be silent either. A
  # monitoring function that stops checking without saying so rebuilds the exact
  # blind spot it was written to close.
  df <- data.frame(week = 21, match_id = "M1", pred_margin = 10,
                   stringsAsFactors = FALSE)
  expect_message(n <- torp:::.warn_missing_lineups(df), "no .*players.* column")
  expect_equal(suppressMessages(torp:::.warn_missing_lineups(df)), 0L)
})

test_that("an empty frame is a no-op", {
  df <- .lineup_frame(numeric(0))
  expect_silent(n <- torp:::.warn_missing_lineups(df))
  expect_equal(n, 0L)
})
