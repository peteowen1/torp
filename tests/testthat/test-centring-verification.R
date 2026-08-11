# The publish guards for position centring. Until 2026-08-11 these ran only
# inside data-raw/03-ratings/run_ratings_pipeline.R, so nothing here had ever
# been executed by CI -- roughly 155 lines of data.table logic whose entire job
# is to be right about a subtle invariant, verified only by production not
# complaining.
#
# What these tests care about most is the FAIL-CLOSED behaviour. Both guards
# are written so an empty check aborts rather than passing, because "the guard
# degraded to a no-op" is the same failure shape, one level up, as the bug the
# guard exists to catch.

# --- helpers -----------------------------------------------------------------

# A minimal player-game frame whose channels are already centred per
# (season, round, bucket) on the TOG weight, i.e. what centre_epv_by_position()
# is supposed to leave behind.
.pgd_fixture <- function(centred = TRUE, suffix = "_adj") {
  set.seed(42)
  n <- 24
  d <- data.frame(
    season = 2026L,
    round = rep(1:2, each = n / 2),
    position_group = rep(c("KEY_DEFENDER", "MIDFIELDER", "KEY_FORWARD"), length.out = n),
    time_on_ground_percentage = rep(c(90, 70, 50, 100), length.out = n),
    stringsAsFactors = FALSE
  )
  w <- pmax(d$time_on_ground_percentage / 100, 0.1)
  for (ch in paste0(EPV_LEVEL_CENTRE_CHANNELS, suffix)) {
    v <- stats::rnorm(n)
    if (centred) {
      # Subtract each cell's own TOG-weighted mean, which is exactly the
      # invariant the guard checks.
      key <- paste(d$season, d$round, .collapse_listed_position(d$position_group))
      for (k in unique(key)) {
        i <- key == k
        v[i] <- v[i] - sum(v[i] * w[i]) / sum(w[i])
      }
    }
    d[[ch]] <- v
  }
  d
}

# Ratings frame whose `epr` is already centred per (season, round, bucket) on
# pred_tog -- what centre_epr_by_position() is supposed to leave behind.
.epr_fixture <- function(centred = TRUE) {
  set.seed(7)
  n <- 24
  d <- data.frame(
    season = 2026L,
    round = rep(1:2, each = n / 2),
    position_group = rep(c("KEY_DEFENDER", "MIDFIELDER", "KEY_FORWARD"), length.out = n),
    pred_tog = rep(c(0.9, 0.7, 0.5, 1.0), length.out = n),
    epr = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  if (centred) {
    w <- pmax(d$pred_tog, 0.01)
    key <- paste(d$season, d$round, .collapse_listed_position(d$position_group))
    for (k in unique(key)) {
      i <- key == k
      d$epr[i] <- d$epr[i] - sum(d$epr[i] * w[i]) / sum(w[i])
    }
  }
  d
}

# --- verify_epv_level_centring ----------------------------------------------

test_that("a correctly centred frame passes, and reports how many cells it checked", {
  res <- verify_epv_level_centring(.pgd_fixture(centred = TRUE))
  expect_gt(res$cells, 0)
  expect_lt(res$worst, 1e-8)
})

test_that("an UNcentred frame is caught", {
  # The guard's actual job. Without this the whole file is decoration.
  expect_error(
    verify_epv_level_centring(.pgd_fixture(centred = FALSE)),
    "did not take"
  )
})

test_that("no mapped position bucket aborts rather than passing vacuously", {
  d <- .pgd_fixture()
  d$position_group <- NA_character_
  expect_error(verify_epv_level_centring(d), "no row has a mapped position bucket")
})

test_that("an unrecognised position label aborts rather than being skipped", {
  # .collapse_listed_position() maps unknown labels to NA, so every row drops
  # out of the check. That must abort, not report success over zero rows.
  d <- .pgd_fixture()
  d$position_group <- "WINGER_OF_THE_FUTURE"
  expect_error(
    suppressWarnings(verify_epv_level_centring(d)),
    "no row has a mapped position bucket"
  )
})

test_that("every channel non-finite aborts -- an empty check is not a pass", {
  d <- .pgd_fixture()
  for (ch in paste0(EPV_LEVEL_CENTRE_CHANNELS, "_adj")) d[[ch]] <- NA_real_
  expect_error(verify_epv_level_centring(d), "zero cells had a finite channel value")
})

test_that("shrinkage on with no recorded cells aborts", {
  # A shrunk residual is indistinguishable from centring having failed unless
  # the centring recorded what it left behind.
  expect_error(
    verify_epv_level_centring(.pgd_fixture(), cells = NULL, shrink_on = TRUE),
    "recorded no per-cell corrections"
  )
})

test_that("an explicitly NA recorded residual aborts", {
  d <- .pgd_fixture()
  cells <- data.frame(
    season = 2026L, round = 1:2, pos_bucket = "key_def",
    channel = "epv_recv_adj", resid_expected = c(0, NA_real_),
    stringsAsFactors = FALSE
  )
  expect_error(verify_epv_level_centring(d, cells = cells), "no recorded correction")
})

test_that("a cell MISSING from the recorded corrections is NOT detected (known gap)", {
  # Documents actual behaviour, which is not what the guard's own comment
  # claims ("A cell the centring never recorded is a cell it never
  # corrected").
  #
  # `m[ce, expected := i.resid_expected, on = ...]` is a join-UPDATE: it only
  # writes to rows of `m` that match `ce`. A cell absent from `ce` therefore
  # keeps its `expected = 0` default and never becomes NA, so the anyNA()
  # branch cannot see it. The guard cannot tell "recorded as 0" from "never
  # recorded".
  #
  # Not currently harmful: EPV_POSITION_SHRINK is FALSE, so every true residual
  # is 0 and `expected = 0` is right. With shrinkage ON an unrecorded cell
  # would carry a non-zero residual against an expectation of 0 and would trip
  # the tolerance check instead -- so it still fails, just with a misleading
  # message pointing at centring rather than at the missing record.
  #
  # Left as-is deliberately: this PR moves the guard, it does not re-specify
  # it. Tightening it could make production abort on cells that are currently
  # tolerated, which is a ratings-pipeline decision, not a refactor.
  d <- .pgd_fixture(centred = TRUE)
  cells <- data.frame(
    season = 2026L, round = 1L, pos_bucket = "key_def",
    channel = "epv_recv_adj", resid_expected = 0, stringsAsFactors = FALSE
  )
  res <- verify_epv_level_centring(d, cells = cells)
  expect_gt(res$cells, 0)
})

test_that("_oadj columns are preferred over _adj when both could apply", {
  # Centring _adj while EPR reads _oadj would be a silent no-op, so the guard
  # has to check the same suffix the centring wrote.
  d <- .pgd_fixture(centred = TRUE, suffix = "_oadj")
  expect_silent_res <- verify_epv_level_centring(d)
  expect_gt(expect_silent_res$cells, 0)

  # Uncentred _oadj must still be caught even if _adj columns are absent.
  bad <- .pgd_fixture(centred = FALSE, suffix = "_oadj")
  expect_error(verify_epv_level_centring(bad), "did not take")
})

# --- verify_epr_position_centring -------------------------------------------

test_that("a correctly centred ratings frame passes", {
  res <- verify_epr_position_centring(.epr_fixture(centred = TRUE))
  expect_gt(res$cells, 0)
  expect_lt(res$worst, 1e-6)
})

test_that("an UNcentred ratings frame is caught", {
  expect_error(
    verify_epr_position_centring(.epr_fixture(centred = FALSE)),
    "did not take"
  )
})

test_that("no rated player anywhere aborts rather than passing vacuously", {
  d <- .epr_fixture()
  d$epr <- NA_real_
  expect_error(
    suppressMessages(verify_epr_position_centring(d)),
    "no position bucket has a single rated player"
  )
})

test_that("cells with no rated players are reported, not silently dropped", {
  # "nothing was checkable" must never look like "everything checked out".
  d <- .epr_fixture()
  d$epr[d$round == 1] <- NA_real_
  expect_message(verify_epr_position_centring(d), "no rated players")
})

test_that("grouping uses the collapsed 6-way bucket, not raw position_group", {
  # MEDIUM_FORWARD and MIDFIELDER_FORWARD collapse into one bucket and are only
  # mean-zero JOINTLY. A guard keyed on raw position_group would fail on a
  # frame that is correctly centred -- which is how this gets silently
  # loosened. Centre on the collapsed key, then confirm the guard agrees.
  set.seed(11)
  n <- 12
  d <- data.frame(
    season = 2026L, round = 1L,
    position_group = rep(c("MEDIUM_FORWARD", "MIDFIELDER_FORWARD"), length.out = n),
    pred_tog = rep(c(0.9, 0.6), length.out = n),
    epr = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  w <- pmax(d$pred_tog, 0.01)
  d$epr <- d$epr - sum(d$epr * w) / sum(w)   # centred JOINTLY, one bucket

  expect_silent_res <- verify_epr_position_centring(d)
  expect_equal(expect_silent_res$cells, 1)   # one bucket, not two
  expect_lt(expect_silent_res$worst, 1e-6)
})
