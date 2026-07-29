# Position-centring of the published EPR channels.
#
# Motivation: EPV_POSITION_STANDARDISE equalises between-position SPREAD at the
# player-game level, keyed on lineup_position, and it works exactly there (the
# TOG-weighted mean of epv_recv_adj is 0.000 in all 20 lineup positions). But it
# does not survive to the published rating -- the TOG weighting, opponent
# adjustment, decay and global prior that follow are all position-blind -- so on
# the listed taxonomy key defenders still sat at median EPR -2.18 against medium
# forwards' +0.66.
#
# This centres the published rating directly. It is a normalisation, not a
# measurement: position levels are unidentifiable from match margins
# (F(5,1113) = 0.47, p = 0.80).

# EVERY test below asserts the FULL correction -- "each position's TOG-weighted
# mean is exactly zero". Since 2026-07-29 EPR_POSITION_SHRINK is TRUE, which
# deliberately leaves ~10% of the position mean behind, so these would all fail
# on correct code. Pin the whole file to the full correction: the invariant it
# protects is the centring KEY and the arithmetic, not the magnitude. Shrinkage
# has its own test in test-player-ratings.R.
#
# File-scoped via teardown_env() so the namespace is restored even if a test
# errors partway through -- leaving a mutated constant behind would silently
# change every test file sourced after this one.
local({
  .old_shrink <- get("EPR_POSITION_SHRINK", envir = asNamespace("torp"))
  assignInNamespace("EPR_POSITION_SHRINK", FALSE, ns = "torp")
  withr::defer(assignInNamespace("EPR_POSITION_SHRINK", .old_shrink, ns = "torp"),
               envir = testthat::teardown_env())
})

.epr_frame <- function() {
  # Two positions, two rounds, deliberately different means and TOG spreads.
  data.frame(
    player_id = paste0("P", 1:8),
    season = 2026,
    round = rep(c(1L, 2L), each = 4),
    position_group = rep(c("KEY_DEFENDER", "KEY_DEFENDER", "MIDFIELDER", "MIDFIELDER"), 2),
    epr_recv  = c(1, 3, 10, 14,  2, 4, 20, 24),
    epr_disp  = c(-2, -4, 1, 3,  -1, -5, 2, 6),
    epr_spoil = c(0.5, 1.5, 0, 0, 1, 2, 0, 0),
    epr_hitout = c(0, 0, 0, 0, 0, 0, 0, 0),
    pred_tog = c(1, 1, 1, 1, 1, 1, 1, 1),
    stringsAsFactors = FALSE
  )
}

.wmean <- function(x, w) sum(x * w) / sum(w)

test_that("each position's TOG-weighted mean is zero, per channel, per round", {
  out <- torp:::centre_epr_by_position(.epr_frame())
  for (ch in c("epr_recv", "epr_disp", "epr_spoil")) {
    for (r in c(1L, 2L)) {
      for (g in c("KEY_DEFENDER", "MIDFIELDER")) {
        s <- out[out$round == r & out$position_group == g, ]
        expect_equal(.wmean(s[[ch]], s$pred_tog), 0, tolerance = 1e-10)
      }
    }
  }
})

test_that("epr is rebuilt as the sum of its centred channels", {
  out <- torp:::centre_epr_by_position(.epr_frame())
  expect_equal(out$epr,
               out$epr_recv + out$epr_disp + out$epr_spoil + out$epr_hitout,
               tolerance = 1e-10)
})

test_that("centring is per (season, round) and never pools across rounds", {
  # Round 2's midfielders average 22 on recv against round 1's 12. Pooling would
  # leave round means non-zero and leak later rounds into earlier ratings.
  out <- torp:::centre_epr_by_position(.epr_frame())
  r1 <- out[out$round == 1 & out$position_group == "MIDFIELDER", ]
  r2 <- out[out$round == 2 & out$position_group == "MIDFIELDER", ]
  expect_equal(mean(r1$epr_recv), 0, tolerance = 1e-10)
  expect_equal(mean(r2$epr_recv), 0, tolerance = 1e-10)
})

test_that("TOG weighting actually weights", {
  d <- .epr_frame()
  d$pred_tog[d$position_group == "KEY_DEFENDER" & d$round == 1] <- c(0.1, 0.9)
  out <- torp:::centre_epr_by_position(d)
  s <- out[out$round == 1 & out$position_group == "KEY_DEFENDER", ]
  expect_equal(.wmean(s$epr_recv, s$pred_tog), 0, tolerance = 1e-10)
  # The unweighted mean should NOT be zero, else the weights did nothing.
  expect_gt(abs(mean(s$epr_recv)), 1e-6)
})

test_that("relative order within a position is untouched", {
  # Centring subtracts a constant per group, so it must not reorder anyone.
  d <- .epr_frame()
  before <- d$epr_recv[d$round == 1 & d$position_group == "MIDFIELDER"]
  out <- torp:::centre_epr_by_position(d)
  after <- out$epr_recv[out$round == 1 & out$position_group == "MIDFIELDER"]
  expect_equal(rank(before), rank(after))
  expect_equal(diff(before), diff(after), tolerance = 1e-10)
})

test_that("rows with no position group are left alone rather than pooled", {
  d <- .epr_frame()
  d$position_group[1] <- NA_character_
  out <- torp:::centre_epr_by_position(d)
  expect_equal(out$epr_recv[1], d$epr_recv[1])
})

test_that("a frame without position_group ABORTS rather than returning uncentred", {
  # Previously this warned and returned unchanged. The pipeline guard could not
  # catch it either -- with no position_group its own filter leaves zero rows to
  # check, so it passed and shipped uncentred ratings labelled as centred.
  d <- .epr_frame(); d$position_group <- NULL
  expect_error(torp:::centre_epr_by_position(d), "position_group")
})

test_that("a PARTIAL channel set aborts, because it would silently redefine epr", {
  # The dangerous case: epr gets rebuilt from only the channels found, dropping
  # a whole rating dimension for every player while still looking
  # self-consistent (mean-zero per position, summing to itself).
  d <- .epr_frame(); d$epr_hitout <- NULL
  expect_error(torp:::centre_epr_by_position(d), "channel")
})

test_that("a missing TOG column warns loudly before falling back to equal weights", {
  # The fallback is acceptable; doing it silently is not. The function's whole
  # contract is a TOG-WEIGHTED mean.
  d <- .epr_frame(); d$pred_tog <- NULL
  expect_warning(out <- torp:::centre_epr_by_position(d), "UNWEIGHTED")
  s <- out[out$round == 1 & out$position_group == "MIDFIELDER", ]
  expect_equal(mean(s$epr_recv), 0, tolerance = 1e-10)
})

test_that("a row with a missing channel gets NA epr, not a partial sum", {
  # rowSums(na.rm = TRUE) alone would make such a row equal the sum of its
  # remaining channels -- i.e. a player with no data would read as exactly
  # average, the worst possible representation of "unknown".
  d <- .epr_frame()
  d$epr_recv[1] <- NA_real_
  expect_warning(out <- torp:::centre_epr_by_position(d), "non-finite channel")
  expect_true(is.na(out$epr[1]))
  expect_false(any(is.na(out$epr[-1])))
})

test_that("an all-NA position cell does not fabricate values for its members", {
  d <- .epr_frame()
  d$epr_recv[d$round == 1 & d$position_group == "KEY_DEFENDER"] <- NA_real_
  expect_warning(out <- torp:::centre_epr_by_position(d), "non-finite channel")
  aff <- out$round == 1 & out$position_group == "KEY_DEFENDER"
  expect_true(all(is.na(out$epr[aff])))
})

test_that("one NA weight excludes that row, it does not blank the whole position", {
  # weighted.mean()'s na.rm drops NA VALUES, not NA WEIGHTS -- and pmax(NA, 0.01)
  # is NA, so the usual floor does not rescue it. Left unhandled, a single
  # player missing pred_tog turns an entire (season, round, position) cell to NA.
  d <- .epr_frame()
  d$pred_tog[d$round == 1 & d$position_group == "MIDFIELDER"][1] <- NA_real_
  expect_warning(out <- torp:::centre_epr_by_position(d), "no .*pred_tog")
  s <- out[out$round == 1 & out$position_group == "MIDFIELDER", ]
  expect_false(any(is.na(s$epr_recv)))          # the cell survives
  # Centred on the ONE remaining weighted row, so that row sits at 0.
  expect_equal(s$epr_recv[2], 0, tolerance = 1e-10)
})

test_that("a cell with no usable weights at all falls back to an unweighted mean", {
  d <- .epr_frame()
  d$pred_tog[d$round == 1 & d$position_group == "MIDFIELDER"] <- NA_real_
  expect_warning(out <- torp:::centre_epr_by_position(d), "no .*pred_tog")
  s <- out[out$round == 1 & out$position_group == "MIDFIELDER", ]
  expect_false(any(is.na(s$epr_recv)))
  expect_equal(mean(s$epr_recv), 0, tolerance = 1e-10)
})
