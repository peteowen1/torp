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

test_that("a frame without position_group warns and returns unchanged", {
  d <- .epr_frame(); d$position_group <- NULL
  expect_warning(out <- torp:::centre_epr_by_position(d), "position_group")
  expect_equal(out$epr_recv, d$epr_recv)
})

test_that("a missing TOG column falls back to equal weights rather than failing", {
  d <- .epr_frame(); d$pred_tog <- NULL
  out <- torp:::centre_epr_by_position(d)
  s <- out[out$round == 1 & out$position_group == "MIDFIELDER", ]
  expect_equal(mean(s$epr_recv), 0, tolerance = 1e-10)
})
