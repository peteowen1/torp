# player_epv_breakdown() — the parts must add to the whole.
#
# The point of this function is that a profile page can show where a player's
# value comes from WITHOUT the categories disagreeing with the rating printed
# beside them. Every test here defends that property; none of them check the
# numbers are "reasonable", because reasonable is not the claim.

.fake_pg <- function() {
  data.table::data.table(
    player_id = c("p1", "p2"), match_id = c("m1", "m1"),
    season = 2026L, round = 1L, player_name = c("A", "B"),
    position_group = c("MIDFIELDER", "RUCK"), team = "T", tog = c(0.8, 0.7),
    epv_recv = 0, epv_disp = 0, epv_spoil = 0, epv_hitout = 0, epv = 0
  )
}

.fake_ps <- function() {
  stats <- unlist(lapply(EPV_BOX_TERMS, names), use.names = FALSE)
  d <- data.table::data.table(player_id = c("p1", "p2"), match_id = c("m1", "m1"))
  for (s in stats) data.table::set(d, j = s, value = c(2, 3))
  d[]
}

# Build a player_game frame whose channels are exactly the weighted box terms,
# so the residual is zero by construction and any gap is a real defect.
.consistent_pair <- function() {
  p <- default_epv_params()
  ps <- .fake_ps()
  pg <- .fake_pg()
  for (ch in names(EPV_BOX_TERMS)) {
    terms <- EPV_BOX_TERMS[[ch]]
    tot <- rowSums(vapply(names(terms), function(s) ps[[s]] * p[[terms[[s]]]],
                          numeric(nrow(ps))))
    data.table::set(pg, j = paste0("epv_", ch), value = tot)
  }
  pg[, epv := epv_recv + epv_disp + epv_spoil + epv_hitout]
  list(pg = pg, ps = ps)
}

test_that("the breakdown reproduces published epv exactly", {
  x <- .consistent_pair()
  b <- player_epv_breakdown(player_game = x$pg, player_stats = x$ps)
  agg <- b[, .(rebuilt = sum(epv)), by = .(player_id, match_id)]
  cmp <- merge(x$pg[, .(player_id, match_id, epv)], agg,
               by = c("player_id", "match_id"))
  expect_equal(cmp$rebuilt, cmp$epv)
})

test_that("the residual absorbs whatever the box terms do not explain", {
  x <- .consistent_pair()
  # Push 5 points of unexplained value into epv_disp. It must surface as the
  # disp `chain` residual, not vanish and not break the total.
  x$pg[player_id == "p1", `:=`(epv_disp = epv_disp + 5, epv = epv + 5)]
  b <- player_epv_breakdown(player_game = x$pg, player_stats = x$ps)
  resid <- b[player_id == "p1" & channel == "disp" & category == "chain"]$epv
  expect_equal(resid, 5)
  expect_equal(sum(b[player_id == "p1"]$epv), x$pg[player_id == "p1"]$epv)
})

test_that("verify aborts rather than returning a breakdown that does not add up", {
  x <- .consistent_pair()
  # Corrupt the total WITHOUT touching a channel, so the parts genuinely cannot
  # reconstruct it. This is the failure the gate exists for.
  x$pg[player_id == "p1", epv := epv + 3]
  expect_error(
    player_epv_breakdown(player_game = x$pg, player_stats = x$ps),
    "does not reproduce published"
  )
  # ...and that the same input passes when verification is off, proving the
  # error comes from the gate rather than from the decomposition failing.
  expect_no_error(
    player_epv_breakdown(player_game = x$pg, player_stats = x$ps, verify = FALSE)
  )
})

test_that("every box term maps to a real weight", {
  p <- default_epv_params()
  for (ch in names(EPV_BOX_TERMS)) {
    for (s in names(EPV_BOX_TERMS[[ch]])) {
      w <- EPV_BOX_TERMS[[ch]][[s]]
      expect_true(w %in% names(p), info = paste(ch, s, "->", w))
      expect_true(is.numeric(p[[w]]), info = w)
    }
  }
})

test_that("a missing box-score column warns instead of silently shrinking a category", {
  x <- .consistent_pair()
  x$ps[, tackles := NULL]
  expect_warning(
    player_epv_breakdown(player_game = x$pg, player_stats = x$ps, verify = FALSE),
    "tackles"
  )
})

test_that("share is computed over ABSOLUTE epv, so a near-zero total cannot explode it", {
  x <- .consistent_pair()
  b <- player_epv_breakdown(player_game = x$pg, player_stats = x$ps)
  # Shares are bounded by construction when the denominator is sum(|epv|).
  expect_true(all(abs(b$share) <= 1 + 1e-9, na.rm = TRUE))
  # And the absolute shares sum to 1 per player-game.
  s <- b[, .(tot = sum(abs(share))), by = .(player_id, match_id)]
  expect_equal(s$tot, rep(1, nrow(s)))
})

test_that("an all-zero player-game gives NA share rather than NaN", {
  # 0/0 is NaN in R, and a NaN reaching a page renders as literal "NaN". This is
  # reachable: a player who came on late with no recorded stat and no chain value.
  ps <- .fake_ps()
  for (s in unlist(lapply(EPV_BOX_TERMS, names), use.names = FALSE)) {
    data.table::set(ps, j = s, value = c(0, 0))
  }
  pg <- .fake_pg()   # channels and epv already all zero
  b <- player_epv_breakdown(player_game = pg, player_stats = ps)
  expect_true(all(is.na(b$share)))
  expect_false(any(is.nan(b$share)))
  # The gate must still pass -- zero really does reconstruct zero.
  expect_equal(sum(b$epv), 0)
})

test_that("EPV_BOX_TERMS covers the 29 documented box-score categories", {
  # A guard on the lockstep requirement: if someone adds a term to
  # create_player_game_data()'s mutate and here, this count moves and the change
  # is deliberate. If they add it only there, the residual hides it -- which is
  # why the doc says these two checks are not interchangeable.
  expect_equal(length(unlist(lapply(EPV_BOX_TERMS, names))), 29L)
  expect_identical(names(EPV_BOX_TERMS), c("disp", "recv", "spoil", "hitout"))
})
