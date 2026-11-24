# Positional mirror arm (NP_POOL_SPREAD = "zone"): docs/plans/POSITIONAL-MIRROR-ARM.md

zone_lineup <- function() {
  data.table::data.table(
    match_id = "m1", team = "Def",
    player_id = c("kb1", "kb2", "bp1", "mid1", "int1"),
    position  = c("FB", "CHB", "BPL", "C", "INT"),
    tog = c(1, 0.5, 1, 0.8, 0.4), def_acts = 1,
    hitouts_to_advantage = 0, ruck_contests = 0, hitouts = 0)
}

zone_mix_toy <- function() {
  data.table::data.table(
    zone_key = "D:key_fwd",
    def_zone = c("key_back", "back_pocket", "mid", "bench", "half_back"),
    share    = c(0.40, 0.20, 0.10, 0.10, 0.20), n_src = 100)
}

test_that("a named contest defender takes the whole pool remainder", {
  pool <- data.table::data.table(match_id = "m1", def_team = "Def", winner_slot = "FB",
                                 loser_pid = "f1", zone_key = "P:bp1", pool_hm = 2)
  out <- .np_spread_pool(pool, zone_lineup(), "zone", 0.5, zone_mix = zone_mix_toy())
  expect_equal(out[player_id == "bp1", np_defensive], 2)
  expect_equal(sum(out$np_defensive), 2)
})

test_that("a zone-keyed pool splits by the mix, then by TOG within a zone; empty zones renormalise", {
  pool <- data.table::data.table(match_id = "m1", def_team = "Def", winner_slot = "FB",
                                 loser_pid = "f1", zone_key = "D:key_fwd", pool_hm = 1)
  out <- .np_spread_pool(pool, zone_lineup(), "zone", 0.5, zone_mix = zone_mix_toy())
  # half_back (0.20) has nobody on the ground: the other 0.80 renormalises to 1
  present <- 0.40 + 0.20 + 0.10 + 0.10
  get <- function(p) out[player_id == p, np_defensive]
  expect_equal(get("kb1"), 0.40 / present * 1 / 1.5)
  expect_equal(get("kb2"), 0.40 / present * 0.5 / 1.5)
  expect_equal(get("bp1"), 0.20 / present)
  expect_equal(get("mid1"), 0.10 / present)
  expect_equal(get("int1"), 0.10 / present)          # bench is paid as its own zone
  expect_equal(sum(out$np_defensive), 1)
})

test_that("an unkeyed pool (stoppage, offence) spreads by TOG exactly as matchup does with no slot", {
  pool <- data.table::data.table(match_id = "m1", def_team = "Def", winner_slot = NA_character_,
                                 loser_pid = NA_character_, pool_hm = 3)
  z <- .np_spread_pool(data.table::copy(pool), zone_lineup(), "zone", 0.5, zone_mix = zone_mix_toy())
  m <- .np_spread_pool(data.table::copy(pool), zone_lineup(), "matchup", 0.5)
  data.table::setkey(z, player_id); data.table::setkey(m, player_id)
  expect_equal(z$np_defensive, m$np_defensive)
})

test_that("an unseen source zone falls back to TOG and says so", {
  pool <- data.table::data.table(match_id = "m1", def_team = "Def", winner_slot = "FB",
                                 loser_pid = "f1", zone_key = "D:ruck", pool_hm = 1)
  expect_message(
    out <- .np_spread_pool(pool, zone_lineup(), "zone", 0.5, zone_mix = zone_mix_toy()),
    "no zone weight")
  expect_equal(out[player_id == "kb1", np_defensive], 1 / sum(zone_lineup()$tog))
})

test_that("the zone mix is shrunk toward the overall mix and each key sums to one", {
  pbp <- data.table::data.table(
    match_id = "m1", display_order = 1:8,
    description = c("Kick", "Mark", "Kick", "Mark", "Handball", "Gather", "Kick", "Spoil"),
    team_id = c("A", "B", "A", "B", "A", "B", "A", "B"),
    player_id = c("f1", "kb1", "f1", "kb1", "m1", "mid1", "f2", "bp1"),
    contest_target_id = c(rep(NA, 6), "f2", NA),
    contest_defender_id = c(rep(NA, 6), "bp1", NA))
  ps <- data.table::data.table(match_id = "m1",
                               player_id = c("f1", "f2", "m1", "kb1", "bp1", "mid1"),
                               position = c("FF", "CHF", "C", "FB", "BPL", "C"))
  mix <- suppressWarnings(suppressMessages(.np_fit_zone_mix(pbp, chains = NULL, player_stats = ps, shrink_n = 2)))
  expect_true(all(abs(mix[, sum(share), by = zone_key]$V1 - 1) < 1e-12))
  # D:key_fwd saw 2 events, both won by a key back; overall D mix is 2/3 key_back, 1/3 mid
  d <- mix[zone_key == "D:key_fwd"]
  expect_equal(d[def_zone == "key_back", share], (2 + 2 * 2/3) / (2 + 2))
  expect_equal(d[def_zone == "mid", share], (0 + 2 * 1/3) / (2 + 2))
  # the named contest is a T event and is not counted again as a plain turnover
  expect_equal(mix[zone_key == "T:key_fwd" & def_zone == "back_pocket", n_src], 1L)
  expect_false(any(grepl("^D:", mix$zone_key) & mix$n_src > 2))
})

test_that("team-margin zone credit: named defender takes his row, plain rows split by zone, unkeyed rows are returned", {
  prow <- data.table::data.table(
    match_id = "m1", display_order = c(10L, 20L, 30L), team = "Def",
    pool_row = c(2, 1, 0.5), dcred = TRUE)
  p <- data.table::data.table(match_id = "m1", display_order = c(10L, 20L, 30L),
                              player_id = c("f1", "f2", "zz"))
  lu <- zone_lineup()[, .(match_id, team, player_id, tog, position)]
  lu <- rbind(lu, data.table::data.table(match_id = "m1", team = "Att",
                                         player_id = c("f1", "f2"), tog = 1, position = c("FF", "CHF")))
  zk <- data.table::data.table(match_id = "m1", display_order = 10L, dfd = "bp1", tgt = "f1")
  res <- suppressMessages(.np_tm_zone_credit(prow, p, lu, zk, zone_mix_toy()))
  k <- res$keyed
  # row 10: named defender bp1 takes 2; row 20: D:key_fwd (f2 is CHF) splits 1 by the toy mix
  present <- 0.40 + 0.20 + 0.10 + 0.10
  expect_equal(k[player_id == "bp1", zalloc], 2 + 0.20 / present)
  expect_equal(k[player_id == "kb1", zalloc], 0.40 / present / 1.5)
  expect_equal(sum(k$zalloc), 3)
  # row 30: the actor has no slot, so it is unkeyed and goes back for the TOG spread
  expect_equal(res$unkeyed$pool_du, 0.5)
})
