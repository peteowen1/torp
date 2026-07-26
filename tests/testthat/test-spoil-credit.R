# Tests for compute_spoil_credit() — contextual (WS2a) spoil pricing
# ==================================================================

# Mock chains covering every branch the function has to get right. Lineages
# mirror what 2024 chains actually contain (see the lag distribution in
# docs/plans/FABLE-DEFENDER-VALUE-PLAN.md §6.2).
#
#   1-2   Kick(T1) -> Spoil(T2)
#         plain-Kick lineage at lag 1 — the single biggest group (6,577 of
#         13,124) that compute_contest_credit() misses entirely.  PRICED
#   3-5   Kick(T1) -> Contest Target(T1) -> Spoil(T2) at identical x,y
#         already priced by compute_contest_credit().  EXCLUDED
#   6-7   Kick(T1) -> Spoil(T1)
#         spoiler logged on the kicking team, a chain artifact.  DROPPED
#   8-10  Handball(T1) -> Handball Received(T1) -> Spoil(T2)
#         a possession intervenes, so no kick is reachable.  DROPPED
#   11-14 Kick(T1) -> Kick Into F50 -> Kick Inside 50 Result -> Spoil(T2)
#         all-annotation lineage at lag 3 (4,344 cases).  PRICED
create_spoil_chains <- function() {
  data.table::data.table(
    match_id = rep("M001", 14),
    display_order = 1:14,
    period = rep(1L, 14),
    period_seconds = seq(100L, 1400L, by = 100L),
    team_id = c(
      "T1", "T2",
      "T1", "T1", "T2",
      "T1", "T1",
      "T1", "T1", "T2",
      "T1", "T1", "T1", "T2"
    ),
    player_id = paste0("P", sprintf("%03d", 1:14)),
    description = c(
      "Kick", "Spoil",
      "Kick", "Contest Target", "Spoil",
      "Kick", "Spoil",
      "Handball", "Handball Received", "Spoil",
      "Kick", "Kick Into F50", "Kick Inside 50 Result", "Spoil"
    ),
    # Rows 4-5 share coordinates (that is what marks them as a contest
    # triple). Rows 13-14 deliberately do NOT, so they exercise the
    # in-flight scan rather than the exclusion path.
    x = c(50L, 55L,
          30L, 30L, 30L,
          20L, 25L,
          10L, 12L, 14L,
          60L, 70L, 80L, 85L),
    y = c(-5L, 0L,
          10L, 10L, 10L,
          15L, 15L,
          0L, 0L, 0L,
          2L, 2L, 2L, 4L)
  )
}

create_spoil_pbp <- function() {
  data.table::data.table(
    match_id = rep("M001", 14),
    display_order = 1:14,
    delta_epv = c(
      -0.9, NA,        # row 1 kick — spoiled, bad for the kicking team
      0.5, NA, NA,     # row 3 kick
      0.8, NA,         # row 6 kick
      NA, NA, NA,
      -0.6, NA, NA, NA # row 11 kick
    )
  )
}


test_that("compute_spoil_credit returns correct schema on empty input", {
  empty_chains <- create_spoil_chains()[0]
  result <- compute_spoil_credit(empty_chains, create_spoil_pbp()[0])

  expect_s3_class(result, "data.table")
  expect_true(all(c("player_id", "match_id", "spoil_epv_ctx", "spoils_priced")
                  %in% names(result)))
  expect_equal(nrow(result), 0)
})

test_that("compute_spoil_credit prices a plain-Kick spoil the contest path misses", {
  result <- compute_spoil_credit(create_spoil_chains(), create_spoil_pbp())

  # P002 spoiled the row-1 kick (delta_epv = -0.9) from the opposing team
  p2 <- result[player_id == "P002"]
  expect_equal(nrow(p2), 1L)
  # credit = -delta_epv * share = -(-0.9) / 3 = +0.3
  expect_equal(p2$spoil_epv_ctx, 0.3)
  expect_equal(p2$spoils_priced, 1L)
})

test_that("compute_spoil_credit excludes spoils already priced as contest triples", {
  result <- compute_spoil_credit(create_spoil_chains(), create_spoil_pbp())

  # P005's spoil follows a Contest Target at identical x,y from the opposing
  # team — compute_contest_credit() already credits it. Double counting here
  # would silently inflate every aerial defender.
  expect_equal(nrow(result[player_id == "P005"]), 0L)
})

test_that("compute_spoil_credit drops same-team spoils", {
  result <- compute_spoil_credit(create_spoil_chains(), create_spoil_pbp())

  # P007 is logged on T1, the same team that kicked — a chain artifact, not a
  # genuine defensive act.
  expect_equal(nrow(result[player_id == "P007"]), 0L)
})

test_that("compute_spoil_credit will not scan back across a possession", {
  # Rows 8-10 are Handball -> Handball Received -> Spoil. A naive 5-row
  # lookback reaches the row-6 kick and credits this spoil against a kick it
  # never touched. Only in-flight annotation rows may sit between.
  result <- compute_spoil_credit(create_spoil_chains(), create_spoil_pbp())

  expect_equal(nrow(result[player_id == "P010"]), 0L)
})

test_that("compute_spoil_credit scans through in-flight annotation rows", {
  # Kick -> Kick Into F50 -> Kick Inside 50 Result -> Spoil is the single most
  # common real lineage (4,344 of 13,124 spoils in 2024). All three
  # intervening rows are annotations, so the kick must still be found.
  chains <- data.table::data.table(
    match_id = "M001", display_order = 1:4, period = 1L,
    period_seconds = c(100L, 200L, 300L, 400L),
    team_id = c("T1", "T1", "T1", "T2"),
    player_id = paste0("P", sprintf("%03d", 1:4)),
    description = c("Kick", "Kick Into F50", "Kick Inside 50 Result", "Spoil"),
    # The spoil must NOT share coordinates with the "Kick Inside 50 Result"
    # row: that combination is a contest triple compute_contest_credit()
    # already prices, and the exclusion path would (correctly) drop it before
    # the in-flight scan ever runs.
    x = c(10L, 20L, 30L, 35L), y = c(5L, 5L, 5L, 7L)
  )
  pbp <- data.table::data.table(
    match_id = "M001", display_order = 1:4,
    delta_epv = c(-0.6, NA, NA, NA)
  )

  result <- compute_spoil_credit(chains, pbp)
  expect_equal(nrow(result), 1L)
  expect_equal(result$player_id, "P004")
  expect_equal(result$spoil_epv_ctx, 0.2)
})

test_that("compute_spoil_credit uses the same sign convention as contest credit", {
  # A spoil on a kick that was GOOD for the attacking team earns negative
  # credit. This is the discrimination a flat per-spoil weight cannot express,
  # and it must match compute_contest_credit()'s defender convention exactly.
  chains <- create_spoil_chains()
  pbp <- create_spoil_pbp()
  # Row 1's kick now GAINED expected points despite being spoiled.
  pbp[display_order == 1L, delta_epv := 0.6]

  result <- compute_spoil_credit(chains, pbp)
  expect_equal(result[player_id == "P002"]$spoil_epv_ctx, -0.2)

  defender <- compute_contest_credit(chains, pbp)
  # P005 defended the row-3 kick (+0.5): -0.5/3 under the same convention.
  expect_equal(defender[player_id == "P005"]$contest_epv, -0.5 / 3)
})

test_that("compute_spoil_credit honours contest_share", {
  result <- compute_spoil_credit(create_spoil_chains(), create_spoil_pbp(),
                                 contest_share = 0.5)
  # -(-0.9) * 0.5
  expect_equal(result[player_id == "P002"]$spoil_epv_ctx, 0.45)
})

test_that("compute_spoil_credit aggregates multiple spoils per player-match", {
  chains <- create_spoil_chains()
  pbp <- create_spoil_pbp()
  # Give P002 a second spoil off another plain kick, past the fixture's end.
  chains <- rbind(chains, data.table::data.table(
    match_id = "M001", display_order = 15:16, period = 1L,
    period_seconds = c(1500L, 1600L), team_id = c("T1", "T2"),
    player_id = c("P015", "P002"), description = c("Kick", "Spoil"),
    x = c(40L, 45L), y = c(2L, 4L)
  ))
  pbp <- rbind(pbp, data.table::data.table(
    match_id = "M001", display_order = 15:16, delta_epv = c(-0.3, NA)
  ))

  result <- compute_spoil_credit(chains, pbp)
  p2 <- result[player_id == "P002"]
  expect_equal(p2$spoils_priced, 2L)
  expect_equal(p2$spoil_epv_ctx, 0.3 + 0.1)
})

test_that("compute_spoil_credit does not cross match boundaries", {
  chains <- data.table::data.table(
    match_id = c(rep("M001", 2), rep("M002", 2)),
    display_order = c(1:2, 1:2),
    period = 1L, period_seconds = c(100L, 200L, 100L, 200L),
    team_id = c("T1", "T1", "T2", "T2"),
    player_id = paste0("P", sprintf("%03d", 1:4)),
    # M002 opens with a Spoil; the kick at the end of M001 must not reach it.
    description = c("Kick", "Handball", "Spoil", "Handball"),
    x = c(10L, 12L, 20L, 22L), y = c(5L, 5L, 8L, 8L)
  )
  pbp <- data.table::data.table(
    match_id = c(rep("M001", 2), rep("M002", 2)),
    display_order = c(1:2, 1:2),
    delta_epv = c(0.7, NA, NA, NA)
  )

  result <- compute_spoil_credit(chains, pbp)
  expect_equal(nrow(result[player_id == "P003"]), 0L)
})

test_that("compute_spoil_credit leaves no temp columns on the input", {
  chains <- create_spoil_chains()
  compute_spoil_credit(chains, create_spoil_pbp())
  expect_length(grep("^\\.", names(chains), value = TRUE), 0)
})
