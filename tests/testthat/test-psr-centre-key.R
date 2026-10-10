# Tests for PSR's centring keys (Thread G, 2026-10-11): calculate_psr(psr_centre =),
# .attach_season_listing() (published), .attach_weekly_pos() (scoring only).

.wk_coef <- data.frame(stat_name = "kicks", beta = 1, stringsAsFactors = FALSE)

# Four players in one round. Career labels say two forwards and two defenders;
# the team sheet that round names P2 (a career forward) in defence.
.wk_skills <- function() {
  data.table::data.table(
    player_id = c("P1", "P2", "P3", "P4"),
    player_name = c("A", "B", "C", "D"),
    season = 2026L, round = 5L,
    pos_group = c("KEY_FORWARD", "KEY_FORWARD", "KEY_DEFENDER", "KEY_DEFENDER"),
    lineup_pos_group = c("KEY_FORWARD", "KEY_DEFENDER", "KEY_DEFENDER", "KEY_FORWARD"),
    kicks_rating = c(10, 4, 6, 8)
  )
}

test_that("career centring ignores a weekly column in the frame", {
  sk <- .wk_skills()
  with_col <- calculate_psr(sk, .wk_coef, psr_centre = "career")
  without  <- calculate_psr(sk[, !"lineup_pos_group"], .wk_coef, psr_centre = "career")
  expect_equal(with_col$psr, without$psr)
  # and it really is the career key: each career group averages zero
  expect_equal(as.numeric(tapply(with_col$psr, sk$pos_group, mean)), c(0, 0))
})

test_that("weekly centring compares each player with his named slot", {
  sk <- .wk_skills()
  wk <- calculate_psr(sk, .wk_coef, psr_centre = "weekly")
  expect_equal(as.numeric(tapply(wk$psr, sk$lineup_pos_group, mean)), c(0, 0))
  # the two keys give different answers for this frame, or the test proves nothing
  car <- calculate_psr(sk, .wk_coef, psr_centre = "career")
  expect_false(isTRUE(all.equal(wk$psr, car$psr)))
})

test_that("weekly centring without the column aborts instead of falling back", {
  sk <- .wk_skills()[, !"lineup_pos_group"]
  expect_error(calculate_psr(sk, .wk_coef, psr_centre = "weekly"), "lineup_pos_group")
})

test_that("calculate_psr defaults to the career key", {
  sk <- .wk_skills()
  expect_equal(calculate_psr(sk, .wk_coef)$psr,
               calculate_psr(sk, .wk_coef, psr_centre = "career")$psr)
})

test_that(".attach_weekly_pos takes that round's slot, else the latest earlier one", {
  skills <- data.table::data.table(
    player_id = c("P1", "P1", "P1", "P2", "P3"),
    season    = c(2026L, 2026L, 2026L, 2026L, 2026L),
    round     = c(1L, 2L, 3L, 1L, 1L),
    pos_group = c("KEY_FORWARD", "KEY_FORWARD", "KEY_FORWARD", "MIDFIELDER", "RUCK")
  )
  lineup <- data.table::data.table(
    player_id = c("P1", "P1", "P2"),
    season    = c(2025L, 2026L, 2026L),
    round     = c(20L, 2L, 1L),
    lineup_position = c("FB", "INT", "CHF")
  )
  out <- suppressMessages(.attach_weekly_pos(skills, lineup))
  # P1 r1: not named in 2026 yet -> last year's FB. r2: on the bench (INT is
  # skipped) -> still FB. r3: not named -> FB. P2 r1: named CHF that round.
  # P3: never named -> career pos_group.
  expect_equal(out$lineup_pos_group,
               c("KEY_DEFENDER", "KEY_DEFENDER", "KEY_DEFENDER", "KEY_FORWARD", "RUCK"))
})

test_that(".attach_weekly_pos never reads a LATER round", {
  # P2 matches so the no-match guard stays quiet; P1's only slot is round 4.
  skills <- data.table::data.table(player_id = c("P1", "P2"), season = 2026L, round = 3L,
                                   pos_group = c("MIDFIELDER", "RUCK"))
  lineup <- data.table::data.table(player_id = c("P1", "P2"), season = 2026L,
                                   round = c(4L, 3L), lineup_position = c("FF", "RK"))
  out <- suppressMessages(.attach_weekly_pos(skills, lineup))
  expect_equal(out$lineup_pos_group, c("MIDFIELDER", "RUCK"))
})

test_that(".attach_weekly_pos refuses a team sheet with no on-field slots", {
  skills <- data.table::data.table(player_id = "P1", season = 2026L, round = 1L,
                                   pos_group = "MIDFIELDER")
  lineup <- data.table::data.table(player_id = "P1", season = 2026L, round = 1L,
                                   lineup_position = "INT")
  expect_error(.attach_weekly_pos(skills, lineup), "No named on-field slots")
})

test_that(".attach_weekly_pos refuses a join that matches nothing", {
  skills <- data.table::data.table(player_id = "P1", season = 2026L, round = 1L,
                                   pos_group = "MIDFIELDER")
  lineup <- data.table::data.table(player_id = "OTHER", season = 2026L, round = 1L,
                                   lineup_position = "FF")
  expect_error(suppressMessages(.attach_weekly_pos(skills, lineup)), "matched no team sheet")
})

test_that("the published centring key is visible to the vintage drift guard", {
  expect_identical(PSR_PUBLISHED_CENTRE, "listed")
  expect_identical(.rating_defining_constants()$PSR_PUBLISHED_CENTRE, "listed")
})

test_that("listed centring compares each player with that season's listing", {
  sk <- .wk_skills()[, listed_pos_group := c("key_fwd", "key_def", "key_def", "key_fwd")]
  ls <- calculate_psr(sk, .wk_coef, psr_centre = "listed")
  expect_equal(as.numeric(tapply(ls$psr, sk$listed_pos_group, mean)), c(0, 0))
  expect_error(calculate_psr(sk[, !"listed_pos_group"], .wk_coef, psr_centre = "listed"),
               "listed_pos_group")
})

test_that(".attach_season_listing uses that season, else an earlier one, else career", {
  skills <- data.table::data.table(
    player_id = c("P1", "P1", "P2", "P3"),
    season    = c(2022L, 2023L, 2023L, 2023L),
    round     = 1L,
    pos_group = c("KEY_FORWARD", "KEY_FORWARD", "MIDFIELDER", "RUCK")
  )
  listing <- data.table::data.table(
    player_id = c("P1", "P1", "P2"),
    season    = c(2022L, 2024L, 2023L),
    position  = c("KEY_FORWARD", "KEY_DEFENDER", "MIDFIELDER_FORWARD")
  )
  out <- suppressMessages(.attach_season_listing(skills, listing))
  # P1 2022: listed key forward that season. P1 2023: no 2023 listing, so his
  # 2022 one -- never the LATER 2024 key-defender listing. P2: MIDFIELDER_FORWARD
  # collapses to med_fwd, as torp_ratings presents it. P3: unlisted -> career.
  expect_equal(out$listed_pos_group, c("key_fwd", "key_fwd", "med_fwd", "rucks"))
})

test_that(".attach_season_listing refuses a mostly-unmatched join", {
  skills <- data.table::data.table(player_id = c("P1", "P2", "P3"), season = 2023L,
                                   pos_group = "MIDFIELDER")
  listing <- data.table::data.table(player_id = "P1", season = 2023L, position = "RUCK")
  expect_error(suppressMessages(.attach_season_listing(skills, listing)), "matched a listing")
})
