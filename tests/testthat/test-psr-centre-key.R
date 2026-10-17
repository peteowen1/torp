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
    pos_group = c("KEY_FORWARD", "KEY_FORWARD", "MIDFIELDER", "RUCK"),
    # the fallback rows carry no weight, so the per-season floor stays quiet
    wt_80s    = c(1, 0, 1, 0)
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
  expect_error(suppressMessages(.attach_season_listing(skills, listing)), "own season's listing")
})

test_that(".attach_season_listing refuses ONE missing season even when the overall share is high", {
  # Five seasons fully listed, 2026 not at all: 5/6 of rows match overall,
  # which an overall floor would pass. The per-season floor must not.
  skills <- data.table::data.table(
    player_id = rep(c("P1", "P2"), 6),
    season    = rep(2021:2026, each = 2),
    pos_group = "MIDFIELDER",
    wt_80s    = 1
  )
  listing <- data.table::data.table(
    player_id = rep(c("P1", "P2"), 5),
    season    = rep(2021:2025, each = 2),
    position  = "MIDFIELDER"
  )
  expect_error(suppressMessages(.attach_season_listing(skills, listing)), "2026")
})

test_that(".attach_season_listing judges coverage by wt_80s, not row count", {
  # 1 of 3 rows matches, but it carries all the weight: a fringe player with no
  # games and no listing must not block publication.
  skills <- data.table::data.table(player_id = c("P1", "P2", "P3"), season = 2023L,
                                   pos_group = "MIDFIELDER", wt_80s = c(10, 0, 0))
  listing <- data.table::data.table(player_id = "P1", season = 2023L, position = "RUCK")
  out <- suppressMessages(.attach_season_listing(skills, listing))
  expect_equal(out$listed_pos_group, c("rucks", "midfield", "midfield"))
})

test_that(".load_season_listing aborts when a requested season fails to load", {
  local_mocked_bindings(load_player_details = function(seasons, comp = "AFLM", ...) {
    if (seasons == 2026) stop("API down")
    data.frame(player_id = "P1", position = "RUCK")
  })
  expect_error(.load_season_listing(2025:2026), "2026")
  # ...but tolerates the convenience prior season failing
  local_mocked_bindings(load_player_details = function(seasons, comp = "AFLM", ...) {
    if (seasons == 2024) stop("not cached")
    data.frame(player_id = "P1", position = "RUCK")
  })
  out <- .load_season_listing(2025:2026)
  expect_equal(sort(unique(out$season)), c(2025L, 2026L))
})

test_that(".attach_weekly_pos flags a season with no usable team sheet even when other seasons match", {
  # Sheets exist for 2023-2026 only: 2021 and 2022 rows have nothing at or
  # before them. 8 of 12 rows match overall, so the all-rows check passes; the
  # per-season check must name the empty seasons.
  skills <- data.table::data.table(
    player_id = rep(c("P1", "P2"), 6), season = rep(2021:2026, each = 2),
    round = 1L, pos_group = "MIDFIELDER", wt_80s = 1
  )
  lineup <- data.table::data.table(
    player_id = rep(c("P1", "P2"), 4), season = rep(2023:2026, each = 2),
    round = 1L, lineup_position = "C"
  )
  msgs <- gsub("\\s+", " ", paste(testthat::capture_messages(.attach_weekly_pos(skills, lineup)), collapse = " "))
  expect_match(msgs, "too little.*2021 0%, 2022 0%")
  full <- data.table::data.table(
    player_id = rep(c("P1", "P2"), 6), season = rep(2021:2026, each = 2),
    round = 1L, lineup_position = "C"
  )
  msgs <- testthat::capture_messages(.attach_weekly_pos(skills, full))
  expect_false(any(grepl("too little", msgs)))
})

test_that(".dt_copy_once returns an independent copy of a data.table or data.frame", {
  d <- data.table::data.table(a = 1:3)
  cp <- .dt_copy_once(d)
  cp[, a := 0L]
  expect_equal(d$a, 1:3)
  f <- data.frame(a = 1:3)
  cp2 <- .dt_copy_once(f)
  expect_true(data.table::is.data.table(cp2))
  cp2[, a := 0L]
  expect_equal(f$a, 1:3)
})

test_that(".load_season_listing keeps one row per player-season and drops NA positions", {
  local_mocked_bindings(load_player_details = function(seasons, comp = "AFLM", ...) {
    data.table::data.table(player_id = c("P1", "P2", "P3"),
                           position_group = c("RUCK", NA, "MIDFIELDER"))
  })
  out <- suppressMessages(.load_season_listing(2026L))
  expect_setequal(out$player_id, c("P1", "P3"))
  expect_true(all(out$season %in% c(2025L, 2026L)))
  expect_equal(anyDuplicated(out[, c("player_id", "season")]), 0L)
})
