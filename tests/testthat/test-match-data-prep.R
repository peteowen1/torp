# Tests for .build_team_ratings_df PSR rolling-join (no forward leakage)
# =====================================================================
# Regression test for the PSR leakage bug. Previously the join used
# `slice_tail(n=1)` per player which applied each player's LATEST PSR to
# every historical lineup row, leaking future information into past games.
# The current implementation uses join_by(closest(.lineup_key > psr_key))
# which picks the most recent PSR strictly before each lineup row.

# -----------------------------------------------------------------------------
# Fixture builders
# -----------------------------------------------------------------------------

.tdp_make_teams <- function() {
  # 1 player per team, 2 teams (so we get a valid match), playing 2 rounds.
  # Position "C" (centre) — POSITION_AVG_TOG["C"] = 0.80, in MATCH_INDIVIDUAL_POS.
  expand.grid(
    player_id = c("P1", "P2"),
    round_number = c(0L, 1L),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  ) |>
    transform(
      season = 2026L,
      lineup_position = "C",
      position_group = "MID",
      team_name = ifelse(player_id == "P1", "Adelaide Crows", "Brisbane Lions"),
      team_id = ifelse(player_id == "P1", 10L, 20L),
      team_type = ifelse(player_id == "P1", "home", "away"),
      match_id = paste0("M-2026-R", round_number)
    )
}

.tdp_make_torp <- function() {
  # EPR per (player, season, round). Use constant EPR so we can reason about
  # it. round_number on lineups joins to `round` here.
  data.frame(
    player_id = rep(c("P1", "P2"), each = 2),
    season = 2026L,
    round = c(0L, 1L, 0L, 1L),
    epr = 10,
    recv_epr = 2.5,
    disp_epr = 2.5,
    spoil_epr = 2.5,
    hitout_epr = 2.5,
    stringsAsFactors = FALSE
  )
}

.tdp_make_psr <- function() {
  # PSR computed AT each round. Distinct values per round per player so we
  # can verify the rolling join picks the right one.
  data.frame(
    player_id = c("P1", "P1", "P2", "P2"),
    season = 2026L,
    round = c(0L, 1L, 0L, 1L),
    psr = c(100, 200, 300, 400),
    osr = c(50, 100, 150, 200),
    dsr = c(50, 100, 150, 200),
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# Tests
# -----------------------------------------------------------------------------

test_that("PSR join uses STRICT less-than: round 0 has no prior PSR", {
  teams  <- .tdp_make_teams()
  torp_df <- .tdp_make_torp()
  psr_df  <- .tdp_make_psr()

  result <- torp:::.build_team_ratings_df(teams, torp_df, psr_df)

  tog_c <- POSITION_AVG_TOG[["C"]]  # 0.80
  prior <- PSR_PRIOR_RATE

  r0 <- result[result$round_number == 0L, ]
  expect_equal(nrow(r0), 2L)
  # round 0: no prior PSR exists for either player → falls back to PSR_PRIOR_RATE
  expect_equal(r0$psr, rep(prior * tog_c, 2L))
  expect_equal(r0$osr, rep(prior / 2 * tog_c, 2L))
  expect_equal(r0$dsr, rep(prior / 2 * tog_c, 2L))
})

test_that("PSR join shifts forward by one round (no forward leakage)", {
  teams  <- .tdp_make_teams()
  torp_df <- .tdp_make_torp()
  psr_df  <- .tdp_make_psr()

  result <- torp:::.build_team_ratings_df(teams, torp_df, psr_df)

  tog_c <- POSITION_AVG_TOG[["C"]]  # 0.80

  # round 1 lineup should pick PSR from (season=2026, round=0):
  #   P1: psr 100 → expected 100 * 0.80
  #   P2: psr 300 → expected 300 * 0.80
  r1 <- result[result$round_number == 1L, ]
  expect_equal(nrow(r1), 2L)
  r1_p1 <- r1[r1$team_id == 10L, ]   # team P1
  r1_p2 <- r1[r1$team_id == 20L, ]   # team P2
  expect_equal(r1_p1$psr, 100 * tog_c)
  expect_equal(r1_p2$psr, 300 * tog_c)
  # round 1 should NOT have used the round-1 PSR (200 / 400) -- that would be
  # forward leakage:
  expect_false(isTRUE(all.equal(r1_p1$psr, 200 * tog_c)))
  expect_false(isTRUE(all.equal(r1_p2$psr, 400 * tog_c)))
})

test_that("future round (no PSR yet) picks the latest available prior PSR", {
  # Add a round-2 lineup row but no round-2 PSR. The join should pick the
  # round-1 PSR (the latest strictly before round 2). This mimics the
  # production prediction case where we predict round R using PSR through R-1.
  teams_extra <- rbind(
    .tdp_make_teams(),
    data.frame(
      player_id = c("P1", "P2"),
      round_number = 2L,
      season = 2026L,
      lineup_position = "C",
      position_group = "MID",
      team_name = c("Adelaide Crows", "Brisbane Lions"),
      team_id = c(10L, 20L),
      team_type = c("home", "away"),
      match_id = "M-2026-R2",
      stringsAsFactors = FALSE
    )
  )
  torp_extra <- rbind(
    .tdp_make_torp(),
    data.frame(player_id = c("P1", "P2"), season = 2026L, round = 2L,
               epr = 10, recv_epr = 2.5, disp_epr = 2.5,
               spoil_epr = 2.5, hitout_epr = 2.5,
               stringsAsFactors = FALSE)
  )
  psr_df  <- .tdp_make_psr()  # only rounds 0 and 1

  result <- torp:::.build_team_ratings_df(teams_extra, torp_extra, psr_df)

  tog_c <- POSITION_AVG_TOG[["C"]]
  r2 <- result[result$round_number == 2L, ]
  r2_p1 <- r2[r2$team_id == 10L, ]
  r2_p2 <- r2[r2$team_id == 20L, ]
  # round 2 should pick PSR from round 1: P1=200, P2=400
  expect_equal(r2_p1$psr, 200 * tog_c)
  expect_equal(r2_p2$psr, 400 * tog_c)
})

test_that("PSR join works without osr/dsr (back-compat)", {
  # Old psr_df without osr/dsr columns should still work; only `psr` populated.
  teams  <- .tdp_make_teams()
  torp_df <- .tdp_make_torp()
  psr_df  <- .tdp_make_psr()[, c("player_id", "season", "round", "psr")]

  result <- torp:::.build_team_ratings_df(teams, torp_df, psr_df)

  tog_c <- POSITION_AVG_TOG[["C"]]
  r1_p1 <- result[result$round_number == 1L & result$team_id == 10L, ]
  expect_equal(r1_p1$psr, 100 * tog_c)
  # osr/dsr should not be in output
  expect_false("osr" %in% names(result))
  expect_false("dsr" %in% names(result))
})

test_that("PSR fallback works when psr_df is NULL", {
  teams  <- .tdp_make_teams()
  torp_df <- .tdp_make_torp()

  result <- torp:::.build_team_ratings_df(teams, torp_df, psr_df = NULL)

  # When psr_df is NULL: psr is set to PSR_PRIOR_RATE BEFORE the position
  # columns / TOG-multiply logic runs. The aggregation sums per team, but
  # there's only 1 player per team, so psr per row should be PSR_PRIOR_RATE.
  expect_true(all(result$psr == PSR_PRIOR_RATE))
})
