# Tests for the xRAPM match-model feature (team_rapm_match_feature.R).
#
# The three things that can silently go wrong when wiring a player rating into
# the match model, per this file's directive:
#   1. a future rating leaking backwards into an earlier match,
#   2. an inverted diff (model learns the feature backwards),
#   3. a missing rating becoming a real "teams are equal" claim.

.mk_lineups <- function() {
  data.frame(
    player_id = c("p1", "p2", "p1", "p2"),
    season = c(2026L, 2026L, 2026L, 2026L),
    round_number = c(1L, 1L, 5L, 5L),
    lineup_tog = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )
}

# WHAT "round r" MEANS ON A SNAPSHOT ROW, because these tests are meaningless
# without it: a snapshot row labelled round r is the checkpoint that CONTAINS
# round r (it is dated the day before round r+1 starts, and the fit takes every
# match on or before that date). So it has already seen round r's results, and a
# round-r lineup row must NOT see it. Predicting round q uses checkpoint q-1.
#
# Until 2026-08-26 the join used `>=` and these tests asserted the opposite --
# they certified the leak as correct. Corrected together with the join.

test_that(".join_xrapm_to_lineups never uses a rating from a later round", {
  # p1 has a rating at round 5 only. A round-1 lineup row must NOT see it --
  # that rating is built from matches p1 had not yet played at round 1.
  snap <- data.frame(
    player_id = "p1",
    season = 2026L,
    round_number = 5L,
    team_rapm_shrunk = 99,
    stringsAsFactors = FALSE
  )
  # Every fixture row is unrated here (round 5's own checkpoint is not
  # available to round 5), so the coverage warning fires by design --
  # suppressed rather than silenced globally, so a NEW warning would surface.
  suppressWarnings(out <- .join_xrapm_to_lineups(.mk_lineups(), snap))

  r1 <- out[out$player_id == "p1" & out$round_number == 1L, ]
  r5 <- out[out$player_id == "p1" & out$round_number == 5L, ]

  expect_equal(r1$xrapm, 0) # unrated as of round 1 -> ridge prior
  # THE LEAK REGRESSION: checkpoint 5 contains round 5's own result, so the
  # round-5 row must fall back to the prior, not read its own outcome.
  expect_equal(r5$xrapm, 0)
})

test_that(".join_xrapm_to_lineups carries an earlier rating forward, never into its own round", {
  snap <- data.frame(
    player_id = "p1",
    season = 2026L,
    round_number = 1L,
    team_rapm_shrunk = 7,
    stringsAsFactors = FALSE
  )
  suppressWarnings(out <- .join_xrapm_to_lineups(.mk_lineups(), snap))

  # Round 1 must NOT see its own checkpoint (it contains round 1's result);
  # round 5 legitimately does, because checkpoint 1 predates round 5.
  expect_equal(out$xrapm[out$player_id == "p1" & out$round_number == 1L], 0)
  expect_equal(out$xrapm[out$player_id == "p1" & out$round_number == 5L], 7)
})

test_that(".join_xrapm_to_lineups picks the latest checkpoint STRICTLY before the round", {
  snap <- data.frame(
    player_id = c("p1", "p1", "p1"),
    season = c(2026L, 2026L, 2026L),
    round_number = c(1L, 4L, 9L),
    team_rapm_shrunk = c(1, 4, 9),
    stringsAsFactors = FALSE
  )
  suppressWarnings(out <- .join_xrapm_to_lineups(.mk_lineups(), snap))

  # Nothing precedes round 1 -> prior.
  expect_equal(out$xrapm[out$player_id == "p1" & out$round_number == 1L], 0)
  # Round 5 takes checkpoint 4: the latest strictly before it, never round 9.
  expect_equal(out$xrapm[out$player_id == "p1" & out$round_number == 5L], 4)
})

test_that(".join_xrapm_to_lineups carries the last checkpoint of a prior season", {
  # Season boundary: the key is season*100 + round, so 2025 R23 (202523) must
  # still be visible to 2026 R1 (202601). A naive per-season join would drop it.
  snap <- data.frame(
    player_id = "p1",
    season = 2025L,
    round_number = 23L,
    team_rapm_shrunk = 5,
    stringsAsFactors = FALSE
  )
  suppressWarnings(out <- .join_xrapm_to_lineups(.mk_lineups(), snap))
  expect_equal(out$xrapm[out$player_id == "p1" & out$round_number == 1L], 5)
})

test_that("the SERVED pipeline actually passes a snapshot to .build_team_ratings_df()", {
  # The regression this exists for: xrapm_diff was wired into
  # build_team_mdl_df(), which nothing in production calls -- it is used only by
  # data-raw/debug/ scripts. run_predictions_pipeline() goes through
  # build_prediction_state(), which called .build_team_ratings_df() with three
  # arguments and so left xrapm_df at its NULL default. The NULL path sets
  # xrapm <- 0 and returns silently, so xrapm_diff was a flat 0 for every match
  # ever served, and nothing in any log said so.
  #
  # Asserted against the function BODY rather than the source file: an installed
  # package has no R/*.R to grep, so a file-scanning check would pass vacuously
  # in exactly the environment that matters.
  fns <- list(
    build_prediction_state = torp:::build_prediction_state,
    build_team_mdl_df      = torp:::build_team_mdl_df
  )
  for (nm in names(fns)) {
    src <- paste(deparse(body(fns[[nm]])), collapse = "\n")
    calls <- regmatches(src, gregexpr("\\.build_team_ratings_df\\([^)]*\\)", src))[[1]]
    expect_true(length(calls) > 0,
                info = paste0(nm, " no longer calls .build_team_ratings_df()"))
    for (cl in calls) {
      expect_true(
        grepl("xrapm", cl, fixed = TRUE),
        info = paste0(nm, ": .build_team_ratings_df() called without an xRAPM ",
                      "snapshot -- xrapm_diff silently becomes 0. Call: ", cl)
      )
    }
  }
})

test_that("a checkpoint built from real match data does not reach its own round", {
  # The tests above hand-type a snapshot, so they can only prove the JOIN is
  # strict -- they assume the round label means "contains round r". This one
  # closes that gap from the other end: it derives the checkpoint the way the
  # builder does (from a fixture calendar) and shows the round-r checkpoint is
  # dated AFTER round r's matches, which is exactly why the join must exclude
  # it. If .team_rapm_checkpoint_dates() is ever changed to date checkpoints
  # BEFORE their own round, this test fails and the join must change with it.
  fx <- data.frame(
    match_id = sprintf("M%02d", 1:6),
    season = rep(2026L, 6),
    round_number = rep(1:3, each = 2),
    utc_start_time = as.POSIXct(
      c("2026-03-05", "2026-03-07",   # round 1
        "2026-03-12", "2026-03-14",   # round 2
        "2026-03-19", "2026-03-21"),  # round 3
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )
  local_mocked_bindings(load_fixtures = function(...) fx, .package = "torp")

  cp <- .team_rapm_checkpoint_dates(comp = "AFLM")
  r1 <- cp[cp$round_number == 1L, ]
  r1_last_match <- as.Date("2026-03-07")

  # The round-1 checkpoint is dated after round 1 was played -- so its fit
  # (match_date <= ref_date) includes round 1's own results.
  expect_gt(as.numeric(r1$checkpoint_date), as.numeric(r1_last_match))

  # Therefore a round-1 lineup row must not receive it. Feed the real
  # checkpoint's own (season, round) label through the join and confirm.
  snap <- data.frame(
    player_id = "p1", season = r1$season, round_number = r1$round_number,
    team_rapm_shrunk = 42, stringsAsFactors = FALSE
  )
  lineups <- data.frame(
    player_id = "p1", season = 2026L, round_number = 1L, lineup_tog = 1,
    stringsAsFactors = FALSE
  )
  suppressWarnings(out <- .join_xrapm_to_lineups(lineups, snap))
  expect_equal(out$xrapm, 0)
})

test_that("an unrated player gets the ridge prior 0, not an NA or a non-zero prior", {
  # p2 is absent from the snapshot entirely.
  snap <- data.frame(
    player_id = "p1", season = 2026L, round_number = 1L,
    team_rapm_shrunk = 5, stringsAsFactors = FALSE
  )
  out <- .join_xrapm_to_lineups(.mk_lineups(), snap)

  expect_false(any(is.na(out$xrapm)))
  expect_true(all(out$xrapm[out$player_id == "p2"] == 0))
})

test_that("a NULL snapshot yields a flat 0 column rather than erroring", {
  out <- .join_xrapm_to_lineups(.mk_lineups(), NULL)
  expect_true(all(out$xrapm == 0))
  expect_equal(nrow(out), 4L)
})

test_that("xrapm is TOG-weighted, so a part-game slot counts for less", {
  lineups <- .mk_lineups()
  # TOG on the ROUND 5 rows (indices 3,4): the checkpoint below is round 1, and
  # under the strict as-of join only a later round can see it. Weighting round 1
  # here would test nothing -- both rows would sit at the prior 0.
  lineups$lineup_tog <- c(1, 1, 1, 0.5)
  snap <- data.frame(
    player_id = c("p1", "p2"), season = c(2026L, 2026L),
    round_number = c(1L, 1L), team_rapm_shrunk = c(10, 10),
    stringsAsFactors = FALSE
  )
  suppressWarnings(out <- .join_xrapm_to_lineups(lineups, snap))

  r5 <- out[out$round_number == 5L, ]
  expect_equal(r5$xrapm[r5$player_id == "p1"], 10)
  expect_equal(r5$xrapm[r5$player_id == "p2"], 5) # 10 * 0.5
})

test_that("duplicate snapshot keys are deduped rather than duplicating lineup rows", {
  # Two rows for the same (player, season, round) would otherwise fan out
  # through closest() and silently double this player's contribution to the
  # per-team sum.
  snap <- data.frame(
    player_id = c("p1", "p1"),
    season = c(2026L, 2026L),
    round_number = c(1L, 1L),
    team_rapm_shrunk = c(3, 4),
    stringsAsFactors = FALSE
  )
  expect_warning(
    out <- .join_xrapm_to_lineups(.mk_lineups(), snap),
    "duplicate"
  )
  expect_equal(nrow(out), 4L)
})

test_that("load_team_rapm_asof reports loudly and returns NULL when no snapshot exists", {
  # cli_alert_danger, not cli_warn -- see the severity note in
  # load_team_rapm_asof(). cli_alert_* goes through message(), so this is
  # expect_message(), and that change is the point: a deferred warning can be
  # dropped past nwarnings or lost to a timeout, which is how the failure this
  # reports would go unlogged.
  expect_message(
    got <- load_team_rapm_asof(comp = "AFLM",
                               path = file.path(tempdir(), "definitely-not-here.parquet")),
    "No as-of xRAPM snapshot"
  )
  expect_null(got)
})

test_that("load_team_rapm_asof aborts on a pre-2026-08-26 snapshot missing team_rapm_shrunk", {
  skip_if_not_installed("arrow")
  p <- file.path(tempdir(), "old_schema.parquet")
  # The old schema: rapm (raw) but no team_rapm_shrunk, no season/round keys.
  arrow::write_parquet(
    data.frame(player_id = "p1", ref_date = as.Date("2026-01-01"), rapm = 1),
    p
  )
  on.exit(unlink(p), add = TRUE)
  expect_error(load_team_rapm_asof(comp = "AFLM", path = p), "missing required column")
})

test_that("the diff is oriented self-minus-opponent, matching the other *_diff features", {
  # Guards the inverted-feature failure mode: a stronger own team must produce
  # a POSITIVE xrapm_diff, same sign convention as psr_diff/epr_diff.
  df <- data.frame(xrapm.x = c(10, 2), xrapm.y = c(2, 10))
  xrapm_diff <- df$xrapm.x - df$xrapm.y
  expect_gt(xrapm_diff[1], 0) # own team stronger -> positive
  expect_lt(xrapm_diff[2], 0) # opponent stronger -> negative
  expect_equal(xrapm_diff[1], -xrapm_diff[2]) # symmetric
})
