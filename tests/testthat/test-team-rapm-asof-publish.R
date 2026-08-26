# Tests for the as-of xRAPM publish/refresh infrastructure:
#   * load_team_rapm_asof()          -- resolution order + contract
#   * .team_rapm_played_checkpoints() -- the future-rounds trap
#   * .warn_stale_xrapm_snapshot()   -- the silent-rot guard
#
# The failure this whole file exists to prevent is that all three of these
# degrade QUIETLY: a missing snapshot flattens a feature to 0, a phantom
# checkpoint looks like a current one, and a stale snapshot keeps joining
# successfully while serving a frozen rating.

.mk_snapshot <- function(seasons = 2026L, rounds = 1:3) {
  grid <- expand.grid(season = seasons, round_number = rounds,
                      player_id = c("p1", "p2"), stringsAsFactors = FALSE)
  grid$team_rapm_shrunk <- seq_len(nrow(grid)) / 10
  grid[, c("player_id", "season", "round_number", "team_rapm_shrunk")]
}

.write_snapshot <- function(dt) {
  p <- withr::local_tempfile(fileext = ".parquet", .local_envir = parent.frame())
  arrow::write_parquet(dt, p)
  p
}

# -----------------------------------------------------------------------------
# load_team_rapm_asof
# -----------------------------------------------------------------------------

test_that("load_team_rapm_asof reads an explicit path and returns the contract columns", {
  p <- .write_snapshot(.mk_snapshot())
  out <- load_team_rapm_asof(comp = "AFLM", path = p)

  expect_s3_class(out, "data.frame")
  expect_true(all(c("player_id", "season", "round_number", "team_rapm_shrunk") %in% names(out)))
  expect_equal(nrow(out), 6)
})

test_that("load_team_rapm_asof warns and returns NULL for a missing explicit path -- never errors", {
  # Non-fatal by design: xrapm_diff is optional and must degrade to a flat 0
  # rather than taking down the served prediction pipeline.
  missing_path <- file.path(tempdir(), "definitely-not-here-xrapm.parquet")
  expect_false(file.exists(missing_path))

  # expect_message, not expect_warning: these paths use cli_alert_danger so
  # they print immediately. A cli_warn here would be deferred to end-of-Rscript
  # and could be dropped past nwarnings or lost to a timeout -- the exact way
  # this failure would otherwise go unlogged.
  expect_message(
    out <- load_team_rapm_asof(comp = "AFLM", path = missing_path),
    "No as-of xRAPM snapshot"
  )
  expect_null(out)
})

test_that("load_team_rapm_asof ABORTS when the shipping column is absent", {
  # A snapshot built before 2026-08-26 carried only raw `rapm`. Silently
  # accepting it would ship a different rating than the one that was chosen,
  # so this is the one case that must be loud rather than NULL.
  bad <- .mk_snapshot()
  bad$team_rapm_shrunk <- NULL
  bad$rapm <- 1
  p <- .write_snapshot(bad)

  expect_error(
    load_team_rapm_asof(comp = "AFLM", path = p),
    "missing required column"
  )
})

test_that("load_team_rapm_asof reports loudly and returns NULL on an empty snapshot", {
  p <- .write_snapshot(.mk_snapshot()[0, ])
  expect_message(
    out <- load_team_rapm_asof(comp = "AFLM", path = p),
    "EMPTY"
  )
  expect_null(out)
})

test_that("load_team_rapm_asof validates comp", {
  expect_error(load_team_rapm_asof(comp = "NBA"), regexp = ".")
})

# -----------------------------------------------------------------------------
# .team_rapm_played_checkpoints -- the future-rounds trap
# -----------------------------------------------------------------------------

.mk_fixtures_3_rounds <- function() {
  tibble::tibble(
    match_id = paste0("m", 1:6), season = 2026L,
    round_number = c(1L, 1L, 2L, 2L, 3L, 3L),
    utc_start_time = as.Date(c("2026-04-04", "2026-04-07",
                               "2026-04-11", "2026-04-14",
                               "2026-04-18", "2026-04-21"))
  )
}

test_that(".team_rapm_played_checkpoints drops scheduled-but-unplayed rounds", {
  # Rounds 1-2 played, round 3 scheduled only. Round 3 must not produce a
  # checkpoint: its fit window would keep growing until its date passes, so
  # the row would change on every run.
  results <- tibble::tibble(
    season = 2026L,
    round_number = c(1L, 1L, 2L, 2L, 3L, 3L),
    home_score = c(80, 70, 90, 60, NA, NA),
    away_score = c(75, 65, 85, 55, NA, NA)
  )
  local_mocked_bindings(
    load_fixtures = function(all = TRUE, comp = "AFLM") .mk_fixtures_3_rounds(),
    load_results = function(seasons = TRUE, comp = "AFLM") results
  )

  cp <- torp:::.team_rapm_played_checkpoints(comp = "AFLM")
  expect_equal(sort(cp$round_number), c(1L, 2L))
  expect_false(3L %in% cp$round_number)
})

test_that(".team_rapm_played_checkpoints excludes a PAST round with no score (postponed)", {
  # A date-based filter (checkpoint_date <= Sys.Date()) would wave this
  # through. Filtering on a recorded score is what makes it correct.
  results <- tibble::tibble(
    season = 2026L,
    round_number = c(1L, 1L, 2L, 2L, 3L, 3L),
    home_score = c(80, 70, NA, NA, 90, 60),
    away_score = c(75, 65, NA, NA, 85, 55)
  )
  local_mocked_bindings(
    load_fixtures = function(all = TRUE, comp = "AFLM") .mk_fixtures_3_rounds(),
    load_results = function(seasons = TRUE, comp = "AFLM") results
  )

  cp <- torp:::.team_rapm_played_checkpoints(comp = "AFLM")
  expect_false(2L %in% cp$round_number)
  expect_equal(sort(cp$round_number), c(1L, 3L))
})

test_that(".team_rapm_played_checkpoints returns zero rows when nothing is played", {
  results <- tibble::tibble(
    season = 2026L, round_number = c(1L, 2L, 3L),
    home_score = NA_real_, away_score = NA_real_
  )
  local_mocked_bindings(
    load_fixtures = function(all = TRUE, comp = "AFLM") .mk_fixtures_3_rounds(),
    load_results = function(seasons = TRUE, comp = "AFLM") results
  )

  expect_warning(
    cp <- torp:::.team_rapm_played_checkpoints(comp = "AFLM"),
    "No played matches"
  )
  expect_equal(nrow(cp), 0)
})

test_that(".team_rapm_played_checkpoints aborts rather than silently falling back when results lack score columns", {
  # Falling back to the unfiltered fixture calendar here would reintroduce the
  # exact bug this function exists to prevent, so it must fail loudly.
  local_mocked_bindings(
    load_fixtures = function(all = TRUE, comp = "AFLM") .mk_fixtures_3_rounds(),
    load_results = function(seasons = TRUE, comp = "AFLM") {
      tibble::tibble(season = 2026L, round_number = 1L)
    }
  )
  expect_error(
    torp:::.team_rapm_played_checkpoints(comp = "AFLM"),
    "Cannot confirm which rounds were played"
  )
})

# -----------------------------------------------------------------------------
# .warn_stale_xrapm_snapshot -- the silent-rot guard
# -----------------------------------------------------------------------------

test_that(".warn_stale_xrapm_snapshot reports loudly when the snapshot is behind the predicted round", {
  snap <- .mk_snapshot(rounds = 1:3)  # latest = 2026 R3
  expect_message(
    torp:::.warn_stale_xrapm_snapshot(snap, season = 2026L, round_number = 9L, comp = "AFLM"),
    "STALE xRAPM snapshot"
  )
})

test_that(".warn_stale_xrapm_snapshot is silent when the snapshot is current", {
  snap <- .mk_snapshot(rounds = 1:9)
  expect_silent(
    lag <- torp:::.warn_stale_xrapm_snapshot(snap, season = 2026L, round_number = 9L, comp = "AFLM")
  )
  expect_equal(lag, 0L)
})

test_that(".warn_stale_xrapm_snapshot tolerates exactly one round of lag", {
  # The checkpoint for round r is built just before round r, so a correctly
  # refreshed snapshot legitimately sits one round back.
  snap <- .mk_snapshot(rounds = 1:8)
  expect_silent(
    torp:::.warn_stale_xrapm_snapshot(snap, season = 2026L, round_number = 9L, comp = "AFLM")
  )
})

test_that(".warn_stale_xrapm_snapshot is a no-op for NULL/empty input", {
  expect_silent(torp:::.warn_stale_xrapm_snapshot(NULL, 2026L, 5L))
  expect_silent(torp:::.warn_stale_xrapm_snapshot(.mk_snapshot()[0, ], 2026L, 5L))
})

test_that(".warn_stale_xrapm_snapshot reports loudly across a season boundary", {
  # Last season's final round is not "one round behind" this season's round 1.
  snap <- .mk_snapshot(seasons = 2025L, rounds = 1:3)
  expect_message(
    torp:::.warn_stale_xrapm_snapshot(snap, season = 2026L, round_number = 1L, comp = "AFLM"),
    "STALE xRAPM snapshot"
  )
})
