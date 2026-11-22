# Team-list snapshots (R/lineup_snapshots.R): window selection, change
# detection, and the append-only save. No network: the release I/O is mocked.

roster_body <- function(home_players = c("A", "B"), last_updated = "2026-08-20T08:00:00Z",
                        status = "FINAL_TEAM") {
  jsonlite::toJSON(list(
    matchRoster = list(
      lastUpdated = last_updated,
      homeTeam = list(teamId = "CD_T10", teamStatus = status,
                      positions = lapply(home_players, function(p) list(player = list(playerId = p)))),
      awayTeam = list(teamId = "CD_T20", teamStatus = status, positions = list())
    )
  ), auto_unbox = TRUE)
}

test_that(".select_snapshot_matches keeps bounces from 6h ago to 8 days ahead", {
  now <- as.POSIXct("2026-08-19T08:00:00", tz = "UTC")
  fx <- data.frame(
    match_id = c("old", "just_played", "thu", "next_week", "too_far"),
    utc_start_time = c("2026-08-18T08:00:00.000+0000", "2026-08-19T03:00:00.000+0000",
                       "2026-08-20T09:30:00.000+0000", "2026-08-26T09:30:00.000+0000",
                       "2026-08-28T09:30:00.000+0000"),
    stringsAsFactors = FALSE
  )
  out <- .select_snapshot_matches(fx, now)
  expect_equal(out$match_id, c("just_played", "thu", "next_week"))
  expect_s3_class(out$start_utc, "POSIXct")
})

test_that(".roster_hash tracks the lists, not match-level fields", {
  h1 <- .roster_hash(roster_body())
  expect_match(h1, "^[0-9a-f]{40}$")
  expect_identical(.roster_hash(roster_body(last_updated = "2026-08-20T09:00:00Z")), h1)
  expect_false(identical(.roster_hash(roster_body(home_players = c("A", "C"))), h1))
  expect_false(identical(.roster_hash(roster_body(status = "PROVISIONAL")), h1))
  expect_true(is.na(.roster_hash(NA_character_)))
  expect_true(is.na(.roster_hash("not json")))
  expect_true(is.na(.roster_hash('{"match": {}}')))
})

test_that(".mark_changed_rosters flags new and changed lists only", {
  t0 <- as.POSIXct("2026-08-19T08:00:00", tz = "UTC")
  existing <- data.frame(match_id = c("m1", "m1", "m2"),
                         fetched_at = t0 + c(0, 3600, 0),
                         roster_hash = c("a", "b", "x"), stringsAsFactors = FALSE)
  log <- data.frame(match_id = c("m1", "m2", "m3", "m4"),
                    fetched_at = t0 + 7200,
                    roster_hash = c("b", "y", "z", NA), stringsAsFactors = FALSE)
  out <- .mark_changed_rosters(log, existing)
  # m1 matches its LATEST hash (b, not a); m2 changed; m3 is new; m4 has no roster
  expect_equal(out$changed, c(FALSE, TRUE, TRUE, FALSE))
  expect_equal(.mark_changed_rosters(log, NULL)$changed, c(TRUE, TRUE, TRUE, FALSE))
})

test_that("save_lineup_snapshots appends changed versions and logs every fetch", {
  store <- new.env()
  uploads <- character(0)
  local_mocked_bindings(
    .read_accumulating_asset = function(name, tag, ...) {
      stem <- sub("\\.parquet$", "", name)
      store[[stem]]
    },
    save_to_release = function(df, file_name, release_tag, ...) {
      uploads <<- c(uploads, file_name)
      store[[file_name]] <- df
      invisible(NULL)
    }
  )
  t1 <- as.POSIXct("2026-08-19T08:00:00", tz = "UTC")
  make_snap <- function(t, players) {
    body <- roster_body(home_players = players)
    raw <- tibble::tibble(match_id = "m1", fetched_at = t, http_status = 200L,
                          roster_hash = .roster_hash(body), body = body)
    list(log = raw[, 1:4], raw = raw,
         parsed = tibble::tibble(match_id = "m1", player_id = players,
                                 fetched_at = t, roster_hash = raw$roster_hash))
  }

  save_lineup_snapshots(make_snap(t1, c("A", "B")), 2026, tag = "test")
  expect_setequal(uploads, c("teams_snapshots_raw_2026", "teams_snapshots_2026", "teams_snapshot_log_2026"))
  expect_equal(tail(uploads, 1), "teams_snapshot_log_2026")   # log goes last

  uploads <- character(0)
  out <- save_lineup_snapshots(make_snap(t1 + 3600, c("A", "B")), 2026, tag = "test")
  expect_false(out$changed)
  expect_equal(uploads, "teams_snapshot_log_2026")             # unchanged: log only
  expect_equal(nrow(store$teams_snapshot_log_2026), 2)
  expect_equal(nrow(store$teams_snapshots_2026), 2)

  uploads <- character(0)
  save_lineup_snapshots(make_snap(t1 + 7200, c("A", "C")), 2026, tag = "test")
  expect_equal(length(uploads), 3)
  expect_equal(nrow(store$teams_snapshots_2026), 4)
  expect_equal(nrow(store$teams_snapshots_raw_2026), 2)
  expect_equal(store$teams_snapshot_log_2026$changed, c(TRUE, FALSE, TRUE))
})
