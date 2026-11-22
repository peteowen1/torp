# AFL Team-List Snapshots
# =======================
# get_afl_lineups() returns the team lists as they stand when it is called, and
# the teams-data release is overwritten each time, so it only ever holds the
# FINAL list. Nothing records the list as first named (Wednesday/Thursday
# evening) or the late changes after it. These functions keep every version:
# a scheduled run fetches each nearby match's roster, stores the response raw,
# and appends that match's parsed rows whenever its list has changed.
#
# Three assets per season on the teams-snapshots release:
#   teams_snapshot_log_<season>  one row per match per fetch, changed or not
#   teams_snapshots_<season>     parsed roster rows, one set per CHANGED version
#   teams_snapshots_raw_<season> the raw response text for each changed version
# The list in force at time T for a match is its latest version with
# fetched_at <= T; the log shows the fetches between versions found no change.
# If a run's uploads fail part-way, the next run re-saves the same version
# with a later fetched_at, so one roster_hash can appear twice: count versions
# as distinct (match_id, roster_hash) and take min(fetched_at) as first seen.


#' Pick the matches a snapshot run should fetch
#'
#' Every match whose bounce falls between `lookback_hours` before `now` and
#' `lookahead_days` after it. Selected by start time rather than round number,
#' because a Wednesday run must already reach the next round's Thursday match.
#'
#' @param fixtures Fixtures frame with `match_id` and `utc_start_time`
#'   (e.g. `"2026-08-20T09:30:00.000+0000"`).
#' @param now POSIXct reference time.
#' @param lookback_hours,lookahead_days Window either side of `now`.
#' @return The selected rows of `fixtures`, with a POSIXct `start_utc` column.
#' @keywords internal
#' @noRd
.select_snapshot_matches <- function(fixtures, now,
                                     lookback_hours = LINEUP_SNAPSHOT_LOOKBACK_HOURS,
                                     lookahead_days = LINEUP_SNAPSHOT_LOOKAHEAD_DAYS) {
  if (is.null(fixtures) || nrow(fixtures) == 0) return(fixtures)
  start <- as.POSIXct(substr(fixtures$utc_start_time, 1, 19),
                      format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  n_bad <- sum(is.na(start) & !is.na(fixtures$utc_start_time))
  if (n_bad > 0) {
    cli::cli_warn("{n_bad} fixture start time{?s} could not be parsed; those matches are not snapshotted.")
  }
  now <- as.POSIXct(now, tz = "UTC")
  keep <- !is.na(start) &
    start >= now - lookback_hours * 3600 &
    start <= now + lookahead_days * 86400
  out <- fixtures[keep, , drop = FALSE]
  out$start_utc <- start[keep]
  out
}


#' Fingerprint the team lists in one roster response
#'
#' Hashes the two teams' blocks of `matchRoster` (players and positions,
#' `teamStatus`, `ins`, `outs`, `lateChanges`) and nothing else, so a change
#' in a match-level field such as `lastUpdated` or the weather does not count
#' as a new version of the list.
#'
#' @param body Raw response text.
#' @return A SHA-1 string, or `NA` when the body is missing or not JSON with a
#'   roster in it.
#' @keywords internal
#' @noRd
.roster_hash <- function(body) {
  if (length(body) != 1L || is.na(body) || !nzchar(body)) return(NA_character_)
  j <- tryCatch(jsonlite::fromJSON(body, simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(j)) return(NA_character_)
  r <- j$matchRoster %||% j
  if (is.null(r$homeTeam) && is.null(r$awayTeam)) return(NA_character_)
  txt <- as.character(jsonlite::toJSON(list(home = r$homeTeam, away = r$awayTeam),
                                       auto_unbox = TRUE, null = "null", digits = NA))
  digest::digest(txt, algo = "sha1", serialize = FALSE)
}


#' Fetch one snapshot of the team lists for nearby matches
#'
#' Fetches the roster of every match from [.select_snapshot_matches()] and
#' returns what came back. Does not upload anything; see
#' [save_lineup_snapshots()].
#'
#' @param season Season to read fixtures for (default: current season).
#' @param now Reference time for the match window (default: now).
#' @param comp `"AFLM"` (default) or `"AFLW"`.
#' @return A list with `log` (one row per match fetched: `match_id`,
#'   `fetched_at`, `http_status`, `roster_hash`), `raw` (the same rows plus
#'   `body`, the response text as received) and `parsed` (the roster rows, as
#'   [get_afl_lineups()] returns them, plus `fetched_at` and `roster_hash`).
#'   All three have zero rows when no match is in the window.
#' @export
snapshot_afl_lineups <- function(season = NULL, now = Sys.time(), comp = "AFLM") {
  empty <- list(
    log = tibble::tibble(match_id = character(), fetched_at = as.POSIXct(character(), tz = "UTC"),
                         http_status = integer(), roster_hash = character()),
    raw = NULL, parsed = tibble::tibble()
  )
  empty$raw <- dplyr::mutate(empty$log, body = character())

  fixtures <- get_afl_fixtures(season, comp)
  sel <- .select_snapshot_matches(fixtures, now)
  if (is.null(sel) || nrow(sel) == 0) {
    cli::cli_inform("No matches within the snapshot window; nothing to fetch.")
    return(empty)
  }
  ids <- sel$match_id
  raw_env <- new.env(parent = emptyenv())
  token <- get_token()
  # Stamped at request time, not after parsing: this is when the AFL was
  # showing these lists.
  fetched_at <- as.POSIXct(Sys.time(), tz = "UTC")
  parsed <- .fetch_cfs_batch(
    ids = ids,
    url_template = paste0(AFL_CFS_API_BASE_URL, "matchRoster/full/%s"),
    token = token, parse_fn = .parse_match_roster, label = "roster snapshot",
    raw_env = raw_env
  )

  got <- lapply(ids, function(id) raw_env[[id]] %||% list(status = NA_integer_, body = NA_character_))
  raw <- tibble::tibble(
    match_id = ids,
    fetched_at = fetched_at,
    http_status = vapply(got, function(g) as.integer(g$status), integer(1)),
    body = vapply(got, function(g) as.character(g$body), character(1))
  )
  raw$roster_hash <- vapply(raw$body, .roster_hash, character(1), USE.NAMES = FALSE)
  raw <- raw[, c("match_id", "fetched_at", "http_status", "roster_hash", "body")]

  if (nrow(parsed) > 0) {
    parsed <- .finish_lineups(parsed)
    parsed$fetched_at <- fetched_at
    parsed$roster_hash <- raw$roster_hash[match(parsed$match_id, raw$match_id)]
  }

  cli::cli_inform(
    "Snapshot at {format(fetched_at, '%Y-%m-%d %H:%M UTC')}: {nrow(raw)} match{?es}, {sum(!is.na(raw$roster_hash))} with a roster, {nrow(parsed)} player row{?s}."
  )
  list(log = raw[, c("match_id", "fetched_at", "http_status", "roster_hash")],
       raw = raw, parsed = parsed)
}


#' Mark which fetched rosters are new versions
#'
#' A fetch is a new version when it has a roster and its hash differs from
#' the latest hash already logged for that match (or none is logged). A fetch
#' with no roster (not yet named, HTTP error) is logged but never a version.
#'
#' @param log This run's log rows.
#' @param existing_log The season's log so far, or NULL.
#' @return `log` with a logical `changed` column.
#' @keywords internal
#' @noRd
.mark_changed_rosters <- function(log, existing_log) {
  prev <- character(0)
  if (!is.null(existing_log) && nrow(existing_log) > 0) {
    el <- existing_log[!is.na(existing_log$roster_hash), , drop = FALSE]
    el <- el[order(el$fetched_at), , drop = FALSE]
    last <- !duplicated(el$match_id, fromLast = TRUE)
    prev <- stats::setNames(el$roster_hash[last], el$match_id[last])
  }
  before <- unname(prev[log$match_id])
  log$changed <- !is.na(log$roster_hash) & (is.na(before) | before != log$roster_hash)
  log
}


#' Append a snapshot to the season's team-list history on torpdata
#'
#' Reads the three accumulating assets back first (never starting fresh over
#' history; see `.read_accumulating_asset()`), appends this run's log row for
#' every match, and appends raw and parsed rows only for the matches whose
#' list changed. Raw and parsed are uploaded before the log, because the log
#' is what the next run compares against: if an upload fails part-way, the
#' change is seen again and re-saved next run rather than lost.
#'
#' @param snap Output of [snapshot_afl_lineups()].
#' @param season Season the assets belong to.
#' @param tag Release tag (default `LINEUP_SNAPSHOT_TAG`).
#' @return Invisibly, the log rows written this run (with `changed`).
#' @keywords internal
save_lineup_snapshots <- function(snap, season, tag = LINEUP_SNAPSHOT_TAG) {
  if (is.null(snap$log) || nrow(snap$log) == 0) {
    cli::cli_inform("No snapshot rows to save.")
    return(invisible(snap$log))
  }
  stems <- c(log = "teams_snapshot_log_", raw = "teams_snapshots_raw_",
             parsed = "teams_snapshots_")
  stems <- stats::setNames(paste0(stems, season), names(stems))
  read_existing <- function(stem) {
    .read_accumulating_asset(paste0(stem, ".parquet"), tag, empty_is_absent = TRUE)
  }

  existing_log <- read_existing(stems[["log"]])
  log <- .mark_changed_rosters(snap$log, existing_log)
  changed_ids <- log$match_id[log$changed]
  cli::cli_inform("{length(changed_ids)} of {nrow(log)} match{?es} ha{?s/ve} a new list version this run.")

  if (length(changed_ids) > 0) {
    new_raw <- snap$raw[snap$raw$match_id %in% changed_ids, , drop = FALSE]
    new_parsed <- snap$parsed[snap$parsed$match_id %in% changed_ids, , drop = FALSE]
    if (nrow(new_parsed) == 0) {
      cli::cli_warn("{length(changed_ids)} roster{?s} changed but none parsed into player rows; saving the raw copies only.")
    }
    for (part in c("raw", "parsed")) {
      new_rows <- if (part == "raw") new_raw else new_parsed
      if (nrow(new_rows) == 0) next
      existing <- read_existing(stems[[part]])
      combined <- dplyr::bind_rows(existing, new_rows)
      save_to_release(combined, stems[[part]], tag)
    }
  }

  combined_log <- dplyr::bind_rows(existing_log, log)
  save_to_release(combined_log, stems[["log"]], tag)
  invisible(log)
}
