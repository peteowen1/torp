# Weekly AFLW season-stat snapshot ----
#
# Captures the CURRENT season's cumulative season-to-date totals from
# statspro/playersStats/seasons/{providerId} and publishes them as a DATED
# file, rather than overwriting the canonical per-season file that
# scrape_aflw_season_stats.R maintains.
#
# Why dated snapshots: that endpoint has no as-at-round parameter (confirmed
# against the site's own client source -- AFL-API-REFERENCE.md, "Endpoint
# family: statspro"), and the 28 extended fields it carries (spoils,
# pressure_acts, ...) are empty in CFS's per-match extendedStats for AFLW. So
# the only route to round-level figures for those fields is to snapshot the
# cumulative table each week and difference consecutive captures --
# diff_aflw_season_snapshots(). This works FORWARD ONLY: rounds played before
# the first snapshot stay season-total-only forever.
#
# Run: powershell.exe -Command 'Rscript "data-raw/01-data/scrape_aflw_season_stats_snapshot.R"'
# (working directory = package root, matching the other data-raw/01-data scripts)
#
# DEPENDENCY: get_afl_player_season_stats() lives on feat/aflw-round-stats-scraper.
# Until that branch and this one are combined, this script only runs from a tree
# containing both.

devtools::load_all(quiet = TRUE)

# SNAPSHOT_SEASON lets the workflow's manual-dispatch input target a specific
# season; unset (the scheduled path) means the current one.
season <- if (exists("SNAPSHOT_SEASON", envir = globalenv(), inherits = FALSE)) {
  as.integer(get("SNAPSHOT_SEASON", envir = globalenv()))
} else {
  get_afl_season()
}
# Always today: the file name records when the capture happened, not which
# round it covers. Back-dating would invent a capture that never occurred and
# corrupt every delta computed against it.
as_of <- Sys.Date()
cat(sprintf("AFLW season-stat snapshot: season %d, as of %s\n", season, as_of))

d <- get_afl_player_season_stats(season, comp = "AFLW")

# Validate before publishing -- the upload below is a real, hard-to-reverse
# public release, and a snapshot that is wrong is worse than one that is
# missing: it silently corrupts every future delta computed against it.
if (is.null(d) || nrow(d) == 0) {
  cli::cli_abort("Season {season}: 0 rows returned -- aborting, not publishing an empty snapshot.")
}
if (any(is.na(d$player_id))) {
  cli::cli_abort("Season {season}: NA player_id present -- aborting.")
}
if (anyDuplicated(d$player_id) > 0) {
  cli::cli_abort(paste0(
    "Season {season}: duplicate player_id values -- aborting. Differencing keys on ",
    "player_id, so duplicates would silently produce a cartesian delta."))
}
if (!"games_played" %in% names(d)) {
  cli::cli_abort("Season {season}: no games_played column -- differencing needs it as the window denominator.")
}
if (nrow(d) < 100) {
  cli::cli_warn("Season {season}: only {nrow(d)} rows -- low for a full AFLW list, check before trusting this snapshot.")
}

# A new snapshot must not go BACKWARDS against the most recent existing one:
# cumulative totals only ever grow within a season, so a fall means the scrape
# caught a partial/erroring response and publishing it would poison the delta.
prior <- tryCatch(list_aflw_season_stat_snapshots(season), error = function(e) NULL)
if (!is.null(prior) && nrow(prior) > 0) {
  last_date <- max(prior$snapshot_date)
  if (last_date == as_of) {
    cli::cli_inform("A snapshot for {as_of} already exists; it will be overwritten with this fresh capture.")
  }
  prev <- tryCatch(load_aflw_season_stats_snapshot(season, as_of = last_date), error = function(e) NULL)
  if (!is.null(prev) && nrow(prev) > 0 && last_date != as_of) {
    chk <- merge(
      data.table::as.data.table(d)[, .(player_id, now = games_played)],
      data.table::as.data.table(prev)[, .(player_id, before = games_played)],
      by = "player_id"
    )
    went_back <- chk[now < before]
    if (nrow(went_back) > 0) {
      cli::cli_abort(paste0(
        "Season {season}: {nrow(went_back)} player{?s} have FEWER games_played than in the ",
        "{last_date} snapshot. Cumulative totals cannot fall -- this capture looks partial. ",
        "Not publishing."))
    }
    cli::cli_inform("Monotonicity check passed against the {last_date} snapshot.")
  }
}

cat(sprintf("Validation passed (%d players). Publishing snapshot.\n", nrow(d)))

save_to_release(
  df = d,
  file_name = .aflw_snapshot_file_name(season, as_of),
  release_tag = AFLW_SNAPSHOT_RELEASE_TAG
)

# The asset listing is cached; without this the next list/load call in the same
# session would not see the file just uploaded.
invalidate_release_cache(AFLW_SNAPSHOT_RELEASE_TAG)

cat("Done.\n")
