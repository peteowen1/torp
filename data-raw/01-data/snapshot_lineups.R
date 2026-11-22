# Snapshot the AFL team lists for nearby matches and append them to the
# teams-snapshots release on torpdata (see R/lineup_snapshots.R).
#
# Run hourly by torpdata's lineup-snapshots.yml. Off-season, and on any run
# with no match within the window, it fetches the fixtures and stops: nothing
# is uploaded.
#
#   Rscript data-raw/01-data/snapshot_lineups.R

devtools::load_all(quiet = TRUE)

season <- get_afl_season()
snap <- snapshot_afl_lineups(season)
if (nrow(snap$log) > 0) {
  log <- save_lineup_snapshots(snap, season)
  cli::cli_inform("Saved snapshot log for {nrow(log)} match{?es}; {sum(log$changed)} new list version{?s}.")
}
