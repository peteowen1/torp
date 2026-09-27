# Backfill score_events_{season} and match_items_{season} to the torpdata
# score_events-data release (one-off, 2026-09-27; the daily release keeps the
# current season up to date via update_season_score_events()).
#
# Whole seasons are fetched and uploaded in one go, so a rerun replaces the
# file rather than appending. One API call per match (~220 per season).
#
# Usage (from the torp repo root, with GITHUB_TOKEN set):
#   Rscript data-raw/01-data/backfill_score_events.R            # 2021 to current
#   Rscript data-raw/01-data/backfill_score_events.R 2024 2025  # specific seasons

suppressMessages(devtools::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
seasons <- if (length(args) > 0) as.integer(args) else 2021:get_afl_season()

for (season in seasons) {
  cli::cli_h2("score events {season}")
  events <- get_match_score_events(season)
  items <- attr(events, "match_items")
  n_types <- events[, .N, by = score_type]
  cli::cli_inform("{nrow(items)} matches, {nrow(events)} events: {paste(n_types$score_type, n_types$N, collapse = ', ')}")
  save_to_release(df = events, file_name = paste0("score_events_", season), release_tag = "score_events-data")
  save_to_release(df = items, file_name = paste0("match_items_", season), release_tag = "score_events-data")
}
