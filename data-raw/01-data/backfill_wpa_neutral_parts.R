# Add wpa_neutral_own / wpa_neutral_won / wpa_neutral_team to the published
# player_game_ratings_{season} files (one-off, 2026-09-28): the files torpdata
# builds the blog's game-logs from.
#
# Why: the blog's default WPA is the even-start ledger (wpa_neutral), and it
# needs the same Won back / Own acts / Team share breakdown net points and
# wpa_net have. create_player_game_data() keeps them from now on (Step 3c);
# this fills the seasons already published.
#
# Only the three new columns are written. Everything else in each file is
# kept exactly as published: rebuilding the whole file from current code
# could shift other columns for past seasons. As a check that the inputs and
# code match what produced the file, the recomputed total must equal the
# published wpa_neutral (to 1e-6) or the season is not written.
#
# Usage (from the torp repo root, GITHUB_TOKEN set):
#   Rscript data-raw/01-data/backfill_wpa_neutral_parts.R            # 2021 to current
#   Rscript data-raw/01-data/backfill_wpa_neutral_parts.R 2026       # one season
#   Rscript data-raw/01-data/backfill_wpa_neutral_parts.R --dry-run  # check only
#   Rscript data-raw/01-data/backfill_wpa_neutral_parts.R --replace  # new start
#
# --replace: WPA_NEUTRAL_HOME_PROB changed (0.57 -> 0.5, 2026-09-28), so the
# published wpa_neutral is replaced along with its parts. The equality check
# cannot apply; instead every team's players must add up to exactly +0.5
# (won), -0.5 (lost) or 0 (drew), and the parts to the total.

suppressMessages(devtools::load_all(quiet = TRUE))
suppressMessages(library(data.table))
# Read the PUBLISHED release, never a local torpdata/data copy: this script
# patches the release, and run from the torp folder the loaders would
# otherwise find ../torpdata/data and read old local files.
options(torp.local_data_dir = NA)

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
replace <- "--replace" %in% args
yrs <- suppressWarnings(as.integer(args[!grepl("^--", args)]))
seasons <- if (length(yrs)) yrs else 2021:get_afl_season()
parts <- c("wpa_neutral_own", "wpa_neutral_won", "wpa_neutral_team")

for (season in seasons) {
  cli::cli_h2("player_game_ratings_{season}")
  pgd <- data.table::as.data.table(load_player_game_ratings(season))
  if (!"wpa_neutral" %in% names(pgd)) stop(season, ": published file has no wpa_neutral to check against")
  pbp <- data.table::as.data.table(load_pbp(season))
  pstats <- data.table::as.data.table(load_player_stats(season))

  pm <- .wpa_neutral_pre_match(pbp$match_id)
  wpn <- build_wpa_ledger(pbp, pstats, pm)
  skipped <- attr(wpn, "skipped")
  keep <- pgd[!as.character(match_id) %in% skipped, .(player_id, match_id)]
  dt <- .wpa_respread_lost(wpn, keep, pbp, pstats)
  data.table::setnames(dt, c("wpa_net", "wpa_own", "wpa_won", "wpa_team"),
                       c(".chk", "wpa_neutral_own", "wpa_neutral_won", "wpa_neutral_team"))
  dt <- dt[, c("player_id", "match_id", ".chk", parts), with = FALSE]

  for (cc in intersect(parts, names(pgd))) pgd[, (cc) := NULL]
  old_total <- if (replace) pgd$wpa_neutral else NULL
  out <- merge(pgd, dt, by = c("player_id", "match_id"), all.x = TRUE, sort = FALSE)
  if (nrow(out) != nrow(pgd)) stop(season, ": the merge changed the row count")
  # Same zero / NA rule as .wpa_attach(): a rated match with no ledger row is a
  # genuine zero, a skipped match stays NA.
  rated <- !as.character(out$match_id) %in% skipped
  for (cc in c(".chk", parts)) data.table::set(out, i = which(rated & is.na(out[[cc]])), j = cc, value = 0)

  both <- !is.na(out$wpa_neutral) & !is.na(out$.chk)
  gap <- if (any(both)) max(abs(out$wpa_neutral[both] - out$.chk[both])) else NA_real_
  miss <- sum(is.na(out$.chk) != is.na(out$wpa_neutral))
  sum_gap <- max(abs(out$wpa_neutral_own + out$wpa_neutral_won + out$wpa_neutral_team - out$wpa_neutral), na.rm = TRUE)
  cli::cli_inform("{nrow(out)} rows | recomputed vs published wpa_neutral: max gap {signif(gap, 3)}, NA disagreements {miss} | parts vs total: max gap {signif(sum_gap, 3)}")
  if (replace) {
    # The total is replaced; check it lands on the result from a 50/50 start.
    out[, wpa_neutral := .chk]
    tt <- out[!is.na(wpa_neutral), .(total = sum(wpa_neutral)), by = .(match_id, team)]
    off <- tt[pmin(abs(total - 0.5), abs(total + 0.5), abs(total)) > 1e-6]
    cli::cli_inform("replace: {nrow(tt)} team totals, {nrow(off)} not on +0.5 / -0.5 / 0 | total moved by up to {signif(max(abs(out$wpa_neutral - old_total), na.rm = TRUE), 3)}")
    if (nrow(off) > 0) stop(season, ": ", nrow(off), " team totals are not +0.5, -0.5 or 0, e.g. ", off$match_id[1])
  } else if (!isTRUE(gap < 1e-6) || miss > 0) {
    stop(season, ": recomputed wpa_neutral does not match the published file, so the parts would not ",
         "belong to it. Rebuild the season in full instead.")
  }
  # Checked on the total being written (the recomputed one under --replace).
  sum_gap <- max(abs(out$wpa_neutral_own + out$wpa_neutral_won + out$wpa_neutral_team - out$wpa_neutral), na.rm = TRUE)
  if (!isTRUE(sum_gap < 1e-6)) stop(season, ": the three parts do not add up to wpa_neutral (max gap ", signif(sum_gap, 3), ")")
  out[, .chk := NULL]
  data.table::setcolorder(out, c(names(pgd), parts))

  if (dry_run) { cli::cli_inform("dry run: not uploading"); next }
  save_to_release(out, paste0("player_game_ratings_", season), "player_game_ratings-data", prev_rows_floor = 0.99)
  cli::cli_alert_success("player_game_ratings_{season}: added {length(parts)} columns")
}
