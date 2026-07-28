# Refresh the local torpdata/data directory from the releases
# ===========================================================
# WHY THIS EXISTS. Every load_*() silently prefers a local torpdata/data
# sibling over the GitHub release (get_local_data_dir()). That is a good
# zero-network dev default and a bad rebuild input: a file here can be months
# older than the release and nothing says so.
#
# It is not hypothetical. On 2026-07-27 the ratings rebuild's Stage 3 read
# local player_game_2022 and player_game_2025 that were three months stale and
# predated the metric-first column rename. Their _adj columns read as all-NA,
# every _oadj for those seasons went NA, and EPR deflated both seasons as
# 1/wt_gms -- published, silently, until it was measured a day later.
#
# A 2026-07-28 audit found the staleness is systemic, not confined to those
# two files: 12 dataset families were mixed-vintage with ~100-day spreads
# (results, predictions, player_stats, teams, player_game, player_game_ratings,
# player_season_ratings, psr, retrodictions, xg_data, player_stat_ratings,
# fixtures).
#
# DEFAULT IS DRY RUN. This overwrites files in a working directory, so it
# reports exactly what it would replace and changes nothing unless asked:
#   Rscript refresh_local_data_dir.R           # report only
#   Rscript refresh_local_data_dir.R --apply   # actually download
#
# Only files that ALREADY EXIST locally are refreshed -- this mirrors the
# release into the local mirror, it does not invent new local state. Anything
# with no matching release asset is reported and left alone.

suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

APPLY <- "--apply" %in% commandArgs(trailingOnly = TRUE)
STALE_DAYS <- 7

local_dir <- get_local_data_dir()
if (is.null(local_dir)) cli::cli_abort("No local data dir found -- nothing to refresh.")
cli::cli_h1("Local data dir: {.path {local_dir}}")

local_files <- list.files(local_dir, pattern = "\\.parquet$", full.names = TRUE)
if (length(local_files) == 0) cli::cli_abort("No parquet files in {.path {local_dir}}")

# Map every release asset -> its download URL, across the tags that feed ratings.
tags <- c("pbp-data", "chains-data", "player_game-data", "ratings-data",
          "player_game_ratings-data", "player_season_ratings-data",
          "player_skills-data", "player_stat_ratings-data", "psr-data",
          "xg-data", "weather-data", "predictions", "retrodictions",
          "results-data", "reference-data")
repo <- get_torp_data_repo()

cli::cli_progress_step("Listing release assets")
assets <- rbindlist(lapply(tags, function(tg) {
  a <- tryCatch(vb_list_assets(repo, tg), error = function(e) NULL)
  if (is.null(a) || nrow(a) == 0) return(NULL)
  data.table(tag = tg, name = a$name, size = as.numeric(a$size),
             updated_at = a$updated_at)
}), fill = TRUE)
assets <- assets[grepl("\\.parquet$", name)]
cli::cli_inform("{nrow(assets)} parquet assets across {uniqueN(assets$tag)} tags")

loc <- data.table(path = local_files, name = basename(local_files),
                  local_mtime = file.mtime(local_files),
                  local_size = file.size(local_files))
m <- merge(loc, assets, by = "name", all.x = TRUE)
m[, rel_time := suppressWarnings(as.POSIXct(updated_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))]
m[, age_days := round(as.numeric(difftime(Sys.time(), local_mtime, units = "days")), 1)]
m[, size_differs := !is.na(size) & abs(size - local_size) > 0]
m[, older_than_release := !is.na(rel_time) & local_mtime < rel_time]
m[, needs_refresh := (older_than_release | size_differs) & !is.na(tag)]

cli::cli_h2("Assessment")
cli::cli_inform("local parquet files      : {nrow(m)}")
cli::cli_inform("no matching release asset: {sum(is.na(m$tag))}")
cli::cli_inform("stale (>{STALE_DAYS}d old)         : {sum(m$age_days > STALE_DAYS)}")
cli::cli_inform("WOULD REFRESH            : {sum(m$needs_refresh)}")

if (any(is.na(m$tag))) {
  cli::cli_h3("No release asset -- left untouched")
  print(m[is.na(tag), .(name, age_days)], row.names = FALSE)
}

if (any(m$needs_refresh)) {
  cli::cli_h3("Would be replaced")
  print(m[needs_refresh == TRUE][order(-age_days)][
    , .(name, tag, local = format(local_mtime, "%Y-%m-%d"),
        release = format(rel_time, "%Y-%m-%d"), age_days,
        mb = round(local_size / 1e6, 1))], row.names = FALSE)
}

if (!APPLY) {
  cli::cli_alert_info("DRY RUN -- nothing written. Re-run with {.code --apply} to download.")
  quit(status = 0)
}

todo <- m[needs_refresh == TRUE]
if (nrow(todo) == 0) { cli::cli_alert_success("Already current -- nothing to do."); quit(status = 0) }

cli::cli_h2("Applying: refreshing {nrow(todo)} file{?s}")
bak <- file.path(local_dir, paste0("_backup_", format(Sys.time(), "%Y%m%d_%H%M%S")))
dir.create(bak, showWarnings = FALSE)
cli::cli_inform("Backing up replaced files to {.path {bak}}")

ok <- 0; failed <- character(0)
for (i in seq_len(nrow(todo))) {
  nm <- todo$name[i]
  url <- sprintf("https://github.com/%s/releases/download/%s/%s", repo, todo$tag[i], nm)
  dest <- todo$path[i]
  tmp <- paste0(dest, ".tmp")
  res <- tryCatch({
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    # Verify before replacing: a truncated download must not overwrite a good file.
    if (!file.exists(tmp) || file.size(tmp) == 0) stop("empty download")
    invisible(arrow::read_parquet(tmp, as_data_frame = FALSE))  # parses?
    file.copy(dest, file.path(bak, nm), overwrite = TRUE)
    file.rename(tmp, dest)
    TRUE
  }, error = function(e) { cli::cli_warn("{nm}: {conditionMessage(e)}"); FALSE })
  unlink(tmp)
  if (isTRUE(res)) ok <- ok + 1 else failed <- c(failed, nm)
}

cli::cli_h2("Result")
cli::cli_alert_success("refreshed {ok} of {nrow(todo)}")
if (length(failed) > 0) {
  cli::cli_alert_danger("failed: {.val {failed}} -- originals left in place")
}
cli::cli_inform("Backups: {.path {bak}}")
