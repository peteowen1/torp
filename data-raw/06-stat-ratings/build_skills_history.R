# build_skills_history.R  — issue #87
# ============================================================
# Produce a single per-round, per-player, per-season historical snapshot of all
# skill (*_rating) columns for the blog AFL Age Curves page.
#
# KEY INSIGHT (verified 2026-06-18): the per-round historical stat ratings are
# ALREADY computed and released. `03_estimate_stat_ratings.R` calls
# `.estimate_stat_ratings_batch()` for every (season, round) ref_date across ALL
# seasons the source supports, and `04_export_stat_ratings.R` uploads them
# per-season to the `player_stat_ratings-data` release
# (player_stat_ratings_<season>.parquet, 2021..current). Confirmed identical
# 234-col / 56 *_rating-col schema to the `player_skills_<year>.parquet` the blog
# already consumes — so the history file is column-compatible by construction.
#
# This issue is therefore a PACKAGING change, not a recompute:
#   - The blog only ever ships the CURRENT-year file (build-blog-data.yml pulls
#     player_skills_${YEAR}.parquet -> player-skills.parquet).
#   - We concatenate every season's per-round table, keep only the *_rating
#     columns (+ id/metadata) to control file size, and publish it as a single
#     `player_skills_history.parquet` into the existing player_skills-data
#     release (keeps the blog's download step in one place).
#
# Run AFTER 03_estimate_stat_ratings.R (which writes the cache rds), OR let it
# pull the already-published per-season parquets straight from the release
# (default when the local cache is absent).
#
# Local output: data-raw/cache-stat-ratings/player_skills_history.parquet
# Release      : player_skills-data  ->  player_skills_history.parquet
#
# DRY_RUN = TRUE (default) writes the local parquet but does NOT upload.
# Set DRY_RUN = FALSE (or env TORP_SKILLS_HISTORY_PUBLISH=1) to publish.
# ------------------------------------------------------------

suppressMessages({
  library(arrow)
  library(data.table)
})

DRY_RUN <- !identical(Sys.getenv("TORP_SKILLS_HISTORY_PUBLISH"), "1")

src_release <- "player_stat_ratings-data"   # source: per-season per-round history
out_release <- "player_skills-data"         # target: blog download step lives here
out_stem    <- "player_skills_history"

cache_dir <- file.path("data-raw", "cache-stat-ratings")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
out_path  <- file.path(cache_dir, paste0(out_stem, ".parquet"))

# --- Source: prefer local cache rds from 03_, else download release parquets ---
cache_rds <- file.path(cache_dir, "03_player_stat_ratings.rds")

if (file.exists(cache_rds)) {
  cli::cli_inform("Reading per-round ratings from local cache {.path {cache_rds}}")
  all_ratings <- data.table::as.data.table(readRDS(cache_rds))
} else {
  cli::cli_inform("Local cache absent - downloading per-season parquets from {.val {src_release}}")
  dl_dir <- file.path(cache_dir, "_history_src")
  dir.create(dl_dir, showWarnings = FALSE, recursive = TRUE)
  # Plain args (no shQuote): gh handles the glob; avoids Windows single-quote issues.
  status <- system2("gh", c("release", "download", src_release,
                            "-R", "peteowen1/torpdata",
                            "-p", "player_stat_ratings_*.parquet",
                            "-D", dl_dir, "--clobber"))
  if (!identical(status, 0L)) {
    cli::cli_abort("gh release download failed (exit {status}). Is gh authenticated?")
  }
  files <- list.files(dl_dir, pattern = "^player_stat_ratings_\\d{4}\\.parquet$",
                      full.names = TRUE)
  if (length(files) == 0) cli::cli_abort("No per-season parquets downloaded.")
  cli::cli_inform("Concatenating {length(files)} season file{?s}")
  all_ratings <- data.table::rbindlist(lapply(files, arrow::read_parquet),
                                       fill = TRUE)
}

cli::cli_inform("Loaded {nrow(all_ratings)} player-round rows, {ncol(all_ratings)} cols")
cli::cli_inform("Seasons: {paste(sort(unique(all_ratings$season)), collapse = ', ')}")

# --- Keep id/metadata + every *_rating column; drop the _raw/_n80s/_wt80s/
#     _attempts sidecars. Verified ~2.6x size cut (30MB -> ~11MB/season). ---
nm <- names(all_ratings)
meta_cols   <- intersect(c("player_id", "player_name", "pos_group",
                           "season", "round", "n_games", "wt_games"), nm)
rating_cols <- nm[grepl("_rating$", nm)]
keep_cols   <- c(meta_cols, rating_cols)

history <- all_ratings[, keep_cols, with = FALSE]

# Type hygiene for stable Arrow reads downstream
history[, season := as.integer(season)]
history[, round  := as.integer(round)]
data.table::setcolorder(history, meta_cols)
data.table::setorder(history, player_id, season, round)

cli::cli_inform(
  "History table: {nrow(history)} rows x {ncol(history)} cols ({length(rating_cols)} rating cols)"
)

# --- Sanity checks before publishing ---
stopifnot(
  nrow(history) > 50000,               # expect ~150-200k across seasons
  length(rating_cols) >= 50,           # ~56 rating cols today
  length(unique(history$season)) >= 4, # multi-season
  !anyNA(history$player_id),
  !anyNA(history$season),
  !anyNA(history$round)
)

arrow::write_parquet(history, out_path)
cli::cli_alert_success(
  "Wrote {.path {out_path}} ({round(file.size(out_path) / 1048576, 1)} MB)"
)

# --- Publish to the player_skills-data release (single file, all seasons) ---
if (DRY_RUN) {
  cli::cli_alert_info(
    "DRY_RUN: skipped upload. Set TORP_SKILLS_HISTORY_PUBLISH=1 to publish to {.val {out_release}}."
  )
} else {
  devtools::load_all(quiet = TRUE)   # for save_to_release()
  save_to_release(as.data.frame(history), out_stem, out_release)
  cli::cli_alert_success("Published {out_stem}.parquet to {.val {out_release}}")
}
