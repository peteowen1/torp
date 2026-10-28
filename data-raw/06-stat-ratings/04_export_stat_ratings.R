# 04_export_stat_ratings.R
# ========================
# Export computed player stat ratings to torpdata releases.
#
# Input:  cache-stat-ratings/03_player_stat_ratings.rds
# Output: torpdata player_stat_ratings-data release (parquet per season)

# Setup ----
devtools::load_all()

# Config ----
cache_dir <- file.path("data-raw", "cache-stat-ratings")
release_tag <- "player_stat_ratings-data"

# Load data ----
cli::cli_h1("Loading computed stat ratings")

all_stat_ratings <- readRDS(file.path(cache_dir, "03_player_stat_ratings.rds"))
# Ensure consistent column types for Arrow batch reads
all_stat_ratings$season <- as.integer(all_stat_ratings$season)
all_stat_ratings$round <- as.integer(all_stat_ratings$round)
cli::cli_inform("Total: {nrow(all_stat_ratings)} rows across {length(unique(all_stat_ratings$season))} seasons")

# Export per season ----
cli::cli_h1("Exporting to torpdata")

seasons <- sort(unique(all_stat_ratings$season))

# STAT_RATINGS_EXPORT_SEASONS: "" or "all" exports every season (the manual
# default); "current" exports only get_afl_season() (the nightly workflow,
# torp#289: earlier seasons do not change between nights, so re-uploading
# them every run is pointless); "2025,2026" exports a list.
# STAT_RATINGS_DRY_RUN=1 reports what would be uploaded and uploads nothing.
export_sel <- Sys.getenv("STAT_RATINGS_EXPORT_SEASONS", "")
if (identical(export_sel, "current")) {
  seasons <- intersect(seasons, get_afl_season())
} else if (nzchar(export_sel) && !identical(export_sel, "all")) {
  seasons <- intersect(seasons, as.integer(strsplit(export_sel, ",")[[1]]))
}
if (length(seasons) == 0) {
  cli::cli_abort("STAT_RATINGS_EXPORT_SEASONS={.val {export_sel}} matches no season in the computed stat ratings.")
}
dry_run <- identical(Sys.getenv("STAT_RATINGS_DRY_RUN"), "1")
cli::cli_inform("Exporting {length(seasons)} season{?s}: {paste(seasons, collapse = ', ')}{if (dry_run) ' (DRY RUN)' else ''}")

n_success <- 0
n_fail <- 0

for (szn in seasons) {
  szn_data <- all_stat_ratings[all_stat_ratings$season == szn, ]

  file_name <- paste0("player_stat_ratings_", szn)

  tryCatch({
    if (dry_run) {
      cli::cli_inform("DRY RUN: would upload {file_name} ({nrow(szn_data)} rows, rounds {min(szn_data$round)}-{max(szn_data$round)})")
    } else {
      save_to_release(szn_data, file_name, release_tag)
    }
    if (!dry_run) cli::cli_alert_success("{szn}: {nrow(szn_data)} rows exported")
    n_success <- n_success + 1
  }, error = function(e) {
    cli::cli_warn("Failed to export {szn}: {conditionMessage(e)}")
    n_fail <<- n_fail + 1
  })
}

if (n_success == 0) {
  cli::cli_abort("All {n_fail} exports failed. No data was published.")
}
if (n_fail > 0) {
  cli::cli_warn("{n_fail}/{length(seasons)} season exports failed")
} else {
  cli::cli_alert_success("Export complete!")
}
