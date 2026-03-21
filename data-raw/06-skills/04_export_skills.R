# 04_export_skills.R
# ==================
# Export computed player stat ratings to torpdata releases.
#
# Input:  cache-skills/03_player_skills.rds
# Output: torpdata player_skills-data release (parquet per season)

# Setup ----
devtools::load_all()

# Config ----
cache_dir <- file.path("data-raw", "cache-skills")
release_tag <- "player_skills-data"

# Load data ----
cli::cli_h1("Loading computed stat ratings")

all_stat_ratings <- readRDS(file.path(cache_dir, "03_player_skills.rds"))
# Ensure consistent column types for Arrow batch reads
all_stat_ratings$season <- as.integer(all_stat_ratings$season)
all_stat_ratings$round <- as.integer(all_stat_ratings$round)
cli::cli_inform("Total: {nrow(all_stat_ratings)} rows across {length(unique(all_stat_ratings$season))} seasons")

# Export per season ----
cli::cli_h1("Exporting to torpdata")

seasons <- sort(unique(all_stat_ratings$season))

n_success <- 0
n_fail <- 0

for (szn in seasons) {
  szn_data <- all_stat_ratings[all_stat_ratings$season == szn, ]

  file_name <- paste0("player_skills_", szn)

  tryCatch({
    save_to_release(szn_data, file_name, release_tag)
    cli::cli_alert_success("{szn}: {nrow(szn_data)} rows exported")
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
