# 04_export_skills.R
# ==================
# Export computed player skills to torpdata releases.
#
# Input:  cache-skills/03_player_skills.rds
# Output: torpdata player_skills-data release (parquet per season)

# Setup ----
devtools::load_all()

# Config ----
cache_dir <- file.path("data-raw", "cache-skills")
release_tag <- "player_skills-data"

# Load data ----
cli::cli_h1("Loading computed skills")

all_skills <- readRDS(file.path(cache_dir, "03_player_skills.rds"))
# Ensure consistent column types for Arrow batch reads
all_skills$season <- as.integer(all_skills$season)
all_skills$round <- as.integer(all_skills$round)
cli::cli_inform("Total: {nrow(all_skills)} rows across {length(unique(all_skills$season))} seasons")

# Export per season ----
cli::cli_h1("Exporting to torpdata")

seasons <- sort(unique(all_skills$season))

n_success <- 0
n_fail <- 0

for (szn in seasons) {
  szn_data <- all_skills[all_skills$season == szn, ]

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
