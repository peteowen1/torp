# 01_compute_match_stats.R
# ========================
# Assemble per-player-match stat table for skill estimation.
#
# Input:  player_game_data + player_stats from torpdata releases
# Output: data-raw/cache-skills/01_skill_data.rds

# Setup ----
devtools::load_all()

# Config ----
cache_dir <- file.path("data-raw", "cache-skills")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

# Load data ----
cli::cli_h1("Loading data")

pgd <- load_player_game_data(TRUE, use_disk_cache = TRUE)
cli::cli_inform("Player game data: {nrow(pgd)} rows, {length(unique(pgd$player_id))} players")

ps <- load_player_stats(TRUE, use_disk_cache = TRUE)
cli::cli_inform("Player stats: {nrow(ps)} rows")

# Prepare ----
cli::cli_h1("Preparing skill data")

skill_data <- prepare_skill_data(pgd, ps)

cli::cli_inform("Skill data: {nrow(skill_data)} rows, {length(unique(skill_data$player_id))} players")
cli::cli_inform("Date range: {min(skill_data$match_date_skill)} to {max(skill_data$match_date_skill)}")
cli::cli_inform("Position groups: {paste(sort(unique(skill_data$pos_group)), collapse = ', ')}")

# Save ----
saveRDS(skill_data, file.path(cache_dir, "01_skill_data.rds"))
cli::cli_alert_success("Saved to {file.path(cache_dir, '01_skill_data.rds')}")
