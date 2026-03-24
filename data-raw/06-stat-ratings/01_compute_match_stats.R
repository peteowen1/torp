# 01_compute_match_stats.R
# ========================
# Assemble per-player-match stat table for stat rating estimation.
#
# Input:  player_game_data + player_stats from torpdata releases
# Output: data-raw/cache-stat-ratings/01_stat_rating_data.rds

# Setup ----
devtools::load_all()

# Config ----
cache_dir <- file.path("data-raw", "cache-stat-ratings")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

# Load data ----
cli::cli_h1("Loading data")

pgd <- load_player_game_data(TRUE, use_disk_cache = TRUE)
cli::cli_inform("Player game data: {nrow(pgd)} rows, {length(unique(pgd$player_id))} players")

ps <- load_player_stats(TRUE)
cli::cli_inform("Player stats: {nrow(ps)} rows")

# Load rosters for all seasons (for zero-TOG expansion of non-selected players)
seasons <- sort(unique(pgd$season))
rosters <- data.table::rbindlist(lapply(seasons, load_player_details), fill = TRUE)
cli::cli_inform("Rosters: {nrow(rosters)} player-seasons across {length(seasons)} seasons")

# Load fixtures (for team-round calendar: handles byes + finals correctly)
fixtures <- load_fixtures(all = TRUE, use_disk_cache = TRUE)
cli::cli_inform("Fixtures: {nrow(fixtures)} rows")

# Prepare ----
cli::cli_h1("Preparing stat rating data")

stat_rating_data <- .prepare_stat_rating_data(pgd, ps, rosters = rosters, fixtures = fixtures)

cli::cli_inform("Stat rating data: {nrow(stat_rating_data)} rows, {length(unique(stat_rating_data$player_id))} players")
cli::cli_inform("Date range: {min(stat_rating_data$match_date_rating)} to {max(stat_rating_data$match_date_rating)}")
cli::cli_inform("Position groups: {paste(sort(unique(stat_rating_data$pos_group)), collapse = ', ')}")

# Save ----
saveRDS(stat_rating_data, file.path(cache_dir, "01_stat_rating_data.rds"))
cli::cli_alert_success("Saved to {file.path(cache_dir, '01_stat_rating_data.rds')}")
