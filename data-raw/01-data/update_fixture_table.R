# Setup ----

library(dplyr)
library(purrr)
devtools::load_all()

## Helper Functions ----

safe_fetch <- function(fetch_fn, ...) {
  tryCatch(
    fetch_fn(...),
    error = function(e) {
      cli::cli_warn("Fetch failed: {e$message}")
      NULL
    }
  )
}

has_data <- function(df) {
 !is.null(df) && is.data.frame(df) && nrow(df) > 0
}

season <- get_afl_season()
current_round <- get_afl_week()

cli::cli_h1("AFL Data Update - Season {season}, Round {current_round}")

# 1. Update Fixtures ----
cli::cli_h2("Fixtures")
tictoc::tic()

fixtures_upd <- safe_fetch(get_afl_fixtures, season)

if (has_data(fixtures_upd)) {
  save_to_release(fixtures_upd, glue::glue("fixtures_{season}"), "fixtures-data")
  cli::cli_alert_success("Updated fixtures: {nrow(fixtures_upd)} matches")
} else {
  cli::cli_alert_warning("No fixture data available")
}
tictoc::toc()

# 2. Update Team Lineups ----
cli::cli_h2("Team Lineups")
tictoc::tic()

# Only fetch lineups if season has started (round > 0)
if (current_round > 0) {
  rounds_to_fetch <- max(0, current_round - 1):min(current_round + 1, 28)

  teams_upd <- safe_fetch(get_afl_lineups, season, round = rounds_to_fetch)

  if (has_data(teams_upd)) {
    save_to_release(teams_upd, glue::glue("teams_{season}"), "teams-data")
    cli::cli_alert_success("Updated lineups: {nrow(teams_upd)} player entries")
  } else {
    cli::cli_alert_warning("No lineup data available for rounds {min(rounds_to_fetch)}-{max(rounds_to_fetch)}")
  }
} else {
  cli::cli_alert_info("Pre-season: skipping lineup updates")
}
tictoc::toc()

# 3. Update Results ----
cli::cli_h2("Results")
tictoc::tic()

results_upd <- safe_fetch(get_afl_results, season)

if (has_data(results_upd)) {
  save_to_release(results_upd, glue::glue("results_{season}"), "results-data")
  cli::cli_alert_success("Updated results: {nrow(results_upd)} matches")
} else {
  cli::cli_alert_info("No results yet for season {season}")
}
tictoc::toc()

# 4. Update Player Details ----
cli::cli_h2("Player Details")
tictoc::tic()

player_details_upd <- safe_fetch(get_afl_player_details, season)

if (has_data(player_details_upd)) {
  save_to_release(player_details_upd, glue::glue("player_details_{season}"), "player_details-data")
  cli::cli_alert_success("Updated player details: {nrow(player_details_upd)} players")
} else {
  cli::cli_alert_warning("No player details available")
}
tictoc::toc()

# 5. Update Player Stats ----
cli::cli_h2("Player Stats")
tictoc::tic()

# Only fetch stats if games have been played
if (current_round > 0) {
  player_stats_upd <- safe_fetch(get_afl_player_stats, season)

  if (has_data(player_stats_upd)) {
    player_stats_upd <- player_stats_upd %>%
      janitor::remove_constant() %>%
      janitor::clean_names()
    save_to_release(player_stats_upd, glue::glue("player_stats_{season}"), "player_stats-data")
    cli::cli_alert_success("Updated player stats: {nrow(player_stats_upd)} entries")
  } else {
    cli::cli_alert_warning("No player stats available")
  }
} else {
  cli::cli_alert_info("Pre-season: skipping player stats")
}
tictoc::toc()

cli::cli_h1("Update Complete")
