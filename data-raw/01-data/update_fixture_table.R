library(dplyr)
library(purrr)
library(fitzRoy)
devtools::load_all()

# Helper: safely fetch data, return NULL on failure
safe_fetch <- function(fetch_fn, ...) {

  tryCatch(
    fetch_fn(...),
    error = function(e) {
      cli::cli_warn("Fetch failed: {e$message}")
      NULL
    }
  )
}

# Helper: check if dataframe has data
has_data <- function(df) {
 !is.null(df) && is.data.frame(df) && nrow(df) > 0
}

season <- get_afl_season()
current_round <- get_afl_week()

cli::cli_h1("AFL Data Update - Season {season}, Round {current_round}")

# 1. Update fixtures --------------------------------------------------------
cli::cli_h2("Fixtures")
tictoc::tic()

fixtures_upd <- safe_fetch(fitzRoy::fetch_fixture_afl, season, comp = "AFLM")

if (has_data(fixtures_upd)) {
  save_to_release(fixtures_upd, glue::glue("fixtures_{season}"), "fixtures-data")
  cli::cli_alert_success("Updated fixtures: {nrow(fixtures_upd)} matches")
} else {
  cli::cli_alert_warning("No fixture data available")
}
tictoc::toc()

# 2. Update team lineups ----------------------------------------------------
cli::cli_h2("Team Lineups")
tictoc::tic()

# Only fetch lineups if season has started (round > 0)
if (current_round > 0) {
  rounds_to_fetch <- max(0, current_round - 1):min(current_round + 1, 28)

  teams_list <- map(rounds_to_fetch, ~ {
    result <- safe_fetch(fitzRoy::fetch_lineup_afl, season, .x, comp = "AFLM")
    if (has_data(result)) result else NULL
  })

  teams_upd <- compact(teams_list) %>% list_rbind()

  if (has_data(teams_upd)) {
    teams_upd <- teams_upd %>%
      mutate(
        season = as.numeric(substr(providerId, 5, 8)),
        row_id = paste0(providerId, teamId, player.playerId)
      )
    save_to_release(teams_upd, glue::glue("teams_{season}"), "teams-data")
    cli::cli_alert_success("Updated lineups: {nrow(teams_upd)} player entries")
  } else {
    cli::cli_alert_warning("No lineup data available for rounds {min(rounds_to_fetch)}-{max(rounds_to_fetch)}")
  }
} else {
  cli::cli_alert_info("Pre-season: skipping lineup updates")
}
tictoc::toc()

# 3. Update results ---------------------------------------------------------
cli::cli_h2("Results")
tictoc::tic()

results_upd <- safe_fetch(fitzRoy::fetch_results_afl, season, comp = "AFLM")

if (has_data(results_upd)) {
  save_to_release(results_upd, glue::glue("results_{season}"), "results-data")
  cli::cli_alert_success("Updated results: {nrow(results_upd)} matches")
} else {
  cli::cli_alert_info("No results yet for season {season}")
}
tictoc::toc()

# 4. Update player details --------------------------------------------------
cli::cli_h2("Player Details")
tictoc::tic()

player_details_upd <- safe_fetch(fitzRoy::fetch_player_details_afl, season = season, comp = "AFLM")

if (has_data(player_details_upd)) {
  player_details_upd <- player_details_upd %>%
    mutate(
      player_name = paste(firstName, surname),
      age = lubridate::decimal_date(lubridate::as_date(glue::glue("{season}-07-01"))) -
        lubridate::decimal_date(lubridate::as_date(dateOfBirth)),
      row_id = paste(providerId, season)
    )
  save_to_release(player_details_upd, glue::glue("player_details_{season}"), "player_details-data")
  cli::cli_alert_success("Updated player details: {nrow(player_details_upd)} players")
} else {
  cli::cli_alert_warning("No player details available")
}
tictoc::toc()

# 5. Update player stats ----------------------------------------------------
cli::cli_h2("Player Stats")
tictoc::tic()

# Only fetch stats if games have been played
if (current_round > 0) {
  player_stats_upd <- safe_fetch(fitzRoy::fetch_player_stats_afl, season)

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
