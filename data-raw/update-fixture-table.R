library(dplyr)
devtools::load_all()
### update fixtures file
fixtures <- purrr::map_df(2021:2023,~fitzRoy::fetch_fixture_afl(.,comp = "AFLM"))

usethis::use_data(fixtures,overwrite = TRUE)

### update teams file
# teams <- purrr::map_df(2021:most_recent_season(),~fitzRoy::fetch_lineup_afl(., comp = "AFLM")) %>%
#   dplyr::mutate(season = as.numeric(substr(providerId, 5, 8)),
#                 row_id = paste0(providerId,player.playerId))

team_wk <- fitzRoy::fetch_lineup_afl(most_recent_season(),get_current_week()) %>%
  #dplyr::filter(status != "SCHEDULED") %>%
  dplyr::mutate(season = as.numeric(substr(providerId, 5, 8)),
         row_id = paste0(providerId,player.playerId)
         )

teams <- dplyr::rows_insert(torp::teams,team_wk,"row_id", conflict = "ignore")
teams <- dplyr::rows_update(teams,team_wk,"row_id", unmatched = "ignore" )

usethis::use_data(teams,overwrite = TRUE)

### update results file
results <- purrr::map_df(2021:most_recent_season(),~fitzRoy::fetch_results_afl(., comp = "AFLM"))

usethis::use_data(results,overwrite = TRUE)

