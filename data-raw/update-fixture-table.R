library(dplyr)
library(mgcv)
# devtools::install_github('peteowen1/fitzRoy')
library(fitzRoy)
devtools::load_all()

### update fixtures file (17 secs)
tictoc::tic()
fixtures_upd <- purrr::map(lubridate::year(Sys.Date()), ~ fitzRoy::fetch_fixture_afl(.x, comp = "AFLM")) %>% purrr::list_rbind()
tictoc::toc()

# fixtures <- fixtures_upd # load_fixtures() %>% rows_upsert(fixtures_upd, by = "providerId")
# usethis::use_data(fixtures, overwrite = TRUE)

### update teams file (90 secs per season)
library(furrr)
plan("multisession", workers = (parallelly::availableCores() - 2))
# rows_upsert()
tictoc::tic() #### CHANGE FITZROY FUNCTION TO FUTURE_MAP
teams_upd <- purrr::map((get_afl_week() - 1):(get_afl_week() + 1), ~ fitzRoy::fetch_lineup_afl(get_afl_season(), .x, comp = "AFLM")) %>%
  purrr::list_rbind() %>%
  dplyr::mutate(
    season = as.numeric(substr(providerId, 5, 8)),
    row_id = paste0(providerId, teamId, player.playerId)
  ) # %>% dplyr::filter(!is.na(player.playerId))
tictoc::toc()

# teams <- load_teams(TRUE) %>%
#   rows_upsert(teams_upd, by = "row_id") %>%
#   filter(!is.na(teamId), !is.na(player.playerId)) %>%
#   arrange(providerId)
# 
# usethis::use_data(teams, overwrite = TRUE)

##### update results file (5 secs per season)
tictoc::tic()
results_upd <- purrr::map_df(get_afl_season(), ~ fitzRoy::fetch_results_afl(., comp = "AFLM"))
tictoc::toc()

# results <- load_results(TRUE) %>% rows_upsert(results_upd, by = "match.matchId")
# usethis::use_data(results, overwrite = TRUE)

######## update players data (3 mins)
# tictoc::tic()
# plyr_tm_db <- fitzRoy::fetch_player_details(current = FALSE, comp = "AFLM",source = "AFL") %>%
#   dplyr::mutate(player_name = paste(firstName,surname),
#                 age = lubridate::decimal_date(lubridate::as_date(glue::glue('{season}-07-01')))-
#                       lubridate::decimal_date(lubridate::as_date(dateOfBirth)),
#                 row_id = paste(providerId,season))
# tictoc::toc()
# usethis::use_data(plyr_tm_db, overwrite = TRUE)

##### update players file (20 secs per season)
tictoc::tic()
plyr_tm_df_upd <-
  fitzRoy::fetch_player_details_afl(season = get_afl_season(), comp = "AFLM") %>%
  dplyr::mutate(
    player_name = paste(firstName, surname),
    age = lubridate::decimal_date(lubridate::as_date(glue::glue("{season}-07-01"))) -
      lubridate::decimal_date(lubridate::as_date(dateOfBirth)),
    row_id = paste(providerId, season)
  )
tictoc::toc()

plyr_tm_df <- load_player_details(TRUE) %>% rows_upsert(plyr_tm_df_upd, by = "row_id")
# usethis::use_data(plyr_tm_df, overwrite = TRUE)
