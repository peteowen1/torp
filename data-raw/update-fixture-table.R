library(dplyr)
library(mgcv)
# devtools::install_github('peteowen1/fitzRoy')
library(fitzRoy)
devtools::load_all()

### update fixtures file (7 secs)
tictoc::tic()
fixtures <- purrr::map_df(2021:lubridate::year(Sys.Date()), ~ fitzRoy::fetch_fixture_afl(., comp = "AFLM"))
tictoc::toc()
usethis::use_data(fixtures, overwrite = TRUE)

### update teams file (6 mins)
# rows_upsert()
tictoc::tic()
teams_upd <- purrr::map_df(get_afl_season(), ~ fitzRoy::fetch_lineup_afl(., comp = "AFLM")) %>%
  dplyr::mutate(
    season = as.numeric(substr(providerId, 5, 8)),
    row_id = paste0(providerId, teamId, player.playerId)
  ) # %>% dplyr::filter(!is.na(player.playerId))
tictoc::toc()

teams <- torp::teams %>% rows_upsert(teams_upd, by = "row_id")
usethis::use_data(teams, overwrite = TRUE)

##### update results file (20 secs)
tictoc::tic()
results_upd <- purrr::map_df(get_afl_season(), ~ fitzRoy::fetch_results_afl(., comp = "AFLM"))
tictoc::toc()

teams <- torp::results %>% rows_upsert(results_upd, by = "match.matchId")
usethis::use_data(results, overwrite = TRUE)

######## update players data (3 mins)
# tictoc::tic()
# plyr_tm_db <- fitzRoy::fetch_player_details(current = FALSE, comp = "AFLM",source = "AFL") %>%
#   dplyr::mutate(player_name = paste(firstName,surname),
#                 age = lubridate::decimal_date(lubridate::as_date(glue::glue('{season}-07-01')))-
#                       lubridate::decimal_date(lubridate::as_date(dateOfBirth)),
#                 row_id = paste(providerId,season))
# tictoc::toc()
# usethis::use_data(plyr_tm_db, overwrite = TRUE)

##### update results file (20 secs)
tictoc::tic()
plyr_tm_db_upd <-
  fitzRoy::fetch_player_details_afl(season = get_afl_season(), comp = "AFLM") %>%
  dplyr::mutate(
    player_name = paste(firstName,surname),
    age = lubridate::decimal_date(lubridate::as_date(glue::glue('{season}-07-01')))-
      lubridate::decimal_date(lubridate::as_date(dateOfBirth)),
    row_id = paste(providerId,season)
    )
tictoc::toc()

plyr_tm_db <- torp::plyr_tm_db %>% rows_upsert(plyr_tm_db, by = "row_id")
usethis::use_data(plyr_tm_db, overwrite = TRUE)
