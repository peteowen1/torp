library(dplyr)
# devtools::install_github('peteowen1/fitzRoy')
library(fitzRoy)
devtools::load_all()

### update fixtures file
fixtures <- purrr::map_df(2021:2024, ~ fitzRoy::fetch_fixture_afl(., comp = "AFLM"))

usethis::use_data(fixtures, overwrite = TRUE)

### update teams file
teams <- purrr::map_df(2021:get_afl_season(), ~ fitzRoy::fetch_lineup_afl(., comp = "AFLM")) %>%
  dplyr::mutate(
    season = as.numeric(substr(providerId, 5, 8)),
    row_id = paste0(providerId, teamId, player.playerId)
  ) # %>% dplyr::filter(!is.na(player.playerId))

usethis::use_data(teams, overwrite = TRUE)

##### update results file

results <- purrr::map_df(2021:get_afl_season(), ~ fitzRoy::fetch_results_afl(., comp = "AFLM"))

usethis::use_data(results, overwrite = TRUE)

######## update players data

plyr_tm_db <- fitzRoy::fetch_player_details(current = FALSE, comp = "AFLM",source = "AFL") %>%
  dplyr::mutate(player_name = paste(firstName,surname),
                age = lubridate::decimal_date(lubridate::as_date(glue::glue('{season}-07-01')))-
                      lubridate::decimal_date(lubridate::as_date(dateOfBirth)),
                row_id = paste(providerId,season))

usethis::use_data(plyr_tm_db, overwrite = TRUE)
