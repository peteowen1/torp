library(dplyr)
devtools::load_all()
### update fixtures file
fixtures <- purrr::map_df(2021:2023,~fitzRoy::fetch_fixture_afl(.,comp = "AFLM"))

usethis::use_data(fixtures,overwrite = TRUE)

### update teams file
# teams <- purrr::map_df(2021:get_afl_season(),~fitzRoy::fetch_lineup_afl(., comp = "AFLM")) %>%
#   dplyr::mutate(season = as.numeric(substr(providerId, 5, 8)),
#                 row_id = paste0(providerId,teamId,player.playerId)) %>%
#   dplyr::filter(!is.na(player.playerId))

####
team_wk <- fitzRoy::fetch_lineup_afl(season = get_afl_season(type="next"),
                                     round_number = get_afl_week(type="next"),
                                     comp = "AFLM")

if (!is.null(team_wk)) {


if ("player.playerId" %in% colnames(team_wk)) {

  team_wk <-
    team_wk %>%
    dplyr::mutate(season = as.numeric(substr(providerId, 5, 8)),
                  row_id = paste0(providerId,teamId,player.playerId)
                  )

  teams <- dplyr::rows_insert(x = torp::teams %>% dplyr::filter(!is.na(player.playerId)),y = team_wk,by = "row_id", conflict = "ignore")
  teams <- dplyr::rows_update(x = teams,y = team_wk,by = "row_id", unmatched = "ignore" )

  usethis::use_data(teams,overwrite = TRUE)
}

if (!"player.playerId" %in% colnames(team_wk)) {

team_wk$player.playerId <- NA

team_wk <-
  team_wk %>%
  dplyr::mutate(season = as.numeric(substr(providerId, 5, 8)),
                row_id = paste0(providerId,teamId,player.playerId)
  )

teams <- dplyr::rows_insert(x = torp::teams %>% dplyr::filter(!is.na(player.playerId)),y = team_wk,by = "row_id", conflict = "ignore")
teams <- dplyr::rows_update(x = teams,y = team_wk,by = "row_id", unmatched = "ignore" )

usethis::use_data(teams,overwrite = TRUE)
}

##### update results file

# results <- purrr::map_df(2021:get_afl_season(),~fitzRoy::fetch_results_afl(., comp = "AFLM"))

results_wk <- fitzRoy::fetch_results_afl(get_afl_season(),get_afl_week())

results <- dplyr::rows_insert(x = torp::results,y = results_wk,by = "match.matchId", conflict = "ignore")
results <- dplyr::rows_update(x = results,y = results_wk,by = "match.matchId", unmatched = "ignore" )

usethis::use_data(results,overwrite = TRUE)

}
######## update players data

# plyr_tm_db <- fitzRoy::fetch_player_details(current = FALSE, comp = "AFLM",source = "AFL") %>%
#   dplyr::mutate(player_name = paste(firstName,surname),
#                 age = lubridate::decimal_date(lubridate::as_date(glue::glue('{season}-07-01')))-
#                       lubridate::decimal_date(lubridate::as_date(dateOfBirth)),
#                 row_id = paste(providerId,season))

plyr_tm_db_wk <- fitzRoy::fetch_player_details_afl(season = get_afl_season(type="next"),comp = "AFLM",official_teams = TRUE) %>%
    dplyr::mutate(player_name = paste(firstName,surname),
                  age = lubridate::decimal_date(lubridate::as_date(glue::glue('{season}-07-01')))-
                        lubridate::decimal_date(lubridate::as_date(dateOfBirth)),
                  row_id = paste(providerId,season))

plyr_tm_db <- dplyr::rows_insert(x = torp::plyr_tm_db,y = plyr_tm_db_wk,by = "row_id", conflict = "ignore")
plyr_tm_db <- dplyr::rows_update(x = plyr_tm_db,y = plyr_tm_db,by = "row_id", unmatched = "ignore" )

usethis::use_data(plyr_tm_db,overwrite = TRUE)

