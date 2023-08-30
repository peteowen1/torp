##
devtools::load_all()

tictoc::tic()
# ############ 2021
# #############################
<<<<<<< HEAD
# torp_df_21 <- purrr::map_df(1:27, ~torp_ratings(2021, .)) %>% dplyr::mutate(row_id = paste0(player_id,season,sprintf('%02d',round)))
#
# ############# 2022
# torp_df_22 <- purrr::map_df(1:27, ~ torp_ratings(2022, .)) %>% dplyr::mutate(row_id = paste0(player_id,season,sprintf('%02d',round)))
#
########### 2023
# torp_df_23 <- purrr::map_df(1:get_afl_week(type = 'next'), ~ torp_ratings(2023, .)) %>% dplyr::mutate(row_id = paste0(player_id,season,sprintf('%02d',round)))
=======
torp_df_21 <- purrr::map_df(1:27, ~torp_ratings(2021, .)) %>% dplyr::mutate(row_id = paste0(player_id,season,sprintf('%02d',round)))
>>>>>>> 9d3782651a5c754bb6f224263eb91fb4882ea7f9

############# 2022
torp_df_22 <- purrr::map_df(1:27, ~ torp_ratings(2022, .)) %>% dplyr::mutate(row_id = paste0(player_id,season,sprintf('%02d',round)))

########### 2023
torp_df_23 <- purrr::map_df(1:get_afl_week(type = 'next'), ~ torp_ratings(2023, .)) %>% dplyr::mutate(row_id = paste0(player_id,season,sprintf('%02d',round)))

## final
torp_df_total <- dplyr::bind_rows(torp_df_21, torp_df_22, torp_df_23)

tictoc::toc()

###
<<<<<<< HEAD
torp_wk <-
  # torp_ratings(get_afl_season(type = 'next'), get_afl_week(type = 'next')) %>%
  purrr::map_df(1:get_afl_week(type = "next"), ~ torp_ratings(2023, .)) %>%
  mutate(row_id = paste0(player_id, season, sprintf("%02d", round)))

torp_df_total <- dplyr::rows_insert(x = torp::torp_df_total, y = torp_wk, by = "row_id", conflict = "ignore")
torp_df_total <- dplyr::rows_update(x = torp_df_total, y = torp_wk, by = "row_id", unmatched = "ignore")
=======
# torp_wk <- torp_ratings(get_afl_season(type = 'next'), get_afl_week(type = 'next')) %>%
#   mutate(row_id = paste0(player_id,season,sprintf('%02d',round)))
#
# torp_df_total <- dplyr::rows_insert(x = torp::torp_df_total,y = torp_wk,by = "row_id", conflict = "ignore")
# torp_df_total <- dplyr::rows_update(x = torp_df_total,y = torp_wk,by = "row_id", unmatched = "ignore" )

usethis::use_data(torp_df_total,overwrite = TRUE)
>>>>>>> 9d3782651a5c754bb6f224263eb91fb4882ea7f9

usethis::use_data(torp_df_total, overwrite = TRUE)
