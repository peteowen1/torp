###
devtools::load_all()

############# 2021
##############################
torp_df_21 <- purrr::map_df(1:27, ~torp_ratings(2021, .))

############# 2022
torp_df_22 <- purrr::map_df(1:27, ~ torp_ratings(2022, .))

############ 2023
torp_df_23 <- purrr::map_df(1:get_afl_week(type = 'next'), ~ torp_ratings(2023, .))

### final
torp_df_total <- dplyr::bind_rows(torp_df_21, torp_df_22, torp_df_23)

usethis::use_data(torp_df_total,overwrite = TRUE)

