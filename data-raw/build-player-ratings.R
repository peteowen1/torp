##
library(devtools)
library(fitzRoy)
library(tidyverse)
devtools::load_all()

tictoc::tic() # 1 min per year
# ############ 2021
# #############################
torp_df_21 <- purrr::map(1:27, ~torp_ratings(2021, .)) %>%
  purrr::list_rbind() %>%
  dplyr::mutate(row_id = paste0(player_id,season,sprintf('%02d',round)))

############# 2022
torp_df_22 <- purrr::map(1:27, ~ torp_ratings(2022, .)) %>%
  purrr::list_rbind() %>%
  dplyr::mutate(row_id = paste0(player_id,season,sprintf('%02d',round)))

########### 2023
torp_df_23 <- purrr::map(1:28, ~ torp_ratings(2023, .)) %>%
  purrr::list_rbind() %>%
  dplyr::mutate(row_id = paste0(player_id,season,sprintf('%02d',round)))

########### 2024
torp_df_24 <- purrr::map(0:(get_afl_week(type = 'next')+1), ~ torp_ratings(2024, .)) %>%
  purrr::list_rbind() %>%
  dplyr::mutate(row_id = paste0(player_id,season,sprintf('%02d',round)))

## final
torp_df_total <- dplyr::bind_rows(torp_df_21, torp_df_22, torp_df_23, torp_df_24)

tictoc::toc()

use_data(torp_df_total, overwrite = TRUE)

###### GIT PUSH AFTER CHANGE
