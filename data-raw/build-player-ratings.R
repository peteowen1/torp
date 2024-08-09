##
library(devtools)
library(fitzRoy)
library(tidyverse)
devtools::load_all()

# source('R/player_ratings.R')
# library(furrr)
# plan('multisession', workers = (parallelly::availableCores()-2))

library(foreach)
library(doParallel)
library(dplyr)  # Make sure to load the necessary packages

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

tictoc::tic() # 1 min per year
# ############ 2021
# #############################
# torp_df_21 <- furrr::future_map(1:27, ~torp_ratings(2021, .)) %>%
#   purrr::list_rbind() %>%
#   dplyr::mutate(row_id = paste0(player_id,season,sprintf('%02d',round)))
#

torp_df_21 <- foreach(i = 1:27, .combine = rbind, .packages = c("dplyr", "torp")) %dopar% {
  torp_ratings(2021, i)
}

############# 2022
# torp_df_22 <- furrr::future_map(1:27, ~ torp_ratings(2022, .)) %>%
#   purrr::list_rbind() %>%
#   dplyr::mutate(row_id = paste0(player_id,season,sprintf('%02d',round)))

torp_df_22 <- foreach(i = 1:27, .combine = rbind, .packages = c("dplyr", "torp")) %dopar% {
  torp_ratings(2022, i)
}
########### 2023
# torp_df_23 <- furrr::future_map(1:28, ~ torp_ratings(2023, .)) %>%
#   purrr::list_rbind() %>%
#   dplyr::mutate(row_id = paste0(player_id,season,sprintf('%02d',round)))

torp_df_23 <- foreach(i = 1:28, .combine = rbind, .packages = c("dplyr", "torp")) %dopar% {
  torp_ratings(2023, i)
}

########### 2024
# torp_df_24 <- furrr::future_map(0:(get_afl_week(type = 'next')+1), ~ torp_ratings(2024, .)) %>%
#   purrr::list_rbind() %>%
#   dplyr::mutate(row_id = paste0(player_id,season,sprintf('%02d',round)))

torp_df_24 <- foreach(i = 0:(get_afl_week(type = 'next')+1), .combine = rbind, .packages = c("dplyr", "magrittr", "torp")) %dopar% {
  torp_ratings(2024, i)
}

###
stopCluster(cl)

## final
torp_df_total <- dplyr::bind_rows(torp_df_21, torp_df_22, torp_df_23, torp_df_24)

tictoc::toc()

use_data(torp_df_total, overwrite = TRUE)

###### GIT PUSH AFTER CHANGE
