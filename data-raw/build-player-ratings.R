###
############# 2021
##############################
if (exists("final_21") == FALSE) {
  final_21 <- furrr::future_map_dfr(2:28, ~ plyr_ratings(plyr_gm_df, teams, 2021, .))
  saveRDS(final_21, "./data/plyr_df_2021.rds")
  final_21 <- readRDS("./data/plyr_df_2021.rds")
}
### 2022
n <- 28
### 2022
if (exists("final_22") == FALSE) {
  final_22 <- furrr::future_map_dfr(1:28, ~ plyr_ratings(plyr_gm_df, teams, 2022, .))
  saveRDS(final_22, "./data/plyr_df_2022.rds")
  final_22 <- readRDS("./data/plyr_df_2022.rds")
}
### final
final <- dplyr::bind_rows(final_21, final_22)

final$weight <- exp(as.numeric(-(Sys.Date() - as.Date(final$utcStartTime))) / 250)
final$weight <- final$weight / mean(final$weight, na.rm = T)
#############
