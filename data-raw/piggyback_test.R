#install.packages("piggyback")
library(piggyback)

pb_new_release("peteowen1/torpdata", "v0.0.1")

library(magrittr)

## upload a folder of data
list.files("data-old") %>%
  pb_upload(repo = "peteowen1/torpdata", tag = "v0.0.1")

## upload certain file extensions
list.files(pattern = c("*.tsv.gz", "*.tif", "*.zip")) %>%
  pb_upload(repo = "peteowen1/torpdata", tag = "v0.0.1")


#####
pkgload::load_all()

save_ep_data <- function(season, folder_path, version){

  pbp_object <- torp::pbp_build(season,version = version)

  # rds
  saveRDS(ep_object$ep_weekly, file.path(folder_path, glue::glue('ep_weekly_{season}.rds')))
  saveRDS(ep_object$ep_pbp_rush, file.path(folder_path, glue::glue('ep_pbp_rush_{season}.rds')))
  saveRDS(ep_object$ep_pbp_pass, file.path(folder_path, glue::glue('ep_pbp_pass_{season}.rds')))


  writeLines(as.character(Sys.time()), file.path(folder_path,"timestamp.txt"))
  writeLines(as.character(version), file.path(folder_path,"version.txt"))
}

upload_ep_data <- function(folder_path, version){
  list.files(folder_path, pattern = "csv$|rds$|parquet$|txt$", full.names = TRUE) %>%
    piggyback::pb_upload(repo = "peteowen1/torpdata", tag = "latest-data", overwrite = TRUE)

  list.files(folder_path, pattern = "csv$|rds$|parquet$|txt$", full.names = TRUE) %>%
    piggyback::pb_upload(repo = "peteowen1/torpdata", tag = glue::glue("{version}-data"), overwrite = TRUE)

  cli::cli_alert_success("Completed ep upload! {Sys.time()}")
}

update_ep <- function(season, version = "v1.0.0", folder_path){
  purrr::walk(season, save_ep_data, folder_path = folder_path, version = version)
  upload_ep_data(folder_path, version)
  invisible(NULL)
}

version <- "v1.0.0"
temp <- tempdir(check = TRUE)

try(piggyback::pb_new_release(repo = "peteowen1/torpdata", tag = "latest-data"))
try(piggyback::pb_new_release(repo = "peteowen1/torpdata", tag = glue::glue("{version}-data")))

update_ep(nflreadr::most_recent_season(), version = "latest", folder_path = temp)
update_ep(nflreadr::most_recent_season(), version = "v1.0.0", folder_path = temp)

unlink(temp, recursive = TRUE, force = TRUE)
