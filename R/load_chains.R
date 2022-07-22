#' @title Load Chains
#'
#' @description These functions allows you to load AFL chains quickly
#' @param file_type csv or rds (soon to come)
#' @keywords AFL
#' @export
#' @examples
#' \dontrun{
#' load_chains()
#' }
#'
load_chains <- function(seasons = lubridate::year(Sys.Date()),rounds = 1:27, file_type = "rds") {
  urls <- paste0(
    "https://github.com/peteowen1/torp/blob/main/data/chains_",seasons,"_",rounds,".",file_type,"?raw=true"
  )

 df <- purrr::map_df(urls,~readRDS(url(.)))
 return(df)
}
