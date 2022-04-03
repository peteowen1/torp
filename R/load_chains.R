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
load_chains <- function(seasons = lubridate::year(Sys.Date()), weeks = 1:35, file_type = "rds") {
  urls <- paste0(
    "https://github.com/DataByJosh/AFL-Data/raw/main/AFLM_Match_Chains/csvs/match_chains_",
    seasons, "_", ifelse(weeks < 10, paste0(0, weeks), weeks), ".", file_type
  )

  out <- furrr::future_map(urls, purrr::possibly(data.table::fread, otherwise = data.table::data.table()))
  out <- data.table::rbindlist(out, use.names = TRUE)
  class(out) <- c("tbl_df", "tbl", "data.table", "data.frame")
  out
}
