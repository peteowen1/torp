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
load_chains <- function(seasons = NULL, rounds = NULL, file_type = "rds") {

  if (is.null(seasons)) {
    seasons <- lubridate::year(Sys.Date())
    cli::cli_alert_info("No seasons specified - returning results for {.val {seasons}}")
  }

  if (is.null(rounds)) {
    cli::cli_alert_info("No rounds specified - returning results for all rounds in {.val {seasons}}")
    rounds <- 1:27
  }

  urls <- purrr::map(seasons,~paste0(
    "https://github.com/peteowen1/torp/blob/main/data/chains_", ., "_", sprintf("%02d", rounds), ".", file_type, "?raw=true"
  ))
  urls <- unlist(urls)
  ########################


  df <- purrr::map_df(urls, ~rds_from_url(.)) ### maybe change to purrr::
  return(df)
}

#' @title rds_from_url
#'
#' @description These functions allows you to load AFL chains quickly
#' @param url urls (soon more to come)
#' @keywords AFL
#' @export
#' @examples
#' \dontrun{
#' rds_from_url()
#' }
#'
#'
rds_from_url <- function(url) {
  #cache_message()
  con <- url(url)
  on.exit(close(con))
  load <- try(readRDS(con), silent = TRUE)

  if (inherits(load, "try-error")) {
    cli::cli_warn("Failed to readRDS from {.url {url}}")
    return(data.table::data.table())
  }

  #data.table::setDT(load)
  return(load)
}
