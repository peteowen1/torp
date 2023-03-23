#' Load Chains
#'
#' @description Loads chains data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param rounds A numeric vector associated with given AFL round - defaults to latest round. If set to `TRUE`, returns all available rounds in the given season range.
#'
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_chains(2021:2022)
#' })
#' }
#' @export
load_chains <- function(seasons = get_afl_season(), rounds = get_afl_week()) {


  if(isTRUE(seasons)) seasons <- 2021:get_afl_season()

  stopifnot(is.numeric(seasons),
            seasons >=2021,
            seasons <= get_afl_season())

  if(isTRUE(rounds)) rounds <- 1:27

  stopifnot(is.numeric(rounds),
            rounds >=1,
            rounds <= 27)

  rounds_02d <- sprintf("%02d", rounds)

  urls <- paste0("https://github.com/peteowen1/torpdata/releases/download/chain-data/chain_data_",seasons,"_",rep(rounds_02d,each = length(seasons)), ".rds")

  urls <- urls[which(urls <= glue::glue("https://github.com/peteowen1/torpdata/releases/download/chain-data/chain_data_{get_afl_season()}_{get_afl_week()}.rds"))]

  out <- load_from_url(urls, seasons = T, rounds = T)

  return(out)
}


#' Load Play By Play
#'
#' @description Loads play by play seasons from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param rounds A numeric vector associated with given AFL round - defaults to latest round. If set to `TRUE`, returns all available rounds in the given season range.
#'
#'
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_pbp(2021:2022)
#' })
#' }
#'
#' @export
load_pbp <- function(seasons = get_afl_season(), rounds = get_afl_week()) {

  if(isTRUE(seasons)) seasons <- 2021:get_afl_season()

  stopifnot(is.numeric(seasons),
            seasons >=2021,
            seasons <= get_afl_season())

  if(isTRUE(rounds)) rounds <- 1:27

  stopifnot(is.numeric(rounds),
            rounds >=1,
            rounds <= 27)

  rounds_02d <- sprintf("%02d", rounds)

  urls <- paste0("https://github.com/peteowen1/torpdata/releases/download/pbp-data/pbp_data_",seasons,"_",rep(rounds_02d,each = length(seasons)), ".rds") %>% sort()

  urls <- urls[which(urls <= glue::glue("https://github.com/peteowen1/torpdata/releases/download/pbp-data/pbp_data_{get_afl_season()}_{get_afl_week()}.rds"))]

  out <- load_from_url(urls, seasons = seasons, rounds = rounds)

  return(out)
}



#' Load any rds/csv/csv.gz/parquet/qs file from a remote URL
#'
#' @param url a vector of URLs to load into memory. If more than one URL provided, will row-bind them.
#' @param seasons a numeric vector of years that will be used to filter the dataframe's `season` column. If `TRUE` (default), does not filter.
#' @param peteowen1 TRUE to add peteowen1_data classing and attributes.
#' @param ... named arguments that will be added as attributes to the data, e.g. `peteowen1_type` = "pbp"
#'
#' @export
#'
#' @return a dataframe, possibly of type `peteowen1_data`
#'
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   urls <- c("https://github.com/peteowen1/torpdata/releases/download/rosters/roster_2020.csv",
#'             "https://github.com/peteowen1/torpdata-data/releases/download/rosters/roster_2021.csv")
#'  load_from_url(urls, peteowen1 = TRUE, peteowen1_type = "rosters for 2020 & 2021")
#' })
#' }
load_from_url <- function(url, ..., seasons = TRUE, rounds = TRUE){

  url <- as.character(url)

  if(length(url) == 1) {
    out <- rds_from_url(url)
    if(!isTRUE(seasons)) stopifnot(is.numeric(seasons))
    # if(!isTRUE(seasons) && "season" %in% names(out)) out <- out[out$season %in% seasons]
  }

  if(length(url) > 1) {
    p <- NULL
    if (is_installed("progressr")) p <- progressr::progressor(along = url)
    out <- lapply(url, progressively(rds_from_url, p))
    out <- data.table::rbindlist(out, use.names=TRUE)
  }

  return(out)
}


#' Load .rds file from a remote connection
#'
#' @param url a character url
#'
#' @export
#'
#' @return a dataframe as created by [`readRDS()`]
#'
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   rds_from_url("https://github.com/peteowen1/torp/tree/main/data/results.rda")
#' })
#' }
rds_from_url <- function(url) {
  con <- url(url)
  on.exit(close(con))
  load <- try(readRDS(con), silent = TRUE)

  if (inherits(load, "try-error")) {
    cli::cli_warn("Failed to readRDS from {.url {url}}")
    return(data.table::data.table())
  }

  data.table::setDT(load)
  load
}

