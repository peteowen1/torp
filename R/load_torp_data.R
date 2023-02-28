#' Load Play By Play
#'
#' @description Loads play by play seasons from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given NFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 1999.
#' @param file_type One of `c("rds", "qs", "csv", "parquet")`. Can also be set globally with
#' `options(nflreadr.prefer)`
#'
#' @return The complete nflfastR dataset as returned by `nflfastR::build_nflfastR_pbp()`
#' (see below) for all given `seasons`
#'
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_pbp(2019:2020)
#' })
#' }
#' @seealso <https://nflreadr.peteowen1.com/articles/dictionary_pbp.html> for a web version of the data dictionary
#' @seealso [`dictionary_pbp`] for the data dictionary bundled as a package dataframe
#' @seealso <https://www.nflfastr.com/reference/build_nflfastR_pbp.html> for the nflfastR function `nflfastR::build_nflfastR_pbp()`
#' @seealso Issues with this data should be filed here: <https://github.com/peteowen1/peteowen1-pbp>
#'
#' @export
load_chains <- function(seasons = most_recent_season(), rounds = get_current_week()) {


  if(isTRUE(seasons)) seasons <- 2021:most_recent_season()

  stopifnot(is.numeric(seasons),
            seasons >=2021,
            seasons <= most_recent_season())

  if(isTRUE(rounds)) rounds <- 1:27

  stopifnot(is.numeric(rounds),
            rounds >=1,
            rounds <= 27)

  rounds_02d <- sprintf("%02d", rounds)

  urls <- paste0("https://github.com/peteowen1/torpdata/releases/download/chain-data/chain_data_",seasons,"_",rep(rounds_02d,each = length(seasons)), ".rds")

  urls <- urls[which(urls <= glue::glue("https://github.com/peteowen1/torpdata/releases/download/chain-data/chain_data_{most_recent_season()}_{get_current_week()}.rds"))]

  out <- load_from_url(urls, seasons = T, rounds = T)

  return(out)
}


#' Load Play By Play
#'
#' @description Loads play by play seasons from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given NFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 1999.
#' @param file_type One of `c("rds", "qs", "csv", "parquet")`. Can also be set globally with
#' `options(nflreadr.prefer)`
#'
#' @return The complete nflfastR dataset as returned by `nflfastR::build_nflfastR_pbp()`
#' (see below) for all given `seasons`
#'
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_pbp(2019:2020)
#' })
#' }
#' @seealso <https://nflreadr.peteowen1.com/articles/dictionary_pbp.html> for a web version of the data dictionary
#' @seealso [`dictionary_pbp`] for the data dictionary bundled as a package dataframe
#' @seealso <https://www.nflfastr.com/reference/build_nflfastR_pbp.html> for the nflfastR function `nflfastR::build_nflfastR_pbp()`
#' @seealso Issues with this data should be filed here: <https://github.com/peteowen1/peteowen1-pbp>
#'
#' @export
load_pbp <- function(seasons = most_recent_season(), rounds = get_current_week()) {

  if(isTRUE(seasons)) seasons <- 2021:most_recent_season()

  stopifnot(is.numeric(seasons),
            seasons >=2021,
            seasons <= most_recent_season())

  if(isTRUE(rounds)) rounds <- 1:27

  stopifnot(is.numeric(rounds),
            rounds >=1,
            rounds <= 27)

  rounds_02d <- sprintf("%02d", rounds)

  urls <- paste0("https://github.com/peteowen1/torpdata/releases/download/pbp-data/pbp_data_",seasons,"_",rep(rounds_02d,each = length(seasons)), ".rds") %>% sort()

  urls <- urls[which(urls <= glue::glue("https://github.com/peteowen1/torpdata/releases/download/pbp-data/pbp_data_{most_recent_season()}_{get_current_week()}.rds"))]

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
    if(!isTRUE(seasons) && "season" %in% names(out)) out <- out[out$season %in% seasons]
  }

  if(length(url) > 1) {
    p <- NULL
    if (is_installed("progressr")) p <- progressr::progressor(along = url)
    out <- lapply(url, progressively(rds_from_url, p))
    out <- data.table::rbindlist(out)
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
#'   rds_from_url("https://github.com/peteowen1/torpdata/releases/download/test/combines.rds")
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

