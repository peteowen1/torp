#' Title
#'
#' @param season
#' @param round
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' get_match_chains()
#' }
get_match_chains <- function(season = lubridate::year(Sys.Date()), round = NA) {
  if (season < 2021) {
    stop("Match chain data is not available for seasons prior to 2021.")
  }

  if (is.na(round)) {
    cat("No round value supplied.\nFunction will scrape all rounds in the season.\nThis may take some time.\n")
    games <- get_season_games(season)
    games_vector <- games[, "matchId"]
  } else {
    games <- get_round_games(season, round)
    games_vector <- games[, "matchId"]
  }

  if (length(games) == 0) {
    stop("No data available for the season or round selected.")
  }

  cat("\nScraping match chains...\n\n")
  chains <- progressr::with_progress({
    get_many_game_chains(games_vector)
  })
  players <- get_players()
  chains <- dplyr::inner_join(chains, games, by = "matchId")
  chains <- dplyr::left_join(chains, players, by = "playerId")
  # chains <- chains %>% dplyr::select(
  #   matchId, season, roundNumber, utcStartTime,homeTeamId:providerMatchId, homeTeam.teamName:date, venue.name:venue.state,
  #   venueWidth:homeTeamDirectionQtr1, displayOrder, chain_number,
  #   initialState, finalState, period, periodSeconds,
  #   playerId, playerName.givenName, playerName.surname, teamId, team.teamName, description,
  #   disposal:y
  # )

  cat("\n\nSuccess!\n\n")

  return(chains)
}

### API SCRAPING FUNCTIONS
#' Title
#'
#' @return
#'
#' @examples
get_token <- function() {
  response <- httr::POST("https://api.afl.com.au/cfs/afl/WMCTok")
  token <- httr::content(response)$token

  return(token)
}

#### function to access api
#' Title
#'
#' @param url
#'
#' @return
#' @export
#'
#' @examples
access_api <- function(url) {
  token <- get_token()

  response <- httr::GET(
    url = url,
    httr::add_headers("x-media-mis-token" = token)
  )

  content <- response %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)

  return(content)
}

### MATCH DATA FUNCTIONS
#' @param season
#'
#' @param round
#'
#' @export
get_round_games <- function(season, round) {
  round <- ifelse(round < 10, paste0("0", round), round)
  url <- paste0("https://api.afl.com.au/cfs/afl/fixturesAndResults/season/CD_S", season, "014/round/CD_R", season, "014", round)
  games <- access_api(url)
  games <- games[[5]]

  if (length(games) > 0) {
    games <- games %>% filter(status == "CONCLUDED")
    if (nrow(games) > 0) {
      games <- games #%>%
      #   select(
      #   matchId, utcStartTime, roundNumber, roundId ,venue.name, venue.location, venue.state,venue.venueId,
      #   homeTeam.teamName, awayTeam.teamName,homeTeamId ,awayTeamId ,
      #   homeTeamScore.totalScore, awayTeamScore.totalScore
      # )
      games$date <- substr(games$utcStartTime, 1, 10)
      games$date <- as.Date(games$utcStartTime)
      games$season <- lubridate::year(games$utcStartTime)

      return(games)
    }
  }
}

### getting season games
#' @param season
#'
#' @export
get_season_games <- function(season) {
  games <- purrr::map_df(1:30, ~ get_round_games(season, .))

  return(games)
}

### PLAYER DATA FUNCTIONS
#' Title
#'
#' @return
#' @export
#'
#' @examples
get_players <- function() {
  url <- paste0("https://api.afl.com.au/cfs/afl/players")
  players <- access_api(url)
  players <- players[[5]]
  players <- players #%>% select(playerId, playerName.givenName, playerName.surname, team.teamName)

  return(players)
}

### CHAIN DATA FUNCTIONS
#' @param games_vector
#'
#' @export
get_many_game_chains <- function(games_vector) {
  p <- progressr::progressor(steps = length(games_vector))

  chains <- furrr::future_map_dfr(games_vector,
                                  ~ {
                                    p()
                                    get_game_chains(.)
                                  },
                                  .progress = FALSE
  )

  return(chains)
}

#### get individual game chains
#' @param match_id
#'
#' @export
get_game_chains <- function(match_id) {
  url <- paste0("https://api.afl.com.au/cfs/afl/matchChains/", match_id)
  chains_t1 <- access_api(url)
  chains_t2 <- chains_t1[[8]]

  if (!is.null(dim(chains_t2))) {
    if (nrow(chains_t2) > 0) {
      chains <- purrr::map_df(1:nrow(chains_t2), ~ get_single_chain(chains_t2, .))

      chains$matchId <- chains_t1$matchId
      chains$venueWidth <- chains_t1$venueWidth
      chains$venueLength <- chains_t1$venueLength
      chains$homeTeamDirectionQtr1 <- chains_t1$homeTeamDirectionQtr1

      return(chains)
    }
  }
}

### get single game chain
#' Title
#'
#' @param chains_t2
#' @param chain_number
#'
#' @return
#' @export
#'
#' @examples
get_single_chain <- function(chains_t2, chain_number) {
  if (length(chains_t2) > 5) {
    chains_t3 <- chains_t2[[chain_number, 6]]

    if (length(chains_t3 > 0)) {
      chains_t3$finalState <- chains_t2$finalState[chain_number]
      chains_t3$initialState <- chains_t2$initialState[chain_number]
      chains_t3$period <- chains_t2$period[chain_number]
      chains_t3$chain_number <- chain_number

      return(chains_t3)
    }
  }
}
