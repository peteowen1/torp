#' Get Match Chains
#'
#' Retrieves match chain data for a given season and round.
#'
#' @param season The AFL season year (numeric).
#' @param round The round number (numeric). If NA, retrieves data for all rounds in the season.
#'
#' @return A dataframe containing match chain data.
#' @export
#'
#' @examples
#' \dontrun{
#' chains <- get_match_chains(2022, 1)
#' }
#' @importFrom dplyr inner_join left_join
#' @importFrom progressr with_progress
get_match_chains <- function(season = get_afl_season(), round = NA) {
  if (season < 2021) {
    stop("Match chain data is not available for seasons prior to 2021.")
  }

  games <- if (is.na(round)) {
    message("No round value supplied. Scraping all rounds in the season. This may take some time.")
    get_season_games(season)
  } else {
    get_round_games(season, round)
  }

  if (nrow(games) == 0) {
    stop("No data available for the selected season or round.")
  }

  games_vector <- games$matchId

  message("Scraping match chains...")
  chains <- progressr::with_progress({
    get_many_game_chains(games_vector)
  })

  players <- get_players()
  chains <- chains %>%
    dplyr::inner_join(games, by = "matchId") %>%
    dplyr::left_join(players, by = c("playerId", "season"))

  message("Success!")
  return(chains)
}

#' Get Week Chains
#'
#' Retrieves chain data for a specific week (round) in a season.
#'
#' @param season The AFL season year (numeric).
#' @param roundnum The round number (numeric).
#'
#' @return A dataframe containing chain data for the specified week.
#' @export
#'
#' @importFrom cli cli_warn
#' @importFrom data.table data.table
get_week_chains <- function(season, roundnum) {
  load <- tryCatch(
    get_match_chains(season, round = roundnum),
    error = function(e) {
      cli::cli_warn("Failed to get match chains from {.val {season}} round {.val {roundnum}}")
      return(data.table::data.table())
    }
  )

  return(load)
}

#' Get API Token
#'
#' Retrieves an authentication token for the AFL API.
#'
#' @return A character string containing the API token.
#' @keywords internal
#'
#' @importFrom httr POST content
get_token <- function() {
  response <- httr::POST("https://api.afl.com.au/cfs/afl/WMCTok")
  httr::content(response)$token
}

#' Access API
#'
#' Makes an authenticated request to the AFL API.
#'
#' @param url The API endpoint URL.
#'
#' @return The parsed JSON content of the API response.
#' @export
#'
#' @importFrom httr GET add_headers content
#' @importFrom jsonlite fromJSON
access_api <- function(url) {
  token <- get_token()
  response <- httr::GET(
    url = url,
    httr::add_headers("x-media-mis-token" = token)
  )
  httr::content(response, as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)
}

#' Get Round Games
#'
#' Retrieves game data for a specific round in a season.
#'
#' @param season The AFL season year (numeric).
#' @param round The round number (numeric).
#'
#' @return A dataframe containing game data for the specified round.
#' @export
#'
#' @importFrom dplyr filter mutate
get_round_games <- function(season, round) {
  round <- sprintf("%02d", round)
  url <- paste0("https://api.afl.com.au/cfs/afl/fixturesAndResults/season/CD_S", season, "014/round/CD_R", season, "014", round)
  games <- access_api(url)[[5]]

  if (length(games) > 0) {
    games <- games %>%
      dplyr::filter(.data$status == "CONCLUDED") %>%
      dplyr::mutate(
        date = as.Date(substr(.data$utcStartTime, 1, 10)),
        season = season
      )
    return(games)
  }
  data.frame() # Return empty dataframe if no games found
}

#' Get Season Games
#'
#' Retrieves game data for an entire season.
#'
#' @param season The AFL season year (numeric).
#' @param rounds The number of rounds in the season (default: 27).
#'
#' @return A dataframe containing game data for the entire season.
#' @export
#'
#' @importFrom purrr map_df
get_season_games <- function(season, rounds = 27) {
  purrr::map_df(1:rounds, ~ get_round_games(season, .))
}

#' Get Players
#'
#' Retrieves player data either from the API or from a local database.
#'
#' @param use_api Logical, whether to use the API (TRUE) or local database (FALSE, default).
#'
#' @return A dataframe containing player data.
#' @export
#'
#' @importFrom dplyr mutate select
get_players <- function(use_api = FALSE) {
  if (use_api) {
    url <- "https://api.afl.com.au/cfs/afl/players"
    players <- access_api(url)[[5]] %>%
      dplyr::mutate(season = get_afl_season())
  } else {
    players <- torp::plyr_tm_df %>%
      dplyr::mutate(
        photoURL = NA,
        team.teamId = NA,
        team.teamAbbr = NA
      ) %>%
      dplyr::select(
        playerId = .data$providerId, jumperNumber = .data$jumperNumber,
        playerPosition = .data$position, photoURL = .data$photoURL,
        playerName.givenName = .data$firstName, playerName.surname = .data$surname,
        team.teamId = .data$team.teamId, team.teamAbbr = .data$team.teamAbbr,
        team.teamName = .data$team, season = .data$season
      )
  }
  return(players)
}

#' Get Many Game Chains
#'
#' Retrieves chain data for multiple games.
#'
#' @param games_vector A vector of game IDs.
#'
#' @return A dataframe containing chain data for all specified games.
#' @export
#'
#' @importFrom purrr map_df
#' @importFrom progressr progressor
get_many_game_chains <- function(games_vector) {
  p <- progressr::progressor(steps = length(games_vector))
  purrr::map_df(games_vector, ~ {
    p()
    get_game_chains(.)
  })
}

#' Get Game Chains
#'
#' Retrieves chain data for a single game.
#'
#' @param match_id The ID of the match.
#'
#' @return A dataframe containing chain data for the specified game.
#' @export
#'
#' @importFrom purrr map_df
get_game_chains <- function(match_id) {
  url <- paste0("https://sapi.afl.com.au/afl/matchPlays/", match_id)
  chains_t1 <- access_api(url)
  chains_t2 <- chains_t1[[8]]

  if (!is.null(dim(chains_t2)) && nrow(chains_t2) > 0) {
    chains <- purrr::map_df(1:nrow(chains_t2), ~ get_single_chain(chains_t2, .))
    chains$matchId <- chains_t1$matchId
    chains$venueWidth <- chains_t1$venueWidth
    chains$venueLength <- chains_t1$venueLength
    chains$homeTeamDirectionQtr1 <- chains_t1$homeTeamDirectionQtr1
    return(chains)
  }
  data.frame() # Return empty dataframe if no chains found
}

#' Get Single Chain
#'
#' Processes a single chain from the game data.
#'
#' @param chains_t2 The chain data for a game.
#' @param chain_number The number of the chain to process.
#'
#' @return A dataframe containing data for the specified chain.
#' @export
get_single_chain <- function(chains_t2, chain_number) {
  if (length(chains_t2) > 5) {
    chains_t3 <- chains_t2[[chain_number, 6]]
    if (length(chains_t3) > 0) {
      chains_t3$finalState <- chains_t2$finalState[chain_number]
      chains_t3$initialState <- chains_t2$initialState[chain_number]
      chains_t3$period <- chains_t2$period[chain_number]
      chains_t3$chain_number <- chain_number
      return(chains_t3)
    }
  }
  data.frame() # Return empty dataframe if chain processing fails
}
