#' Get Match Chains
#'
#' Retrieves match chain data for a given season and round, or for a specific
#' match ID.
#'
#' @param season The AFL season year (numeric), or a match ID string
#'   (e.g. `"CD_M20260140001"`). When a match ID is supplied, `round` is
#'   ignored and chains are fetched for that single match.
#' @param round The round number (numeric). If NA, retrieves data for all rounds in the season.
#'
#' @return A dataframe containing match chain data.
#' @export
#'
#' @examples
#' \dontrun{
#' chains <- get_match_chains(2022, 1)
#' chains <- get_match_chains("CD_M20260140001")
#' }
#' @importFrom dplyr inner_join left_join filter
#' @importFrom cli cli_abort cli_inform
get_match_chains <- function(season = get_afl_season(), round = NA) {
  # Detect match ID input (e.g. "CD_M20260140001")
  if (is.character(season) && grepl("^CD_M", season)) {
    return(.get_chains_by_match_id(season))
  }

  if (season < 2021) {
    cli::cli_abort("Match chain data is not available for seasons prior to 2021.")
  }

  games <- if (is.na(round)) {
    cli::cli_inform("No round value supplied. Scraping all rounds in the season. This may take some time.")
    get_season_games(season)
  } else {
    get_round_games(season, round)
  }

  if (nrow(games) == 0) {
    cli::cli_abort("No data available for the selected season or round.")
  }

  games_vector <- games$matchId

  cli::cli_inform("Scraping match chains...")
  chains <- get_many_game_chains(games_vector)

  players <- get_players(season = season)
  chains <- chains |>
    dplyr::inner_join(games, by = "matchId") |>
    dplyr::left_join(players, by = c("playerId", "season"))

  chains <- data.table::as.data.table(chains)
  .normalise_chains_columns(chains)

  cli::cli_inform("Success!")
  return(chains)
}

#' Fetch chains for a single match ID
#'
#' @param match_id A match ID string (e.g. `"CD_M20260140001"`).
#' @return A data.table of chain data with game and player metadata joined.
#' @keywords internal
.get_chains_by_match_id <- function(match_id) {
  match_year <- as.numeric(substr(match_id, 5, 8))
  if (is.na(match_year) || match_year < 2021) {
    cli::cli_abort("Match chain data is not available for seasons prior to 2021.")
  }

  cli::cli_inform("Scraping chains for match {.val {match_id}}...")
  chains <- get_game_chains(match_id)

  if (nrow(chains) == 0) {
    cli::cli_abort("No chain data returned for match {.val {match_id}}.")
  }

  games <- .find_game_by_match_id(match_year, match_id)

  if (nrow(games) > 0) {
    chains <- chains |>
      dplyr::inner_join(games, by = "matchId")
  } else {
    chains$season <- match_year
  }

  players <- get_players(season = match_year)
  chains <- chains |>
    dplyr::left_join(players, by = c("playerId", "season"))

  chains <- data.table::as.data.table(chains)
  .normalise_chains_columns(chains)

  cli::cli_inform("Success!")
  return(chains)
}

#' Find game metadata for a match ID
#'
#' Extracts the round number from the match ID structure
#' (`CD_M{year}{comp}{round}{game}`) and fetches that round's fixtures directly.
#'
#' @param season Numeric season year.
#' @param match_id Match ID string.
#' @return A single-row data.frame of game metadata, or empty data.frame.
#' @keywords internal
.find_game_by_match_id <- function(season, match_id) {
  round_num <- as.integer(substr(match_id, 12, 13))
  games <- tryCatch(get_round_games(season, round_num), error = function(e) data.frame())
  if (nrow(games) == 0) {
    cli::cli_warn("Could not find game metadata for {.val {match_id}} in season {.val {season}}. Returning chains without game metadata.")
    return(data.frame())
  }
  match_row <- games |> dplyr::filter(.data$matchId == match_id)
  if (nrow(match_row) > 0) return(match_row)
  cli::cli_warn("Could not find game metadata for {.val {match_id}} in round {.val {round_num}}. Returning chains without game metadata.")
  data.frame()
}


# Token cache environment (avoids re-authenticating on every API call)
.torp_token_cache <- new.env(parent = emptyenv())

#' Get API Token
#'
#' Retrieves an authentication token for the AFL API, caching it for 5 minutes.
#'
#' @return A character string containing the API token.
#' @keywords internal
#'
#' @importFrom httr POST content
get_token <- function() {
  now <- Sys.time()
  if (!is.null(.torp_token_cache$token) &&
      difftime(now, .torp_token_cache$fetched, units = "mins") < 5) {
    return(.torp_token_cache$token)
  }
  response <- httr::POST("https://api.afl.com.au/cfs/afl/WMCTok")
  httr::stop_for_status(response, task = "authenticate with AFL API")
  token <- httr::content(response)$token
  .torp_token_cache$token <- token
  .torp_token_cache$fetched <- now
  token
}

#' Access API
#'
#' Makes an authenticated request to the AFL API.
#'
#' @param url The API endpoint URL.
#'
#' @return The parsed JSON content of the API response.
#' @keywords internal
#'
#' @importFrom httr GET add_headers content
#' @importFrom jsonlite fromJSON
access_api <- function(url) {
  token <- get_token()
  response <- httr::GET(
    url = url,
    httr::add_headers("x-media-mis-token" = token)
  )
  httr::stop_for_status(response, task = paste("fetch data from", url))
  httr::content(response, as = "text", encoding = "UTF-8") |>
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
#' @keywords internal
#'
#' @importFrom dplyr filter mutate
get_round_games <- function(season, round) {
  round <- sprintf("%02d", round)
  url <- paste0("https://api.afl.com.au/cfs/afl/fixturesAndResults/season/CD_S", season, "014/round/CD_R", season, "014", round)
  api_result <- access_api(url)

  if (length(api_result) < 5) {
    cli::cli_warn("Unexpected API response structure for fixtures (expected 5+ elements, got {length(api_result)})")
    return(data.frame())
  }
  games <- api_result[["items"]] %||% api_result[[5]]

  if (length(games) > 0) {
    games <- games |>
      dplyr::filter(.data$status == "CONCLUDED") |>
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
#' @param rounds The maximum number of rounds to check (default: 28, covers all AFL season formats).
#'
#' @return A dataframe containing game data for the entire season.
#' @keywords internal
#'
#' @importFrom purrr map_df
get_season_games <- function(season, rounds = 28) {
  purrr::map_df(1:rounds, ~ get_round_games(season, .))
}

#' Get Players
#'
#' Retrieves player data either from the API or from a local database.
#'
#' @param season Numeric season(s) to load. Defaults to all seasons (TRUE).
#' @param use_api Logical, whether to use the API (TRUE) or local database (FALSE, default).
#'
#' @return A dataframe containing player data.
#' @keywords internal
#'
#' @importFrom dplyr mutate select
get_players <- function(season = TRUE, use_api = FALSE) {
  if (use_api) {
    url <- "https://api.afl.com.au/cfs/afl/players"
    api_result <- access_api(url)
    if (length(api_result) < 5) {
      cli::cli_abort("Unexpected API response structure for players (expected 5+ elements, got {length(api_result)})")
    }
    players <- (api_result[["players"]] %||% api_result[[5]]) |>
      dplyr::mutate(season = get_afl_season())
  } else {
    players <- load_player_details(seasons = season) |>
      dplyr::mutate(
        photoURL = NA,
        team.teamId = NA,
        team.teamAbbr = NA
      ) |>
      dplyr::select(
        playerId = .data$player_id, jumperNumber = .data$jumper_number,
        playerPosition = .data$position, photoURL = .data$photoURL,
        playerName.givenName = .data$first_name, playerName.surname = .data$surname,
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
#' @keywords internal
#'
get_many_game_chains <- function(games_vector) {
  data.table::rbindlist(lapply(games_vector, get_game_chains), fill = TRUE)
}

#' Get Game Chains
#'
#' Retrieves chain data for a single game.
#'
#' @param match_id The ID of the match.
#'
#' @return A dataframe containing chain data for the specified game.
#' @keywords internal
#'
get_game_chains <- function(match_id) {
  url <- paste0("https://sapi.afl.com.au/afl/matchPlays/", match_id)
  api_response <- access_api(url)

  if (length(api_response) < 8) {
    cli::cli_warn("Unexpected API response structure for match {match_id} (expected 8+ elements, got {length(api_response)})")
    return(data.frame())
  }
  chain_list <- api_response[["chains"]] %||% api_response[[8]]

  if (is.null(dim(chain_list)) || nrow(chain_list) == 0 || length(chain_list) <= 5) {
    return(data.frame())
  }

  # Hoist column index lookup (constant across all chains in a match)
  actions_col <- which(names(chain_list) == "actions")
  col_idx <- if (length(actions_col) == 1) actions_col else 6

  chains <- data.table::rbindlist(lapply(seq_len(nrow(chain_list)), function(i) {
    acts <- chain_list[[i, col_idx]]
    if (length(acts) == 0) return(NULL)
    dt <- data.table::as.data.table(acts)
    dt[, `:=`(
      finalState = chain_list$finalState[i],
      initialState = chain_list$initialState[i],
      period = chain_list$period[i],
      chain_number = i
    )]
    dt
  }), fill = TRUE)

  if (nrow(chains) == 0) return(data.frame())

  chains[, `:=`(
    matchId = api_response$matchId,
    venueWidth = api_response$venueWidth,
    venueLength = api_response$venueLength,
    homeTeamDirectionQtr1 = api_response$homeTeamDirectionQtr1
  )]

  chains[]
}

