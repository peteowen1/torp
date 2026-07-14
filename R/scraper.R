# AFL Chain Scraping
# ==================
# Despite the generic name, this file is the *chains* fetcher only:
#   - get_match_chains() / get_many_game_chains() / get_game_chains()
#       chain-level PBP data (the only public export — `@export get_match_chains`)
#   - get_round_games() / get_season_games() / get_players()
#       internal helpers needed to enumerate matches before scraping chains
#   - get_token() / access_api()
#       shared HTTP infrastructure ALSO used by R/afl_api.R — do not move.
#
# Structured fixture/result/ladder/lineup/player-stats endpoints live in
# R/afl_api.R, not here. If you're adding a new endpoint, decide first which
# file owns it: chains -> here, anything else -> afl_api.R.


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
    chains$roundNumber <- .extract_round_from_match_id(match_id)
  }

  players <- get_players(season = match_year)
  chains <- chains |>
    dplyr::left_join(players, by = c("playerId", "season"))

  chains <- data.table::as.data.table(chains)
  .normalise_chains_columns(chains)

  cli::cli_inform("Success!")
  return(chains)
}

#' Extract round number from an AFL match ID
#'
#' Parses `CD_M{year:4}{comp:3}{round:2}{game:2+}` format via regex.
#' Assumes 4-digit year + 3-digit comp ID (matches all known AFL formats).
#' Vectorised over `match_id`.
#'
#' @param match_id Character match ID (scalar or vector).
#' @return Integer round number(s), NA where the format does not match.
#' @keywords internal
.extract_round_from_match_id <- function(match_id) {
  m <- regmatches(match_id, regexec("^CD_M\\d{4}\\d{3}(\\d{2})", match_id))
  vapply(m, function(x) if (length(x) < 2) NA_integer_ else as.integer(x[2]), integer(1))
}

#' Find game metadata for a match ID
#'
#' Extracts the round number from the match ID structure
#' and fetches that round's fixtures directly.
#'
#' @param season Numeric season year.
#' @param match_id Match ID string.
#' @return A single-row data.frame of game metadata, or empty data.frame.
#' @keywords internal
.find_game_by_match_id <- function(season, match_id) {
  round_num <- .extract_round_from_match_id(match_id)
  if (is.na(round_num)) {
    cli::cli_warn("Could not parse round number from match ID {.val {match_id}}")
    return(data.frame())
  }
  games <- tryCatch(
    get_round_games(season, round_num, concluded_only = FALSE),
    error = function(e) {
      cli::cli_warn("API error fetching round {round_num} for {.val {match_id}}: {conditionMessage(e)}")
      data.frame()
    }
  )
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
# 5-min TTL is conservative — actual token lifetime is undocumented by AFL.
# Short enough to recover from token rotation, long enough to avoid
# re-auth on every call during batch scraping sessions.
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
  response <- httr::POST(paste0(AFL_CFS_API_BASE_URL, "WMCTok"))
  httr::stop_for_status(response, task = "authenticate with AFL API")
  token <- httr::content(response)$token
  if (is.null(token) || !nzchar(token)) {
    cli::cli_abort("AFL API returned empty or missing token (HTTP 200 but no token in body)")
  }
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
get_round_games <- function(season, round, concluded_only = TRUE) {
  round <- sprintf("%02d", round)
  url <- paste0(AFL_CFS_API_BASE_URL, "fixturesAndResults/season/CD_S", season, "014/round/CD_R", season, "014", round)
  api_result <- access_api(url)

  games <- api_result[["fixtures"]] %||% api_result[["items"]]
  if (is.null(games)) {
    cli::cli_warn("AFL API fixtures response missing fixtures key for round {.val {round}}")
    return(data.frame())
  }

  if (length(games) > 0) {
    if (concluded_only) {
      games <- games |>
        dplyr::filter(.data$status == "CONCLUDED")
    }
    games <- games |>
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
#' Retrieves game data for an entire season using parallel HTTP requests.
#'
#' @param season The AFL season year (numeric).
#' @param rounds The maximum number of rounds to check (default: 28, covers all AFL season formats).
#'
#' @return A dataframe containing game data for the entire season.
#' @keywords internal
#'
#' @importFrom curl new_pool new_handle curl_fetch_multi multi_run
get_season_games <- function(season, rounds = 28) {
  token <- get_token()
  urls <- vapply(seq_len(rounds), function(r) {
    paste0(AFL_CFS_API_BASE_URL, "fixturesAndResults/season/CD_S",
           season, "014/round/CD_R", season, "014", sprintf("%02d", r))
  }, character(1))

  # <<- in callbacks is safe: multi_run() is synchronous (no concurrent mutation)
  pool <- curl::new_pool(total_con = 30L, host_con = 10L)
  results <- vector("list", rounds)
  n_failed <- 0L
  n_parse_errors <- 0L

  for (i in seq_len(rounds)) {
    h <- curl::new_handle(httpheader = paste0("x-media-mis-token: ", token))
    local({
      idx <- i
      curl::curl_fetch_multi(urls[idx], done = function(resp) {
        if (resp$status_code == 200L) {
          tryCatch({
            json <- jsonlite::fromJSON(rawToChar(resp$content), flatten = TRUE)
            games <- json[["fixtures"]] %||% json[["items"]]
            if (!is.null(games) && length(games) > 0) {
              games <- games[games$status == "CONCLUDED", , drop = FALSE]
              if (nrow(games) > 0) {
                games$date <- as.Date(substr(games$utcStartTime, 1, 10))
                games$season <- season
                results[[idx]] <<- games
              }
            }
          }, error = function(e) {
            n_parse_errors <<- n_parse_errors + 1L
          })
        } else {
          n_failed <<- n_failed + 1L
        }
      }, fail = function(msg) {
        n_failed <<- n_failed + 1L
      }, handle = h, pool = pool)
    })
  }

  curl::multi_run(pool = pool)

  if (n_failed > 0) {
    cli::cli_warn("{n_failed} round{?s} failed to fetch for season {season}")
  }
  if (n_parse_errors > 0) {
    cli::cli_warn("{n_parse_errors} round{?s} returned unparseable data for season {season}")
  }

  out <- dplyr::bind_rows(purrr::compact(results))
  if (nrow(out) == 0) return(data.frame())
  out
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
    url <- paste0(AFL_CFS_API_BASE_URL, "players")
    api_result <- access_api(url)
    players <- api_result[["players"]]
    if (is.null(players)) {
      cli::cli_abort("AFL API players response missing {.field players} key")
    }
    players <- players |>
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
  url <- paste0(AFL_SAPI_BASE_URL, "matchPlays/", match_id)
  api_response <- access_api(url)

  chain_list <- api_response[["matchChains"]] %||% api_response[["chains"]]
  if (is.null(chain_list)) {
    cli::cli_warn("AFL API match response missing chains key for {.val {match_id}}")
    return(data.frame())
  }

  if (is.null(dim(chain_list)) || nrow(chain_list) == 0) {
    cli::cli_warn("AFL API returned no chain rows for {.val {match_id}}")
    return(data.frame())
  }

  # Hoist column index lookup (constant across all chains in a match)
  # API renamed "actions" → "stats" circa 2026; support both
  actions_col <- which(names(chain_list) %in% c("stats", "actions"))
  if (length(actions_col) == 1) {
    col_idx <- actions_col
  } else {
    cli::cli_warn("AFL API chain response has ambiguous or missing actions/stats column for {.val {match_id}} -- possible schema drift, falling back to positional column 6")
    col_idx <- 6
  }
  stats_col_name <- names(chain_list)[col_idx]

  # Chain-level fields captured below under fixed names that the downstream
  # pipeline depends on. EVERYTHING else the matchPlays response exposes at chain
  # level — including any field an older or future API schema adds — is captured
  # generically with a `chain_` prefix, so the chains archive is complete and a
  # missing column never forces a re-scrape (issue #92 API audit). teamId and
  # periodSeconds get fixed renames because they collide with the action-level
  # columns of the same name.
  known_chain <- c("teamId", "periodSeconds", "initialState", "finalState",
                   "period", stats_col_name)
  extra_chain <- setdiff(names(chain_list), known_chain)

  chains <- data.table::rbindlist(lapply(seq_len(nrow(chain_list)), function(i) {
    acts <- chain_list[[i, col_idx]]
    if (length(acts) == 0) return(NULL)
    dt <- data.table::as.data.table(acts)
    dt[, `:=`(
      finalState = chain_list$finalState[i],
      initialState = chain_list$initialState[i],
      period = chain_list$period[i],
      chain_number = i,
      # Chain-level possessing team (matchChains[i].teamId): defines the
      # attacking coordinate frame for EVERY action row in the chain. The
      # action-level stats[j].teamId only says who performed the event, so it
      # must NOT decide coordinate orientation. See fix_chain_coordinates_dt(),
      # issue #92. (renamed from teamId — collides with the action-level teamId.)
      chain_team_id = chain_list$teamId[i],
      # Chain-level timestamp; renamed from periodSeconds (action rows already
      # carry their own periodSeconds).
      chain_period_seconds = chain_list$periodSeconds[i]
    )]
    # Catch-all: any other chain-level scalar field, prefixed chain_ (no-op when
    # the schema only has the known fields, e.g. 2026).
    for (cc in extra_chain) {
      v <- chain_list[[cc]][i]
      if (is.atomic(v) && length(v) == 1L) dt[, (paste0("chain_", cc)) := v]
    }
    dt
  }), fill = TRUE)

  if (nrow(chains) == 0) return(data.frame())

  # Match-level fields under fixed names; homeTeamId/awayTeamId get an `mp_`
  # (matchPlays) prefix so they don't collide with the homeTeamId/awayTeamId that
  # get_match_chains later joins in from the games/fixtures data (a bare homeTeamId
  # would suffix to home_team_id_x/_y and break home_team_id resolution). They are
  # in the SAME id-space as chain_team_id, so clean_pbp can use them as a
  # self-contained home-end reference for coordinate orientation (issue #92).
  chains[, `:=`(
    matchId = api_response$matchId,
    venueWidth = api_response$venueWidth,
    venueLength = api_response$venueLength,
    homeTeamDirectionQtr1 = api_response$homeTeamDirectionQtr1,
    mp_home_team_id = api_response$homeTeamId,
    mp_away_team_id = api_response$awayTeamId
  )]

  # Catch-all: every other top-level matchPlays scalar field, prefixed mp_, so the
  # archive is complete and new/older-schema columns never force a re-scrape. The
  # exploded matchChains array and the empty `filter` query-echo are excluded.
  known_match <- c("matchId", "venueWidth", "venueLength", "homeTeamDirectionQtr1",
                   "homeTeamId", "awayTeamId", "matchChains", "chains", "filter")
  for (mc in setdiff(names(api_response), known_match)) {
    v <- api_response[[mc]]
    if (is.atomic(v) && length(v) == 1L) chains[, (paste0("mp_", mc)) := v]
  }

  chains[]
}

