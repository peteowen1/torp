# AFL API Functions
# =================
# Optimized in-house replacements for fitzRoy API functions.
# Uses torp's existing get_token()/access_api() from scraper.R.
#
# Key speed wins over fitzRoy:
#   - Fixtures: 3 HTTP calls on cold cache (2 cached per session + 1 per season)
#   - Results:  0 extra calls — derived from fixture data (scores included!)
#   - Lineups:  1+M calls — reuses cached fixtures + shared token
#   - Player stats: 1+M calls — same pattern
#   - Player details: 1+T calls — same pattern


# Internal Helpers ----

#' Batch-fetch JSON from CFS API endpoints
#'
#' Downloads multiple CFS API JSON responses in parallel using
#' [curl::multi_download()], then parses each with a supplied parser.
#' Falls back to sequential [httr::GET()] if parallel download fails.
#'
#' @param ids Character vector of endpoint suffixes (e.g. match IDs).
#' @param url_template Character. `sprintf()`-style template with one `%s` placeholder.
#' @param token Character. CFS API auth token.
#' @param parse_fn Function taking `(parsed_json, id)` and returning a data.frame.
#' @param label Character. Label for progress messages (e.g. "roster", "stats").
#' @return A tibble of all parsed results row-bound together.
#' @keywords internal
#' @importFrom curl new_pool new_handle curl_fetch_multi multi_run
.fetch_cfs_batch <- function(ids, url_template, token, parse_fn, label = "data") {
  if (length(ids) == 0) return(tibble::tibble())

  # --- Incremental disk cache: skip IDs already cached ---
  cache_dir <- file.path(get_disk_cache_dir(), label)
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  cached_results <- vector("list", length(ids))
  fetch_idx <- integer(0)

  for (i in seq_along(ids)) {
    cache_file <- file.path(cache_dir, paste0(ids[i], ".parquet"))
    if (file.exists(cache_file)) {
      tryCatch({
        cached_results[[i]] <- arrow::read_parquet(cache_file)
      }, error = function(e) {
        unlink(cache_file)
        fetch_idx <<- c(fetch_idx, i)
      })
    } else {
      fetch_idx <- c(fetch_idx, i)
    }
  }

  n_cached <- length(ids) - length(fetch_idx)
  if (n_cached > 0) {
    cli::cli_inform("Loaded {n_cached} cached {label} from disk, fetching {length(fetch_idx)} new...")
  } else {
    cli::cli_inform("Fetching {label} for {length(ids)} match{?es} in parallel...")
  }

  # --- Fetch only uncached matches ---
  if (length(fetch_idx) > 0) {
    fetch_ids <- ids[fetch_idx]
    pool <- curl::new_pool(total_con = 200L, host_con = 50L)
    fetch_results <- vector("list", length(fetch_ids))
    n_failed <- 0L

    for (j in seq_along(fetch_ids)) {
      url <- sprintf(url_template, fetch_ids[j])
      h <- curl::new_handle(httpheader = paste0("x-media-mis-token: ", token))

      local({
        jj <- j
        mid <- fetch_ids[j]
        curl::curl_fetch_multi(url, done = function(resp) {
          if (resp$status_code == 200L) {
            tryCatch({
              json <- jsonlite::fromJSON(rawToChar(resp$content), flatten = TRUE)
              fetch_results[[jj]] <<- parse_fn(json, mid)
            }, error = function(e) {
              cli::cli_alert_danger("Failed to parse {label} for {mid}: {conditionMessage(e)}")
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
      cli::cli_alert_danger("{n_failed} of {length(fetch_ids)} {label} request{?s} failed")
    }

    # Write newly fetched results to disk cache
    for (j in seq_along(fetch_ids)) {
      if (!is.null(fetch_results[[j]]) && nrow(fetch_results[[j]]) > 0) {
        cache_file <- file.path(cache_dir, paste0(fetch_ids[j], ".parquet"))
        tryCatch(
          arrow::write_parquet(fetch_results[[j]], cache_file),
          error = function(e) NULL
        )
        cached_results[[fetch_idx[j]]] <- fetch_results[[j]]
      }
    }
  }

  out <- purrr::list_rbind(purrr::compact(cached_results))
  if (is.null(out) || nrow(out) == 0) return(tibble::tibble())
  out
}

#' Sequential fallback for CFS API fetching
#' @inheritParams .fetch_cfs_batch
#' @keywords internal
.fetch_cfs_sequential <- function(ids, url_template, token, parse_fn, label = "data") {
  cli::cli_inform("Fetching {label} for {length(ids)} match{?es} sequentially...")
  results <- purrr::map(ids, function(mid) {
    tryCatch({
      url <- sprintf(url_template, mid)
      resp <- httr::GET(url, httr::add_headers("x-media-mis-token" = token))
      httr::stop_for_status(resp)
      json <- httr::content(resp, as = "text", encoding = "UTF-8") |>
        jsonlite::fromJSON(flatten = TRUE)
      parse_fn(json, mid)
    }, error = function(e) {
      cli::cli_alert_danger("Failed to fetch {label} for {mid}: {conditionMessage(e)}")
      NULL
    })
  })
  purrr::list_rbind(purrr::compact(results))
}


# Bulk Multi-Season Fetcher (Player Details) ----
# Gathers all team-season tuples, then fires one in-memory curl pool.
# Only beneficial for player details (~108 requests across 6 seasons).
# For lineups/stats (~1067 per-match requests), per-season batching
# performs equally or better due to connection contention.

#' Fetch player details for multiple seasons in one parallel batch
#' @param seasons Numeric vector of years
#' @return A tibble of player details across all seasons
#' @keywords internal
.fetch_all_player_details <- function(seasons) {
  # Gather all (team_id, season_id, actual_season) tuples across seasons
  team_season_info <- list()
  for (s in seasons) {
    fixtures <- get_afl_fixtures(s)
    if (nrow(fixtures) == 0) next

    actual_season <- if ("season" %in% names(fixtures)) {
      fixtures$season[1]
    } else if ("compSeason.providerId" %in% names(fixtures)) {
      as.numeric(gsub("CD_S(\\d{4})\\d+", "\\1", fixtures$compSeason.providerId[1]))
    } else {
      s
    }
    season_id <- .afl_comp_season_id(actual_season)
    if (is.null(season_id)) next

    team_ids <- unique(c(fixtures$home.team.id, fixtures$away.team.id))
    team_ids <- team_ids[!is.na(team_ids)]

    for (tid in team_ids) {
      team_season_info[[length(team_season_info) + 1]] <- list(
        team_id = tid, season_id = season_id, actual_season = actual_season
      )
    }
  }

  if (length(team_season_info) == 0) return(tibble::tibble())

  # Build URLs and download all in one parallel batch
  urls <- vapply(team_season_info, function(x) {
    paste0("https://aflapi.afl.com.au/afl/v2/squads?teamId=", x$team_id,
           "&compSeasonId=", x$season_id)
  }, character(1))

  cli::cli_inform("Fetching player details for {length(urls)} team-season{?s} in parallel...")

  # In-memory curl pool — no temp files, no file descriptor limit
  pool <- curl::new_pool(total_con = 50L, host_con = 20L)
  details_list <- vector("list", length(urls))

  for (i in seq_along(urls)) {
    h <- curl::new_handle()
    local({
      idx <- i
      curl::curl_fetch_multi(urls[idx], done = function(resp) {
        if (resp$status_code == 200L) {
          tryCatch({
            json <- jsonlite::fromJSON(rawToChar(resp$content), flatten = TRUE)
            out <- .parse_squad_json(json)
            if (!is.null(out)) out$.actual_season <- team_season_info[[idx]]$actual_season
            details_list[[idx]] <<- out
          }, error = function(e) NULL)
        }
      }, fail = function(msg) NULL, handle = h, pool = pool)
    })
  }

  curl::multi_run(pool = pool)

  result <- purrr::list_rbind(purrr::compact(details_list))
  if (nrow(result) == 0) return(tibble::tibble())

  # Post-process per season group (age depends on season)
  fn_col <- intersect(c("player.firstName", "firstName"), names(result))[1]
  sn_col <- intersect(c("player.surname", "surname"), names(result))[1]
  if (!is.na(fn_col) && !is.na(sn_col)) {
    result$player_name <- paste(result[[fn_col]], result[[sn_col]])
  }
  dob_col <- intersect(c("player.dateOfBirth", "dateOfBirth"), names(result))[1]
  if (!is.na(dob_col)) {
    result$age <- lubridate::decimal_date(
      lubridate::as_date(paste0(result$.actual_season, "-07-01"))
    ) - lubridate::decimal_date(lubridate::as_date(result[[dob_col]]))
  }
  pid_col <- intersect(c("player.providerId", "providerId"), names(result))[1]

  # Strip "player." prefix
  new_names <- sub("^player\\.", "", names(result))
  duped <- new_names[duplicated(new_names)]
  if (length(duped) > 0) {
    collision <- new_names != names(result) & duplicated(new_names, fromLast = FALSE)
    new_names[collision] <- names(result)[collision]
  }
  names(result) <- new_names

  if (!is.na(pid_col)) {
    pid_col_clean <- sub("^player\\.", "", pid_col)
    result$row_id <- paste(result[[pid_col_clean]], result$.actual_season)
    if (pid_col_clean %in% names(result) && !"player_id" %in% names(result)) {
      names(result)[names(result) == pid_col_clean] <- "player_id"
    }
  }

  # Standardise team column
  if ("team.name" %in% names(result) && !"team" %in% names(result)) {
    names(result)[names(result) == "team.name"] <- "team"
  }
  if ("team" %in% names(result)) {
    result$team <- torp_replace_teams(result$team)
  }

  # Add season column, then clean up internal column
  result$season <- result$.actual_season
  result$.actual_season <- NULL

  tibble::as_tibble(result)
}


#' Resolve AFL API compSeasonId for a season
#'
#' The public AFL API (`aflapi.afl.com.au`) uses numeric comp season IDs,
#' not the `CD_S{year}014` format used by the CFS endpoint. This function
#' resolves the numeric ID via a cached HTTP lookup (at most once per session).
#'
#' @param season Numeric year
#' @return Numeric compSeasonId, or NULL if not found
#' @keywords internal
#'
#' @importFrom httr GET http_error content
#' @importFrom jsonlite fromJSON
.afl_comp_season_id <- function(season) {
  cache_key <- paste0("afl_comp_season_id_", season)
  cached <- get_from_cache(cache_key)
  if (!is.null(cached)) return(cached)

  # Fetch all comp seasons (cached once per session)
  seasons_list <- .afl_all_comp_seasons()
  if (is.null(seasons_list) || nrow(seasons_list) == 0) return(NULL)

  # Extract year from providerId (e.g. "CD_S2025014" → 2025)
  yr <- as.numeric(season)
  if ("providerId" %in% names(seasons_list)) {
    seasons_list$.year <- as.numeric(gsub("CD_S(\\d{4})\\d+", "\\1", seasons_list$providerId))
  } else if ("year" %in% names(seasons_list)) {
    seasons_list$.year <- as.numeric(seasons_list$year)
  } else {
    # Extract from name (e.g. "2025 Toyota AFL Premiership")
    seasons_list$.year <- as.numeric(gsub("^(\\d{4}).*", "\\1", seasons_list$name))
  }
  target <- seasons_list[seasons_list$.year == yr, ]
  if (nrow(target) == 0) return(NULL)

  season_id <- target$id[1]
  store_in_cache(cache_key, season_id)
  season_id
}

#' Fetch all AFL comp seasons (cached)
#'
#' Uses the public AFL API to look up competition season IDs.
#' The AFL Premiership competition code is "AFL" (or "CD_AFLPrem").
#'
#' @return A data.frame with id and year columns, or NULL on failure
#' @keywords internal
.afl_all_comp_seasons <- function() {
  cached <- get_from_cache("afl_all_comp_seasons")
  if (!is.null(cached)) return(cached)

  # Step 1: Get AFLM competition ID
  comp_resp <- tryCatch(
    httr::GET("https://aflapi.afl.com.au/afl/v2/competitions"),
    error = function(e) NULL
  )
  if (is.null(comp_resp) || httr::http_error(comp_resp)) {
    cli::cli_alert_danger("Could not reach AFL API competitions endpoint")
    return(NULL)
  }

  comp_text <- httr::content(comp_resp, as = "text", encoding = "UTF-8")
  comp_json <- tryCatch(
    jsonlite::fromJSON(comp_text, flatten = TRUE),
    error = function(e) {
      cli::cli_alert_danger("Failed to parse AFL API competitions response: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(comp_json)) return(NULL)

  # The response may have competitions at top level or nested
  comps <- comp_json$competitions %||% comp_json
  if (!is.data.frame(comps) || nrow(comps) == 0) return(NULL)

  # Find AFL Premiership competition — try multiple code patterns
  aflm <- if ("code" %in% names(comps)) {
    comps[comps$code %in% c("AFL", "AFLM", "CD_AFLPrem"), ]
  } else {
    comps[1, ]  # fallback: first competition
  }
  if (nrow(aflm) == 0) return(NULL)
  comp_id <- aflm$id[1]

  # Step 2: Get all comp seasons for this competition
  cs_resp <- tryCatch(
    httr::GET(paste0("https://aflapi.afl.com.au/afl/v2/competitions/", comp_id, "/compseasons")),
    error = function(e) NULL
  )
  if (is.null(cs_resp) || httr::http_error(cs_resp)) {
    cli::cli_alert_danger("Could not fetch comp seasons from AFL API (comp_id={comp_id})")
    return(NULL)
  }

  cs_text <- httr::content(cs_resp, as = "text", encoding = "UTF-8")
  cs_json <- tryCatch(
    jsonlite::fromJSON(cs_text, flatten = TRUE),
    error = function(e) {
      cli::cli_alert_danger("Failed to parse AFL API comp seasons response: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(cs_json)) return(NULL)

  # Seasons may be at top level or under $compSeasons
  seasons <- cs_json$compSeasons %||% cs_json
  if (!is.data.frame(seasons) || nrow(seasons) == 0) return(NULL)

  store_in_cache("afl_all_comp_seasons", seasons)
  seasons
}


#' Parse match roster response into a flat tibble
#'
#' @param resp Parsed JSON from the matchRoster endpoint
#' @param match_id The match providerId
#' @return A tibble with player lineup data
#' @keywords internal
.parse_match_roster <- function(resp, match_id) {
  parse_team <- function(team_data, team_type) {
    if (is.null(team_data) || length(team_data) == 0) return(NULL)

    # Force scalar — flatten=TRUE can produce vectors from nested fields
    team_id <- as.character(team_data$teamId)[1] %||% NA_character_
    team_name <- as.character(team_data$teamName)[1] %||% NA_character_

    # Players are in the 'positions' field
    positions <- team_data$positions
    if (is.null(positions)) return(NULL)

    # positions might be:
    # (a) A data.frame of individual players (26+ rows) — already flat
    # (b) A data.frame of position groups (3 rows) with nested 'players' list-column
    if (is.data.frame(positions)) {
      # Check for nested players column (position groups)
      list_cols <- names(positions)[vapply(positions, is.list, logical(1))]
      players_col <- intersect(list_cols, c("players", "player"))
      if (length(players_col) > 0 && nrow(positions) <= 10) {
        # Unnest position groups → individual players
        pos_name_col <- intersect(names(positions), c("positionName", "position", "name"))
        player_rows <- lapply(seq_len(nrow(positions)), function(i) {
          pl <- positions[[players_col[1]]][[i]]
          if (is.null(pl) || !is.data.frame(pl) || nrow(pl) == 0) return(NULL)
          if (length(pos_name_col) > 0) {
            pl$position <- positions[[pos_name_col[1]]][i]
          }
          pl
        })
        players <- dplyr::bind_rows(player_rows)
      } else {
        # Already player-level rows
        players <- positions
      }
    } else if (is.list(positions)) {
      # Raw list — try to bind
      players <- tryCatch(dplyr::bind_rows(positions), error = function(e) {
        cli::cli_alert_danger("Failed to bind roster positions for match {match_id} ({team_type}): {conditionMessage(e)}")
        NULL
      })
    } else {
      return(NULL)
    }

    if (is.null(players) || !is.data.frame(players) || nrow(players) == 0) return(NULL)

    # Drop any remaining list-columns before adding metadata
    lcols <- names(players)[vapply(players, is.list, logical(1))]
    if (length(lcols) > 0) {
      players <- players[, !names(players) %in% lcols, drop = FALSE]
    }

    players$teamId <- team_id
    players$teamName <- team_name
    players$teamType <- team_type
    players$providerId <- match_id
    players
  }

  roster <- resp$matchRoster %||% resp
  home <- parse_team(roster$homeTeam, "home")
  away <- parse_team(roster$awayTeam, "away")
  result <- dplyr::bind_rows(home, away)
  if (is.null(result) || nrow(result) == 0) return(tibble::tibble())
  result
}


#' Parse match player stats response
#'
#' @param resp Parsed JSON from the playerStats endpoint
#' @param match_id The match providerId
#' @return A tibble with player stat data
#' @keywords internal
.parse_match_stats <- function(resp, match_id) {
  parse_team_stats <- function(team_stats, team_status) {
    if (is.null(team_stats) || length(team_stats) == 0) return(NULL)

    # Handle both data.frame and list inputs
    if (is.data.frame(team_stats)) {
      df <- team_stats
    } else {
      df <- tryCatch(
        jsonlite::fromJSON(jsonlite::toJSON(team_stats, auto_unbox = TRUE), flatten = TRUE),
        error = function(e) {
          cli::cli_alert_danger("Failed to parse {team_status} team stats for match {match_id}: {conditionMessage(e)}")
          NULL
        }
      )
    }
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

    # Drop list-columns
    lcols <- names(df)[vapply(df, is.list, logical(1))]
    if (length(lcols) > 0) {
      df <- df[, !names(df) %in% lcols, drop = FALSE]
    }

    # Strip playerStats. prefix from column names
    names(df) <- gsub("^playerStats\\.", "", names(df))

    # Deduplicate column names (flatten=TRUE can create teamId...1, teamId...10)
    duped <- duplicated(names(df))
    if (any(duped)) {
      df <- df[, !duped, drop = FALSE]
    }

    df$teamStatus <- team_status
    df$providerId <- match_id
    df
  }

  home <- parse_team_stats(resp$homeTeamPlayerStats, "home")
  away <- parse_team_stats(resp$awayTeamPlayerStats, "away")
  dplyr::bind_rows(home, away)
}


#' Parse squad JSON into a player data.frame
#'
#' @param json Parsed JSON from the squads endpoint
#' @return A data.frame of players with team info, or NULL
#' @keywords internal
.parse_squad_json <- function(json) {
  players <- json$squad$players
  if (is.null(players) || !is.data.frame(players) || nrow(players) == 0) return(NULL)

  players$team.name <- json$squad$team$name %||% NA_character_
  players$team.providerId <- json$squad$team$providerId %||% NA_character_

  # Drop list-columns
  list_cols <- names(players)[vapply(players, is.list, logical(1))]
  if (length(list_cols) > 0) {
    players <- players[, !names(players) %in% list_cols, drop = FALSE]
  }
  players
}


# Exported API Functions ----

#' Fetch AFL Fixtures
#'
#' Fetches fixture data for a season from the AFL API in a single HTTP call.
#' Includes scores for completed games (same data the results endpoint returns).
#'
#' @param season Numeric year, or `TRUE` for all available seasons (default: current season via [get_afl_season()])
#' @return A tibble of fixture data
#' @export
#'
#' @examples
#' \dontrun{
#' fixtures <- get_afl_fixtures()
#' fixtures <- get_afl_fixtures(2025)
#' }
#'
#' @importFrom httr GET stop_for_status content
#' @importFrom jsonlite fromJSON
get_afl_fixtures <- function(season = NULL) {
  # TRUE = all available seasons
  if (isTRUE(season)) {
    seasons_df <- .afl_all_comp_seasons()
    if (is.null(seasons_df) || nrow(seasons_df) == 0) {
      cli::cli_abort("Could not fetch available seasons from AFL API.")
    }
    all_ids <- seasons_df$id
    cli::cli_inform("Fetching fixtures for {length(all_ids)} season{?s}...")
    results <- purrr::map(all_ids, function(sid) {
      tryCatch(.fetch_fixtures_for_season_id(sid), error = function(e) {
        cli::cli_alert_danger("Failed to fetch fixtures for season ID {sid}: {conditionMessage(e)}")
        NULL
      })
    })
    n_failed <- sum(vapply(results, is.null, logical(1)))
    if (n_failed > 0) {
      cli::cli_alert_danger("{n_failed} of {length(all_ids)} season{?s} failed to load")
    }
    all_fixtures <- purrr::list_rbind(purrr::compact(results))
    .normalise_fixture_columns(all_fixtures)
    return(all_fixtures)
  }

  if (is.null(season)) {
    season <- get_afl_season()
    auto_season <- TRUE
  } else {
    auto_season <- FALSE
  }

  result <- .fetch_fixtures_for_season(season)

  # If the default season doesn't exist yet (pre-season), fall back to previous year
  if (is.null(result) && auto_season) {
    cli::cli_inform("Season {season} not yet available in AFL API, falling back to {season - 1}.")
    result <- .fetch_fixtures_for_season(season - 1L)
  }

  if (is.null(result)) {
    cli::cli_abort("Could not fetch fixtures for season {season}. The AFL API may not have this season yet.")
  }

  result
}

#' Fetch fixtures for a single season by year (internal)
#' @param season Numeric year
#' @return A tibble, or NULL if the API returns an error
#' @keywords internal
.fetch_fixtures_for_season <- function(season) {
  cache_key <- paste0("afl_fixtures_", season)
  cached <- get_from_cache(cache_key)
  if (!is.null(cached)) return(cached)

  season_id <- .afl_comp_season_id(season)
  if (is.null(season_id)) return(NULL)
  result <- .fetch_fixtures_for_season_id(season_id)
  if (!is.null(result) && nrow(result) > 0) {
    store_in_cache(cache_key, result)
  }
  result
}

#' Fetch fixtures for a single comp season ID (internal)
#' @param season_id Numeric comp season ID (e.g. 85)
#' @return A tibble, or NULL if the API returns an error
#' @keywords internal
.fetch_fixtures_for_season_id <- function(season_id) {
  url <- paste0(
    "https://aflapi.afl.com.au/afl/v2/matches?compSeasonId=", season_id,
    "&pageSize=1000"
  )

  resp <- tryCatch(httr::GET(url), error = function(e) {
    cli::cli_alert_danger("HTTP request failed for season {season_id}: {conditionMessage(e)}")
    NULL
  })
  if (is.null(resp) || httr::http_error(resp)) {
    if (!is.null(resp)) {
      cli::cli_alert_danger("AFL API returned HTTP {httr::status_code(resp)} for season {season_id}")
    }
    return(NULL)
  }

  json <- tryCatch(
    httr::content(resp, as = "text", encoding = "UTF-8") |>
      jsonlite::fromJSON(flatten = TRUE),
    error = function(e) {
      cli::cli_alert_danger("Failed to parse fixtures JSON for season {season_id}: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(json)) return(NULL)

  matches <- json$matches
  if (is.null(matches) || length(matches) == 0) return(NULL)

  # Drop list-columns (nested structs) that arrow can't serialize
  list_cols <- names(matches)[vapply(matches, is.list, logical(1))]
  if (length(list_cols) > 0) {
    matches <- matches[, !names(matches) %in% list_cols, drop = FALSE]
  }

  # Add compSeason.year (extracted from providerId) — normalised to `season` by .normalise_fixture_columns()
  if (!"compSeason.year" %in% names(matches) && "compSeason.providerId" %in% names(matches)) {
    matches$compSeason.year <- as.numeric(
      gsub("CD_S(\\d{4})\\d+", "\\1", matches$compSeason.providerId)
    )
  }

  result <- tibble::as_tibble(matches)
  .normalise_fixture_columns(result)
  result
}


#' Fetch AFL Results
#'
#' Returns completed match results for a season. Zero additional HTTP calls —
#' derives results from fixture data (which includes scores).
#'
#' @param season Numeric year (default: current season via [get_afl_season()])
#' @return A tibble of completed match data (fixture schema, filtered to concluded games)
#' @export
#'
#' @examples
#' \dontrun{
#' results <- get_afl_results(2025)
#' }
get_afl_results <- function(season = NULL) {
  fixtures <- get_afl_fixtures(season)
  if (nrow(fixtures) == 0) return(tibble::tibble())

  # Filter to completed games
  if ("status" %in% names(fixtures)) {
    results <- fixtures[fixtures$status == "CONCLUDED", ]
  } else {
    # Fallback: games with non-zero scores
    cli::cli_inform("No 'status' column in fixtures -- using score-based fallback for completed games")
    results <- fixtures[
      !is.na(fixtures$home_score) & fixtures$home_score > 0, ]
  }

  results
}


#' Fetch AFL Lineups
#'
#' Fetches team lineups/rosters for a season (optionally filtered by round).
#' Uses a single shared auth token across all per-match roster calls.
#'
#' @param season Numeric year (default: current season via [get_afl_season()])
#' @param round Optional round number to filter to. If NULL, fetches all rounds.
#' @return A tibble of player lineup data
#' @export
#'
#' @examples
#' \dontrun{
#' lineups <- get_afl_lineups(2025, round = 1)
#' }
#'
#' @importFrom httr GET add_headers content
#' @importFrom purrr map list_rbind
get_afl_lineups <- function(season = NULL, round = NULL) {
  fixtures <- get_afl_fixtures(season)
  if (nrow(fixtures) == 0) return(tibble::tibble())

  if (!is.null(round)) {
    fixtures <- fixtures[fixtures$round_number %in% round, ]
    if (nrow(fixtures) == 0) {
      cli::cli_alert_danger("No fixtures for season {season} round {round}")
      return(tibble::tibble())
    }
  }

  # Only fetch rosters for matches that have lineup data (not future scheduled)
  if ("status" %in% names(fixtures)) {
    fixtures <- fixtures[fixtures$status != "SCHEDULED", ]
    if (nrow(fixtures) == 0) return(tibble::tibble())
  }

  match_ids <- fixtures$match_id
  token <- get_token()

  result <- .fetch_cfs_batch(
    ids = match_ids,
    url_template = "https://api.afl.com.au/cfs/afl/matchRoster/full/%s",
    token = token,
    parse_fn = .parse_match_roster,
    label = "roster"
  )
  if (nrow(result) == 0) return(tibble::tibble())

  # Add season and row_id to match existing schema
  result$season <- as.numeric(substr(result$providerId, 5, 8))
  if ("player.playerId" %in% names(result)) {
    result$row_id <- paste0(result$providerId, result$teamId, result$player.playerId)
  }

  # Normalise column names (providerId → match_id, teamId → team_id, etc.)
  .normalise_teams_columns(result)

  result
}


#' Fetch AFL Player Stats
#'
#' Fetches per-player per-match stats for a season.
#' Uses cached fixtures for match IDs and a single shared auth token.
#'
#' @param season Numeric year (default: current season via [get_afl_season()])
#' @return A tibble of player match stats
#' @export
#'
#' @examples
#' \dontrun{
#' stats <- get_afl_player_stats(2025)
#' }
#'
#' @importFrom httr GET add_headers content
#' @importFrom purrr map list_rbind
get_afl_player_stats <- function(season = NULL) {
  fixtures <- get_afl_fixtures(season)
  if (nrow(fixtures) == 0) return(tibble::tibble())

  # Only fetch stats for concluded matches
  concluded <- fixtures[fixtures$status == "CONCLUDED", ]
  if (nrow(concluded) == 0) return(tibble::tibble())

  match_ids <- concluded$match_id
  token <- get_token()

  result <- .fetch_cfs_batch(
    ids = match_ids,
    url_template = "https://api.afl.com.au/cfs/afl/playerStats/match/%s",
    token = token,
    parse_fn = .parse_match_stats,
    label = "player stats"
  )
  if (nrow(result) == 0) return(tibble::tibble())

  # Join match details from fixtures (normalised column names)
  join_cols <- intersect(
    c("match_id", "venue_name", "round_number",
      "home_team_name", "away_team_name", "utc_start_time"),
    names(concluded)
  )
  match_info <- concluded[, join_cols, drop = FALSE]
  result <- dplyr::left_join(result, match_info, by = c("providerId" = "match_id"))

  # Rename providerId → match_id to match canonical naming
  if ("providerId" %in% names(result)) {
    names(result)[names(result) == "providerId"] <- "match_id"
  }

  result
}


#' Fetch AFL Player Details
#'
#' Fetches player biographical/squad details for a season.
#' Uses team IDs from fixture data and the public squads endpoint.
#'
#' @param season Numeric year (default: current season via [get_afl_season()])
#' @return A tibble of player details with player_name, age, row_id columns
#' @export
#'
#' @examples
#' \dontrun{
#' details <- get_afl_player_details(2025)
#' }
#'
#' @importFrom httr GET stop_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map list_rbind
get_afl_player_details <- function(season = NULL) {
  fixtures <- get_afl_fixtures(season)
  if (nrow(fixtures) == 0) return(tibble::tibble())

  # Derive actual season from fixture data (handles auto-fallback)
  actual_season <- if ("season" %in% names(fixtures)) {
    fixtures$season[1]
  } else if ("compSeason.providerId" %in% names(fixtures)) {
    as.numeric(gsub("CD_S(\\d{4})\\d+", "\\1", fixtures$compSeason.providerId[1]))
  } else {
    season %||% get_afl_season()
  }
  season_id <- .afl_comp_season_id(actual_season)
  if (is.null(season_id)) {
    cli::cli_abort("Could not resolve comp season ID for {actual_season}")
  }

  # Get unique numeric team IDs (the public API needs numeric IDs, not CD_T providerIds)
  team_ids <- unique(c(fixtures$home.team.id, fixtures$away.team.id))
  team_ids <- team_ids[!is.na(team_ids)]

  cli::cli_inform("Fetching player details for {length(team_ids)} team{?s} in parallel...")

  urls <- paste0(
    "https://aflapi.afl.com.au/afl/v2/squads?teamId=", team_ids,
    "&compSeasonId=", season_id
  )

  # In-memory curl pool — no temp files
  pool <- curl::new_pool(total_con = 50L, host_con = 20L)
  details_list <- vector("list", length(team_ids))

  for (i in seq_along(urls)) {
    h <- curl::new_handle()
    local({
      idx <- i
      curl::curl_fetch_multi(urls[idx], done = function(resp) {
        if (resp$status_code == 200L) {
          tryCatch({
            json <- jsonlite::fromJSON(rawToChar(resp$content), flatten = TRUE)
            details_list[[idx]] <<- .parse_squad_json(json)
          }, error = function(e) {
            cli::cli_alert_danger("Failed to parse squad for team {team_ids[idx]}: {conditionMessage(e)}")
          })
        }
      }, fail = function(msg) {
        cli::cli_alert_danger("Failed to fetch squad: {msg}")
      }, handle = h, pool = pool)
    })
  }

  curl::multi_run(pool = pool)

  result <- purrr::list_rbind(purrr::compact(details_list))
  if (nrow(result) == 0) return(tibble::tibble())

  # Add derived columns to match existing post-processing
  fn_col <- intersect(c("player.firstName", "firstName"), names(result))[1]
  sn_col <- intersect(c("player.surname", "surname"), names(result))[1]
  if (!is.na(fn_col) && !is.na(sn_col)) {
    result$player_name <- paste(result[[fn_col]], result[[sn_col]])
  }
  dob_col <- intersect(c("player.dateOfBirth", "dateOfBirth"), names(result))[1]
  if (!is.na(dob_col)) {
    result$age <- lubridate::decimal_date(lubridate::as_date(paste0(actual_season, "-07-01"))) -
      lubridate::decimal_date(lubridate::as_date(result[[dob_col]]))
  }
  pid_col <- intersect(c("player.providerId", "providerId"), names(result))[1]

  # Standardise column names — strip "player." prefix from flattened API response
  new_names <- sub("^player\\.", "", names(result))
  duped <- new_names[duplicated(new_names)]
  if (length(duped) > 0) {
    cli::cli_alert_danger("Prefix stripping created duplicate columns: {.val {unique(duped)}}. Keeping originals for conflicts.")
    collision <- new_names != names(result) & duplicated(new_names, fromLast = FALSE)
    new_names[collision] <- names(result)[collision]
  }
  names(result) <- new_names

  if (!is.na(pid_col)) {
    pid_col_clean <- sub("^player\\.", "", pid_col)
    result$row_id <- paste(result[[pid_col_clean]], actual_season)
    # Rename providerId → player_id (this is a player ID, not a match ID)
    if (pid_col_clean %in% names(result) && !"player_id" %in% names(result)) {
      names(result)[names(result) == pid_col_clean] <- "player_id"
    }
  }

  # Standardise team column name and values
  if ("team.name" %in% names(result) && !"team" %in% names(result)) {
    names(result)[names(result) == "team.name"] <- "team"
  }
  if ("team" %in% names(result)) {
    result$team <- torp_replace_teams(result$team)
  }

  result$season <- actual_season

  tibble::as_tibble(result)
}


# Team/Venue Name Standardisation ----

#' Standardise AFL Team Names
#'
#' Maps team name variants (abbreviations, nicknames, Indigenous round names)
#' to canonical team names using [AFL_TEAM_ALIASES]. Drop-in replacement for
#' `fitzRoy::replace_teams()`.
#'
#' @param team Character vector of team names
#' @return Character vector with standardised names. Unknown values pass through unchanged.
#' @export
#'
#' @examples
#' torp_replace_teams("Adelaide Crows")
#' torp_replace_teams(c("GWS Giants", "Narrm", "WB"))
torp_replace_teams <- function(team) {
  if (length(team) == 0L) return(character(0))
  mapped <- unname(AFL_TEAM_ALIASES[team])
  ifelse(!is.na(mapped), mapped, team)
}


#' Get AFL Team Abbreviation
#'
#' Converts any team name variant to its canonical AFL API abbreviation.
#'
#' @param team Character vector of team names (any recognised variant)
#' @return Character vector of abbreviations (e.g. "ADEL", "BL", "WB")
#' @export
#'
#' @examples
#' torp_team_abbr("Adelaide Crows")
#' torp_team_abbr(c("Narrm", "Western Bulldogs", "CARL"))
torp_team_abbr <- function(team) {
  canonical <- torp_replace_teams(team)
  AFL_TEAMS$abbr[match(canonical, AFL_TEAMS$name)]
}


#' Get AFL Team Full Name
#'
#' Converts any team name variant to its canonical full name.
#'
#' @param team Character vector of team names (any recognised variant)
#' @return Character vector of full names (e.g. "Adelaide Crows", "GWS Giants")
#' @export
#'
#' @examples
#' torp_team_full("Adelaide")
#' torp_team_full(c("WB", "Narrm", "Cats"))
torp_team_full <- function(team) {
  canonical <- torp_replace_teams(team)
  AFL_TEAMS$full[match(canonical, AFL_TEAMS$name)]
}


#' Standardise AFL Venue Names
#'
#' Maps venue name variants (sponsor names, old names) to canonical stable names.
#' Drop-in replacement for `fitzRoy::replace_venues()`.
#'
#' @param venue Character vector of venue names
#' @return Character vector with standardised names
#' @export
#'
#' @examples
#' torp_replace_venues("Marvel Stadium")
#' torp_replace_venues(c("MCG", "Optus Stadium"))
#'
#' @importFrom dplyr case_when
torp_replace_venues <- function(venue) {
  dplyr::case_when(
    venue == "Blacktown International" ~ "Blacktown",
    venue == "Blundstone Arena" ~ "Bellerive Oval",
    venue == "People First Stadium" ~ "Carrara",
    venue == "Carrara Stadium" ~ "Carrara",
    venue == "Metricon Stadium" ~ "Carrara",
    venue == "Etihad Stadium" ~ "Docklands",
    venue == "Marvel Stadium" ~ "Docklands",
    venue == "Docklands Stadium" ~ "Docklands",
    venue == "Mars Stadium" ~ "Eureka Stadium",
    venue == "The Gabba" ~ "Gabba",
    venue == "AAMI Stadium" ~ "Football Park",
    venue == "GMHBA Stadium" ~ "Kardinia Park",
    venue == "Melbourne Cricket Ground" ~ "M.C.G.",
    venue == "MCG" ~ "M.C.G.",
    venue == "UNSW Canberra Oval" ~ "Manuka Oval",
    venue == "Canberra Oval" ~ "Manuka Oval",
    venue == "TIO Stadium" ~ "Marrara Oval",
    venue == "Optus Stadium" ~ "Perth Stadium",
    venue == "Sydney Cricket Ground" ~ "S.C.G.",
    venue == "SCG" ~ "S.C.G.",
    venue == "Accor Stadium" ~ "Stadium Australia",
    venue == "ANZ Stadium" ~ "Stadium Australia",
    venue == "Domain Stadium" ~ "Subiaco",
    venue == "Adelaide Hills" ~ "Summit Sports Park",
    venue == "Summit Sport and Recreation Park" ~ "Summit Sports Park",
    venue == "Sydney Showground Stadium" ~ "Sydney Showground",
    venue == "Spotless Stadium" ~ "Sydney Showground",
    venue == "Showground Stadium" ~ "Sydney Showground",
    venue == "GIANTS Stadium" ~ "Sydney Showground",
    venue == "ENGIE Stadium" ~ "Sydney Showground",
    venue == "TIO Traeger Park" ~ "Traeger Park",
    venue == "TIO Traegar Park" ~ "Traeger Park",
    venue == "Westpac Stadium" ~ "Wellington",
    venue == "University of Tasmania Stadium" ~ "York Park",
    venue == "UTAS Stadium" ~ "York Park",
    TRUE ~ venue
  )
}
