#' AFL Injury Data Functions
#'
#' Centralized functions for scraping, loading, and matching AFL injury data.
#' Used by both the predictions pipeline and season simulation system.
#'
#' @name injuries
NULL


#' Scrape AFL Weekly Injury List
#'
#' Scrapes the current injury list from afl.com.au. Returns player names,
#' injury descriptions, and estimated return timelines.
#'
#' @param timeout Numeric timeout in seconds for the HTTP request.
#' @return A data.frame with columns: `player`, `injury`, `estimated_return`,
#'   `player_norm`.
#' @export
scrape_injuries <- function(timeout = 30) {
  tryCatch({
    if (!requireNamespace("rvest", quietly = TRUE)) {
      cli::cli_abort("Package {.pkg rvest} is required to scrape injuries.")
    }
    url <- "https://www.afl.com.au/matches/injury-list"
    session <- rvest::session(url, httr::timeout(timeout))
    inj <- session |>
      rvest::html_table() |>
      do.call(rbind, args = _) |>
      torp_clean_names()

    # Standardise known name mismatches between AFL injury list and player data
    inj$player <- dplyr::case_match(
      inj$player,
      "Cam Zurhaar" ~ "Cameron Zurhaar",
      .default = inj$player
    )

    inj$player_norm <- norm_name(inj$player)
    inj
  }, error = function(e) {
    cli::cli_warn("Failed to scrape injury list: {conditionMessage(e)}")
    data.frame(
      player = character(),
      injury = character(),
      estimated_return = character(),
      player_norm = character(),
      stringsAsFactors = FALSE
    )
  })
}


#' Load Preseason Injury List
#'
#' Reads a curated CSV of preseason injuries from `inst/extdata/`. These
#' capture long-term injuries known before the season starts (e.g., ACL
#' reconstructions, stress fractures) that won't appear on the weekly AFL
#' injury list until teams are required to report.
#'
#' @param season Numeric season year (e.g. 2026).
#' @return A data.frame with columns: `player`, `team`, `injury`,
#'   `estimated_return`, `player_norm`. Returns an empty data.frame (with
#'   correct columns) if no file exists for the requested season.
#' @export
load_preseason_injuries <- function(season) {
  filename <- paste0("preseason_injuries_", season, ".csv")
  path <- system.file("extdata", filename, package = "torp")

  empty_df <- data.frame(
    player = character(),
    team = character(),
    injury = character(),
    estimated_return = character(),
    player_norm = character(),
    stringsAsFactors = FALSE
  )

  if (path == "") {
    cli::cli_alert_info("No preseason injury file for {season}")
    return(empty_df)
  }

  inj <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE),
    error = function(e) {
      cli::cli_warn("Failed to read preseason injuries: {conditionMessage(e)}")
      return(empty_df)
    }
  )

  if (nrow(inj) == 0) return(empty_df)

  # Ensure expected columns exist
  expected <- c("player", "team", "injury", "estimated_return")
  missing <- setdiff(expected, names(inj))
  if (length(missing) > 0) {
    cli::cli_warn("Preseason injury file missing columns: {paste(missing, collapse = ', ')}")
    return(empty_df)
  }

  inj$player_norm <- norm_name(inj$player)
  inj
}


#' Get All Injuries (Weekly + Preseason)
#'
#' Combines the live AFL weekly injury list with the curated preseason
#' injury list. Weekly entries take precedence when a player appears in both
#' sources (deduplicated by normalized name).
#'
#' @param season Numeric season year (e.g. 2026).
#' @param scrape Logical; if TRUE (default), scrapes the live AFL injury list.
#'   Set to FALSE for pre-season use or testing when the AFL page is empty.
#' @return A data.frame with columns: `player`, `injury`, `estimated_return`,
#'   `player_norm`, `source`. The `source` column is `"weekly"` or
#'   `"preseason"`.
#' @export
get_all_injuries <- function(season, scrape = TRUE) {
  preseason <- load_preseason_injuries(season)
  if (nrow(preseason) > 0) {
    preseason$source <- "preseason"
  }

  if (scrape) {
    weekly <- scrape_injuries()
    if (nrow(weekly) > 0) {
      weekly$source <- "weekly"
      # Ensure team column exists (weekly scrape may not have it)
      if (!"team" %in% names(weekly)) weekly$team <- NA_character_
    }
  } else {
    weekly <- data.frame(
      player = character(), injury = character(),
      estimated_return = character(), player_norm = character(),
      source = character(), team = character(),
      stringsAsFactors = FALSE
    )
  }

  if (nrow(weekly) == 0 && nrow(preseason) == 0) {
    return(data.frame(
      player = character(), injury = character(),
      estimated_return = character(), player_norm = character(),
      source = character(), team = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Ensure both have the same columns before binding
  all_cols <- c("player", "team", "injury", "estimated_return", "player_norm", "source")
  for (col in all_cols) {
    if (!col %in% names(weekly)) weekly[[col]] <- rep(NA_character_, nrow(weekly))
    if (!col %in% names(preseason)) preseason[[col]] <- rep(NA_character_, nrow(preseason))
  }

  combined <- rbind(
    weekly[, all_cols, drop = FALSE],
    preseason[, all_cols, drop = FALSE]
  )

  # Deduplicate: weekly takes precedence over preseason.
  # Use (player_norm, team) when team is available to handle same-name players
  # (e.g., Max King at St Kilda vs Max King at Sydney Swans).
  has_team <- !all(is.na(combined$team))
  if (has_team) {
    dedup_key <- paste(combined$player_norm, combined$team, sep = "|")
  } else {
    dedup_key <- combined$player_norm
  }
  combined <- combined[!duplicated(dedup_key, fromLast = FALSE), ]
  combined
}


#' Match Injuries to a Ratings Data Frame
#'
#' Joins injury data to any data frame containing a `player_name` column
#' (e.g., TORP ratings). Matching uses normalized names for robustness.
#'
#' @param ratings_df A data.frame with a `player_name` column.
#' @param injuries_df A data.frame from [get_all_injuries()] or
#'   [scrape_injuries()] with a `player_norm` column.
#' @return The input `ratings_df` with `injury` and `estimated_return` columns
#'   added. Healthy players have NA values.
#' @export
match_injuries <- function(ratings_df, injuries_df) {
  if (is.null(injuries_df) || nrow(injuries_df) == 0) {
    ratings_df$injury <- NA_character_
    ratings_df$estimated_return <- NA_character_
    return(ratings_df)
  }

  if (!"player_name" %in% names(ratings_df)) {
    cli::cli_abort("{.arg ratings_df} must have a {.field player_name} column.")
  }

  ratings_df$player_norm <- norm_name(ratings_df$player_name)

  # Keep only the columns we need for the join
  inj_join <- injuries_df[, c("player_norm", "injury", "estimated_return"), drop = FALSE]
  inj_join <- inj_join[!duplicated(inj_join$player_norm), ]

  merged <- merge(ratings_df, inj_join, by = "player_norm", all.x = TRUE)

  # Remove the temporary join column
  merged$player_norm <- NULL
  merged
}
