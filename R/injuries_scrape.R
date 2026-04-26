# AFL Injury Data: Scraping & Loading
# ===================================
# Sources for injury data: scrape the live afl.com.au weekly injury list and
# load the curated preseason CSV from inst/extdata. Combined and matched to
# rating data downstream (see R/injuries_match.R).

#' AFL Injury Data Functions
#'
#' Centralized functions for scraping, loading, matching, and validating AFL
#' injury data. Used by both the predictions pipeline and season simulation.
#'
#' @name injuries
NULL


#' Scrape AFL Weekly Injury List
#'
#' Scrapes the current injury list from afl.com.au. The page contains 18 tables
#' (one per team) in alphabetical team order. Each table's last row contains an
#' "Updated: ..." date stamp. Returns player names, injury descriptions,
#' estimated return timelines, team names, and the per-team updated date.
#'
#' @param timeout Numeric timeout in seconds for the HTTP request.
#' @return A data.frame with columns: `player`, `team`, `injury`,
#'   `estimated_return`, `updated`, `player_norm`.
#' @export
scrape_injuries <- function(timeout = 30) {
  empty_df <- data.frame(
    player = character(), team = character(), injury = character(),
    estimated_return = character(), updated = as.Date(character()),
    player_norm = character(), stringsAsFactors = FALSE
  )

  tryCatch({
    if (!requireNamespace("rvest", quietly = TRUE)) {
      cli::cli_abort("Package {.pkg rvest} is required to scrape injuries.")
    }
    url <- "https://www.afl.com.au/matches/injury-list"
    session <- rvest::session(url, httr::timeout(timeout))
    raw_tables <- rvest::html_table(session)

    # AFL page has 18 tables in alphabetical team order
    team_names <- sort(AFL_TEAMS$name)
    n_tables <- length(raw_tables)

    if (n_tables != length(team_names)) {
      cli::cli_warn("Expected {length(team_names)} injury tables but found {n_tables}")
      # Fall back to as many as we can match
      team_names <- team_names[seq_len(min(n_tables, length(team_names)))]
    }

    all_rows <- vector("list", n_tables)

    for (i in seq_len(min(n_tables, length(team_names)))) {
      tbl <- torp_clean_names(raw_tables[[i]])

      if (nrow(tbl) == 0) next

      # Last row contains "Updated: March 17, 2026" -- extract and remove it
      last_vals <- as.character(tbl[nrow(tbl), 1])
      updated_date <- NA
      if (grepl("^Updated:", last_vals, ignore.case = TRUE)) {
        date_str <- trimws(sub("^Updated:\\s*", "", last_vals, ignore.case = TRUE))
        updated_date <- tryCatch(
          as.Date(date_str, format = "%B %d, %Y"),
          error = function(e) NA
        )
        tbl <- tbl[-nrow(tbl), , drop = FALSE]
      }

      if (nrow(tbl) == 0) next

      tbl$team <- team_names[i]
      tbl$updated <- updated_date
      all_rows[[i]] <- tbl
    }

    inj <- do.call(rbind, all_rows[!vapply(all_rows, is.null, logical(1))])
    if (is.null(inj) || nrow(inj) == 0) return(empty_df)

    # Standardise known name mismatches between AFL injury list and player data
    # Add new entries here as they arise (injury_name = canonical_name)
    injury_name_fixes <- c(
      "Cam Zurhaar" = "Cameron Zurhaar"
    )
    fix_idx <- match(inj$player, names(injury_name_fixes))
    inj$player[!is.na(fix_idx)] <- injury_name_fixes[fix_idx[!is.na(fix_idx)]]

    inj$player_norm <- norm_name(inj$player)
    inj
  }, error = function(e) {
    cli::cli_warn("Failed to scrape injury list: {conditionMessage(e)}")
    empty_df
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
