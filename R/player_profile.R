#' Resolve a player name to a unique player ID
#'
#' Internal helper that takes a player name string (partial match OK) and
#' resolves it to a single player ID using fuzzy matching via \code{norm_name()}.
#'
#' @param player_name A character string of the player's name (full or partial).
#' @param seasons Seasons to search across. Passed to \code{load_player_details()}.
#'
#' @return A list with elements \code{player_id}, \code{player_name}, \code{team}, \code{position}.
#'
#' @importFrom cli cli_abort cli_warn
#' @keywords internal
resolve_player <- function(player_name, seasons = TRUE) {
  if (!is.character(player_name) || length(player_name) != 1 || nchar(trimws(player_name)) == 0) {
    cli::cli_abort("{.arg player_name} must be a non-empty character string")
  }

  details <- load_player_details(seasons)

  # Build a normalised full name column for matching
  details$full_name_norm <- norm_name(paste(details$firstName, details$surname))
  search_norm <- norm_name(player_name)

  matches <- details[grepl(search_norm, details$full_name_norm, fixed = TRUE), ]

  if (nrow(matches) == 0) {
    cli::cli_abort("No player found matching {.val {player_name}}")
  }

  # Deduplicate to unique players (keep latest season entry)
  matches <- matches[order(matches$season, decreasing = TRUE), ]
  unique_players <- matches[!duplicated(matches$providerId), ]

  if (nrow(unique_players) > 1) {
    player_list <- paste(
      paste(unique_players$firstName, unique_players$surname),
      paste0("(", unique_players$team, ")"),
      collapse = ", "
    )
    cli::cli_warn("Multiple players match {.val {player_name}}: {player_list}. Using first match.")
  }

  picked <- unique_players[1, ]

  list(
    player_id = picked$providerId,
    player_name = paste(picked$firstName, picked$surname),
    team = picked$team,
    position = picked$position
  )
}

#' Get a Player Profile
#'
#' Combines raw stats, TORP season ratings, and current TORP rating for a
#' player into a single object. Accepts partial name matches (e.g. "Heeney").
#'
#' @param player_name A character string of the player's name (full or partial).
#' @param seasons Seasons to include. Numeric vector of years, or \code{TRUE} for all available.
#'
#' @return A list of class \code{torp_player_profile} with elements:
#' \describe{
#'   \item{player_info}{1-row tibble with player_id, name, team, position.}
#'   \item{yearly_stats}{Per-season aggregated raw stats from \code{load_player_stats()}.}
#'   \item{torp_season}{Per-season TORP ratings from \code{player_season_ratings()}.}
#'   \item{current_torp}{Current TORP rating from \code{calculate_torp_ratings()}.}
#' }
#'
#' @export
#'
#' @importFrom data.table as.data.table
#' @importFrom cli cli_abort
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   player_profile("Heeney")
#'   player_profile("Heeney", seasons = 2022:2024)
#' })
#' }
player_profile <- function(player_name, seasons = TRUE) {
  player <- resolve_player(player_name, seasons = seasons)
  pid <- player$player_id

  # --- Player info ---
  player_info <- data.frame(
    player_id = pid,
    name = player$player_name,
    team = player$team,
    position = player$position,
    stringsAsFactors = FALSE
  )

  # --- Yearly raw stats ---
  raw <- tryCatch(load_player_stats(seasons), error = function(e) NULL)

  if (!is.null(raw)) {
    dt <- data.table::as.data.table(raw)

    # player_id column is normalised by load_player_stats()
    if (!"player_id" %in% names(dt)) {
      cli::cli_warn("No {.val player_id} column found in player stats data.")
    } else {
      dt <- dt[dt[["player_id"]] == pid]
    }

    # Detect season column
    if (!"season" %in% names(dt)) {
      if ("utc_start_time" %in% names(dt)) {
        dt[, season := as.integer(format(as.Date(utc_start_time), "%Y"))]
      }
    }

    # Stat columns to aggregate (sum per season)
    sum_cols <- c("goals", "behinds", "shots_at_goal", "disposals", "kicks",
                  "handballs", "inside50s", "marks", "tackles",
                  "contested_possessions", "clearances_total_clearances")
    # Percentage columns to average per season
    pct_cols <- c("disposal_efficiency", "time_on_ground_percentage")

    avail_sum <- intersect(sum_cols, names(dt))
    avail_pct <- intersect(pct_cols, names(dt))

    if ((length(avail_sum) + length(avail_pct)) > 0 && nrow(dt) > 0) {
      # Sum stats + games count
      yearly_stats <- dt[, c(
        list(games = .N),
        lapply(.SD, sum, na.rm = TRUE)
      ), by = season, .SDcols = avail_sum]

      # Average the percentage columns
      if (length(avail_pct) > 0) {
        pct_agg <- dt[, lapply(.SD, function(x) round(mean(x, na.rm = TRUE), 1)),
                       by = season, .SDcols = avail_pct]
        yearly_stats <- merge(yearly_stats, pct_agg, by = "season")
      }

      data.table::setorder(yearly_stats, season)
    } else {
      yearly_stats <- data.frame()
    }
  } else {
    yearly_stats <- data.frame()
  }

  # --- TORP season ratings ---
  torp_season <- tryCatch({
    season_vec <- if (isTRUE(seasons)) 2021:get_afl_season() else seasons
    sr <- player_season_ratings(season_val = season_vec)
    sr[sr$player_id == pid, , drop = FALSE]
  }, error = function(e) data.frame())

  # --- Current TORP rating ---
  current_torp <- tryCatch({
    cr <- calculate_torp_ratings()
    cr[cr$player_id == pid, , drop = FALSE]
  }, error = function(e) data.frame())

  out <- list(
    player_info = player_info,
    yearly_stats = yearly_stats,
    torp_season = torp_season,
    current_torp = current_torp
  )
  class(out) <- "torp_player_profile"
  out
}

#' Print a player profile
#'
#' @param x A \code{torp_player_profile} object.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns \code{x}.
#' @export
print.torp_player_profile <- function(x, ...) {
  info <- x$player_info
  cat(paste0("=== ", info$name, " (", info$team, " - ", info$position, ") ===\n\n"))

  if (nrow(x$yearly_stats) > 0) {
    cat("--- Yearly Stats ---\n")
    print(x$yearly_stats, row.names = FALSE)
    cat("\n")
  }

  if (nrow(x$torp_season) > 0) {
    cat("--- TORP Season Ratings ---\n")
    print(x$torp_season, row.names = FALSE)
    cat("\n")
  }

  if (nrow(x$current_torp) > 0) {
    cat("--- Current TORP Rating ---\n")
    print(x$current_torp, row.names = FALSE)
    cat("\n")
  }

  invisible(x)
}
