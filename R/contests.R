#' Extract Head-to-Head Contests from Chains Data
#'
#' Identifies contest pairs from raw chains data by examining adjacent rows
#' at the same field coordinates. The raw chains data contains contest
#' descriptions (Spoil, Contest Target, Tackle, etc.) that are stripped
#' during PBP cleaning.
#'
#' The primary contest signal is **matching x,y coordinates** on consecutive
#' rows — both players were at the same spot contesting.
#'
#' @param chains A data.frame or data.table of chains data (from
#'   \code{load_chains()}). If NULL, loads chains data using \code{seasons}
#'   and \code{rounds}.
#' @param type Contest type to extract: \code{"aerial"},
#'   \code{"ground_ball"}, or \code{"all"} (default).
#' @param seasons Seasons to load if \code{chains} is NULL. Passed to
#'   \code{load_chains()}.
#' @param rounds Rounds to load if \code{chains} is NULL. Passed to
#'   \code{load_chains()}.
#'
#' @return A data.table with one row per contest, containing columns:
#'   \code{match_id}, \code{season}, \code{round_number},
#'   \code{player1_id}, \code{team1_id}, \code{player1_desc},
#'   \code{player2_id}, \code{team2_id}, \code{player2_desc},
#'   \code{contest_type} (\code{"aerial"} or \code{"ground_ball"}),
#'   \code{outcome} (\code{"mark"}, \code{"intercept_mark"}, \code{"spoil"},
#'   or \code{NA} for ground ball),
#'   \code{winner} (\code{"player1"} or \code{"player2"}),
#'   \code{x}, \code{y}, \code{period}, \code{period_seconds}.
#'
#' @details
#' Aerial contest outcomes (Contest Target / Kick Inside 50 Result at same x,y):
#' \describe{
#'   \item{mark}{A player from the same team as the Contest Target marks it.
#'     The kicking team won the aerial contest (winner = player1).}
#'   \item{intercept_mark}{A player from the opposing team marks it
#'     (Contested Mark, Uncontested Mark, Mark On Lead). The defending team
#'     won (winner = player2).}
#'   \item{spoil}{A player from the opposing team spoils it. The defending
#'     team won (winner = player2).}
#' }
#'
#' Ground ball contests are consecutive ground ball actions (Hard Ball Get,
#' Loose Ball Get, crumbs) at the same x,y from opposing teams.
#'
#' @export
#' @importFrom data.table as.data.table shift setorder
#' @importFrom cli cli_abort
#'
#' @examples
#' \dontrun{
#' try({
#'   contests <- extract_contests(seasons = 2024, rounds = 1:5, type = "aerial")
#'   head(contests)
#' })
#' }
extract_contests <- function(chains = NULL, type = "all", seasons = TRUE,
                             rounds = TRUE) {
  valid_types <- c("all", "aerial", "ground_ball")
  if (!type %in% valid_types) {
    cli::cli_abort("{.arg type} must be one of {.val {valid_types}}, not {.val {type}}")
  }

  if (is.null(chains)) {
    chains <- load_chains(seasons = seasons, rounds = rounds)
  }
  dt <- data.table::as.data.table(chains)

  # Detect column naming convention (camelCase vs snake_case)
  col_map <- detect_chains_columns(dt)

  # Normalize column names to snake_case for consistent downstream processing
  rename_map <- c(
    setNames(col_map$match_id, "match_id"),
    setNames(col_map$display_order, "display_order"),
    setNames(col_map$description, "description"),
    setNames(col_map$player_id, "player_id"),
    setNames(col_map$team_id, "team_id"),
    setNames(col_map$x, "x"),
    setNames(col_map$y, "y"),
    setNames(col_map$period, "period"),
    setNames(col_map$period_seconds, "period_seconds"),
    setNames(col_map$season, "season"),
    setNames(col_map$round_number, "round_number")
  )
  needs_rename <- rename_map[names(rename_map) != rename_map]
  existing <- needs_rename[needs_rename %in% names(dt)]
  if (length(existing) > 0) {
    data.table::setnames(dt, existing, names(existing))
  }

  data.table::setorderv(dt, c("match_id", "display_order"))

  # Add adjacent-row info within each match
  dt[, `:=`(
    .next_desc      = data.table::shift(description, 1L, type = "lead"),
    .next_player_id = data.table::shift(player_id, 1L, type = "lead"),
    .next_team_id   = data.table::shift(team_id, 1L, type = "lead"),
    .next_x         = data.table::shift(x, 1L, type = "lead"),
    .next_y         = data.table::shift(y, 1L, type = "lead")
  ), by = match_id]

  # Same x,y filter for both opposing and same-team pairs
  same_xy <- dt[
    x == .next_x &
    y == .next_y &
    !is.na(.next_team_id)
  ]

  results <- list()

  # --- Aerial contests ---
  if (type %in% c("all", "aerial")) {
    aerial_rows <- same_xy[description %in% CHAINS_CONTEST_TARGET_DESCS]

    # Spoil: opposing team spoils (defender wins)
    spoils <- aerial_rows[
      .next_desc == "Spoil" & team_id != .next_team_id
    ]
    if (nrow(spoils) > 0) {
      results[["spoil"]] <- format_contest_rows(
        spoils, contest_type = "aerial", outcome = "spoil", winner = "player2"
      )
    }

    # Intercept mark: opposing team marks (defender wins)
    intercepts <- aerial_rows[
      .next_desc %in% CHAINS_MARK_WIN_DESCS & team_id != .next_team_id
    ]
    if (nrow(intercepts) > 0) {
      results[["intercept_mark"]] <- format_contest_rows(
        intercepts, contest_type = "aerial", outcome = "intercept_mark",
        winner = "player2"
      )
    }

    # Mark: same team marks (attacker's team wins)
    marks <- aerial_rows[
      .next_desc %in% CHAINS_MARK_WIN_DESCS & team_id == .next_team_id
    ]
    if (nrow(marks) > 0) {
      results[["mark"]] <- format_contest_rows(
        marks, contest_type = "aerial", outcome = "mark", winner = "player1"
      )
    }
  }

  # --- Ground ball contests ---
  if (type %in% c("all", "ground_ball")) {
    gb <- same_xy[
      description %in% CONTEST_GROUND_BALL_DESCS &
      .next_desc %in% CONTEST_GROUND_BALL_DESCS &
      team_id != .next_team_id
    ]
    if (nrow(gb) > 0) {
      results[["ground_ball"]] <- format_contest_rows(
        gb, contest_type = "ground_ball", outcome = NA_character_,
        winner = "player2"
      )
    }
  }

  # Clean up temp columns (on the original dt, not same_xy)
  temp_cols <- intersect(
    c(".next_desc", ".next_player_id", ".next_team_id", ".next_x", ".next_y"),
    names(dt)
  )
  if (length(temp_cols) > 0) dt[, (temp_cols) := NULL]

  if (length(results) == 0) {
    return(empty_contests_dt())
  }

  data.table::rbindlist(results, use.names = TRUE)
}


#' Head-to-Head Contest History Between Two Players
#'
#' Retrieves the contest history between two specific players across their
#' career matchups. Uses fuzzy name matching via \code{resolve_player()}.
#'
#' @param player1 Character string of the first player's name (partial match OK).
#' @param player2 Character string of the second player's name (partial match OK).
#' @param type Contest type: \code{"aerial"}, \code{"ground_ball"},
#'   or \code{"all"} (default).
#' @param seasons Seasons to search. Numeric vector or \code{TRUE} for all.
#' @param rounds Rounds to include. Numeric vector or \code{TRUE} for all.
#' @param chains Optional pre-loaded chains data to avoid re-downloading.
#'
#' @return An S3 object of class \code{torp_head_to_head} with elements:
#'   \describe{
#'     \item{player1}{List with \code{player_id}, \code{player_name}, \code{team}.}
#'     \item{player2}{List with \code{player_id}, \code{player_name}, \code{team}.}
#'     \item{contests}{data.table of all matchups between the two players.}
#'     \item{summary}{data.table with win/loss record per contest type.}
#'   }
#'
#' @export
#' @importFrom cli cli_abort
#'
#' @examples
#' \dontrun{
#' try({
#'   h2h <- head_to_head("Harris Andrews", "Charlie Curnow", type = "aerial")
#'   print(h2h)
#' })
#' }
head_to_head <- function(player1, player2, type = "all", seasons = TRUE,
                         rounds = TRUE, chains = NULL) {
  p1 <- resolve_player(player1, seasons = seasons)
  p2 <- resolve_player(player2, seasons = seasons)

  if (p1$player_id == p2$player_id) {
    cli::cli_abort("Both names resolve to the same player: {.val {p1$player_name}}")
  }

  contests <- extract_contests(
    chains = chains, type = type, seasons = seasons, rounds = rounds
  )

  if (nrow(contests) == 0) {
    h2h_contests <- empty_contests_dt()
  } else {
    pid1 <- p1$player_id
    pid2 <- p2$player_id

    h2h_contests <- contests[
      (player1_id == pid1 & player2_id == pid2) |
      (player1_id == pid2 & player2_id == pid1)
    ]
  }

  # Build summary
  summary_dt <- build_h2h_summary(h2h_contests, p1, p2)

  out <- list(
    player1 = p1,
    player2 = p2,
    contests = h2h_contests,
    summary = summary_dt
  )
  class(out) <- "torp_head_to_head"
  out
}


#' Print Head-to-Head Results
#'
#' @param x A \code{torp_head_to_head} object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns \code{x}.
#' @export
print.torp_head_to_head <- function(x, ...) {
  p1 <- x$player1
  p2 <- x$player2

  cat(paste0("=== ", p1$player_name, " vs ", p2$player_name, " ===\n"))
  cat(paste0("    ", p1$team, " vs ", p2$team, "\n\n"))

  if (nrow(x$contests) == 0) {
    cat("No contests found between these players.\n")
    return(invisible(x))
  }

  cat(paste0("Total contests: ", nrow(x$contests), "\n\n"))

  if (nrow(x$summary) > 0) {
    cat("--- Summary by Contest Type ---\n")
    print(x$summary, row.names = FALSE)
    cat("\n")
  }

  # Season breakdown
  season_counts <- x$contests[, .N, by = season]
  data.table::setorder(season_counts, season)
  cat("--- Contests by Season ---\n")
  print(season_counts, row.names = FALSE)
  cat("\n")

  invisible(x)
}


# Internal helpers ---------------------------------------------------------

#' Detect chains column naming convention
#'
#' Chains data uses camelCase (matchId, displayOrder, teamId) while PBP data
#' after clean_pbp() uses snake_case (match_id, display_order, team_id).
#' This detects which convention is present.
#'
#' @param dt A data.table of chains data
#' @return Named list mapping logical names to actual column names
#' @keywords internal
detect_chains_columns <- function(dt) {
  nms <- names(dt)
  is_camel <- "matchId" %in% nms

  if (is_camel) {
    list(
      match_id = "matchId",
      display_order = "displayOrder",
      description = "description",
      player_id = "playerId",
      team_id = "teamId",
      x = "x",
      y = "y",
      period = "period",
      period_seconds = "periodSeconds",
      season = "season",
      round_number = "round_number"
    )
  } else {
    list(
      match_id = "match_id",
      display_order = "display_order",
      description = "description",
      player_id = "player_id",
      team_id = "team_id",
      x = "x",
      y = "y",
      period = "period",
      period_seconds = "period_seconds",
      season = "season",
      round_number = "round_number"
    )
  }
}


#' Format contest rows into standard output schema
#'
#' Columns are already normalized to snake_case by extract_contests().
#'
#' @param rows data.table of matched contest rows (snake_case columns)
#' @param contest_type Character label for the contest type
#' @param outcome Character label for the contest outcome
#' @param winner Which player won ("player1" or "player2")
#' @return data.table in standard contest schema
#' @keywords internal
format_contest_rows <- function(rows, contest_type, outcome, winner) {
  has_season <- "season" %in% names(rows)
  has_round <- "round_number" %in% names(rows)

  rows[, .(
    match_id,
    season         = if (has_season) season else NA_integer_,
    round_number   = if (has_round) round_number else NA_integer_,
    player1_id     = player_id,
    team1_id       = team_id,
    player1_desc   = description,
    player2_id     = .next_player_id,
    team2_id       = .next_team_id,
    player2_desc   = .next_desc,
    contest_type   = contest_type,
    outcome        = outcome,
    winner         = winner,
    x, y, period, period_seconds
  )]
}


#' Build head-to-head summary table
#'
#' @param contests data.table of filtered contests
#' @param p1 Player 1 info list
#' @param p2 Player 2 info list
#' @return data.table summary
#' @keywords internal
build_h2h_summary <- function(contests, p1, p2) {
  if (nrow(contests) == 0) {
    return(data.table::data.table(
      contest_type = character(),
      outcome = character(),
      total = integer(),
      p1_wins = integer(),
      p2_wins = integer(),
      p1_win_pct = numeric()
    ))
  }

  pid1 <- p1$player_id
  pid2 <- p2$player_id

  # Determine who won from each player's perspective
  contests[, p1_won := (
    (player2_id == pid1 & winner == "player2") |
    (player1_id == pid1 & winner == "player1")
  )]

  summary_dt <- contests[, .(
    total = .N,
    p1_wins = sum(p1_won),
    p2_wins = sum(!p1_won)
  ), by = .(contest_type, outcome)]

  summary_dt[, p1_win_pct := round(100 * p1_wins / total, 1)]

  # Rename columns to include player names
  data.table::setnames(summary_dt,
    old = c("p1_wins", "p2_wins", "p1_win_pct"),
    new = c(
      paste0(p1$player_name, "_wins"),
      paste0(p2$player_name, "_wins"),
      paste0(p1$player_name, "_win_pct")
    )
  )

  # Clean up temp column
  contests[, p1_won := NULL]

  summary_dt
}


#' Create an empty contests data.table with correct schema
#' @return An empty data.table
#' @keywords internal
empty_contests_dt <- function() {
  data.table::data.table(
    match_id = character(),
    season = integer(),
    round_number = integer(),
    player1_id = character(),
    team1_id = character(),
    player1_desc = character(),
    player2_id = character(),
    team2_id = character(),
    player2_desc = character(),
    contest_type = character(),
    outcome = character(),
    winner = character(),
    x = numeric(),
    y = numeric(),
    period = integer(),
    period_seconds = numeric()
  )
}
