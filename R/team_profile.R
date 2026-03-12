# Team Profile & Team Skills
# ===========================
# Team-level equivalents of player_profile.R and player_skills.R functions.
# Provides team profiles, team skill aggregation, and team skill profiles
# with league-wide percentile ranks.


# ============================================================================
# Team name resolution
# ============================================================================

#' Resolve a team name to its canonical form
#'
#' Internal helper that takes a team name string (partial match OK) and
#' resolves it to a canonical AFL team name using [AFL_TEAM_ALIASES].
#'
#' @param team_name A character string of the team's name (full, short, or abbreviation).
#'
#' @return A list with elements \code{name}, \code{full}, \code{abbr}.
#'
#' @importFrom cli cli_abort
#' @keywords internal
resolve_team <- function(team_name) {
  if (!is.character(team_name) || length(team_name) != 1 || nchar(trimws(team_name)) == 0) {
    cli::cli_abort("{.arg team_name} must be a non-empty character string")
  }

  # Try exact alias match first (handles abbreviations, full names, etc.)
  canonical <- torp_replace_teams(team_name)

  # Check if it resolved to a known team
  if (canonical %in% AFL_TEAMS$name) {
    idx <- match(canonical, AFL_TEAMS$name)
    return(list(
      name = AFL_TEAMS$name[idx],
      full = AFL_TEAMS$full[idx],
      abbr = AFL_TEAMS$abbr[idx]
    ))
  }

  # Fuzzy match: case-insensitive substring search
  search_lower <- tolower(trimws(team_name))
  names_lower <- tolower(AFL_TEAMS$name)
  full_lower <- tolower(AFL_TEAMS$full)

  hits <- which(grepl(search_lower, names_lower, fixed = TRUE) |
                grepl(search_lower, full_lower, fixed = TRUE))

  if (length(hits) == 0) {
    cli::cli_abort(c(
      "No team found matching {.val {team_name}}.",
      "i" = "Use one of: {.val {AFL_TEAMS$name}}"
    ))
  }
  if (length(hits) > 1) {
    cli::cli_abort(c(
      "Multiple teams match {.val {team_name}}: {.val {AFL_TEAMS$name[hits]}}.",
      "i" = "Please be more specific."
    ))
  }

  list(
    name = AFL_TEAMS$name[hits],
    full = AFL_TEAMS$full[hits],
    abbr = AFL_TEAMS$abbr[hits]
  )
}


# ============================================================================
# Team profile
# ============================================================================

#' Get a Team Profile
#'
#' Combines team TORP rating, season record, and top players into a single
#' object. Accepts partial name matches, abbreviations, or full names
#' (e.g. "Sydney", "SYD", "Sydney Swans").
#'
#' @param team_name A character string of the team's name (partial OK).
#' @param seasons Seasons to include for the record. Numeric vector of years,
#'   or \code{TRUE} for all available. Default is the current season.
#' @param top_n Number of top players to include (by current TORP). Default 10.
#'
#' @return A list of class \code{torp_team_profile} with elements:
#' \describe{
#'   \item{team_info}{1-row data.frame with name, full name, abbreviation.}
#'   \item{team_rating}{Current TORP team rating (1-row data.frame, or empty).}
#'   \item{season_record}{Per-season W/L/D record and percentage.}
#'   \item{top_players}{Top players by current TORP rating.}
#' }
#'
#' @export
#'
#' @importFrom data.table as.data.table
#' @importFrom cli cli_warn
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   team_profile("Sydney")
#'   team_profile("SYD", seasons = 2022:2025)
#' })
#' }
team_profile <- function(team_name, seasons = get_afl_season(), top_n = 10) {
  tm <- resolve_team(team_name)

  team_info <- data.frame(
    name = tm$name,
    full = tm$full,
    abbr = tm$abbr,
    stringsAsFactors = FALSE
  )

  # --- Team TORP rating (latest snapshot only) ---
  team_rating <- tryCatch({
    tr <- data.table::as.data.table(load_team_ratings())
    tr <- tr[team == tm$name]
    if (nrow(tr) > 0) {
      latest <- tr[season == max(season)]
      latest <- latest[round == max(round)]
      latest
    } else {
      data.frame()
    }
  }, error = function(e) data.frame())

  # --- Season record from results ---
  season_record <- tryCatch({
    results <- data.table::as.data.table(load_results(seasons))
    .compute_season_record(results, tm$name)
  }, error = function(e) data.frame())

  # --- Top players by TORP (latest snapshot only) ---
  top_players <- tryCatch({
    ratings <- data.table::as.data.table(load_torp_ratings())
    # Filter to latest snapshot
    ratings <- ratings[season == max(season, na.rm = TRUE)]
    ratings <- ratings[round == max(round, na.rm = TRUE)]
    team_players <- ratings[team == tm$name & !is.na(torp)]
    if (nrow(team_players) > 0) {
      data.table::setorder(team_players, -torp)
      head(team_players, top_n)
    } else {
      data.frame()
    }
  }, error = function(e) data.frame())

  out <- list(
    team_info = team_info,
    team_rating = team_rating,
    season_record = season_record,
    top_players = top_players
  )
  class(out) <- "torp_team_profile"
  out
}


#' Compute season W/L/D record for a team
#'
#' @param results A data.table of match results.
#' @param team_name Canonical team name.
#' @return A data.table with season, wins, losses, draws, points_for, points_against, percentage.
#' @keywords internal
.compute_season_record <- function(results, team_name) {
  # Results should have home_team_name, away_team_name, home_score, away_score
  # Standardise team names in results if needed
  if ("home_team_name" %in% names(results)) {
    home_col <- "home_team_name"
    away_col <- "away_team_name"
  } else if ("home_team" %in% names(results)) {
    home_col <- "home_team"
    away_col <- "away_team"
  } else {
    return(data.frame())
  }

  # Score columns
  if ("home_score" %in% names(results)) {
    home_score_col <- "home_score"
    away_score_col <- "away_score"
  } else if ("home_points" %in% names(results)) {
    home_score_col <- "home_points"
    away_score_col <- "away_points"
  } else {
    return(data.frame())
  }

  # Filter to team's matches and compute results
  home_games <- results[get(home_col) == team_name & !is.na(get(home_score_col))]
  away_games <- results[get(away_col) == team_name & !is.na(get(away_score_col))]

  if (nrow(home_games) + nrow(away_games) == 0) return(data.frame())

  # Home games
  if (nrow(home_games) > 0) {
    home_games[, `:=`(
      pf = get(home_score_col),
      pa = get(away_score_col),
      win = as.integer(get(home_score_col) > get(away_score_col)),
      loss = as.integer(get(home_score_col) < get(away_score_col)),
      draw = as.integer(get(home_score_col) == get(away_score_col))
    )]
  }

  # Away games
  if (nrow(away_games) > 0) {
    away_games[, `:=`(
      pf = get(away_score_col),
      pa = get(home_score_col),
      win = as.integer(get(away_score_col) > get(home_score_col)),
      loss = as.integer(get(away_score_col) < get(home_score_col)),
      draw = as.integer(get(away_score_col) == get(home_score_col))
    )]
  }

  # Combine
  keep_cols <- c("season", "pf", "pa", "win", "loss", "draw")
  all_games <- data.table::rbindlist(list(
    home_games[, ..keep_cols],
    away_games[, ..keep_cols]
  ))

  record <- all_games[, .(
    games = .N,
    wins = sum(win),
    losses = sum(loss),
    draws = sum(draw),
    points_for = sum(pf, na.rm = TRUE),
    points_against = sum(pa, na.rm = TRUE)
  ), by = season]

  record[, percentage := round(points_for / points_against * 100, 1)]
  data.table::setorder(record, season)
  record
}


#' Print a team profile
#'
#' @param x A \code{torp_team_profile} object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns \code{x}.
#' @export
print.torp_team_profile <- function(x, ...) {
  info <- x$team_info
  cat(paste0("=== ", info$full, " (", info$abbr, ") ===\n\n"))

  if (nrow(x$team_rating) > 0) {
    cat("--- Team TORP Rating ---\n")
    print(x$team_rating, row.names = FALSE)
    cat("\n")
  }

  if (nrow(x$season_record) > 0) {
    cat("--- Season Record ---\n")
    print(x$season_record, row.names = FALSE)
    cat("\n")
  }

  if (nrow(x$top_players) > 0) {
    cat("--- Top Players (TORP) ---\n")
    # Show a compact view
    display_cols <- intersect(
      c("player_name", "torp", "torp_recv", "torp_disp", "torp_spoil", "torp_hitout"),
      names(x$top_players)
    )
    if (length(display_cols) > 0) {
      print(x$top_players[, ..display_cols], row.names = FALSE)
    } else {
      print(x$top_players, row.names = FALSE)
    }
    cat("\n")
  }

  invisible(x)
}


# ============================================================================
# Team skill aggregation
# ============================================================================

#' Get Team Skills
#'
#' Aggregates player-level skill estimates to team level. For each team,
#' sums and averages the Bayesian skill estimates of its current players.
#'
#' @param team_name Optional team name (partial OK). If NULL (default), returns
#'   all teams.
#' @param top_n Maximum number of players per team to include (ordered by
#'   total skill). Default 22 (a full team).
#'
#' @return A data.table with one row per team, containing \code{team},
#'   \code{n_players}, and for each stat: \code{{stat}_sum} and
#'   \code{{stat}_mean}.
#'
#' @seealso [get_player_skills()], [aggregate_team_skills()],
#'   [team_skill_profile()]
#'
#' @importFrom data.table as.data.table setorder
#' @importFrom cli cli_abort cli_warn
#' @export
#'
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   get_team_skills()
#'   get_team_skills("Sydney")
#' })
#' }
get_team_skills <- function(team_name = NULL, top_n = 22) {
  # Load current player skills (current season only — faster than loading all history)
  skills <- data.table::as.data.table(get_player_skills(seasons = get_afl_season(), current = TRUE))

  if (nrow(skills) == 0) {
    cli::cli_warn("No player skills data available.")
    return(data.table::data.table())
  }

  # Get team assignments from TORP ratings (latest snapshot only)
  ratings <- tryCatch(
    data.table::as.data.table(load_torp_ratings()),
    error = function(e) {
      cli::cli_warn("Could not load TORP ratings for team assignments: {conditionMessage(e)}")
      NULL
    }
  )

  if (is.null(ratings) || nrow(ratings) == 0) {
    cli::cli_abort("Cannot determine team assignments without TORP ratings data.")
  }

  # Filter to latest snapshot for current team assignments
  ratings <- ratings[season == max(season, na.rm = TRUE)]
  ratings <- ratings[round == max(round, na.rm = TRUE)]
  team_lookup <- unique(ratings[!is.na(team), .(player_id, team)])

  skills_with_team <- merge(skills, team_lookup, by = "player_id", all.x = FALSE)

  if (nrow(skills_with_team) == 0) {
    cli::cli_warn("No players matched between skills and ratings data.")
    return(data.table::data.table())
  }

  # Try to get latest lineups to filter to actual selected players
  lineup_data <- .get_latest_lineup_ids()
  lineup_ids <- if (!is.null(lineup_data)) lineup_data$players else NULL
  match_info <- if (!is.null(lineup_data)) lineup_data$match_info else NULL

  # Find skill columns
  stat_defs <- skill_stat_definitions()
  skill_cols <- paste0(stat_defs$stat_name, "_skill")
  skill_cols <- intersect(skill_cols, names(skills_with_team))

  if (length(skill_cols) == 0) {
    cli::cli_warn("No skill columns found in data.")
    return(data.table::data.table())
  }

  # Compute total skill for ordering (used as fallback when no lineup)
  skills_with_team[, .total_skill := rowSums(.SD, na.rm = TRUE), .SDcols = skill_cols]

  # For each team: use lineup if available, else top_n by skill
  all_teams <- unique(skills_with_team$team)
  result_list <- vector("list", length(all_teams))

  for (i in seq_along(all_teams)) {
    tm_name <- all_teams[i]
    tm_skills <- skills_with_team[team == tm_name]

    # Check if we have lineup data for this team
    if (!is.null(lineup_ids)) {
      tm_lineup <- lineup_ids[team == tm_name]
      if (nrow(tm_lineup) > 0) {
        # Filter to players in the lineup
        tm_skills <- tm_skills[player_id %in% tm_lineup$player_id]
      }
    }

    # Fallback: take top_n by total skill
    if (nrow(tm_skills) > top_n) {
      data.table::setorder(tm_skills, -.total_skill)
      tm_skills <- head(tm_skills, top_n)
    }

    if (nrow(tm_skills) == 0) next

    out <- list(team = tm_name, n_players = nrow(tm_skills))
    for (sc in skill_cols) {
      stat_nm <- sub("_skill$", "", sc)
      vals <- tm_skills[[sc]]
      out[[paste0(stat_nm, "_sum")]] <- sum(vals, na.rm = TRUE)
      out[[paste0(stat_nm, "_mean")]] <- mean(vals, na.rm = TRUE)
    }
    result_list[[i]] <- data.table::as.data.table(out)
  }

  result <- data.table::rbindlist(result_list[!vapply(result_list, is.null, logical(1))])

  # Filter to specific team if requested
  if (!is.null(team_name)) {
    tm <- resolve_team(team_name)
    result <- result[team == tm$name]
    if (nrow(result) == 0) {
      cli::cli_abort("No skill data found for {.val {tm$name}}")
    }
  }

  data.table::setorder(result, team)

  # Attach lineup match info as attribute for downstream display
  if (!is.null(match_info)) {
    attr(result, "match_info") <- match_info
  }

  result
}


#' Get latest lineup player IDs per team
#'
#' Loads lineup data for the current season and extracts each team's most
#' recent match lineup. Maps abbreviation-style team names to canonical names.
#'
#' @return A list with elements:
#' \describe{
#'   \item{players}{data.table with \code{player_id} and \code{team} (canonical name)}
#'   \item{match_info}{data.table with \code{team} and \code{match_id} for each team's latest match}
#' }
#' Returns NULL if lineup data is unavailable.
#' @keywords internal
.get_latest_lineup_ids <- function() {
  lineups <- tryCatch(
    data.table::as.data.table(load_teams()),
    error = function(e) NULL
  )

  if (is.null(lineups) || nrow(lineups) == 0) return(NULL)

  # Map abbreviation team_name to canonical name
  abbr_to_name <- stats::setNames(AFL_TEAMS$name, AFL_TEAMS$abbr)
  lineups[, team := abbr_to_name[team_name]]
  lineups <- lineups[!is.na(team)]

  if (nrow(lineups) == 0) return(NULL)

  # For each team, keep only the latest match's lineup
  lineups[, match_id := as.character(match_id)]
  latest <- lineups[, .(last_match = max(match_id)), by = team]
  lineups <- merge(lineups, latest, by = "team")
  lineups <- lineups[match_id == last_match]

  list(
    players = unique(lineups[, .(player_id, team)]),
    match_info = unique(latest)
  )
}


# ============================================================================
# Team skill profile
# ============================================================================

#' Get a Team Skill Profile
#'
#' Aggregates player skills for a team and computes league-wide percentile
#' ranks across all 18 teams. The team equivalent of [player_skill_profile()].
#'
#' @param team_name A character string of the team's name (partial OK).
#' @param top_n Maximum number of players per team to include. Default 22.
#'
#' @return A list of class \code{torp_team_skill_profile} with elements:
#' \describe{
#'   \item{team_info}{1-row data.frame with team name, full name, abbreviation.}
#'   \item{skills}{Data.frame of skill estimates with league percentile ranks.}
#'   \item{n_players}{Number of players included in the aggregation.}
#' }
#'
#' @seealso [get_team_skills()], [player_skill_profile()]
#'
#' @importFrom data.table as.data.table
#' @export
#'
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   team_skill_profile("Sydney")
#'   team_skill_profile("Geelong")
#' })
#' }
team_skill_profile <- function(team_name, top_n = 22) {
  tm <- resolve_team(team_name)

  # Get all team skills
  all_teams <- get_team_skills(top_n = top_n)

  if (nrow(all_teams) == 0) {
    cli::cli_abort("No team skill data available.")
  }

  target <- all_teams[team == tm$name]
  if (nrow(target) == 0) {
    cli::cli_abort("No skill data found for {.val {tm$name}}")
  }

  # Identify stat columns (sum and mean variants)
  stat_defs <- skill_stat_definitions()
  sum_cols <- paste0(stat_defs$stat_name, "_sum")
  mean_cols <- paste0(stat_defs$stat_name, "_mean")
  sum_cols <- intersect(sum_cols, names(all_teams))
  mean_cols <- intersect(mean_cols, names(all_teams))
  stat_names <- sub("_sum$", "", sum_cols)

  # Build profile using mean (per-player average) for comparability
  profile <- data.frame(stat = stat_names, stringsAsFactors = FALSE)

  # Team's mean skill per stat
  profile$team_mean <- as.numeric(target[, ..mean_cols])

  # Team's total skill per stat
  profile$team_sum <- as.numeric(target[, ..sum_cols])

  # League average (mean across all teams)
  profile$league_avg <- colMeans(all_teams[, ..mean_cols], na.rm = TRUE)


  # Percentile rank among all teams (by mean), flipped for negative stats
  hib_lookup <- stats::setNames(stat_defs$higher_is_better, stat_defs$stat_name)

  profile$league_pct <- vapply(seq_along(mean_cols), function(i) {
    col <- mean_cols[i]
    vals <- all_teams[[col]]
    team_val <- target[[col]]
    if (is.na(team_val) || all(is.na(vals))) return(NA_real_)
    pct <- sum(vals <= team_val, na.rm = TRUE) / sum(!is.na(vals)) * 100
    # Flip for negative stats (lower is better)
    sn <- stat_names[i]
    if (!is.na(hib_lookup[sn]) && !hib_lookup[sn]) pct <- 100 - pct
    pct
  }, numeric(1))

  # Add category from stat definitions
  profile <- merge(
    profile,
    stat_defs[, c("stat_name", "category")],
    by.x = "stat", by.y = "stat_name", all.x = TRUE
  )

  # Reorder columns
  col_order <- c("category", "stat", "team_mean", "team_sum", "league_avg", "league_pct")
  col_order <- intersect(col_order, names(profile))
  profile <- profile[, col_order]

  # Resolve lineup match info for display
  lineup_info <- .resolve_lineup_info(all_teams, tm$name)

  out <- list(
    team_info = data.frame(
      name = tm$name,
      full = tm$full,
      abbr = tm$abbr,
      stringsAsFactors = FALSE
    ),
    skills = profile[order(-profile$league_pct), ],
    n_players = as.integer(target$n_players),
    lineup_info = lineup_info
  )
  class(out) <- "torp_team_skill_profile"
  out
}


#' Convert UTC match time to local venue time
#'
#' Parses a UTC start time string and converts it to local time based on
#' the venue timezone from fixture data.
#'
#' @param utc_start_time Character string in ISO 8601 format
#'   (e.g. \code{"2026-03-05T08:30:00.000+0000"}).
#' @param venue_timezone Olson timezone string (e.g. \code{"Australia/Sydney"}).
#'   If NULL or NA, defaults to \code{"Australia/Melbourne"}.
#'
#' @return A formatted local time string (e.g. \code{"2026-03-05 19:30 AEDT"}),
#'   or NA if the input cannot be parsed.
#'
#' @importFrom lubridate ymd_hms
#' @export
#'
#' @examples
#' match_local_time("2026-03-05T08:30:00.000+0000", "Australia/Sydney")
#' match_local_time("2026-03-05T08:30:00.000+0000", "Australia/Perth")
match_local_time <- function(utc_start_time, venue_timezone = NULL) {
  if (is.na(utc_start_time) || is.null(utc_start_time)) return(NA_character_)

  if (is.null(venue_timezone) || is.na(venue_timezone)) {
    venue_timezone <- "Australia/Melbourne"
  }

  utc_time <- tryCatch(
    lubridate::ymd_hms(utc_start_time, tz = "UTC"),
    warning = function(w) NA,
    error = function(e) NA
  )

  if (is.na(utc_time)) return(NA_character_)

  format(utc_time, tz = venue_timezone, format = "%Y-%m-%d %H:%M %Z")
}


#' Resolve lineup match info for a team
#'
#' Looks up the match_id from the lineup attribute and joins with fixture
#' data to get the opponent, round, and local datetime.
#'
#' @param team_skills The result of \code{get_team_skills()} (carries match_info attribute).
#' @param team_name Canonical team name.
#' @return A list with \code{match_id}, \code{opponent}, \code{round}, \code{datetime},
#'   or NULL if unavailable.
#' @keywords internal
.resolve_lineup_info <- function(team_skills, team_name) {
  match_info <- attr(team_skills, "match_info")
  if (is.null(match_info)) return(NULL)

  tm_match <- match_info[team == team_name]
  if (nrow(tm_match) == 0) return(NULL)

  mid <- tm_match$last_match[1]

  # Try to get fixture details
  fixture <- tryCatch({
    fix <- data.table::as.data.table(load_fixtures(all = TRUE))
    fix[match_id == mid | as.character(match_id) == mid]
  }, error = function(e) NULL)

  if (is.null(fixture) || nrow(fixture) == 0) {
    return(list(match_id = mid, opponent = NA, round = NA, datetime = NA))
  }

  row <- fixture[1]

  # Determine opponent (standardise fixture team names to canonical for comparison)
  home_raw <- if ("home_team_name" %in% names(row)) as.character(row$home_team_name) else NA
  away_raw <- if ("away_team_name" %in% names(row)) as.character(row$away_team_name) else NA
  home <- torp_replace_teams(home_raw)
  away <- torp_replace_teams(away_raw)
  opponent <- if (!is.na(home) && home == team_name) away else home

  # Round
  rnd <- if ("round_number" %in% names(row)) row$round_number else NA

  # Local datetime using venue timezone
  tz_val <- if ("venue_timezone" %in% names(row)) as.character(row$venue_timezone) else NULL
  utc_val <- if ("utc_start_time" %in% names(row)) as.character(row$utc_start_time) else NA
  dt_val <- match_local_time(utc_val, tz_val)

  list(match_id = mid, opponent = opponent, round = rnd, datetime = dt_val)
}


#' Print a team skill profile
#'
#' @param x A \code{torp_team_skill_profile} object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns \code{x}.
#' @export
print.torp_team_skill_profile <- function(x, ...) {
  info <- x$team_info
  li <- x$lineup_info

  # Header
  cat(paste0(
    "=== Team Skill Profile: ", info$full,
    " (", info$abbr, ") ===\n",
    "Players: ", x$n_players
  ))

  # Lineup source
  if (!is.null(li)) {
    parts <- character(0)
    if (!is.na(li$round)) parts <- c(parts, paste0("R", li$round))
    if (!is.na(li$opponent)) parts <- c(parts, paste0("vs ", li$opponent))
    if (!is.na(li$datetime)) parts <- c(parts, li$datetime)
    if (length(parts) > 0) {
      cat(paste0("  |  Lineup: ", paste(parts, collapse = ", ")))
    }
  }
  cat("\n\n")

  sk <- x$skills

  # Format display
  display_cols <- intersect(
    c("category", "stat", "team_mean", "team_sum", "league_avg", "league_pct"),
    names(sk)
  )
  display <- sk[, display_cols]

  fmt_sig4 <- function(v) {
    vapply(v, function(val) {
      if (is.na(val)) return("")
      format(signif(val, 4), scientific = FALSE, drop0trailing = TRUE, trim = TRUE)
    }, character(1), USE.NAMES = FALSE)
  }
  fmt_1dp <- function(v) {
    ifelse(is.na(v), "", formatC(round(v, 1), format = "f", digits = 1))
  }

  for (col in intersect(c("team_mean", "team_sum", "league_avg"), names(display))) {
    display[[col]] <- fmt_sig4(display[[col]])
  }
  if ("league_pct" %in% names(display)) {
    display[["league_pct"]] <- fmt_1dp(display[["league_pct"]])
  }

  print(display, row.names = FALSE)
  invisible(x)
}
