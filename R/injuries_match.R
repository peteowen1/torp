# AFL Injury Data: Combine & Match
# ================================
# Combine weekly + preseason sources into a single deduplicated injury list,
# match injuries to player rating data frames by normalised player name,
# and parse free-text return strings (e.g. "1-2 weeks") into round numbers.


#' Get All Injuries (Weekly + Preseason)
#'
#' Combines the live AFL weekly injury list with the curated preseason
#' injury list. Weekly entries take precedence when a player appears in both
#' sources (deduplicated by normalized name). Preseason entries for players
#' who have already played a senior game this season are dropped — otherwise
#' stale "TBC" entries linger indefinitely and exclude healthy players from
#' team ratings (e.g. a preseason concussion listing for someone who has
#' played every round since).
#'
#' @param season Numeric season year (e.g. 2026).
#' @param scrape Logical; if TRUE (default), scrapes the live AFL injury list.
#'   Set to FALSE for pre-season use or testing when the AFL page is empty.
#' @param drop_played_preseason Logical; if TRUE (default), drops preseason
#'   entries for players who appear in this season's `load_player_stats()`.
#'   Set to FALSE to inspect the raw merged list.
#' @return A data.frame with columns: `player`, `team`, `injury`,
#'   `estimated_return`, `updated`, `player_norm`, `source`. The `source`
#'   column is `"weekly"` or `"preseason"`. `updated` is the per-team date
#'   from the AFL injury page (Date class; NA for preseason entries).
#' @export
get_all_injuries <- function(season, scrape = TRUE, drop_played_preseason = TRUE) {
  preseason <- load_preseason_injuries(season)
  if (nrow(preseason) > 0) {
    preseason$source <- "preseason"

    # Filter preseason entries for players who've actually played this season.
    # Without this, "TBC" / "Mid-season" entries never age out, and healthy
    # players stay excluded from team ratings with phantom "return" boosts.
    if (drop_played_preseason) {
      played_norms <- tryCatch({
        ps <- load_player_stats(season)
        if (nrow(ps) > 0 && "player_name" %in% names(ps)) {
          unique(norm_name(ps$player_name))
        } else {
          character(0)
        }
      }, error = function(e) {
        cli::cli_alert_warning(
          "Preseason filter skipped - could not load player_stats: {conditionMessage(e)}"
        )
        character(0)
      })

      if (length(played_norms) > 0) {
        stale <- preseason$player_norm %in% played_norms
        if (any(stale)) {
          cli::cli_alert_info(
            "Dropped {sum(stale)} stale preseason injur{?y/ies} for player{?s} already playing in {season}: {paste(preseason$player[stale], collapse = ', ')}"
          )
          preseason <- preseason[!stale, , drop = FALSE]
        }
      }
    }
  }

  if (scrape) {
    weekly <- scrape_injuries()
    if (nrow(weekly) > 0) {
      weekly$source <- "weekly"
      if (!"team" %in% names(weekly)) weekly$team <- NA_character_
      if (!"updated" %in% names(weekly)) weekly$updated <- as.Date(NA)
    }
  } else {
    weekly <- data.frame(
      player = character(), injury = character(),
      estimated_return = character(), player_norm = character(),
      source = character(), team = character(),
      updated = as.Date(character()),
      stringsAsFactors = FALSE
    )
  }

  if (nrow(weekly) == 0 && nrow(preseason) == 0) {
    return(data.frame(
      player = character(), injury = character(),
      estimated_return = character(), player_norm = character(),
      source = character(), team = character(),
      updated = as.Date(character()),
      stringsAsFactors = FALSE
    ))
  }

  # Ensure both have the same columns before binding
  all_cols <- c("player", "team", "injury", "estimated_return", "updated", "player_norm", "source")
  for (col in all_cols) {
    fill_val <- if (col == "updated") as.Date(NA) else NA_character_
    if (!col %in% names(weekly)) weekly[[col]] <- rep(fill_val, nrow(weekly))
    if (!col %in% names(preseason)) preseason[[col]] <- rep(fill_val, nrow(preseason))
  }

  combined <- rbind(
    weekly[, all_cols, drop = FALSE],
    preseason[, all_cols, drop = FALSE]
  )

  # Normalise team names to canonical full form BEFORE dedup. Weekly scrape
  # uses "Carlton Blues" / "Melbourne Demons" etc; preseason CSV historically
  # used short forms ("Carlton", "Melbourne"). Without this step dedup on
  # (player_norm, team) misses any overlap on a non-verbatim-matching team
  # and leaves duplicate rows in the combined output.
  if (!all(is.na(combined$team))) {
    combined$team <- torp_replace_teams(combined$team)
  }

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
  # Use (player_norm, team) as join key when both sides have team to handle
  # same-name players (e.g., Max King at different teams). Normalise both
  # sides' team values first so "Carlton" and "Carlton Blues" (or any
  # short/full variant) collapse to the same canonical name before the merge.
  has_team <- "team" %in% names(ratings_df) && "team" %in% names(injuries_df)
  if (has_team) {
    inj_join <- injuries_df[, c("player_norm", "team", "injury", "estimated_return"), drop = FALSE]
    inj_join$team <- torp_replace_teams(inj_join$team)
    ratings_df$team <- torp_replace_teams(ratings_df$team)
    inj_join <- inj_join[!duplicated(paste(inj_join$player_norm, inj_join$team)), ]
    merged <- merge(ratings_df, inj_join, by = c("player_norm", "team"), all.x = TRUE)
  } else {
    inj_join <- injuries_df[, c("player_norm", "injury", "estimated_return"), drop = FALSE]
    inj_join <- inj_join[!duplicated(inj_join$player_norm), ]
    merged <- merge(ratings_df, inj_join, by = "player_norm", all.x = TRUE)
  }

  # Remove the temporary join column
  merged$player_norm <- NULL
  merged
}


#' Parse Estimated Return into Round Number
#'
#' Vectorized function that converts free-text estimated return strings from the
#' AFL injury list into numeric round numbers for use in simulation scheduling.
#'
#' @param estimated_return Character vector of return estimates (e.g., "Round 14",
#'   "TBC", "2027", "Mid-season").
#' @param season Numeric season year (e.g. 2026). Used to detect next-year returns.
#' @param current_round Integer current round number. Used to compute TBC fallback.
#' @return Numeric vector. `Inf` = out for season. `NA` = not injured / already
#'   available. Finite values = round the player is expected to return.
#' @keywords internal
parse_return_round <- function(estimated_return, season, current_round = 1L) {
  current_round <- as.integer(current_round)
  n <- length(estimated_return)
  result <- rep(NA_real_, n)

  for (i in seq_len(n)) {
    val <- estimated_return[i]

    # NA / empty / "None" -> NA (not injured)
    if (is.na(val) || val == "" || tolower(val) == "none") {
      result[i] <- NA_real_
      next
    }

    val_lower <- tolower(trimws(val))

    # "Round N-M" range (conservative: use upper bound)
    m <- regmatches(val_lower, regexec("^round\\s+(\\d+)\\s*-\\s*(\\d+)$", val_lower))[[1]]
    if (length(m) == 3) {
      result[i] <- as.numeric(m[3])
      next
    }

    # "Round N"
    m <- regmatches(val_lower, regexec("^round\\s+(\\d+)$", val_lower))[[1]]
    if (length(m) == 2) {
      result[i] <- as.numeric(m[2])
      next
    }

    # Pure year (e.g., "2027") -- out for season if future year
    m <- regmatches(val_lower, regexec("^(\\d{4})$", val_lower))[[1]]
    if (length(m) == 2) {
      yr <- as.integer(m[2])
      result[i] <- if (yr > season) Inf else NA_real_
      next
    }

    # "Second half of YYYY"
    if (grepl("second half", val_lower)) {
      result[i] <- SIM_INJURY_SECOND_HALF
      next
    }

    # "Mid-season" / "Mid-to-late season"
    if (grepl("mid", val_lower) && grepl("season", val_lower)) {
      result[i] <- SIM_INJURY_SEASON_MID
      next
    }

    # "Late season"
    if (grepl("late", val_lower) && grepl("season", val_lower)) {
      result[i] <- SIM_INJURY_SEASON_LATE
      next
    }

    # "Season" (alone) -- out for the season
    if (val_lower == "season") {
      result[i] <- Inf
      next
    }

    # "N-M weeks" range (conservative: use upper bound, ~1 round per week)
    m <- regmatches(val_lower, regexec("(\\d+)\\s*-\\s*(\\d+)\\s*weeks?", val_lower))[[1]]
    if (length(m) == 3) {
      result[i] <- current_round + as.integer(ceiling(as.numeric(m[3])))
      next
    }

    # "N-plus weeks" / "N+ weeks" (conservative: treat as N * 1.5 weeks)
    m <- regmatches(val_lower, regexec("(\\d+)\\s*[-]?plus\\s*weeks?", val_lower))[[1]]
    if (length(m) == 2) {
      weeks <- as.numeric(m[2])
      result[i] <- current_round + as.integer(ceiling(weeks * 1.5))
      next
    }

    # "N weeks" single value
    m <- regmatches(val_lower, regexec("^(\\d+)\\s*weeks?$", val_lower))[[1]]
    if (length(m) == 2) {
      result[i] <- current_round + as.integer(ceiling(as.numeric(m[2])))
      next
    }

    # "N-M months" range (conservative: upper bound, ~4 weeks per month)
    m <- regmatches(val_lower, regexec("(\\d+)\\s*-\\s*(\\d+)\\s*months?", val_lower))[[1]]
    if (length(m) == 3) {
      weeks <- as.numeric(m[3]) * 4
      result[i] <- current_round + as.integer(ceiling(weeks))
      next
    }

    # "N months" single value (~4 weeks per month)
    m <- regmatches(val_lower, regexec("^(\\d+)\\s*months?$", val_lower))[[1]]
    if (length(m) == 2) {
      weeks <- as.numeric(m[2]) * 4
      result[i] <- current_round + as.integer(ceiling(weeks))
      next
    }

    # "TBC" / "Indefinite" / "Assess" / "Individualised program" / unknown
    if (val_lower %in% c("tbc", "indefinite", "test", "unknown", "assess",
                          "individualised program")) {
      result[i] <- current_round + SIM_INJURY_TBC_BUFFER
      next
    }

    # Fallback: treat unrecognised strings as TBC
    cli::cli_warn("Unrecognised estimated return: {.val {val}} -- treating as TBC (~{SIM_INJURY_TBC_BUFFER} rounds)")
    result[i] <- current_round + SIM_INJURY_TBC_BUFFER
  }

  as.numeric(result)
}
