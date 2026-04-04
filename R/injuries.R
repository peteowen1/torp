#' AFL Injury Data Functions
#'
#' Centralized functions for scraping, loading, and matching AFL injury data.
#' Used by both the predictions pipeline and season simulation system.
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

      # Last row contains "Updated: March 17, 2026" — extract and remove it
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


#' Get All Injuries (Weekly + Preseason)
#'
#' Combines the live AFL weekly injury list with the curated preseason
#' injury list. Weekly entries take precedence when a player appears in both
#' sources (deduplicated by normalized name).
#'
#' @param season Numeric season year (e.g. 2026).
#' @param scrape Logical; if TRUE (default), scrapes the live AFL injury list.
#'   Set to FALSE for pre-season use or testing when the AFL page is empty.
#' @return A data.frame with columns: `player`, `team`, `injury`,
#'   `estimated_return`, `updated`, `player_norm`, `source`. The `source`
#'   column is `"weekly"` or `"preseason"`. `updated` is the per-team date
#'   from the AFL injury page (Date class; NA for preseason entries).
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

  # same-name players (e.g., Max King at different teams)
  has_team <- "team" %in% names(ratings_df) && "team" %in% names(injuries_df)
  if (has_team) {
    inj_join <- injuries_df[, c("player_norm", "team", "injury", "estimated_return"), drop = FALSE]
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


#' Build Injury Return Schedule for Simulation
#'
#' Takes an injury data frame (with `return_round` already parsed) and
#' player-level TORP ratings, and computes per-team TORP boosts at each
#' round when injured players return.
#'
#' @param injuries_df A data.frame with at least `player_norm` and
#'   `return_round` columns. Typically from [get_all_injuries()] after
#'   calling [parse_return_round()].
#' @param player_ratings_dt A data.table of player ratings with `player_name`,
#'   `team`, `torp`, and optionally `pred_tog` columns.
#' @return A data.table with columns `team`, `torp_boost`, `return_round`.
#'   One row per (team, return_round) combination.
#' @export
build_injury_schedule <- function(injuries_df, player_ratings_dt) {
  if (is.null(injuries_df) || nrow(injuries_df) == 0) {
    return(data.table::data.table(
      team = character(), torp_boost = numeric(), return_round = numeric()
    ))
  }

  inj_dt <- data.table::as.data.table(injuries_df)
  pr_dt <- data.table::as.data.table(player_ratings_dt)

  # Only players who will actually return this season (finite return_round)
  inj_dt <- inj_dt[is.finite(return_round) & !is.na(return_round)]
  if (nrow(inj_dt) == 0) {
    return(data.table::data.table(
      team = character(), torp_boost = numeric(), return_round = numeric()
    ))
  }

  # Normalise player and team names for joining
  if (!"player_norm" %in% names(inj_dt)) {
    inj_dt[, player_norm := norm_name(player)]
  }
  pr_dt[, player_norm := norm_name(player_name)]
  if ("team" %in% names(inj_dt)) inj_dt[, team := torp_replace_teams(team)]
  pr_dt[, team := torp_replace_teams(team)]

  # Resolve the rating column name (torp or epr depending on data source)
  torp_col <- if ("torp" %in% names(pr_dt)) "torp" else if ("epr" %in% names(pr_dt)) "epr" else NULL
  if (is.null(torp_col)) {
    cli::cli_warn("Player ratings have no {.field torp} or {.field epr} column")
    return(data.table::data.table(
      team = character(), torp_boost = numeric(), return_round = numeric()
    ))
  }
  if (torp_col != "torp") data.table::setnames(pr_dt, torp_col, "torp")

  # Join injured players to their ratings
  # Use (player_norm, team) when available to distinguish same-name players
  pr_cols <- c("player_norm", "team", "torp")
  if ("pred_tog" %in% names(pr_dt)) pr_cols <- c(pr_cols, "pred_tog")
  inj_merge_cols <- c("player_norm", "player", "return_round")
  inj_merge_cols <- intersect(inj_merge_cols, names(inj_dt))

  has_team <- "team" %in% names(inj_dt) && !all(is.na(inj_dt$team))
  if (has_team) {
    inj_merge_cols <- union(inj_merge_cols, "team")
    join_by <- c("player_norm", "team")
  } else {
    join_by <- "player_norm"
  }
  matched <- merge(inj_dt[, ..inj_merge_cols],
                   pr_dt[, ..pr_cols],
                   by = join_by, all.x = TRUE)

  # Drop unmatched (players not in ratings)
  n_unmatched <- sum(is.na(matched$torp))
  if (n_unmatched > 0) {
    unmatched_names <- if ("player" %in% names(matched)) {
      matched[is.na(torp), player]
    } else {
      matched[is.na(torp), player_norm]
    }
    cli::cli_warn("Dropped {n_unmatched} injured player{?s} not found in ratings: {paste(utils::head(unmatched_names, 5), collapse = ', ')}")
  }
  matched <- matched[!is.na(torp)]

  if (nrow(matched) == 0) {
    return(data.table::data.table(
      team = character(), torp_boost = numeric(), return_round = numeric()
    ))
  }

  # Compute TOG-weighted TORP contribution (same logic as prepare_sim_data)
  # Team names already normalised via torp_replace_teams() before the join

  # Get team-level TOG sums from full player ratings for proper weighting
  team_tog <- pr_dt[, .(team_tog_sum = sum(pred_tog, na.rm = TRUE),
                         n_players = .N), by = .(team)]

  matched <- merge(matched, team_tog, by = "team", all.x = TRUE)

  if ("pred_tog" %in% names(matched) && !all(is.na(matched$pred_tog))) {
    matched[, tog_wt := data.table::fifelse(
      team_tog_sum > 0 & !is.na(pred_tog),
      pred_tog * 18 / team_tog_sum,
      18 / n_players
    )]
    matched[, player_boost := torp * tog_wt]
  } else {
    matched[, player_boost := torp]
  }

  # Apply the same discount that was used when removing them
  matched[, player_boost := player_boost * INJURY_KNOWN_DISCOUNT]

  # Aggregate by (team, return_round)
  schedule <- matched[, .(torp_boost = sum(player_boost, na.rm = TRUE)),
                       by = .(team, return_round)]

  schedule[]
}


#' Save Injury Data to GitHub Release
#'
#' Saves a timestamped snapshot of injury data to the torpdata repository
#' as a parquet file for historical tracking.
#'
#' @param injuries_df A data.frame of injuries (e.g., from [get_all_injuries()]).
#' @param season Numeric season year (e.g. 2026).
#' @return Invisible NULL. Called for side effects (upload).
#' @keywords internal
save_injury_data <- function(injuries_df, season) {
  injuries_df$scraped_date <- Sys.Date()
  save_to_release(injuries_df, paste0("injury_list_", season), "injury-data")
  invisible(NULL)
}
