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
      pred_tog * AFL_TEAM_SIZE / team_tog_sum,
      AFL_TEAM_SIZE / n_players
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
#' Appends the current scrape to the season's injury history on torpdata,
#' deduping to one row per state change. Weekly-source rows are collapsed
#' on (player_norm, team, injury, estimated_return, updated) so you get
#' one row each time a player's status changes (e.g. Test -> Out). Preseason
#' rows are collapsed on (player_norm, team).
#'
#' @param injuries_df A data.frame of injuries (e.g., from [get_all_injuries()]).
#' @param season Numeric season year (e.g. 2026).
#' @return Invisible NULL. Called for side effects (upload).
#' @keywords internal
save_injury_data <- function(injuries_df, season) {
  if (is.null(injuries_df) || nrow(injuries_df) == 0) {
    cli::cli_inform("No injuries to save for {season}")
    return(invisible(NULL))
  }

  now <- Sys.time()
  injuries_df$scraped_at   <- now
  injuries_df$scraped_date <- as.Date(now)

  existing <- tryCatch(load_injury_data(season), error = function(e) NULL)

  if (!is.null(existing) && nrow(existing) > 0) {
    # Backfill scraped_at on pre-upgrade rows that only have scraped_date
    if (!"scraped_at" %in% names(existing)) {
      existing$scraped_at <- as.POSIXct(
        paste(existing$scraped_date, "00:00:00"), tz = "UTC"
      )
    }
    combined <- dplyr::bind_rows(existing, injuries_df)
  } else {
    combined <- injuries_df
  }

  combined <- dplyr::arrange(combined, scraped_at)

  weekly <- combined |>
    dplyr::filter(source == "weekly") |>
    dplyr::distinct(player_norm, team, injury, estimated_return, updated,
                    .keep_all = TRUE)

  preseason <- combined |>
    dplyr::filter(source == "preseason") |>
    dplyr::distinct(player_norm, team, .keep_all = TRUE)

  combined <- dplyr::bind_rows(weekly, preseason) |>
    dplyr::arrange(player_norm, scraped_at)

  n_prev <- if (is.null(existing)) 0L else nrow(existing)
  n_new  <- nrow(combined) - n_prev
  cli::cli_inform("Injury history: {nrow(combined)} rows ({n_new} new state change{?s} this scrape)")

  save_to_release(combined, paste0("injury_list_", season), "injury-data")
  invisible(NULL)
}


# Internal context builder: loads injury history + round-start timestamps +
# played-player lookup keys used by the listing-accuracy helpers
# (test_played_rate, tbc_played_rate, injury_return_accuracy,
# tbc_return_survival). Returns NULL on any missing dependency (pre-tracking
# history, no fixtures, no teams) so callers can short-circuit cleanly.
.injury_accuracy_context <- function(season, round = NULL) {
  inj_hist <- tryCatch(load_injury_data(season), error = function(e) NULL)
  if (is.null(inj_hist) || nrow(inj_hist) == 0) {
    cli::cli_alert_info("No injury history for {season} yet")
    return(NULL)
  }
  if (!"scraped_at" %in% names(inj_hist)) {
    cli::cli_alert_info("Injury history for {season} predates scraped_at tracking -- waiting on fresh snapshots")
    return(NULL)
  }

  fixtures <- load_fixtures(all = TRUE) |>
    dplyr::filter(season == .env$season)
  teams <- load_teams(season)
  if (nrow(teams) == 0) {
    cli::cli_alert_info("No lineup data for {season}")
    return(NULL)
  }

  round_starts <- fixtures |>
    dplyr::mutate(
      utc_dt = as.POSIXct(utc_start_time, format = "%Y-%m-%dT%H:%M", tz = "UTC")
    ) |>
    dplyr::group_by(round_number) |>
    dplyr::summarise(round_start = min(utc_dt, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(round_start < Sys.time())

  if (!is.null(round)) {
    round_starts <- dplyr::filter(round_starts, round_number %in% round)
  }
  if (nrow(round_starts) == 0) {
    cli::cli_alert_info("No completed rounds to analyse")
    return(NULL)
  }

  played_df <- teams |>
    dplyr::filter(!lineup_position %in% c("EMERG", "SUB") | is.na(lineup_position)) |>
    dplyr::mutate(player_norm = norm_name(paste(given_name, surname))) |>
    dplyr::transmute(round = as.integer(.data$round_number),
                     player_norm = .data$player_norm) |>
    dplyr::distinct()

  played_keys <- paste(played_df$round, played_df$player_norm)

  list(
    inj_hist = inj_hist,
    round_starts = round_starts,
    played_df = played_df,
    played_keys = played_keys
  )
}


# Internal: for each round with a fixture start, collect the latest
# pre-kickoff weekly listing per (player_norm, team) whose estimated_return
# matches `return_filter` (a predicate on the lowercased string). Returns a
# tibble with `round_number` (evaluation round), `scrape_round` (round the
# listing was published for -- first upcoming round after scraped_at),
# `player`, `team`, `injury`, `estimated_return`, `player_norm`,
# `scraped_at`, `played`.
.latest_listings_per_round <- function(ctx, return_filter) {
  rs_sorted <- ctx$round_starts |> dplyr::arrange(.data$round_start)

  listings <- purrr::map_dfr(seq_len(nrow(ctx$round_starts)), function(i) {
    r  <- ctx$round_starts$round_number[i]
    rs <- ctx$round_starts$round_start[i]
    ctx$inj_hist |>
      dplyr::filter(.data$source == "weekly",
                    .data$scraped_at < rs,
                    return_filter(tolower(trimws(.data$estimated_return)))) |>
      dplyr::arrange(.data$player_norm, .data$scraped_at) |>
      dplyr::group_by(.data$player_norm) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup() |>
      dplyr::mutate(round_number = r)
  })

  if (nrow(listings) == 0) return(listings)

  # scrape_round = first upcoming round whose start is after scraped_at.
  # parse_return_round() needs this (not the evaluation round), otherwise
  # a "1-2 weeks" listing keeps sliding forward each round we evaluate.
  listings$scrape_round <- vapply(listings$scraped_at, function(ts) {
    idx <- which(rs_sorted$round_start > ts)
    if (length(idx) == 0) NA_integer_ else as.integer(rs_sorted$round_number[idx[1]])
  }, integer(1))

  listings |>
    dplyr::mutate(played = paste(.data$round_number, .data$player_norm) %in% ctx$played_keys)
}


#' TEST Listing Played-Rate
#'
#' For each completed round, finds players listed as "Test" on the weekly
#' injury list at the latest scrape before the round's first match, and
#' checks whether they were named in the selected 22 (non-EMERG/SUB). Useful
#' for validating whether the model's TEST-as-TBC assumption is too harsh.
#'
#' Requires accumulated injury snapshots from [save_injury_data()] -- returns
#' an empty tibble if the release predates `scraped_at` tracking or if no
#' TEST listings have been captured yet.
#'
#' @param season Season year. Defaults to current via [get_afl_season()].
#' @param round Optional round number (or vector) to filter to. If `NULL`,
#'   analyses all rounds with injury history and lineup data available.
#' @return A tibble with one row per TEST listing: `round`, `player`,
#'   `team`, `injury`, `scraped_at`, `played` (logical).
#' @export
test_played_rate <- function(season = NULL, round = NULL) {
  if (is.null(season)) season <- get_afl_season()

  ctx <- .injury_accuracy_context(season, round)
  if (is.null(ctx)) return(tibble::tibble())

  tests <- .latest_listings_per_round(ctx, function(x) x == "test")

  if (nrow(tests) == 0) {
    cli::cli_alert_info("No TEST listings captured yet -- give it a few rounds of scrapes")
    return(tibble::tibble())
  }

  tests |>
    dplyr::transmute(round = .data$round_number, .data$player, .data$team, .data$injury,
                     .data$scraped_at, .data$played) |>
    dplyr::arrange(.data$round, .data$team, .data$player)
}


#' TBC Listing Played-Rate
#'
#' Per-round analogue of [test_played_rate()] for players listed as "TBC"
#' (estimated return unknown). For each completed round, finds players whose
#' latest pre-kickoff weekly listing says "TBC" and checks whether they were
#' named in the selected 22. The overall played fraction here directly
#' calibrates the `current_round + 3` fallback used by [parse_return_round()]
#' for TBC entries.
#'
#' Also returns a `summary` attribute with the overall and per-round played
#' rate -- useful for dashboards and for feeding back into simulation
#' defaults.
#'
#' @param season Season year. Defaults to current via [get_afl_season()].
#' @param round Optional round number (or vector) to filter to.
#' @return A tibble with one row per TBC listing: `round`, `player`, `team`,
#'   `injury`, `scraped_at`, `played` (logical). Attached `summary` attribute
#'   contains overall and per-round rates.
#' @export
tbc_played_rate <- function(season = NULL, round = NULL) {
  if (is.null(season)) season <- get_afl_season()

  ctx <- .injury_accuracy_context(season, round)
  if (is.null(ctx)) return(tibble::tibble())

  tbcs <- .latest_listings_per_round(ctx, function(x) x == "tbc")

  if (nrow(tbcs) == 0) {
    cli::cli_alert_info("No TBC listings captured yet -- give it a few rounds of scrapes")
    return(tibble::tibble())
  }

  out <- tbcs |>
    dplyr::transmute(round = .data$round_number, .data$player, .data$team, .data$injury,
                     .data$scraped_at, .data$played) |>
    dplyr::arrange(.data$round, .data$team, .data$player)

  per_round <- out |>
    dplyr::group_by(.data$round) |>
    dplyr::summarise(n = dplyr::n(),
                     played = sum(.data$played),
                     played_pct = mean(.data$played),
                     .groups = "drop")

  attr(out, "summary") <- list(
    n_listings = nrow(out),
    overall_played_pct = mean(out$played),
    per_round = per_round
  )
  out
}


#' Injury-Return Accuracy (Predicted vs Actual Rounds Out)
#'
#' For every round with injury-history coverage, take the latest pre-kickoff
#' weekly listing for each player, compute the predicted return round via
#' [parse_return_round()], and compare to whether the player actually played
#' that round. Produces a listing-level tibble plus a per-band summary
#' (`attr(x, "by_band")`) showing calibration of each `estimated_return` band
#' (e.g. "1-2 weeks", "TBC", "Test").
#'
#' Interpretation: for listings predicted to be unavailable that round
#' (`predicted_rounds_out > 0`), `played_pct` is a false-positive rate -- the
#' fraction incorrectly flagged as out. For listings predicted back
#' (`predicted_rounds_out <= 0`), it's the true-positive rate.
#'
#' @param season Season year. Defaults to current via [get_afl_season()].
#' @param round Optional round number (or vector) to filter to.
#' @return A tibble with one row per listing-round:
#'   `round`, `player`, `team`, `injury`, `estimated_return`,
#'   `predicted_return_round` (may be `Inf` = out for season),
#'   `predicted_rounds_out` (predicted - round, `Inf` for season-out),
#'   `played` (actual appearance in selected 22), `scraped_at`.
#'   Attached `by_band` attribute summarises accuracy per
#'   `estimated_return` value.
#' @export
injury_return_accuracy <- function(season = NULL, round = NULL) {
  if (is.null(season)) season <- get_afl_season()

  ctx <- .injury_accuracy_context(season, round)
  if (is.null(ctx)) return(tibble::tibble())

  # All weekly listings (any estimated_return that isn't NA/empty/none)
  listings <- .latest_listings_per_round(ctx, function(x) {
    !is.na(x) & nzchar(x) & x != "none"
  })

  if (nrow(listings) == 0) {
    cli::cli_alert_info("No weekly listings captured yet -- give it a few rounds of scrapes")
    return(tibble::tibble())
  }

  listings$predicted_return_round <- purrr::map_dbl(
    seq_len(nrow(listings)), function(i) {
      parse_return_round(listings$estimated_return[i], season,
                         current_round = listings$scrape_round[i])
    }
  )

  out <- listings |>
    dplyr::mutate(
      predicted_rounds_out = .data$predicted_return_round - .data$round_number
    ) |>
    dplyr::transmute(
      round = .data$round_number, .data$player, .data$team, .data$injury,
      .data$estimated_return, .data$predicted_return_round,
      .data$predicted_rounds_out, .data$played, .data$scraped_at
    ) |>
    dplyr::arrange(.data$round, .data$team, .data$player)

  by_band <- out |>
    dplyr::mutate(band = tolower(trimws(.data$estimated_return))) |>
    dplyr::group_by(.data$band) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_predicted_out = mean(.data$predicted_rounds_out[is.finite(.data$predicted_rounds_out)]),
      played_pct = mean(.data$played),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$n))

  attr(out, "by_band") <- by_band
  out
}


#' Empirical Return Distribution for TBC Listings
#'
#' For every weekly TBC listing in the season's injury history, measures
#' rounds-until-first-senior-game after the listing's scrape. Returns a
#' tibble of listing-level observations plus an `attr(x, "cdf")` with the
#' empirical P(returned by +N rounds) and median rounds-to-return (NA if
#' more than half are still censored).
#'
#' Players who have not yet returned are right-censored: their observed
#' rounds-out is a lower bound. The CDF uses a simple observed / total
#' estimator -- it under-estimates the true return probability at higher
#' offsets (because some censored players will eventually return), so treat
#' it as a conservative floor.
#'
#' Used to audit the `parse_return_round("TBC")` default of
#' `current_round + 3` -- if the median actual return is 1-2 rounds, the
#' simulation is treating TBC as more severe than the data supports.
#'
#' @param season Season year. Defaults to current via [get_afl_season()].
#' @return A tibble with one row per TBC listing episode:
#'   `player`, `team`, `injury`, `first_tbc_round` (round at scrape),
#'   `return_round` (first lineup appearance, NA if censored),
#'   `rounds_out` (observed, with censoring flag), `censored` (logical).
#'   Attached `cdf` attribute: offset, returned, total, cumulative P(return).
#' @export
tbc_return_survival <- function(season = NULL) {
  if (is.null(season)) season <- get_afl_season()

  ctx <- .injury_accuracy_context(season)
  if (is.null(ctx)) return(tibble::tibble())

  tbc_hist <- ctx$inj_hist |>
    dplyr::filter(.data$source == "weekly",
                  tolower(trimws(.data$estimated_return)) == "tbc") |>
    dplyr::arrange(.data$player_norm, .data$team, .data$scraped_at)

  if (nrow(tbc_hist) == 0) {
    cli::cli_alert_info("No TBC listings captured yet -- give it a few rounds of scrapes")
    return(tibble::tibble())
  }

  # For each (player_norm, team) keep the first TBC scrape -- that's the
  # start of the TBC episode. Later scrapes with the same state were already
  # collapsed by save_injury_data(), but we guard anyway.
  tbc_first <- tbc_hist |>
    dplyr::group_by(.data$player_norm, .data$team) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup()

  # Tag each episode with its scrape round (first round whose kickoff is
  # after scraped_at; if scrape precedes round 1, use round 1).
  rs <- ctx$round_starts |> dplyr::arrange(.data$round_start)
  tbc_first$first_tbc_round <- vapply(tbc_first$scraped_at, function(ts) {
    idx <- which(rs$round_start > ts)
    if (length(idx) == 0) NA_integer_ else as.integer(rs$round_number[idx[1]])
  }, integer(1))

  completed_rounds <- rs$round_number
  last_completed <- if (length(completed_rounds) > 0) max(completed_rounds) else NA_integer_

  played_df <- ctx$played_df

  # For each episode, find first played round >= first_tbc_round
  out <- tbc_first |>
    dplyr::mutate(
      return_round = purrr::map2_int(.data$player_norm, .data$first_tbc_round, function(pn, r0) {
        if (is.na(r0)) return(NA_integer_)
        hits <- played_df$round[played_df$player_norm == pn & played_df$round >= r0]
        if (length(hits) == 0) NA_integer_ else min(hits)
      }),
      censored = is.na(.data$return_round),
      rounds_out = dplyr::if_else(
        .data$censored,
        as.integer(last_completed) - .data$first_tbc_round,
        .data$return_round - .data$first_tbc_round
      )
    ) |>
    dplyr::transmute(
      .data$player, .data$team, .data$injury, .data$first_tbc_round,
      .data$return_round, .data$rounds_out, .data$censored
    ) |>
    dplyr::filter(!is.na(.data$first_tbc_round))

  # Empirical CDF: at offset k, fraction of all episodes whose observed
  # rounds_out <= k AND NOT censored. Conservative (under-counts at high k).
  max_off <- suppressWarnings(max(out$rounds_out, na.rm = TRUE))
  if (!is.finite(max_off)) max_off <- 0L
  offsets <- seq.int(0L, max(1L, as.integer(max_off)))
  total <- nrow(out)
  returned_by <- vapply(offsets, function(k) {
    sum(!out$censored & out$rounds_out <= k)
  }, integer(1))
  p_returned <- if (total == 0) {
    rep(NA_real_, length(offsets))
  } else {
    returned_by / total
  }
  cdf <- tibble::tibble(
    offset = offsets,
    returned = returned_by,
    total = total,
    p_returned = p_returned
  )

  # Median rounds-to-return (NA if fewer than half have returned)
  returned_out <- out$rounds_out[!out$censored]
  median_out <- if (length(returned_out) >= max(1L, total / 2)) {
    stats::median(returned_out)
  } else {
    NA_real_
  }

  attr(out, "cdf") <- cdf
  attr(out, "median_rounds_out") <- median_out
  attr(out, "n_returned") <- length(returned_out)
  attr(out, "n_censored") <- sum(out$censored)
  out
}
