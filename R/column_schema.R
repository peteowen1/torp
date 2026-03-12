# Column Schema: Canonical Column Names
# =======================================
# Single source of truth for all column name mappings across the torp ecosystem.
# Each COL_MAP is a named character vector: variant_name -> canonical_name.
# .normalise_columns() applies a map to any data.table by reference.


# ============================================================================
# Generic normaliser
# ============================================================================

#' Normalise column names using a mapping
#'
#' Renames columns in a data.table by reference using a named character vector
#' where names are variant (old) names and values are canonical (new) names.
#' Only renames when the variant exists and the canonical does not.
#'
#' @param dt A data.frame, tibble, or data.table to modify (renames by reference via `data.table::setnames`).
#' @param col_map Named character vector: variant_name -> canonical_name.
#' @param verbose Logical. If TRUE, emits a message listing renames.
#' @param label Optional label for the log message (e.g. "PBP", "Player Stats").
#' @return Invisible NULL (modifies dt by reference).
#' @keywords internal
.normalise_columns <- function(dt, col_map, verbose = TRUE, label = NULL) {
  if (is.null(dt) || length(dt) == 0L) return(invisible(NULL))

  nms <- names(dt)
  remapped <- character()

  for (from in names(col_map)) {
    to <- col_map[[from]]
    if (!to %in% nms && from %in% nms) {
      data.table::setnames(dt, from, to)
      nms[nms == from] <- to
      remapped <- c(remapped, paste0(from, " -> ", to))
    }
  }

  if (verbose && length(remapped) > 0) {
    lbl <- if (!is.null(label)) paste0(label, " normalisation: ") else "Column normalisation: "
    cli::cli_inform("{lbl}remapped {length(remapped)} column{?s}: {paste(remapped, collapse = ', ')}")
  }

  invisible(NULL)
}


#' Bulk-convert remaining non-snake_case column names
#'
#' Applies a generic camelCase/dot.notation to snake_case conversion on any
#' columns that are not already snake_case. Run AFTER explicit column maps
#' to catch API columns not covered by manual mappings.
#'
#' @param dt A data.frame, tibble, or data.table to modify.
#' @param verbose Logical. If TRUE, emits a message listing renames.
#' @param label Optional label for the log message.
#' @return Invisible NULL (modifies names by reference for data.table, or in place for data.frame).
#' @keywords internal
.bulk_snake_case <- function(dt, verbose = TRUE, label = NULL) {
  if (is.null(dt) || length(dt) == 0L) return(invisible(NULL))

  nms <- names(dt)

  # Detect non-snake_case columns (contain uppercase, dots, or spaces)
  needs_fix <- grepl("[A-Z. ]", nms)
  if (!any(needs_fix)) return(invisible(NULL))

  old_names <- nms[needs_fix]
  new_names <- .to_snake_case(old_names)

  # Avoid collisions with existing columns
  safe <- !new_names %in% nms[!needs_fix] & new_names != old_names
  if (!any(safe)) return(invisible(NULL))

  # setnames works by reference on both data.table and data.frame
  data.table::setnames(dt, old_names[safe], new_names[safe])

  if (verbose && sum(safe) > 0) {
    lbl <- if (!is.null(label)) paste0(label, " snake_case: ") else "Snake_case: "
    renamed <- paste0(old_names[safe], " -> ", new_names[safe])
    cli::cli_inform("{lbl}converted {sum(safe)} column{?s}: {paste(renamed, collapse = ', ')}")
  }

  invisible(NULL)
}


#' Convert a character vector to snake_case
#'
#' Handles camelCase, dot.notation, and mixed patterns.
#'
#' @param x Character vector of names.
#' @return Character vector in snake_case.
#' @keywords internal
.to_snake_case <- function(x) {
  # camelCase boundaries: insert _ before uppercase preceded by lowercase/digit
  x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x)
  # Replace dots, spaces with underscores
  x <- gsub("[. ]+", "_", x)
  x <- tolower(x)
  # Remove non-alphanumeric except underscore
  x <- gsub("[^a-z0-9_]", "_", x)
  # Collapse multiple underscores
  x <- gsub("_+", "_", x)
  # Trim leading/trailing underscores
  x <- gsub("^_|_$", "", x)
  x
}


# ============================================================================
# Player Stats column map
# ============================================================================

#' @keywords internal
PLAYER_STATS_COL_MAP <- c(
  # --- v2 API prefix stripping (handled separately in .normalise_player_stats_columns) ---
  # These are applied AFTER the bulk stats_ prefix strip

  # --- Player/match/round ID renames ---
  # Multiple nesting depths exist across API versions; map them all
  "player_player_player_player_id"             = "player_id",
  "player_player_id"                           = "player_id",
  "player_player_player_given_name"            = "given_name",
  "player_player_player_player_name_given_name" = "given_name",
  "player_player_name_given_name"              = "given_name",
  "player_player_player_surname"               = "surname",
  "player_player_player_player_name_surname"   = "surname",
  "player_player_name_surname"                 = "surname",
  "player_player_position"                     = "position",
  "player_jumper_number"                       = "jumper_number",
  "player_player_jumper_number"                = "jumper_number",
  "player_player_player_player_jumper_number"  = "jumper_number",
  "player_player_player_captain"               = "captain",
  "player_captain"                             = "captain",
  "stats_last_updated"                           = "last_updated",
  "provider_id"                                = "match_id",
  "round_round_number"                         = "round_number",

  # --- extended_stats_ prefix stripping ---
  "extended_stats_spoils"                     = "spoils",
  "extended_stats_pressure_acts"              = "pressure_acts",
  "extended_stats_def_half_pressure_acts"     = "def_half_pressure_acts",
  "extended_stats_hitouts_to_advantage"       = "hitouts_to_advantage",
  "extended_stats_ruck_contests"              = "ruck_contests",
  "extended_stats_ground_ball_gets"           = "ground_ball_gets",
  "extended_stats_effective_disposals"        = "effective_disposals",
  "extended_stats_effective_kicks"            = "effective_kicks",
  "extended_stats_kick_efficiency"            = "kick_efficiency",
  "extended_stats_intercept_marks"            = "intercept_marks",
  "extended_stats_f50ground_ball_gets"        = "f50_ground_ball_gets",
  "extended_stats_score_launches"             = "score_launches",
  "extended_stats_marks_on_lead"              = "marks_on_lead",
  "extended_stats_kickins"                    = "kickins",
  "extended_stats_centre_bounce_attendances"  = "centre_bounce_attendances",
  "extended_stats_contest_def_one_on_ones"    = "contest_def_one_on_ones",
  "extended_stats_contest_off_one_on_ones"    = "contest_off_one_on_ones",
  "extended_stats_contest_off_wins"           = "contest_off_wins",
  "extended_stats_contest_def_losses"         = "contest_def_losses",

  # --- Clearances ---
  "clearances_total_clearances"    = "clearances",
  "clearances_centre_clearances"   = "centre_clearances",
  "clearances_stoppage_clearances" = "stoppage_clearances"
)


# ============================================================================
# PBP column map
# ============================================================================

#' @keywords internal
PBP_COL_MAP <- c(
  # --- Score columns ---
  # v2 API: home.score.totalScore -> home_score_total_score -> home_score
  "home_score_total_score"       = "home_score",
  "away_score_total_score"       = "away_score",
  # CFS schema: homeTeamScore.totalScore -> home_team_score_total_score -> home_score
  "home_team_score_total_score"  = "home_score",
  "away_team_score_total_score"  = "away_score",


  # --- Team names ---
  # v2 API: home.team.name -> home_team_name (already canonical)
  # CFS schema: homeTeam.teamName -> home_team_team_name -> home_team_name
  "home_team_team_name"          = "home_team_name",
  "away_team_team_name"          = "away_team_name",

  # --- Team abbreviations ---
  # v2 API: home.team.abbreviation -> home_team_abbreviation -> home_team_abbr
  "home_team_abbreviation"       = "home_team_abbr",
  "away_team_abbreviation"       = "away_team_abbr",
  # CFS schema: homeTeam.teamAbbr -> home_team_team_abbr -> home_team_abbr
  "home_team_team_abbr"          = "home_team_abbr",
  "away_team_team_abbr"          = "away_team_abbr",

  # --- Team IDs ---
  "home_team_provider_id"        = "home_team_id",
  "away_team_provider_id"        = "away_team_id",

  # --- Round number ---
  "round_round_number"           = "round_number"
)


# ============================================================================
# Fixture column map
# ============================================================================

#' @keywords internal
FIXTURE_COL_MAP <- c(
  # --- Match ID ---
  "providerId"              = "match_id",
  "match.matchId"           = "match_id",    # CFS results schema
  "provider_id"             = "match_id",    # janitor-cleaned variant

  # --- Season ---
  "compSeason.year"         = "season",
  "comp_season_year"        = "season",      # janitor-cleaned variant

  # --- Round ---
  "round.roundNumber"       = "round_number",
  "round_round_number"      = "round_number", # janitor-cleaned variant

  # --- Team names ---
  "home.team.name"          = "home_team_name",
  "away.team.name"          = "away_team_name",

  # --- Team IDs (providerId strings like "CD_T10") ---
  "home.team.providerId"    = "home_team_id",
  "away.team.providerId"    = "away_team_id",
  "home_team_provider_id"   = "home_team_id", # janitor-cleaned variant
  "away_team_provider_id"   = "away_team_id", # janitor-cleaned variant

  # --- Scores ---
  "home.score.totalScore"   = "home_score",
  "away.score.totalScore"   = "away_score",
  "home.score.goals"        = "home_goals",
  "away.score.goals"        = "away_goals",

  "home.score.behinds"      = "home_behinds",
  "away.score.behinds"      = "away_behinds",

  # CFS results schema score columns
  "homeTeamScore.matchScore.totalScore" = "home_score",
  "homeTeamScore.matchScore.goals"      = "home_goals",
  "homeTeamScore.matchScore.behinds"    = "home_behinds",
  "awayTeamScore.matchScore.totalScore" = "away_score",
  "awayTeamScore.matchScore.goals"      = "away_goals",
  "awayTeamScore.matchScore.behinds"    = "away_behinds",

  # --- Time ---
  "utcStartTime"            = "utc_start_time",
  "match.utcStartTime"      = "utc_start_time", # CFS results schema

  # --- Venue ---
  "venue.name"              = "venue_name",
  "venue.timezone"          = "venue_timezone",

  # --- Numeric team IDs (distinct from providerId-based team IDs) ---
  "home.team.id"            = "home_team_api_id",
  "away.team.id"            = "away_team_api_id"
)


# ============================================================================
# Teams / Lineups column map
# ============================================================================

#' @keywords internal
TEAMS_COL_MAP <- c(
  "player.playerId"              = "player_id",
  "teamId"                       = "team_id",
  "teamName"                     = "team_name",
  "teamType"                     = "team_type",
  "providerId"                   = "match_id",
  "round.roundNumber"            = "round_number",
  "venue.name"                   = "venue_name",
  "player.captain"               = "captain",
  "player.playerJumperNumber"    = "jumper_number",
  "player.playerName.givenName"  = "given_name",
  "player.playerName.surname"    = "surname"
)


# ============================================================================
# Chains column map
# ============================================================================

#' @keywords internal
CHAINS_COL_MAP <- c(
  "matchId"        = "match_id",
  "displayOrder"   = "display_order",
  "playerId"       = "player_id",
  "teamId"         = "team_id",
  "periodSeconds"  = "period_seconds",
  "finalState"     = "final_state",
  "initialState"   = "initial_state",
  "roundNumber"    = "round_number"
)


# ============================================================================
# Player Game Data column map (output of create_player_game_data)
# ============================================================================

#' @keywords internal
PLAYER_GAME_COL_MAP <- c(
  # Abbreviated column names → canonical (for old parquets)
  "plyr_nm"         = "player_name",
  "tm"              = "team",
  "opp"             = "opponent",
  "pos"             = "listed_position",
  "tot_p"           = "total_credits",
  "tot_p_adj"       = "total_credits_adj",
  "recv_pts"        = "recv_credits",
  "disp_pts"        = "disp_credits",
  "spoil_pts"       = "spoil_credits",
  "hitout_pts"      = "hitout_credits",
  "recv_pts_adj"    = "recv_credits_adj",
  "disp_pts_adj"    = "disp_credits_adj",
  "spoil_pts_adj"   = "spoil_credits_adj",
  "hitout_pts_adj"  = "hitout_credits_adj",
  "recvs"           = "receptions",
  "disp"            = "disposals_pbp"
)


# ============================================================================
# Player Details column map
# ============================================================================

#' @keywords internal
PLAYER_DETAILS_COL_MAP <- c(
  "providerId"          = "player_id",
  "firstName"           = "first_name",
  "surname"             = "surname",
  "heightInCm"          = "height_cm",
  "weightInKg"          = "weight_kg",
  "dateOfBirth"         = "date_of_birth",
  "team.name"           = "team",
  "team.providerId"     = "team_provider_id",
  "gamesPlayed"         = "games_played"
)


# ============================================================================
# Predictions column map
# ============================================================================

#' @keywords internal
PREDICTIONS_COL_MAP <- c(
  "providerId"          = "match_id"
)


# ============================================================================
# XG column map
# ============================================================================

#' @keywords internal
XG_COL_MAP <- c(
  "home_sG"  = "home_scored_goals",
  "home_sB"  = "home_scored_behinds",
  "away_sG"  = "away_scored_goals",
  "away_sB"  = "away_scored_behinds"
)


# ============================================================================
# Normalise Player Stats Columns
# ============================================================================

#' Normalise Player Stats Column Names
#'
#' Maps both old (2021-2025) and new (2026+ v2 API) player stats schemas to
#' clean canonical names. Handles:
#' \itemize{
#'   \item v2 prefix stripping: \code{stats_goals} -> \code{goals}
#'   \item Player column renames: \code{player_player_player_player_id} -> \code{player_id}
#'   \item Match column renames: \code{provider_id} -> \code{match_id}
#'   \item Round column: \code{round_round_number} -> \code{round_number}
#'   \item Position column: \code{player_player_position} -> \code{position}
#'   \item Extended stats prefix: \code{extended_stats_spoils} -> \code{spoils}
#'   \item Clearance columns: \code{clearances_total_clearances} -> \code{clearances}
#'   \item Missing \code{season} column: derived from \code{utc_start_time}
#' }
#'
#' @param dt A data.table or data.frame of player stats.
#' @return The input (coerced to data.table) with normalised column names, modified by reference.
#' @keywords internal
.normalise_player_stats_columns <- function(dt) {
  if (!data.table::is.data.table(dt)) {
    dt <- data.table::as.data.table(dt)
  }

  nms <- names(dt)

  # --- 1. Strip v2 `stats_` prefix from stat columns ---
  # e.g. stats_goals -> goals, stats_extended_stats_spoils -> extended_stats_spoils,
  #      stats_clearances_total_clearances -> clearances_total_clearances
  stats_cols <- grep("^stats_", nms, value = TRUE)
  if (length(stats_cols) > 0) {
    new_names <- sub("^stats_", "", stats_cols)
    # Avoid collisions: only rename if the target name doesn't already exist
    safe <- !new_names %in% nms
    if (any(safe)) {
      data.table::setnames(dt, stats_cols[safe], new_names[safe])
      nms <- names(dt)
    }
  }

  # --- 2. Apply the centralised column map ---
  .normalise_columns(dt, PLAYER_STATS_COL_MAP, verbose = TRUE, label = "Player stats")

  # --- 3. Catch any remaining extended_stats_ columns not in the explicit map ---
  nms <- names(dt)
  ext_cols <- grep("^extended_stats_", nms, value = TRUE)
  if (length(ext_cols) > 0) {
    new_ext <- sub("^extended_stats_", "", ext_cols)
    safe <- !new_ext %in% nms
    if (any(safe)) {
      data.table::setnames(dt, ext_cols[safe], new_ext[safe])
    }
  }

  # --- 4. Bulk snake_case for any remaining camelCase columns ---
  .bulk_snake_case(dt, verbose = TRUE, label = "Player stats")

  # --- 5. Add `player_name` from given_name + surname if missing ---
  nms <- names(dt)
  if (!"player_name" %in% nms && all(c("given_name", "surname") %in% nms)) {
    dt[, player_name := paste(given_name, surname)]
  }

  # --- 6. Add `season` column if missing ---
  nms <- names(dt)
  if (!"season" %in% nms && "utc_start_time" %in% nms) {
    dt[, season := as.integer(format(as.Date(utc_start_time), "%Y"))]
  }

  # --- 7. Drop leftover columns that map to an already-existing canonical name ---
  # e.g. player_player_id when player_id already exists from a deeper variant
  nms <- names(dt)
  drop_cols <- character()
  for (from in names(PLAYER_STATS_COL_MAP)) {
    to <- PLAYER_STATS_COL_MAP[[from]]
    if (from %in% nms && to %in% nms && from != to) {
      drop_cols <- c(drop_cols, from)
    }
  }
  if (length(drop_cols) > 0) {
    dt[, (drop_cols) := NULL]
  }

  invisible(dt)
}


# ============================================================================
# Normalise Fixture Columns
# ============================================================================

#' Normalise Fixture Column Names
#'
#' Maps dot-notation columns from the AFL API (`jsonlite::fromJSON(flatten=TRUE)`)
#' and CFS results schema to canonical snake_case names. Works on both new API
#' output and old parquet files.
#'
#' @param df A data.frame, tibble, or data.table of fixture data.
#' @return The input with normalised column names, modified by reference.
#' @keywords internal
.normalise_fixture_columns <- function(df) {
  .normalise_columns(df, FIXTURE_COL_MAP, verbose = TRUE, label = "Fixture")
  .bulk_snake_case(df, verbose = TRUE, label = "Fixture")
  invisible(df)
}


# ============================================================================
# Normalise Teams / Lineups Columns
# ============================================================================

#' Normalise Teams/Lineups Column Names
#'
#' Maps camelCase columns from roster data to canonical snake_case names.
#'
#' @param df A data.frame, tibble, or data.table of teams/lineups data.
#' @return The input with normalised column names, modified by reference.
#' @keywords internal
.normalise_teams_columns <- function(df) {
  .normalise_columns(df, TEAMS_COL_MAP, verbose = TRUE, label = "Teams")
  .bulk_snake_case(df, verbose = TRUE, label = "Teams")

  # Derive round_number from match_id when absent (AFL API data lacks it)
  if (!"round_number" %in% names(df) && "match_id" %in% names(df)) {
    df[["round_number"]] <- as.integer(substr(df[["match_id"]], 12L, 13L))
  }

  df
}


# ============================================================================
# Normalise Chains Columns
# ============================================================================

#' Normalise Chains Column Names
#'
#' Maps camelCase columns from chains data to canonical snake_case names.
#'
#' @param df A data.frame, tibble, or data.table of chains data.
#' @return The input with normalised column names, modified by reference.
#' @keywords internal
.normalise_chains_columns <- function(df) {
  .normalise_columns(df, CHAINS_COL_MAP, verbose = TRUE, label = "Chains")
  .bulk_snake_case(df, verbose = TRUE, label = "Chains")
  invisible(df)
}


# ============================================================================
# Normalise Player Details Columns
# ============================================================================

#' Normalise Player Details Column Names
#'
#' Maps camelCase columns from squad/player details data to canonical snake_case.
#'
#' @param df A data.frame, tibble, or data.table of player details data.
#' @return The input with normalised column names, modified by reference.
#' @keywords internal
.normalise_player_details_columns <- function(df) {
  .normalise_columns(df, PLAYER_DETAILS_COL_MAP, verbose = TRUE, label = "Player details")
  .bulk_snake_case(df, verbose = TRUE, label = "Player details")
  invisible(df)
}


# ============================================================================
# Normalise Predictions Columns
# ============================================================================

#' Normalise Predictions Column Names
#'
#' Maps old column names in predictions data to canonical snake_case.
#'
#' @param df A data.frame, tibble, or data.table of predictions data.
#' @return The input with normalised column names, modified by reference.
#' @keywords internal
.normalise_predictions_columns <- function(df) {
  .normalise_columns(df, PREDICTIONS_COL_MAP, verbose = TRUE, label = "Predictions")
  .bulk_snake_case(df, verbose = TRUE, label = "Predictions")
  invisible(df)
}
