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
#' @param dt A data.table to modify.
#' @param col_map Named character vector: variant_name -> canonical_name.
#' @param verbose Logical. If TRUE, emits a message listing renames.
#' @param label Optional label for the log message (e.g. "PBP", "Player Stats").
#' @return Invisible NULL (modifies dt by reference).
#' @keywords internal
.normalise_columns <- function(dt, col_map, verbose = TRUE, label = NULL) {
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


# ============================================================================
# Player Stats column map
# ============================================================================

#' @keywords internal
PLAYER_STATS_COL_MAP <- c(
  # --- v2 API prefix stripping (handled separately in .normalise_player_stats_columns) ---
  # These are applied AFTER the bulk stats_ prefix strip

  # --- Player/match/round ID renames ---
  "player_player_player_player_id" = "player_id",
  "player_player_id"               = "player_id",
  "player_player_player_given_name" = "given_name",
  "player_player_player_player_name_given_name" = "given_name",
  "player_player_player_surname"   = "surname",
  "player_player_player_player_name_surname" = "surname",
  "player_player_position"         = "position",
  "player_jumper_number"           = "jumper_number",
  "player_player_jumper_number"    = "jumper_number",
  "provider_id"                    = "match_id",
  "round_round_number"             = "round_number",

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
  "venue.timezone"          = "venue_timezone"
)


# ============================================================================
# Teams / Lineups column map
# ============================================================================

#' @keywords internal
TEAMS_COL_MAP <- c(
  "player.playerId"   = "player_id",
  "teamId"            = "team_id",
  "teamName"          = "team_name",
  "teamType"          = "team_type",
  "providerId"        = "match_id",
  "round.roundNumber" = "round_number",
  "venue.name"        = "venue_name"
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

  # --- 4. Add `season` column if missing ---
  nms <- names(dt)
  if (!"season" %in% nms && "utc_start_time" %in% nms) {
    dt[, season := as.integer(format(as.Date(utc_start_time), "%Y"))]
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
  invisible(df)
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
  invisible(df)
}
