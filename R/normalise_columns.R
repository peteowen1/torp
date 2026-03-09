#' Normalise Player Stats Column Names
#'
#' Maps both old (2021-2025) and new (2026+ v2 API) player stats schemas to
#' clean canonical names. Handles:
#' \itemize{
#'   \item v2 prefix stripping: \code{stats_goals} → \code{goals}
#'   \item Player column renames: \code{player_player_player_player_id} → \code{player_id}
#'   \item Match column renames: \code{provider_id} → \code{match_id}
#'   \item Round column: \code{round_round_number} → \code{round_number}
#'   \item Position column: \code{player_player_position} → \code{position}
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

  # --- 2. Rename player/match/round columns ---
  # Mapping: old_name -> canonical_name
  # Multiple old names can map to the same canonical name (checked in priority order)
  rename_map <- list(
    # Player ID (v2: player_player_id, old: player_player_player_player_id)
    player_id = c("player_player_player_player_id", "player_player_id"),
    # Given name (v2: player_player_player_player_name_given_name, old: player_player_player_given_name)
    given_name = c("player_player_player_given_name", "player_player_player_player_name_given_name"),
    # Surname (v2: player_player_player_player_name_surname, old: player_player_player_surname)
    surname = c("player_player_player_surname", "player_player_player_player_name_surname"),
    # Position
    position = c("player_player_position"),
    # Jumper number
    jumper_number = c("player_jumper_number", "player_player_jumper_number"),
    # Match ID
    match_id = c("provider_id"),
    # Round number
    round_number = c("round_round_number")
  )

  remapped <- character()
  for (canonical in names(rename_map)) {
    if (canonical %in% nms) next
    candidates <- rename_map[[canonical]]
    found <- intersect(candidates, nms)
    if (length(found) > 0) {
      data.table::setnames(dt, found[1], canonical)
      nms[nms == found[1]] <- canonical
      remapped <- c(remapped, paste0(found[1], " -> ", canonical))
    }
  }

  if (length(remapped) > 0) {
    cli::cli_inform(
      "Player stats normalisation: remapped {length(remapped)} column{?s}: {paste(remapped, collapse = ', ')}"
    )
  }

  # --- 3. Add `season` column if missing ---
  if (!"season" %in% nms && "utc_start_time" %in% nms) {
    dt[, season := as.integer(format(as.Date(utc_start_time), "%Y"))]
  }

  invisible(dt)
}
