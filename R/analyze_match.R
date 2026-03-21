#' Get Player Game Ratings (Live)
#'
#' Scrapes chain data for a match (or full round) and produces per-player
#' EPV ratings in the same format as [player_game_ratings()]. Designed to
#' be run immediately after a game finishes, using live API data rather
#' than stored releases.
#'
#' @param match Input: either a match ID string (e.g. `"CD_M20260140201"`)
#'   or a pre-scraped chains data.frame from [get_match_chains()].
#'   If `NULL` (default), uses `season` and `round` to fetch chains.
#' @param season Numeric season year (default: current season via
#'   [get_afl_season()]). Only used when `match` is `NULL`.
#' @param round Numeric round number. Only used when `match` is `NULL`.
#'
#' @return A tibble with the same columns as [player_game_ratings()]:
#'   identifiers, TOG-weighted centered EPV components, and position-adjusted
#'   EPV per-80 metrics.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # From match ID (scrapes everything automatically)
#' result <- get_player_game_ratings("CD_M20260140201")
#'
#' # From season and round (all matches in the round)
#' result <- get_player_game_ratings(round = 2)
#' result <- get_player_game_ratings(season = 2025, round = 14)
#'
#' # From pre-scraped chains
#' chains <- get_match_chains(2026, 2)
#' result <- get_player_game_ratings(chains)
#' }
get_player_game_ratings <- function(match = NULL,
                                    season = get_afl_season(),
                                    round = NULL) {
  # --- Resolve input: match ID, season+round, or pre-scraped chains ---
  if (is.null(match)) {
    if (is.null(round)) {
      cli::cli_abort("Must provide either {.arg match} or {.arg round}.")
    }
    cli::cli_inform("Fetching chains for {.val {season}} round {.val {round}}...")
    chains <- data.table::as.data.table(get_match_chains(season, round))
    match_ids <- unique(chains$match_id)
  } else if (is.character(match) && length(match) == 1 && grepl("^CD_M", match)) {
    match_ids <- match
    chains <- get_match_chains(match)
  } else if (is.data.frame(match) && nrow(match) > 0) {
    chains <- data.table::as.data.table(match)
    match_ids <- unique(chains$match_id)
  } else {
    cli::cli_abort("{.arg match} must be a match ID string, a chains data.frame, or {.code NULL} (with {.arg round} specified).")
  }

  # --- Step 1: Run EPV pipeline ---
  cli::cli_inform("Running EPV pipeline...")
  pbp <- chains |>
    clean_pbp() |>
    clean_model_data_epv() |>
    clean_shots_data() |>
    add_shot_vars() |>
    add_epv_vars() |>
    clean_model_data_wp() |>
    add_wp_vars()

  # --- Step 2: Fetch player stats and lineups from API ---
  cli::cli_inform("Fetching player stats and lineups from API...")
  season <- as.numeric(substr(match_ids[1], 5, 8))

  player_stats <- .fetch_match_player_stats(match_ids)
  teams <- .fetch_match_lineups(match_ids, season)

  # Add utc_start_time to player_stats (needed by create_player_game_data for
  # decay weighting). Extract from chains metadata since API stats don't include it.
  if (!"utc_start_time" %in% names(player_stats) && "utc_start_time" %in% names(chains)) {
    start_times <- unique(chains[, .(match_id, utc_start_time)])
    player_stats <- merge(player_stats, start_times, by = "match_id", all.x = TRUE)
  }

  # --- Step 3: Player-game EPV credit ---
  cli::cli_inform("Computing player EPV credit...")
  player_epv <- create_player_game_data(
    pbp_data = pbp,
    player_stats = player_stats,
    teams = teams
  )

  # Skip position adjustment for single-game analysis (too few players per
  # position group for a meaningful baseline). Use raw EPV instead.
  player_epv <- data.table::as.data.table(player_epv)
  player_epv[, `:=`(
    epv_adj = epv,
    recv_epv_adj = recv_epv,
    disp_epv_adj = disp_epv,
    spoil_epv_adj = spoil_epv,
    hitout_epv_adj = hitout_epv
  )]

  # --- Step 4: Format as game ratings (same output as player_game_ratings) ---
  cli::cli_inform("Done!")
  round_val <- unique(player_epv$round)
  .compute_player_game_ratings(player_epv, season, round_val)
}


#' Fetch player stats for specific match IDs
#'
#' @param match_ids Character vector of match IDs.
#' @return A data.frame of player stats with normalised column names.
#' @keywords internal
.fetch_match_player_stats <- function(match_ids) {
  token <- get_token()
  result <- .fetch_cfs_batch(
    ids = match_ids,
    url_template = "https://api.afl.com.au/cfs/afl/playerStats/match/%s",
    token = token,
    parse_fn = .parse_match_stats,
    label = "player stats"
  )
  if (nrow(result) == 0) {
    cli::cli_abort("No player stats returned for match{?es} {.val {match_ids}}.")
  }

  # Rename providerId -> match_id
  if ("providerId" %in% names(result)) {
    names(result)[names(result) == "providerId"] <- "match_id"
  }

  # Normalise column names to snake_case
  result <- data.table::as.data.table(result)
  .normalise_player_stats_columns(result)
  result
}


#' Fetch team lineups for specific match IDs
#'
#' @param match_ids Character vector of match IDs.
#' @param season Numeric season year.
#' @return A data.frame of team lineup data with normalised column names.
#' @keywords internal
.fetch_match_lineups <- function(match_ids, season) {
  token <- get_token()
  result <- .fetch_cfs_batch(
    ids = match_ids,
    url_template = "https://api.afl.com.au/cfs/afl/matchRoster/full/%s",
    token = token,
    parse_fn = .parse_match_roster,
    label = "lineups"
  )
  if (nrow(result) == 0) {
    cli::cli_abort("No lineup data returned for match{?es} {.val {match_ids}}.")
  }

  result$season <- season

  # Normalise column names
  result <- .normalise_teams_columns(result)
  result
}


#' Apply PSR coefficients to raw per-game stats
#'
#' Computes a game-level PSR by applying the glmnet coefficients directly
#' to each player's box-score stats from a single game. Rate stats
#' (efficiency percentages) are computed from the raw counts.
#'
#' @param player_epv A data.table from [create_player_game_data()].
#' @return The input data.table with `game_psr`, `game_osr`, `game_dsr` added.
#' @keywords internal
.add_game_psr <- function(player_epv) {
  dt <- data.table::as.data.table(player_epv)

  # Load coefficient files
  psr_coefs <- .load_psr_coefs("psr_v2_coefficients.csv")
  osr_coefs <- .load_psr_coefs("osr_v2_coefficients.csv")
  dsr_coefs <- .load_psr_coefs("dsr_v2_coefficients.csv")

  if (is.null(psr_coefs)) {
    cli::cli_warn("PSR coefficient file not found -- skipping game PSR")
    dt[, c("game_psr", "game_osr", "game_dsr") := NA_real_]
    return(dt)
  }

  # Ensure efficiency columns exist (may be missing from some API responses)
  for (col in c("effective_disposals", "effective_kicks")) {
    if (!col %in% names(dt)) dt[, (col) := 0L]
  }

  # Compute rate stats from raw counts
  dt[, `:=`(
    disposal_efficiency = data.table::fifelse(disposals > 0, effective_disposals / disposals, 0),
    goal_accuracy = data.table::fifelse(shots_at_goal > 0, goals / shots_at_goal, 0),
    kick_efficiency = data.table::fifelse(kicks > 0, effective_kicks / kicks, 0),
    contested_poss_rate = data.table::fifelse(
      (contested_possessions + uncontested_possessions) > 0,
      contested_possessions / (contested_possessions + uncontested_possessions),
      0
    ),
    hitout_win_pct = data.table::fifelse(
      hitouts > 0,
      hitouts_to_advantage / hitouts,
      0
    )
  )]

  # Apply coefficients
  dt[, game_psr := .apply_coefs(dt, psr_coefs)]
  if (!is.null(osr_coefs) && !is.null(dsr_coefs)) {
    raw_osr <- .apply_coefs(dt, osr_coefs)
    raw_dsr <- .apply_coefs(dt, dsr_coefs)
    # Reconcile so osr + dsr = psr
    delta <- (dt$game_psr - raw_osr - raw_dsr) / 2
    dt[, game_osr := raw_osr + delta]
    dt[, game_dsr := raw_dsr + delta]
  } else {
    dt[, c("game_osr", "game_dsr") := NA_real_]
  }

  # Clean up temporary rate columns
  dt[, c("disposal_efficiency", "goal_accuracy", "kick_efficiency",
         "contested_poss_rate", "hitout_win_pct") := NULL]

  dt
}


#' Apply PSR coefficients to a data.table of player stats
#'
#' @param dt A data.table with stat columns.
#' @param coefs A data.frame with stat_name, beta, sd columns.
#' @return A numeric vector of PSR values (one per row).
#' @keywords internal
.apply_coefs <- function(dt, coefs) {
  # Filter to stats that exist in the data
  available <- coefs$stat_name %in% names(dt)
  if (sum(available) == 0) return(rep(0, nrow(dt)))

  coefs <- coefs[available, , drop = FALSE]
  stat_cols <- coefs$stat_name
  betas <- coefs$beta
  sds <- coefs$sd
  sds[sds == 0 | is.na(sds)] <- 1

  mat <- as.matrix(dt[, stat_cols, with = FALSE])
  mat[is.na(mat)] <- 0

  # Standardise then apply betas
  mat <- sweep(mat, 2, sds, "/")
  as.numeric(mat %*% betas)
}


#' Load PSR coefficient CSV
#'
#' @param filename The coefficient CSV filename.
#' @return A data.frame with stat_name, beta, sd columns, or NULL if not found.
#' @keywords internal
.load_psr_coefs <- function(filename) {
  path <- system.file("extdata", filename, package = "torp")
  if (path == "") {
    fallback <- file.path(
      find.package("torp", quiet = TRUE)[1] %||% ".",
      "data-raw", "cache-skills", filename
    )
    if (file.exists(fallback)) path <- fallback
  }
  if (path == "" || !file.exists(path)) return(NULL)
  coefs <- utils::read.csv(path)
  coefs[coefs$beta != 0, , drop = FALSE]
}
