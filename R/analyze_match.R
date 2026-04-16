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
#' @param per80 Logical. If `TRUE`, return per-80-minute rates instead of
#'   totals for EPV and PSV columns. Default `FALSE` (totals).
#'
#' @return A data.table with EPV components, PSV/OSV/DSV, and TORP value.
#'   If `per80 = TRUE`, values are divided by TOG (per full game).
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
#' # Per-80 rates instead of totals
#' result <- get_player_game_ratings(round = 2, per80 = TRUE)
#'
#' # From pre-scraped chains
#' chains <- get_match_chains(2026, 2)
#' result <- get_player_game_ratings(chains)
#' }
get_player_game_ratings <- function(match = NULL,
                                    season = get_afl_season(),
                                    round = NULL,
                                    per80 = FALSE) {
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
  round_val <- unique(player_epv$round)
  pgr <- .compute_player_game_ratings(player_epv, season, round_val)

  # --- Step 5: Add PSV/OSV/DSV from box-score stats ---
  ps_dt <- data.table::as.data.table(player_stats)
  if (!"tog" %in% names(ps_dt) && "time_on_ground_percentage" %in% names(ps_dt)) {
    ps_dt[, tog := pmax(time_on_ground_percentage / 100, 0.1)]
  }
  if ("season" %in% names(player_epv) && !"season" %in% names(ps_dt)) {
    ps_dt[, season := season]
  }
  if ("round" %in% names(player_epv) && !"round" %in% names(ps_dt)) {
    ps_dt[, round := as.integer(round_val[1])]
  }

  psv_result <- tryCatch(.compute_psv(ps_dt), error = function(e) {
    cli::cli_warn("PSV computation skipped: {conditionMessage(e)}")
    NULL
  })

  # Ensure stable output schema: PSV columns always present (NA on failure)
  if (is.null(psv_result)) {
    for (col in c("psv", "osv", "dsv", "torp_value")) {
      pgr[, (col) := NA_real_]
      pgr[, (paste0(col, "_p80")) := NA_real_]
    }
  }

  if (!is.null(psv_result)) {
    psv_cols <- intersect(c("psv", "osv", "dsv"), names(psv_result))
    if (length(psv_cols) > 0 && "player_id" %in% names(psv_result) &&
        "match_id" %in% names(psv_result)) {
      psv_slim <- psv_result[, c("player_id", "match_id", psv_cols), with = FALSE]
      pgr <- merge(pgr, psv_slim, by = c("player_id", "match_id"), all.x = TRUE)

      # PSV per-80 rates
      for (col in psv_cols) {
        p80_col <- paste0(col, "_p80")
        pgr[, (p80_col) := round(get(col) / tog, 1)]
      }

      # TORP value: 50% EPV + 50% PSV
      if (all(c("epv", "psv") %in% names(pgr))) {
        pgr[, torp_value := round(TORP_EPR_WEIGHT * epv + (1 - TORP_EPR_WEIGHT) * psv, 1)]
        pgr[, torp_value_p80 := round(torp_value / tog, 1)]
      }
    }
  }

  # Select totals or p80 columns based on per80 parameter
  if (per80) {
    val_cols <- c("epv_p80", "recv_epv_p80", "disp_epv_p80", "spoil_epv_p80", "hitout_epv_p80",
                  "psv_p80", "osv_p80", "dsv_p80", "torp_value_p80")
  } else {
    val_cols <- c("epv", "recv_epv", "disp_epv", "spoil_epv", "hitout_epv",
                  "psv", "osv", "dsv", "torp_value")
  }

  col_order <- c(
    "season", "round", "player_name", "position_group", "team", "opp", "tog",
    val_cols,
    "player_id", "team_id", "match_id"
  )
  col_order <- intersect(col_order, names(pgr))
  # Drop the other set (totals if per80, p80s if not)
  keep_cols <- c("season", "round", "player_name", "position_group", "team", "opp", "tog",
                 col_order[col_order %in% val_cols],
                 "player_id", "team_id", "match_id")
  pgr <- pgr[, intersect(keep_cols, names(pgr)), with = FALSE]
  data.table::setcolorder(pgr, intersect(col_order, names(pgr)))

  sort_col <- if (per80) "torp_value_p80" else "torp_value"
  if (!sort_col %in% names(pgr)) sort_col <- if (per80) "epv_p80" else "epv"
  if (sort_col %in% names(pgr)) data.table::setorderv(pgr, sort_col, order = -1L)

  cli::cli_inform("Done!")
  pgr
}


#' Fetch Player Game Stats from AFL API
#'
#' Fetches live player statistics for one or more matches directly from the
#' AFL API. Returns normalised snake_case columns.
#'
#' @param match_ids Character vector of match IDs (e.g. `"CD_M20260140405"`).
#' @return A data.table of player stats with normalised column names.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' stats <- get_player_game_stats("CD_M20260140405")
#' stats <- get_player_game_stats(c("CD_M20260140401", "CD_M20260140402"))
#' }
get_player_game_stats <- function(match_ids) {
  result <- .fetch_match_player_stats(match_ids)
  result[]
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
    url_template = paste0(AFL_CFS_API_BASE_URL, "playerStats/match/%s"),
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
    url_template = paste0(AFL_CFS_API_BASE_URL, "matchRoster/full/%s"),
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
  psr_coefs <- .load_psr_coefs("psr_coefficients.csv")
  osr_coefs <- .load_psr_coefs("osr_coefficients.csv")
  dsr_coefs <- .load_psr_coefs("dsr_coefficients.csv")

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
      "data-raw", "cache-stat-ratings", filename
    )
    if (file.exists(fallback)) path <- fallback
  }
  if (path == "" || !file.exists(path)) return(NULL)
  coefs <- utils::read.csv(path)
  coefs[coefs$beta != 0, , drop = FALSE]
}
