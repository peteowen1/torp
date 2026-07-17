# Directed Matchup Table
# =======================
# torp#108: a 612-row directed matchup table (18x17 ordered team pairs, at
# the host's real ground + the same 306 pairs at the MCG) priced by the FULL
# GAM+XGBoost match model -- not the torp-diff formula. Consumed by
# inthegame-blog's afl/season-sim.js `opts.matchupTable` hook to override
# per-tie finals pricing ahead of its residual-based fallback formula.
#
# Design (see torp#108 for the full feasibility investigation):
#   1. .freeze_match_state() runs one real pipeline-equivalent build --
#      fixtures/ratings/features, the injury/roster overlay that
#      run_predictions_pipeline() already uses for hypothetical fixtures
#      (match_model.R:633-644), weather, and the trained GAM+XGBoost chain --
#      producing a single frozen team_mdl_df plus fitted models.
#   2. .extract_frozen_teams() reads a per-team SNAPSHOT off that frozen
#      state (current roster/injury-adjusted ratings, home ground, and
#      historical venue familiarity) WITHOUT ever pooling synthetic rows
#      back into it -- the distortion trap flagged in the issue.
#   3. .build_matchup_newdata() fabricates 612 x 2 (host-perspective +
#      visitor-perspective) rows against that frozen snapshot, recomputing
#      only venue-dependent features (log_dist, familiarity) per row.
#   4. .predict_match_model() replicates run_predictions_pipeline()'s exact
#      predict-time sequence (GAM chain -> XGBoost chain -> 50/50 blend ->
#      GAM win head fed the blended margin -> margin recalibration) on that
#      newdata, using the models fitted in step 1.
#   5. build_matchup_table() averages the host/visitor perspectives exactly
#      as .format_match_preds() does for real fixtures, and returns the
#      final 612-row table.
#
# Step 1 duplicates a meaningful slice of run_predictions_pipeline()'s
# fixtures->team_mdl_df->trained-models pipeline (match_model.R) rather than
# calling that function directly, because run_predictions_pipeline() has no
# dry-run mode (it always attempts save_to_release()/torpmodels uploads) and
# carries zero test coverage (torp/CLAUDE.md) -- extracting a shared
# pre-upload core was judged riskier than this bounded duplication. If
# run_predictions_pipeline() gains a shared core helper later, this file
# should be the first caller migrated onto it.

# .gf_anchor_date ----

#' Grand-Final-era anchor date for fabricated matchup rows
#'
#' The temporal GAM/XGB features (game_year_decimal, game_prop_through_year,
#' etc.) need SOME calendar date. Since this table's primary consumer prices
#' finals ties (torp#108), every fabricated row is anchored to the same
#' plausible date: the last Saturday of September in \code{season} (the
#' long-run AFL Grand Final date convention). This is a single shared
#' snapshot date, not a per-tie round date -- the table itself carries no
#' round dimension (see afl/season-sim.js's \code{opts.matchupTable} header).
#'
#' @param season Season year
#' @return A single Date
#' @keywords internal
.gf_anchor_date <- function(season) {
  sept_end <- as.Date(sprintf("%d-09-30", season))
  # ISO weekday (%u): Mon=1 .. Sun=7, Saturday=6
  offset <- (as.integer(format(sept_end, "%u")) - 6) %% 7
  sept_end - offset
}

# .freeze_match_state ----

#' Build one real pipeline-equivalent match-model state, frozen for reuse
#'
#' Mirrors \code{run_predictions_pipeline()} (match_model.R) through the
#' trained GAM+XGBoost chain -- data loads, fixture/team-rating features,
#' the roster/injury overlay for hypothetical (unannounced-lineup) fixtures,
#' weather, \code{.build_team_mdl_df()}, and both model families -- but
#' stops before predictions are formatted/validated/uploaded. Read-only: no
#' \code{save_to_release()} or torpmodels upload call is made.
#'
#' @param season Season year (default: current via \code{get_afl_season()})
#' @param week Round number used for the roster/injury-adjusted snapshot
#'   (default: next round via \code{get_afl_week("next")})
#' @param nthreads Threads for \code{mgcv::bam()} (default 4, matches
#'   \code{run_predictions_pipeline()})
#' @return A list with \code{team_mdl_df} (fully featured + blended +
#'   calibrated), \code{team_rt_fix_df} (post roster/injury overlay),
#'   \code{all_grounds}, \code{gam_models}, \code{xgb_models} (possibly
#'   \code{NULL}), \code{margin_calib}, \code{xgb_osr_dsr_cols},
#'   \code{xgb_weather_cols}, \code{season}, \code{week}
#' @keywords internal
.freeze_match_state <- function(season = NULL, week = NULL, nthreads = 4L) {
  if (is.null(season)) season <- get_afl_season()
  if (is.null(week)) week <- get_afl_week(type = "next")
  target_weeks <- week

  cli::cli_h2("build_matchup_table: freezing match-model state ({season} R{week})")

  all_grounds <- file_reader("stadium_data", "reference-data")
  xg_df <- load_xg(TRUE)
  fixtures <- load_fixtures(TRUE)
  results <- load_results(TRUE)
  teams <- load_teams(TRUE)

  torp_df_total <- tryCatch(load_torp_ratings(), error = function(e) NULL)
  if (is.null(torp_df_total) || nrow(torp_df_total) < 100) {
    cli::cli_abort("build_matchup_table: TORP ratings unavailable or too small ({if (is.null(torp_df_total)) 0 else nrow(torp_df_total)} rows)")
  }
  if (nrow(fixtures) < 100) cli::cli_abort("build_matchup_table: fixtures too small ({nrow(fixtures)} rows)")
  if (nrow(teams) < 100) cli::cli_abort("build_matchup_table: teams too small ({nrow(teams)} rows)")

  target_fixtures <- fixtures |> dplyr::filter(season == .env$season, round_number %in% target_weeks)
  weight_anchor_date <- if (nrow(target_fixtures) > 0) as.Date(min(target_fixtures$utc_start_time)) else Sys.Date()

  cli::cli_h2("Building fixture features")
  fix_df <- .build_fixtures_df(fixtures)

  cli::cli_h2("Loading PSR")
  psr_df <- NULL
  tryCatch({
    psr_df <- load_psr(TRUE)
    if (nrow(psr_df) == 0) psr_df <- NULL
  }, error = function(e) {
    cli::cli_warn("Could not load PSR from release: {conditionMessage(e)}")
  })
  if (is.null(psr_df) || !all(c("osr", "dsr") %in% names(psr_df))) {
    tryCatch({
      skills <- load_player_stat_ratings(TRUE)
      psr_df <- .compute_psr_from_stat_ratings(skills)
    }, error = function(e) {
      cli::cli_warn("Failed to compute PSR: {conditionMessage(e)}")
    })
  }

  cli::cli_h2("Processing lineups")
  team_rt_df <- .build_team_ratings_df(teams, torp_df_total, psr_df)

  cli::cli_h2("Computing features")
  team_rt_fix_df <- .build_match_features(fix_df, team_rt_df, all_grounds)

  # Injury / roster overlay -- duplicated from run_predictions_pipeline()
  # (match_model.R:535-653), minus the save_injury_data() upload. This is
  # exactly the "lineup fallback" the torp#108 investigation identified as
  # already solving the hypothetical-fixture case: when a team has no
  # announced lineup (count NA/0) for the target week, roster + injury-
  # adjusted ratings (epr_week etc) are used instead of lineup-summed ones.
  cli::cli_h2("Loading injuries")
  inj_df <- get_all_injuries(season)
  if (nrow(inj_df) > 0) {
    inj_df$return_round <- parse_return_round(inj_df$estimated_return, season, min(target_weeks))
  }

  tr <- torp_ratings(season, min(target_weeks))
  if (nrow(tr) == 0 || !"player_name" %in% names(tr)) {
    cli::cli_abort("build_matchup_table: no TORP ratings available for {season} R{min(target_weeks)}")
  }
  tr[c("injury", "estimated_return")] <- NULL
  tr <- match_injuries(tr, inj_df)
  if (!"estimated_return" %in% names(tr)) tr$estimated_return <- NA_character_
  tr <- dplyr::mutate(tr, estimated_return = tidyr::replace_na(estimated_return, "None"))

  if (!is.null(psr_df)) {
    tr <- tr |>
      dplyr::select(-dplyr::any_of("psr")) |>
      dplyr::left_join(
        psr_df |> dplyr::select(player_id, season, round, psr),
        by = c("player_id", "season", "round")
      ) |>
      dplyr::mutate(psr = tidyr::replace_na(psr, PSR_PRIOR_RATE))
  } else {
    tr$psr <- PSR_PRIOR_RATE
  }

  if (nrow(inj_df) > 0 && "return_round" %in% names(inj_df)) {
    inj_return <- inj_df |>
      dplyr::select(player_norm, return_round) |>
      dplyr::distinct(player_norm, .keep_all = TRUE)
    tr <- tr |>
      dplyr::mutate(player_norm = tolower(trimws(player_name))) |>
      dplyr::left_join(inj_return, by = "player_norm") |>
      dplyr::select(-player_norm)
  } else {
    tr$return_round <- NA_real_
  }

  .build_week_ratings <- function(tr_data, w) {
    weeks_ahead <- max(w - min(target_weeks), 0)
    discount <- max(INJURY_KNOWN_DISCOUNT - 0.01 * weeks_ahead, INJURY_DISCOUNT_FLOOR)

    tr_data |>
      dplyr::filter(
        !is.na(epr),
        is.na(injury) | (!is.na(return_round) & return_round <= w)
      ) |>
      dplyr::mutate(team_name = team) |>
      dplyr::group_by(team_name, season, round) |>
      dplyr::mutate(
        n_players = dplyr::n(),
        team_tog_sum = sum(pred_tog, na.rm = TRUE),
        tog_wt = dplyr::if_else(team_tog_sum > 0, pred_tog * 18 / team_tog_sum, 18 / n_players)
      ) |>
      dplyr::summarise(
        epr_week = sum(epr * tog_wt, na.rm = TRUE) * discount,
        epr_recv_week = sum(recv_epr * tog_wt, na.rm = TRUE) * discount,
        epr_disp_week = sum(disp_epr * tog_wt, na.rm = TRUE) * discount,
        epr_spoil_week = sum(spoil_epr * tog_wt, na.rm = TRUE) * discount,
        epr_hitout_week = sum(hitout_epr * tog_wt, na.rm = TRUE) * discount,
        psr_week = sum(psr * tog_wt, na.rm = TRUE) * discount,
        .groups = "drop"
      ) |>
      dplyr::mutate(round = w)
  }

  tr_week <- purrr::map_dfr(target_weeks, function(w) .build_week_ratings(tr, w))

  team_rt_fix_df <- team_rt_fix_df |>
    dplyr::left_join(tr_week, by = c("team_name" = "team_name", "season" = "season", "round_number" = "round")) |>
    dplyr::mutate(
      use_roster = !is.na(epr_week) & (is.na(count) | count == 0),
      epr = dplyr::if_else(use_roster, epr_week, epr),
      recv_epr = dplyr::if_else(use_roster, epr_recv_week, recv_epr),
      disp_epr = dplyr::if_else(use_roster, epr_disp_week, disp_epr),
      spoil_epr = dplyr::if_else(use_roster, epr_spoil_week, spoil_epr),
      hitout_epr = dplyr::if_else(use_roster, epr_hitout_week, hitout_epr),
      psr = dplyr::if_else(use_roster, psr_week, psr),
      use_roster = NULL
    )

  cli::cli_h2("Loading weather")
  weather_df <- .load_match_weather(fixtures, all_grounds, target_weeks, season)

  cli::cli_h2("Building model dataset")
  team_mdl_df <- .build_team_mdl_df(team_rt_fix_df, results, xg_df, weather_df, weight_anchor_date)

  cli::cli_h2("Training GAM models")
  gam_result <- .train_match_gams(team_mdl_df, nthreads = nthreads)
  team_mdl_df <- gam_result$data

  cli::cli_h2("Training XGBoost models")
  xgb_result <- tryCatch(
    .train_match_xgb(team_mdl_df),
    error = function(e) {
      cli::cli_warn("build_matchup_table: XGBoost training failed ({conditionMessage(e)}), falling back to GAM-only")
      NULL
    }
  )
  if (!is.null(xgb_result)) team_mdl_df <- xgb_result$data

  # Same availability checks .train_match_xgb() used to decide its own
  # feature-column set -- must be reproduced exactly here (not re-derived
  # from synthetic newdata) so predict-time model.matrix() columns line up
  # with what the fitted xgboost models actually expect.
  xgb_osr_dsr_cols <- if (all(c("osr_diff", "dsr_diff") %in% names(team_mdl_df)) &&
                          !all(is.na(team_mdl_df$osr_diff))) {
    c("osr_diff", "dsr_diff")
  } else {
    character(0)
  }
  weather_candidates <- c("log_wind", "log_precip", "temp_avg", "humidity_avg", "is_roof")
  xgb_weather_cols <- if (all(weather_candidates %in% names(team_mdl_df))) weather_candidates else character(0)

  margin_calib <- load_match_margin_calibration()

  cli::cli_alert_success("build_matchup_table: frozen state ready ({nrow(team_mdl_df)} team_mdl_df rows)")

  list(
    team_mdl_df = team_mdl_df,
    team_rt_fix_df = team_rt_fix_df,
    all_grounds = all_grounds,
    gam_models = gam_result$models,
    xgb_models = if (!is.null(xgb_result)) xgb_result$models else NULL,
    margin_calib = margin_calib,
    xgb_osr_dsr_cols = xgb_osr_dsr_cols,
    xgb_weather_cols = xgb_weather_cols,
    season = season,
    week = week
  )
}

# .extract_frozen_teams ----

#' Extract a per-team snapshot off the frozen match-model state
#'
#' Reads current roster/injury-adjusted ratings for every team at the frozen
#' (season, week), plus each team's home ground and historical venue
#' familiarity -- all computed FROM the already-frozen \code{team_rt_fix_df},
#' never by pooling any fabricated matchup row back into it.
#'
#' @param state Output of \code{.freeze_match_state()}
#' @return A list with \code{snapshot} (one row per team: ratings),
#'   \code{home_venue} (team_id -> venue), \code{familiarity_now}
#'   (team_id x venue -> familiarity, 0 for unseen combinations)
#' @keywords internal
.extract_frozen_teams <- function(state) {
  team_rt_fix_df <- state$team_rt_fix_df
  season <- state$season
  week <- state$week

  snapshot <- team_rt_fix_df |>
    dplyr::filter(season == .env$season, round_number == .env$week) |>
    dplyr::distinct(team_id, .keep_all = TRUE) |>
    dplyr::select(
      team_id, team_name, epr, recv_epr, disp_epr, spoil_epr, hitout_epr,
      psr, dplyr::any_of(c("osr", "dsr"))
    ) |>
    dplyr::filter(!is.na(team_name))

  n_missing_ratings <- sum(is.na(snapshot$epr))
  if (n_missing_ratings > 0) {
    cli::cli_abort("build_matchup_table: {n_missing_ratings} team(s) have NA epr in the frozen snapshot ({season} R{week}) -- roster/injury overlay did not resolve them")
  }

  # History used for home-ground + venue familiarity: matches strictly
  # before the frozen target week, across all seasons available (mirrors
  # .build_match_features()'s get_mode()/cumulative-proportion scope, which
  # is also not season-reset -- recomputed fresh here as a per-team/venue
  # SUMMARY because that function only returns a per-REAL-ROW familiarity
  # column, not a lookup table usable for fabricated venues).
  history <- team_rt_fix_df |>
    dplyr::filter(!is.na(venue), !is.na(team_id)) |>
    dplyr::filter(season < .env$season | (season == .env$season & round_number < .env$week))

  home_venue <- history |>
    dplyr::group_by(team_id) |>
    dplyr::summarise(venue = get_mode(venue), .groups = "drop")

  missing_home <- setdiff(snapshot$team_id, home_venue$team_id)
  if (length(missing_home) > 0) {
    cli::cli_abort("build_matchup_table: no historical home-ground data for {length(missing_home)} team(s)")
  }

  total_games <- history |> dplyr::count(team_id, name = "n_total")
  venue_games <- history |> dplyr::count(team_id, venue, name = "n_venue")
  familiarity_now <- venue_games |>
    dplyr::left_join(total_games, by = "team_id") |>
    dplyr::mutate(familiarity = n_venue / n_total) |>
    dplyr::select(team_id, venue, familiarity)

  list(snapshot = snapshot, home_venue = home_venue, familiarity_now = familiarity_now)
}

# .build_matchup_newdata ----

#' Fabricate the 18x17x2 directed matchup rows against the frozen state
#'
#' Builds two prediction-ready rows per matchup (host-perspective,
#' team_type_fac = "home"; visitor-perspective, team_type_fac = "away"),
#' exactly mirroring team_mdl_df's self-joined long format, so
#' \code{.predict_match_model()} can score them with the frozen GAM/XGBoost
#' models unmodified. Only venue-dependent features (log_dist, familiarity,
#' venue_fac, is_roof) vary by the "home" vs "mcg" tier; everything else is
#' the frozen team-level snapshot plus the shared GF-era temporal anchor.
#'
#' @param state Output of \code{.freeze_match_state()}
#' @param frozen Output of \code{.extract_frozen_teams()}
#' @param days_rest Constant days' rest applied to both teams (default
#'   \code{MATCHUP_TABLE_DAYS_REST})
#' @return A data.frame with 1224 rows (612 matchups x 2 perspectives) and
#'   all columns \code{.predict_match_model()} needs
#' @keywords internal
.build_matchup_newdata <- function(state, frozen, days_rest = MATCHUP_TABLE_DAYS_REST) {
  season <- state$season
  snapshot <- frozen$snapshot
  team_names <- snapshot$team_name
  n_teams <- length(team_names)
  if (n_teams < 2) cli::cli_abort("build_matchup_table: fewer than 2 teams in frozen snapshot")

  rating_cols <- intersect(c("epr", "recv_epr", "disp_epr", "spoil_epr", "hitout_epr", "psr", "osr", "dsr"),
                            names(snapshot))
  rating_vec <- stats::setNames(
    lapply(rating_cols, function(col) stats::setNames(snapshot[[col]], team_names)),
    rating_cols
  )
  team_id_vec <- stats::setNames(snapshot$team_id, team_names)

  # Elo (current rating after each team's most recent completed match) --
  # same fallback semantics join_elo_diff_to_team_mdl_df() uses for
  # upcoming/unplayed fixtures.
  elo_matches <- .matches_from_team_mdl_df(state$team_mdl_df)
  elo_result <- build_team_elo(elo_matches)
  elo_vec <- stats::setNames(elo_result$current$elo_current, elo_result$current$team_name)
  missing_elo <- setdiff(team_names, names(elo_vec))
  if (length(missing_elo) > 0) {
    cli::cli_warn("build_matchup_table: {length(missing_elo)} team(s) have no Elo history, using neutral 1500: {paste(missing_elo, collapse = ', ')}")
    elo_vec[missing_elo] <- 1500
  }

  # home_venue is keyed by team_id; re-key by team_name for lookup below
  hv <- frozen$home_venue
  hv$team_name <- snapshot$team_name[match(hv$team_id, snapshot$team_id)]
  home_venue_vec <- stats::setNames(hv$venue, hv$team_name)

  all_grounds <- state$all_grounds
  ground_coords <- all_grounds |>
    dplyr::filter(!is.na(venue)) |>
    dplyr::distinct(venue, .keep_all = TRUE) |>
    dplyr::select(venue, Latitude, Longitude)
  lat_vec <- stats::setNames(ground_coords$Latitude, ground_coords$venue)
  lon_vec <- stats::setNames(ground_coords$Longitude, ground_coords$venue)

  missing_venue_coords <- setdiff(unique(home_venue_vec[team_names]), names(lat_vec))
  if (length(missing_venue_coords) > 0) {
    cli::cli_abort("build_matchup_table: no ground coordinates for venue(s): {paste(missing_venue_coords, collapse = ', ')}")
  }
  if (!"M.C.G." %in% names(lat_vec)) {
    cli::cli_abort("build_matchup_table: 'M.C.G.' not found in stadium reference data")
  }

  fam <- frozen$familiarity_now
  fam_key <- paste(fam$team_id, fam$venue, sep = "")
  fam_vec <- stats::setNames(fam$familiarity, fam_key)
  .familiarity_of <- function(team, venue) {
    key <- paste(team_id_vec[team], venue, sep = "")
    out <- unname(fam_vec[key])
    out[is.na(out)] <- 0
    out
  }

  .log_dist_of <- function(team, venue) {
    d <- geosphere::distHaversine(
      cbind(lon_vec[venue], lat_vec[venue]),
      cbind(lon_vec[home_venue_vec[team]], lat_vec[home_venue_vec[team]])
    )
    log(d + MATCH_LOG_DIST_OFFSET)
  }

  # GF-era temporal anchor, shared by every fabricated row (see
  # .gf_anchor_date() -- the table has no round dimension).
  anchor <- .gf_anchor_date(season)
  game_year <- as.integer(format(anchor, "%Y"))
  game_yday <- as.integer(format(anchor, "%j"))
  days_in_year <- if (lubridate::leap_year(anchor)) 366 else 365
  game_month <- as.integer(format(anchor, "%m"))
  game_mday <- as.integer(format(anchor, "%d"))
  game_wday <- lubridate::wday(anchor, week_start = 1)
  game_prop_through_year <- game_yday / days_in_year
  game_prop_through_month <- game_mday / lubridate::days_in_month(game_month)
  # Typical AFL Grand Final bounce time (~2:30pm local)
  game_prop_through_day <- 14.5 / 24
  game_year_decimal <- game_year + game_prop_through_year

  # Weather: median imputation (matches .build_team_mdl_df()'s own fallback;
  # inserting copies of the median at the median position does not move it,
  # so recomputing off the already-imputed team_mdl_df column reproduces the
  # exact constant used at training time).
  tmdf <- state$team_mdl_df
  weather_const <- list(
    temp_avg = stats::median(tmdf$temp_avg, na.rm = TRUE),
    wind_avg = stats::median(tmdf$wind_avg, na.rm = TRUE),
    humidity_avg = stats::median(tmdf$humidity_avg, na.rm = TRUE),
    precipitation_total = 0
  )
  log_wind_const <- log1p(weather_const$wind_avg)
  log_precip_const <- log1p(weather_const$precipitation_total)

  # Fail loud rather than silently NA: factor(x, levels=) drops any value not
  # in `levels` to NA without warning, and an NA smooth-term input either
  # errors deep inside predict.gam() or (worse) predicts silently on a
  # partial model -- verify every fabricated label is a real training-time
  # level before it ever reaches predict().
  candidate_venues <- unique(c(hv$venue, "M.C.G."))
  bad_venues <- setdiff(candidate_venues, levels(tmdf$venue_fac))
  if (length(bad_venues) > 0) {
    cli::cli_abort("build_matchup_table: venue(s) not in the trained model's venue_fac levels: {paste(bad_venues, collapse = ', ')}")
  }
  if (!"0" %in% levels(tmdf$days_rest_diff_fac)) {
    cli::cli_abort("build_matchup_table: days_rest_diff_fac has no '0' level in the trained model -- cannot fabricate a symmetric-rest row")
  }
  if (!all(c("home", "away") %in% levels(tmdf$team_type_fac))) {
    cli::cli_abort("build_matchup_table: team_type_fac is missing 'home'/'away' levels in the trained model")
  }
  if (!as.character(game_wday) %in% levels(tmdf$game_wday_fac.x)) {
    cli::cli_abort("build_matchup_table: game_wday_fac.x has no level for weekday {game_wday} (GF anchor {anchor}) in the trained model")
  }

  pairs <- tidyr::expand_grid(host = team_names, visitor = team_names) |>
    dplyr::filter(host != visitor)
  pairs <- dplyr::bind_rows(
    dplyr::mutate(pairs, tier = "home"),
    dplyr::mutate(pairs, tier = "mcg")
  )
  pairs$matchup_id <- seq_len(nrow(pairs))
  pairs$match_venue <- ifelse(pairs$tier == "mcg", "M.C.G.", home_venue_vec[pairs$host])
  pairs$is_roof <- pairs$match_venue %in% AFL_ROOF_VENUES

  # One row per (matchup, perspective): self = "own" team, opp = the other.
  persp <- dplyr::bind_rows(
    dplyr::transmute(pairs, matchup_id, tier, match_venue, is_roof,
                      team_type = "home", self = host, opp = visitor),
    dplyr::transmute(pairs, matchup_id, tier, match_venue, is_roof,
                      team_type = "away", self = visitor, opp = host)
  )

  n <- nrow(persp)
  df <- data.frame(
    matchup_id = persp$matchup_id,
    tier = persp$tier,
    team_type_fac = factor(persp$team_type, levels = levels(tmdf$team_type_fac)),
    team_name.x = persp$self,
    team_name.y = persp$opp,
    team_name_season.x = paste(persp$self, season),
    team_name_season.y = paste(persp$opp, season),
    venue_fac = factor(persp$match_venue, levels = levels(tmdf$venue_fac)),
    days_rest_diff_fac = factor("0", levels = levels(tmdf$days_rest_diff_fac)),
    game_year_decimal.x = rep(game_year_decimal, n),
    game_prop_through_year.x = rep(game_prop_through_year, n),
    game_prop_through_month.x = rep(game_prop_through_month, n),
    game_wday_fac.x = factor(rep(as.character(game_wday), n), levels = levels(tmdf$game_wday_fac.x)),
    game_prop_through_day.x = rep(game_prop_through_day, n),
    temp_avg = rep(weather_const$temp_avg, n),
    wind_avg = rep(weather_const$wind_avg, n),
    humidity_avg = rep(weather_const$humidity_avg, n),
    precipitation_total = rep(weather_const$precipitation_total, n),
    log_wind = rep(log_wind_const, n),
    log_precip = rep(log_precip_const, n),
    is_roof = persp$is_roof,
    log_dist.x = .log_dist_of(persp$self, persp$match_venue),
    log_dist.y = .log_dist_of(persp$opp, persp$match_venue),
    familiarity.x = .familiarity_of(persp$self, persp$match_venue),
    familiarity.y = .familiarity_of(persp$opp, persp$match_venue),
    elo_diff = unname(elo_vec[persp$self] - elo_vec[persp$opp]),
    stringsAsFactors = FALSE
  )

  for (col in rating_cols) {
    df[[paste0(col, ".x")]] <- unname(rating_vec[[col]][persp$self])
    df[[paste0(col, ".y")]] <- unname(rating_vec[[col]][persp$opp])
  }

  df$epr_diff <- df$epr.x - df$epr.y
  df$epr_recv_diff <- df$recv_epr.x - df$recv_epr.y
  df$epr_disp_diff <- df$disp_epr.x - df$disp_epr.y
  df$epr_spoil_diff <- df$spoil_epr.x - df$spoil_epr.y
  df$epr_hitout_diff <- df$hitout_epr.x - df$hitout_epr.y
  df$psr_diff <- df$psr.x - df$psr.y
  if (all(c("osr.x", "osr.y") %in% names(df))) df$osr_diff <- df$osr.x - df$osr.y
  if (all(c("dsr.x", "dsr.y") %in% names(df))) df$dsr_diff <- df$dsr.x - df$dsr.y
  df$torp.x <- TORP_EPR_WEIGHT * df$epr.x + (1 - TORP_EPR_WEIGHT) * df$psr.x
  df$torp.y <- TORP_EPR_WEIGHT * df$epr.y + (1 - TORP_EPR_WEIGHT) * df$psr.y
  df$torp_diff <- df$torp.x - df$torp.y
  df$log_dist_diff <- df$log_dist.x - df$log_dist.y
  df$familiarity_diff <- df$familiarity.x - df$familiarity.y

  df
}

# .predict_match_model ----

#' Score newdata with the frozen GAM+XGBoost match model
#'
#' Replicates \code{run_predictions_pipeline()}'s exact predict-time
#' sequence (match_model.R): sequential GAM chain (total xPoints -> xScore
#' diff -> conversion diff -> score diff), the parallel XGBoost chain (same
#' four steps), a 50/50 Input Blend of the two, the GAM win head re-fed the
#' BLENDED total/margin (not its own GAM-only training inputs -- this
#' asymmetry is intentional in production, see match_model.R's blend-block
#' comment), then margin recalibration.
#'
#' @param df newdata built by \code{.build_matchup_newdata()} (or any frame
#'   with the same columns)
#' @param state Output of \code{.freeze_match_state()} (supplies models,
#'   margin_calib, and the xgb feature-availability flags)
#' @return \code{df} with \code{pred_tot_xscore}, \code{pred_score_diff}
#'   (post-calibration), and \code{pred_win} columns added
#' @keywords internal
.predict_match_model <- function(df, state) {
  gam_models <- state$gam_models
  xgb_models <- state$xgb_models

  df$gam_pred_tot_xscore  <- as.numeric(predict(gam_models$total_xpoints, newdata = df, type = "response"))
  df$gam_pred_xscore_diff <- as.numeric(predict(gam_models$xscore_diff, newdata = df, type = "response"))
  df$gam_pred_conv_diff   <- as.numeric(predict(gam_models$conv_diff, newdata = df, type = "response"))
  df$gam_pred_score_diff  <- as.numeric(predict(gam_models$score_diff, newdata = df, type = "response"))

  if (!is.null(xgb_models)) {
    base_cols <- c(
      "team_type_fac",
      "game_year_decimal.x", "game_prop_through_year.x",
      "game_prop_through_month.x", "game_prop_through_day.x",
      "epr_diff", "epr_recv_diff", "epr_disp_diff", "epr_spoil_diff", "epr_hitout_diff",
      "torp_diff", "psr_diff", state$xgb_osr_dsr_cols,
      "elo_diff", "log_dist_diff", "familiarity_diff", "days_rest_diff_fac"
    )

    xgb_levels <- list(
      team_type_fac = levels(state$team_mdl_df$team_type_fac),
      days_rest_diff_fac = levels(state$team_mdl_df$days_rest_diff_fac)
    )
    .relevel <- function(d) {
      for (col in names(xgb_levels)) {
        if (col %in% names(d)) d[[col]] <- factor(as.character(d[[col]]), levels = xgb_levels[[col]])
      }
      d
    }
    .xgb_predict <- function(model, cols) {
      mat <- stats::model.matrix(~ . - 1, data = .relevel(df[, cols, drop = FALSE]))
      predict(model, xgboost::xgb.DMatrix(data = mat))
    }

    s1_cols <- c(base_cols, state$xgb_weather_cols)
    df$xgb_pred_tot_xscore <- .xgb_predict(xgb_models$total_xpoints, s1_cols)
    s2_cols <- c(base_cols, "xgb_pred_tot_xscore")
    df$xgb_pred_xscore_diff <- .xgb_predict(xgb_models$xscore_diff, s2_cols)
    s3_cols <- c(base_cols, "xgb_pred_tot_xscore", "xgb_pred_xscore_diff")
    df$xgb_pred_conv_diff <- .xgb_predict(xgb_models$conv_diff, s3_cols)
    s4_cols <- c(base_cols, "xgb_pred_xscore_diff", "xgb_pred_conv_diff", "xgb_pred_tot_xscore")
    df$xgb_pred_score_diff <- .xgb_predict(xgb_models$score_diff, s4_cols)

    df$pred_tot_xscore <- 0.5 * df$gam_pred_tot_xscore + 0.5 * df$xgb_pred_tot_xscore
    df$pred_score_diff <- 0.5 * df$gam_pred_score_diff + 0.5 * df$xgb_pred_score_diff
  } else {
    df$pred_tot_xscore <- df$gam_pred_tot_xscore
    df$pred_score_diff <- df$gam_pred_score_diff
  }

  df$pred_win <- as.numeric(predict(gam_models$win, newdata = df, type = "response"))
  df$pred_score_diff <- apply_match_margin_calibration(df$pred_score_diff, state$margin_calib)

  df
}

# build_matchup_table ----

#' Build the directed 612-row AFL matchup table
#'
#' Fabricates every ordered (host, visitor) pair among the season's teams at
#' the host's real home ground ("home" tier), plus the same pairs at the
#' MCG ("mcg" tier, for Grand-Final-only pricing), and prices each with the
#' FULL GAM+XGBoost match model (not the torp-diff formula). Consumed by
#' inthegame-blog's \code{afl/season-sim.js} \code{opts.matchupTable} hook.
#'
#' Per-team state (roster/injury-adjusted ratings, home ground, venue
#' familiarity, Elo) is frozen from a single real pipeline-equivalent build
#' at (\code{season}, \code{week}) -- see \code{.freeze_match_state()} --
#' and reused unmodified for every fabricated row; fabricated rows are never
#' pooled back into that aggregation (the distortion trap the underlying
#' feature-engineering functions are not designed to tolerate, since
#' \code{.build_match_features()} derives home ground and venue familiarity
#' by aggregating over whatever rows it's given).
#'
#' Decisions (torp#108, documented per the issue's request):
#' \itemize{
#'   \item \strong{days_rest}: a symmetric constant (\code{MATCHUP_TABLE_DAYS_REST},
#'     default 13 -- a "GF-era" Prelim-to-Grand-Final gap). Only the ROUNDED
#'     DIFFERENCE between the two teams' rest enters the model
#'     (\code{days_rest_diff_fac}); applying the same constant to both teams
#'     always yields a "0" diff regardless of magnitude, so this is a
#'     documentation choice, not a numerically load-bearing one.
#'   \item \strong{MCG-tier "home" designation}: the listed \code{home} team is
#'     still treated as the model's \code{team_type_fac == "home"} perspective
#'     even though the MCG isn't literally every host's ground (there is no
#'     neutral-venue flag in the match model) -- \code{log_dist}/
#'     \code{familiarity} are still recomputed per-team for the MCG, so a
#'     Victorian "home" team correctly gets its real (typically high) MCG
#'     familiarity while an interstate one gets its real (typically low)
#'     familiarity. That asymmetry is the venue effect working as intended,
#'     not a bug.
#'   \item \strong{weather}: median imputation, matching
#'     \code{.build_team_mdl_df()}'s own fallback for missing weather.
#' }
#'
#' @param season Season year (default: current via \code{get_afl_season()})
#' @param week Round number used to snapshot each team's current
#'   roster/injury-adjusted strength (default: next round via
#'   \code{get_afl_week("next")})
#' @param days_rest Symmetric days'-rest constant applied to both teams in
#'   every fabricated row (default \code{MATCHUP_TABLE_DAYS_REST})
#' @param state Optional pre-built output of \code{.freeze_match_state()} --
#'   pass this to reuse an already-trained frozen state (e.g. across repeated
#'   calls in one session) instead of retraining. Mostly useful for tests.
#' @return A tibble with one row per (host, visitor, venue-tier): columns
#'   \code{home}, \code{away}, \code{venue} (\code{"home"} or \code{"mcg"}),
#'   \code{p_home}, \code{pred_margin}, \code{pred_total}, \code{season},
#'   \code{round}, \code{updated}. 18 teams x 17 opponents x 2 tiers = 612 rows.
#' @export
#' @examples
#' \dontrun{
#' tbl <- build_matchup_table(2026)
#' }
build_matchup_table <- function(season = NULL, week = NULL,
                                days_rest = MATCHUP_TABLE_DAYS_REST,
                                state = NULL) {
  if (is.null(state)) {
    state <- .freeze_match_state(season = season, week = week)
  }
  season <- state$season
  week <- state$week

  frozen <- .extract_frozen_teams(state)
  newdata <- .build_matchup_newdata(state, frozen, days_rest = days_rest)
  scored <- .predict_match_model(newdata, state)

  # Symmetrize host/visitor perspectives exactly as .format_match_preds()
  # does for real fixtures: average the host-perspective row with the
  # FLIPPED visitor-perspective row (pred_score_diff negated, pred_win
  # complemented), both re-expressed in (home=host, away=visitor) terms so
  # a plain group_by(matchup_id, tier, home, away) pairs them up.
  host_rows <- scored |>
    dplyr::filter(team_type_fac == "home") |>
    dplyr::transmute(matchup_id, tier, home = team_name.x, away = team_name.y,
                      pred_win, pred_score_diff, pred_tot_xscore)
  visitor_rows <- scored |>
    dplyr::filter(team_type_fac == "away") |>
    dplyr::transmute(matchup_id, tier, home = team_name.y, away = team_name.x,
                      pred_win = 1 - pred_win, pred_score_diff = -pred_score_diff,
                      pred_tot_xscore)

  out <- dplyr::bind_rows(host_rows, visitor_rows) |>
    dplyr::group_by(matchup_id, tier, home, away) |>
    dplyr::summarise(
      p_home = mean(pred_win),
      pred_margin = mean(pred_score_diff),
      pred_total = mean(pred_tot_xscore),
      .groups = "drop"
    ) |>
    dplyr::transmute(
      home, away,
      venue = tier,
      p_home, pred_margin, pred_total,
      season = season, round = week,
      updated = format(Sys.time())
    ) |>
    dplyr::arrange(venue, home, away)

  n_na <- sum(is.na(out$p_home) | is.na(out$pred_margin) | is.na(out$pred_total))
  if (n_na > 0) {
    cli::cli_warn("build_matchup_table: {n_na} row(s) have NA p_home/pred_margin/pred_total")
  }
  expected_n <- length(unique(frozen$snapshot$team_name)) * (length(unique(frozen$snapshot$team_name)) - 1) * 2
  if (nrow(out) != expected_n) {
    cli::cli_warn("build_matchup_table: expected {expected_n} rows, got {nrow(out)}")
  }

  tibble::as_tibble(out)
}
