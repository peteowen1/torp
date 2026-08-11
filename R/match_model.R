# Match Model Orchestration
# ========================
# Convenience wrappers and the full predictions pipeline.
# Data prep helpers are in match_data_prep.R; training in match_train.R.

# .blend_gam_xgb ----

#' The GAM/XGBoost Input Blend
#'
#' One definition of the blend that produces every published match prediction.
#' Called by `run_predictions_pipeline()` (this file),
#' `fit_match_margin_calibration()` (match_calibration.R) and
#' `build_matchup_table()` (matchup_table.R) -- before 2026-08-11 each wrote the
#' arithmetic out itself, so the sidecar that gates releases and the table that
#' prices finals for the blog scored a copy of production rather than
#' production.
#'
#' @param gam_pred,xgb_pred Numeric vectors of equal length.
#' @param weight GAM weight; the XGBoost weight is `1 - weight`.
#' @return Numeric vector, the blended prediction.
#' @keywords internal
.blend_gam_xgb <- function(gam_pred, xgb_pred, weight = MATCH_BLEND_WEIGHT) {
  weight * gam_pred + (1 - weight) * xgb_pred
}


# build_team_mdl_df (convenience wrapper) ----

#' Build complete match model dataset end-to-end
#'
#' Convenience wrapper chaining all internal .build_* functions.
#' Loads data, builds fixtures, ratings, features, weather, and model dataset.
#'
#' @param season Season to build for (default: current via get_afl_season())
#' @param target_weeks Numeric vector of target round numbers (used for weather forecasting)
#' @return Complete team_mdl_df ready for GAM training
#' @keywords internal
build_team_mdl_df <- function(season = NULL, target_weeks = NULL,
                              psr_coef_path = NULL) {
  if (is.null(season)) season <- get_afl_season()

  cli::cli_h2("Loading data")
  all_grounds <- file_reader("stadium_data", "reference-data")
  xg_df <- load_xg(TRUE)
  fixtures <- load_fixtures(TRUE)
  results <- load_results(TRUE)
  teams <- load_teams(TRUE)
  torp_df <- load_torp_ratings()

  # Load PSR (Player Skill Ratings) with osr/dsr decomposition
  psr_df <- NULL
  tryCatch({
    skills <- load_player_stat_ratings(TRUE)
    psr_df <- .compute_psr_from_stat_ratings(skills, psr_coef_path)
    if (!is.null(psr_df)) {
      cli::cli_inform("PSR computed for {nrow(psr_df)} player-rounds")
    }
  }, error = function(e) {
    cli::cli_warn("Failed to compute PSR: {e$message}")
  })

  cli::cli_inform(paste0(
    "Loaded: fixtures=", nrow(fixtures), ", results=", nrow(results),
    ", teams=", nrow(teams), ", ratings=", nrow(torp_df)
  ))

  # Input validation
  if (nrow(fixtures) < 100) cli::cli_abort("Fixtures too small ({nrow(fixtures)} rows)")
  if (nrow(torp_df) < 100) cli::cli_abort("Ratings too small ({nrow(torp_df)} rows)")
  if (nrow(teams) < 100) cli::cli_abort("Teams too small ({nrow(teams)} rows)")

  cli::cli_h2("Building fixture features")
  fix_df <- .build_fixtures_df(fixtures)

  cli::cli_h2("Processing lineups")
  team_rt_df <- .build_team_ratings_df(teams, torp_df, psr_df)

  cli::cli_h2("Computing features")
  team_rt_fix_df <- .build_match_features(fix_df, team_rt_df, all_grounds)

  cli::cli_h2("Loading weather")
  weather_df <- .load_match_weather(fixtures, all_grounds, target_weeks, season)

  # Weight anchor: most recent fixture date
  weight_anchor_date <- if (!is.null(target_weeks) && !is.null(season)) {
    target_fix <- fixtures |>
      dplyr::filter(season == .env$season, round_number %in% target_weeks)
    if (nrow(target_fix) > 0) as.Date(min(target_fix$utc_start_time)) else Sys.Date()
  } else {
    max(as.Date(fix_df$utc_start_time), na.rm = TRUE)
  }
  cli::cli_inform("Weight anchor date: {weight_anchor_date}")

  cli::cli_h2("Building model dataset")
  team_mdl_df <- .build_team_mdl_df(
    team_rt_fix_df, results, xg_df, weather_df, weight_anchor_date
  )

  team_mdl_df
}


# get_lineup_ratings ----

#' Get player-level ratings for match lineups
#'
#' Returns individual player EPR, PSR, and TORP ratings for selected lineups,
#' with TOG-weighting applied (same logic as match predictions).
#'
#' @param season Season year (default: current via get_afl_season())
#' @param round Round number (default: next week via get_afl_week("next"))
#' @param match_id Optional match ID to filter to a single match
#' @return Tibble with one row per player, columns: season, round, match_id,
#'   team_name, player_name, lineup_position, position_group, epr, epr_recv,
#'   epr_disp, epr_spoil, epr_hitout, psr, torp (blended EPR+PSR)
#' @export
get_lineup_ratings <- function(season = NULL, round = NULL, match_id = NULL) {
  # Allow positional match_id: get_lineup_ratings("CD_M20260140601")
  if (!is.null(season) && is.character(season) && all(grepl("^CD_M", season))) {
    match_id <- season
    season <- NULL
  }
  if (!is.null(match_id) && any(!grepl("^CD_M\\d+$", match_id))) {
    cli::cli_warn("match_id values should start with {.val CD_M} followed by digits (e.g. {.val CD_M20260140601})")
  }
  if (is.null(season)) season <- get_afl_season()
  if (is.null(round) && is.null(match_id)) round <- get_afl_week("next")

  # Filter teams early to avoid processing all seasons
  if (!is.null(match_id)) {
    # Extract season from match_id (e.g. CD_M20260140601 → 2026)
    target_season <- as.integer(unique(substr(match_id, 5, 8)))
    teams <- load_teams(target_season)
  } else {
    teams <- load_teams(season)
  }
  torp_df <- load_torp_ratings()

  # Load PSR
  psr_df <- tryCatch({
    skills <- load_player_stat_ratings(TRUE)
    .compute_psr_from_stat_ratings(skills)
  }, error = function(e) {
    cli::cli_warn("Failed to compute PSR: {conditionMessage(e)}")
    NULL
  })

  torp_prior_total <- EPR_PRIOR_RATE_RECV + EPR_PRIOR_RATE_DISP +
    EPR_PRIOR_RATE_SPOIL + EPR_PRIOR_RATE_HITOUT

  # Drop PSR columns from torp_df to avoid collision
  torp_df <- torp_df |> dplyr::select(-dplyr::any_of(c("psr", "osr", "dsr")))

  lineup_df <- teams |>
    dplyr::left_join(
      torp_df,
      by = c("player_id" = "player_id", "season" = "season", "round_number" = "round")
    ) |>
    # Keep every named player bar emergencies -- see the note in
    # match_data_prep.R's .build_team_ratings_df(). These two filters must stay
    # in step: one builds the training frame and the other the serving frame,
    # so a divergence would be a silent train/serve skew.
    dplyr::filter(lineup_position != "EMERG" | is.na(lineup_position))

  # Impute missing EPR with priors
  lineup_df <- lineup_df |>
    dplyr::mutate(
      epr = tidyr::replace_na(epr, torp_prior_total),
      epr_recv = tidyr::replace_na(epr_recv, EPR_PRIOR_RATE_RECV),
      epr_disp = tidyr::replace_na(epr_disp, EPR_PRIOR_RATE_DISP),
      epr_spoil = tidyr::replace_na(epr_spoil, EPR_PRIOR_RATE_SPOIL),
      epr_hitout = tidyr::replace_na(epr_hitout, EPR_PRIOR_RATE_HITOUT),
      lineup_tog = tidyr::replace_na(POSITION_AVG_TOG[lineup_position], POSITION_AVG_TOG_DEFAULT),
      epr = epr * lineup_tog,
      epr_recv = epr_recv * lineup_tog,
      epr_disp = epr_disp * lineup_tog,
      epr_spoil = epr_spoil * lineup_tog,
      epr_hitout = epr_hitout * lineup_tog
    )

  # Join PSR (latest per player)
  if (!is.null(psr_df)) {
    has_osr_dsr <- all(c("osr", "dsr") %in% names(psr_df))
    latest_psr <- psr_df |>
      dplyr::select(dplyr::any_of(c("player_id", "season", "round", "psr", "osr", "dsr"))) |>
      dplyr::arrange(player_id, season, round) |>
      dplyr::group_by(player_id) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup() |>
      dplyr::select(-season, -round)

    lineup_df <- lineup_df |>
      dplyr::left_join(latest_psr, by = "player_id") |>
      dplyr::mutate(psr = tidyr::replace_na(psr, PSR_PRIOR_RATE) * lineup_tog)

    if (has_osr_dsr) {
      lineup_df <- lineup_df |>
        dplyr::mutate(
          osr = tidyr::replace_na(osr, PSR_PRIOR_RATE / 2) * lineup_tog,
          dsr = tidyr::replace_na(dsr, PSR_PRIOR_RATE / 2) * lineup_tog
        )
    }
  } else {
    lineup_df$psr <- PSR_PRIOR_RATE * lineup_df$lineup_tog
  }

  # Compute blended TORP
  lineup_df <- lineup_df |>
    dplyr::mutate(
      torp = TORP_EPR_WEIGHT * epr + (1 - TORP_EPR_WEIGHT) * psr,
      player_name = paste(given_name, surname)
    )

  # Filter to requested season/round/match
  if (!is.null(match_id)) {
    lineup_df <- lineup_df |> dplyr::filter(match_id %in% .env$match_id)
  } else {
    lineup_df <- lineup_df |>
      dplyr::filter(season == .env$season, round_number == .env$round)
  }

  if (nrow(lineup_df) == 0) {
    cli::cli_warn("No lineup data found for the requested filters")
    return(tibble::tibble())
  }

  select_cols <- c(
    "season", "round_number", "match_id", "team_name", "player_name",
    "lineup_position", "position_group", "lineup_tog",
    "epr", "epr_recv", "epr_disp", "epr_spoil", "epr_hitout",
    "psr", "torp"
  )
  if ("osr" %in% names(lineup_df)) select_cols <- c(select_cols, "osr", "dsr")

  lineup_df |>
    dplyr::select(dplyr::any_of(select_cols)) |>
    dplyr::arrange(match_id, team_name, dplyr::desc(torp))
}


# run_predictions_pipeline ----

#' Run weekly match predictions pipeline
#'
#' Average home/away rows into one match-level prediction
#'
#' Flips away-team predictions to the home-team perspective, then averages
#' home and (flipped) away predictions per match. Expects columns from
#' \code{.train_match_gams()} output (team_mdl_df).
#'
#' @param df Long-form team_mdl_df with both home and away rows per match
#' @return One-row-per-match tibble with averaged predictions from the home
#'   team perspective
#' @keywords internal
.format_match_preds <- function(df) {
  home <- df |>
    dplyr::filter(team_type_fac.x == "home") |>
    dplyr::select(
      season = season.x, round = round_number.x, players = count.x, match_id,
      home_team = team_name.x, home_epr = epr.x, home_psr = psr.x,
      away_team = team_name.y, away_epr = epr.y, away_psr = psr.y,
      pred_xtotal = pred_tot_xscore, pred_xmargin = pred_xscore_diff,
      pred_margin = pred_score_diff, pred_win, bits,
      margin = score_diff, start_time = local_start_time_str,
      utc_start_time, venue = venue.x
    )
  away <- df |>
    dplyr::mutate(
      pred_xscore_diff = -pred_xscore_diff,
      pred_score_diff = -pred_score_diff,
      pred_win = 1 - pred_win,
      score_diff = -score_diff
    ) |>
    dplyr::filter(team_type_fac.x == "away") |>
    dplyr::select(
      season = season.x, round = round_number.x, players = count.x, match_id,
      home_team = team_name.y, home_epr = epr.y, home_psr = psr.y,
      away_team = team_name.x, away_epr = epr.x, away_psr = psr.x,
      pred_xtotal = pred_tot_xscore, pred_xmargin = pred_xscore_diff,
      pred_margin = pred_score_diff, pred_win, bits,
      margin = score_diff, start_time = local_start_time_str,
      utc_start_time, venue = venue.x
    )
  dplyr::bind_rows(home, away) |>
    dplyr::group_by(season, round, match_id, home_team, home_epr, home_psr,
                    away_team, away_epr, away_psr, start_time, venue) |>
    dplyr::summarise(
      players = mean(players), pred_xtotal = mean(pred_xtotal),
      pred_margin = mean(pred_margin), pred_win = mean(pred_win),
      margin = mean(margin), .groups = "drop"
    ) |>
    dplyr::mutate(
      epr_diff = home_epr - away_epr,
      psr_diff = home_psr - away_psr,
      # team_name.x / team_name.y are factors (set in .build_team_mdl_df for
      # GAM categorical predictors). Coerce here so the predictions parquet
      # stores character columns -- factor home_team / away_team round-trip
      # through arrow and break torp_replace_teams() lookups in any caller
      # that doesn't go through .normalise_team_values(). Do NOT remove.
      home_team = as.character(home_team),
      away_team = as.character(away_team)
    ) |>
    dplyr::select(season, round, match_id:away_psr, start_time, venue,
                  epr_diff, psr_diff, players:margin)
}


#' Parse the AFL API's UTC start-time strings
#'
#' The API emits "2026-06-04T09:00:00.000+0000". Base R's default parse of an
#' ISO string containing "T" silently yields MIDNIGHT rather than erroring, so
#' the format has to be explicit or the comparison it feeds is quietly wrong.
#'
#' @param x Character vector of UTC start times.
#' @return POSIXct in UTC; NA where unparseable.
#' @keywords internal
.parse_utc_start <- function(x) {
  x <- as.character(x)
  # Strip fractional seconds and any trailing offset, then parse as UTC.
  cleaned <- sub("\\.[0-9]+", "", x)
  cleaned <- sub("([+-][0-9]{4}|Z)$", "", cleaned)
  cleaned <- trimws(sub("T", " ", cleaned))
  as.POSIXct(cleaned, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

#' Flag locked predictions that were computed after their game started
#'
#' The point of the `generated_utc` stamp: a locked prediction whose
#' computation time is after its own game start is a retrodiction sitting in
#' the locked-predictions release, where consumers reasonably assume everything
#' is as-at. Retrodictions have their own release; they do not belong here.
#'
#' Warns rather than aborts. The upload has already been judged safe by the
#' accumulate guard, the offending rows are historical fact by the time this
#' runs, and refusing to publish would discard genuinely-locked rows alongside
#' the bad ones. Loud enough to notice, cheap enough not to block a release.
#'
#' Rows with no stamp (published before stamping existed) are skipped -- absent
#' is not the same as post-hoc, and treating it as such would cry wolf over
#' every pre-2026-07-28 row forever.
#'
#' @param combined The prediction frame about to be uploaded.
#' @return Invisibly, the number of post-hoc rows found.
#' @keywords internal
.warn_post_hoc_predictions <- function(combined) {
  # Compare against utc_start_time, never start_time. The latter is LOCAL venue
  # time carrying a timezone abbreviation ("2026-06-04 19:00:00 ACST"), which
  # varies by venue (ACST/AEST/AWST, plus daylight saving) -- comparing a UTC
  # stamp against it is wrong by up to 11 hours. Worse, as.POSIXct() on an
  # ISO-with-T string silently parses to MIDNIGHT rather than failing, so a
  # naive version of this check flagged almost every legitimate prediction as
  # post-hoc. Both were caught only by testing against the real column values.
  if (!all(c("generated_utc", "utc_start_time") %in% names(combined))) return(invisible(0L))
  gen <- suppressWarnings(as.POSIXct(combined$generated_utc,
                                     format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  start <- suppressWarnings(.parse_utc_start(combined$utc_start_time))
  bad <- !is.na(gen) & !is.na(start) & gen > start
  n <- sum(bad)
  if (n > 0) {
    wk <- sort(unique(combined$week[bad]))
    cli::cli_warn(c(
      "{n} locked prediction{?s} {?was/were} computed AFTER {?its/their} game started (round{?s} {.val {as.character(wk)}}).",
      "!" = "Those rows are retrodictions in the locked-predictions release; consumers assume everything here is as-at.",
      "i" = "Check the started-game lock in .build_locked_predictions() -- it should have kept the original rows."
    ))
  }
  invisible(n)
}

#' Flag predictions about to be locked without real team lists
#'
#' `players` is `count.x` -- the number of named players aggregated per team.
#' When the AFL has not yet published team lists for a round, the lineup join
#' produces no rows, `players` is NA, and every player's EPR falls back to the
#' position prior. The prediction is still computed and still published; it is
#' just built on a generic squad rather than the actual 22.
#'
#' This went undetected for three rounds. Rounds 19, 20 and 21 of 2026 were all
#' locked with `players = NA` while rounds 13-18 carried 23, and nothing said so.
#' It surfaced only via a paired audit against Squiggle's record of our submitted
#' tips, weeks later: on rounds 19-20 the served predictions disagreed with a
#' correctly-fed model by a mean of 8.73 points per game (vs 4.15 on rounds where
#' lineups were present) and cost roughly 3.2 MAE.
#'
#' Warns rather than aborts, deliberately. A prior-based prediction beats no
#' prediction -- tips have a submission deadline -- and the started-game lock in
#' [.build_locked_predictions()] already lets a later run replace any match that
#' has not started. The failure mode worth preventing is silence, not publication.
#'
#' @param week_gms The new-week prediction rows about to be merged.
#' @return Invisibly, the number of rows lacking a lineup.
#' @keywords internal
.warn_missing_lineups <- function(week_gms) {
  if (!"players" %in% names(week_gms)) {
    # Say so rather than returning silently. This function exists because a
    # lineup failure went unnoticed for three rounds; a version of it that can
    # stop checking without saying so reintroduces the same blind spot in a new
    # place. `players` has no column_schema.R entry to catch its removal.
    cli::cli_inform("Lineup check skipped: no {.field players} column on the prediction frame.")
    return(invisible(0L))
  }
  if (nrow(week_gms) == 0) return(invisible(0L))

  # NA means the lineup join found nothing at all. But `players` is a COUNT
  # (count.x, named players per team), so a PARTIALLY published team sheet --
  # the AFL sometimes releases ins/outs before the full 22 -- yields a small
  # non-NA number instead. Checking only for NA would miss that, and it degrades
  # predictions the same way: most of the side still on position priors.
  # Published rounds 13-18 of 2026 all carried exactly 23.
  n_named <- suppressWarnings(as.numeric(week_gms$players))
  missing <- is.na(n_named)
  partial <- !missing & n_named < MIN_PLAUSIBLE_LINEUP

  if (any(missing)) {
    wk <- sort(unique(week_gms$week[missing]))
    cli::cli_warn(c(
      "{sum(missing)} prediction{?s} being locked with NO team list (round{?s} {.val {as.character(wk)}}).",
      "!" = "Every player fell back to the position prior, so these are squad-average predictions, not team-specific ones.",
      "i" = "Re-run once the AFL publishes team lists -- the started-game lock replaces any match that has not yet started."
    ))
  }
  if (any(partial)) {
    wk <- sort(unique(week_gms$week[partial]))
    cli::cli_warn(c(
      "{sum(partial)} prediction{?s} being locked with only PART of a team list (round{?s} {.val {as.character(wk)}}; fewest named: {min(n_named[partial])}).",
      "!" = "A full named side is {MIN_PLAUSIBLE_LINEUP}+; the unnamed remainder fell back to the position prior.",
      "i" = "Re-run once the full teams are published."
    ))
  }
  invisible(sum(missing) + sum(partial))
}

#' Merge new-week predictions into the locked full-season predictions file
#'
#' Encapsulates torp C2's overwrite guard (ECOSYSTEM-FIX-PLAN.md T3): a
#' transient read of the existing locked-predictions file (any error other
#' than a confirmed-absent 404) aborts rather than being treated as "no
#' existing file" -- that collapse is exactly what let one week's data
#' silently replace a full season of locked history. A fresh, non-accumulating
#' upload is only permitted after independently confirming via
#' [vb_confirm_absent()] that the release asset really is absent. An
#' accumulating merge is floor-guarded against a >10% shrink via
#' [vb_guard_accumulate()].
#'
#' @param pred_file_name Base file name (no extension), e.g. "predictions_2026".
#' @param season Season year.
#' @param week_gms This run's freshly-computed predictions for `target_weeks`.
#' @param team_mdl_df Full model data frame (used to find already-started matches).
#' @param target_weeks Numeric vector of round numbers being (re)computed.
#' @param completed_margins Data frame of match_id/.actual_margin for completed matches.
#' @return The combined data frame to upload.
#' @keywords internal
.build_locked_predictions <- function(pred_file_name, season, week_gms, team_mdl_df,
                                      target_weeks, completed_margins) {
  # Check the INCOMING rows, before the started-game filter below drops any.
  # A match that has already started keeps its locked prediction, so filtering
  # first would hide exactly the case worth reporting: a round locked without
  # team lists that is now beyond the point where a re-run could fix it.
  .warn_missing_lineups(week_gms)

  pred_repo <- get_torp_data_repo()
  existing <- tryCatch(
    file_reader(pred_file_name, "predictions"),
    vb_error_absent = function(e) NULL,
    error = function(e) {
      cli::cli_abort(c(
        "Could not load existing locked predictions for {season} ({conditionMessage(e)}).",
        "x" = "Refusing to upload -- this was not a confirmed-absent (404) error."
      ), parent = e)
    }
  )

  if (!is.null(existing) && nrow(existing) > 0) {
    # Backward compat: existing predictions may use old providerId column
    if (!"match_id" %in% names(existing) && "providerId" %in% names(existing)) {
      data.table::setnames(existing, "providerId", "match_id")
    }

    # Backward compat: merge home_rating/away_rating/rating_diff -> home_epr/away_epr/epr_diff
    old_to_new <- c(home_rating = "home_epr", away_rating = "away_epr", rating_diff = "epr_diff")
    for (old_nm in names(old_to_new)) {
      new_nm <- old_to_new[old_nm]
      if (old_nm %in% names(existing)) {
        if (new_nm %in% names(existing)) {
          existing[[new_nm]] <- dplyr::coalesce(existing[[new_nm]], existing[[old_nm]])
          existing[[old_nm]] <- NULL
        } else {
          names(existing)[names(existing) == old_nm] <- new_nm
        }
      }
    }

    n_backfilled <- sum(is.na(existing$margin) & existing$match_id %in% completed_margins$match_id)
    if (n_backfilled > 0) {
      existing <- existing |>
        dplyr::left_join(completed_margins, by = "match_id") |>
        dplyr::mutate(margin = dplyr::coalesce(margin, .actual_margin)) |>
        dplyr::select(-.actual_margin)
      cli::cli_alert_success("Backfilled {n_backfilled} match margin{?s} from results")
    }

    # Only replace predictions for games that haven't started yet
    started_ids <- team_mdl_df |>
      dplyr::filter(
        season.x == season,
        round_number.x %in% target_weeks,
        team_type_fac.x == "home",
        utc_dt <= Sys.time()
      ) |>
      dplyr::pull(match_id)

    week_gms <- week_gms |>
      dplyr::filter(!match_id %in% started_ids)

    if (length(started_ids) > 0) {
      cli::cli_alert_info("Keeping locked predictions for {length(started_ids)} already-started match{?es}")
    }

    # Rows not being replaced keep whatever generated_utc they already carry;
    # bind_rows fills NA for pre-stamp history, which is honest -- those rows
    # genuinely have no recorded computation time.
    if (!"generated_utc" %in% names(existing)) existing$generated_utc <- NA_character_

    combined <- existing |>
      dplyr::ungroup() |>
      dplyr::filter(!match_id %in% week_gms$match_id) |>
      dplyr::bind_rows(dplyr::ungroup(week_gms)) |>
      dplyr::arrange(week)

    vb_guard_accumulate(existing, combined, floor = 0.9)
    .warn_post_hoc_predictions(combined)
  } else {
    # existing is NULL only when file_reader() confirmed absence (404) above,
    # or the file genuinely has 0 rows (loaded fine). Independently confirm
    # the release asset really is absent before doing a fresh, non-accumulating
    # upload -- this is the mandatory guard before any "start fresh" branch.
    is_absent <- tryCatch(
      vb_confirm_absent(pred_repo, "predictions", paste0(pred_file_name, ".parquet")),
      error = function(e) {
        cli::cli_abort("Could not verify {.val {pred_file_name}.parquet} is absent from the predictions release before a fresh upload: {conditionMessage(e)}")
      }
    )
    if (!isTRUE(is_absent)) {
      cli::cli_abort(c(
        "Refusing fresh upload of {.val {pred_file_name}}: the file is present on the predictions release but was not readable as non-empty.",
        "x" = "This should not happen without a prior error -- aborting rather than risk overwriting locked history."
      ))
    }
    combined <- week_gms
  }

  combined
}


#' Build everything a match prediction needs, and publish nothing
#'
#' The whole of `run_predictions_pipeline()` except the uploads: loads data,
#' builds fixture/rating/injury features, trains the GAM + XGBoost chain,
#' generates and validates predictions, and hands the lot back.
#'
#' Split out 2026-08-11. `run_predictions_pipeline()` bundled an upload it
#' could not opt out of, and that single fact caused three separate problems:
#' `build_matchup_table()` re-implemented this sequence rather than call it
#' (see `matchup_table.R`'s own header), the orchestration had no test
#' coverage because exercising it published, and the margin-calibration
#' sidecar scored its own copy of the blend. Callers that want the state
#' without the side effects now have a seam to use.
#'
#' @param week Single target week (auto-detected if NULL)
#' @param weeks Vector of weeks, or "all" for all fixture weeks
#' @param season Season year (default: current via get_afl_season())
#' @param refresh_results If TRUE (production default), refresh the season's
#'   results from the AFL API and publish them to the `results-data` release
#'   before building. This is the ONE side effect inside the state half; pass
#'   FALSE for a read-only build. **Note what FALSE costs:** `results` feeds
#'   `.build_team_mdl_df()` and therefore GAM training, so a read-only build
#'   trains on whatever the release already held, with nothing at runtime
#'   saying it was stale. Fine for a same-day dry run, wrong for anything
#'   whose numbers get compared against production.
#' @return A list with `season`, `target_weeks`, `is_backfill`, `all_preds`,
#'   `week_gms`, `team_mdl_df`, `gam_result`, `xgb_result`,
#'   `validation_errors` and `pipeline_start` — **or `NULL`** when there is
#'   nothing to predict (no TORP ratings for the target week yet: pre-season,
#'   or fixtures not published). Callers MUST handle the `NULL`;
#'   `NULL$anything` is `NULL` in R, so an unchecked caller reads every field
#'   as empty and fails much later somewhere unrelated.
#'
#'   `validation_errors` is non-empty only on an interactive run; a
#'   non-interactive run aborts instead, exactly as the pipeline did before
#'   the split.
#' @keywords internal
build_prediction_state <- function(week = NULL, weeks = NULL, season = NULL,
                                   refresh_results = TRUE) {

  # Assembled in two places (early return on validation failure, and the
  # normal path), so it is built once here to keep them identical.
  .prediction_state <- function(early = FALSE) {
    list(
      season            = season,
      target_weeks      = target_weeks,
      is_backfill       = is_backfill,
      all_preds         = all_preds,
      week_gms          = week_gms,
      team_mdl_df       = team_mdl_df,
      gam_result        = gam_result,
      xgb_result        = xgb_result,
      validation_errors = validation_errors,
      pipeline_start    = .pipeline_start
    )
  }


  if (is.null(season)) season <- get_afl_season()

  if (!is.null(week) && !is.null(weeks)) {
    cli::cli_abort("Specify either {.arg week} or {.arg weeks}, not both")
  }
  if (is.null(week) && is.null(weeks)) week <- get_afl_week(type = "next")

  cli::cli_h1("Match Predictions Pipeline")
  .pipeline_start <- proc.time()

  # Load Data ----
  cli::cli_h2("Loading data")

  all_grounds <- file_reader("stadium_data", "reference-data")
  xg_df <- load_xg(TRUE)
  fixtures <- load_fixtures(TRUE)
  results <- load_results(TRUE)

  # Refresh current season results from AFL API.
  # cli_progress_done() is REQUIRED here: cli progress steps auto-close only
  # when a sibling progress_step starts in the same frame OR the frame exits.
  # The match_train.R progress_steps live in nested function frames, so they
  # don't supersede this one -- without an explicit done(), this spinner stays
  # alive for the entire pipeline and its line state interleaves with later
  # cli_inform() prints (you see "from AFL API" smeared into other messages).
  if (isTRUE(refresh_results)) tryCatch({
    cli::cli_progress_step("Refreshing {season} results from AFL API")
    fresh_results <- get_afl_results(season)
    if (!is.null(fresh_results) && nrow(fresh_results) > 0) {
      save_to_release(fresh_results, paste0("results_", season), "results-data")
      results <- load_results(TRUE)
      cli::cli_inform("Refreshed results: {nrow(fresh_results)} rows for {season}")
    }
    cli::cli_progress_done()
  }, error = function(e) {
    cli::cli_warn("Could not refresh {season} results: {e$message}")
  })

  teams <- load_teams(TRUE)

  # Load TORP ratings with compute-from-scratch fallback
  torp_df_total <- tryCatch(load_torp_ratings(), error = function(e) {
    cli::cli_warn("Could not load TORP ratings from release: {e$message}")
    NULL
  })
  if (is.null(torp_df_total) || nrow(torp_df_total) < 100) {
    cli::cli_warn("TORP ratings unavailable or too small from release - computing from scratch (this may be slow)")
    torp_df_total <- tryCatch(
      calculate_epr(season_val = season, round_val = get_afl_week(type = "next")),
      error = function(e) {
        cli::cli_abort("Failed to compute TORP ratings from scratch: {e$message}")
      }
    )
  }

  cli::cli_inform("Loaded: fixtures={nrow(fixtures)}, results={nrow(results)}, teams={nrow(teams)}, ratings={nrow(torp_df_total)}")

  if (nrow(fixtures) < 100) cli::cli_abort("Fixtures too small ({nrow(fixtures)} rows)")
  if (nrow(torp_df_total) < 100) cli::cli_abort("Ratings too small ({nrow(torp_df_total)} rows)")
  if (nrow(teams) < 100) cli::cli_abort("Teams too small ({nrow(teams)} rows)")

  # Resolve target weeks
  is_backfill <- identical(weeks, "all")
  if (!is.null(weeks)) {
    if (is_backfill) {
      target_weeks <- sort(unique(fixtures$round_number[fixtures$season == season]))
    } else {
      target_weeks <- weeks
    }
  } else {
    target_weeks <- week
  }
  cli::cli_inform("Season: {season}, Week{?s}: {paste(target_weeks, collapse = ', ')}")

  # Weight anchor date (deterministic)
  target_fixtures <- fixtures |>
    dplyr::filter(season == .env$season, round_number %in% target_weeks)
  weight_anchor_date <- if (nrow(target_fixtures) > 0) {
    as.Date(min(target_fixtures$utc_start_time))
  } else {
    Sys.Date()
  }
  cli::cli_inform("Weight anchor date: {weight_anchor_date}")

  # Build Shared Data ----
  cli::cli_h2("Building fixture features")
  fix_df <- .build_fixtures_df(fixtures)

  cli::cli_h2("Loading PSR")
  psr_df <- NULL

  # Strategy 1: Load pre-computed PSR from torpdata releases

  tryCatch({
    psr_df <- load_psr(TRUE)
    if (nrow(psr_df) > 0) {
      cli::cli_inform("PSR loaded from release: {nrow(psr_df)} player-rounds")
    } else {
      psr_df <- NULL
    }
  }, error = function(e) {
    cli::cli_warn("Could not load PSR from release: {e$message}")
  })

  # Strategy 2: Compute from skills + coefficients (with osr/dsr decomposition)
  # Also triggered when release PSR lacks osr/dsr columns
  if (is.null(psr_df) || !all(c("osr", "dsr") %in% names(psr_df))) {
    tryCatch({
      skills <- load_player_stat_ratings(TRUE)
      psr_df <- .compute_psr_from_stat_ratings(skills)
      if (!is.null(psr_df)) {
        cli::cli_inform("PSR computed from skills+coefficients: {nrow(psr_df)} player-rounds")
      }
    }, error = function(e) {
      cli::cli_warn("Failed to compute PSR: {e$message}")
    })
  }

  if (is.null(psr_df)) {
    cli::cli_warn("PSR unavailable - predictions will proceed without PSR features")
  }

  cli::cli_h2("Processing lineups")
  team_rt_df <- .build_team_ratings_df(teams, torp_df_total, psr_df)

  cli::cli_h2("Computing features")
  team_rt_fix_df <- .build_match_features(fix_df, team_rt_df, all_grounds)

  # Injuries ----
  cli::cli_h2("Loading injuries")
  inj_df <- get_all_injuries(season)
  cli::cli_inform("Injuries loaded: {nrow(inj_df)} rows ({sum(inj_df$source == 'weekly', na.rm = TRUE)} weekly, {sum(inj_df$source == 'preseason', na.rm = TRUE)} preseason)")
  if (nrow(inj_df) == 0 && min(target_weeks) > 1) {
    cli::cli_warn("0 injuries loaded during active season - all players will be treated as available")
  }

  # Parse return rounds and save injury snapshot to torpdata release
  if (nrow(inj_df) > 0) {
    inj_df$return_round <- parse_return_round(
      inj_df$estimated_return, season, min(target_weeks)
    )
    tryCatch(
      save_injury_data(inj_df, season),
      error = function(e) cli::cli_warn("Failed to save injury data: {conditionMessage(e)}")
    )
  }

  tr <- torp_ratings(season, min(target_weeks))
  if (nrow(tr) == 0 || !"player_name" %in% names(tr)) {
    cli::cli_alert_info("No TORP ratings available for {season} R{min(target_weeks)} (pre-season or fixtures not ready) - skipping predictions")
    elapsed <- (proc.time() - .pipeline_start)[["elapsed"]]
    cli::cli_inform("Pipeline aborted after {round(elapsed, 1)}s")
    return(invisible(NULL))
  }
  # torp_ratings() already joins injuries -- drop those columns before re-joining
  # with the pipeline's own injury data (which includes return_round parsing)
  tr[c("injury", "estimated_return")] <- NULL
  tr <- match_injuries(tr, inj_df)
  if (!"estimated_return" %in% names(tr)) tr$estimated_return <- NA_character_
  tr <- dplyr::mutate(tr, estimated_return = tidyr::replace_na(estimated_return, "None"))

  # Join PSR to player-level ratings for injury-adjusted weekly summary
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

  # Join return_round from injury data to player ratings
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

  # Per-week roster ratings -- see .build_week_ratings() in match_data_prep.R
  # for the injury/discount/TOG rules. build_matchup_table() calls the same
  # function, so the two cannot drift.
  tr_week <- purrr::map_dfr(
    target_weeks, function(w) .build_week_ratings(tr, w, target_weeks)
  ) |>
    dplyr::arrange(-epr_week)

  # Overlay injury-adjusted ratings
  # When real lineups exist (count > 0), keep lineup-based ratings.
  # Otherwise use roster + pred_tog + injury-adjusted ratings (epr_week).
  team_rt_fix_df <- team_rt_fix_df |>
    dplyr::left_join(tr_week, by = c("team_name" = "team_name", "season" = "season", "round_number" = "round")) |>
    dplyr::mutate(
      use_roster = !is.na(epr_week) & (is.na(count) | count == 0),
      epr = dplyr::if_else(use_roster, epr_week, epr),
      epr_recv = dplyr::if_else(use_roster, epr_recv_week, epr_recv),
      epr_disp = dplyr::if_else(use_roster, epr_disp_week, epr_disp),
      epr_spoil = dplyr::if_else(use_roster, epr_spoil_week, epr_spoil),
      epr_hitout = dplyr::if_else(use_roster, epr_hitout_week, epr_hitout),
      psr = dplyr::if_else(use_roster, psr_week, psr),
      use_roster = NULL
    )

  # Warn if any prediction-week teams still have NA ratings
  pred_rows <- team_rt_fix_df |>
    dplyr::filter(season == .env$season, round_number %in% target_weeks)
  na_epr <- pred_rows |> dplyr::filter(is.na(epr))
  if (nrow(na_epr) > 0) {
    na_teams <- unique(na_epr$team_name)
    cli::cli_warn("Missing EPR for {length(na_teams)} team{?s} in prediction weeks: {paste(na_teams, collapse = ', ')}")
  }

  # Weather ----
  cli::cli_h2("Loading weather")
  weather_df <- .load_match_weather(fixtures, all_grounds, target_weeks, season)

  # Build Model Dataset ----
  cli::cli_h2("Building model dataset")
  team_mdl_df <- .build_team_mdl_df(team_rt_fix_df, results, xg_df, weather_df, weight_anchor_date)

  # Train GAMs & Predict ----
  cli::cli_h2("Training GAM models on completed matches")
  gam_result <- .train_match_gams(team_mdl_df)
  team_mdl_df <- gam_result$data

  # Train XGBoost & Blend ----
  xgb_result <- tryCatch({
    cli::cli_h2("Training XGBoost models")
    res <- .train_match_xgb(team_mdl_df)
    team_mdl_df <- res$data
    res
  }, error = function(e) {
    cli::cli_warn("XGBoost training failed ({conditionMessage(e)}), using GAM-only predictions")
    NULL
  })

  # Blend GAM + XGBoost if XGBoost succeeded. The bare pred_* columns hold
  # the final values used downstream; gam_pred_* and xgb_pred_* remain on
  # team_mdl_df for inspection. pred_win is re-derived by feeding the blended
  # margin back through the GAM win model (xgb_pred_win is intentionally not
  # used). Kept outside tryCatch so GAM predict failures are not misattributed
  # to XGBoost and data is never left in a half-blended state.
  if (!is.null(xgb_result)) {
    team_mdl_df$pred_tot_xscore  <- .blend_gam_xgb(
      team_mdl_df$gam_pred_tot_xscore,  team_mdl_df$xgb_pred_tot_xscore
    )
    team_mdl_df$pred_xscore_diff <- .blend_gam_xgb(
      team_mdl_df$gam_pred_xscore_diff, team_mdl_df$xgb_pred_xscore_diff
    )
    team_mdl_df$pred_score_diff  <- .blend_gam_xgb(
      team_mdl_df$gam_pred_score_diff,  team_mdl_df$xgb_pred_score_diff
    )
    team_mdl_df$pred_win <- predict(
      gam_result$models$win, newdata = team_mdl_df, type = "response"
    )
    cli::cli_alert_success("Blended GAM + XGBoost inputs, derived WP from GAM model")
  } else {
    team_mdl_df$pred_tot_xscore  <- team_mdl_df$gam_pred_tot_xscore
    team_mdl_df$pred_xscore_diff <- team_mdl_df$gam_pred_xscore_diff
    team_mdl_df$pred_score_diff  <- team_mdl_df$gam_pred_score_diff
    team_mdl_df$pred_win         <- team_mdl_df$gam_pred_win
  }

  # Margin recalibration (2026-07, FABLE-MATCH-MAE-PLAN.md WS1 "V1a"):
  # applied to the FINAL served margin only, AFTER pred_win has already been
  # derived from the raw (uncalibrated) blended margin above -- this exactly
  # mirrors how the "C6" candidate was validated (recalibration never fed
  # back into win-probability). Identity fallback (scale=1) when the sidecar
  # is absent -- see match_calibration.R.
  margin_calib <- load_match_margin_calibration()
  team_mdl_df$pred_score_diff <- apply_match_margin_calibration(team_mdl_df$pred_score_diff, margin_calib)

  # Format Predictions ----
  cli::cli_h2("Generating predictions for {length(target_weeks)} week{?s}")

  # All matches (for analysis)
  all_preds <- .format_match_preds(team_mdl_df)

  # Target week predictions (for upload)
  week_gms <- all_preds |>
    dplyr::filter(season == .env$season, round %in% target_weeks) |>
    dplyr::select(-season)

  # Validate ----
  cli::cli_h2("Validating predictions")
  validation_errors <- character(0)

  if (nrow(week_gms) == 0) cli::cli_abort("No predictions generated for week{?s} {paste(target_weeks, collapse = ', ')}")
  if (any(is.na(week_gms$pred_win))) validation_errors <- c(validation_errors, "NA values in pred_win")
  if (any(week_gms$pred_win < 0 | week_gms$pred_win > 1, na.rm = TRUE)) validation_errors <- c(validation_errors, "pred_win values out of [0,1] range")
  if (any(is.na(week_gms$pred_margin))) validation_errors <- c(validation_errors, "NA values in pred_margin")

  # Margin and win probability must agree in direction
  margin_sign <- sign(week_gms$pred_margin)
  win_sign <- sign(week_gms$pred_win - 0.5)
  # Exclude near-zero margins (< 1 point) and near-50/50 win probs where sign can legitimately differ
  meaningful <- abs(week_gms$pred_margin) > 1 & abs(week_gms$pred_win - 0.5) > 0.02
  disagreements <- meaningful & margin_sign != win_sign
  if (any(disagreements, na.rm = TRUE)) {
    bad <- week_gms[disagreements, ]
    validation_errors <- c(validation_errors, paste0(
      "Margin/win probability direction disagreement for ", sum(disagreements), " match(es). ",
      "e.g. ", bad$home_team[1], " vs ", bad$away_team[1],
      " (", season, " R", bad$round[1], ", ", bad$match_id[1], ")",
      ": margin=", round(bad$pred_margin[1], 1), ", win=", round(bad$pred_win[1], 3)
    ))
  }

  # Total expected score should be in a plausible range (100-250 points)
  if (any(week_gms$pred_xtotal < 100 | week_gms$pred_xtotal > 250, na.rm = TRUE)) {
    bad_xt <- week_gms[week_gms$pred_xtotal < 100 | week_gms$pred_xtotal > 250, ]
    validation_errors <- c(validation_errors, paste0(
      "Implausible pred_xtotal for ", nrow(bad_xt), " match(es) outside 100-250 range. ",
      "e.g. ", bad_xt$home_team[1], " vs ", bad_xt$away_team[1],
      " (", season, " R", bad_xt$round[1], ", ", bad_xt$match_id[1], ")",
      ": pred_xtotal=", round(bad_xt$pred_xtotal[1], 1)
    ))
  }

  if (length(validation_errors) > 0) {
    if (interactive()) {
      cli::cli_warn(c("Prediction validation failed ({length(validation_errors)} issue{?s}):", validation_errors))
      cli::cli_alert_info("Returning models and data for debugging (predictions NOT uploaded)")
      return(.prediction_state(early = TRUE))
    } else {
      cli::cli_abort(c("Prediction validation failed ({length(validation_errors)} issue{?s}):", validation_errors))
    }
  }

  cli::cli_alert_success("Validation passed: {nrow(week_gms)} matches")

  .prediction_state()
}


#' Builds team_mdl_df with injury-adjusted ratings, trains the 5-model sequential
#' GAM pipeline, generates predictions for target weeks, and uploads to torpdata
#' releases.
#'
#' @param week Single target week (auto-detected if NULL)
#' @param weeks Vector of weeks, or "all" for all fixture weeks
#' @param season Season year (default: current via get_afl_season())
#' @return A list (invisibly) with:
#'   \item{predictions}{All match predictions across all seasons (season, round,
#'     providerId, home_team, away_team, pred_margin, pred_win, margin, etc.)}
#'   \item{models}{Named list of 5 GAM models: total_xpoints, xscore_diff,
#'     conv_diff, score_diff, win}
#' @keywords internal
run_predictions_pipeline <- function(week = NULL, weeks = NULL, season = NULL) {

  state <- build_prediction_state(week = week, weeks = weeks, season = season)

  # NULL means there was nothing to predict -- no TORP ratings for the target
  # week yet (pre-season, or fixtures not published). Before the state/upload
  # split this was a bare `return(invisible(NULL))` from the middle of one
  # function; now it has to cross a seam, and without this check `state$x`
  # silently yields NULL for every field (NULL$anything is NULL, and
  # length(NULL) == 0 slips past the validation gate below) until the upload
  # half dies on `NULL |> dplyr::ungroup()` -- an unhandled crash in place of a
  # clean, expected no-op, with the real cause already scrolled past.
  if (is.null(state)) return(invisible(NULL))

  season         <- state$season
  target_weeks   <- state$target_weeks
  is_backfill    <- state$is_backfill
  all_preds      <- state$all_preds
  week_gms       <- state$week_gms
  team_mdl_df    <- state$team_mdl_df
  gam_result     <- state$gam_result
  xgb_result     <- state$xgb_result
  .pipeline_start <- state$pipeline_start

  # Interactive validation failure: build_prediction_state() returns rather
  # than aborting (a non-interactive run has already aborted inside it), and
  # the caller reports and bails without uploading -- same shape and same
  # order of output as before the split.
  if (length(state$validation_errors) > 0) {
    cli::cli_warn(c("Prediction validation failed ({length(state$validation_errors)} issue{?s}):",
                    state$validation_errors))
    cli::cli_alert_info("Returning models and data for debugging (predictions NOT uploaded)")
    return(invisible(list(
      predictions = all_preds,
      gam_models = gam_result$models,
      xgb_models = if (!is.null(xgb_result)) xgb_result$models else NULL,
      model_data = team_mdl_df,
      validation_errors = state$validation_errors
    )))
  }


  # Upload ----
  cli::cli_h2("Uploading predictions")
  week_gms <- week_gms |> dplyr::ungroup() |> dplyr::rename(week = round) |> dplyr::relocate(week)

  # Stamp when each prediction was actually computed.
  #
  # Without this, "was this row genuinely predicted before the game?" is not
  # answerable from the artifact -- it has to be reconstructed by comparing
  # against Squiggle's submitted tips, which is how three rounds of
  # stored-vs-submitted divergence (13, 19, 20) ended up an open forensic
  # question. With it, the check is `generated_utc < start_time`.
  #
  # Rows carried over from a previous run KEEP their original stamp (see
  # .build_locked_predictions()) -- that is the point. A stamp that updated on
  # every write would record when the file was last touched, not when the
  # prediction was made, and would be worthless for exactly this question.
  week_gms$generated_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  # --- Locked predictions: frozen at game start, never overwritten ---
  pred_file_name <- paste0("predictions_", season)

  # Actual margins from completed matches (shared by locked preds + retrodictions)
  completed_margins <- team_mdl_df |>
    dplyr::filter(!is.na(score_diff), team_type_fac.x == "home") |>
    dplyr::distinct(match_id, .keep_all = TRUE) |>
    dplyr::transmute(match_id, .actual_margin = score_diff)

  combined <- .build_locked_predictions(
    pred_file_name, season, week_gms, team_mdl_df, target_weeks, completed_margins
  )

  tryCatch(
    {
      save_to_release(combined, pred_file_name, "predictions", also_csv = TRUE)
      cli::cli_alert_success("Uploaded locked predictions ({nrow(combined)} rows, week{?s} {paste(target_weeks, collapse = ', ')} updated)")
    },
    error = function(e) {
      local_path <- file.path("data-raw", paste0(pred_file_name, ".parquet"))
      arrow::write_parquet(combined, local_path)
      # cli_alert_danger FIRST, because it prints immediately. R defers warnings
      # to the end of the script, and on 2026-07-29 the job was killed by its
      # timeout one second after the pipeline finished -- so this warning never
      # printed and a predictions/CSV divergence that fed Squiggle stale tips
      # left no trace anywhere in the log.
      cli::cli_alert_danger(
        "FAILED to upload locked predictions for {pred_file_name}: {conditionMessage(e)}")
      cli::cli_alert_danger(
        "squiggle.com.au may now be serving the PREVIOUS round's tips. Verify predictions_<season>.csv and .parquet timestamps agree before first bounce.")
      cli::cli_warn(c(
        "Failed to upload locked predictions: {conditionMessage(e)}",
        "i" = "Saved locally to {local_path}",
        "i" = "Check WORKFLOW_PAT if this is a permissions issue"
      ))
    }
  )

  # --- Retrodictions: current model on all matches, fully overwritten each run ---
  retro_all <- all_preds |>
    dplyr::rename(week = round) |>
    dplyr::relocate(week)

  if (nrow(completed_margins) > 0) {
    retro_all <- retro_all |>
      dplyr::left_join(completed_margins, by = "match_id") |>
      dplyr::mutate(margin = dplyr::coalesce(.actual_margin, margin)) |>
      dplyr::select(-.actual_margin)
  }

  # Daily runs: current season only. Full backfill when weeks = "all"
  retro_seasons <- if (is_backfill) sort(unique(retro_all$season)) else season
  retro_failures <- 0L
  for (retro_s in retro_seasons) {
    retro_preds <- retro_all |> dplyr::filter(season == retro_s)
    if (nrow(retro_preds) == 0) {
      cli::cli_warn("Skipping retrodictions_{retro_s}: 0 rows")
      next
    }
    retro_file_name <- paste0("retrodictions_", retro_s)
    tryCatch(
      {
        save_to_release(retro_preds, retro_file_name, "retrodictions", also_csv = TRUE)
        cli::cli_alert_success("Uploaded retrodictions_{retro_s} ({nrow(retro_preds)} rows)")
      },
      error = function(e) {
        retro_failures <<- retro_failures + 1L
        local_path <- file.path("data-raw", paste0(retro_file_name, ".parquet"))
        arrow::write_parquet(retro_preds, local_path)
        cli::cli_warn(c(
          "Failed to upload retrodictions_{retro_s}: {conditionMessage(e)}",
          "i" = "Saved locally to {local_path}"
        ))
      }
    )
  }
  if (retro_failures > 0) {
    cli::cli_warn("Retrodictions: {retro_failures}/{length(retro_seasons)} season(s) failed to upload")
  } else {
    cli::cli_alert_success("Retrodictions uploaded for {length(retro_seasons)} season{?s}")
  }

  # Upload models to torpmodels ----
  tryCatch(
    {
      cache_dir <- getOption("torpmodels.cache_dir",
                             file.path(tools::R_user_dir("torpmodels", "cache"), "models"))

      # torpmodels is a Suggests, not a hard dependency (DESCRIPTION:48) --
      # stamp provenance + update the manifest when available, but never let
      # its absence break the daily run: fall back to an unstamped upload.
      has_torpmodels <- requireNamespace("torpmodels", quietly = TRUE)
      if (!has_torpmodels) {
        cli::cli_warn("torpmodels not available -- uploading match_gams/match_xgb_pipeline without a provenance stamp")
      }
      match_seasons <- sort(unique(team_mdl_df$season.x))
      match_seasons_range <- if (length(match_seasons) > 0) paste(range(match_seasons), collapse = "-") else NA_character_
      uploaded_files <- character(0)

      gam_path <- file.path(tempdir(), "match_gams.rds")
      t0 <- proc.time()[["elapsed"]]
      match_gams_out <- .strip_gam_models(gam_result$models)
      if (has_torpmodels) {
        gam_meta <- torpmodels:::build_model_meta(
          "match_gams", match_seasons_range, list(), NA_character_,
          n_matches = nrow(team_mdl_df) / 2,
          extra = list(script = "run_predictions_pipeline")
        )
        match_gams_out <- torpmodels:::stamp_model_meta(match_gams_out, gam_meta)
      }
      saveRDS(match_gams_out, gam_path)
      t1 <- proc.time()[["elapsed"]]
      gam_size <- file.size(gam_path) / 1e6
      cli::cli_alert_info("match_gams saveRDS: {round(t1 - t0, 1)}s ({round(gam_size, 1)} MB)")
      piggyback::pb_upload(gam_path, repo = "peteowen1/torpmodels", tag = "core-models")
      t2 <- proc.time()[["elapsed"]]
      cli::cli_alert_info("match_gams pb_upload: {round(t2 - t1, 1)}s")
      local_cache <- file.path(cache_dir, "core", "match_gams.rds")
      if (dir.exists(dirname(local_cache))) {
        file.copy(gam_path, local_cache, overwrite = TRUE)
      }
      cli::cli_alert_success("Uploaded match_gams to torpmodels")
      uploaded_files <- c(uploaded_files, "match_gams.rds")

      if (!is.null(xgb_result)) {
        xgb_path <- file.path(tempdir(), "match_xgb_pipeline.rds")
        t3 <- proc.time()[["elapsed"]]
        match_xgb_out <- xgb_result$models
        if (has_torpmodels) {
          xgb_meta <- torpmodels:::build_model_meta(
            "match_xgb_pipeline", match_seasons_range, list(), NA_character_,
            n_matches = nrow(team_mdl_df) / 2,
            extra = list(script = "run_predictions_pipeline")
          )
          match_xgb_out <- torpmodels:::stamp_model_meta(match_xgb_out, xgb_meta)
        }
        saveRDS(match_xgb_out, xgb_path)
        t4 <- proc.time()[["elapsed"]]
        xgb_size <- file.size(xgb_path) / 1e6
        cli::cli_alert_info("match_xgb saveRDS: {round(t4 - t3, 1)}s ({round(xgb_size, 1)} MB)")
        piggyback::pb_upload(xgb_path, repo = "peteowen1/torpmodels", tag = "core-models")
        t5 <- proc.time()[["elapsed"]]
        cli::cli_alert_info("match_xgb pb_upload: {round(t5 - t4, 1)}s")
        local_cache_xgb <- file.path(cache_dir, "core", "match_xgb_pipeline.rds")
        if (dir.exists(dirname(local_cache_xgb))) {
          file.copy(xgb_path, local_cache_xgb, overwrite = TRUE)
        }
        cli::cli_alert_success("Uploaded match_xgb_pipeline to torpmodels")
        uploaded_files <- c(uploaded_files, "match_xgb_pipeline.rds")
      }

      # Margin calibration sidecar (2026-07, FABLE-MATCH-MAE-PLAN.md WS1) --
      # fit fresh each retrain via a temporal holdout. Release gate (mirrors
      # the WP temporal slope gate): a raw OOS slope outside
      # MATCH_MARGIN_SLOPE_GATE, OR a cold-start identity fit (calib_fit$b
      # forced to 1 because fit_match_margin_calibration()'s n_oos fell below
      # MATCH_RECAL_MIN_N -- a coincidentally in-range slope_raw must NOT
      # count as a pass in that case), skips uploading a new sidecar (keeps
      # whatever was previously published, or identity if none exists yet)
      # rather than aborting the whole pipeline -- weekly predictions still
      # need to ship even when this diagnostic signal is off; graceful
      # degradation matches this function's existing house style (xgb
      # failure -> GAM-only, PSR failure -> warn+continue, etc.), not a
      # hard-fail research gate.
      calib_fit <- tryCatch(
        fit_match_margin_calibration(team_mdl_df),
        error = function(e) {
          cli::cli_warn("Margin calibration fit failed: {conditionMessage(e)}")
          NULL
        }
      )
      if (!is.null(calib_fit)) {
        gate <- MATCH_MARGIN_SLOPE_GATE
        slope_ok <- is.na(calib_fit$slope_raw) || (calib_fit$slope_raw >= gate[1] && calib_fit$slope_raw <= gate[2])
        if (isTRUE(calib_fit$cold_start)) {
          cli::cli_warn(c(
            "Margin calibration cold start: only {calib_fit$n_oos} OOS holdout matches (< {MATCH_RECAL_MIN_N}) -- b was forced to identity.",
            "i" = "Skipping match_margin_calibration upload this run -- serving will fall back to the previously published sidecar (or identity if none exists)."
          ))
        } else if (!slope_ok) {
          cli::cli_warn(c(
            "Margin calibration slope gate breached: raw OOS slope {round(calib_fit$slope_raw, 3)} outside [{gate[1]}, {gate[2]}].",
            "i" = "Skipping match_margin_calibration upload this run -- serving will fall back to the previously published sidecar (or identity if none exists)."
          ))
        } else {
          calib_path <- file.path(tempdir(), "match_margin_calibration.rds")
          calib_out <- calib_fit
          if (has_torpmodels) {
            calib_meta <- torpmodels:::build_model_meta(
              "match_margin_calibration", match_seasons_range, list(), NA_character_,
              n_matches = calib_fit$n_oos,
              extra = list(script = "run_predictions_pipeline", holdout_season = calib_fit$holdout_season)
            )
            calib_out <- torpmodels:::stamp_model_meta(calib_out, calib_meta)
          }
          saveRDS(calib_out, calib_path)
          piggyback::pb_upload(calib_path, repo = "peteowen1/torpmodels", tag = "core-models")
          local_cache_calib <- file.path(cache_dir, "core", "match_margin_calibration.rds")
          if (dir.exists(dirname(local_cache_calib))) {
            file.copy(calib_path, local_cache_calib, overwrite = TRUE)
          }
          cli::cli_alert_success("Uploaded match_margin_calibration to torpmodels (b={round(calib_fit$b, 3)})")
          uploaded_files <- c(uploaded_files, "match_margin_calibration.rds")
          # Invalidate this session's cached sidecar so the recalibrated
          # margin above (already computed with the PREVIOUS sidecar, or
          # identity) doesn't silently diverge from what just got published --
          # next call in this session picks up the fresh one.
          if (exists("match_margin_calibration", envir = .torp_model_cache)) {
            rm("match_margin_calibration", envir = .torp_model_cache)
          }
        }
      }

      if (has_torpmodels) {
        torpmodels::update_models_manifest(uploaded_files, tempdir(), "peteowen1/torpmodels", "core-models")
      }
    },
    error = function(e) {
      cli::cli_warn("Could not upload models to torpmodels: {conditionMessage(e)}")
    }
  )

  elapsed <- (proc.time() - .pipeline_start)[["elapsed"]]
  cli::cli_h2("Pipeline Complete")
  cli::cli_inform("Total elapsed: {round(elapsed, 1)}s")

  invisible(list(
    predictions = all_preds,
    locked_predictions = combined,
    gam_models = gam_result$models,
    xgb_models = if (!is.null(xgb_result)) xgb_result$models else NULL,
    model_data = team_mdl_df
  ))
}


#' Show TORP Match Predictions
#'
#' Display a formatted summary of TORP match predictions for a given round,
#' including predicted margins, win probabilities, and actual results where
#' available.
#'
#' @param season Season year (default: current via `get_afl_season()`)
#' @param week Round number (default: current via `get_afl_week()`)
#' @param refresh If `TRUE`, run `run_predictions_pipeline()` first
#' @return Predictions tibble with results joined (invisibly)
#' @export
#' @examples
#' \dontrun{
#' show_predictions()
#' show_predictions(2025, 10)
#' show_predictions(refresh = TRUE)
#' }
show_predictions <- function(season = get_afl_season(),
                             week = get_afl_week(),
                             refresh = FALSE) {
  if (isTRUE(refresh)) {
    run_predictions_pipeline(week = week, season = season)
  }

  preds <- tryCatch(
    load_predictions(season, week),
    error = function(e) {
      cli::cli_abort(c(
        "Could not load predictions for {season} Round {week}.",
        "i" = "Run {.code show_predictions(refresh = TRUE)} to generate them.",
        "x" = e$message
      ))
    }
  )

  if (is.null(preds) || nrow(preds) == 0) {
    cli::cli_alert_warning("No predictions found for {season} Round {week}")
    return(invisible(preds))
  }

  # Backfill actual margins for completed games missing results
  now <- Sys.time()
  game_duration_hrs <- 3.5
  preds$start_time_utc <- .parse_start_time(preds$start_time)
  preds$is_complete <- !is.na(preds$start_time_utc) &
    (now > preds$start_time_utc + game_duration_hrs * 3600)

  needs_results <- any(preds$is_complete & is.na(preds$margin))
  if (needs_results) {
    tryCatch({
      fresh <- get_afl_results(season)
      if (!is.null(fresh) && nrow(fresh) > 0) {
        result_margins <- fresh |>
          dplyr::transmute(
            match_id = match_id,
            .actual_margin = home_score - away_score
          )
        # Backward compat: preds may use old providerId column
        if (!"match_id" %in% names(preds) && "providerId" %in% names(preds)) {
          data.table::setnames(preds, "providerId", "match_id")
        }
        preds <- preds |>
          dplyr::left_join(result_margins, by = "match_id") |>
          dplyr::mutate(margin = dplyr::coalesce(margin, .actual_margin)) |>
          dplyr::select(-.actual_margin)
      }
    }, error = function(e) {
      cli::cli_warn("Could not fetch results to backfill: {e$message}")
    })
  }

  # Format and print
  cli::cli_h1("TORP Predictions: {season} Round {week}")

  # Column widths
  w_team <- 20
  header <- paste0(
    format("Home", width = w_team),
    format("Away", width = w_team),
    format("Pred", width = 8, justify = "right"),
    format("Win%", width = 8, justify = "right"),
    format("Result", width = 10, justify = "right")
  )
  cli::cli_text("{.strong {header}}")

  tips_correct <- 0
  tips_total <- 0
  abs_errors <- rep(NA_real_, nrow(preds))

  for (i in seq_len(nrow(preds))) {
    row <- preds[i, ]
    pred_str <- sprintf("%+.1f", row$pred_margin)
    win_pct <- sprintf("%.1f%%", row$pred_win * 100)

    if (!is.na(row$margin)) {
      result_str <- sprintf("%+.0f", row$margin)
      # Tip correct: predicted winner matches actual winner (same sign)
      tip_ok <- (row$pred_margin > 0 & row$margin > 0) |
        (row$pred_margin < 0 & row$margin < 0) |
        (row$margin == 0)  # draw counts as correct
      icon <- if (tip_ok) cli::col_green("\u2713") else cli::col_red("\u2717")
      result_display <- paste(result_str, icon)
      tips_total <- tips_total + 1
      tips_correct <- tips_correct + as.integer(tip_ok)
      abs_errors[i] <- abs(row$pred_margin - row$margin)
    } else if (isTRUE(row$is_complete)) {
      result_display <- "?"
    } else {
      result_display <- "-"
    }

    line <- paste0(
      format(row$home_team, width = w_team),
      format(row$away_team, width = w_team),
      format(pred_str, width = 8, justify = "right"),
      format(win_pct, width = 8, justify = "right"),
      format(result_display, width = 10, justify = "right")
    )
    cli::cli_text(line)
  }

  # Summary
  cli::cli_text("")
  parts <- c()
  if (tips_total > 0) {
    parts <- c(parts, paste0("Tips: ", tips_correct, "/", tips_total, " correct"))
    parts <- c(parts, paste0("MAE: ", sprintf("%.1f", mean(abs_errors, na.rm = TRUE))))
  }
  n_complete <- sum(preds$is_complete | !is.na(preds$margin))
  parts <- c(parts, paste0("Completed: ", n_complete, "/", nrow(preds)))
  cli::cli_text(paste(parts, collapse = " | "))

  preds$start_time_utc <- NULL
  preds$is_complete <- NULL
  invisible(preds)
}


#' Add weather to a predictions tibble
#'
#' Joins historical or forecast weather onto an existing predictions tibble
#' by `match_id`. Adds a compact `weather` summary string (e.g.
#' `"18\u00B0C, wind 22kph, rain 2.4mm"`, or `"Indoor"` for roofed games),
#' positioned after `venue`. Optionally also returns raw weather columns.
#'
#' For completed matches, weather comes from the Open-Meteo archive via the
#' torpdata `weather-data` release. For upcoming matches, the Open-Meteo
#' forecast API is queried per venue.
#'
#' @param preds Predictions tibble with `match_id`, `season`, `round` columns
#'   (typically the `$predictions` slot of `run_predictions_pipeline()`, or
#'   the output of `load_predictions()`).
#' @param raw_cols If `TRUE`, also include raw `temp_avg`, `wind_avg`,
#'   `precipitation_total`, `humidity_avg`, and `is_roof` columns.
#' @return `preds` with a `weather` column (and optionally raw weather
#'   columns) joined by `match_id`.
#' @export
#' @examples
#' \dontrun{
#' result <- run_predictions_pipeline()
#' result$predictions |>
#'   dplyr::filter(season == 2026, round == 6) |>
#'   add_weather_to_preds() |>
#'   print()
#' }
add_weather_to_preds <- function(preds, raw_cols = FALSE) {
  required <- c("match_id", "season", "round")
  missing_cols <- setdiff(required, names(preds))
  if (length(missing_cols) > 0) {
    cli::cli_abort("{.arg preds} is missing required column{?s}: {.field {missing_cols}}")
  }
  if (nrow(preds) == 0) return(preds)

  fixtures <- load_fixtures(all = TRUE)
  all_grounds <- file_reader("stadium_data", "reference-data")

  seasons <- unique(preds$season)
  weather_df <- dplyr::bind_rows(lapply(seasons, function(s) {
    rounds_s <- unique(preds$round[preds$season == s])
    .load_match_weather(
      fixtures = fixtures, all_grounds = all_grounds,
      target_weeks = rounds_s, season = s
    )
  }))

  if (nrow(weather_df) == 0) {
    cli::cli_warn("No weather data available -- adding all-NA {.field weather} column")
    preds$weather <- NA_character_
    return(preds)
  }

  weather_df$weather <- .format_weather_summary(
    weather_df$temp_avg, weather_df$wind_avg,
    weather_df$precipitation_total, weather_df$is_roof
  )

  keep <- c("match_id", "weather")
  if (isTRUE(raw_cols)) {
    keep <- c(keep, "temp_avg", "wind_avg", "precipitation_total",
              "humidity_avg", "is_roof")
  }

  out <- preds |>
    dplyr::left_join(weather_df[, intersect(keep, names(weather_df))], by = "match_id")

  added <- setdiff(keep, "match_id")
  if (length(added) > 0 && "venue" %in% names(out)) {
    out <- dplyr::relocate(out, dplyr::any_of(added), .after = "venue")
  }
  out
}


#' Format weather values into a compact human-readable summary
#'
#' @param temp_avg Numeric vector of avg temperature (degrees C)
#' @param wind_avg Numeric vector of avg wind speed (kph)
#' @param precipitation_total Numeric vector of total precipitation (mm)
#' @param is_roof Logical vector indicating roofed venues
#' @return Character vector of weather summary strings
#' @keywords internal
.format_weather_summary <- function(temp_avg, wind_avg, precipitation_total, is_roof) {
  n <- length(temp_avg)
  out <- character(n)
  for (i in seq_len(n)) {
    if (!is.na(is_roof[i]) && isTRUE(is_roof[i])) {
      out[i] <- "Indoor"
      next
    }
    parts <- character(0)
    if (!is.na(temp_avg[i])) {
      parts <- c(parts, sprintf("%.0f\u00B0C", temp_avg[i]))
    }
    if (!is.na(wind_avg[i])) {
      parts <- c(parts, sprintf("wind %.0fkph", wind_avg[i]))
    }
    if (!is.na(precipitation_total[i]) && precipitation_total[i] >= 0.1) {
      parts <- c(parts, sprintf("rain %.1fmm", precipitation_total[i]))
    }
    out[i] <- if (length(parts) == 0) NA_character_ else paste(parts, collapse = ", ")
  }
  out
}


#' Parse start_time strings to POSIXct
#'
#' Handles the local-time formatted strings stored in predictions data.
#'
#' @param x Character vector of start time strings
#' @return POSIXct vector
#' @keywords internal
.parse_start_time <- function(x) {
  # start_time is formatted as "YYYY-MM-DD HH:MM:SS TZ"
  # Try parsing with timezone, fall back to UTC
  parsed <- suppressWarnings(lubridate::ymd_hms(x, tz = "Australia/Melbourne"))
  if (all(is.na(parsed))) {
    parsed <- suppressWarnings(as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Melbourne"))
  }
  if (all(is.na(parsed)) && length(x) > 0 && !all(is.na(x))) {
    cli::cli_warn("Could not parse any start_time values. Example: {x[!is.na(x)][1]}")
  }
  parsed
}
