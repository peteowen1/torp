#' Build a ratings history without publishing it
#'
#' Extracted from \code{data-raw/03-ratings/run_ratings_pipeline.R} on
#' 2026-07-29. It lived inside that script, which writes to GitHub Releases on
#' its way through, so the ONLY way to obtain a ratings history was to publish
#' one. That made any change to the rating definition untestable against match
#' MAE -- the metric that decides whether it ships -- which is why the position
#' centring work had no MAE number.
#'
#' Nothing here writes anywhere. The pipeline now calls the same function, so
#' the offline path and production cannot drift.
#'
#' @name ratings_build
#' @keywords internal
NULL

#' Compute one season's EPR ratings
#'
#' @param year Season.
#' @param rounds Integer vector of rounds.
#' @param pgd Player game data, opponent-adjusted.
#' @param stat_ratings Player stat ratings, or NULL.
#' @param fixtures Fixtures frame.
#' @return Ratings rows for that season.
#' @keywords internal
.build_epr_season <- function(year, rounds, pgd, stat_ratings, fixtures) {
  plyr_tm_df <- load_player_details(year)
  if (nrow(plyr_tm_df) == 0 || !"season" %in% names(plyr_tm_df)) {
    plyr_tm_df <- load_player_details(year - 1)
  }

  # Build round_info with dates from fixtures
  fix_dt <- data.table::as.data.table(fixtures)
  fix_dates <- fix_dt[
    season == year & round_number %in% rounds,
    .(date_val = lubridate::as_date(min(utc_start_time))),
    by = .(round_val = round_number)
  ]

  round_info <- data.table::data.table(
    round_val = rounds,
    match_ref = paste0("CD_M", year, "014", sprintf("%02d", rounds))
  )
  round_info <- round_info[fix_dates, on = "round_val", nomatch = NULL]

  if (nrow(round_info) == 0) {
    cli::cli_alert_danger("No fixtures found for {year}")
    return(data.frame())
  }

  # Batch compute all rounds' player stats in one data.table pass
  batch_stats <- calculate_epr_stats_batch(pgd, round_info)

  # Attach decomposed TOG from stat ratings
  batch_stats[, pred_tog := NA_real_]
  batch_stats[, pred_selection := NA_real_]
  batch_stats[, pred_cond_tog := NA_real_]
  if (!is.null(stat_ratings)) {
    stat_ratings_dt <- data.table::as.data.table(stat_ratings)
    batch_stats[stat_ratings_dt, `:=`(
      pred_selection = i.squad_selection_rating,
      pred_cond_tog = i.cond_tog_rating
    ), on = "player_id"]
    batch_stats[is.na(pred_selection), pred_selection := 0]
    batch_stats[is.na(pred_cond_tog), pred_cond_tog := 0]
    batch_stats[, pred_tog := pred_selection * pred_cond_tog]
  }

  # Pre-compute fixtures summary once (avoids re-summarising 6K rows per round)
  fix_summary <- fixtures |>
    dplyr::group_by(season = .data$season, round = .data$round_number) |>
    dplyr::summarise(ref_date = lubridate::as_date(min(.data$utc_start_time)), .groups = "drop")

  # Per-round: roster join + TOG centering (lightweight ~700 rows per round)
  results <- lapply(round_info$round_val, function(rv) {
    round_dt <- batch_stats[round_val == rv]
    final_df <- .prepare_final_dataframe(plyr_tm_df, round_dt, year, rv, fixtures, fix_summary = fix_summary)

    if (!is.null(stat_ratings) && nrow(final_df) > 0) {
      final_df$pred_tog[is.na(final_df$pred_tog)] <- 0
      tot_tog <- sum(final_df$pred_tog)
      if (tot_tog > 0) {
        n_teams <- length(unique(final_df$team))
        target_tog <- n_teams * 18L
        final_df$pred_tog <- final_df$pred_tog * (target_tog / tot_tog)
        comps <- c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout")
        for (comp in comps) {
          avg_val <- sum(final_df[[comp]] * final_df$pred_tog, na.rm = TRUE) / sum(final_df$pred_tog)
          final_df[[comp]] <- final_df[[comp]] - avg_val
        }
        final_df$epr <- round(final_df$epr_recv + final_df$epr_disp + final_df$epr_spoil + final_df$epr_hitout, 2)
        for (comp in comps) {
          final_df[[comp]] <- round(final_df[[comp]], 2)
        }
      }
    }

    final_df
  })

  n_empty <- sum(vapply(results, function(x) nrow(x) == 0, logical(1)))
  if (n_empty == length(round_info$round_val) && length(round_info$round_val) > 1) {
    cli::cli_abort("All {length(round_info$round_val)} rounds empty for {year}")
  }

  results |>
    dplyr::bind_rows() |>
    (\(df) {
      if (nrow(df) == 0) return(df)
      if (!"player_id" %in% names(df)) {
        cli::cli_alert_danger("player_id column missing from ratings output for {year} - row_id cannot be computed")
        return(df)
      }
      dplyr::mutate(df, row_id = paste0(player_id, season, sprintf("%02d", round)))
    })()
}

#' Build a full ratings history in memory
#'
#' The offline twin of the ratings pipeline's stages 2-3. Takes the same
#' player-game input and the same per-season builder, applies whichever
#' normalisation toggles you ask for, and RETURNS the frame instead of
#' releasing it -- so an alternative rating definition can be scored against
#' match MAE before anyone decides to publish it.
#'
#' The toggles default to the live production constants, so calling this with
#' no arguments should reproduce the published artifact. That is the fidelity
#' check worth running before trusting any arm built on it: a harness that
#' silently differs from production measures the wrong thing, and every gate we
#' have asks "is it better", never "is it the same".
#'
#' @param seasons Seasons to build.
#' @param pgd Player game data. Loaded from the release when NULL. Pass a
#'   pre-loaded frame to compare arms without re-downloading.
#' @param stat_ratings Player stat ratings; loaded when NULL.
#' @param fixtures Fixtures; loaded when NULL.
#' @param epv_level_centre Centre EPV on listed-position levels per round.
#' @param epr_position_centre Apply the EPR-layer centring backstop.
#' @param psr_df PSR frame for \code{calculate_torp()}. Skipped when NULL.
#' @param opponent_adjust Apply \code{adjust_epv_for_opponents()}. Leave TRUE
#'   unless \code{pgd} already carries \code{_oadj} columns.
#' @return A ratings data frame. Never written anywhere.
#' @keywords internal
build_ratings_history <- function(seasons,
                                  pgd = NULL,
                                  stat_ratings = NULL,
                                  fixtures = NULL,
                                  epv_level_centre = EPV_LEVEL_CENTRE,
                                  epr_position_centre = EPR_POSITION_CENTRE,
                                  psr_df = NULL,
                                  opponent_adjust = TRUE) {
  if (is.null(pgd)) pgd <- load_player_game_data(TRUE)
  if (is.null(fixtures)) fixtures <- load_fixtures(TRUE)
  if (is.null(stat_ratings)) {
    stat_ratings <- tryCatch(get_player_stat_ratings(current = FALSE),
                             error = function(e) NULL)
  }

  pgd <- data.table::as.data.table(pgd)
  if (isTRUE(opponent_adjust)) pgd <- adjust_epv_for_opponents(pgd)
  if (isTRUE(epv_level_centre)) pgd <- centre_epv_by_position(pgd)
  data.table::setkey(pgd, match_id)

  out <- list()
  for (s in seasons) {
    start_round <- if (s >= 2024) 0 else 1
    max_round <- if (s == get_afl_season()) get_afl_week(type = "next") else 28
    out[[as.character(s)]] <- tryCatch(
      .build_epr_season(s, start_round:max_round, pgd, stat_ratings, fixtures),
      error = function(e) {
        cli::cli_alert_danger("Ratings build failed for {s}: {conditionMessage(e)}")
        NULL
      })
  }
  res <- dplyr::bind_rows(out)
  if (nrow(res) == 0) {
    cli::cli_abort("build_ratings_history() produced no rows for seasons {.val {seasons}}.")
  }

  if (isTRUE(epr_position_centre)) res <- centre_epr_by_position(res)
  if (!is.null(psr_df) && nrow(psr_df) > 0) res <- calculate_torp(res, psr_df)

  cli::cli_alert_success(
    "Built {nrow(res)} rating rows for {length(seasons)} season{?s} (epv_level_centre = {epv_level_centre}, epr_position_centre = {epr_position_centre})")
  res
}

