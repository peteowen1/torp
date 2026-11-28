# Match-model state hand-over between two steps of one pipeline run
# ==================================================================
# daily-ratings-predictions.yml runs run_predictions_pipeline() and then
# data-raw/02-models/build_matchup_table.R. Both built the match-model state
# from scratch, so every in-season run fitted the GAMs and XGBoost twice,
# minutes apart, on the same matches (run 36234988123, 26 Sep 2026: round 29,
# the same weight anchor date and 2,336 training matches both times; the
# second fit took 143 s).
#
# With TORP_MATCH_STATE_HANDOFF set to a file path, the predictions step
# writes the state it has just validated to that file and the matchup step
# reads it back. Anything missing, stale, mismatched or unreadable resolves to
# REBUILD, which is exactly what the matchup step did before.

#' Path of the match-state hand-over file
#' @return The value of \code{TORP_MATCH_STATE_HANDOFF}, or \code{""} when the
#'   hand-over is off.
#' @keywords internal
.match_state_handoff_path <- function() {
  Sys.getenv("TORP_MATCH_STATE_HANDOFF", "")
}

#' Save a validated prediction state for the matchup step
#'
#' Never throws: a failure here must not stop predictions from publishing.
#' Nothing is written for a backfill or a multi-round run, or for a state that
#' failed validation.
#'
#' @param state Output of \code{build_prediction_state()}.
#' @param path File to write. Default: \code{.match_state_handoff_path()}.
#' @return \code{TRUE} if the file was written, otherwise \code{FALSE},
#'   invisibly.
#' @keywords internal
.save_match_state_handoff <- function(state, path = .match_state_handoff_path()) {
  if (!nzchar(path)) return(invisible(FALSE))
  ok <- tryCatch({
    if (is.null(state) || isTRUE(state$is_backfill) || length(state$target_weeks) != 1L ||
        length(state$validation_errors) > 0) {
      cli::cli_inform("Match state hand-over: not saved (backfill, multi-round or invalid state)")
      FALSE
    } else {
      # gam_result and xgb_result are trimmed to what .freeze_match_state()
      # reads from them: gam_result$data is a second copy of team_mdl_df.
      out <- list(
        meta = list(
          season = as.integer(state$season),
          week = as.integer(state$target_weeks),
          saved_at = Sys.time(),
          torp_version = as.character(utils::packageVersion("torp")),
          n_rows = nrow(state$team_mdl_df)
        ),
        state = list(
          team_mdl_df = state$team_mdl_df,
          team_rt_fix_df = state$team_rt_fix_df,
          all_grounds = state$all_grounds,
          gam_result = list(models = state$gam_result$models),
          xgb_result = if (is.null(state$xgb_result)) NULL else list(
            models = state$xgb_result$models,
            feature_names = state$xgb_result$feature_names
          ),
          margin_calib = state$margin_calib,
          validation_errors = state$validation_errors
        )
      )
      # Write then rename, so a killed run cannot leave half a file under the
      # real name.
      tmp <- paste0(path, ".tmp")
      t0 <- proc.time()[["elapsed"]]
      saveRDS(out, tmp, compress = FALSE)
      if (!file.rename(tmp, path)) stop("could not rename the temporary file")
      secs <- round(proc.time()[["elapsed"]] - t0, 1)
      mb <- round(file.size(path) / 1e6, 1)
      cli::cli_inform("Match state hand-over: saved {out$meta$season} R{out$meta$week} ({mb} MB, {secs}s)")
      TRUE
    }
  }, error = function(e) {
    msg <- conditionMessage(e)
    cli::cli_warn("Match state hand-over: save failed, the matchup step will rebuild. {msg}")
    FALSE
  })
  invisible(ok)
}

#' Decide whether a saved state may be reused
#'
#' Pure decision: no file access, no clock. Reuse only when the saved state is
#' for the same season and round, was written by the same torp version within
#' \code{max_age_mins}, passed validation, and holds the frames and GAM models
#' \code{.freeze_match_state()} reads.
#'
#' @param saved The object read from the hand-over file.
#' @param season,week The season and round the matchup step is building for.
#' @param now POSIXct. The moment the decision is made for.
#' @param torp_version Version string of the torp doing the reading.
#' @param max_age_mins Oldest a saved state may be.
#' @return \code{list(reuse = logical(1), reason = character(1))}.
#' @keywords internal
.match_state_handoff_decision <- function(saved, season, week, now, torp_version,
                                          max_age_mins = 90) {
  no <- function(why) list(reuse = FALSE, reason = why)
  one <- function(x) length(x) == 1L && !is.na(x)
  if (!is.list(saved) || !is.list(saved$meta) || !is.list(saved$state)) {
    return(no("saved file is not a hand-over state"))
  }
  m <- saved$meta
  if (!one(m$season) || !one(m$week) || !one(m$torp_version) ||
      !inherits(m$saved_at, "POSIXct") || !one(m$saved_at)) {
    return(no("saved state has no usable season, round, version or time"))
  }
  if (!one(season) || !one(week)) return(no("no single season and round to match"))
  if (m$season != season || m$week != week) {
    return(no(sprintf("saved state is for %s R%s, not %s R%s", m$season, m$week, season, week)))
  }
  if (!one(torp_version) || !identical(as.character(m$torp_version), as.character(torp_version))) {
    return(no(sprintf("saved by torp %s, this is torp %s", m$torp_version, torp_version)))
  }
  if (!inherits(now, "POSIXct") || !one(now)) return(no("no usable clock"))
  age <- as.numeric(difftime(now, m$saved_at, units = "mins"))
  if (age < 0 || age > max_age_mins) {
    return(no(sprintf("saved state is %.0f minutes old (limit %g)", age, max_age_mins)))
  }
  s <- saved$state
  if (!is.data.frame(s$team_mdl_df) || nrow(s$team_mdl_df) == 0L ||
      !is.data.frame(s$team_rt_fix_df) || nrow(s$team_rt_fix_df) == 0L ||
      is.null(s$all_grounds) || !is.list(s$gam_result$models) ||
      length(s$gam_result$models) == 0L) {
    return(no("saved state has an empty frame, no grounds or no GAM models"))
  }
  if (length(s$validation_errors) > 0) return(no("saved state failed validation"))
  list(reuse = TRUE, reason = sprintf(
    "state for %s R%s saved %.0f minutes ago by the predictions step (%d rows)",
    m$season, m$week, age, nrow(s$team_mdl_df)))
}

#' Read the hand-over file, if it may be reused
#'
#' Any error resolves to rebuild.
#' @param season,week The season and round the matchup step is building for.
#' @param path File to read. Default: \code{.match_state_handoff_path()}.
#' @param now POSIXct, defaults to the current time.
#' @return \code{list(state, reuse, reason)}; \code{state} is \code{NULL}
#'   unless \code{reuse} is \code{TRUE}.
#' @keywords internal
.load_match_state_handoff <- function(season, week, path = .match_state_handoff_path(),
                                      now = Sys.time()) {
  no <- function(why) list(state = NULL, reuse = FALSE, reason = why)
  if (!nzchar(path)) return(no("hand-over is off (TORP_MATCH_STATE_HANDOFF not set)"))
  if (!file.exists(path)) return(no("no saved state from the predictions step"))
  tryCatch({
    saved <- readRDS(path)
    d <- .match_state_handoff_decision(saved, season, week, now,
                                       as.character(utils::packageVersion("torp")))
    list(state = if (isTRUE(d$reuse)) saved$state else NULL,
         reuse = isTRUE(d$reuse), reason = d$reason)
  }, error = function(e) no(paste("saved state unreadable:", conditionMessage(e))))
}
