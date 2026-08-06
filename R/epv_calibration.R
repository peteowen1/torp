# Putting the DESCRIPTIVE product's channels into points.
#
# Raw `epv` already conserves -- it converts to margin at 0.9879 on the ship
# build and 0.9936 under the difficulty split. Its channels do not. Under the
# difficulty split they read recv 1.399, disp 1.103, contest 0.368: the total is
# right by cancellation, and a player's contest number is not in points.
#
# Scaling each channel by its own margin coefficient fixes that at no cost. The
# sum of coefficient-scaled channels IS the OLS fitted value, so regressing
# margin on it returns exactly 1.000 -- every channel and the total, together.
# (The reciprocal does the opposite and inflates whatever under-converts. That
# mistake cost an hour on 2026-08-05 and produced a confident, wrong claim that
# the two criteria were in conflict.)
#
# It also fixes the largest structural problem in v3: contest carries 42.9% of
# raw variance share against 6% of the margin-explaining share, and scaling by
# 0.368 takes it to 6.1%. The raw leaderboard goes from two rucks in the top
# three to Warner / Watson / Heeney / Bontempelli.
#
# THIS IS NOT `EPV3_POINTS_SCALE`. That is the same idea applied at the EPR
# layer, on the RATING path, after opponent adjustment and position centring.
# This is the raw per-game frame -- the descriptive product. They are different
# products (see docs/plans/EPV-REBUILD-HANDOVER.md §1) and must not share a
# constant: applying both to the same numbers double-scales them.
#
# ADDITIVE BY DESIGN. New `*_cal` columns; nothing existing is touched. Callers
# opt in, so no pipeline changes behaviour by loading this file.

#' Channel scales for the raw descriptive product
#'
#' \code{NULL} means "fit from the data you are given", which is the default and
#' the safe choice: a stored scale fitted on one build and applied to another is
#' exactly the staleness trap that invalidated a gate on 2026-08-05. Set this
#' only to reproduce a specific published vintage.
#'
#' Measured 2026-08-05 over 1,241 matches, for reference — do not paste these in
#' without checking which engine and flags produced them:
#' \itemize{
#'   \item v3 ship: recv 0.8931, disp 1.5559, contest 0.3441
#'   \item v3 + difficulty split: recv 1.3993, disp 1.1027, contest 0.3676
#' }
#' @keywords internal
EPV_RAW_CHANNEL_SCALE <- NULL

#' Fit the raw-layer channel scales against match margin
#'
#' @param pgd Player-game frame carrying \code{epv_recv}, \code{epv_disp},
#'   \code{epv_spoil}, \code{match_id} and \code{team}.
#' @param results Match results with \code{match_id}, \code{home_team_name},
#'   \code{away_team_name}, \code{home_score}, \code{away_score}.
#' @return Named numeric of length 3, plus a \code{"n_matches"} attribute.
#' @keywords internal
fit_epv_channel_scale <- function(pgd, results) {
  ch <- c("epv_recv", "epv_disp", "epv_spoil")
  d <- data.table::as.data.table(pgd)
  miss <- setdiff(c(ch, "match_id", "team"), names(d))
  if (length(miss)) {
    cli::cli_abort("Player-game frame is missing {.field {miss}}.")
  }
  r <- data.table::as.data.table(results)
  tg <- r[, .(match_id = as.character(match_id), home = home_team_name,
              away = away_team_name, margin = home_score - away_score)]
  tg <- tg[is.finite(margin)]

  ts <- d[, lapply(.SD, function(v) sum(v, na.rm = TRUE)), .SDcols = ch,
          by = .(match_id = as.character(match_id), team)]
  h <- merge(tg, ts, by.x = c("match_id", "home"), by.y = c("match_id", "team"))
  a <- merge(tg, ts, by.x = c("match_id", "away"), by.y = c("match_id", "team"))
  m <- merge(h[, c("match_id", "margin", ch), with = FALSE],
             a[, c("match_id", ch), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  if (nrow(m) < 100) {
    cli::cli_abort(c("Only {nrow(m)} match{?es} usable for the channel-scale fit.",
                     "x" = "Refusing to fit a calibration constant on that."))
  }
  for (v in ch) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  fml <- stats::as.formula(paste("margin ~ 0 +", paste0("d_", ch, collapse = " + ")))
  co <- stats::coef(stats::lm(fml, data = m))
  # lm() returns NA for an aliased coefficient instead of erroring -- a channel
  # with no variance across teams (a thin sample, a single round, a season
  # before that channel accrues) comes back NA. That NA then multiplies into
  # the channel and, because the total sums all channels, makes epv_cal NA for
  # EVERY row while the function still reports success. Fail here instead.
  if (anyNA(co)) {
    cli::cli_abort(c(
      "Channel scale did not fit: {paste(ch[is.na(co)], collapse = ', ')} came back NA.",
      "i" = "lm() aliases a coefficient when its channel has no variance across teams.",
      "x" = "Refusing to return a scale that would make every calibrated total NA."))
  }
  out <- stats::setNames(as.numeric(co), ch)
  data.table::setattr(out, "n_matches", nrow(m))
  out
}

#' Put a player-game frame's EPV channels into points
#'
#' Adds \code{epv_recv_cal}, \code{epv_disp_cal}, \code{epv_spoil_cal} and their
#' sum \code{epv_cal}. Existing columns are untouched.
#'
#' @param pgd Player-game frame.
#' @param results Match results, needed only when \code{scale} is \code{NULL}.
#' @param scale Optional named numeric to use instead of fitting.
#' @return \code{pgd} with four columns added; the scale used is attached as the
#'   \code{"epv_channel_scale"} attribute.
#' @keywords internal
calibrate_epv_channels <- function(pgd, results = NULL,
                                   scale = EPV_RAW_CHANNEL_SCALE) {
  ch <- c("epv_recv", "epv_disp", "epv_spoil")
  if (is.null(scale)) {
    if (is.null(results)) {
      cli::cli_abort(c("Need either {.arg scale} or {.arg results}.",
                       "i" = "Fitting from the frame in hand is the default because a stored scale fitted on a different build is the staleness trap."))
    }
    scale <- fit_epv_channel_scale(pgd, results)
    cli::cli_alert_info(
      "Fitted EPV channel scale on {attr(scale, 'n_matches')} matches: {paste(names(scale), round(scale, 4), sep = ' ', collapse = ', ')}")
  }
  if (!all(ch %in% names(scale))) {
    cli::cli_abort("{.arg scale} must be named for {.field {ch}}.")
  }
  d <- data.table::copy(data.table::as.data.table(pgd))
  for (v in ch) {
    data.table::set(d, j = paste0(v, "_cal"),
                    value = scale[[v]] * data.table::fifelse(is.na(d[[v]]), 0, d[[v]]))
  }
  data.table::set(d, j = "epv_cal",
                  value = d$epv_recv_cal + d$epv_disp_cal + d$epv_spoil_cal)
  data.table::setattr(d, "epv_channel_scale", scale)
  d
}
