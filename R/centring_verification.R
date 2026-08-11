# Centring Verification
# =====================
# The publish guards for position centring. These check that
# `centre_epv_by_position()` and `centre_epr_by_position()` actually did what
# they claim, and abort the ratings build if not.
#
# They lived inline in `data-raw/03-ratings/run_ratings_pipeline.R` until
# 2026-08-11 -- roughly 155 lines of data.table logic that ran only in
# production and were never once executed by CI. That is a bad place for code
# whose whole job is to be right about a subtle invariant, and whose own
# comments record two occasions where the *guard* was the thing that broke
# (a cli literal starting with a dot took the pipeline down AFTER centring had
# succeeded; an equivalent guard was loosened 50x to accommodate a change and
# had to be reverted).
#
# Moved here verbatim in logic so the pipeline behaves identically, and so
# tests can exercise the failure modes rather than waiting for a production run
# to demonstrate them.
#
# Both FAIL CLOSED. An empty check is never a pass -- that is the failure shape
# these guards exist to catch, one level up: the guard degrading to a no-op
# rather than the normalisation.

# verify_epv_level_centring ----

#' Verify EPV level centring took
#'
#' Checks that the TOG-weighted mean of each centred channel per
#' `(season, round, position bucket)` is what the centring recorded it should
#' be. The weighted sum is what EPR's numerator accumulates, so it is the thing
#' that must vanish -- an unweighted mean would look centred while EPR stayed
#' skewed.
#'
#' With `EPV_POSITION_SHRINK` on, a cell is deliberately NOT left at zero: it
#' keeps `(1 - lambda) * (round mean - bucket's earlier mean)`. That is still an
#' exact quantity, so this compares against the per-cell residual centring
#' recorded rather than loosening the tolerance to accommodate the feature.
#'
#' @param pgd Player-game data returned by `centre_epv_by_position()`.
#' @param channels Channel stems that were centred.
#' @param cells The per-cell record centring left behind; defaults to the
#'   `epv_level_centring` attribute of `pgd`. Injectable so tests can drive the
#'   shrinkage path without running the centring.
#' @param tol Maximum tolerated deviation from the expected residual.
#' @param shrink_on Whether shrinkage was active for the run being checked.
#' @return Invisibly, `list(cells = <n cells checked>, worst = <max deviation>)`.
#'   Aborts on any failure.
#' @keywords internal
verify_epv_level_centring <- function(pgd,
                                      channels = EPV_LEVEL_CENTRE_CHANNELS,
                                      cells = attr(pgd, "epv_level_centring"),
                                      tol = 1e-8,
                                      shrink_on = isTRUE(EPV_POSITION_SHRINK)) {
  lc <- if (all(paste0(channels, "_oadj") %in% names(pgd))) "_oadj" else "_adj"
  chk <- data.table::as.data.table(pgd)
  chk[, `:=`(pos_bucket = .collapse_listed_position(position_group),
             w = pmax(dplyr::coalesce(time_on_ground_percentage / 100, 0.1), 0.1))]
  chk <- chk[!is.na(pos_bucket)]

  # Fail CLOSED. `worst` starts at 0 and is only ever RAISED, so if every cell
  # is empty -- position_group missing or renamed, or every value unmapped --
  # the loop never runs, `worst` stays 0, the abort is skipped, and this
  # reports "verified (max deviation = 0)" having verified nothing.
  if (nrow(chk) == 0) {
    cli::cli_abort(c(
      "Cannot verify EPV level centring: no row has a mapped position bucket.",
      "x" = "Refusing to build ratings on EPV whose centring cannot be checked."
    ))
  }

  if (shrink_on && (is.null(cells) || nrow(cells) == 0)) {
    cli::cli_abort(c(
      "EPV_POSITION_SHRINK is ON but centre_epv_by_position() recorded no per-cell corrections.",
      "x" = "Without them a shrunk residual is indistinguishable from centring having failed."
    ))
  }

  worst <- 0
  n_cells <- 0L
  for (cc in paste0(channels, lc)) {
    m <- chk[is.finite(get(cc)),
             .(wm = stats::weighted.mean(get(cc), w)),
             by = .(season, round, pos_bucket)]
    if (nrow(m) == 0) next
    m[, expected := 0]
    if (!is.null(cells) && nrow(cells) > 0) {
      ce <- data.table::as.data.table(cells)[channel == cc,
                                             .(season, round, pos_bucket, resid_expected)]
      if (nrow(ce) > 0) {
        m[ce, expected := i.resid_expected, on = .(season, round, pos_bucket)]
        # KNOWN GAP, documented rather than fixed here (see
        # test-centring-verification.R). This is a join-UPDATE: it writes only
        # to rows of `m` that MATCH `ce`. A cell absent from `ce` keeps its
        # `expected = 0` default and never becomes NA, so the check below sees
        # only explicitly-NA recorded residuals -- not missing ones. The
        # original comment here claimed otherwise.
        #
        # Harmless today: EPV_POSITION_SHRINK is FALSE, so every true residual
        # is 0 and 0 is the right expectation. With shrinkage ON an unrecorded
        # cell trips the tolerance check instead, so it still fails -- just
        # with a message blaming centring rather than the missing record.
        # Tightening it is a ratings-pipeline decision, not a refactor.
        if (anyNA(m$expected)) {
          cli::cli_abort(c(
            "{sum(is.na(m$expected))} cell{?s} of {.field {cc}} have no recorded correction.",
            "x" = "Refusing to treat an uncorrected cell as if it had been centred."
          ))
        }
      }
    }
    worst <- max(worst, max(abs(m$wm - m$expected), na.rm = TRUE))
    n_cells <- n_cells + nrow(m)
  }

  if (n_cells == 0L) {
    cli::cli_abort(c(
      "Cannot verify EPV level centring: zero cells had a finite channel value.",
      "x" = "An empty check is not a pass."
    ))
  }
  # NOTE the reporting names below are deliberately NOT dot-prefixed. cli
  # >= 3.4.0 reads `{.name}` as style markup, so a bare `{.checked}` in any
  # cli_* string is a hard error ("Invalid cli literal: starts with a dot").
  # That took this pipeline down on 2026-07-29 AFTER centring had already
  # succeeded -- the guard crashed while reporting a pass. `{signif(worst, 3)}`
  # is safe either way because it starts with a function call, which is exactly
  # why the abort paths survived and only the happy path blew up.
  if (!is.finite(worst) || worst > tol) {
    cli::cli_abort(c(
      "EPV level centring did not take: max |cell mean - expected residual| = {signif(worst, 3)}",
      "x" = "Refusing to build ratings on EPV that is not centred as claimed."
    ))
  }
  cli::cli_alert_success(
    "EPV level centring verified across {n_cells} cell{?s} (max deviation from expected = {signif(worst, 3)}; shrinkage {shrink_on})")
  invisible(list(cells = n_cells, worst = worst))
}


# verify_epr_position_centring ----

#' Verify EPR position centring took
#'
#' Checks EVERY `(season, round, position)` cell, not just the latest --
#' centring runs across all history, and it is the historical rounds that feed
#' model training, so sampling one round would let an earlier one fail silently.
#'
#' Groups by the SAME collapsed 6-way bucket `centre_epr_by_position()` used.
#' Keying this on raw `position_group` while centring uses the collapsed map
#' would fail every run for the two merged forward groups, each of which is only
#' mean-zero jointly. A guard that groups differently from the code it guards is
#' not a guard.
#'
#' This checks the weighted mean of `epr` (the total) while centring is applied
#' per CHANNEL. Those agree exactly only because the channels are finite
#' together or NA together (verified 2026-07-28: 107,448 rows all finite, 23,480
#' all NA, ZERO partial), so every channel's mean is taken over the same players
#' and their sum is the total's mean. That invariant holds by data, not by
#' contract; if partial-NA rows appear the check fails loud rather than passing
#' something wrong, but the message would be misleading.
#'
#' @param epr_df Ratings returned by `centre_epr_by_position()`.
#' @param tol Maximum tolerated absolute weighted cell mean.
#' @return Invisibly, `list(cells = <n cells checked>, worst = <max |mean|>)`.
#'   Aborts on any failure.
#' @keywords internal
verify_epr_position_centring <- function(epr_df, tol = 1e-6) {
  chk <- data.table::as.data.table(epr_df)
  chk[, pos_bucket := .collapse_listed_position(position_group)]
  chk <- chk[
    !is.na(pos_bucket),
    .(wmean = stats::weighted.mean(epr, pmax(pred_tog, 0.01), na.rm = TRUE),
      n = .N, n_rated = sum(is.finite(epr))),
    by = .(season, round, pos_bucket)]

  # A cell where NOBODY is rated yet has nothing to centre and no mean to check
  # -- that is the start of the dataset, not a failure. Skip those, but COUNT
  # them, so "nothing was checkable" can never be mistaken for "everything
  # checked out".
  unrated <- chk[n_rated == 0]
  if (nrow(unrated) > 0) {
    cli::cli_inform(
      "Position centring: {nrow(unrated)} cell{?s} have no rated players (earliest: season {unrated$season[1]} round {unrated$round[1]} {unrated$pos_bucket[1]}) -- nothing to verify there")
  }
  # Known, accepted limitation: a cell with n_rated == 1 passes vacuously -- the
  # weighted mean of one point IS that point, so subtracting it leaves exactly 0
  # whatever the grouping logic did. Early rounds are where such cells live, so
  # the guard's power is weakest precisely where this filter newly admits cells.
  # Left as-is because the failure this exists to catch (a whole taxonomy or
  # channel not centring) shows up across many cells at once, not in one.
  chk <- chk[n_rated > 0]

  # Fail CLOSED. Zero rows means nothing had a position group, which is exactly
  # the state in which centring cannot have happened.
  if (nrow(chk) == 0) {
    cli::cli_abort(c(
      "Cannot verify EPR position centring: no position bucket has a single rated player.",
      "x" = "Refusing to publish ratings whose centring cannot be checked."
    ))
  }
  if (!all(is.finite(chk$wmean))) {
    bad <- chk[!is.finite(wmean)]
    cli::cli_abort(c(
      "EPR position centring produced {nrow(bad)} non-finite cell mean{?s}.",
      "i" = "First: season {bad$season[1]} round {bad$round[1]} {bad$pos_bucket[1]}"
    ))
  }
  worst <- max(abs(chk$wmean))
  if (worst > tol) {
    b <- chk[which.max(abs(wmean))]
    cli::cli_abort(c(
      "EPR position centring did not take: max |weighted mean| = {signif(worst, 3)}",
      "i" = "Worst cell: season {b$season} round {b$round} {b$pos_bucket} (n = {b$n})",
      "x" = "Refusing to publish ratings whose positions are not centred as claimed."
    ))
  }
  cli::cli_alert_success(
    "Position centring verified across {nrow(chk)} (season, round, position) cell{?s}")
  invisible(list(cells = nrow(chk), worst = worst))
}
