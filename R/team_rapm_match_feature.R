# team_rapm_match_feature.R ----
#
# Wires the decay-weighted, SPM-shrunk RAPM ("xRAPM") into the production match
# model as a per-team rating feature, `xrapm_diff`.
#
# EVIDENCE LEVEL -- READ THIS BEFORE TRUSTING THE FEATURE.
# This feature was wired at Pete's explicit direction (2026-08-25) having FAILED
# this repo's own `g7_verdict()` gate. Measured as a match-model feature over the
# pooled 2025-2026 window (n = 387, AFL-DECAY-XRAPM-PLAN.md sec23):
#
#     beta = 1.056, p = 0.075, dMAE = -0.182, 95% CI [-0.477, +0.083]
#
# It was the best of five candidate arms on MAE/RMSE/Brier/logloss/bits and the
# closest to significance, but p = 0.075 does not clear the additivity bar
# (beta > 0 AND p < 0.05), and the CI spans zero. It is shipped as a judgement
# call, not because the evidence cleared the gate.
#
# TWO CAVEATS THAT ARE NOT IN THAT GATE NUMBER, both structural:
#
#  1. THE GATE MEASURED A DIFFERENT WEIGHTING THAN THIS CODE USES. The sec14-23
#     backtest harness aggregated player ratings using each player's ACTUAL
#     `time_on_ground_percentage`, which only exists after a match is played. A
#     production feature must be computable BEFORE the bounce, so this code uses
#     `POSITION_AVG_TOG[lineup_position]` -- the expected TOG implied by the
#     named lineup slot -- exactly as `.build_team_ratings_df()` already does for
#     EPR and PSR. That is the correct production convention, but it means the
#     feature scored in sec23 and the feature computed here are not identical,
#     and the gate number should not be assumed to transfer unchanged.
#
#  2. (RESOLVED 2026-08-26) The snapshot is now a published torpdata artifact
#     (`team_rapm_asof-data`), loaded by `load_team_rapm_asof()` in
#     `load_data.R`, refreshed on the ratings-pipeline cadence, and guarded
#     against silent staleness by `.warn_stale_xrapm_snapshot()` below.
#
# Leak-safety: the as-of engine builds each snapshot from matches strictly
# before its `ref_date`, and `.team_rapm_checkpoint_dates()` sets that date to
# (round's first match - 1 day). So the snapshot for (season s, round r)
# incorporates nothing from round r onward, and using it to predict round r is
# not leakage -- the same argument `.build_team_ratings_df()` already documents
# for PSR(s, r). The rolling join below preserves that by only ever matching a
# lineup row to a checkpoint at or before its own round.

# NOTE: `load_team_rapm_asof()` lives in `load_data.R` with the rest of the
# `load_*()` family. It used to be defined here, back when the snapshot was a
# gitignored local-only artifact with no release behind it; it was moved (not
# duplicated) on 2026-08-26 when the artifact was published.

# .warn_stale_xrapm_snapshot ----

#' Warn loudly when the xRAPM snapshot is behind the round being predicted
#'
#' The failure this exists to catch is silent by construction. The rolling
#' as-of join in \code{.join_xrapm_to_lineups()} matches each lineup row to the
#' latest checkpoint at or before its own round. If the snapshot stops being
#' refreshed, that join keeps succeeding -- it just keeps returning the last
#' checkpoint it has, for every future round, forever. Nothing errors, no row
#' count changes, and the feature quietly degrades into a frozen rating.
#'
#' Warn rather than abort, deliberately, and for the same reason
#' \code{load_team_rapm_asof()} returns NULL rather than erroring: this is one
#' optional feature inside a served prediction pipeline, and a stale rating is
#' strictly better than no predictions at all. The warning names the gap in
#' rounds so it is actionable rather than decorative.
#'
#' @param xrapm_df Snapshot table, or NULL.
#' @param season Integer season being predicted.
#' @param round_number Integer round being predicted.
#' @param comp "AFLM" or "AFLW", for the message only.
#' @param max_lag_rounds Rounds of lag tolerated before warning. Default 1 --
#'   the checkpoint for round r is built just before round r, so a
#'   correctly-refreshed snapshot is never more than one round behind.
#' @return Invisibly, the lag in rounds (\code{NA} when it cannot be computed).
#' @keywords internal
.warn_stale_xrapm_snapshot <- function(xrapm_df, season, round_number,
                                       comp = "AFLM", max_lag_rounds = 1L) {
  if (is.null(xrapm_df) || nrow(xrapm_df) == 0) return(invisible(NA_integer_))
  if (length(season) != 1L || length(round_number) != 1L ||
      is.na(season) || is.na(round_number)) {
    return(invisible(NA_integer_))
  }

  target_key <- as.integer(season) * 100L + as.integer(round_number)
  snap_keys <- as.integer(xrapm_df$season) * 100L + as.integer(xrapm_df$round_number)
  snap_keys <- snap_keys[!is.na(snap_keys)]
  if (length(snap_keys) == 0) return(invisible(NA_integer_))

  latest_key <- max(snap_keys)
  if (latest_key >= target_key) return(invisible(0L))

  latest_season <- latest_key %/% 100L
  latest_round <- latest_key %% 100L

  lag_rounds <- if (latest_season == as.integer(season)) {
    as.integer(round_number) - latest_round
  } else {
    NA_integer_
  }

  if (is.na(lag_rounds) || lag_rounds > max_lag_rounds) {
    cli::cli_warn(c(
      "STALE xRAPM snapshot for comp {.val {comp}}: latest checkpoint is {latest_season} R{latest_round}, but {season} R{round_number} is being predicted.",
      "!" = "The rolling join will reuse {latest_season} R{latest_round}'s ratings for this match -- xrapm_diff is frozen, not current.",
      "i" = "Refresh it: data-raw/03-ratings/publish_team_rapm_asof.R (runs on the ratings-pipeline cadence)."
    ))
  }
  invisible(lag_rounds)
}

# .join_xrapm_to_lineups ----

#' Attach a TOG-weighted, leak-safe `xrapm` column to a lineup table
#'
#' Mirrors the PSR rolling as-of join in `.build_team_ratings_df()`: for each
#' lineup row, take the latest snapshot row for that player whose
#' (season, round) is at or before the lineup row's own (season, round).
#'
#' @param team_lineup_df Lineup rows, already carrying `lineup_tog`.
#' @param xrapm_df Output of `load_team_rapm_asof()`.
#' @return `team_lineup_df` with an added numeric `xrapm` column.
#' @keywords internal
.join_xrapm_to_lineups <- function(team_lineup_df, xrapm_df) {
  if (is.null(xrapm_df) || nrow(xrapm_df) == 0) {
    team_lineup_df$xrapm <- 0
    return(team_lineup_df)
  }

  xrapm_for_join <- xrapm_df |>
    dplyr::select(dplyr::all_of(c("player_id", "season", "round_number", "team_rapm_shrunk"))) |>
    dplyr::mutate(xrapm_key = .data$season * 100L + as.integer(.data$round_number)) |>
    dplyr::select("player_id", "xrapm_key", xrapm = "team_rapm_shrunk")

  if (any(is.na(xrapm_for_join$xrapm_key))) {
    cli::cli_abort("As-of xRAPM snapshot has {sum(is.na(xrapm_for_join$xrapm_key))} row{?s} with NA season/round.")
  }

  # Dedup on (player, key): `closest()` defaults to multiple = "all", which
  # would duplicate lineup rows and silently inflate the per-team sum below.
  # Same failure mode the PSR join guards against.
  n_before <- nrow(xrapm_for_join)
  xrapm_for_join <- dplyr::distinct(xrapm_for_join, .data$player_id, .data$xrapm_key, .keep_all = TRUE)
  n_dropped <- n_before - nrow(xrapm_for_join)
  if (n_dropped > 0) {
    cli::cli_warn("Dropped {n_dropped} duplicate (player, season, round) xRAPM row{?s} -- check the snapshot builder.")
  }

  n_rows_before <- nrow(team_lineup_df)
  team_lineup_df <- team_lineup_df |>
    dplyr::mutate(.xrapm_lineup_key = .data$season * 100L + as.integer(.data$round_number)) |>
    dplyr::left_join(
      xrapm_for_join,
      by = dplyr::join_by("player_id", closest(".xrapm_lineup_key" >= "xrapm_key"))
    ) |>
    dplyr::select(-".xrapm_lineup_key", -"xrapm_key")

  if (nrow(team_lineup_df) != n_rows_before) {
    cli::cli_abort(
      "xRAPM join changed lineup row count ({n_rows_before} -> {nrow(team_lineup_df)}) -- a duplicate snapshot key survived dedup."
    )
  }

  # Coverage telemetry, mirroring the EPR/PSR diagnostic blocks. NOTE the prior
  # here is 0, and unlike EPR/PSR that is not a shrug: RAPM is a ridge-penalised
  # effect estimated against a zero prior, so "no rating" genuinely means "no
  # measured effect on margin", which is 0 in this rating's own units. A
  # non-zero prior would be the wrong claim. What 0 must NOT be used for is a
  # whole missing snapshot -- that is handled by the caller, which keeps the
  # feature at a flat 0 across every match and says so loudly, rather than
  # letting a partial file look like real signal.
  n_missing <- sum(is.na(team_lineup_df$xrapm))
  if (n_missing > 0) {
    pct <- round(100 * n_missing / nrow(team_lineup_df), 1)
    cli::cli_inform("xRAPM: {n_missing} ({pct}%) lineup row{?s} unrated -- using the ridge prior (0).")
    if (pct > 50) {
      cli::cli_warn("More than half of lineup rows ({pct}%) have no xRAPM rating -- the snapshot is probably stale or truncated.")
    }
  }

  team_lineup_df |>
    dplyr::mutate(
      xrapm = tidyr::replace_na(.data$xrapm, 0),
      xrapm = .data$xrapm * .data$lineup_tog
    )
}
