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
#  2. THE SNAPSHOT IS NOT A PUBLISHED ARTIFACT. See `load_team_rapm_asof()`.
#
# Leak-safety: the as-of engine builds each snapshot from matches strictly
# before its `ref_date`, and `.team_rapm_checkpoint_dates()` sets that date to
# (round's first match - 1 day). So the snapshot for (season s, round r)
# incorporates nothing from round r onward, and using it to predict round r is
# not leakage -- the same argument `.build_team_ratings_df()` already documents
# for PSR(s, r). The rolling join below preserves that by only ever matching a
# lineup row to a checkpoint at or before its own round.

# load_team_rapm_asof ----

#' Load the as-of xRAPM snapshot table for a competition
#'
#' @section Infrastructure gap:
#' Unlike every other rating input to the match model, this snapshot is **not**
#' published to a torpdata release and has no entry in the `load_*()` family
#' proper. It is a local build artifact written by
#' `data-raw/03-ratings/build_team_rapm_asof_snapshots.R`, and that path is
#' gitignored. Consequences, stated plainly rather than discovered later:
#'
#' * A clean checkout (and therefore CI, and therefore the scheduled prediction
#'   workflow) has no snapshot file, so `xrapm_diff` falls back to neutral 0 --
#'   see `.build_team_ratings_df()`'s caller-side handling.
#' * The snapshot must be REBUILT as the season advances. It is a per-round
#'   artifact; a stale file silently stops gaining checkpoints, and the rolling
#'   join then reuses the last available round's ratings for every later match.
#'   That degrades quietly rather than erroring.
#'
#' Making this feature genuinely live requires publishing the snapshot to a
#' torpdata release and refreshing it on the same cadence as the ratings
#' pipeline. That work does not exist yet.
#'
#' @param comp "AFLM" (default) or "AFLW".
#' @param path Optional explicit path, primarily for tests.
#' @return A data.frame with at least `player_id`, `season`, `round_number`,
#'   `team_rapm_shrunk`; or `NULL` if no snapshot is available.
#' @keywords internal
load_team_rapm_asof <- function(comp = "AFLM", path = NULL) {
  .validate_afl_comp(comp)
  if (is.null(path)) {
    path <- file.path("data-raw", "03-ratings",
                      sprintf("career_team_rapm_asof_%s.parquet", comp))
  }
  if (!file.exists(path)) {
    cli::cli_warn(c(
      "No as-of xRAPM snapshot found for comp {.val {comp}} at {.path {path}}.",
      "i" = "Build it with data-raw/03-ratings/build_team_rapm_asof_snapshots.R.",
      "x" = "xrapm_diff will fall back to neutral 0 for every match."
    ))
    return(NULL)
  }
  out <- as.data.frame(arrow::read_parquet(path))
  required <- c("player_id", "season", "round_number", "team_rapm_shrunk")
  missing <- setdiff(required, names(out))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "As-of xRAPM snapshot at {.path {path}} is missing required column{?s}: {missing}.",
      "i" = "Snapshots built before 2026-08-26 dropped `team_rapm_shrunk` and the season/round keys -- rebuild the snapshot."
    ))
  }
  out
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
