# Player Skill Rating (PSR)
# =========================
# Predict match margin from team-aggregated player skills via glmnet,
# then apportion coefficients back to individual players.
# PSR = "predicted margin contribution points" above league average.


#' Attach the listed-position centring key to a PSR input frame
#'
#' Adds \code{.listed_bucket}, the 6-way LISTED taxonomy that EPV, EPR and PSV
#' already centre on, so PSR can be centred on the same key rather than on the
#' PBP-derived playstyle label the stat-ratings frame happens to carry.
#'
#' Why this exists as a join rather than a column preference: the released
#' \code{player_stat_ratings} frame carries \code{pos_group} and \code{wt_80s}
#' and no listed position at all, so there is nothing local to prefer.
#'
#' Why a missing frame ABORTS instead of falling back: a silent fallback here
#' reproduces the exact defect this change is fixing. The old preference chain
#' fell through to \code{pos_group} without comment, and the resulting
#' centred-on-A-read-as-B mismatch survived every existing check for months. The
#' same shape took down the PSV guard (tested \code{round}, frame had
#' \code{round_number}, silently centred nothing, spread unchanged at 0.812).
#' A normalisation that quietly does the wrong thing is worse than one that
#' stops.
#'
#' @param dt Data.table of stat ratings.
#' @param listed_pos Data frame with \code{player_id} plus one of
#'   \code{position_group} / \code{position}; or NULL.
#' @param center Whether centring was requested at all. When FALSE this is a
#'   no-op, because no key is needed.
#' @return \code{dt}, with \code{.listed_bucket} added when available.
#' @keywords internal
.attach_listed_pos <- function(dt, listed_pos, center = TRUE,
                               centre_on_listed = PSR_CENTRE_ON_LISTED) {
  if (!isTRUE(center) || !isTRUE(centre_on_listed)) return(dt)

  # Self-load when the caller did not supply one. This is deliberate: making
  # listed_pos a REQUIRED argument would abort ~15 existing call sites across
  # torp's pipeline, the stat-rating trainers, the tests and seven torpmodels
  # harness scripts -- and every one of them wants listed centring. Defaulting
  # to "load it" fixes them all at once; defaulting to "abort" would have made
  # the correct behaviour the one you must remember to ask for, which is how
  # the playstyle key survived unnoticed in the first place.
  # RESIDUAL COMP GAP, deliberately left rather than plumbed:
  # this self-load has no comp to forward, so it defaults to AFLM. An AFLW
  # caller reaches it only if BOTH (a) PSR_CENTRE_ON_LISTED is TRUE (FALSE
  # today) and (b) .compute_psr_from_stat_ratings()'s own comp-aware load
  # returned NULL -- i.e. load_player_details() failed for EVERY requested
  # season. Doubly latent. Closing it properly means adding `comp` to
  # calculate_psr()/calculate_psr_components(), both EXPORTED, with 49 call
  # sites across 19 files; that churn is not justified by a dead branch.
  # If PSR_CENTRE_ON_LISTED is ever flipped on for AFLW, thread comp through
  # from calculate_psr() BEFORE trusting AFLW centring.
  if (is.null(listed_pos)) {
    listed_pos <- .load_listed_positions(unique(dt$season))
  }

  if (is.null(listed_pos)) {
    cli::cli_abort(c(
      "PSR_CENTRE_ON_LISTED is TRUE but no listed positions are available.",
      "i" = "Pass {.arg listed_pos} explicitly, or make {.fun load_player_details} reachable.",
      "x" = "Refusing to centre PSR on a playstyle label while TORP reads it as a listed one.",
      "i" = "Set {.code PSR_CENTRE_ON_LISTED <- FALSE} to deliberately score the old arm."
    ))
  }

  lp <- data.table::as.data.table(listed_pos)
  pcol <- intersect(c("position_group", "position"), names(lp))[1]
  if (is.na(pcol) || !"player_id" %in% names(lp)) {
    cli::cli_abort(c(
      "{.arg listed_pos} must have {.field player_id} and one of {.field position_group} / {.field position}.",
      "i" = "Got: {.val {names(lp)}}"
    ))
  }

  # One listed position per player. Take the LAST non-missing value, matching
  # how player_details resolves a mid-season relisting.
  lp <- lp[!is.na(get(pcol)), .(.lp_raw = get(pcol)[.N]), by = player_id]
  lp[, .listed_bucket := .collapse_listed_position(.lp_raw)]

  out <- merge(dt, lp[, .(player_id, .listed_bucket)], by = "player_id",
               all.x = TRUE, sort = FALSE)

  matched <- sum(!is.na(out$.listed_bucket))
  if (matched == 0L) {
    cli::cli_abort(c(
      "No row matched a listed position -- PSR centring key would be entirely NA.",
      "x" = "An empty join is not a successful one."
    ))
  }
  if (matched < 0.9 * nrow(out)) {
    # alert_danger, not warn: this means PSR centring has quietly degraded to a
    # global-mean fallback for >10% of rows, i.e. a wrong published number, and
    # a deferred warning is the reporting path that already failed once today.
    cli::cli_alert_danger(
      "Only {matched} of {nrow(out)} stat-rating rows matched a listed position -- the rest fall back to the GLOBAL mean, not their position's.")
  }
  out
}

#' Build a comp-qualified coefficient filename
#'
#' AFLM (default) is unchanged; any other comp gets its coefficient set
#' suffixed (e.g. "psr_coefficients.csv" -> "psr_coefficients_aflw.csv"),
#' matching the naming the AFLW training pipeline
#' (data-raw/06-stat-ratings/aflw_run_pipeline.R) writes.
#' @param base Base filename, e.g. "psr_coefficients.csv"
#' @param comp Competition: "AFLM" (default) or "AFLW"
#' @return The comp-qualified filename
#' @keywords internal
.comp_coef_filename <- function(base, comp = "AFLM") {
  if (comp == "AFLM") return(base)
  sub("\\.csv$", paste0("_", tolower(comp), ".csv"), base)
}

#' Resolve path to PSR coefficient CSV
#'
#' Checks inst/extdata first, then falls back to data-raw/cache-stat-ratings/.
#' @param coef_file Filename (default "psr_coefficients.csv")
#' @param comp Competition: "AFLM" (default) or "AFLW". AFLW's PSR/PSV is a
#'   box-score-only pipeline sourced from load_player_stats(comp="AFLW") --
#'   see docs/plans/AFLW-MIGRATION-PLAN.md Sec 6 -- with its own coefficient
#'   files, never comparable to men's PSR/PSV numbers.
#' @return Absolute path to the CSV, or "" if not found
#' @keywords internal
.find_psr_coef_path <- function(coef_file = "psr_coefficients.csv", comp = "AFLM") {
  .validate_afl_comp(comp)
  coef_file <- .comp_coef_filename(coef_file, comp)
  path <- system.file("extdata", coef_file, package = "torp")
  if (path == "") {
    path <- file.path(
      find.package("torp", quiet = TRUE)[1] %||% ".",
      "data-raw", "cache-stat-ratings", coef_file
    )
  }
  if (!file.exists(path)) return("")
  path
}


# Stats excluded from PSV calculation: efficiency ratios (not rate stats),
# bounces (negative coefficient, not causal), and availability metrics
.PSV_EXCLUDE <- c("disposal_efficiency", "goal_accuracy", "contested_poss_rate",
                   "hitout_win_pct", "kick_efficiency", "bounces",
                   "cond_tog", "squad_selection")


#' Calculate Player Skill Ratings (PSR)
#'
#' Computes PSR for each player-round by applying glmnet coefficients to
#' individual player stat rating values. PSR represents each player's predicted
#' contribution to match margin based on their stat rating profile.
#'
#' @param skills A data.table/data.frame from \code{load_player_stat_ratings()},
#'   containing \code{player_id}, \code{player_name}, \code{season},
#'   \code{round}, \code{pos_group}, and \code{*_rating} columns.
#' @param coef_df A data.frame with columns \code{stat_name} and \code{beta},
#'   as produced by the PSR training script. If an \code{sd} column is present,
#'   each stat rating is divided by its SD before multiplying by beta (i.e. the
#'   coefficients are on the standardized scale).
#' @param center Logical. If TRUE (default), subtract the league mean so
#'   PSR = contribution above average player.
#'
#' @param centre_by_round Logical. Group the position centring by
#'   \code{(season, round)} as well as position. Defaults to
#'   \code{PSR_CENTRE_BY_ROUND}. FALSE reproduces the pre-2026-07-29
#'   pooled-over-all-history behaviour, and exists so that arm can be
#'   scored on the match harness from production code rather than a replica.
#' @param listed_pos Optional data frame carrying each player's LISTED position:
#'   \code{player_id} plus one of \code{position_group} / \code{position}. Supply
#'   it (from \code{load_player_details()}) to centre PSR on the same listed
#'   taxonomy EPV, EPR and PSV use. Required when
#'   \code{PSR_CENTRE_ON_LISTED} is TRUE; see the note in the function body for
#'   why a missing frame aborts rather than falling back.
#' @param centre_on_listed Logical. Centre on the LISTED position taxonomy that
#'   EPV/EPR/PSV use, rather than the stat-ratings frame's own \code{pos_group}.
#'   Defaults to \code{PSR_CENTRE_ON_LISTED}, which is FALSE: the listed arm was
#'   scored on 2026-07-29 and REGRESSED (dMAE +0.382, CI entirely above zero).
#'   Exists so that arm stays reachable from production code rather than a
#'   replica.
#' @return A data.table with columns: \code{player_id}, \code{player_name},
#'   \code{season}, \code{round}, \code{pos_group}, \code{psr_raw}, \code{psr}.
#'
#' @export
calculate_psr <- function(skills, coef_df, center = TRUE,
                          centre_by_round = PSR_CENTRE_BY_ROUND,
                          listed_pos = NULL,
                          centre_on_listed = PSR_CENTRE_ON_LISTED) {
  dt <- data.table::as.data.table(skills)

  # Validate coef_df
  if (!all(c("stat_name", "beta") %in% names(coef_df))) {
    cli::cli_abort("{.arg coef_df} must have columns {.val stat_name} and {.val beta}")
  }

  # AFTER argument validation, deliberately: .attach_listed_pos() can hit the
  # network via load_player_details(), and a caller who passed a malformed
  # coef_df should get the coef_df error rather than a centring one raised
  # while fetching data for a call that was never going to succeed.
  dt <- .attach_listed_pos(dt, listed_pos, center, centre_on_listed)

  # Filter to non-zero coefficients
  coef_df <- coef_df[coef_df$beta != 0, , drop = FALSE]

  if (nrow(coef_df) == 0) {
    cli::cli_warn("All coefficients are zero -- PSR will be zero for all players")
    dt[, c("psr_raw", "psr") := 0]
    id_cols <- intersect(c("player_id", "player_name", "season", "round", "pos_group"), names(dt))
    return(dt[, c(id_cols, "psr_raw", "psr"), with = FALSE])
  }

  # Map stat_name to rating column name (try _rating first, fall back to _skill)
  skill_cols <- paste0(coef_df$stat_name, "_rating")
  available <- skill_cols %in% names(dt)
  # Backward compat: if _rating columns not found, try _skill
  if (sum(available) == 0) {
    skill_cols <- paste0(coef_df$stat_name, "_skill")
    available <- skill_cols %in% names(dt)
  }

  if (sum(available) == 0) {
    cli::cli_abort("No matching stat rating columns found in data")
  }
  if (any(!available)) {
    missing <- coef_df$stat_name[!available]
    cli::cli_warn("Skill columns not found (skipped): {paste(missing, collapse = ', ')}")
  }

  coef_df <- coef_df[available, , drop = FALSE]
  skill_cols <- skill_cols[available]
  betas <- coef_df$beta
  # Points-scale calibration on the SHARED coefficient vector. PSR is not
  # downstream of PSV -- they are parallel applications of these same betas to
  # rated vs actual stats -- so the betas are the single point that moves both
  # together. PSR converted at 1.579 points per rating point, so new = 1.579*old.
  # Centring and standardisation downstream are both scale-equivariant
  # ((k*x - k*m)/(k*sd)*(k*pooled) = k * result), so the factor survives intact.
  if (is.finite(PSV_POINTS_SCALE) && !isTRUE(all.equal(PSV_POINTS_SCALE, 1))) {
    betas <- betas * PSV_POINTS_SCALE
  }

  # Compute PSR: sum of beta_i * (skill_i / sd_i) for each player-round
  mat <- as.matrix(dt[, skill_cols, with = FALSE])
  mat[is.na(mat)] <- 0

  # If coef_df has SD column, normalize skills before applying betas
  if ("sd" %in% names(coef_df)) {
    sds <- coef_df$sd
    sds[sds == 0 | is.na(sds)] <- 1
    mat <- sweep(mat, 2, sds, "/")
  }

  dt[, psr_raw := as.numeric(mat %*% betas)]

  # Center by position (wt_80s-weighted mean subtraction).
  #
  # Preference order matters and is evidence-based (FABLE-DEFENDER-VALUE-PLAN
  # §7.15). What drives calibration is TEMPORAL RESOLUTION, not granularity:
  #   - lineup_pos_group  6-way, from the WEEKLY team sheet   <- best
  #   - lineup_position   20-way, weekly but unnecessarily fine
  #   - pos_group         6-way, but effectively season-constant (it varies in
  #                       only 0.6% of player-seasons), which is what production
  #                       silently used for years because the stat-ratings frame
  #                       carries no lineup column at all
  # Moving from pos_group to a weekly 6-way role improved overall positional
  # calibration by 0.138 on mean|beta-1| with P(improves) 0.956; going finer
  # than 6-way added nothing (P 0.417).
  # `.listed_bucket` first: it is the LISTED taxonomy that EPV, EPR and PSV all
  # centre on, attached by .attach_listed_pos(). Until 2026-07-29 this list
  # started at lineup_pos_group and, since the released stat-ratings frame
  # carries none of the weekly columns, always fell through to `pos_group` --
  # a PBP-derived PLAYSTYLE label, not a listed one. Measured against
  # torp_ratings' listed position the two disagree for 13.2% of player-rounds
  # (20.3% in 2026), and centring on one while TORP reads the other put ~0.30 of
  # positional level straight back into TORP. 2021, where the labels agree 100%,
  # showed exactly 0.000 spread -- the control that identified the mechanism.
  psr_pos_col <- if (".listed_bucket" %in% names(dt)) ".listed_bucket"
                 else if ("lineup_pos_group" %in% names(dt)) "lineup_pos_group"
                 else if ("lineup_position" %in% names(dt)) "lineup_position"
                 else if ("pos_group" %in% names(dt)) "pos_group"
                 else NULL
  if (center && is.null(psr_pos_col)) {
    cli::cli_warn("No position column found for PSR centering; using global mean subtraction")
  }
  # Group per (season, round) when those columns exist, not pooled over all
  # history. Pooling makes every position average zero ACROSS the dataset while
  # leaving any individual round skewed -- measured on the served round, rucks
  # sat +0.451 and key forwards -0.030, a 0.481 spread that TORP inherited half
  # of. It is also a backtest leak: a 2021 round-1 rating centred with 2026
  # games. Mirrors the (season, round) grouping EPR has always used.
  .psr_by <- c(if (isTRUE(centre_by_round)) intersect(c("season", "round"), names(dt)),
               psr_pos_col)
  if (center && !is.null(psr_pos_col) && "wt_80s" %in% names(dt)) {
    dt[!is.na(get(psr_pos_col)), psr := psr_raw - weighted.mean(psr_raw, wt_80s, na.rm = TRUE), by = c(.psr_by)]
    dt[is.na(get(psr_pos_col)), psr := psr_raw - weighted.mean(psr_raw, wt_80s, na.rm = TRUE)]
  } else if (center && !is.null(psr_pos_col)) {
    dt[!is.na(get(psr_pos_col)), psr := psr_raw - mean(psr_raw, na.rm = TRUE), by = c(.psr_by)]
    dt[is.na(get(psr_pos_col)), psr := psr_raw - mean(psr_raw, na.rm = TRUE)]
  } else if (center) {
    dt[, psr := psr_raw - mean(psr_raw, na.rm = TRUE)]
  } else {
    dt[, psr := psr_raw]
  }

  # Rescale to the pooled cross-position spread as well as recentring, so PSR
  # stops carrying the between-position spread differences that under-disperse
  # key defenders. Mirrors EPV_POSITION_STANDARDISE; see
  # PSR_POSITION_STANDARDISE for why this is separately flagged.
  if (center && isTRUE(PSR_POSITION_STANDARDISE) && !is.null(psr_pos_col)) {
    w <- if ("wt_80s" %in% names(dt)) dt$wt_80s else rep(1, nrow(dt))
    pooled <- .wtd_sd(dt$psr, w)
    dt[, .psr_w := w]
    dt[!is.na(get(psr_pos_col)), .psr_sd := .wtd_sd(psr, .psr_w), by = c(psr_pos_col)]
    dt[is.na(get(psr_pos_col)), .psr_sd := NA_real_]
    # degenerate or missing within-group SD -> leave the row centred only,
    # never divide by ~zero (the failure mode that excluded hitout on the EPV side)
    dt[!is.na(.psr_sd) & .psr_sd > 1e-6 & !is.na(pooled),
       psr := psr / .psr_sd * pooled]
    dt[, c(".psr_w", ".psr_sd") := NULL]
  }

  id_cols <- intersect(
    c("player_id", "player_name", "season", "round", "pos_group",
      "team", "n_games", "wt_games", "wt_80s"),
    names(dt)
  )

  dt[, c(id_cols, "psr_raw", "psr"), with = FALSE]
}


#' Calculate PSR with Offensive/Defensive Decomposition
#'
#' Computes the margin-based PSR (the best single predictor of match outcomes),
#' then decomposes it into offensive (OSR) and defensive (DSR) components using
#' separately trained coefficient models. The decomposition uses an additive
#' shift so that \code{osr + dsr = psr} exactly.
#'
#' @inheritParams calculate_psr
#' @param osr_coef_df Coefficient data.frame for the offensive model (same
#'   format as \code{coef_df}: columns \code{stat_name}, \code{beta},
#'   optionally \code{sd}).
#' @param dsr_coef_df Coefficient data.frame for the defensive model.
#'
#' @param centre_by_round Logical. Group the position centring by
#'   \code{(season, round)} as well as position. Defaults to
#'   \code{PSR_CENTRE_BY_ROUND}. FALSE reproduces the pre-2026-07-29
#'   pooled-over-all-history behaviour, and exists so that arm can be
#'   scored on the match harness from production code rather than a replica.
#' @param centre_on_listed Logical. Centre on the LISTED position taxonomy
#'   rather than the stat-ratings frame's own pos_group. Defaults to
#'   PSR_CENTRE_ON_LISTED (FALSE -- the listed arm was measured and
#'   regressed, dMAE +0.382).
#' @param listed_pos Optional data frame of player_id plus a listed position
#'   column. Passed through to calculate_psr(); loaded from
#'   load_player_details() when NULL and centring on listed is requested.
#' @return A data.table with columns: \code{player_id}, \code{player_name},
#'   \code{season}, \code{round}, \code{pos_group}, \code{psr_raw}, \code{psr},
#'   \code{osr}, \code{dsr}.
#'
#' @details
#' The margin-PSR is the gold-standard total rating. OSR and DSR come from
#' models trained on points-scored and points-conceded respectively. Since
#' these are trained independently, \code{raw_osr + raw_dsr != psr} in general.
#' We reconcile by distributing the residual evenly:
#'
#' \deqn{\delta = (psr - raw\_osr - raw\_dsr) / 2}
#' \deqn{osr = raw\_osr + \delta}
#' \deqn{dsr = raw\_dsr + \delta}
#'
#' @export
calculate_psr_components <- function(skills, coef_df, osr_coef_df, dsr_coef_df,
                                     center = TRUE,
                                     centre_by_round = PSR_CENTRE_BY_ROUND,
                                     listed_pos = NULL,
                                     centre_on_listed = PSR_CENTRE_ON_LISTED) {
  # All three arms MUST get the same listed_pos. osr + dsr are shifted to sum to
  # psr below, so centring them on different keys would push the discrepancy
  # into that shift and silently distort the decomposition rather than erroring.
  # Margin PSR (the authoritative total)
  psr_result <- calculate_psr(skills, coef_df, center = center, centre_by_round = centre_by_round, listed_pos = listed_pos, centre_on_listed = centre_on_listed)

  # Raw offensive and defensive scores
  osr_result <- calculate_psr(skills, osr_coef_df, center = center, centre_by_round = centre_by_round, listed_pos = listed_pos, centre_on_listed = centre_on_listed)
  dsr_result <- calculate_psr(skills, dsr_coef_df, center = center, centre_by_round = centre_by_round, listed_pos = listed_pos, centre_on_listed = centre_on_listed)

  # Additive shift: distribute residual evenly so osr + dsr = psr
  raw_osr <- osr_result$psr
  raw_dsr <- dsr_result$psr
  delta <- (psr_result$psr - raw_osr - raw_dsr) / 2

  psr_result[, osr := raw_osr + delta]
  psr_result[, dsr := raw_dsr + delta]

  psr_result
}


# ============================================================================
# PSV: Per-Game Stat Value
# ============================================================================

#' Calculate Player Stat Value (PSV) from Per-Game Stats
#'
#' Applies the same glmnet coefficients used by PSR to raw single-game stats
#' to produce a per-game margin contribution score. While PSR uses Bayesian
#' smoothed career estimates (\code{_rating} columns), PSV uses actual
#' box-score stats from a single game.
#'
#' @param player_stats A data.table/data.frame of per-game player data with
#'   raw stat columns (e.g. \code{goals}, \code{kicks}, \code{disposals}) and
#'   a \code{tog} column (time-on-ground as a fraction 0-1).
#' @param coef_df A data.frame with columns \code{stat_name} and \code{beta}
#'   (same format as for \code{calculate_psr()}). If an \code{sd} column is
#'   present, raw rates are divided by SD before applying betas.
#' @param tog_adjust Logical. If TRUE (default), divide raw counts by TOG to
#'   get per-full-game rates (matching the scale the coefficients were trained
#'   on). If FALSE, use raw counts directly.
#' @param center Logical. If TRUE (default), subtract the per-round league mean
#'   so PSV represents contribution above the average player that round.
#'
#' @return A data.table with identifier columns plus \code{psv_raw},
#'   \code{psv_p80} (centered per-full-game rate) and \code{psv}
#'   (per-game value = \code{psv_p80 * tog}, matching the \code{epv} scale).
#'
#' @export
calculate_psv <- function(player_stats, coef_df, tog_adjust = TRUE, center = TRUE) {
  dt <- data.table::as.data.table(player_stats)

  if (!all(c("stat_name", "beta") %in% names(coef_df))) {
    cli::cli_abort("{.arg coef_df} must have columns {.val stat_name} and {.val beta}")
  }

  coef_df <- coef_df[coef_df$beta != 0, , drop = FALSE]

  if (nrow(coef_df) == 0) {
    dt[, c("psv_raw", "psv_p80", "psv") := 0]
    id_cols <- intersect(c("player_id", "player_name", "season", "round", "match_id", "team"), names(dt))
    return(dt[, c(id_cols, "psv_raw", "psv_p80", "psv"), with = FALSE])
  }

  # Map stat_name to raw stat columns (direct column names, not _rating)
  stat_cols <- coef_df$stat_name

  # Exclude stats that don't belong in per-game PSV before checking availability
  # (avoids noisy warnings for stats that would be dropped anyway):
  #  - Efficiency %s (redundant — numerator + denominator already in as rate stats)
  #  - bounces (negative coefficient, not causal)
  #  - cond_tog, squad_selection (availability metrics, not performance)
  psv_exclude <- .PSV_EXCLUDE
  keep <- !stat_cols %in% psv_exclude
  coef_df <- coef_df[keep, , drop = FALSE]
  stat_cols <- stat_cols[keep]

  available <- stat_cols %in% names(dt)

  if (sum(available) == 0) {
    cli::cli_abort("No matching stat columns found in data for PSV calculation")
  }
  if (any(!available)) {
    missing <- stat_cols[!available]
    cli::cli_warn("Stat columns not found (skipped): {paste(missing, collapse = ', ')}")
  }

  coef_df <- coef_df[available, , drop = FALSE]
  stat_cols <- stat_cols[available]
  betas <- coef_df$beta
  # Points-scale calibration on the SHARED coefficient vector. PSR is not
  # downstream of PSV -- they are parallel applications of these same betas to
  # rated vs actual stats -- so the betas are the single point that moves both
  # together. PSR converted at 1.579 points per rating point, so new = 1.579*old.
  # Centring and standardisation downstream are both scale-equivariant
  # ((k*x - k*m)/(k*sd)*(k*pooled) = k * result), so the factor survives intact.
  if (is.finite(PSV_POINTS_SCALE) && !isTRUE(all.equal(PSV_POINTS_SCALE, 1))) {
    betas <- betas * PSV_POINTS_SCALE
  }

  # Use _oadj (opponent-adjusted) columns when available, fall back to raw
  oadj_cols <- paste0(stat_cols, "_oadj")
  has_oadj <- all(oadj_cols %in% names(dt))
  use_cols <- if (has_oadj) oadj_cols else stat_cols

  # Extract stat values
  mat <- as.matrix(dt[, use_cols, with = FALSE])
  mat[is.na(mat)] <- 0

  # TOG-adjust: divide counts by TOG to get per-full-game rates
  if (tog_adjust && "tog" %in% names(dt)) {
    tog_vec <- as.numeric(dt$tog)
    tog_vec[is.na(tog_vec) | tog_vec <= 0] <- 1
    mat <- mat / tog_vec
  }

  # Standardize using SDs from coefficient file (same scale as PSR training)
  if ("sd" %in% names(coef_df)) {
    sds <- coef_df$sd
    sds[sds == 0 | is.na(sds)] <- 1
    mat <- sweep(mat, 2, sds, "/")
  }

  dt[, psv_raw := as.numeric(mat %*% betas)]

  # A MISSING position_group used to skip centring in silence: the guard below
  # tests `"position_group" %in% names(dt)`, so a frame without it fell straight
  # through with no abort and no warning. That is the same shape as the bug this
  # whole guard was written for -- the original PSV check tested for `round`
  # when the frame carries `round_number`, quietly centred nothing, and left the
  # spread unchanged at 0.812 while every downstream check passed.
  #
  # It is a live risk, not theoretical: load_player_stats() returns `position`,
  # NOT `position_group`, so a caller passing that frame directly gets silently
  # uncentred PSV. Fail loud instead, and name the column we actually looked for.
  if (center && isTRUE(PSV_LEVEL_CENTRE) && !"position_group" %in% names(dt)) {
    cli::cli_abort(c(
      "Cannot centre PSV by position: no {.field position_group} column.",
      "i" = "Found position-ish columns: {.val {grep('^pos|position', names(dt), value = TRUE)}}",
      "i" = "{.fun load_player_stats} returns {.field position}, not {.field position_group} -- join the roster first.",
      "x" = "Refusing to return uncentred PSV that callers will treat as centred."
    ))
  }

  # Resolve position column for centering
  pos_col <- if ("lineup_position" %in% names(dt)) "lineup_position"
             else if ("position_group" %in% names(dt)) "position_group"
             else if ("pos_group" %in% names(dt)) "pos_group"
             else NULL

  # Optionally merge the arbitrary left/right mirrors, so the role stage groups
  # on `lineup_group` (16) rather than raw `lineup_position` (21). Must match
  # EPV's `.position_adjust()` grouping -- the two value layers have to be built
  # the same way or TORP blends halves that disagree about what a role is.
  if (isTRUE(ROLE_USE_LINEUP_GROUP) && identical(pos_col, "lineup_position")) {
    dt[, .role_grp := .collapse_lineup_group(lineup_position)]
    if (all(is.na(dt$.role_grp))) {
      cli::cli_abort("PSV role key is entirely NA after collapsing -- nothing would be centred.")
    }
    pos_col <- ".role_grp"
  }

  # PSV's role stage pools across ALL history: `by = pos_col` with no season or
  # round. Every wing across six seasons is centred against one mean, so each
  # position averages zero across the dataset while any single round stays
  # skewed -- and a 2021 value is centred using 2026 games. Same defect as the
  # one PSR_CENTRE_BY_ROUND fixed. This flag adds the (season, round) grouping.
  .psv_role_by <- if (isTRUE(PSV_ROLE_CENTRE_BY_ROUND)) {
    rc <- intersect(c("round", "round_number"), names(dt))[1]
    if (is.na(rc) || !"season" %in% names(dt)) {
      cli::cli_abort(c(
        "PSV_ROLE_CENTRE_BY_ROUND is TRUE but the frame lacks season/round.",
        "x" = "Refusing to pool history while claiming to centre per round."
      ))
    }
    c("season", rc, pos_col)
  } else {
    pos_col
  }
  if (center && is.null(pos_col)) {
    # Reachable only when PSV_LEVEL_CENTRE is FALSE -- the abort above already
    # rejected a centring request with no position_group. Previously this
    # printed "using global mean subtraction" and then the function aborted
    # anyway, advertising a fallback the code no longer allows to complete.
    cli::cli_warn("No position column found for PSV role centering; using global mean subtraction")
  }

  # Center by position to produce psv_p80, a centered per-full-game (per-80)
  # rate — mirrors the EPV approach in player_credit.R Step 7:
  #   1. psv_raw is a per-full-game rate (stats already divided by TOG)
  #   2. subtract TOG-weighted positional mean → centered per-80 rate (psv_p80)
  # The per-game value (psv = psv_p80 * tog) is derived below so that psv is on
  # the same scale as epv (per-game), keeping torp_value = 0.5*epv + 0.5*psv
  # apples-to-apples (issue #80).
  if (center && !is.null(pos_col) && "tog" %in% names(dt)) {
    dt[, .tog_wt := pmax(as.numeric(tog), 0.1)]
    dt[!is.na(get(pos_col)), psv_p80 := psv_raw - weighted.mean(psv_raw, .tog_wt, na.rm = TRUE),
       by = c(.psv_role_by)]
    dt[is.na(get(pos_col)), psv_p80 := psv_raw - weighted.mean(psv_raw, .tog_wt, na.rm = TRUE)]
    dt[, .tog_wt := NULL]
  } else if (center && !is.null(pos_col)) {
    dt[!is.na(get(pos_col)), psv_p80 := psv_raw - mean(psv_raw, na.rm = TRUE), by = c(.psv_role_by)]
    dt[is.na(get(pos_col)), psv_p80 := psv_raw - mean(psv_raw, na.rm = TRUE)]
  } else if (center) {
    dt[, psv_p80 := psv_raw - mean(psv_raw, na.rm = TRUE)]
  } else {
    dt[, psv_p80 := psv_raw]
  }

  # Second pass: centre on the LISTED position bucket, per (season, round).
  #
  # The block above centres on lineup_position -- the weekly on-field role --
  # and does it to machine precision (1.7e-15 across all 20 roles). But that
  # removes the ROLE effect, not the PLAYER-TYPE one: key forwards are a subset
  # of the players filling forward roles and sit above those roles' own means.
  # Measured on 2026, psv_p80 still spans 0.812 across the six listed buckets
  # (key_fwd +0.517, med_def -0.295) with every lineup_position at zero. This is
  # the exact defect found at the EPV layer on 2026-07-29, at ~28% the size, and
  # it is why fixing EPV alone left per-game torp_value carrying ~0.41 of
  # positional bias -- all of it from here.
  #
  # TOG-weighted and per-round for the same reasons as centre_epv_by_position():
  # psv is psv_p80 * tog, so the TOG-weighted mean is the quantity aggregation
  # actually sees, and a full-history mean would centre early rounds with games
  # that had not been played.
  #
  # The round column is `round` in player_game data but `round_number` in
  # player_stats, which is what this function is actually fed. The first version
  # of this guard tested only for "round", so it silently did nothing and the
  # measured spread came back unchanged at 0.812 -- a skipped normalisation that
  # every downstream check would have passed. Hence: resolve both spellings, and
  # ABORT rather than skip if centring was asked for and cannot be done.
  .psv_round_col <- intersect(c("round", "round_number"), names(dt))[1]

  if (center && isTRUE(PSV_LEVEL_CENTRE) && "position_group" %in% names(dt) &&
      (is.na(.psv_round_col) || !"season" %in% names(dt))) {
    cli::cli_abort(c(
      "Cannot centre PSV by listed position: need {.field season} and {.field round}/{.field round_number}.",
      "i" = "Found: {.val {intersect(c('season','round','round_number'), names(dt))}}",
      "x" = "Refusing to return uncentred PSV that callers will treat as centred."
    ))
  }
  if (center && isTRUE(PSV_LEVEL_CENTRE) && "position_group" %in% names(dt)) {
    data.table::setnames(dt, .psv_round_col, ".lc_round")
    dt[, .lc_bucket := .collapse_listed_position(position_group)]
    dt[, .lc_w := if ("tog" %in% names(dt)) pmax(as.numeric(tog), 0.1) else 1]
    .lc_wmean <- function(x, w) {
      ok <- is.finite(x) & is.finite(w) & w > 0
      if (!any(ok)) return(NA_real_)
      sum(x[ok] * w[ok]) / sum(w[ok])
    }

    for (cc in intersect(c("psv_p80", "osv_p80", "dsv_p80"), names(dt))) {
      dt[!is.na(.lc_bucket), (cc) := get(cc) - .lc_wmean(get(cc), .lc_w),
         by = .(season, .lc_round, .lc_bucket)]
    }
    # Count BOTH populations, like centre_epv_by_position() does. A join that
    # silently loses position_group leaves those rows at role-level centring
    # only -- the second, player-type correction never reaches them -- and
    # counting only the unmapped ones reports nothing at all in that case.
    n_missing  <- sum(is.na(dt$position_group))
    n_unmapped <- sum(!is.na(dt$position_group) & is.na(dt$.lc_bucket))
    if (n_missing > 0) {
      cli::cli_alert_danger(
        "{n_missing} player-game{?s} {?has/have} no {.field position_group} and {?was/were} left UNCENTRED at the PSV layer.")
    }
    if (n_unmapped > 0) {
      cli::cli_alert_danger(
        "{n_unmapped} player-game{?s} carr{?ies/y} an UNMAPPED {.field position_group} and {?was/were} left UNCENTRED at the PSV layer.")
    }
    dt[, c(".lc_bucket", ".lc_w") := NULL]
    data.table::setnames(dt, ".lc_round", .psv_round_col)
  }

  # Derive per-game value: psv = psv_p80 * tog (per-game scale, matches epv).
  # Must stay AFTER the centring above, or psv keeps the uncentred level while
  # psv_p80 looks corrected.
  if ("tog" %in% names(dt)) {
    dt[, psv := psv_p80 * pmax(as.numeric(tog), 0.1)]
  } else {
    dt[, psv := psv_p80]
  }

  id_cols <- intersect(
    c("player_id", "player_name", "season", "round", "match_id",
      "team", "opponent", "position_group", "tog"),
    names(dt)
  )

  dt[, c(id_cols, "psv_raw", "psv_p80", "psv"), with = FALSE]
}


#' Calculate PSV with Offensive/Defensive Decomposition
#'
#' Applies offensive and defensive coefficient models to per-game stats,
#' producing \code{psv}, \code{osv}, and \code{dsv} columns where
#' \code{osv + dsv = psv}.
#'
#' @inheritParams calculate_psv
#' @param osr_coef_df Coefficient data.frame for the offensive model.
#' @param dsr_coef_df Coefficient data.frame for the defensive model.
#'
#' @return A data.table with identifier columns plus \code{psv_raw},
#'   \code{psv_p80}, \code{psv}, \code{osv_p80}, \code{osv}, \code{dsv_p80},
#'   \code{dsv}. The \code{*_p80} columns are centered per-full-game rates and
#'   the unsuffixed columns are per-game values (\code{*_p80 * tog}).
#'
#' @export
calculate_psv_components <- function(player_stats, coef_df, osr_coef_df,
                                      dsr_coef_df, tog_adjust = TRUE,
                                      center = TRUE) {
  psv_result <- calculate_psv(player_stats, coef_df, tog_adjust = tog_adjust,
                               center = center)
  osv_result <- calculate_psv(player_stats, osr_coef_df, tog_adjust = tog_adjust,
                               center = center)
  dsv_result <- calculate_psv(player_stats, dsr_coef_df, tog_adjust = tog_adjust,
                               center = center)

  # Decompose on the per-80 (psv_p80) scale so osv_p80 + dsv_p80 = psv_p80,
  # using an additive shift, then derive the per-game values via * tog.
  raw_osv <- osv_result$psv_p80
  raw_dsv <- dsv_result$psv_p80
  delta <- (psv_result$psv_p80 - raw_osv - raw_dsv) / 2

  psv_result[, osv_p80 := raw_osv + delta]
  psv_result[, dsv_p80 := raw_dsv + delta]

  if ("tog" %in% names(psv_result)) {
    tog_mult <- pmax(as.numeric(psv_result$tog), 0.1)
    psv_result[, osv := osv_p80 * tog_mult]
    psv_result[, dsv := dsv_p80 * tog_mult]
  } else {
    psv_result[, osv := osv_p80]
    psv_result[, dsv := dsv_p80]
  }

  psv_result
}


#' Convenience wrapper to compute PSV from coefficient files
#'
#' Loads the margin, offensive, and defensive coefficient CSVs from
#' \code{inst/extdata} and calls \code{\link{calculate_psv_components}}.
#'
#' @inheritParams calculate_psv
#' @param psr_coef_path Path to the margin PSR coefficient CSV. If NULL,
#'   searches \code{inst/extdata/psr_coefficients.csv}.
#' @param comp Competition: "AFLM" (default) or "AFLW". Only used to resolve
#'   \code{psr_coef_path} when it is NULL; ignored if an explicit path is
#'   given.
#'
#' @return A data.table with \code{psv}, \code{osv}, \code{dsv} columns.
#'
#' @keywords internal
.compute_psv <- function(player_stats, psr_coef_path = NULL, tog_adjust = TRUE,
                          center = TRUE, comp = "AFLM") {
  if (is.null(psr_coef_path)) {
    psr_coef_path <- .find_psr_coef_path(comp = comp)
  }

  if (!nzchar(psr_coef_path) || !file.exists(psr_coef_path)) {
    cli::cli_warn("PSR coefficient file not found: {psr_coef_path}")
    return(NULL)
  }

  coef_df <- utils::read.csv(psr_coef_path)

  coef_dir <- dirname(psr_coef_path)
  osr_path <- file.path(coef_dir, .comp_coef_filename("osr_coefficients.csv", comp))
  dsr_path <- file.path(coef_dir, .comp_coef_filename("dsr_coefficients.csv", comp))

  if (file.exists(osr_path) && file.exists(dsr_path)) {
    osr_coef_df <- utils::read.csv(osr_path)
    dsr_coef_df <- utils::read.csv(dsr_path)
    calculate_psv_components(player_stats, coef_df, osr_coef_df, dsr_coef_df,
                              tog_adjust = tog_adjust, center = center)
  } else {
    cli::cli_inform("OSR/DSR coefficient files not found -- computing PSV only (no osv/dsv decomposition)")
    calculate_psv(player_stats, coef_df, tog_adjust = tog_adjust, center = center)
  }
}


#' Explain a Player's Game Value (EPV + PSV Breakdown)
#'
#' Shows both the possession-value (EPV) and stat-value (PSV) decomposition
#' for a player's game performance. EPV comes from play-by-play chain data;
#' PSV comes from applying glmnet coefficients to box-score stats.
#'
#' @param player_id Player ID string (e.g. \code{"CD_I1001024"}).
#' @param match_id Match ID string (e.g. \code{"CD_M20260140102"}).
#' @param player_stats Optional pre-loaded player stats data.table. If NULL,
#'   loads from API via \code{load_player_stats()}.
#' @param season Season year (used if \code{player_stats} is NULL).
#' @param per80 Logical. If TRUE, show per-80-minute contributions.
#'   Default TRUE.
#'
#' @return Invisibly returns a list with \code{epv} (EPV summary data.table)
#'   and \code{psv} (PSV per-stat breakdown data.table).
#'
#' @export
explain_player_game <- function(player_id, match_id, player_stats = NULL,
                                 season = NULL, per80 = TRUE) {

  if (is.null(season)) season <- as.integer(substr(match_id, 5, 8))
  pid <- as.character(player_id)
  mid <- as.character(match_id)

  # --- Load player stats ---
  if (is.null(player_stats)) {
    player_stats <- data.table::as.data.table(load_player_stats(season))
  }
  ps <- data.table::as.data.table(player_stats)
  ps[, player_id := as.character(player_id)]
  ps[, match_id := as.character(match_id)]

  row <- ps[ps$player_id == pid & ps$match_id == mid]
  if (nrow(row) == 0) cli::cli_abort("No data found for player {player_id} in match {match_id}")
  if (nrow(row) > 1) row <- row[1]

  # TOG
  tog_val <- if ("tog" %in% names(row)) {
    as.numeric(row$tog)
  } else if ("time_on_ground_percentage" %in% names(row)) {
    pmax(as.numeric(row$time_on_ground_percentage) / 100, 0.1)
  } else 1
  if (is.na(tog_val) || tog_val <= 0) tog_val <- 1

  pname <- if ("player_name" %in% names(row)) row$player_name[1] else player_id

  # Derive team/opp if not present (player_stats uses home/away + team_status)
  if (!"team" %in% names(row) &&
      all(c("home_team_name", "away_team_name", "team_status") %in% names(row))) {
    row[, team := data.table::fifelse(team_status == "home", home_team_name, away_team_name)]
    row[, opp := data.table::fifelse(team_status == "home", away_team_name, home_team_name)]
  }
  team_nm <- if ("team" %in% names(row)) row$team[1] else ""
  opp_nm <- ""
  for (ocol in c("opp", "opponent")) {
    if (ocol %in% names(row)) { opp_nm <- row[[ocol]][1]; break }
  }

  # =========================================================================
  # SECTION 1: EPV (from player game ratings — chain-based possession value)
  # =========================================================================
  cli::cli_h2("{pname} | {team_nm} v {opp_nm} | TOG {round(tog_val * 100)}%")

  suffix <- if (per80) " (per 80 min)" else " (total)"
  epv_summary <- NULL
  pgr <- tryCatch(load_player_game_ratings(season), error = function(e) NULL)
  if (!is.null(pgr)) {
    pgr_dt <- data.table::as.data.table(pgr)
    pgr_dt[, player_id := as.character(player_id)]
    pgr_dt[, match_id := as.character(match_id)]
    epv_row <- pgr_dt[pgr_dt$player_id == pid & pgr_dt$match_id == mid]

    if (nrow(epv_row) > 0) {
      epv_row <- epv_row[1]

      # Pick total or p80 columns based on per80 flag
      if (per80) {
        comp_cols <- c(epv_recv = "epv_recv_p80", epv_disp = "epv_disp_p80",
                       epv_spoil = "epv_spoil_p80", epv_hitout = "epv_hitout_p80")
        epv_total_col <- "epv_p80"
      } else {
        comp_cols <- c(epv_recv = "epv_recv", epv_disp = "epv_disp",
                       epv_spoil = "epv_spoil", epv_hitout = "epv_hitout")
        epv_total_col <- "epv"
      }

      # Build EPV summary table
      epv_vals <- vapply(comp_cols, function(col) {
        if (col %in% names(epv_row)) round(as.numeric(epv_row[[col]]), 1) else NA_real_
      }, numeric(1))
      epv_total <- if (epv_total_col %in% names(epv_row)) {
        round(as.numeric(epv_row[[epv_total_col]]), 1)
      } else {
        round(sum(epv_vals, na.rm = TRUE), 1)
      }

      epv_summary <- data.table::data.table(
        component = c(names(comp_cols), "TOTAL"),
        epv = c(epv_vals, epv_total)
      )

      # Add position context: percentile among same-position players that round
      position_col <- intersect(c("position_group", "position"), names(epv_row))
      if (length(position_col) > 0) {
        pos <- epv_row[[position_col[1]]]
        pos_peers <- pgr_dt[pgr_dt$season == season &
                            pgr_dt[[position_col[1]]] == pos]
        if (nrow(pos_peers) > 1 && epv_total_col %in% names(pos_peers)) {
          pctile <- round(mean(pos_peers[[epv_total_col]] <= epv_total, na.rm = TRUE) * 100)
          epv_summary[component == "TOTAL", pctile := pctile]
        }
      }

      # Also grab PSV/OSV/DSV from the same row if available
      psv_from_pgr <- list()
      for (vcol in c("psv", "osv", "dsv", "torp_value")) {
        if (vcol %in% names(epv_row)) psv_from_pgr[[vcol]] <- round(as.numeric(epv_row[[vcol]]), 1)
      }

      cli::cli_h3("EPV{suffix} -- from play-by-play chains")
      print(epv_summary, row.names = FALSE)

      if (length(psv_from_pgr) > 0) {
        psv_line <- paste(names(psv_from_pgr), "=", psv_from_pgr, collapse = " | ")
        cat(sprintf("  PSV summary: %s\n", psv_line))
      }
    } else {
      cli::cli_inform("No EPV game ratings found for this match (not yet released?)")
    }
  }

  # =========================================================================
  # SECTION 2: PSV (stat-by-stat breakdown from glmnet coefficients)
  # =========================================================================
  cli::cli_h3("PSV{suffix} -- stat-by-stat breakdown")

  # Load coefficients
  psr_path <- .find_psr_coef_path()
  if (!nzchar(psr_path)) cli::cli_abort("PSR coefficient file not found")

  coef_df <- utils::read.csv(psr_path)
  coef_dir <- dirname(psr_path)
  osr_path <- file.path(coef_dir, "osr_coefficients.csv")
  dsr_path <- file.path(coef_dir, "dsr_coefficients.csv")
  has_components <- file.exists(osr_path) && file.exists(dsr_path)
  if (has_components) {
    osr_coefs <- utils::read.csv(osr_path)
    dsr_coefs <- utils::read.csv(dsr_path)
  }

  # PSV exclude list
  psv_exclude <- .PSV_EXCLUDE

  # Build breakdown for a single coefficient set
  .breakdown <- function(cdf, label) {
    cdf <- cdf[cdf$beta != 0 & !cdf$stat_name %in% psv_exclude, , drop = FALSE]
    available <- cdf$stat_name %in% names(row)
    cdf <- cdf[available, , drop = FALSE]

    raw_vals <- vapply(cdf$stat_name, function(s) {
      v <- as.numeric(row[[s]]); if (is.na(v)) 0 else v
    }, numeric(1))

    rate_vals <- raw_vals / tog_val
    sds <- if ("sd" %in% names(cdf)) { s <- cdf$sd; s[s == 0 | is.na(s)] <- 1; s } else rep(1, nrow(cdf))
    contributions <- (rate_vals / sds) * cdf$beta

    data.table::data.table(
      stat = cdf$stat_name,
      contribution = round(contributions, 2)
    ) |> data.table::setnames("contribution", label)
  }

  psv_result <- .breakdown(coef_df, "psv")
  if (has_components) {
    osv_dt <- .breakdown(osr_coefs, "osv")
    dsv_dt <- .breakdown(dsr_coefs, "dsv")
    psv_result <- merge(psv_result, osv_dt, by = "stat", all = TRUE)
    psv_result <- merge(psv_result, dsv_dt, by = "stat", all = TRUE)
  }

  # Add raw values and rates
  psv_result[, raw_value := vapply(stat, function(s) {
    v <- as.numeric(row[[s]]); if (is.na(v)) 0 else v
  }, numeric(1))]
  psv_result[, rate := round(raw_value / tog_val, 1)]

  # Reorder and sort
  front_cols <- c("stat", "raw_value", "rate", "psv")
  if (has_components) front_cols <- c(front_cols, "osv", "dsv")
  data.table::setcolorder(psv_result, intersect(front_cols, names(psv_result)))
  data.table::setorderv(psv_result, "psv", order = -1L)

  # Per-game totals (not per-80) — multiply by tog
  if (!per80) {
    for (col in intersect(c("psv", "osv", "dsv"), names(psv_result)))
      psv_result[, (col) := round(get(col) * tog_val, 2)]
  }

  total_psv <- sum(psv_result$psv, na.rm = TRUE)
  cat(sprintf("  Total PSV: %.1f\n", total_psv))

  # Filter out zero-contribution stats for cleaner output
  psv_result <- psv_result[psv != 0 | raw_value != 0]

  print(psv_result, nrows = nrow(psv_result))
  invisible(list(epv = epv_summary, psv = psv_result))
}


#' Show a Player's Biggest Plays from PBP
#'
#' Pulls play-by-play rows for a player and computes the EPV credit each play
#' generated (using the same formula as [create_player_game_data()]). Shows
#' the surrounding context (1 row before, 1 after) for each top play so you
#' can see how the passage unfolded.
#'
#' @param player Character player name (partial match OK) or player_id string.
#' @param season Season year(s) to search. Default is current season.
#' @param match_id Optional match ID to filter to a single game.
#' @param top_n Number of top plays to show per role. Default 5.
#' @param context Number of rows before/after each play to show. Default 1.
#' @param pbp_data Optional pre-loaded PBP data. If NULL, loads automatically.
#'
#' @return Invisibly returns a list with \code{as_receiver} and
#'   \code{as_disposer} data.tables of the biggest plays.
#'
#' @export
explain_player_plays <- function(player,
                                  season = get_afl_season(),
                                  match_id = NULL,
                                  top_n = 5,
                                  context = 1,
                                  pbp_data = NULL) {

  # Resolve player
  if (grepl("^CD_I", player)) {
    pid <- player
    pname <- player
  } else {
    resolved <- resolve_player(player)
    pid <- resolved$player_id
    pname <- resolved$player_name
  }

  # Load PBP
  if (is.null(pbp_data)) {
    pbp_data <- load_pbp(season)
  }
  dt <- data.table::as.data.table(pbp_data)
  data.table::setorder(dt, match_id, display_order)

  if (!is.null(match_id)) {
    dt <- dt[dt$match_id %in% match_id]
  }

  # Columns to show in context view
  ctx_cols <- c("display_order", "period", "player_name", "team",
                "description", "play_type", "phase_of_play",
                "x", "y", "pos_team", "delta_epv", "exp_pts",
                "lead_player", "lead_desc_tot")
  ctx_cols <- intersect(ctx_cols, names(dt))

  # Add a global row index for context lookups
  dt[, .row_idx := .I]

  # Load EPV params (same as create_player_game_data uses)
  p <- default_epv_params()

  # Helper: get context rows around a set of row indices
  .get_context <- function(idx, n = context) {
    offsets <- seq(-n, n)
    all_idx <- sort(unique(unlist(lapply(idx, function(i) i + offsets))))
    all_idx <- all_idx[all_idx >= 1L & all_idx <= nrow(dt)]
    out <- dt[all_idx, c(".row_idx", ctx_cols), with = FALSE]
    # Mark which rows are the target plays
    out[, is_play := .row_idx %in% idx]
    out
  }

  # --- Receiving plays (player is lead_player_id = next to touch the ball) ---
  recv_plays <- dt[dt$lead_player_id == pid]
  if (nrow(recv_plays) > 0) {
    recv_plays[, is_intercept_mark := pos_team == -1L & grepl("ted Mark|Mark On", lead_desc_tot)]
    recv_plays[, recv_credit := data.table::fifelse(
      is_intercept_mark,
      ((p$recv_neg_mult * delta_epv * pos_team) + p$recv_neg_offset) * p$recv_intercept_mark_scale,
      data.table::fifelse(
        pos_team == -1L,
        ((p$recv_neg_mult * delta_epv * pos_team) + p$recv_neg_offset) * p$recv_scale,
        ((p$recv_pos_mult * delta_epv * pos_team) + p$recv_pos_offset) * p$recv_scale
      )
    )]
    recv_plays[, is_intercept_mark := NULL]
  }

  # --- Disposal plays (player is player_id = disposer) ---
  disp_plays <- dt[dt$player_id == pid]
  if (nrow(disp_plays) > 0) {
    disp_plays[, disp_credit := data.table::fifelse(
      pos_team == -1,
      delta_epv + p$disp_neg_offset,
      delta_epv + p$disp_pos_offset
    ) * p$disp_scale]
  }

  # Print
  cli::cli_h1("{pname} | Biggest Plays (season {paste(season, collapse=',')})")

  # --- Receiving output ---
  if (nrow(recv_plays) > 0) {
    cli::cli_h2("As Receiver (top {min(top_n, nrow(recv_plays))} of {nrow(recv_plays)} plays)")

    top_recv <- recv_plays[order(-abs(recv_credit))][seq_len(min(top_n, nrow(recv_plays)))]
    top_recv_idx <- top_recv$.row_idx

    # Print each top play with context
    for (i in seq_along(top_recv_idx)) {
      r <- top_recv[i]
      round_lbl <- if ("round_week" %in% names(r)) r$round_week else "?"
      opp_lbl <- if (all(c("home_away", "away_team_name", "home_team_name") %in% names(r))) {
        if (r$home_away == "Home") r$away_team_name else r$home_team_name
      } else "?"
      cli::cli_h3("Play {i}: recv_credit={round(r$recv_credit, 2)} | Rd {round_lbl} v {opp_lbl} | Q{r$period}")
      ctx <- .get_context(top_recv_idx[i])
      # Round numeric cols for display
      for (nc in intersect(c("delta_epv", "exp_pts", "x", "y"), names(ctx))) {
        ctx[, (nc) := round(get(nc), 1)]
      }
      # Mark the target row
      ctx[, marker := ifelse(is_play, ">>>", "")]
      ctx[, is_play := NULL]
      ctx[, .row_idx := NULL]
      print(ctx, row.names = FALSE)
      cat("\n")
    }

    cat(sprintf("  Total recv credit: %.1f across %d plays\n",
                sum(recv_plays$recv_credit, na.rm = TRUE), nrow(recv_plays)))

    # Per-game summary
    recv_by_game <- recv_plays[, .(
      plays = .N,
      total_credit = round(sum(recv_credit, na.rm = TRUE), 1),
      max_play = round(max(recv_credit, na.rm = TRUE), 1),
      min_play = round(min(recv_credit, na.rm = TRUE), 1)
    ), by = .(match_id, round_week)]
    data.table::setorderv(recv_by_game, "total_credit", order = -1L)
    cli::cli_h3("Per-game recv credit summary")
    print(recv_by_game, row.names = FALSE)
  } else {
    cli::cli_inform("No receiving plays found")
  }

  # --- Disposal output ---
  if (nrow(disp_plays) > 0) {
    cli::cli_h2("As Disposer (top {min(top_n, nrow(disp_plays))} of {nrow(disp_plays)} plays)")

    top_disp <- disp_plays[order(-abs(disp_credit))][seq_len(min(top_n, nrow(disp_plays)))]
    top_disp_idx <- top_disp$.row_idx

    for (i in seq_along(top_disp_idx)) {
      r <- top_disp[i]
      round_lbl <- if ("round_week" %in% names(r)) r$round_week else "?"
      opp_lbl <- if (all(c("home_away", "away_team_name", "home_team_name") %in% names(r))) {
        if (r$home_away == "Home") r$away_team_name else r$home_team_name
      } else "?"
      cli::cli_h3("Play {i}: disp_credit={round(r$disp_credit, 2)} | Rd {round_lbl} v {opp_lbl} | Q{r$period}")
      ctx <- .get_context(top_disp_idx[i])
      for (nc in intersect(c("delta_epv", "exp_pts", "x", "y"), names(ctx))) {
        ctx[, (nc) := round(get(nc), 1)]
      }
      ctx[, marker := ifelse(is_play, ">>>", "")]
      ctx[, is_play := NULL]
      ctx[, .row_idx := NULL]
      print(ctx, row.names = FALSE)
      cat("\n")
    }

    cat(sprintf("  Total disp credit: %.1f across %d plays\n",
                sum(disp_plays$disp_credit, na.rm = TRUE), nrow(disp_plays)))
  } else {
    cli::cli_inform("No disposal plays found")
  }

  # Clean up temp column
  dt[, .row_idx := NULL]

  invisible(list(as_receiver = recv_plays, as_disposer = disp_plays))
}


#' Explain a Player's EPR Calculation (Per-Game Trace)
#'
#' Traces the exact per-game inputs to the EPR calculation: shows each game's
#' \code{epv_recv_adj}, \code{epv_disp_adj}, etc. (position-adjusted per-80-min
#' rates), the decay weight for each game, and how Bayesian shrinkage produces
#' the final EPR components.
#'
#' @param player Character player name (partial match OK) or player_id string.
#' @param season_val Season year. Default is current season.
#' @param round_val Round number. Default is next round.
#' @param top_n Number of most recent games to show. Default 15.
#'
#' @return Invisibly returns a list with \code{game_trace} (per-game data.table)
#'   and \code{shrinkage} (named list of component calculations).
#'
#' @export
explain_epr <- function(player,
                         season_val = get_afl_season(type = "current"),
                         round_val = get_afl_week(type = "next"),
                         top_n = 15) {

  # Resolve player
  if (grepl("^CD_I", player)) {
    pid <- player
    pname <- player
  } else {
    resolved <- resolve_player(player)
    pid <- resolved$player_id
    pname <- resolved$player_name
  }

  # Load player game data (the raw _adj values that feed EPR)
  pgd <- data.table::as.data.table(load_player_game_data(TRUE))

  # Build match_ref and date_val the same way calculate_epr does
  fixtures <- load_fixtures(TRUE)
  gwk <- sprintf("%02d", round_val)
  match_ref <- paste0("CD_M", season_val, "014", gwk)

  date_val <- fixtures[fixtures$season == season_val & fixtures$round_number == round_val, ]
  date_val <- min(as.Date(date_val$utc_start_time), na.rm = TRUE)

  # Filter to this player's games before the reference round
  data.table::setkey(pgd, match_id)
  dt <- pgd[match_id <= match_ref & player_id == pid]
  if (nrow(dt) == 0) cli::cli_abort("No games found for {pname} before {season_val} round {round_val}")

  dt[, days_diff := as.numeric(as.Date(date_val) - as.Date(utc_start_time))]
  dt <- dt[days_diff >= 0]

  # Decay weights (same constants as calculate_epr_stats)
  dt[, `:=`(
    wt_recv   = exp(-days_diff / EPR_DECAY_RECV),
    wt_disp   = exp(-days_diff / EPR_DECAY_DISP),
    wt_spoil  = exp(-days_diff / EPR_DECAY_SPOIL),
    wt_hitout = exp(-days_diff / EPR_DECAY_HITOUT),
    # Mirror production's tog_safe (player_ratings.R:303) exactly, including
    # the NA -> 100 imputation. Without it a single game with a missing
    # time_on_ground_percentage gives tog = NA, which propagates through
    # wt_gms_* and the aggregate sums and makes the player's whole epr_* NA --
    # while production, having imputed, reports a number. Dormant today (zero
    # NAs in the current player_game_data) and that is exactly why it would
    # have gone unnoticed until the one game that has one.
    tog = pmax(data.table::fifelse(
      is.na(time_on_ground_percentage), 100, time_on_ground_percentage) / 100, 0.1)
  )]

  # Pick the SAME columns calculate_epr_stats() picks (player_ratings.R:265).
  # It prefers the opponent-adjusted `_oadj` channels when the frame carries
  # them and falls back to `_adj`; the production pipeline runs
  # adjust_epv_for_opponents() before EPR, so the published numbers ARE
  # opponent-adjusted. This function hardcoded `_adj`, which meant its trace
  # silently omitted that adjustment while a comment two lines down claimed it
  # was "same as calculate_epr_stats". Same claim, now true.
  has_oadj <- all(c("epv_recv_oadj", "epv_disp_oadj",
                    "epv_spoil_oadj", "epv_hitout_oadj") %in% names(dt))
  sfx <- if (has_oadj) "_oadj" else "_adj"
  c_recv   <- paste0("epv_recv", sfx)
  c_disp   <- paste0("epv_disp", sfx)
  c_spoil  <- paste0("epv_spoil", sfx)
  c_hitout <- paste0("epv_hitout", sfx)

  # Show per-game trace
  cli::cli_h1("{pname} | EPR Calculation Trace")
  cli::cli_h2("Per-game inputs (position-adjusted per-80-min rates)")

  # Extract via [[ ]] into plain vectors and order those, rather than calling
  # get() inside the j-expression. get() inside dt[i, j] breaks data.table's
  # fast column-reference path -- documented in this tree's own R/data.table
  # gotchas after one such line left ~5.4GB unreclaimed on a table whose output
  # was ~60MB. Irrelevant at this scale (one player's career, a few hundred
  # rows) but there is no reason to plant the pattern where someone copies it.
  ord <- order(-dt$season, -dt$round)
  v_recv   <- dt[[c_recv]][ord]
  v_disp   <- dt[[c_disp]][ord]
  v_spoil  <- dt[[c_spoil]][ord]
  v_hitout <- dt[[c_hitout]][ord]
  v_tog    <- dt$tog[ord]

  show_dt <- data.table::data.table(
    season = dt$season[ord], round = dt$round[ord],
    team = dt$team[ord], opponent = dt$opponent[ord], tog = v_tog,
    recv_adj   = round(v_recv, 2),
    disp_adj   = round(v_disp, 2),
    spoil_adj  = round(v_spoil, 2),
    hitout_adj = round(v_hitout, 2),
    recv_total = round(v_recv * v_tog, 2),
    days_ago = dt$days_diff[ord],
    wt_recv = round(dt$wt_recv[ord], 3)
  )

  cat(sprintf("  Showing %d of %d career games:\n", min(top_n, nrow(show_dt)), nrow(dt)))
  cat(sprintf("  Channels: %s%s\n", sfx,
              if (has_oadj) " (opponent-adjusted, as production uses)" else
                " (NOT opponent-adjusted -- frame carries no _oadj columns)"))
  print(head(show_dt, top_n), row.names = FALSE)

  # Compute the aggregates (same as calculate_epr_stats -- TOG-weighted)
  # the channel is a per-80 rate; multiply by tog for the game total, weight by decay
  recv_sum   <- sum(dt[[c_recv]]   * dt$tog * dt$wt_recv,   na.rm = TRUE)
  disp_sum   <- sum(dt[[c_disp]]   * dt$tog * dt$wt_disp,   na.rm = TRUE)
  spoil_sum  <- sum(dt[[c_spoil]]  * dt$tog * dt$wt_spoil,  na.rm = TRUE)
  hitout_sum <- sum(dt[[c_hitout]] * dt$tog * dt$wt_hitout, na.rm = TRUE)

  # Denominator is weighted minutes (wt * tog), not weighted games
  wt_gms_recv   <- sum(dt$wt_recv * dt$tog, na.rm = TRUE)
  wt_gms_disp   <- sum(dt$wt_disp * dt$tog, na.rm = TRUE)
  wt_gms_spoil  <- sum(dt$wt_spoil * dt$tog, na.rm = TRUE)
  wt_gms_hitout <- sum(dt$wt_hitout * dt$tog, na.rm = TRUE)

  # Bayesian shrinkage (use actual per-component constants)
  loading <- EPR_LOADING_DEFAULT
  prior_gms <- c(recv = EPR_PRIOR_GAMES_RECV, disp = EPR_PRIOR_GAMES_DISP,
                  spoil = EPR_PRIOR_GAMES_SPOIL, hitout = EPR_PRIOR_GAMES_HITOUT)
  # Call the production shrinkage, do not retype it. These four lines used to
  # spell out `(loading * sum + prior * rate) / (wt + prior)` by hand -- the
  # exact body of `.bayesian_shrink()` (player_ratings.R). An explainer that
  # reimplements the thing it explains will keep printing a confident,
  # plausible trace of the OLD formula the day the real one changes, and
  # nothing would fail.
  epr_recv   <- .bayesian_shrink(recv_sum,   wt_gms_recv,   loading,
                                 prior_gms[["recv"]],   EPR_PRIOR_RATE_RECV)
  epr_disp   <- .bayesian_shrink(disp_sum,   wt_gms_disp,   loading,
                                 prior_gms[["disp"]],   EPR_PRIOR_RATE_DISP)
  epr_spoil  <- .bayesian_shrink(spoil_sum,  wt_gms_spoil,  loading,
                                 prior_gms[["spoil"]],  EPR_PRIOR_RATE_SPOIL)
  epr_hitout <- .bayesian_shrink(hitout_sum, wt_gms_hitout, loading,
                                 prior_gms[["hitout"]], EPR_PRIOR_RATE_HITOUT)

  cli::cli_h2("Shrinkage Calculation (pre-centering, TOG-weighted)")

  components <- data.table::data.table(
    component = c("recv", "disp", "spoil", "hitout"),
    weighted_sum = round(c(recv_sum, disp_sum, spoil_sum, hitout_sum), 2),
    wt_mins = round(c(wt_gms_recv, wt_gms_disp, wt_gms_spoil, wt_gms_hitout), 2),
    decay_days = c(EPR_DECAY_RECV, EPR_DECAY_DISP, EPR_DECAY_SPOIL, EPR_DECAY_HITOUT),
    prior_games = unname(prior_gms),
    prior_rate = c(EPR_PRIOR_RATE_RECV, EPR_PRIOR_RATE_DISP, EPR_PRIOR_RATE_SPOIL, EPR_PRIOR_RATE_HITOUT),
    epr_raw = round(c(epr_recv, epr_disp, epr_spoil, epr_hitout), 2)
  )
  print(components, row.names = FALSE)

  cat(sprintf("\n  Formula: EPR_i = (%.1f * sum_i + prior_i * rate_i) / (wt_mins_i + prior_i)\n", loading))
  cat(sprintf("  epr_recv = (%.1f * %.2f + %.1f * %.1f) / (%.2f + %.1f) = %.2f\n",
              loading, recv_sum, prior_gms["recv"], EPR_PRIOR_RATE_RECV, wt_gms_recv, prior_gms["recv"], epr_recv))
  cat(sprintf("  epr_disp = (%.1f * %.2f + %.1f * %.1f) / (%.2f + %.1f) = %.2f\n",
              loading, disp_sum, prior_gms["disp"], EPR_PRIOR_RATE_DISP, wt_gms_disp, prior_gms["disp"], epr_disp))

  # Weighted average recv rate (what epr_recv approximates before shrinkage)
  avg_recv <- if (wt_gms_recv > 0) recv_sum / wt_gms_recv else 0
  cat(sprintf("\n  Weighted avg recv rate: %.2f (shrinkage pulls toward %.1f)\n",
              avg_recv, EPR_PRIOR_RATE_RECV))
  cat(sprintf("  Shrinkage strength: %.0f%% data / %.0f%% prior (wt_mins=%.1f vs prior=%.1f)\n",
              wt_gms_recv / (wt_gms_recv + prior_gms["recv"]) * 100,
              prior_gms["recv"] / (wt_gms_recv + prior_gms["recv"]) * 100,
              wt_gms_recv, prior_gms["recv"]))

  cat(sprintf("\n  Note: sums use game totals (per80 * tog), denominator is weighted minutes\n"))
  cat(sprintf("  (wt * tog). These are epr_RAW: the PUBLISHED EPR is this, then\n"))
  cat(sprintf("  position-centred by centre_epr_by_position() -- called from\n"))
  cat(sprintf("  build_ratings_history() and run_ratings_pipeline.R, NOT from\n"))
  cat(sprintf("  calculate_epr() -- so it will not match the leaderboard exactly.\n"))

  invisible(list(
    game_trace = dt,
    shrinkage = list(
      components = components,
      recv_sum = recv_sum, disp_sum = disp_sum,
      spoil_sum = spoil_sum, hitout_sum = hitout_sum,
      wt_mins = c(recv = wt_gms_recv, disp = wt_gms_disp,
                   spoil = wt_gms_spoil, hitout = wt_gms_hitout),
      epr_raw = c(recv = epr_recv, disp = epr_disp,
                   spoil = epr_spoil, hitout = epr_hitout)
    )
  ))
}


#' Explain a Player's TORP Rating Decomposition
#'
#' Diagnostic function showing why a player has a given TORP, EPR, and PSR.
#' Breaks down EPR into recv/disp/spoil/hitout components with shrinkage
#' diagnostics, and PSR into per-stat contributions. Useful for understanding
#' divergences between EPR and PSR.
#'
#' @param player Character player name (partial match OK) or player_id string.
#' @param season_val Season year. Default is current season.
#' @param round_val Round number. Default is next round.
#' @param top_n Integer. Number of top PSR contributors to show. Default 15.
#'
#' @return Invisibly returns a list with \code{epr_breakdown}, \code{psr_breakdown},
#'   \code{game_log}, and \code{shrinkage_info} components.
#'
#' @export
explain_player_rating <- function(player,
                                   season_val = get_afl_season(type = "current"),
                                   round_val = get_afl_week(type = "next"),
                                   top_n = 15) {

  # Resolve player
  if (grepl("^CD_I", player)) {
    pid <- player
    pname <- player
  } else {
    resolved <- resolve_player(player)
    pid <- resolved$player_id
    pname <- resolved$player_name
  }

  # --- 1. TORP ratings (has EPR + PSR already blended) ---
  tr <- torp_ratings(season_val, round_val)
  player_row <- tr[tr$player_id == pid, ]
  if (nrow(player_row) == 0) {
    cli::cli_abort("Player {.val {pname}} ({pid}) not found in TORP ratings for {season_val} round {round_val}")
  }
  pr <- player_row[1, ]

  cli::cli_h1("{pname} | {pr$team} | TORP Rating Breakdown")
  cli::cli_h2("Overall: TORP {pr$torp} = 0.5 * EPR ({pr$epr}) + 0.5 * PSR ({round(pr$psr, 2)})")

  # --- 2. EPR decomposition ---
  cli::cli_h2("EPR Breakdown (recv + disp + spoil + hitout)")
  epr_cols <- c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout")
  epr_vals <- vapply(epr_cols, function(col) {
    if (col %in% names(pr)) round(as.numeric(pr[[col]]), 2) else NA_real_
  }, numeric(1))
  names(epr_vals) <- gsub("_epr$", "", epr_cols)

  epr_dt <- data.table::data.table(
    component = names(epr_vals),
    value = epr_vals,
    pct_of_epr = round(epr_vals / sum(epr_vals, na.rm = TRUE) * 100, 1)
  )
  print(epr_dt, row.names = FALSE)

  # Shrinkage diagnostics
  shrink_cols <- c("gms", "wt_gms", "wt_tog", "wt_gms_recv")
  shrink_vals <- list()
  for (col in shrink_cols) {
    if (col %in% names(pr)) shrink_vals[[col]] <- round(as.numeric(pr[[col]]), 2)
  }

  cli::cli_h3("Shrinkage context")
  cat(sprintf("  Games: %s | Weighted games: %s | Weighted TOG: %s%%\n",
              shrink_vals$gms %||% "?",
              shrink_vals$wt_gms %||% shrink_vals$wt_gms_recv %||% "?",
              shrink_vals$wt_tog %||% "?"))
  cat(sprintf("  Prior games (shrinkage strength): 3.0 per component\n"))
  cat(sprintf("  Bayesian formula: EPR_i = (loading * sum_i + 3 * prior_rate) / (wt_gms_i + 3)\n"))

  # --- 3. PSR decomposition ---
  cli::cli_h2("PSR Breakdown (stat rating contributions to predicted margin)")

  psr_path <- .find_psr_coef_path()

  psr_breakdown <- NULL
  if (nzchar(psr_path)) {
    coef_df <- utils::read.csv(psr_path)
    coef_df <- coef_df[coef_df$beta != 0, , drop = FALSE]

    # Load player's stat ratings
    skills <- tryCatch(load_player_stat_ratings(season_val), error = function(e) NULL)
    if (!is.null(skills)) {
      skills_dt <- data.table::as.data.table(skills)
      player_skills <- skills_dt[skills_dt$player_id == pid]
      if (nrow(player_skills) > 0) {
        # Take latest round
        player_skills <- player_skills[order(-round)][1]

        skill_cols <- paste0(coef_df$stat_name, "_rating")
        available <- skill_cols %in% names(player_skills)
        if (sum(available) == 0) {
          skill_cols <- paste0(coef_df$stat_name, "_skill")
          available <- skill_cols %in% names(player_skills)
        }

        coef_sub <- coef_df[available, , drop = FALSE]
        skill_cols_avail <- skill_cols[available]

        raw_ratings <- vapply(skill_cols_avail, function(col) {
          v <- as.numeric(player_skills[[col]])
          if (is.na(v)) 0 else v
        }, numeric(1))

        # Apply SD normalisation if present
        if ("sd" %in% names(coef_sub)) {
          sds <- coef_sub$sd
          sds[sds == 0 | is.na(sds)] <- 1
          normalised <- raw_ratings / sds
        } else {
          normalised <- raw_ratings
        }

        contributions <- normalised * coef_sub$beta

        psr_breakdown <- data.table::data.table(
          stat = coef_sub$stat_name,
          rating = round(raw_ratings, 3),
          beta = round(coef_sub$beta, 4),
          contribution = round(contributions, 3)
        )
        data.table::setorderv(psr_breakdown, "contribution", order = -1L)

        cat(sprintf("\n  Top %d PSR contributors (total PSR raw: %.2f):\n", top_n,
                    sum(contributions)))
        print(head(psr_breakdown, top_n), row.names = FALSE)

        if (nrow(psr_breakdown) > top_n) {
          remaining <- sum(psr_breakdown$contribution[(top_n + 1):nrow(psr_breakdown)])
          cat(sprintf("  ... plus %d more stats contributing %.2f\n",
                      nrow(psr_breakdown) - top_n, remaining))
        }
      } else {
        cli::cli_warn("No stat ratings found for this player")
      }
    }
  } else {
    cli::cli_warn("PSR coefficient file not found")
  }

  # --- 4. Recent game log ---
  cli::cli_h2("Recent Game Log (EPV + PSV per game)")
  pgr <- tryCatch(
    load_player_game_ratings(season_val),
    error = function(e) NULL
  )
  game_log <- NULL
  if (!is.null(pgr)) {
    pgr_dt <- data.table::as.data.table(pgr)
    game_log <- pgr_dt[pgr_dt$player_id == pid]
    if (nrow(game_log) > 0) {
      show_cols <- intersect(
        c("season", "round", "team", "opp", "tog", "epv", "epv_recv",
          "epv_disp", "epv_spoil", "epv_hitout", "psv", "osv", "dsv",
          "torp_value"),
        names(game_log)
      )
      game_log <- game_log[order(-season, -round)]
      print(head(game_log[, show_cols, with = FALSE], 10), row.names = FALSE)
    }
  }

  # --- 5. Visual: percentile among all players ---
  cli::cli_h2("Percentile Ranks (vs all players)")
  pctile_metrics <- c("torp", "epr", "psr", "osr", "dsr",
                       "epr_recv", "epr_disp", "epr_spoil", "epr_hitout")
  pctile_metrics <- intersect(pctile_metrics, names(tr))
  pctiles <- vapply(pctile_metrics, function(col) {
    round(mean(tr[[col]] <= as.numeric(pr[[col]]), na.rm = TRUE) * 100)
  }, numeric(1))

  pctile_dt <- data.table::data.table(
    metric = names(pctiles),
    value = vapply(pctile_metrics, function(col) round(as.numeric(pr[[col]]), 2), numeric(1)),
    percentile = pctiles
  )
  print(pctile_dt, row.names = FALSE)

  # Plot if interactive
  if (interactive() && requireNamespace("graphics", quietly = TRUE)) {
    cols <- ifelse(pctile_dt$percentile >= 90, "#2166ac",
              ifelse(pctile_dt$percentile >= 70, "#67a9cf",
                ifelse(pctile_dt$percentile >= 30, "#d1e5f0",
                  ifelse(pctile_dt$percentile >= 10, "#fddbc7", "#b2182b"))))
    op <- graphics::par(mar = c(4, 8, 3, 1))
    on.exit(graphics::par(op), add = TRUE)
    bp <- graphics::barplot(
      pctile_dt$percentile,
      names.arg = pctile_dt$metric,
      horiz = TRUE, las = 1,
      col = cols, border = NA,
      xlim = c(0, 100),
      main = paste0(pname, " - Percentile Ranks"),
      xlab = "Percentile"
    )
    graphics::abline(v = 50, lty = 2, col = "grey40")
    graphics::text(pctile_dt$percentile + 3, bp, labels = paste0(pctile_dt$percentile, "%"),
                   cex = 0.8, adj = 0)
  }

  invisible(list(
    player_row = pr,
    epr_breakdown = epr_dt,
    psr_breakdown = psr_breakdown,
    game_log = game_log,
    percentiles = pctile_dt,
    shrinkage_info = shrink_vals
  ))
}


#' Load PSR Coefficient Files and Compute Components
#'
#' Convenience wrapper that loads the margin, offensive, and defensive
#' coefficient CSVs from \code{inst/extdata} and calls
#' \code{\link{calculate_psr_components}}.
#'
#' @inheritParams calculate_psr
#' @param psr_coef_path Path to the margin PSR coefficient CSV. If NULL,
#'   searches \code{inst/extdata/psr_coefficients.csv}.
#'
#' @param centre_by_round Logical. Group the position centring by
#'   \code{(season, round)} as well as position. Defaults to
#'   \code{PSR_CENTRE_BY_ROUND}. FALSE reproduces the pre-2026-07-29
#'   pooled-over-all-history behaviour, and exists so that arm can be
#'   scored on the match harness from production code rather than a replica.
#' @param centre_on_listed Logical. Centre on the LISTED position taxonomy
#'   rather than the stat-ratings frame's own pos_group. Defaults to
#'   PSR_CENTRE_ON_LISTED (FALSE -- the listed arm was measured and
#'   regressed, dMAE +0.382).
#' @param listed_pos Optional data frame of player_id plus a listed position
#'   column. Passed through to calculate_psr(); loaded from
#'   load_player_details() when NULL and centring on listed is requested.
#' @param comp Competition: "AFLM" (default) or "AFLW". Only used to resolve
#'   \code{psr_coef_path} when it is NULL; ignored if an explicit path is
#'   given.
#' @return A data.table with \code{psr}, \code{osr}, \code{dsr} columns.
#' @keywords internal
.compute_psr_from_stat_ratings <- function(skills, psr_coef_path = NULL, center = TRUE,
                                          centre_by_round = PSR_CENTRE_BY_ROUND,
                                          listed_pos = NULL,
                                          centre_on_listed = PSR_CENTRE_ON_LISTED,
                                          comp = "AFLM") {
  # This is already the I/O boundary for the PSR path (it reads the coefficient
  # CSVs), so it is also the right place to source listed positions when the
  # caller has not. Doing it here rather than in calculate_psr() keeps that
  # function pure and testable, while making it hard for a caller to
  # accidentally centre on the wrong taxonomy by forgetting an argument.
  if (is.null(listed_pos) && isTRUE(center) && isTRUE(centre_on_listed)) {
    # comp MUST be forwarded: .load_listed_positions() calls
    # load_player_details(), which defaults to the men's competition. Without
    # this an AFLW scoring run would centre on men's listed positions --
    # silently, since the join simply wouldn't match. Latent rather than live
    # (PSR_CENTRE_ON_LISTED defaults FALSE, so this branch is dead today), but
    # it fires the moment that flag is flipped for AFLW.
    listed_pos <- .load_listed_positions(
      unique(data.table::as.data.table(skills)$season), comp = comp)
  }

  # Resolve margin coefficient path

  if (is.null(psr_coef_path)) {
    psr_coef_path <- .find_psr_coef_path(comp = comp)
  }

  if (!nzchar(psr_coef_path) || !file.exists(psr_coef_path)) {
    cli::cli_warn("PSR coefficient file not found: {psr_coef_path}")
    return(NULL)
  }

  coef_df <- utils::read.csv(psr_coef_path)

  # Try to find osr/dsr coefficient files in the same directory
  coef_dir <- dirname(psr_coef_path)
  osr_path <- file.path(coef_dir, .comp_coef_filename("osr_coefficients.csv", comp))
  dsr_path <- file.path(coef_dir, .comp_coef_filename("dsr_coefficients.csv", comp))

  if (file.exists(osr_path) && file.exists(dsr_path)) {
    osr_coef_df <- utils::read.csv(osr_path)
    dsr_coef_df <- utils::read.csv(dsr_path)
    calculate_psr_components(skills, coef_df, osr_coef_df, dsr_coef_df, center = center,
                             centre_by_round = centre_by_round, listed_pos = listed_pos, centre_on_listed = centre_on_listed)
  } else {
    cli::cli_inform("OSR/DSR coefficient files not found -- computing PSR only (no osr/dsr decomposition)")
    calculate_psr(skills, coef_df, center = center, centre_by_round = centre_by_round,
                  listed_pos = listed_pos, centre_on_listed = centre_on_listed)
  }
}

#' Load listed positions for a set of seasons
#'
#' Sources the SAME column EPR keys on -- \code{load_player_details()} -- so PSR
#' and EPR agree by construction rather than by coincidence. Measured 2026-07-29:
#' \code{player_details} position vs \code{torp_ratings$position_group} agree
#' 100.0%.
#'
#' @param seasons Integer vector of seasons.
#' @param comp Competition: "AFLM" (default) or "AFLW". Forwarded to
#'   \code{load_player_details()} so an AFLW caller centres on AFLW listings
#'   rather than silently picking up the men's.
#' @return A data.table of \code{player_id}, \code{position}, or NULL when no
#'   season yielded rows.
#' @keywords internal
.load_listed_positions <- function(seasons, comp = "AFLM") {
  seasons <- sort(unique(seasons[!is.na(seasons)]))
  if (length(seasons) == 0) return(NULL)

  got <- lapply(seasons, function(s) {
    # Name the season AND the reason. Swallowing this silently means a single
    # season vanishing from the join shows up only as a lower match rate
    # downstream -- a symptom with no cause attached, which is not debuggable.
    d <- tryCatch(data.table::as.data.table(load_player_details(s, comp = comp)),
                  error = function(e) {
                    cli::cli_alert_danger(
                      "Listed positions for season {s} ({comp}) failed to load: {conditionMessage(e)}")
                    NULL
                  })
    if (is.null(d) || nrow(d) == 0) return(NULL)
    pcol <- intersect(c("position_group", "position"), names(d))[1]
    if (is.na(pcol) || !"player_id" %in% names(d)) return(NULL)
    d[!is.na(get(pcol)), .(player_id, position = as.character(get(pcol)), season = s)]
  })
  got <- got[!vapply(got, is.null, logical(1))]
  if (length(got) == 0) {
    # Not a warning. The caller aborts on NULL when centring is required, and a
    # warning here would read as "handled" for a condition that is not.
    return(NULL)
  }
  out <- data.table::rbindlist(got, use.names = TRUE, fill = TRUE)
  # Latest season's listing wins for a player who changed position across years.
  data.table::setorder(out, player_id, season)
  out[, .(position = position[.N]), by = player_id]
}
