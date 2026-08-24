# Team RAPM/SPM "career xRAPM" -- decay-weighted, as-of-date engine
# =========================================================================
# Ports panna's decay-weighted career-trait mechanism (career_rapm.R,
# spm_asof.R) on top of the season-block point-in-time engine
# (team_rapm.R/team_spm.R, already merged to dev). See
# docs/plans/AFL-DECAY-XRAPM-PLAN.md for the full design and the five
# decisions this file implements without re-litigating them:
#
#   1. halflife_days: swept empirically (optimize_team_rapm_decay()), not
#      assumed to transfer from panna's 365d.
#   2. Storage: a snapshot builder script writes one parquet per comp,
#      career_panna_asof.parquet-shaped -- not implemented in this R/ file
#      (belongs in data-raw/, this file is just the fitting machinery).
#   3. Checkpoint dates: derived from the fixture calendar
#      (.team_rapm_checkpoint_dates()), never a hardcoded weekday.
#   4. Same cadence both comps -- this file is comp-parameterized like
#      team_rapm.R/team_spm.R, not two versions.
#   5. Second-order SPM leak fix: fit_team_spm_asof() refits SPM per ref_date
#      on strictly-prior seasons, mirroring panna's fit_expanding_skill_spm().
#
# LEAK-SAFETY, the single most important detail (AFL-DECAY-XRAPM-PLAN.md
# sec1): decay alone is NOT leak-safe -- a future match gets a >1 decay
# weight (up-weighted, not filtered out). build_team_rapm_asof() FILTERS
# match_date <= ref_date FIRST, decays second. Never call
# build_team_rapm_split()/build_team_rapm_net() directly with unfiltered
# rows and try to fix it after the fact with weights alone.

# .team_rapm_match_dates ----

#' One row per match_id with its calendar date -- the join key every
#' as-of/decay function in this file uses for point-in-time filtering.
#'
#' @param seasons Passed to \code{load_results()}.
#' @param comp "AFLM" (default) or "AFLW".
#' @return data.table(match_id, match_date).
#' @keywords internal
.team_rapm_match_dates <- function(seasons = TRUE, comp = "AFLM") {
  .validate_afl_comp(comp)
  res <- load_results(seasons, comp = comp)
  if (nrow(res) == 0) {
    return(data.table::data.table(match_id = character(0), match_date = as.Date(character(0))))
  }
  dt <- data.table::as.data.table(res)
  dt <- dt[!is.na(utc_start_time), .(match_id, match_date = as.Date(utc_start_time))]
  unique(dt, by = "match_id")
}

# .team_rapm_checkpoint_dates ----

#' Dynamic per-round checkpoint dates, derived from the fixture calendar --
#' AFL-DECAY-XRAPM-PLAN.md sec3/sec6 point 3, NOT a hardcoded weekday
#' ("Tuesday of the round" breaks on real, recurring exceptions: ANZAC Day,
#' irregular extended rounds, the finals-boundary NA gap in round numbering).
#'
#' For every round except a season's last home-and-away round: checkpoint =
#' next round's first match date - 1 day (always safely inside the gap, by
#' construction). For the last round of each season (no next round to look
#' up): checkpoint = this round's last match date + 2 days -- still inside
#' the measured 3-8 day gap to finals.
#'
#' @param seasons Passed to \code{load_fixtures(all=TRUE)} -- accepted for a
#'   consistent signature with this file's other functions but ignored
#'   (fixtures are always fetched with \code{all=TRUE} so the "last round of
#'   the season" and "next round" logic sees the whole season, not a
#'   caller-truncated slice).
#' @param comp "AFLM" (default) or "AFLW".
#' @return data.table(season, round_number, round_first_match, round_last_match,
#'   checkpoint_date) sorted by season, round_number. One row per
#'   (season, round_number) that has at least one dated match; finals rounds
#'   (no numeric round_number, or NA) are excluded -- they are not on the
#'   regular per-round cadence this function is for.
#' @keywords internal
.team_rapm_checkpoint_dates <- function(seasons = TRUE, comp = "AFLM") {
  .validate_afl_comp(comp)
  fx <- load_fixtures(all = TRUE, comp = comp)
  if (nrow(fx) == 0) {
    cli::cli_abort("No fixtures available to derive checkpoint dates for comp {.val {comp}}.")
  }
  dt <- data.table::as.data.table(fx)
  dt <- dt[!is.na(utc_start_time) & !is.na(round_number) & !is.na(season),
           .(match_id, season, round_number, match_date = as.Date(utc_start_time))]

  by_round <- dt[, .(round_first_match = min(match_date), round_last_match = max(match_date)),
                 by = .(season, round_number)]
  data.table::setorder(by_round, season, round_number)

  by_round[, next_round_first := data.table::shift(round_first_match, type = "lead"), by = season]
  by_round[, checkpoint_date := data.table::fifelse(
    !is.na(next_round_first),
    next_round_first - 1L,
    round_last_match + 2L
  )]
  by_round[, next_round_first := NULL]
  by_round[]
}

# build_team_rapm_asof ----

#' Point-in-time, decay-weighted RAPM design as of a reference date --
#' filters match_date <= ref_date FIRST (leak safety), then composes a
#' \code{0.5^(age_days/halflife_days)} decay weight per row, mirroring
#' panna's \code{fit_career_rapm()} but built on \code{team_rapm.R}'s
#' existing split-design machinery via its \code{player_rows} override
#' (reused, not duplicated).
#'
#' @param ref_date Date (or coercible). Only matches with
#'   \code{match_date <= ref_date} are used.
#' @param comp "AFLM" (default) or "AFLW".
#' @param halflife_days Decay half-life in days. Default 365 (panna's prior --
#'   see \code{optimize_team_rapm_decay()} to check whether AFL data prefers a
#'   different value; AFL-DECAY-XRAPM-PLAN.md sec6 point 1 is explicit this
#'   should be validated, not assumed).
#' @param seasons Numeric vector of seasons, or \code{TRUE} (default) for all
#'   available -- the full pool to filter from; \code{ref_date} does the
#'   actual point-in-time restriction.
#' @param min_train_matches Minimum matches required after the point-in-time
#'   filter. Default 50 -- below this the design is too thin to trust (return
#'   \code{NULL} with a warning rather than fit garbage).
#' @inheritParams build_team_rapm_split
#' @return A \code{build_team_rapm_split()}-shaped list plus
#'   \code{decay_weight} (aligned to \code{match_ids}), \code{ref_date},
#'   \code{halflife_days}, \code{n_train_matches}. \code{NULL} (with a
#'   warning) if fewer than \code{min_train_matches} matches survive the
#'   point-in-time filter.
#' @keywords internal
build_team_rapm_asof <- function(ref_date, comp = "AFLM", halflife_days = 365,
                                 seasons = TRUE, exposure = NULL, threshold = NULL,
                                 unit = NULL, game_minutes = NULL,
                                 min_train_matches = 50L) {
  .validate_afl_comp(comp)
  stopifnot(halflife_days > 0)
  ref_date <- as.Date(ref_date)

  match_dates <- .team_rapm_match_dates(seasons, comp = comp)
  if (nrow(match_dates) == 0) {
    cli::cli_abort("No dated matches available for comp {.val {comp}}.")
  }

  rows <- .prepare_team_rapm_player_rows(seasons, comp = comp)
  if (nrow(rows) == 0) {
    cli::cli_abort("No player rows available for comp {.val {comp}}.")
  }
  rows <- merge(rows, match_dates, by = "match_id")

  # LEAK SAFETY: filter FIRST. Decay alone would up-weight (not exclude) a
  # future match -- see file header. This line is the entire leak-safety
  # property of everything downstream in this function.
  rows_ptid <- rows[match_date <= ref_date]

  n_matches <- length(unique(rows_ptid$match_id))
  if (n_matches < min_train_matches) {
    cli::cli_warn(paste0(
      "build_team_rapm_asof: only {n_matches} match{?es} on or before {as.character(ref_date)} ",
      "for comp {.val {comp}} (need >= {min_train_matches}) -- skipping."))
    return(NULL)
  }

  rapm_data <- build_team_rapm_split(player_rows = rows_ptid, comp = comp,
                                     exposure = exposure, threshold = threshold,
                                     unit = unit, game_minutes = game_minutes)

  row_dates <- match_dates$match_date[match(rapm_data$match_ids, match_dates$match_id)]
  age_days <- as.numeric(ref_date - row_dates)
  if (any(age_days < 0)) {
    cli::cli_abort("build_team_rapm_asof: {sum(age_days < 0)} row{?s} have a match_date AFTER ref_date despite the point-in-time filter -- this is a bug, not data noise.")
  }
  decay_weight <- 0.5 ^ (age_days / halflife_days)

  c(rapm_data, list(
    decay_weight = decay_weight, ref_date = ref_date,
    halflife_days = halflife_days, n_train_matches = n_matches
  ))
}

# fit_team_rapm_asof ----

#' Ridge-fit an as-of RAPM design, weighting observations by
#' \code{rapm_data$decay_weight} -- otherwise identical to
#' \code{fit_team_rapm_split()} (whole-match CV folds, same reasoning: both
#' sides of a match share players via the opp-block columns).
#'
#' @param rapm_data Output of \code{build_team_rapm_asof()}.
#' @inheritParams fit_team_rapm_split
#' @return list(model, lambda_min, cv_r2, in_sample_r2, n, p, n_over_p).
#' @keywords internal
fit_team_rapm_asof <- function(rapm_data, nfolds = 10, seed = 20260825) {
  set.seed(seed)
  match_ids <- unique(rapm_data$match_ids)
  fold_of_match <- stats::setNames(
    sample(rep_len(seq_len(min(nfolds, length(match_ids))), length(match_ids))),
    match_ids
  )
  foldid <- unname(fold_of_match[rapm_data$match_ids])
  fit <- .team_rapm_fit(rapm_data$X, rapm_data$y, foldid = foldid, seed = seed,
                        weights = rapm_data$decay_weight)
  c(fit, list(n = rapm_data$n, p = rapm_data$p, n_over_p = rapm_data$n_over_p))
}

# fit_team_spm_asof ----

#' Second-order leak fix (AFL-DECAY-XRAPM-PLAN.md sec1/sec6 point 5): the SPM
#' shrinkage-prior itself must be point-in-time, or future box-score
#' information leaks through the shrinkage step even when RAPM's own rows
#' are correctly filtered. Mirrors panna's \code{fit_expanding_skill_spm()},
#' generalized from season-cutoff to \code{ref_date} -- trains SPM only on
#' seasons that ended strictly before whichever season \code{ref_date} falls
#' in (never the in-progress season, even matches before \code{ref_date}
#' within it -- same conservative granularity panna's own
#' \code{season_end_year < cutoff_year} filter uses; box-score features are
#' season-aggregated by \code{build_team_spm_features()}, so this is the
#' finest point-in-time grain available without changing that function).
#'
#' @param ref_date Date. Determines the training-season cutoff.
#' @param rapm_asof_ratings Output of
#'   \code{extract_team_rapm_ratings()} run on this SAME \code{ref_date}'s
#'   \code{build_team_rapm_asof()}/\code{fit_team_rapm_asof()} output -- the
#'   RAPM target this SPM predicts must itself already be point-in-time, or
#'   this function's own leak-safety is moot.
#' @param comp "AFLM" (default) or "AFLW".
#' @param seasons Passed to \code{load_results()} to enumerate all available
#'   seasons (the pool to restrict to \code{< cutoff_season}).
#' @inheritParams fit_team_spm
#' @param prior_games Passed to \code{shrink_team_rapm()}.
#' @return \code{shrink_team_rapm()}'s output (data.table) with \code{season}
#'   set to the training cutoff. \code{NULL} (with a LOUD warning) if
#'   \code{ref_date} falls in or before the earliest available season --
#'   there is no strictly-prior season to train SPM on. Callers must not
#'   silently substitute the all-history SPM in this case (that is exactly
#'   the leak this function exists to prevent) -- fall back only with an
#'   explicit, visible warning at the call site, matching panna's own
#'   convention (AFL-DECAY-XRAPM-PLAN.md sec1).
#' @keywords internal
fit_team_spm_asof <- function(ref_date, rapm_asof_ratings, comp = "AFLM", seasons = TRUE,
                              alpha = 0.5, nfolds = 10, seed = 20260825, prior_games = 10) {
  .validate_afl_comp(comp)
  ref_date <- as.Date(ref_date)

  match_dates <- .team_rapm_match_dates(seasons, comp = comp)
  res <- load_results(seasons, comp = comp)
  res_dt <- data.table::as.data.table(res)
  res_dt <- merge(res_dt[, .(match_id, season)], match_dates, by = "match_id")

  prior_matches <- res_dt[match_date <= ref_date]
  if (nrow(prior_matches) == 0) {
    cli::cli_warn(paste0(
      "fit_team_spm_asof: NO matches on or before {as.character(ref_date)} for comp {.val {comp}} -- ",
      "cannot determine a cutoff season. Returning NULL; callers must NOT silently fall back to an ",
      "all-history SPM (that reintroduces the exact leak this function exists to prevent)."))
    return(NULL)
  }
  cutoff_season <- max(prior_matches$season)
  all_seasons <- sort(unique(res_dt$season))
  train_seasons <- all_seasons[all_seasons < cutoff_season]

  if (length(train_seasons) == 0) {
    cli::cli_warn(paste0(
      "fit_team_spm_asof: ref_date {as.character(ref_date)} falls in the EARLIEST available season ",
      "({cutoff_season}) for comp {.val {comp}} -- no strictly-prior season exists to train SPM on. ",
      "Returning NULL; callers must NOT silently fall back to an all-history SPM."))
    return(NULL)
  }

  spm_features <- build_team_spm_features(train_seasons, comp = comp)
  spm_fit <- fit_team_spm(spm_features, rapm_asof_ratings, alpha = alpha, nfolds = nfolds, seed = seed)
  spm_pred <- predict_team_spm(spm_fit, spm_features)
  shrunk <- shrink_team_rapm(rapm_asof_ratings, spm_pred, prior_games = prior_games)
  shrunk[, `:=`(ref_date = ref_date, spm_train_seasons_max = cutoff_season - 1L)]
  shrunk[]
}

# optimize_team_rapm_decay ----

#' Tune the RAPM decay half-life on held-out match prediction -- temporal
#' holdout, mirroring panna's \code{optimize_panna_decay()}
#' (AFL-DECAY-XRAPM-PLAN.md sec6 point 1: don't assume panna's 365d
#' transfers, sweep it). Builds ONE shared point-in-time design (all matches
#' strictly before \code{ref_date}) and row-subsets by date for train/holdout,
#' so every half-life in the grid is compared on an identical column set.
#'
#' @param comp "AFLM" (default) or "AFLW".
#' @param seasons Passed through to the row-fetching helpers.
#' @param halflife_grid Half-lives (days) to evaluate.
#' @param ref_date "Today" of the test (Date). Default = latest available
#'   match date for \code{comp}.
#' @param holdout_days Width of the holdout window (days back from
#'   \code{ref_date}).
#' @inheritParams build_team_rapm_split
#' @param nfolds,seed Passed to the per-halflife fit.
#' @return list(results (data.table halflife_days/holdout_mse, sorted),
#'   best_halflife, ref_date, train_end, n_train, n_holdout).
#' @keywords internal
optimize_team_rapm_decay <- function(comp = "AFLM", seasons = TRUE,
                                     halflife_grid = c(180, 365, 545, 730, 1095),
                                     ref_date = NULL, holdout_days = 150L,
                                     exposure = NULL, threshold = NULL, unit = NULL,
                                     game_minutes = NULL, nfolds = 10, seed = 20260825) {
  .validate_afl_comp(comp)
  match_dates <- .team_rapm_match_dates(seasons, comp = comp)
  if (nrow(match_dates) == 0) {
    cli::cli_abort("No dated matches available for comp {.val {comp}}.")
  }
  if (is.null(ref_date)) ref_date <- max(match_dates$match_date)
  ref_date <- as.Date(ref_date)
  train_end <- ref_date - as.integer(holdout_days)

  rows <- .prepare_team_rapm_player_rows(seasons, comp = comp)
  rows <- merge(rows, match_dates, by = "match_id")
  rows_pool <- rows[match_date < ref_date]
  if (nrow(rows_pool) == 0) {
    cli::cli_abort("optimize_team_rapm_decay: no rows before ref_date {as.character(ref_date)} for comp {.val {comp}}.")
  }

  full_design <- build_team_rapm_split(player_rows = rows_pool, comp = comp,
                                       exposure = exposure, threshold = threshold,
                                       unit = unit, game_minutes = game_minutes)
  row_dates <- match_dates$match_date[match(full_design$match_ids, match_dates$match_id)]

  train_idx <- which(row_dates < train_end)
  hold_idx  <- which(row_dates >= train_end & row_dates < ref_date)
  cli::cli_inform(paste0(
    "optimize_team_rapm_decay: comp {.val {comp}} | ref {as.character(ref_date)} | train_end ",
    "{as.character(train_end)} | train rows {length(train_idx)} | holdout rows {length(hold_idx)}"))
  if (length(hold_idx) < 20L) {
    cli::cli_warn("optimize_team_rapm_decay: holdout has only {length(hold_idx)} rows -- half-life choice may be unreliable.")
  }
  if (length(train_idx) == 0) {
    cli::cli_abort("optimize_team_rapm_decay: no training rows before train_end {as.character(train_end)}.")
  }

  age_train <- as.numeric(train_end - row_dates[train_idx])
  match_ids_train <- full_design$match_ids[train_idx]
  Xh <- full_design$X[hold_idx, , drop = FALSE]
  yh <- full_design$y[hold_idx]

  set.seed(seed)
  um <- unique(match_ids_train)
  fold_of_match <- stats::setNames(
    sample(rep_len(seq_len(min(nfolds, length(um))), length(um))), um
  )
  foldid_train <- unname(fold_of_match[match_ids_train])

  res <- data.table::rbindlist(lapply(halflife_grid, function(hl) {
    w <- 0.5 ^ (age_train / hl)
    fit <- .team_rapm_fit(full_design$X[train_idx, , drop = FALSE], full_design$y[train_idx],
                          foldid = foldid_train, seed = seed, weights = w)
    yhat <- as.vector(stats::predict(fit$model, newx = Xh, s = "lambda.min"))
    mse <- mean((yh - yhat)^2)
    cli::cli_alert_success("optimize_team_rapm_decay: comp {.val {comp}} halflife {hl}d -> holdout MSE {round(mse, 4)}")
    data.table::data.table(halflife_days = hl, holdout_mse = mse)
  }))

  list(results = res[order(holdout_mse)],
       best_halflife = res$halflife_days[which.min(res$holdout_mse)],
       ref_date = ref_date, train_end = train_end,
       n_train = length(train_idx), n_holdout = length(hold_idx))
}
