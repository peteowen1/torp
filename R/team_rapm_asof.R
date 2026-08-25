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

# build_team_spm_features_asof ----

#' Point-in-time, decay-weighted SPM box-score features -- the SPM-side
#' analogue of \code{build_team_rapm_asof()}. Fixes a real inconsistency
#' found 2026-08-25 (AFL-DECAY-XRAPM-PLAN.md sec19-21): the season-block
#' \code{fit_team_spm_asof()} trained its SPM prior on
#' \code{all_seasons[all_seasons < cutoff_season]} -- every prior SEASON
#' weighted equally, no decay -- while \code{fit_team_rapm_asof()} decay-
#' weights its RAPM half. That mismatch (a recency-weighted RAPM shrunk
#' toward a flat-history SPM prior) measurably cost accuracy: raw RAPM beat
#' the shrunk blend by more than the entire halflife sweep's span. This
#' function decay-weights the box-score aggregation itself, at MATCH grain
#' (\code{load_player_stats()}'s native granularity), matching RAPM's own
#' \code{0.5^(age_days/halflife_days)} formula and its filter-first-decay-
#' second leak-safety discipline exactly.
#'
#' @param ref_date Date (or coercible). Only matches with
#'   \code{match_date <= ref_date} contribute -- LEAK SAFETY: filtered first,
#'   decayed second, identical discipline to \code{build_team_rapm_asof()}.
#' @param comp "AFLM" (default) or "AFLW".
#' @param halflife_days Decay half-life in days. Default 730, matching the
#'   shipped RAPM default (\code{HALFLIFE_DAYS} in
#'   \code{data-raw/03-ratings/build_team_rapm_asof_snapshots.R}) -- the two
#'   halves of a shrinkage blend should decay at the same rate unless there is
#'   a specific reason to diverge them, which has not been investigated.
#' @param seasons Numeric vector of seasons, or \code{TRUE} (default) -- the
#'   pool to filter from; \code{ref_date} does the actual point-in-time
#'   restriction, same convention as \code{build_team_rapm_asof()}.
#' @return Same shape as \code{build_team_spm_features()}
#'   (list(model_df, feature_cols, degenerate_cols)) -- a drop-in replacement
#'   for \code{fit_team_spm()}'s \code{spm_features} argument. \code{NULL}
#'   (with a warning) if no matches survive the point-in-time filter.
#'   \code{model_df$total_tog_minutes} and \code{model_df}'s stat sums are
#'   now DECAY-WEIGHTED ("effective" minutes/counts, not raw totals) --
#'   \code{model_df$n_games} stays a raw, unweighted count for diagnostics.
#' @keywords internal
build_team_spm_features_asof <- function(ref_date, comp = "AFLM", halflife_days = 730,
                                         seasons = TRUE) {
  .validate_afl_comp(comp)
  stopifnot(halflife_days > 0)
  ref_date <- as.Date(ref_date)
  game_minutes <- if (comp == "AFLM") 120 else 80

  ps <- data.table::as.data.table(load_player_stats(seasons, comp = comp))
  if (nrow(ps) == 0) {
    cli::cli_abort("build_team_spm_features_asof: no player_stats returned for comp {.val {comp}}.")
  }
  ps <- ps[!is.na(time_on_ground_percentage) & !is.na(player_id)]

  match_dates <- .team_rapm_match_dates(seasons, comp = comp)
  ps <- merge(ps, match_dates, by = "match_id")

  # LEAK SAFETY: filter FIRST, decay SECOND -- identical discipline to
  # build_team_rapm_asof(); see that function's header for why this order
  # is the entire leak-safety property, not a stylistic choice.
  ps <- ps[match_date <= ref_date]
  if (nrow(ps) == 0) {
    cli::cli_warn(paste0(
      "build_team_spm_features_asof: no matches on or before {as.character(ref_date)} ",
      "for comp {.val {comp}} -- returning NULL."))
    return(NULL)
  }
  age_days <- as.numeric(ref_date - ps$match_date)
  if (any(age_days < 0)) {
    cli::cli_abort("build_team_spm_features_asof: {sum(age_days < 0)} row{?s} have a match_date AFTER ref_date despite the point-in-time filter -- this is a bug, not data noise.")
  }
  ps[, decay_weight := 0.5 ^ (age_days / halflife_days)]
  ps[, tog_minutes := pmin(pmax(time_on_ground_percentage / 100, 0), 1) * game_minutes]

  exclude_cols <- c("player_id", "player_name", "team_id", "match_id", "round_number",
                     "season", "jumper_number", "time_on_ground_percentage", "tog_minutes",
                     "venue_name", "home_team_name", "away_team_name", "utc_start_time",
                     "position", "position_group", "team_status", "match_date", "decay_weight")
  numeric_cols <- names(ps)[vapply(ps, is.numeric, logical(1))]
  stat_cols <- setdiff(numeric_cols, exclude_cols)

  agg <- ps[, c(list(total_tog_minutes = sum(tog_minutes * decay_weight), n_games = .N),
                lapply(.SD, function(x) sum(x * decay_weight, na.rm = TRUE))),
            by = player_id, .SDcols = stat_cols]

  rate_cols <- paste0(stat_cols, "_prate")
  agg[, (rate_cols) := lapply(.SD, function(x) x / pmax(total_tog_minutes, 1) * game_minutes),
      .SDcols = stat_cols]

  # Position bucket: SAME point-in-time-filtered rows as everything above --
  # build_team_spm_features() restricts this to its `seasons` arg too (not a
  # global lookup), so this stays symmetric with that leak-safety discipline.
  rows <- .prepare_team_rapm_player_rows(seasons, comp = comp)
  if (nrow(rows) > 0) {
    rows <- merge(rows, match_dates, by = "match_id")
    rows_ptid <- rows[match_date <= ref_date]
    col_map <- .team_rapm_prune_columns(rows_ptid, comp = comp)
    pos_lookup <- unique(col_map[, .(player_id, position_bucket)])
    agg <- merge(agg, pos_lookup, by = "player_id", all.x = TRUE)
  } else {
    agg[, position_bucket := NA_character_]
  }
  n_no_pos <- sum(is.na(agg$position_bucket))
  if (n_no_pos > 0) {
    cli::cli_warn("build_team_spm_features_asof: {n_no_pos} player{?s} have no resolvable position_bucket -- position dummies default to all-0 for {?them/these}")
  }
  agg[, `:=`(
    is_def  = as.integer(!is.na(position_bucket) & position_bucket == "DEF"),
    is_mid  = as.integer(!is.na(position_bucket) & position_bucket == "MID"),
    is_fwd  = as.integer(!is.na(position_bucket) & position_bucket == "FWD"),
    is_ruck = as.integer(!is.na(position_bucket) & position_bucket == "RUCK")
  )]

  sds <- vapply(rate_cols, function(cn) stats::sd(agg[[cn]], na.rm = TRUE), numeric(1))
  degenerate_cols <- names(sds)[is.na(sds) | sds < 1e-8]
  usable_rate_cols <- setdiff(rate_cols, degenerate_cols)

  list(
    model_df = agg,
    feature_cols = c(usable_rate_cols, "is_def", "is_mid", "is_fwd", "is_ruck"),
    degenerate_cols = degenerate_cols
  )
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
#' are correctly filtered. **Decay-weighted as of 2026-08-25** (sec19-21) --
#' previously trained on \code{all_seasons[all_seasons < cutoff_season]} with
#' every prior season weighted equally, mismatched against RAPM's own decay
#' weighting; now uses \code{build_team_spm_features_asof()}, which filters
#' \code{match_date <= ref_date} at MATCH grain and decay-weights at the same
#' \code{halflife_days} as the RAPM half, by default. This is STILL a leak
#' fix for the same reason as before (a flat all-history SPM fit would leak
#' future box-score information through the shrinkage step even when RAPM's
#' own rows are correctly filtered) -- the change is that the point-in-time
#' filter is now applied at match grain, not season grain, matching
#' \code{fit_team_rapm_asof()}'s own precision.
#'
#' @param ref_date Date. Only matches with \code{match_date <= ref_date}
#'   contribute to the SPM fit.
#' @param rapm_asof_ratings Output of
#'   \code{extract_team_rapm_ratings()} run on this SAME \code{ref_date}'s
#'   \code{build_team_rapm_asof()}/\code{fit_team_rapm_asof()} output -- the
#'   RAPM target this SPM predicts must itself already be point-in-time, or
#'   this function's own leak-safety is moot.
#' @param comp "AFLM" (default) or "AFLW".
#' @param halflife_days Decay half-life in days for the SPM box-score
#'   aggregation. Default 730, matching \code{fit_team_rapm_asof()}'s
#'   shipped default -- the two halves of a shrinkage blend should decay at
#'   the same rate unless there's a specific, separately-investigated reason
#'   to diverge them.
#' @param seasons Passed to \code{build_team_spm_features_asof()} as the pool
#'   to filter from.
#' @inheritParams fit_team_spm
#' @param prior_games Passed to \code{shrink_team_rapm()}.
#' @return \code{shrink_team_rapm()}'s output (data.table) with \code{ref_date}
#'   attached. \code{NULL} (with a LOUD warning) if EITHER no matches survive
#'   the point-in-time filter OR the resulting training pool has too few
#'   individually-rated players for \code{fit_team_spm()}'s cross-validation
#'   (seen on AFLW's early checkpoints, where the RAPM pruning threshold pools
#'   almost everyone into replacement-level: n=2 rows is not enough for even
#'   3-fold CV). Callers must not silently substitute the all-history SPM in
#'   either case (that is exactly the leak this function exists to prevent)
#'   -- fall back only with an explicit, visible warning at the call site,
#'   matching panna's own convention (AFL-DECAY-XRAPM-PLAN.md sec1).
#' @keywords internal
fit_team_spm_asof <- function(ref_date, rapm_asof_ratings, comp = "AFLM", halflife_days = 730,
                              seasons = TRUE, alpha = 0.5, nfolds = 10, seed = 20260825,
                              prior_games = 10) {
  .validate_afl_comp(comp)
  ref_date <- as.Date(ref_date)

  spm_features <- build_team_spm_features_asof(ref_date, comp = comp,
                                                halflife_days = halflife_days, seasons = seasons)
  if (is.null(spm_features)) {
    return(NULL)
  }
  spm_fit <- tryCatch(
    fit_team_spm(spm_features, rapm_asof_ratings, alpha = alpha, nfolds = nfolds, seed = seed),
    torp_spm_too_few_rows = function(e) {
      cli::cli_warn(paste0(
        "fit_team_spm_asof: ref_date {as.character(ref_date)} for comp {.val {comp}} -- ",
        "{conditionMessage(e)} Returning NULL; callers must NOT silently fall back to an ",
        "all-history SPM (that reintroduces the exact leak this function exists to prevent)."))
      NULL
    }
  )
  if (is.null(spm_fit)) {
    return(NULL)
  }
  spm_pred <- predict_team_spm(spm_fit, spm_features)
  shrunk <- shrink_team_rapm(rapm_asof_ratings, spm_pred, prior_games = prior_games)
  shrunk[, `:=`(ref_date = ref_date, spm_halflife_days = halflife_days)]
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
