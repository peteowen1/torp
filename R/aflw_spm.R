# AFLW SPM (Statistical Plus-Minus) and RAPM shrinkage
# ========================================================
# Retest of daisychain's BPM (docs/plans/AFLW-MIGRATION-PLAN.md sections 2,
# 3a point 2, 6.4 item 5), now that RAPM has been reworked and cleared its
# gate (R/aflw_rapm.R, CV R^2 0.5147 net design / 0.368 split design).
# Original BPM scored CV R^2 -0.1219 predicting RAPM's OLD, unreliable
# whole-match-row target -- that was inherited noise, not necessarily a BPM
# defect. This file re-tests the same idea (box-score -> predicted RAPM)
# against the reworked, trustworthy `build_aflw_rapm_split()` target, and
# adopts panna's full SPM architecture, not a bare rename:
#
#   raw RAPM (build_aflw_rapm_split() + fit_aflw_rapm_split()
#             + extract_aflw_rapm_ratings())
#     -> SPM predicts it from box-score stats (this file)
#     -> RAPM shrunk toward SPM as a Bayesian prior (shrink_aflw_rapm())
#     -> the shrunk result is the closest thing to a "published" AFLW
#        player rating this pipeline produces
#
# See pannaverse/docs/explainers/spm.md and rapm.md for the reference
# architecture. Deliberately simplified relative to panna's own SPM
# (elastic net ONLY, no XGBoost blend) -- panna's n is tens of thousands of
# players; AFLW's is ~470 rated players, where a second nonlinear model
# blended 50/50 adds variance without the data to support it. Documented
# here rather than silently doing less than the reference implementation.
#
# Do not brand this "TORP for AFLW" -- see R/aflw_rapm.R's header for the
# same note; it applies here too.

# .aflw_spm_position_dummies ----

#' Position-bucket dummy columns for the SPM feature set (panna includes
#' position dummies in its own SPM inputs -- see spm.md "Inputs").
#' @param position_bucket Character vector: "DEF"/"MID"/"FWD"/"INT" (from
#'   \code{.aflw_rapm_position_bucket()} via \code{.aflw_rapm_prune_columns()}).
#' @return data.table(is_def, is_mid, is_fwd) -- INT is the implicit
#'   reference level (all three dummies 0), same convention as any
#'   one-hot-minus-one-level encoding.
#' @keywords internal
.aflw_spm_position_dummies <- function(position_bucket) {
  data.table::data.table(
    is_def = as.integer(position_bucket == "DEF"),
    is_mid = as.integer(position_bucket == "MID"),
    is_fwd = as.integer(position_bucket == "FWD")
  )
}

# build_aflw_spm_features ----

#' Build the player-level box-score feature table SPM is trained/predicted
#' from -- one row per player who has ever appeared in AFLW `player_stats`,
#' NOT limited to the ~470 players RAPM could rate individually. This is
#' deliberate: SPM's whole value is that it can rate players RAPM pooled
#' into a replacement-level column for lack of minutes (see R/aflw_rapm.R
#' `.aflw_rapm_prune_columns()`) -- a career-rate box-score model doesn't
#' need the same n>p row-budget RAPM's design matrix does.
#'
#' @param seasons Numeric vector of seasons, or \code{TRUE} for all
#'   available AFLW seasons. Passed to \code{load_player_stats()}.
#' @return list(model_df (data.table: player_id, total_tog_minutes, n_games,
#'   position_bucket, is_def/is_mid/is_fwd, and one \verb{<stat>_p80} rate
#'   column per usable box-score stat), feature_cols (character vector of
#'   the \verb{<stat>_p80} + position-dummy columns actually usable),
#'   degenerate_cols (stat rate columns dropped for near-zero variance --
#'   reported, not hidden, matching the PSR/PSV build's own convention)).
#' @keywords internal
build_aflw_spm_features <- function(seasons = TRUE) {
  ps <- data.table::as.data.table(load_player_stats(seasons, comp = "AFLW"))
  if (nrow(ps) == 0) {
    cli::cli_abort("No AFLW player_stats returned for seasons {paste(seasons, collapse = ', ')}")
  }
  ps <- ps[!is.na(time_on_ground_percentage) & !is.na(player_id)]
  ps[, tog_minutes := pmin(pmax(time_on_ground_percentage / 100, 0), 1) * 80]

  exclude_cols <- c("player_id", "player_name", "team_id", "match_id", "round_number",
                     "season", "jumper_number", "time_on_ground_percentage", "tog_minutes",
                     "team_status", "position", "venue_name", "home_team_name",
                     "away_team_name", "utc_start_time")
  stat_cols <- setdiff(names(ps)[vapply(ps, is.numeric, logical(1))], exclude_cols)

  agg <- ps[, c(list(total_tog_minutes = sum(tog_minutes), n_games = .N),
                lapply(.SD, sum, na.rm = TRUE)),
            by = player_id, .SDcols = stat_cols]

  rate_cols <- paste0(stat_cols, "_p80")
  agg[, (rate_cols) := lapply(.SD, function(x) x / pmax(total_tog_minutes, 1) * 80),
      .SDcols = stat_cols]

  # position bucket: reuse RAPM's own bucketing so DEF/MID/FWD mean exactly
  # the same thing across both ratings, not two slightly different taxonomies
  player_rows <- .prepare_aflw_rapm_player_rows(seasons)
  col_map <- .aflw_rapm_prune_columns(player_rows)
  agg <- merge(agg, col_map[, .(player_id, position_bucket)], by = "player_id", all.x = TRUE)
  agg[is.na(position_bucket), position_bucket := "INT"]
  agg <- cbind(agg, .aflw_spm_position_dummies(agg$position_bucket))

  sds <- vapply(rate_cols, function(cn) stats::sd(agg[[cn]], na.rm = TRUE), numeric(1))
  degenerate_cols <- names(sds)[is.na(sds) | sds < 1e-8]
  usable_rate_cols <- setdiff(rate_cols, degenerate_cols)

  list(
    model_df = agg,
    feature_cols = c(usable_rate_cols, "is_def", "is_mid", "is_fwd"),
    degenerate_cols = degenerate_cols
  )
}

# .aflw_spm_defense_sign_constraints ----

#' Sign-constraint bounds for the defense SPM fit only (panna's convention --
#' offense carries no equivalent constraint, see spm.md "How it's built"
#' point 6 and "Key decisions").
#'
#' \code{rapm_defense} uses the NEGATIVE-is-good convention (see
#' \code{extract_aflw_rapm_ratings()}): a stat that mechanically indicates
#' GOOD defense should predict a MORE NEGATIVE \code{rapm_defense}, i.e. a
#' non-positive coefficient; a stat indicating BAD defense (errors,
#' conceded frees) should predict a MORE POSITIVE \code{rapm_defense}, i.e.
#' non-negative. Deliberately conservative: only stats with an unambiguous
#' defensive-quality reading, none of which are in
#' \code{build_aflw_spm_features()}'s own degenerate-column list.
#'
#' @param feature_cols Character vector, the full feature column set (from
#'   \code{build_aflw_spm_features()}).
#' @return list(lower, upper) -- named numeric vectors, one entry per
#'   \code{feature_cols}, for \code{glmnet::cv.glmnet(lower.limits=,
#'   upper.limits=)}.
#' @keywords internal
.aflw_spm_defense_sign_constraints <- function(feature_cols) {
  good_defense_stats <- c("tackles_p80", "intercepts_p80", "one_percenters_p80",
                           "tackles_inside50_p80", "rebound50s_p80")
  bad_defense_stats  <- c("clangers_p80", "frees_against_p80")

  # intersect() silently drops any of the above not present in feature_cols
  # (schema drift, or the near-zero-variance exclusion in
  # build_aflw_spm_features() catching one of them) -- when that happens the
  # corresponding coefficient fits completely UNCONSTRAINED with nothing in
  # the fit's own output distinguishing it from a deliberately-unconstrained
  # feature. Same failure shape as the append-vs-noop bug already fixed once
  # in aflw_psr.R (docs/plans/AFLW-MIGRATION-PLAN.md) -- surface it instead
  # of letting it degrade silently a second time.
  missing <- setdiff(c(good_defense_stats, bad_defense_stats), feature_cols)
  if (length(missing) > 0) {
    cli::cli_warn("Defense sign constraint stat{?s} not present in feature_cols (fit unconstrained): {.val {missing}}")
  }

  lower <- stats::setNames(rep(-Inf, length(feature_cols)), feature_cols)
  upper <- stats::setNames(rep(Inf, length(feature_cols)), feature_cols)
  upper[intersect(good_defense_stats, feature_cols)] <- 0
  lower[intersect(bad_defense_stats, feature_cols)] <- 0
  list(lower = lower, upper = upper)
}

# fit_aflw_spm ----

#' Fit AFLW SPM: two elastic-net models (offense, defense) predicting the
#' reworked RAPM split ratings from box-score rate features, sqrt(minutes)
#' weighted, defense-only sign constraints.
#'
#' @param spm_features Output of \code{build_aflw_spm_features()}.
#' @param rapm_ratings Output of \code{extract_aflw_rapm_ratings()}.
#'   \code{replacement_*} pooling rows (\code{rating_type == "replacement"})
#'   are filtered out here explicitly -- they have no box-score feature row
#'   to train against, and relying on the merge to silently drop them (their
#'   player_id never matches a real player) would be the same
#'   report-nothing failure this function's other merge check exists to
#'   avoid.
#' @param alpha Elastic-net mixing parameter. Default 0.5, matching panna's
#'   own glmnet SPM leg.
#' @param nfolds CV folds. Default 10.
#' @param seed RNG seed for fold assignment.
#' @return list(model_offense, model_defense (cv.glmnet objects),
#'   feature_cols, cv_r2_offense, cv_r2_defense, cv_r2_net (out-of-fold,
#'   offense-minus-defense), in_sample_r2_offense, in_sample_r2_defense,
#'   n, p). The CV numbers are what get compared against the pre-rework
#'   BPM baseline (-0.1219) per plan section 6.3.
#' @keywords internal
fit_aflw_spm <- function(spm_features, rapm_ratings, alpha = 0.5, nfolds = 10,
                         seed = 20260824) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    cli::cli_abort("glmnet package required for AFLW SPM.")
  }
  if ("rating_type" %in% names(rapm_ratings)) {
    rapm_ratings <- rapm_ratings[rapm_ratings$rating_type == "individual", ]
  }
  n_rapm <- nrow(rapm_ratings)
  model_df <- merge(rapm_ratings, spm_features$model_df, by = "player_id")
  if (nrow(model_df) == 0) {
    cli::cli_abort("No overlap between rapm_ratings and spm_features player_ids.")
  }
  if (nrow(model_df) < n_rapm) {
    cli::cli_warn("{n_rapm - nrow(model_df)} of {n_rapm} rapm_ratings player{?s} had no matching spm_features row and {?was/were} dropped from SPM training.")
  }
  feature_cols <- spm_features$feature_cols

  X <- as.matrix(model_df[, feature_cols, with = FALSE])
  n_na_x <- sum(is.na(X))
  if (n_na_x > 0) {
    cli::cli_warn("Replacing {n_na_x} NA feature value{?s} with 0 in SPM training data.")
  }
  X[is.na(X)] <- 0
  w <- sqrt(model_df$total_tog_minutes)
  y_off <- model_df$rapm_offense
  y_def <- model_df$rapm_defense

  constraints <- .aflw_spm_defense_sign_constraints(feature_cols)

  set.seed(seed)
  foldid <- sample(rep_len(seq_len(min(nfolds, nrow(X))), nrow(X)))

  r2_fn <- function(a, p) 1 - sum((a - p)^2) / sum((a - mean(a))^2)

  cv_off <- glmnet::cv.glmnet(X, y_off, weights = w, alpha = alpha, foldid = foldid,
                              standardize = TRUE, keep = TRUE)
  idx_off <- cv_off$index["min", "Lambda"]
  stopifnot(length(idx_off) == 1L)
  oof_off <- cv_off$fit.preval[, idx_off]
  cv_r2_off <- r2_fn(y_off, oof_off)
  in_r2_off <- r2_fn(y_off, as.numeric(stats::predict(cv_off, newx = X, s = "lambda.min")))

  cv_def <- glmnet::cv.glmnet(X, y_def, weights = w, alpha = alpha, foldid = foldid,
                              standardize = TRUE, keep = TRUE,
                              lower.limits = constraints$lower, upper.limits = constraints$upper)
  idx_def <- cv_def$index["min", "Lambda"]
  stopifnot(length(idx_def) == 1L)
  oof_def <- cv_def$fit.preval[, idx_def]
  cv_r2_def <- r2_fn(y_def, oof_def)
  in_r2_def <- r2_fn(y_def, as.numeric(stats::predict(cv_def, newx = X, s = "lambda.min")))

  cv_r2_net <- r2_fn(y_off - y_def, oof_off - oof_def)

  list(
    model_offense = cv_off, model_defense = cv_def, feature_cols = feature_cols,
    cv_r2_offense = cv_r2_off, cv_r2_defense = cv_r2_def, cv_r2_net = cv_r2_net,
    in_sample_r2_offense = in_r2_off, in_sample_r2_defense = in_r2_def,
    n = nrow(X), p = length(feature_cols)
  )
}

# predict_aflw_spm ----

#' Apply a fitted AFLW SPM to ANY player with box-score data -- including
#' the ~374 players RAPM pooled into a replacement-level column for lack of
#' minutes. This is SPM's actual value-add over RAPM alone: a career-rate
#' box-score model doesn't need RAPM's row-budget, so it can rate players
#' RAPM structurally cannot.
#'
#' @param spm_fit Output of \code{fit_aflw_spm()}.
#' @param spm_features Output of \code{build_aflw_spm_features()} (typically
#'   the SAME object passed to \code{fit_aflw_spm()}, so every player in
#'   \code{model_df} gets a prediction, not just the ones RAPM could rate).
#' @return data.table(player_id, n_games, total_tog_minutes, spm_offense,
#'   spm_defense, spm_net = spm_offense - spm_defense).
#' @keywords internal
predict_aflw_spm <- function(spm_fit, spm_features) {
  model_df <- spm_features$model_df
  X <- as.matrix(model_df[, spm_fit$feature_cols, with = FALSE])
  n_na_x <- sum(is.na(X))
  if (n_na_x > 0) {
    cli::cli_warn("Replacing {n_na_x} NA feature value{?s} with 0 in SPM prediction data.")
  }
  X[is.na(X)] <- 0

  spm_offense <- as.numeric(stats::predict(spm_fit$model_offense, newx = X, s = "lambda.min"))
  spm_defense <- as.numeric(stats::predict(spm_fit$model_defense, newx = X, s = "lambda.min"))

  data.table::data.table(
    player_id = model_df$player_id,
    n_games = model_df$n_games,
    total_tog_minutes = model_df$total_tog_minutes,
    spm_offense = spm_offense,
    spm_defense = spm_defense,
    spm_net = spm_offense - spm_defense
  )
}

# shrink_aflw_rapm ----

#' Shrink raw RAPM toward its SPM prediction, Bayesian-prior style -- the
#' step that makes this "SPM" and not a bare rename of BPM (see this file's
#' header, and plan section 3a point 2).
#'
#' Mechanism: an empirical-Bayes-style blend weighted by games played,
#' \code{weight = n_games / (n_games + prior_games)}. A player with few
#' games leans heavily on the SPM prior (their raw RAPM is unreliable, per
#' R/aflw_rapm.R's own "noisy at low sample sizes" limitation, matching
#' panna's rapm.md); a player with many games keeps mostly their own raw
#' RAPM. \code{prior_games} is a placeholder starting value, NOT swept /
#' cross-validated the way \code{AFLW_RAPM_MIN_TOG_MINUTES} was -- that
#' tuning pass is future work, flagged here rather than silently assumed
#' correct.
#'
#' @param rapm_ratings Output of \code{extract_aflw_rapm_ratings()}.
#'   \code{replacement_*} pooling rows (\code{rating_type == "replacement"})
#'   are filtered out here explicitly, same reasoning as \code{fit_aflw_spm()}.
#' @param spm_predictions Output of \code{predict_aflw_spm()}.
#' @param prior_games Numeric, the shrinkage strength in games-equivalent
#'   units. Must be positive. Default 10 (placeholder -- see above).
#' @return data.table(player_id, n_games, rapm_offense, rapm_defense,
#'   spm_offense, spm_defense, shrinkage_weight, aflw_rapm_shrunk_offense,
#'   aflw_rapm_shrunk_defense, aflw_rapm_shrunk). Players in
#'   \code{rapm_ratings} without an SPM prediction are dropped -- shouldn't
#'   happen in practice (SPM covers every player with box-score data, a
#'   strict superset of RAPM's rated players) -- and reported with a count
#'   if it does, rather than left to the merge's silent default.
#' @keywords internal
shrink_aflw_rapm <- function(rapm_ratings, spm_predictions, prior_games = 10) {
  if (prior_games <= 0) {
    cli::cli_abort("prior_games must be positive, got {prior_games}")
  }
  if ("rating_type" %in% names(rapm_ratings)) {
    rapm_ratings <- rapm_ratings[rapm_ratings$rating_type == "individual", ]
  }
  n_rapm <- nrow(rapm_ratings)
  merged <- merge(rapm_ratings, spm_predictions, by = "player_id")
  if (nrow(merged) < n_rapm) {
    cli::cli_warn("{n_rapm - nrow(merged)} of {n_rapm} rapm_ratings player{?s} had no matching SPM prediction and {?was/were} dropped -- shouldn't happen in practice, see this function's docs.")
  }
  merged[, shrinkage_weight := n_games / (n_games + prior_games)]
  merged[, `:=`(
    aflw_rapm_shrunk_offense = shrinkage_weight * rapm_offense + (1 - shrinkage_weight) * spm_offense,
    aflw_rapm_shrunk_defense = shrinkage_weight * rapm_defense + (1 - shrinkage_weight) * spm_defense
  )]
  merged[, aflw_rapm_shrunk := aflw_rapm_shrunk_offense - aflw_rapm_shrunk_defense]
  data.table::setorder(merged, -aflw_rapm_shrunk)
  merged[, .(player_id, n_games, rapm_offense, rapm_defense, spm_offense, spm_defense,
             shrinkage_weight, aflw_rapm_shrunk_offense, aflw_rapm_shrunk_defense,
             aflw_rapm_shrunk)]
}
