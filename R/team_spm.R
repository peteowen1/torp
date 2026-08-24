# Team SPM (Statistical Plus-Minus) and RAPM shrinkage -- shared AFLM/AFLW engine
# ===================================================================================
# Companion to team_rapm.R -- see that file's header for why this is one
# comp-parameterized engine, not two copies. Architecture (panna's
# RAPM+SPM pattern, pannaverse/docs/explainers/spm.md / rapm.md):
#
#   raw RAPM (team_rapm.R: build_team_rapm_split() + fit_team_rapm_split()
#             + extract_team_rapm_ratings())
#     -> SPM predicts it from box-score stats (this file)
#     -> RAPM shrunk toward SPM as a Bayesian prior (shrink_team_rapm())
#     -> the shrunk result is what a match-model candidate feature is built
#        from (docs/plans/AFLW-MIGRATION-PLAN.md / AFLM-RAPM-SPM-PLAN.md)
#
# COLUMN NAMES ARE SHARED, NOT COMP-SPECIFIC: both comps' rate columns use
# the same "<stat>_prate" suffix (a per-nominal-full-game rate), not AFLW's
# old "_p80"/AFLM's old "_p100" -- the underlying box-score stat names are
# already mostly identical between comps (verified: AFLW's playerStats/match
# endpoint uses the same field names as men's, just a narrower subset -- 59
# vs 84 columns). Unifying the suffix means .team_spm_defense_sign_constraints()
# needs no comp branching at all -- the existing intersect()-based "only
# constrain what's present" logic already handles AFLW's narrower column set.
# The per-game normalisation BASE still differs by comp internally (AFLW: 80
# nominal minutes; AFLM: 120) -- that's a real, already-validated numeric
# choice per comp, not something to silently unify along with the name.
#
# Point-in-time: SPM has the IDENTICAL leak risk RAPM does (see team_rapm.R's
# header) -- if SPM is fit once on ALL of RAPM's history and then used to
# rate an earlier season's players, SPM itself leaks future information
# forward. build_team_spm_expanding() (below) refits SPM per season-cutoff,
# predicting only the point-in-time RAPM ratings available as of that
# cutoff. shrink_team_rapm() needs NO separate point-in-time treatment --
# it's a per-row formula, safe by construction once its two inputs (RAPM,
# SPM) are already point-in-time.
#
# Do not brand this "TORP for AFLW"/"TORP for AFLM" -- see team_rapm.R.

# build_team_spm_features ----

#' Build the player-level box-score feature table SPM is trained/predicted
#' from -- one row per player who has ever appeared in \code{player_stats},
#' NOT limited to the players RAPM could rate individually (a career-rate
#' box-score model doesn't need RAPM's n>p row-budget -- this is SPM's actual
#' value-add over RAPM alone).
#'
#' @param seasons Numeric vector of seasons, or \code{TRUE} for all available
#'   seasons.
#' @param comp "AFLM" (default) or "AFLW".
#' @return list(model_df (data.table: player_id, total_tog_minutes, n_games,
#'   position_bucket, is_def/is_mid/is_fwd/is_ruck -- \code{is_ruck} is
#'   always 0 for AFLW, which has no RUCK bucket -- and one
#'   \verb{<stat>_prate} rate column per usable box-score stat),
#'   feature_cols, degenerate_cols (near-zero-variance stat columns dropped,
#'   reported not hidden)).
#' @keywords internal
build_team_spm_features <- function(seasons = TRUE, comp = "AFLM") {
  .validate_afl_comp(comp)
  game_minutes <- if (comp == "AFLM") 120 else 80

  ps <- if (comp == "AFLM") {
    data.table::as.data.table(load_player_stats(seasons))
  } else {
    data.table::as.data.table(load_player_stats(seasons, comp = "AFLW"))
  }
  if (nrow(ps) == 0) {
    cli::cli_abort("No player_stats returned for seasons {paste(seasons, collapse = ', ')}, comp {.val {comp}}")
  }
  ps <- ps[!is.na(time_on_ground_percentage) & !is.na(player_id)]
  ps[, tog_minutes := pmin(pmax(time_on_ground_percentage / 100, 0), 1) * game_minutes]

  exclude_cols <- c("player_id", "player_name", "team_id", "match_id", "round_number",
                     "season", "jumper_number", "time_on_ground_percentage", "tog_minutes",
                     "venue_name", "home_team_name", "away_team_name", "utc_start_time",
                     "position", "position_group", "team_status")
  numeric_cols <- names(ps)[vapply(ps, is.numeric, logical(1))]
  stat_cols <- setdiff(numeric_cols, exclude_cols)

  agg <- ps[, c(list(total_tog_minutes = sum(tog_minutes), n_games = .N),
                lapply(.SD, sum, na.rm = TRUE)),
            by = player_id, .SDcols = stat_cols]

  rate_cols <- paste0(stat_cols, "_prate")
  agg[, (rate_cols) := lapply(.SD, function(x) x / pmax(total_tog_minutes, 1) * game_minutes),
      .SDcols = stat_cols]

  # position bucket: reuse RAPM's own bucketing so DEF/MID/FWD[/RUCK] mean
  # exactly the same thing across both ratings.
  player_rows <- .prepare_team_rapm_player_rows(seasons, comp = comp)
  if (nrow(player_rows) > 0) {
    col_map <- .team_rapm_prune_columns(player_rows, comp = comp)
    pos_lookup <- unique(col_map[, .(player_id, position_bucket)])
    agg <- merge(agg, pos_lookup, by = "player_id", all.x = TRUE)
  } else {
    agg[, position_bucket := NA_character_]
  }
  n_no_pos <- sum(is.na(agg$position_bucket))
  if (n_no_pos > 0) {
    cli::cli_warn("{n_no_pos} player{?s} have no resolvable position_bucket -- position dummies default to all-0 for {?them/these}")
  }
  agg[, `:=`(
    is_def  = as.integer(!is.na(position_bucket) & position_bucket == "DEF"),
    is_mid  = as.integer(!is.na(position_bucket) & position_bucket == "MID"),
    is_fwd  = as.integer(!is.na(position_bucket) & position_bucket == "FWD"),
    # AFLW has no RUCK bucket (INT instead) -- is_ruck is always 0 there.
    # Kept as a real column (not omitted) so feature_cols is IDENTICAL by
    # name across comps, per the file header's shared-columns rationale.
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

# .team_spm_defense_sign_constraints ----

#' Sign-constraint bounds for the defense SPM fit only (panna's convention --
#' offense carries no equivalent constraint). Shared by name across comps
#' (see file header) -- the \code{intersect()}-based "only constrain what's
#' present" logic already handles AFLW's narrower box-score column set
#' gracefully, so no comp branching is needed here.
#'
#' \code{rapm_defense} uses the NEGATIVE-is-good convention: a stat that
#' mechanically indicates GOOD defense should predict a MORE NEGATIVE
#' \code{rapm_defense} (non-positive coefficient); a stat indicating BAD
#' defense should predict MORE POSITIVE (non-negative).
#'
#' @param feature_cols Character vector, the full feature column set.
#' @return list(lower, upper) for \code{glmnet::cv.glmnet(lower.limits=,
#'   upper.limits=)}.
#' @keywords internal
.team_spm_defense_sign_constraints <- function(feature_cols) {
  good_defense_stats <- c("tackles_prate", "intercepts_prate", "one_percenters_prate",
                           "tackles_inside50_prate", "rebound50s_prate")
  bad_defense_stats  <- c("clangers_prate", "frees_against_prate")

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

# fit_team_spm ----

#' Fit SPM: two elastic-net models (offense, defense) predicting raw RAPM
#' split ratings from box-score rate features, sqrt(minutes) weighted,
#' defense-only sign constraints.
#'
#' @param spm_features Output of \code{build_team_spm_features()}.
#' @param rapm_ratings Output of \code{extract_team_rapm_ratings()}.
#'   \code{replacement_*} rows are filtered out here explicitly.
#' @param alpha Elastic-net mixing parameter. Default 0.5.
#' @param nfolds CV folds. Default 10.
#' @param seed RNG seed.
#' @return list(model_offense, model_defense, feature_cols, cv_r2_offense,
#'   cv_r2_defense, cv_r2_net, in_sample_r2_offense, in_sample_r2_defense, n, p).
#' @keywords internal
fit_team_spm <- function(spm_features, rapm_ratings, alpha = 0.5, nfolds = 10,
                         seed = 20260825) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    cli::cli_abort("glmnet package required for team SPM.")
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

  constraints <- .team_spm_defense_sign_constraints(feature_cols)

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

# predict_team_spm ----

#' Apply a fitted SPM to ANY player with box-score data -- including players
#' RAPM pooled into a replacement-level column for lack of games/minutes.
#'
#' @param spm_fit Output of \code{fit_team_spm()}.
#' @param spm_features Output of \code{build_team_spm_features()}.
#' @return data.table(player_id, n_games, total_tog_minutes, spm_offense,
#'   spm_defense, spm_net).
#' @keywords internal
predict_team_spm <- function(spm_fit, spm_features) {
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

# shrink_team_rapm ----

#' Shrink raw RAPM toward its SPM prediction, Bayesian-prior style -- games-
#' weighted empirical-Bayes blend. No separate point-in-time treatment needed
#' beyond its inputs already being point-in-time (see file header).
#'
#' @param rapm_ratings Output of \code{extract_team_rapm_ratings()}.
#'   \code{replacement_*} rows filtered out here explicitly.
#' @param spm_predictions Output of \code{predict_team_spm()}.
#' @param prior_games Numeric, shrinkage strength in games-equivalent units.
#'   Must be positive. Default 10 (a documented placeholder, not swept for
#'   either comp -- future tuning work).
#' @return data.table(player_id, n_games, rapm_offense, rapm_defense,
#'   spm_offense, spm_defense, shrinkage_weight, team_rapm_shrunk_offense,
#'   team_rapm_shrunk_defense, team_rapm_shrunk).
#' @keywords internal
shrink_team_rapm <- function(rapm_ratings, spm_predictions, prior_games = 10) {
  if (prior_games <= 0) {
    cli::cli_abort("prior_games must be positive, got {prior_games}")
  }
  if ("rating_type" %in% names(rapm_ratings)) {
    rapm_ratings <- rapm_ratings[rapm_ratings$rating_type == "individual", ]
  }
  n_rapm <- nrow(rapm_ratings)
  merged <- merge(rapm_ratings, spm_predictions, by = "player_id")
  if (nrow(merged) < n_rapm) {
    cli::cli_warn("{n_rapm - nrow(merged)} of {n_rapm} rapm_ratings player{?s} had no matching SPM prediction and {?was/were} dropped.")
  }
  merged[, shrinkage_weight := n_games / (n_games + prior_games)]
  merged[, `:=`(
    team_rapm_shrunk_offense = shrinkage_weight * rapm_offense + (1 - shrinkage_weight) * spm_offense,
    team_rapm_shrunk_defense = shrinkage_weight * rapm_defense + (1 - shrinkage_weight) * spm_defense
  )]
  merged[, team_rapm_shrunk := team_rapm_shrunk_offense - team_rapm_shrunk_defense]
  data.table::setorder(merged, -team_rapm_shrunk)
  merged[, .(player_id, n_games, rapm_offense, rapm_defense, spm_offense, spm_defense,
             shrinkage_weight, team_rapm_shrunk_offense, team_rapm_shrunk_defense,
             team_rapm_shrunk)]
}

# build_team_spm_expanding ----

#' Point-in-time SPM + shrinkage: refits SPM per season-cutoff against the
#' point-in-time RAPM output available as of that cutoff (from
#' \code{build_team_rapm_expanding()}), then shrinks. SPM has the identical
#' leak risk RAPM does (file header) -- fitting it once on all-history RAPM
#' and using it to rate an earlier season leaks future information forward
#' just as surely as a static RAPM fit does.
#'
#' @param rapm_expanding Output of \code{build_team_rapm_expanding(design="split")}
#'   -- a data.table with a \code{season} column marking each row's cutoff.
#' @param seasons,comp Passed to \code{build_team_spm_features()} for each
#'   cutoff's feature set (features themselves aren't leak-prone -- box-score
#'   totals for prior seasons are just historical fact -- but restricting to
#'   \code{seasons < cutoff} keeps SPM's own training population honestly
#'   point-in-time rather than mixing in future-season players/stats).
#' @param prior_games Passed to \code{shrink_team_rapm()}.
#' @inheritParams fit_team_spm
#' @return data.table: one row per player per season cutoff, all of
#'   \code{shrink_team_rapm()}'s columns plus \code{season}.
#' @keywords internal
build_team_spm_expanding <- function(rapm_expanding, seasons = TRUE, comp = "AFLM",
                                     alpha = 0.5, nfolds = 10, seed = 20260825,
                                     prior_games = 10) {
  .validate_afl_comp(comp)
  cutoffs <- sort(unique(rapm_expanding$season))
  all_train_seasons <- sort(unique(load_results(seasons, comp = comp)$season))

  results <- vector("list", length(cutoffs))
  for (i in seq_along(cutoffs)) {
    cutoff <- cutoffs[i]
    train_seasons <- all_train_seasons[all_train_seasons < cutoff]
    cutoff_rapm <- rapm_expanding[season == cutoff]

    spm_features <- tryCatch(
      build_team_spm_features(train_seasons, comp = comp),
      error = function(e) {
        cli::cli_warn("build_team_spm_expanding: cutoff {cutoff} failed to build SPM features: {conditionMessage(e)}")
        NULL
      }
    )
    if (is.null(spm_features)) next

    spm_fit <- fit_team_spm(spm_features, cutoff_rapm, alpha = alpha, nfolds = nfolds, seed = seed)
    spm_pred <- predict_team_spm(spm_fit, spm_features)
    shrunk <- shrink_team_rapm(cutoff_rapm, spm_pred, prior_games = prior_games)
    shrunk[, season := cutoff]
    results[[i]] <- shrunk
  }

  out <- data.table::rbindlist(results, fill = TRUE)
  if (nrow(out) == 0) {
    cli::cli_abort("build_team_spm_expanding: every season cutoff failed to fit.")
  }
  out[]
}
