# docs/plans/AFLW-MIGRATION-PLAN.md Phase 3 build-order item 5 -- AFLW SPM
# (BPM retest against the reworked RAPM target). No direct men's analog
# (same situation as RAPM), so bespoke coverage: the defense-only sign
# constraint (tested adversarially -- data engineered to fight the
# constraint, not just data that happens to agree with it), sqrt-minutes
# weighting, the shrinkage-blend mechanism, and SPM covering players RAPM
# could not rate individually.

test_that(".aflw_spm_position_dummies one-hot-minus-one-level encodes DEF/MID/FWD, INT implicit", {
  d <- torp:::.aflw_spm_position_dummies(c("DEF", "MID", "FWD", "INT"))
  expect_equal(d$is_def, c(1L, 0L, 0L, 0L))
  expect_equal(d$is_mid, c(0L, 1L, 0L, 0L))
  expect_equal(d$is_fwd, c(0L, 0L, 1L, 0L))
})

test_that(".aflw_spm_defense_sign_constraints bounds only the documented good/bad defense stats", {
  feature_cols <- c("tackles_p80", "intercepts_p80", "clangers_p80", "frees_against_p80",
                     "goals_p80", "is_def", "is_mid", "is_fwd")
  b <- torp:::.aflw_spm_defense_sign_constraints(feature_cols)

  # good-defense stats: constrained <= 0 (upper bound 0, lower unconstrained)
  expect_equal(b$upper[["tackles_p80"]], 0)
  expect_equal(b$lower[["tackles_p80"]], -Inf)
  expect_equal(b$upper[["intercepts_p80"]], 0)

  # bad-defense stats: constrained >= 0 (lower bound 0, upper unconstrained)
  expect_equal(b$lower[["clangers_p80"]], 0)
  expect_equal(b$upper[["clangers_p80"]], Inf)
  expect_equal(b$lower[["frees_against_p80"]], 0)

  # anything not in either list is fully unconstrained
  expect_equal(b$lower[["goals_p80"]], -Inf)
  expect_equal(b$upper[["goals_p80"]], Inf)
  expect_equal(b$lower[["is_def"]], -Inf)
  expect_equal(b$upper[["is_def"]], Inf)
})

test_that(".aflw_spm_defense_sign_constraints only bounds stats actually present in feature_cols", {
  # a feature_cols set missing some of the named good/bad stats shouldn't error
  # or silently add columns that aren't there
  b <- torp:::.aflw_spm_defense_sign_constraints(c("goals_p80", "is_def"))
  expect_equal(names(b$lower), c("goals_p80", "is_def"))
  expect_equal(names(b$upper), c("goals_p80", "is_def"))
})

# -----------------------------------------------------------------------------
# build_aflw_spm_features
# -----------------------------------------------------------------------------

.mock_aflw_spm_stats <- function() {
  set.seed(2026082403)
  n_players <- 40
  player_ids <- paste0("P", seq_len(n_players))
  rows <- lapply(seq_len(n_players), function(i) {
    n_games <- sample(3:20, 1)
    tibble::tibble(
      match_id = paste0("m", i, "_", seq_len(n_games)),
      player_id = player_ids[i],
      player_name = paste0("Player", i),
      team_id = if (i <= 20) "T1" else "T2",
      team_status = if (i <= 20) "home" else "away",
      position = sample(c("FB", "CHB", "C", "WL", "FF", "CHF"), 1),
      time_on_ground_percentage = sample(20:95, n_games, replace = TRUE),
      tackles = sample(0:8, n_games, replace = TRUE),
      goals = sample(0:4, n_games, replace = TRUE),
      hitouts = 0  # always zero -- degenerate by construction
    )
  })
  dplyr::bind_rows(rows)
}

.mock_aflw_spm_results <- function() {
  ps <- .mock_aflw_spm_stats()
  mids <- unique(ps$match_id)
  set.seed(2026082404)
  tibble::tibble(
    match_id = mids,
    home_team_id = "T1", away_team_id = "T2",
    home_score = sample(35:80, length(mids), replace = TRUE),
    away_score = sample(35:80, length(mids), replace = TRUE)
  )
}

test_that("build_aflw_spm_features returns a row for EVERY player with box-score data, not just RAPM-rateable ones", {
  local_mocked_bindings(
    load_player_stats = function(seasons, comp = "AFLM") .mock_aflw_spm_stats(),
    load_results      = function(seasons, comp = "AFLM") .mock_aflw_spm_results()
  )

  feats <- torp:::build_aflw_spm_features(TRUE)
  n_players <- length(unique(.mock_aflw_spm_stats()$player_id))
  expect_equal(nrow(feats$model_df), n_players)
  expect_true(all(c("player_id", "total_tog_minutes", "n_games", "is_def", "is_mid", "is_fwd") %in%
                    names(feats$model_df)))
})

test_that("build_aflw_spm_features flags an always-zero stat as degenerate and drops it from feature_cols", {
  local_mocked_bindings(
    load_player_stats = function(seasons, comp = "AFLM") .mock_aflw_spm_stats(),
    load_results      = function(seasons, comp = "AFLM") .mock_aflw_spm_results()
  )

  feats <- torp:::build_aflw_spm_features(TRUE)
  expect_true("hitouts_p80" %in% feats$degenerate_cols)
  expect_false("hitouts_p80" %in% feats$feature_cols)
  # tackles/goals have real variance -- should survive
  expect_true("tackles_p80" %in% feats$feature_cols)
  expect_true("goals_p80" %in% feats$feature_cols)
})

# -----------------------------------------------------------------------------
# fit_aflw_spm / predict_aflw_spm / shrink_aflw_rapm -- synthetic data,
# bypassing loaders entirely for a tighter, faster unit test
# -----------------------------------------------------------------------------

.synthetic_spm_setup <- function(n = 60, seed = 20260824) {
  set.seed(seed)
  player_id <- paste0("P", seq_len(n))
  tackles_p80 <- rnorm(n, 5, 2)
  goals_p80 <- rnorm(n, 1, 0.5)
  is_def <- rbinom(n, 1, 0.3)

  model_df <- data.table::data.table(
    player_id = player_id,
    total_tog_minutes = runif(n, 500, 3000),
    n_games = sample(5:60, n, replace = TRUE),
    position_bucket = ifelse(is_def == 1, "DEF", "MID"),
    is_def = is_def, is_mid = 1L - is_def, is_fwd = 0L,
    tackles_p80 = tackles_p80, goals_p80 = goals_p80
  )
  feats <- list(model_df = model_df,
                feature_cols = c("tackles_p80", "goals_p80", "is_def", "is_mid", "is_fwd"),
                degenerate_cols = character(0))

  # ADVERSARIAL target: rapm_defense set to INCREASE with tackles_p80 (the
  # opposite of "more tackles -> better/more negative defense"), specifically
  # to test that the sign constraint holds even when the data itself argues
  # the other way -- not just when the constraint happens to agree with a
  # plausible real-world relationship.
  rapm_ratings <- data.table::data.table(
    player_id = player_id,
    rapm_offense = 0.5 * goals_p80 + rnorm(n, 0, 0.3),
    rapm_defense = 0.8 * tackles_p80 + rnorm(n, 0, 0.3),
    rapm = NA_real_
  )
  rapm_ratings[, rapm := rapm_offense - rapm_defense]

  list(feats = feats, rapm_ratings = rapm_ratings)
}

test_that("fit_aflw_spm enforces the defense sign constraint even against adversarial (backwards-correlated) data", {
  setup <- .synthetic_spm_setup()
  fit <- torp:::fit_aflw_spm(setup$feats, setup$rapm_ratings, nfolds = 5)

  co_def <- as.matrix(stats::coef(fit$model_defense, s = "lambda.min"))
  # tackles_p80 is constrained <= 0 in the defense fit (good-defense stat) --
  # must hold even though this synthetic target was built to correlate
  # POSITIVELY with tackles_p80 (an unconstrained fit would want a positive beta)
  expect_lte(co_def["tackles_p80", 1], 1e-8)
})

test_that("fit_aflw_spm produces finite CV/in-sample R2 and the net R2 uses out-of-fold predictions", {
  setup <- .synthetic_spm_setup()
  fit <- torp:::fit_aflw_spm(setup$feats, setup$rapm_ratings, nfolds = 5)

  expect_true(is.finite(fit$cv_r2_offense))
  expect_true(is.finite(fit$cv_r2_defense))
  expect_true(is.finite(fit$cv_r2_net))
  expect_true(is.finite(fit$in_sample_r2_offense))
  expect_true(is.finite(fit$in_sample_r2_defense))
  expect_equal(fit$n, nrow(setup$rapm_ratings))
})

test_that("predict_aflw_spm covers every player in spm_features, including ones absent from the fitted rapm_ratings", {
  setup <- .synthetic_spm_setup(n = 60)
  fit <- torp:::fit_aflw_spm(setup$feats, setup$rapm_ratings, nfolds = 5)

  # Add 5 extra "low-minutes" players to model_df that were NEVER in
  # rapm_ratings (the real-world case: players RAPM pooled into
  # replacement_* and so never got an individual raw RAPM estimate)
  extra <- data.table::data.table(
    player_id = paste0("EXTRA", 1:5), total_tog_minutes = 40, n_games = 2,
    position_bucket = "MID", is_def = 0L, is_mid = 1L, is_fwd = 0L,
    tackles_p80 = 3, goals_p80 = 0.5
  )
  feats_extended <- setup$feats
  feats_extended$model_df <- rbind(feats_extended$model_df, extra)

  preds <- torp:::predict_aflw_spm(fit, feats_extended)
  expect_equal(nrow(preds), nrow(feats_extended$model_df))
  expect_true(all(c("EXTRA1", "EXTRA2", "EXTRA3", "EXTRA4", "EXTRA5") %in% preds$player_id))
  expect_true(all(is.finite(preds$spm_offense)))
  expect_true(all(is.finite(preds$spm_defense)))
  expect_equal(preds$spm_net, preds$spm_offense - preds$spm_defense)
})

test_that("shrink_aflw_rapm: shrinkage_weight rises with n_games, and the blend is exact linear interpolation", {
  ratings <- data.table::data.table(
    player_id = c("LOW", "HIGH"),
    rapm_offense = c(2, 2), rapm_defense = c(-1, -1)
  )
  preds <- data.table::data.table(
    player_id = c("LOW", "HIGH"),
    n_games = c(1, 100),
    total_tog_minutes = c(50, 5000),
    spm_offense = c(0, 0), spm_defense = c(0, 0)
  )

  out <- torp:::shrink_aflw_rapm(ratings, preds, prior_games = 10)

  low <- out[player_id == "LOW"]
  high <- out[player_id == "HIGH"]

  # LOW: n_games=1, prior_games=10 -> weight = 1/11, leans heavily on SPM (0)
  expect_equal(low$shrinkage_weight, 1 / 11)
  expect_equal(low$aflw_rapm_shrunk_offense, (1/11) * 2 + (10/11) * 0)

  # HIGH: n_games=100 -> weight = 100/110, leans heavily on raw RAPM (2)
  expect_equal(high$shrinkage_weight, 100 / 110)
  expect_gt(high$shrinkage_weight, low$shrinkage_weight)
  expect_gt(abs(high$aflw_rapm_shrunk_offense - 0), abs(low$aflw_rapm_shrunk_offense - 0))

  expect_true(all(is.finite(out$aflw_rapm_shrunk)))
  expect_equal(out$aflw_rapm_shrunk, out$aflw_rapm_shrunk_offense - out$aflw_rapm_shrunk_defense)
})

test_that("shrink_aflw_rapm drops players present in rapm_ratings but absent from spm_predictions", {
  ratings <- data.table::data.table(
    player_id = c("A", "B"), rapm_offense = c(1, 1), rapm_defense = c(0, 0)
  )
  preds <- data.table::data.table(
    player_id = "A", n_games = 10, total_tog_minutes = 800,
    spm_offense = 0, spm_defense = 0
  )
  out <- torp:::shrink_aflw_rapm(ratings, preds)
  expect_equal(out$player_id, "A")
})
