# docs/plans/AFLW-MIGRATION-PLAN.md / AFLM-RAPM-SPM-PLAN.md -- shared
# comp-parameterized SPM + RAPM-shrinkage engine (team_spm.R). Covers both
# comps plus the point-in-time expanding-window leak-safety guarantee for
# build_team_spm_expanding() (SPM has the identical leak risk RAPM does --
# see team_spm.R's header).

.spm_stat_cols <- c("disposals", "tackles", "intercepts", "one_percenters",
                    "tackles_inside50", "rebound50s", "clangers", "frees_against")

.mock_spm_ps <- function(seasons, comp = "AFLM", player_ids = paste0("P", 1:6),
                         new_player_season = NULL, new_player_id = "P99") {
  set.seed(2026082510)
  n_matches <- 20L
  rows <- do.call(rbind, lapply(seasons, function(s) {
    ids <- player_ids
    if (!is.null(new_player_season) && s == new_player_season) ids <- c(ids, new_player_id)
    do.call(rbind, lapply(seq_len(n_matches), function(i) {
      d <- tibble::tibble(
        match_id = paste0(if (comp == "AFLW") "w" else "m", s, "_", i), season = s,
        player_id = ids, player_name = ids,
        team_id = rep(c("T1", "T2"), length.out = length(ids)),
        time_on_ground_percentage = stats::runif(length(ids), 50, 95)
      )
      for (col in .spm_stat_cols) {
        d[[col]] <- stats::rpois(length(ids), 5)
      }
      if (comp == "AFLW") {
        d$team_status <- rep(c("home", "away"), length.out = length(ids))
        d$position <- rep(c("FB", "C", "FF", "CHB", "WL", "CHF"), length.out = length(ids))
      } else {
        d$position_group <- rep(c("KEY_DEFENDER", "MIDFIELDER", "KEY_FORWARD",
                                  "MEDIUM_DEFENDER", "MIDFIELDER", "MEDIUM_FORWARD"),
                                length.out = length(ids))
      }
      d
    }))
  }))
  rows
}

.mock_spm_pgd <- function(seasons, ...) {
  ps <- .mock_spm_ps(seasons, comp = "AFLM")
  ps[, setdiff(names(ps), c("time_on_ground_percentage", .spm_stat_cols))]
  tibble::tibble(
    match_id = ps$match_id, season = ps$season, player_id = ps$player_id,
    player_name = ps$player_name, team_id = ps$team_id,
    position_group = ps$position_group,
    time_on_ground_percentage = ps$time_on_ground_percentage
  )
}

.mock_spm_results <- function(seasons, comp = "AFLM") {
  do.call(rbind, lapply(seasons, function(s) {
    n <- 20L
    tibble::tibble(
      match_id = paste0(if (comp == "AFLW") "w" else "m", s, "_", seq_len(n)), season = s,
      home_team_id = "T1", away_team_id = "T2",
      home_score = sample(35:80, n, replace = TRUE), away_score = sample(35:80, n, replace = TRUE)
    )
  }))
}

# -----------------------------------------------------------------------------
# build_team_spm_features
# -----------------------------------------------------------------------------

test_that("build_team_spm_features (AFLM): correct shape, feature_cols use _prate suffix", {
  local_mocked_bindings(
    load_player_stats = function(seasons, ...) .mock_spm_ps(seasons, comp = "AFLM"),
    load_player_game_data = .mock_spm_pgd,
    load_results = function(seasons, comp = "AFLM") .mock_spm_results(seasons, comp = "AFLM")
  )
  feats <- build_team_spm_features(2024L, comp = "AFLM")
  expect_true(all(c("is_def", "is_mid", "is_fwd", "is_ruck") %in% feats$feature_cols))
  expect_true(any(grepl("_prate$", feats$feature_cols)))
  expect_true(all(c("player_id", "total_tog_minutes", "n_games") %in% names(feats$model_df)))
  expect_equal(nrow(feats$model_df), 6L)
})

test_that("build_team_spm_features (AFLW): is_ruck is always 0 (no RUCK bucket)", {
  local_mocked_bindings(
    load_player_stats = function(seasons, comp = "AFLM") .mock_spm_ps(seasons, comp = "AFLW"),
    load_results = function(seasons, comp = "AFLM") .mock_spm_results(seasons, comp = "AFLW")
  )
  feats <- build_team_spm_features(2024L, comp = "AFLW")
  expect_true(all(feats$model_df$is_ruck == 0))
})

test_that("build_team_spm_features drops near-zero-variance rate columns as degenerate, and reports them", {
  ps_const <- .mock_spm_ps(2024L, comp = "AFLM")
  # clangers set EXACTLY proportional to each row's own tog_minutes (k=0.05)
  # so the aggregated rate (sum(clangers)/sum(tog_minutes)*game_minutes) comes
  # out to the same constant 6 for every player regardless of how much they
  # played -- a genuinely degenerate (zero-variance) rate column.
  tog_minutes <- pmin(pmax(ps_const$time_on_ground_percentage / 100, 0), 1) * 120
  ps_const$clangers <- 0.05 * tog_minutes
  local_mocked_bindings(
    load_player_stats = function(seasons, ...) ps_const,
    load_player_game_data = .mock_spm_pgd,
    load_results = function(seasons, comp = "AFLM") .mock_spm_results(seasons, comp = "AFLM")
  )
  feats <- build_team_spm_features(2024L, comp = "AFLM")
  expect_true("clangers_prate" %in% feats$degenerate_cols)
  expect_false("clangers_prate" %in% feats$feature_cols)
})

test_that("build_team_spm_features aborts cleanly on zero rows", {
  local_mocked_bindings(load_player_stats = function(seasons, ...) tibble::tibble())
  expect_error(build_team_spm_features(2024L, comp = "AFLM"), "No player_stats")
})

# -----------------------------------------------------------------------------
# .team_spm_defense_sign_constraints -- shared, no comp branching
# -----------------------------------------------------------------------------

test_that(".team_spm_defense_sign_constraints: good-defense stats capped at 0 upper, bad-defense floored at 0 lower", {
  cols <- c("tackles_prate", "intercepts_prate", "clangers_prate", "frees_against_prate", "is_mid")
  con <- torp:::.team_spm_defense_sign_constraints(cols)
  expect_equal(con$upper[["tackles_prate"]], 0)
  expect_equal(con$upper[["intercepts_prate"]], 0)
  expect_equal(con$lower[["clangers_prate"]], 0)
  expect_equal(con$lower[["frees_against_prate"]], 0)
  expect_equal(con$upper[["is_mid"]], Inf)
  expect_equal(con$lower[["is_mid"]], -Inf)
})

test_that(".team_spm_defense_sign_constraints warns (not errors) when constraint stats are missing, and fits unconstrained for them", {
  expect_warning(
    con <- torp:::.team_spm_defense_sign_constraints(c("is_mid", "is_fwd")),
    "not present in feature_cols"
  )
  expect_equal(con$upper[["is_mid"]], Inf)
})

# -----------------------------------------------------------------------------
# fit_team_spm / predict_team_spm / shrink_team_rapm
# -----------------------------------------------------------------------------

.mock_rapm_ratings <- function(player_ids = paste0("P", 1:6)) {
  set.seed(2026082511)
  data.table::data.table(
    player_id = player_ids, rating_type = "individual",
    rapm_offense = stats::rnorm(length(player_ids), 0, 5),
    rapm_defense = stats::rnorm(length(player_ids), 0, 5)
  )[, rapm := rapm_offense - rapm_defense]
}

test_that("fit_team_spm + predict_team_spm: finite CV R2, predictions cover ALL box-score players including those RAPM never rated individually", {
  local_mocked_bindings(
    load_player_stats = function(seasons, ...) .mock_spm_ps(seasons, comp = "AFLM"),
    load_player_game_data = .mock_spm_pgd,
    load_results = function(seasons, comp = "AFLM") .mock_spm_results(seasons, comp = "AFLM")
  )
  feats <- build_team_spm_features(2024L, comp = "AFLM")
  rapm <- .mock_rapm_ratings()  # rates P1-P6, matches all 6 box-score players
  fit <- suppressWarnings(fit_team_spm(feats, rapm, nfolds = 3))

  expect_true(is.finite(fit$cv_r2_offense))
  expect_true(is.finite(fit$cv_r2_defense))
  expect_equal(fit$n, 6L)

  pred <- predict_team_spm(fit, feats)
  expect_equal(nrow(pred), nrow(feats$model_df))  # predicts for every box-score player, not just RAPM-rated ones
  expect_true(all(is.finite(pred$spm_net)))
})

test_that("fit_team_spm warns and drops rows when rapm_ratings has players absent from spm_features", {
  local_mocked_bindings(
    load_player_stats = function(seasons, ...) .mock_spm_ps(seasons, comp = "AFLM"),
    load_player_game_data = .mock_spm_pgd,
    load_results = function(seasons, comp = "AFLM") .mock_spm_results(seasons, comp = "AFLM")
  )
  feats <- build_team_spm_features(2024L, comp = "AFLM")
  rapm <- .mock_rapm_ratings(c(paste0("P", 1:6), "GHOST"))
  expect_warning(
    fit <- fit_team_spm(feats, rapm, nfolds = 3),
    "no matching spm_features row"
  )
  expect_equal(fit$n, 6L)
})

test_that("fit_team_spm aborts (torp_spm_too_few_rows) rather than passing a sub-3 fold count to glmnet", {
  # AFLW-crash regression (AFL-DECAY-XRAPM-PLAN.md sec9): the RAPM pruning
  # threshold can pool almost every player on a thin early-history training
  # pool, leaving a merged design of 2 rows -- min(nfolds, nrow(X)) = 2, which
  # glmnet::cv.glmnet rejects with its own cryptic "nfolds must be bigger than
  # 3" error. This must surface as a controlled, catchable condition instead.
  local_mocked_bindings(
    load_player_stats = function(seasons, ...) .mock_spm_ps(seasons, comp = "AFLM"),
    load_player_game_data = .mock_spm_pgd,
    load_results = function(seasons, comp = "AFLM") .mock_spm_results(seasons, comp = "AFLM")
  )
  feats <- build_team_spm_features(2024L, comp = "AFLM")
  rapm_thin <- .mock_rapm_ratings(c("P1", "P2"))  # only 2 of the 6 box-score players individually rated

  err <- tryCatch({
    fit_team_spm(feats, rapm_thin, nfolds = 10)
    NULL
  }, error = function(e) e)

  expect_false(is.null(err))
  expect_true(inherits(err, "torp_spm_too_few_rows"))
  expect_match(conditionMessage(err), "only 2 training row")
})

test_that("fit_team_spm_asof degrades to a loud NULL (not a crash) when the training pool is too thin for fit_team_spm", {
  mock_results_with_dates <- function(seasons, comp = "AFLM") {
    res <- .mock_spm_results(seasons, comp = comp)
    res$utc_start_time <- as.POSIXct(paste0(res$season, "-04-01"), tz = "UTC") +
      (seq_len(nrow(res)) * 3600)
    res
  }
  local_mocked_bindings(
    load_player_stats = function(seasons, ...) .mock_spm_ps(seasons, comp = "AFLM"),
    load_player_game_data = .mock_spm_pgd,
    load_results = function(seasons, comp = "AFLM") {
      rbind(mock_results_with_dates(2023L, comp = "AFLM"), mock_results_with_dates(2024L, comp = "AFLM"))
    }
  )
  rapm_thin <- .mock_rapm_ratings(c("P1", "P2"))  # same thin-pool shape as the AFLW crash

  expect_warning(
    out <- fit_team_spm_asof(as.Date("2024-06-01"), rapm_thin, comp = "AFLM"),
    "only 2 training row"
  )
  expect_null(out)
})

test_that("shrink_team_rapm: hand-computed shrinkage weight and blend", {
  # rapm_ratings mirrors extract_team_rapm_ratings()'s real columns -- no
  # n_games there; n_games/total_tog_minutes come only from spm_predictions
  # (predict_team_spm()'s output), never from the RAPM side.
  rapm <- data.table::data.table(player_id = "P1", rating_type = "individual",
                                 rapm_offense = 10, rapm_defense = 2)
  spm_pred <- data.table::data.table(player_id = "P1", n_games = 10, total_tog_minutes = 900,
                                     spm_offense = 4, spm_defense = 1, spm_net = 3)
  out <- shrink_team_rapm(rapm, spm_pred, prior_games = 10)

  # weight = n_games / (n_games + prior_games) = 10/20 = 0.5
  expect_equal(out$shrinkage_weight, 0.5)
  expect_equal(out$team_rapm_shrunk_offense, 0.5 * 10 + 0.5 * 4)  # 7
  expect_equal(out$team_rapm_shrunk_defense, 0.5 * 2 + 0.5 * 1)   # 1.5
  expect_equal(out$team_rapm_shrunk, 7 - 1.5)
})

test_that("shrink_team_rapm rejects non-positive prior_games", {
  rapm <- data.table::data.table(player_id = "P1", rapm_offense = 1, rapm_defense = 1)
  spm_pred <- data.table::data.table(player_id = "P1", n_games = 10, total_tog_minutes = 900,
                                     spm_offense = 1, spm_defense = 1, spm_net = 0)
  expect_error(shrink_team_rapm(rapm, spm_pred, prior_games = 0), "must be positive")
  expect_error(shrink_team_rapm(rapm, spm_pred, prior_games = -5), "must be positive")
})

test_that("shrink_team_rapm warns and drops rows when SPM has no prediction for a rated player", {
  rapm <- data.table::data.table(player_id = c("P1", "P2"), rating_type = "individual",
                                 rapm_offense = c(1, 1), rapm_defense = c(1, 1))
  spm_pred <- data.table::data.table(player_id = "P1", n_games = 10, total_tog_minutes = 900,
                                     spm_offense = 1, spm_defense = 1, spm_net = 0)
  expect_warning(out <- shrink_team_rapm(rapm, spm_pred), "no matching SPM prediction")
  expect_equal(nrow(out), 1L)
})

# -----------------------------------------------------------------------------
# build_team_spm_expanding -- point-in-time leak safety
# -----------------------------------------------------------------------------

test_that("build_team_spm_expanding: a season cutoff's SPM training population excludes a player who debuts in or after that cutoff's own season", {
  # P99 debuts in season 2024. The 2023 cutoff's SPM must be fit on seasons
  # < 2023 = {2021,2022} box-score data, which never contain P99 -- so P99
  # must not influence, or appear predicted for, the 2023-cutoff output's
  # training population. He CAN appear in predict_team_spm's OUTPUT rows
  # (predict is applied to spm_features built for that cutoff's *own*
  # `seasons` arg passed through, which is the full window here) -- the
  # assertion that matters is on TRAINING, which build_team_rapm_expanding's
  # own fixture already proves; here we check the mock capture instead.
  seasons_avail <- c(2021L, 2022L, 2023L, 2024L)
  train_calls <- list()

  local_mocked_bindings(
    load_player_stats = function(seasons, comp = "AFLM") {
      train_calls[[length(train_calls) + 1]] <<- seasons
      .mock_spm_ps(seasons, comp = "AFLM", new_player_season = 2024L, new_player_id = "P99")
    },
    load_player_game_data = function(seasons, ...) {
      .mock_spm_pgd(seasons) |>
        (\(d) if (2024L %in% seasons) rbind(d, tibble::tibble(
          match_id = paste0("m2024_", seq_len(20L)), season = 2024L, player_id = "P99",
          player_name = "P99", team_id = "T1", position_group = "MIDFIELDER",
          time_on_ground_percentage = 90
        )) else d)()
    },
    load_results = function(seasons, comp = "AFLM") .mock_spm_results(seasons, comp = "AFLM")
  )

  rapm_expanding <- data.table::data.table(
    player_id = rep(paste0("P", 1:6), 2), rating_type = "individual",
    rapm_offense = stats::rnorm(12), rapm_defense = stats::rnorm(12),
    season = rep(c(2023L, 2024L), each = 6)
  )

  spm_expanding <- suppressWarnings(suppressMessages(
    build_team_spm_expanding(rapm_expanding, seasons_avail, comp = "AFLM", nfolds = 3)
  ))

  expect_equal(sort(unique(spm_expanding$season)), c(2023L, 2024L))

  # Find the load_player_stats() call made for the 2023 cutoff's SPM features
  # (train_seasons < 2023 = {2021,2022}) and confirm P99's debut season
  # (2024) was never requested as part of it.
  seasons_for_2023_cutoff <- Filter(function(s) all(s < 2023L), train_calls)
  expect_true(length(seasons_for_2023_cutoff) > 0)
  for (s in seasons_for_2023_cutoff) expect_false(2024L %in% s)
})

test_that("build_team_spm_expanding aborts cleanly if every cutoff fails to fit", {
  local_mocked_bindings(
    load_player_stats = function(seasons, ...) tibble::tibble(),
    load_results = function(seasons, comp = "AFLM") .mock_spm_results(seasons, comp = "AFLM")
  )
  rapm_expanding <- data.table::data.table(
    player_id = "P1", rating_type = "individual", rapm_offense = 1, rapm_defense = 1, season = 2023L
  )
  expect_error(
    suppressWarnings(build_team_spm_expanding(rapm_expanding, c(2021L, 2022L, 2023L), comp = "AFLM")),
    "every season cutoff failed"
  )
})
