# docs/plans/AFL-DECAY-XRAPM-PLAN.md -- decay-weighted, as-of-date "career
# xRAPM/SPM" engine (team_rapm_asof.R). Point-in-time filtering is the whole
# reason this file exists (decay ALONE is not leak-safe, see the file
# header) -- the leak-safety tests below are the entire point.

.N_MOCK_MATCHES_ASOF <- 10L

.mock_pgd_dated <- function(seasons = 2024L, extra_player = NULL, extra_player_date = NULL) {
  set.seed(2026082601)
  base_date <- as.Date("2024-03-01")
  do.call(rbind, lapply(seasons, function(s) {
    yr_offset <- (s - 2024L) * 365L
    do.call(rbind, lapply(seq_len(.N_MOCK_MATCHES_ASOF), function(i) {
      d <- tibble::tibble(
        match_id = paste0("m", s, "_", i), season = s,
        player_id = c("P1", "P2", "P3", "P4", "P5", "P6"),
        player_name = c("A", "B", "C", "D", "E", "F"),
        team_id = c("T1", "T1", "T1", "T2", "T2", "T2"),
        position_group = c("KEY_DEFENDER", "MIDFIELDER", "KEY_FORWARD",
                           "MEDIUM_DEFENDER", "MIDFIELDER", "MEDIUM_FORWARD"),
        time_on_ground_percentage = c(90, 85, 80, 88, 82, 78)
      )
      d
    }))
  }))
}

.mock_results_dated <- function(seasons = 2024L) {
  set.seed(2026082602)
  do.call(rbind, lapply(seasons, function(s) {
    n <- .N_MOCK_MATCHES_ASOF
    yr_offset <- (s - 2024L) * 365L
    tibble::tibble(
      match_id = paste0("m", s, "_", seq_len(n)), season = s,
      home_team_id = "T1", away_team_id = "T2",
      home_score = sample(35:80, n, replace = TRUE),
      away_score = sample(35:80, n, replace = TRUE),
      utc_start_time = as.Date("2024-03-01") + yr_offset + (seq_len(n) - 1L) * 7L
    )
  }))
}

# -----------------------------------------------------------------------------
# .team_rapm_match_dates
# -----------------------------------------------------------------------------

test_that(".team_rapm_match_dates returns one row per match_id with its date", {
  local_mocked_bindings(load_results = function(seasons, comp = "AFLM") .mock_results_dated(seasons))
  md <- torp:::.team_rapm_match_dates(2024L, comp = "AFLM")
  expect_equal(nrow(md), .N_MOCK_MATCHES_ASOF)
  expect_true(all(c("match_id", "match_date") %in% names(md)))
  expect_s3_class(md$match_date, "Date")
})

# -----------------------------------------------------------------------------
# .team_rapm_checkpoint_dates -- dynamic, fixture-calendar-derived
# -----------------------------------------------------------------------------

test_that(".team_rapm_checkpoint_dates: mid-season round uses next round's first match - 1 day", {
  fx <- tibble::tibble(
    match_id = paste0("m", 1:6), season = 2024L,
    round_number = c(1L, 1L, 2L, 2L, 3L, 3L),
    utc_start_time = as.Date(c("2024-04-04", "2024-04-07",   # round 1
                                "2024-04-11", "2024-04-14",   # round 2
                                "2024-04-18", "2024-04-21"))  # round 3
  )
  local_mocked_bindings(load_fixtures = function(all = TRUE, comp = "AFLM") fx)
  cp <- torp:::.team_rapm_checkpoint_dates(comp = "AFLM")
  r1 <- cp[round_number == 1L]
  expect_equal(r1$checkpoint_date, as.Date("2024-04-10"))  # round 2's first match (04-11) - 1
})

test_that(".team_rapm_checkpoint_dates: last round of a season falls back to last match + 2 days", {
  fx <- tibble::tibble(
    match_id = paste0("m", 1:4), season = 2024L,
    round_number = c(1L, 1L, 2L, 2L),
    utc_start_time = as.Date(c("2024-04-04", "2024-04-07", "2024-04-11", "2024-04-14"))
  )
  local_mocked_bindings(load_fixtures = function(all = TRUE, comp = "AFLM") fx)
  cp <- torp:::.team_rapm_checkpoint_dates(comp = "AFLM")
  r2 <- cp[round_number == 2L]  # last round in this mock -- no next round
  expect_equal(r2$checkpoint_date, as.Date("2024-04-16"))  # last match (04-14) + 2
})

test_that(".team_rapm_checkpoint_dates: checkpoint is NEVER after the following round's first match", {
  fx <- tibble::tibble(
    match_id = paste0("m", 1:6), season = 2024L,
    round_number = c(1L, 1L, 2L, 2L, 3L, 3L),
    utc_start_time = as.Date(c("2024-04-04", "2024-04-07",
                                "2024-04-11", "2024-04-14",
                                "2024-04-18", "2024-04-21"))
  )
  local_mocked_bindings(load_fixtures = function(all = TRUE, comp = "AFLM") fx)
  cp <- torp:::.team_rapm_checkpoint_dates(comp = "AFLM")
  data.table::setorder(cp, round_number)
  for (i in seq_len(nrow(cp) - 1)) {
    expect_true(cp$checkpoint_date[i] < cp$round_first_match[i + 1])
  }
})

# -----------------------------------------------------------------------------
# build_team_rapm_asof -- leak safety, the entire point of this file
# -----------------------------------------------------------------------------

test_that("build_team_rapm_asof: a player whose ONLY match is AFTER ref_date never appears in the design at all", {
  # 2024 season, 10 rounds weekly from 2024-03-01. A mid-career debutant P7
  # appears ONLY in the final match (2024-05-03) -- an obviously-individual
  # column with high TOG if it leaked in. ref_date is set to the day before
  # that match, so P7 must not exist in the design under any circumstance --
  # not pooled, not individual, not even present as a zero.
  base <- .mock_pgd_dated(2024L)
  extra <- tibble::tibble(
    match_id = paste0("m2024_", .N_MOCK_MATCHES_ASOF), season = 2024L,
    player_id = "P7", player_name = "NewGuy", team_id = "T1",
    position_group = "MIDFIELDER", time_on_ground_percentage = 95
  )
  pgd_with_debutant <- rbind(base, extra)

  local_mocked_bindings(
    load_player_game_data = function(seasons, ...) pgd_with_debutant,
    load_results = function(seasons, comp = "AFLM") .mock_results_dated(seasons)
  )

  last_match_date <- .mock_results_dated(2024L)$utc_start_time[.N_MOCK_MATCHES_ASOF]
  ref_date <- last_match_date - 1L  # the day BEFORE P7's only match

  design <- suppressWarnings(build_team_rapm_asof(
    ref_date, comp = "AFLM", seasons = 2024L, halflife_days = 365, threshold = 2, min_train_matches = 1L
  ))

  expect_false(is.null(design))
  expect_false("P7" %in% design$columns)
  # And the row-level leak-safety abort (age_days < 0) never fires on a
  # correctly-filtered design -- i.e. every match actually used predates ref_date.
  expect_true(all(design$match_ids != paste0("m2024_", .N_MOCK_MATCHES_ASOF)))
})

test_that("build_team_rapm_asof: the SAME debutant DOES appear once ref_date passes his match date", {
  base <- .mock_pgd_dated(2024L)
  extra <- tibble::tibble(
    match_id = paste0("m2024_", .N_MOCK_MATCHES_ASOF), season = 2024L,
    player_id = "P7", player_name = "NewGuy", team_id = "T1",
    position_group = "MIDFIELDER", time_on_ground_percentage = 95
  )
  pgd_with_debutant <- rbind(base, extra)

  local_mocked_bindings(
    load_player_game_data = function(seasons, ...) pgd_with_debutant,
    load_results = function(seasons, comp = "AFLM") .mock_results_dated(seasons)
  )

  last_match_date <- .mock_results_dated(2024L)$utc_start_time[.N_MOCK_MATCHES_ASOF]
  ref_date <- last_match_date  # ON his match date -- <=, so he IS in scope now

  design <- suppressWarnings(build_team_rapm_asof(
    ref_date, comp = "AFLM", seasons = 2024L, halflife_days = 365, threshold = 1, min_train_matches = 1L
  ))

  expect_true("P7" %in% design$columns)
})

test_that("build_team_rapm_asof: decay weights follow 0.5^(age_days/halflife_days), oldest match gets the smallest weight", {
  local_mocked_bindings(
    load_player_game_data = function(seasons, ...) .mock_pgd_dated(seasons),
    load_results = function(seasons, comp = "AFLM") .mock_results_dated(seasons)
  )
  match_dates <- .mock_results_dated(2024L)
  ref_date <- max(match_dates$utc_start_time)
  halflife <- 30  # short half-life so the 63-day-old first match decays visibly

  design <- suppressWarnings(build_team_rapm_asof(
    ref_date, comp = "AFLM", seasons = 2024L, halflife_days = halflife, threshold = 2, min_train_matches = 1L
  ))

  row_dates <- match_dates$utc_start_time[match(design$match_ids, match_dates$match_id)]
  expected_weight <- 0.5 ^ (as.numeric(ref_date - row_dates) / halflife)
  expect_equal(design$decay_weight, expected_weight, tolerance = 1e-10)

  oldest_idx <- which.min(row_dates)
  newest_idx <- which.max(row_dates)
  expect_true(design$decay_weight[oldest_idx] < design$decay_weight[newest_idx])
})

test_that("build_team_rapm_asof: returns NULL with a warning when too few matches survive the point-in-time filter", {
  local_mocked_bindings(
    load_player_game_data = function(seasons, ...) .mock_pgd_dated(seasons),
    load_results = function(seasons, comp = "AFLM") .mock_results_dated(seasons)
  )
  earliest_date <- min(.mock_results_dated(2024L)$utc_start_time)
  expect_warning(
    result <- build_team_rapm_asof(earliest_date, comp = "AFLM", seasons = 2024L, min_train_matches = 50L),
    "only 1 match"
  )
  expect_null(result)
})

# -----------------------------------------------------------------------------
# fit_team_spm_asof -- the second-order leak fix (AFL-DECAY-XRAPM-PLAN.md sec1/sec6.5)
# -----------------------------------------------------------------------------

.mock_ps_stats_dated <- function(seasons = 2024L) {
  set.seed(2026082604)
  do.call(rbind, lapply(seasons, function(s) {
    do.call(rbind, lapply(seq_len(.N_MOCK_MATCHES_ASOF), function(i) {
      tibble::tibble(
        match_id = paste0("m", s, "_", i), season = s,
        player_id = c("P1", "P2", "P3", "P4", "P5", "P6"),
        player_name = c("A", "B", "C", "D", "E", "F"),
        team_id = c("T1", "T1", "T1", "T2", "T2", "T2"),
        position = c("KEY_DEFENDER", "MIDFIELDER", "KEY_FORWARD",
                    "MEDIUM_DEFENDER", "MIDFIELDER", "MEDIUM_FORWARD"),
        time_on_ground_percentage = c(90, 85, 80, 88, 82, 78),
        disposals = sample(10:30, 6, replace = TRUE),
        tackles = sample(1:10, 6, replace = TRUE)
      )
    }))
  }))
}

test_that("fit_team_spm_asof: returns NULL with a LOUD warning when NO matches survive the point-in-time filter", {
  # 2026-08-25: fit_team_spm_asof() now filters at MATCH grain via
  # build_team_spm_features_asof(), not season grain -- so a ref_date at or
  # after the season's first match now has data to fit on. To genuinely
  # exercise "zero matches survive", ref_date must be BEFORE every match.
  local_mocked_bindings(
    load_player_game_data = function(seasons, ...) .mock_pgd_dated(seasons),
    load_player_stats = function(seasons, comp = "AFLM") .mock_ps_stats_dated(seasons),
    load_results = function(seasons, comp = "AFLM") .mock_results_dated(seasons)
  )
  ref_date <- min(.mock_results_dated(2024L)$utc_start_time) - 1L  # strictly before every match
  fake_ratings <- data.table::data.table(
    player_id = c("P1", "P2"), rating_type = "individual",
    rapm_offense = c(1, 0.5), rapm_defense = c(-0.5, -0.2)
  )
  expect_warning(
    result <- fit_team_spm_asof(ref_date, fake_ratings, comp = "AFLM", seasons = 2024L),
    "no matches on or before"
  )
  expect_null(result)
})

test_that("fit_team_spm_asof: includes matches from the CURRENT (cutoff) season up to ref_date -- match-grain, not season-grain", {
  # Behavioural change 2026-08-25: the old season-block version excluded the
  # entire cutoff season, even matches strictly before ref_date within it.
  # The new match-grain version correctly includes them -- this is a real
  # data-availability improvement, not just a decay-weighting change, and is
  # the thing this test locks in so a regression back to season-grain would fail it.
  seasons_avail <- c(2023L, 2024L)
  local_mocked_bindings(
    load_player_game_data = function(seasons, ...) do.call(rbind, lapply(seasons, .mock_pgd_dated)),
    load_player_stats = function(seasons, comp = "AFLM") do.call(rbind, lapply(seasons, .mock_ps_stats_dated)),
    load_results = function(seasons, comp = "AFLM") do.call(rbind, lapply(seasons, .mock_results_dated))
  )
  res_2024 <- .mock_results_dated(2024L)
  # ref_date well into 2024 -- several rounds should already have been played.
  ref_date <- sort(unique(res_2024$utc_start_time))[5]
  fake_ratings <- data.table::data.table(
    player_id = c("P1", "P2", "P3", "P4", "P5", "P6"), rating_type = "individual",
    rapm_offense = c(1, 0.5, 0.3, 0.2, 0.1, -0.1),
    rapm_defense = c(-0.5, -0.2, -0.1, 0.1, 0.2, 0.3)
  )
  features_2024_included <- build_team_spm_features_asof(ref_date, comp = "AFLM", seasons = seasons_avail)
  features_2023_only <- build_team_spm_features_asof(min(res_2024$utc_start_time) - 1L, comp = "AFLM", seasons = seasons_avail)
  # The 2024-inclusive feature set must reflect MORE effective data than the
  # 2023-only one for at least one player (more matches contributing).
  expect_true(any(features_2024_included$model_df$n_games > (features_2023_only$model_df$n_games[
    match(features_2024_included$model_df$player_id, features_2023_only$model_df$player_id)] %||% 0)))

  result <- suppressWarnings(fit_team_spm_asof(ref_date, fake_ratings, comp = "AFLM", seasons = seasons_avail, nfolds = 4))
  expect_false(is.null(result))
  expect_true("spm_halflife_days" %in% names(result))
})

test_that("build_team_spm_features_asof: a debutant whose ONLY match is AFTER ref_date never appears at all", {
  base <- .mock_ps_stats_dated(2024L)
  extra <- base[base$match_id == paste0("m2024_", .N_MOCK_MATCHES_ASOF), ]
  extra$match_id <- paste0("m2024_", .N_MOCK_MATCHES_ASOF + 1L)
  extra$player_id <- "P_DEBUT"
  extra$time_on_ground_percentage <- 95
  ps_with_debutant <- rbind(base, extra)

  res <- .mock_results_dated(2024L)
  extra_res <- res[1, ]
  extra_res$match_id <- paste0("m2024_", .N_MOCK_MATCHES_ASOF + 1L)
  extra_res$utc_start_time <- max(res$utc_start_time) + 7L  # after every existing match
  res_with_debutant <- rbind(res, extra_res)

  local_mocked_bindings(
    load_player_stats = function(seasons, comp = "AFLM") ps_with_debutant,
    load_results = function(seasons, comp = "AFLM") res_with_debutant
  )
  ref_date <- max(res$utc_start_time)  # before the debutant's match
  feats_before <- build_team_spm_features_asof(ref_date, comp = "AFLM", seasons = 2024L)
  expect_false("P_DEBUT" %in% feats_before$model_df$player_id)

  feats_after <- build_team_spm_features_asof(extra_res$utc_start_time, comp = "AFLM", seasons = 2024L)
  expect_true("P_DEBUT" %in% feats_after$model_df$player_id)
})

test_that("build_team_spm_features_asof: decay weighting reduces an older match's contribution relative to a recent one", {
  # halflife_days = 1 makes the effect large and easy to assert on.
  local_mocked_bindings(
    load_player_stats = function(seasons, comp = "AFLM") .mock_ps_stats_dated(2024L),
    load_results = function(seasons, comp = "AFLM") .mock_results_dated(2024L)
  )
  res <- .mock_results_dated(2024L)
  ref_date <- max(res$utc_start_time)
  feats_no_decay <- build_team_spm_features_asof(ref_date, comp = "AFLM", seasons = 2024L, halflife_days = 100000)
  feats_decayed   <- build_team_spm_features_asof(ref_date, comp = "AFLM", seasons = 2024L, halflife_days = 1)
  # With halflife=1 day, matches from many days ago contribute ~nothing, so
  # total_tog_minutes (the decay-weighted "effective" minutes) must be
  # strictly smaller than the near-no-decay version for every player.
  merged <- merge(feats_no_decay$model_df[, .(player_id, tog_no_decay = total_tog_minutes)],
                   feats_decayed$model_df[, .(player_id, tog_decayed = total_tog_minutes)],
                   by = "player_id")
  expect_true(all(merged$tog_decayed <= merged$tog_no_decay))
  expect_true(any(merged$tog_decayed < merged$tog_no_decay))
})
