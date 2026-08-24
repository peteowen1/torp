# docs/plans/AFLW-MIGRATION-PLAN.md / AFLM-RAPM-SPM-PLAN.md -- shared
# comp-parameterized team RAPM engine (team_rapm.R). Covers both comps plus
# the point-in-time expanding-window leak-safety guarantee, which is the
# entire reason this file exists in its current form (a static full-history
# fit was found leaking future-season information into earlier predictions,
# 2026-08-25 -- see build_team_rapm_expanding()'s own docs).

# -----------------------------------------------------------------------------
# Position bucketing -- comp-dispatched
# -----------------------------------------------------------------------------

test_that(".team_rapm_position_bucket dispatches correctly for AFLM (position_group, no INT)", {
  pos <- c("KEY_DEFENDER", "MEDIUM_DEFENDER", "MIDFIELDER", "KEY_FORWARD", "MEDIUM_FORWARD", "RUCK", NA)
  bucket <- torp:::.team_rapm_position_bucket(pos, comp = "AFLM")
  expect_equal(bucket, c("DEF", "DEF", "MID", "FWD", "FWD", "RUCK", NA_character_))
})

test_that(".team_rapm_position_bucket dispatches correctly for AFLW (raw slot code, has INT)", {
  pos <- c("FB", "BPL", "CHB", "C", "WR", "RK", "CHF", "FF", "INT", "EMERG", "UNKNOWN_CODE")
  bucket <- torp:::.team_rapm_position_bucket(pos, comp = "AFLW")
  expect_equal(bucket, c("DEF", "DEF", "DEF", "MID", "MID", "MID", "FWD", "FWD", "INT", "INT", "INT"))
})

test_that(".team_rapm_position_bucket defaults to AFLM", {
  expect_equal(torp:::.team_rapm_position_bucket("MIDFIELDER"), "MID")
})

# -----------------------------------------------------------------------------
# Column pruning -- unit-aware, comp-dispatched defaults
# -----------------------------------------------------------------------------

test_that(".team_rapm_prune_columns: AFLM default unit is games, threshold TEAM_RAPM_MIN_GAMES", {
  rows <- data.table::data.table(
    player_id = c("A", "A", "A", "B", "C"),
    player_name = c("Alice", "Alice", "Alice", "Bob", "Carol"),
    position_bucket = c("DEF", "DEF", "DEF", "MID", "DEF"),
    tog = c(0.9, 0.85, 0.95, 0.1, 0.05)
  )
  pruned <- torp:::.team_rapm_prune_columns(rows, threshold = 2, comp = "AFLM")
  expect_equal(pruned[player_id == "A"]$rapm_col, "A")  # 3 games >= 2
  expect_equal(pruned[player_id == "B"]$rapm_col, "replacement_MID")  # 1 game < 2
})

test_that(".team_rapm_prune_columns: AFLW default unit is tog_minutes, threshold AFLW_RAPM_MIN_TOG_MINUTES", {
  rows <- data.table::data.table(
    player_id = c("A", "A", "A", "B", "C"),
    player_name = c("Alice", "Alice", "Alice", "Bob", "Carol"),
    position_bucket = c("DEF", "DEF", "DEF", "MID", "DEF"),
    tog = c(0.9, 0.85, 0.95, 0.1, 0.05)
  )
  pruned <- torp:::.team_rapm_prune_columns(rows, threshold = 100, unit = "tog_minutes",
                                            game_minutes = 80, comp = "AFLW")
  expect_equal(pruned[player_id == "A"]$rapm_col, "A")  # 2.7*80=216 >= 100
  expect_equal(pruned[player_id == "B"]$rapm_col, "replacement_MID")  # 0.1*80=8 < 100
})

test_that(".team_rapm_prune_columns warns when a threshold pools EVERY player in a bucket", {
  rows <- data.table::data.table(
    player_id = c("A", "B"), player_name = c("Alice", "Bob"),
    position_bucket = c("FWD", "FWD"), tog = c(0.1, 0.05)
  )
  expect_warning(
    torp:::.team_rapm_prune_columns(rows, threshold = 100, comp = "AFLM"),
    "pools EVERY player"
  )
})

test_that(".team_rapm_prune_columns pools NA position_bucket into replacement_UNKNOWN, not replacement_NA", {
  rows <- data.table::data.table(
    player_id = c("A", "B"), player_name = c("Alice", "Bob"),
    position_bucket = c(NA_character_, "MID"), tog = c(0.1, 0.05)
  )
  pruned <- torp:::.team_rapm_prune_columns(rows, threshold = 100, comp = "AFLM")
  expect_equal(pruned[player_id == "A"]$rapm_col, "replacement_UNKNOWN")
})

# -----------------------------------------------------------------------------
# Mock fixtures for build/fit -- both comps
# -----------------------------------------------------------------------------

.N_MOCK_MATCHES <- 24L

.mock_pgd <- function(seasons = 2024L, sparse_id = NULL) {
  set.seed(2026082501)
  do.call(rbind, lapply(seasons, function(s) {
    do.call(rbind, lapply(seq_len(.N_MOCK_MATCHES), function(i) {
      d <- tibble::tibble(
        match_id = paste0("m", s, "_", i), season = s,
        player_id = c("P1", "P2", "P3", "P4", "P5", "P6"),
        player_name = c("A", "B", "C", "D", "E", "F"),
        team_id = c("T1", "T1", "T1", "T2", "T2", "T2"),
        position_group = c("KEY_DEFENDER", "MIDFIELDER", "KEY_FORWARD",
                           "MEDIUM_DEFENDER", "MIDFIELDER", "MEDIUM_FORWARD"),
        time_on_ground_percentage = c(90, 5, 5, 88, 5, 5)
      )
      # sparse_id appears in only the FIRST match of each season -- gives the
      # design a player who falls below a low games/TOG threshold while
      # everyone else clears it, so pruning/replacement-pooling is exercised.
      if (!is.null(sparse_id) && i > 1L) d <- d[d$player_id != sparse_id, ]
      d
    }))
  }))
}

.mock_results <- function(seasons = 2024L) {
  set.seed(2026082502)
  do.call(rbind, lapply(seasons, function(s) {
    n <- .N_MOCK_MATCHES
    tibble::tibble(
      match_id = paste0("m", s, "_", seq_len(n)), season = s,
      home_team_id = "T1", away_team_id = "T2",
      home_score = sample(35:80, n, replace = TRUE),
      away_score = sample(35:80, n, replace = TRUE)
    )
  }))
}

.mock_ps_aflw <- function(seasons = 2024L) {
  set.seed(2026082503)
  do.call(rbind, lapply(seasons, function(s) {
    do.call(rbind, lapply(seq_len(.N_MOCK_MATCHES), function(i) {
      tibble::tibble(
        match_id = paste0("w", s, "_", i), season = s,
        player_id = c("W1", "W2", "W3", "W4", "W5", "W6"),
        player_name = c("A", "B", "C", "D", "E", "F"),
        team_id = c("T1", "T1", "T1", "T2", "T2", "T2"),
        team_status = c("home", "home", "home", "away", "away", "away"),
        position = c("FB", "C", "FF", "CHB", "WL", "CHF"),
        time_on_ground_percentage = c(90, 5, 5, 88, 5, 5)
      )
    }))
  }))
}

test_that("build_team_rapm_net (AFLM): signed, correctly-shaped matrix", {
  local_mocked_bindings(
    load_player_game_data = function(seasons, ...) .mock_pgd(seasons, sparse_id = "P3"),
    load_results = function(seasons, comp = "AFLM") .mock_results(seasons)
  )
  d <- suppressWarnings(build_team_rapm_net(2024L, comp = "AFLM", threshold = 2))
  expect_equal(d$n, .N_MOCK_MATCHES)
  expect_true("P1" %in% d$columns)
  expect_true(any(startsWith(d$columns, "replacement_")))
})

test_that("build_team_rapm_net (AFLW): signed, correctly-shaped matrix", {
  local_mocked_bindings(
    load_player_stats = function(seasons, comp = "AFLM") .mock_ps_aflw(seasons),
    load_results = function(seasons, comp = "AFLM") .mock_results(seasons) |>
      dplyr::mutate(match_id = paste0("w", season, "_", sub("^m\\d+_", "", match_id)))
  )
  d <- suppressWarnings(build_team_rapm_net(2024L, comp = "AFLW", threshold = 100, unit = "tog_minutes", game_minutes = 80))
  expect_equal(d$n, .N_MOCK_MATCHES)
  expect_true("W1" %in% d$columns)
})

test_that("fit_team_rapm_split + extract_team_rapm_ratings: sane output, replacement rows kept", {
  local_mocked_bindings(
    load_player_game_data = function(seasons, ...) .mock_pgd(seasons, sparse_id = "P3"),
    load_results = function(seasons, comp = "AFLM") .mock_results(seasons)
  )
  d <- suppressWarnings(build_team_rapm_split(2024L, comp = "AFLM", threshold = 2))
  fit <- suppressWarnings(fit_team_rapm_split(d, nfolds = 4))
  ratings <- extract_team_rapm_ratings(d, fit)

  expect_true(all(c("player_id", "rating_type", "rapm_offense", "rapm_defense", "rapm") %in% names(ratings)))
  expect_true(any(ratings$rating_type == "individual"))
  expect_true(any(ratings$rating_type == "replacement"))
  expect_true(all(is.finite(ratings$rapm_offense)))
  expect_equal(ratings$rapm, ratings$rapm_offense - ratings$rapm_defense)
})

# -----------------------------------------------------------------------------
# Co-appearance diagnostic
# -----------------------------------------------------------------------------

test_that(".team_rapm_coappearance computes a Jaccard-based diagnostic without erroring on a small fixture", {
  local_mocked_bindings(
    load_player_game_data = function(seasons, ...) .mock_pgd(seasons),
    load_results = function(seasons, comp = "AFLM") .mock_results(seasons)
  )
  rows <- torp:::.prepare_team_rapm_player_rows(2024L, comp = "AFLM")
  diag <- torp:::.team_rapm_coappearance(rows)
  expect_true(diag$n_pairs > 0)
  expect_true(diag$mean_jaccard >= 0 && diag$mean_jaccard <= 1)
})

# -----------------------------------------------------------------------------
# Point-in-time expanding-window leak safety -- the reason this rework exists
# -----------------------------------------------------------------------------

test_that("build_team_rapm_expanding: a season cutoff's rating NEVER includes a player who only appears in that season or later", {
  # Season 2023: only P1-P6. Season 2024: P1-P6 PLUS a brand-new player P7
  # (a mid-career debutant) who racks up huge, obviously-individual-column
  # TOG. If the cutoff for season 2024 is built from strictly-prior data
  # (season 2023 only), P7 cannot appear in season 2024's rating at all --
  # he doesn't exist yet as of that cutoff's training window. A leaking
  # implementation (accidentally using >= cutoff data) would include him.
  seasons_avail <- c(2021L, 2022L, 2023L, 2024L)

  mock_pgd_multi <- function(seasons, ...) {
    base <- do.call(rbind, lapply(seasons, function(s) .mock_pgd(s)))
    if (2024L %in% seasons) {
      extra <- tibble::tibble(
        match_id = paste0("m2024_", seq_len(.N_MOCK_MATCHES)), season = 2024L,
        player_id = "P7", player_name = "NewGuy", team_id = "T1",
        position_group = "MIDFIELDER", time_on_ground_percentage = 95
      )
      base <- rbind(base, extra)
    }
    base
  }
  mock_results_multi <- function(seasons, comp = "AFLM") {
    do.call(rbind, lapply(seasons, function(s) .mock_results(s)))
  }

  local_mocked_bindings(
    load_player_game_data = mock_pgd_multi,
    load_results = mock_results_multi
  )

  expanding <- suppressWarnings(suppressMessages(
    build_team_rapm_expanding(seasons_avail, comp = "AFLM", min_train_seasons = 2L,
                              design = "split", threshold = 2, nfolds = 3)
  ))

  expect_true("season" %in% names(expanding))
  cutoffs <- sort(unique(expanding$season))
  expect_equal(cutoffs, c(2023L, 2024L))  # 2021,2022 are the minimum training window, no cutoff for them

  # THE ACTUAL LEAK-SAFETY ASSERTION: P7 must not appear ANYWHERE in the
  # 2023 cutoff's ratings (he doesn't exist in seasons < 2023), even though
  # he's a real player in the full dataset.
  ratings_2023 <- expanding[season == 2023L]
  expect_false("P7" %in% ratings_2023$player_id)

  # He's free to appear in the 2024 cutoff's ratings IF he cleared the
  # threshold in seasons < 2024 (i.e. season 2023, where he doesn't exist
  # either) -- so he still shouldn't appear individually at the 2024 cutoff,
  # since 2024 is trained on seasons < 2024 = {2021,2022,2023}, none of which
  # contain him. This is the stronger, more subtle assertion: even the LATER
  # cutoff must not see him, because "strictly before" excludes his own
  # debut season too.
  ratings_2024 <- expanding[season == 2024L]
  expect_false("P7" %in% ratings_2024$player_id)
})

test_that("build_team_rapm_expanding errors cleanly when there isn't enough history for any cutoff", {
  local_mocked_bindings(
    load_results = function(seasons, comp = "AFLM") .mock_results(2024L)
  )
  expect_error(
    build_team_rapm_expanding(2024L, comp = "AFLM", min_train_seasons = 2L),
    "Not enough seasons"
  )
})

test_that(".team_rapm_season_cutoffs returns seasons strictly after the minimum training window", {
  local_mocked_bindings(
    load_results = function(seasons, comp = "AFLM") do.call(rbind, lapply(c(2021L,2022L,2023L,2024L,2025L), .mock_results))
  )
  cutoffs <- torp:::.team_rapm_season_cutoffs(TRUE, comp = "AFLM", min_train_seasons = 2L)
  expect_equal(cutoffs, c(2023L, 2024L, 2025L))
})
