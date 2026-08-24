# docs/plans/AFLW-MIGRATION-PLAN.md Phase 3 -- AFLW RAPM. No direct men's
# analog to mirror (torp's chain-native EPV/EPR doesn't fit a RAPM-shaped
# regression), so this is bespoke coverage per plan §6.5: the TOG-weighted
# design matrix construction, the column-pruning/replacement-level-pooling
# threshold logic, and the fit itself producing sane output.

test_that(".aflw_rapm_position_bucket buckets the 18-slot AFLW taxonomy into 4 groups", {
  pos <- c("FB", "BPL", "CHB", "C", "WR", "RK", "CHF", "FF", "INT", "EMERG", "UNKNOWN_CODE")
  bucket <- torp:::.aflw_rapm_position_bucket(pos)
  expect_equal(bucket, c("DEF", "DEF", "DEF", "MID", "MID", "MID", "FWD", "FWD", "INT", "INT", "INT"))
})

test_that(".aflw_rapm_prune_columns pools low-TOG players into a shared replacement column, keeps high-TOG players individual", {
  rows <- data.table::data.table(
    player_id = c("A", "A", "A", "B", "C"),
    player_name = c("Alice", "Alice", "Alice", "Bob", "Carol"),
    position_bucket = c("DEF", "DEF", "DEF", "MID", "DEF"),
    tog = c(0.9, 0.85, 0.95, 0.1, 0.05)  # A: high total TOG, B/C: low
  )

  pruned <- torp:::.aflw_rapm_prune_columns(rows, min_tog_minutes = 100, game_minutes = 80)

  a_row <- pruned[player_id == "A"]
  expect_equal(a_row$rapm_col, "A")  # 0.9+0.85+0.95 = 2.7 * 80 = 216 >= 100 -> own column
  expect_gte(a_row$total_tog_minutes, 100)

  b_row <- pruned[player_id == "B"]
  expect_equal(b_row$rapm_col, "replacement_MID")  # 0.1*80=8 < 100 -> pooled
  expect_lt(b_row$total_tog_minutes, 100)

  c_row <- pruned[player_id == "C"]
  expect_equal(c_row$rapm_col, "replacement_DEF")  # 0.05*80=4 < 100 -> pooled, different bucket than B
})

test_that(".aflw_rapm_prune_columns warns when a threshold pools EVERY player in a bucket", {
  rows <- data.table::data.table(
    player_id = c("A", "B"),
    player_name = c("Alice", "Bob"),
    position_bucket = c("FWD", "FWD"),
    tog = c(0.1, 0.05)  # both low -- entire FWD bucket pools to replacement
  )
  expect_warning(
    torp:::.aflw_rapm_prune_columns(rows, min_tog_minutes = 100, game_minutes = 80),
    "pools EVERY player"
  )
})

.N_MOCK_AFLW_RAPM_MATCHES <- 24L

.mock_aflw_rapm_stats <- function() {
  # 24 matches, 2 teams (T1/T2), 3 players per team-match. Needs to be
  # bigger than it looks: cv.glmnet's very wide lambda.min.ratio=1e-6 grid
  # (matched to daisychain's own tuning, see fit_aflw_rapm_net()) can't
  # reliably converge/interpolate lambda.1se on truly tiny toy data --
  # 4 matches tripped a sparse-matrix indexing error at n==p, 8 tripped an
  # "at least two non-NA values to interpolate" error. 24 gives real
  # headroom for both the net (n=matches) and split (n=2*matches) designs.
  # Players P1/P4 get lots of TOG (kept individual); P2/P3/P5/P6 get little
  # TOG (pooled).
  set.seed(2026082401)
  make_match <- function(mid, t1_score, t2_score) {
    tibble::tibble(
      match_id = mid,
      player_id = c("P1", "P2", "P3", "P4", "P5", "P6"),
      player_name = c("A", "B", "C", "D", "E", "F"),
      team_id = c("T1", "T1", "T1", "T2", "T2", "T2"),
      team_status = c("home", "home", "home", "away", "away", "away"),
      position = c("FB", "C", "FF", "CHB", "WL", "CHF"),
      time_on_ground_percentage = c(90, 5, 5, 88, 5, 5)
    )
  }
  dplyr::bind_rows(lapply(seq_len(.N_MOCK_AFLW_RAPM_MATCHES), function(i) {
    make_match(paste0("m", i), NA, NA)
  }))
}

.mock_aflw_rapm_results <- function() {
  set.seed(2026082402)
  n <- .N_MOCK_AFLW_RAPM_MATCHES
  tibble::tibble(
    match_id = paste0("m", seq_len(n)),
    home_team_id = "T1", away_team_id = "T2",
    home_score = sample(35:80, n, replace = TRUE),
    away_score = sample(35:80, n, replace = TRUE)
  )
}

test_that(".prepare_aflw_rapm_player_rows aborts on a team_status value outside {home,away}", {
  local_mocked_bindings(
    load_player_stats = function(seasons, comp = "AFLM") {
      d <- .mock_aflw_rapm_stats()
      d$team_status[1] <- "unknown"
      d
    },
    load_results = function(seasons, comp = "AFLM") .mock_aflw_rapm_results()
  )
  expect_error(torp:::.prepare_aflw_rapm_player_rows(TRUE), "team_status")
})

test_that("build_aflw_rapm_net produces a signed, correctly-shaped matrix and n/p diagnostic", {
  local_mocked_bindings(
    load_player_stats = function(seasons, comp = "AFLM") .mock_aflw_rapm_stats(),
    load_results      = function(seasons, comp = "AFLM") .mock_aflw_rapm_results()
  )

  d <- suppressWarnings(build_aflw_rapm_net(TRUE, min_tog_minutes = 100))
  res <- .mock_aflw_rapm_results()

  expect_equal(d$n, .N_MOCK_AFLW_RAPM_MATCHES)
  expect_equal(nrow(d$X), .N_MOCK_AFLW_RAPM_MATCHES)
  expect_equal(ncol(d$X), length(d$columns))
  expect_equal(d$n_over_p, d$n / d$p)
  expected_margin <- stats::setNames(res$home_score - res$away_score, res$match_id)
  expect_equal(d$y, unname(expected_margin[d$match_ids]))  # home_margin per match, order-agnostic

  # P1 (kept, home) contributes a POSITIVE value; P4 (kept, away) NEGATIVE
  expect_true("P1" %in% d$columns)
  expect_true("P4" %in% d$columns)
  p1_col <- which(d$columns == "P1")
  p4_col <- which(d$columns == "P4")
  expect_true(all(as.matrix(d$X)[, p1_col] > 0))
  expect_true(all(as.matrix(d$X)[, p4_col] < 0))

  # low-TOG players pooled -- not their own columns
  expect_false("P2" %in% d$columns)
  expect_false("P5" %in% d$columns)
  expect_true(any(startsWith(d$columns, "replacement_")))
})

test_that("build_aflw_rapm_split doubles columns into own/opp blocks, one row per team-side", {
  local_mocked_bindings(
    load_player_stats = function(seasons, comp = "AFLM") .mock_aflw_rapm_stats(),
    load_results      = function(seasons, comp = "AFLM") .mock_aflw_rapm_results()
  )

  d <- suppressWarnings(build_aflw_rapm_split(TRUE, min_tog_minutes = 100))
  res <- .mock_aflw_rapm_results()

  expect_equal(d$n, 2L * .N_MOCK_AFLW_RAPM_MATCHES)  # 2 sides x N matches
  expect_equal(ncol(d$X), 2 * d$p)
  expect_equal(d$n_over_p, d$n / d$p)
  # y = points scored by that row's own team -- every match contributes both
  # teams' actual scores somewhere in y
  expect_setequal(d$y, c(res$home_score, res$away_score))
})

test_that("fit_aflw_rapm_net + extract via the split design: sane output, no NA/Inf, defense sign convention holds", {
  local_mocked_bindings(
    load_player_stats = function(seasons, comp = "AFLM") .mock_aflw_rapm_stats(),
    load_results      = function(seasons, comp = "AFLM") .mock_aflw_rapm_results()
  )

  d <- suppressWarnings(build_aflw_rapm_split(TRUE, min_tog_minutes = 100))
  fit <- suppressWarnings(fit_aflw_rapm_split(d, nfolds = 4))
  ratings <- extract_aflw_rapm_ratings(d, fit)

  expect_true(all(c("player_id", "rating_type", "rapm_offense", "rapm_defense", "rapm") %in% names(ratings)))
  # Replacement-level rows are kept (not dropped), so a caller has SOME
  # fallback rating available for a pooled/low-minutes player.
  expect_true(any(startsWith(ratings$player_id, "replacement_")))
  expect_true(all(ratings[startsWith(player_id, "replacement_")]$rating_type == "replacement"))
  expect_true(all(ratings[!startsWith(player_id, "replacement_")]$rating_type == "individual"))
  expect_true(all(is.finite(ratings$rapm_offense)))
  expect_true(all(is.finite(ratings$rapm_defense)))
  expect_equal(ratings$rapm, ratings$rapm_offense - ratings$rapm_defense)
  expect_true(is.finite(fit$cv_r2))
  expect_true(is.finite(fit$n_over_p))
})

test_that("extract_aflw_rapm_ratings: rating_type distinguishes individual from replacement rows", {
  local_mocked_bindings(
    load_player_stats = function(seasons, comp = "AFLM") .mock_aflw_rapm_stats(),
    load_results      = function(seasons, comp = "AFLM") .mock_aflw_rapm_results()
  )

  d <- suppressWarnings(build_aflw_rapm_split(TRUE, min_tog_minutes = 100))
  fit <- suppressWarnings(fit_aflw_rapm_split(d, nfolds = 4))
  ratings <- extract_aflw_rapm_ratings(d, fit)

  expect_setequal(unique(ratings$rating_type), c("individual", "replacement"))
  expect_equal(nrow(ratings), length(d$columns))
})

test_that("fit_aflw_rapm_net reports both cv_r2 and in_sample_r2, and n/p matches build_aflw_rapm_net's own diagnostic", {
  local_mocked_bindings(
    load_player_stats = function(seasons, comp = "AFLM") .mock_aflw_rapm_stats(),
    load_results      = function(seasons, comp = "AFLM") .mock_aflw_rapm_results()
  )

  d <- suppressWarnings(build_aflw_rapm_net(TRUE, min_tog_minutes = 100))
  fit <- suppressWarnings(fit_aflw_rapm_net(d, nfolds = 4))

  expect_true(is.finite(fit$cv_r2))
  expect_true(is.finite(fit$in_sample_r2))
  expect_equal(fit$n, d$n)
  expect_equal(fit$p, d$p)
  expect_equal(fit$n_over_p, d$n_over_p)
})
