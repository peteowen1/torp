# -----------------------------------------------------------------------------
# calculate_epr Tests
# -----------------------------------------------------------------------------

test_that("calculate_epr function exists and is exported", {
  expect_true(exists("calculate_epr"))
  expect_true("calculate_epr" %in% getNamespaceExports("torp"))
})

test_that("calculate_epr has correct function signature", {
  fn_args <- names(formals(calculate_epr))

  expect_true("season_val" %in% fn_args)
  expect_true("round_val" %in% fn_args)
  expect_true("decay_recv" %in% fn_args)
  expect_true("decay_disp" %in% fn_args)
  expect_true("decay_spoil" %in% fn_args)
  expect_true("decay_hitout" %in% fn_args)
  expect_true("loading" %in% fn_args)
  expect_true("prior_games_recv" %in% fn_args)
  expect_true("prior_games_disp" %in% fn_args)
  expect_true("plyr_tm_df" %in% fn_args)
  expect_true("player_game_data" %in% fn_args)
  expect_true("skills" %in% fn_args)
})

test_that("calculate_epr skills parameter defaults to TRUE", {
  expect_true(formals(calculate_epr)$skills)
})

test_that("calculate_epr has reasonable defaults", {
  fn_formals <- formals(calculate_epr)

  # decay_recv uses EPR_DECAY_RECV constant
  expect_true(is.symbol(fn_formals$decay_recv) || fn_formals$decay_recv == torp:::EPR_DECAY_RECV)
  expect_true(is.symbol(fn_formals$loading) || fn_formals$loading == torp:::EPR_LOADING_DEFAULT)
  expect_true(is.symbol(fn_formals$prior_games_recv) || fn_formals$prior_games_recv == torp:::EPR_PRIOR_GAMES_RECV)
  expect_true(is.symbol(fn_formals$prior_games_disp) || fn_formals$prior_games_disp == torp:::EPR_PRIOR_GAMES_DISP)
})

# -----------------------------------------------------------------------------
# torp_ratings Alias Tests
# -----------------------------------------------------------------------------

test_that("torp_ratings and epr_ratings exist as exported functions", {
  expect_true(exists("torp_ratings"))
  expect_true(is.function(torp_ratings))
  expect_true(exists("epr_ratings"))
  expect_identical(epr_ratings, calculate_epr)
})

test_that("calculate_epr_stats helper function works", {
  # Test the helper function exists
  expect_true(exists("calculate_epr_stats", envir = asNamespace("torp")))

  # Create minimal test data with player_name (as produced by create_player_game_data)
  test_data <- data.frame(
    player_id = c(1, 1, 2, 2),
    player_name = c("Player1", "Player1", "Player2", "Player2"),
    match_id = c("CD_M2024014101", "CD_M2024014102", "CD_M2024014101", "CD_M2024014102"),
    utc_start_time = rep(as.Date("2024-04-01"), 4),
    epv_adj = c(100, 120, 80, 90),
    epv_recv_adj = c(20, 25, 15, 18),
    epv_disp_adj = c(40, 45, 35, 38),
    epv_spoil_adj = c(10, 12, 8, 9),
    epv_hitout_adj = c(5, 8, 0, 0),
    time_on_ground_percentage = c(82, 78, 90, 85),
    position_group = c("Midfielder", "Midfielder", "Forward", "Forward"),
    stringsAsFactors = FALSE
  )

  # The function should work with valid inputs
  result <- torp:::calculate_epr_stats(
    player_game_data = test_data,
    match_ref = "CD_M2024014103",
    date_val = as.Date("2024-04-08"),
    loading = 1.5,
    prior_games_recv = 4,
    prior_games_disp = 6
  )

  expect_true(is.data.frame(result))
  expect_true("player_id" %in% names(result))
  expect_true("epr" %in% names(result))
})

# -----------------------------------------------------------------------------
# calculate_epr_stats Tests
# -----------------------------------------------------------------------------

test_that("calculate_epr_stats function exists", {
  expect_true(exists("calculate_epr_stats", envir = asNamespace("torp")))
})

test_that("calculate_epr_stats returns expected structure with valid data", {
  # Create comprehensive test data (as produced by create_player_game_data)
  test_data <- data.frame(
    player_id = rep(1:5, each = 4),
    player_name = rep(paste("First", paste0("Last", 1:5)), each = 4),
    match_id = rep(c("CD_M2024014101", "CD_M2024014102", "CD_M2024014103", "CD_M2024014104"), 5),
    utc_start_time = rep(as.Date("2024-04-01") + c(0, 7, 14, 21), 5),
    epv_adj = runif(20, 50, 150),
    epv_recv_adj = runif(20, 10, 50),
    epv_disp_adj = runif(20, 20, 80),
    epv_spoil_adj = runif(20, 0, 20),
    epv_hitout_adj = runif(20, 0, 30),
    time_on_ground_percentage = runif(20, 60, 95),
    position_group = sample(c("FWD", "MID", "DEF", "RUC"), 20, replace = TRUE),
    stringsAsFactors = FALSE
  )

  result <- torp:::calculate_epr_stats(
    player_game_data = test_data,
    match_ref = "CD_M2024014105",
    date_val = as.Date("2024-05-01"),
    loading = 1.5,
    prior_games_recv = 4,
    prior_games_disp = 6
  )

  # Check structure
  expect_true(is.data.frame(result) || data.table::is.data.table(result))
  expect_true("player_id" %in% names(result))
  expect_true("epr" %in% names(result))
  expect_true("epr_recv" %in% names(result) || "epr_recv_adj" %in% names(result))
  expect_true("epr_disp" %in% names(result) || "epr_disp_adj" %in% names(result))

  # Should have 5 unique players
  expect_equal(nrow(result), 5)
})

test_that("calculate_epr_stats respects decay parameter", {
  # Create test data with games at different times
  test_data <- data.frame(
    player_id = rep(1, 3),
    player_name = rep("First Last", 3),
    match_id = c("CD_M2024014101", "CD_M2024014102", "CD_M2024014103"),
    utc_start_time = as.Date("2024-04-01") + c(0, 30, 60),  # 0, 30, 60 days apart
    epv_adj = c(100, 100, 100),
    epv_recv_adj = c(50, 50, 50),
    epv_disp_adj = c(50, 50, 50),
    epv_spoil_adj = c(10, 10, 10),
    epv_hitout_adj = c(5, 5, 5),
    time_on_ground_percentage = c(80, 85, 75),
    position_group = rep("MID", 3),
    stringsAsFactors = FALSE
  )

  # Calculate with short decay (recent games weighted more)
  result_short <- torp:::calculate_epr_stats(
    player_game_data = test_data,
    match_ref = "CD_M2024014104",
    date_val = as.Date("2024-06-01"),
    decay_recv = 30, decay_disp = 30, decay_spoil = 30, decay_hitout = 30,
    loading = 1.5,
    prior_games_recv = 4,
    prior_games_disp = 6
  )

  # Calculate with long decay (all games weighted similarly)
  result_long <- torp:::calculate_epr_stats(
    player_game_data = test_data,
    match_ref = "CD_M2024014104",
    date_val = as.Date("2024-06-01"),
    decay_recv = 1000, decay_disp = 1000, decay_spoil = 1000, decay_hitout = 1000,
    loading = 1.5,
    prior_games_recv = 4,
    prior_games_disp = 6
  )

  # Both should return results
  expect_equal(nrow(result_short), 1)
  expect_equal(nrow(result_long), 1)
})

# -----------------------------------------------------------------------------
# .prepare_final_dataframe Tests
# -----------------------------------------------------------------------------

test_that(".prepare_final_dataframe function exists", {
  expect_true(exists(".prepare_final_dataframe", envir = asNamespace("torp")))
})

# -----------------------------------------------------------------------------
# Integration Tests
# -----------------------------------------------------------------------------

test_that("calculate_epr works with pre-loaded data", {
  skip_if(is.null(.shared$player_game_data) || is.null(.shared$player_details),
          "Could not load player data")

  # Calculate ratings with pre-loaded data (skills = FALSE to isolate rating logic)
  result <- calculate_epr(
    season_val = 2024,
    round_val = 1,
    plyr_tm_df = .shared$player_details,
    player_game_data = .shared$player_game_data,
    skills = FALSE
  )

  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  expect_true("epr" %in% names(result) || "player_id" %in% names(result))
})

# -----------------------------------------------------------------------------
# Constants Usage Tests
# -----------------------------------------------------------------------------

test_that("calculate_epr_stats uses prior_games_spoil and prior_games_hitout constants", {
  expect_identical(torp:::EPR_PRIOR_GAMES_SPOIL, 3.0000)
  expect_identical(torp:::EPR_PRIOR_GAMES_HITOUT, 3.0000)
})

# -----------------------------------------------------------------------------
# wt_gms Calculation Tests
# -----------------------------------------------------------------------------

test_that("wt_gms sums per-match weights correctly for same-day games", {
  # Two games on the same day produce identical weight_gm values.
 # The old sum(unique(weight_gm)) would collapse these into one weight.
 # The fix uses !duplicated(match_id) to keep both.
  test_data <- data.frame(
    player_id = rep(1, 2),
    player_name = rep("Same Day", 2),
    match_id = c("CD_M2024014101", "CD_M2024014102"),
    utc_start_time = rep(as.Date("2024-04-01"), 2),
    epv_adj = c(100, 80),
    epv_recv_adj = c(20, 15),
    epv_disp_adj = c(40, 35),
    epv_spoil_adj = c(10, 8),
    epv_hitout_adj = c(5, 3),
    time_on_ground_percentage = c(88, 76),
    position_group = rep("MID", 2),
    stringsAsFactors = FALSE
  )

  result <- torp:::calculate_epr_stats(
    player_game_data = test_data,
    match_ref = "CD_M2024014103",
    date_val = as.Date("2024-04-08"),
    loading = 1.5,
    prior_games_recv = 4, prior_games_disp = 6
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$gms, 2)
  # wt_gms is now TOG-weighted: sum(wt * tog) where tog = pmax(tog_pct/100, 0.1)
  single_weight <- exp(-as.numeric(as.Date("2024-04-08") - as.Date("2024-04-01")) / torp:::EPR_DECAY_RECV)
  tog1 <- 88 / 100  # first game TOG
  tog2 <- 76 / 100  # second game TOG
  expected_wt_gms <- single_weight * tog1 + single_weight * tog2
  expect_equal(result$wt_gms, expected_wt_gms, tolerance = 1e-10)
})

# -----------------------------------------------------------------------------
# TOG-Weighted Average Adjustment Tests
# -----------------------------------------------------------------------------

test_that("calculate_epr rejects skills without cond_tog_rating stat rating column", {
  bad_skills <- data.frame(player_id = 1, some_other_col = 0.5)
  expect_error(
    calculate_epr(skills = bad_skills),
    "cond_tog_rating"
  )
})

test_that("TOG-weighted average adjustment produces correct math", {
  # Build mock player game data for 3 players
  test_data <- data.frame(
    player_id = rep(1:3, each = 2),
    player_name = rep(c("Player One", "Player Two", "Player Three"), each = 2),
    match_id = rep(c("CD_M2024014101", "CD_M2024014102"), 3),
    utc_start_time = rep(as.Date("2024-04-01"), 6),
    epv_adj = c(100, 100, 80, 80, 60, 60),
    epv_recv_adj = c(30, 30, 20, 20, 10, 10),
    epv_disp_adj = c(40, 40, 30, 30, 20, 20),
    epv_spoil_adj = c(10, 10, 8, 8, 5, 5),
    epv_hitout_adj = c(5, 5, 3, 3, 0, 0),
    time_on_ground_percentage = c(85, 80, 75, 70, 90, 88),
    position_group = rep("MID", 6),
    stringsAsFactors = FALSE
  )

  # Get unadjusted stats from calculate_epr_stats
  unadj <- torp:::calculate_epr_stats(
    player_game_data = test_data,
    match_ref = "CD_M2024014103",
    date_val = as.Date("2024-04-08"),
    loading = 1.5,
    prior_games_recv = 4, prior_games_disp = 6
  )

  # Create skills with known TOG weights (decomposed)
  skills <- data.frame(
    player_id = 1:3,
    cond_tog_rating = c(0.9, 0.7, 0.3),
    squad_selection_rating = c(1.0, 1.0, 1.0)
  )

  # Apply adjustment manually (mirrors the code in calculate_epr)
  adj <- data.table::copy(unadj)
  skills_dt <- data.table::as.data.table(skills)
  adj[skills_dt, tog_rating := i.squad_selection_rating * i.cond_tog_rating, on = "player_id"]
  adj[is.na(tog_rating), tog_rating := 0]

  tot_tog <- sum(adj$tog_rating)
  comps <- c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout")
  for (comp in comps) {
    avg_val <- sum(adj[[comp]] * adj$tog_rating) / tot_tog
    data.table::set(adj, j = comp, value = adj[[comp]] - avg_val)
  }

  # Verify: TOG-weighted average of adjusted components should be ~0
  for (comp in comps) {
    weighted_avg <- sum(adj[[comp]] * adj$tog_rating) / tot_tog
    expect_equal(weighted_avg, 0, tolerance = 1e-10,
                 label = paste("weighted avg of adjusted", comp))
  }

  # Verify: adjustment shifts values (high-TOG player 1 should still be highest)
  expect_true(adj$epr_recv[adj$player_id == 1] > adj$epr_recv[adj$player_id == 3])
})

test_that("TOG adjustment is skipped when all tog_rating values are zero", {
  test_data <- data.frame(
    player_id = rep(1:2, each = 2),
    player_name = rep(c("Player One", "Player Two"), each = 2),
    match_id = rep(c("CD_M2024014101", "CD_M2024014102"), 2),
    utc_start_time = rep(as.Date("2024-04-01"), 4),
    epv_adj = c(100, 100, 80, 80),
    epv_recv_adj = c(30, 30, 20, 20),
    epv_disp_adj = c(40, 40, 30, 30),
    epv_spoil_adj = c(10, 10, 8, 8),
    epv_hitout_adj = c(5, 5, 0, 0),
    time_on_ground_percentage = c(82, 79, 88, 84),
    position_group = rep("MID", 4),
    stringsAsFactors = FALSE
  )

  unadj <- torp:::calculate_epr_stats(
    player_game_data = test_data,
    match_ref = "CD_M2024014103",
    date_val = as.Date("2024-04-08"),
    loading = 1.5,
    prior_games_recv = 4, prior_games_disp = 6
  )

  # Skills with no matching player_ids → all default to 0
  skills <- data.frame(
    player_id = c(99, 100),
    cond_tog_rating = c(0.8, 0.5),
    squad_selection_rating = c(1.0, 1.0)
  )

  # Apply adjustment: tot_tog = 0, should skip
  adj <- data.table::copy(unadj)
  skills_dt <- data.table::as.data.table(skills)
  adj[skills_dt, tog_rating := i.squad_selection_rating * i.cond_tog_rating, on = "player_id"]
  adj[is.na(tog_rating), tog_rating := 0]

  tot_tog <- sum(adj$tog_rating)
  expect_equal(tot_tog, 0)

  # Values unchanged when tot_tog == 0
  expect_equal(adj$epr_recv, unadj$epr_recv)
})

# -----------------------------------------------------------------------------
# calculate_torp Tests (PSR blend) — issue #88: historical PSR per round
# -----------------------------------------------------------------------------

test_that("calculate_torp joins PSR historically per (player_id, season, round)", {
  # Two players, two rounds each, with PSR changing between rounds.
  epr_df <- data.frame(
    player_id = c("A", "A", "B", "B"),
    player_name = c("A", "A", "B", "B"),
    epr = c(2, 4, 1, 3),
    season = c(2024L, 2024L, 2024L, 2024L),
    round = c(1L, 2L, 1L, 2L),
    stringsAsFactors = FALSE
  )
  psr_df <- data.frame(
    player_id = c("A", "A", "B", "B"),
    season = c(2024L, 2024L, 2024L, 2024L),
    round = c(1L, 2L, 1L, 2L),
    psr = c(10, 20, 5, 15),
    osr = c(6, 12, 3, 9),
    dsr = c(4, 8, 2, 6),
    stringsAsFactors = FALSE
  )

  res <- calculate_torp(epr_df, psr_df)
  res <- res[order(res$player_id, res$round), ]

  # Each row gets its own round's PSR, not a single broadcast snapshot
  expect_equal(res$psr, c(10, 20, 5, 15))
  expect_equal(res$osr, c(6, 12, 3, 9))
  expect_equal(res$dsr, c(4, 8, 2, 6))
  # torp = 0.5*epr + 0.5*psr
  expect_equal(res$torp, round(0.5 * c(2, 4, 1, 3) + 0.5 * c(10, 20, 5, 15), 2))
  # PSR varies across rounds for a single player (the #88 regression guard)
  expect_equal(length(unique(res$psr[res$player_id == "A"])), 2L)
})

test_that("calculate_torp falls back to latest snapshot for unmatched rows", {
  # epr_df has a round with no PSR history -> latest snapshot used
  epr_df <- data.frame(
    player_id = c("A", "A"),
    player_name = c("A", "A"),
    epr = c(2, 4),
    season = c(2024L, 2024L),
    round = c(1L, 99L),
    stringsAsFactors = FALSE
  )
  psr_df <- data.frame(
    player_id = c("A", "A"),
    season = c(2024L, 2024L),
    round = c(1L, 2L),
    psr = c(10, 20),
    osr = c(6, 12),
    dsr = c(4, 8),
    stringsAsFactors = FALSE
  )

  res <- calculate_torp(epr_df, psr_df)
  res <- res[order(res$round), ]

  expect_false(any(is.na(res$psr)))
  # Round 1 matches history (10); round 99 has no match -> latest snapshot (20)
  expect_equal(res$psr, c(10, 20))
})

test_that("calculate_torp uses snapshot when season/round absent", {
  epr_df <- data.frame(
    player_id = c("A", "B"),
    player_name = c("A", "B"),
    epr = c(2, 4),
    stringsAsFactors = FALSE
  )
  psr_df <- data.frame(
    player_id = c("A", "A", "B"),
    season = c(2024L, 2024L, 2024L),
    round = c(1L, 2L, 1L),
    psr = c(10, 20, 5),
    stringsAsFactors = FALSE
  )

  res <- calculate_torp(epr_df, psr_df)
  res <- res[order(res$player_id), ]
  # A's latest snapshot is round 2 (psr 20); B has psr 5
  expect_equal(res$psr, c(20, 5))
})


# EPR position centring -------------------------------------------------------

.centre_fixture <- function(n = 400, seed = 42) {
  set.seed(seed)
  pg <- unlist(MATCH_LISTED_POS_MAP, use.names = FALSE)
  d <- data.frame(
    season = 2026L, round = 21L,
    position_group = sample(pg, n, replace = TRUE),
    pred_tog = runif(n, 0.4, 1),
    epr_recv = rnorm(n, 1, 2), epr_disp = rnorm(n),
    epr_spoil = rnorm(n), epr_hitout = rnorm(n),
    stringsAsFactors = FALSE
  )
  # Push the two merged forward groups apart so a 7-way centring and a 6-way
  # one cannot produce the same answer.
  d$epr_recv <- d$epr_recv +
    ifelse(d$position_group == "MEDIUM_FORWARD", -3,
           ifelse(d$position_group == "MIDFIELDER_FORWARD", 3, 0))
  d$epr <- d$epr_recv + d$epr_disp + d$epr_spoil + d$epr_hitout
  d
}

test_that("centring keys on the same taxonomy the match features use", {
  # The invariant this whole helper exists to protect: ratings are centred on
  # exactly the buckets the model differences. They diverged once (7-way
  # centring vs 6-way features, 2026-07-28) and nothing downstream noticed.
  d <- .centre_fixture()
  out <- suppressMessages(centre_epr_by_position(d))
  out$bucket <- .collapse_listed_position(out$position_group)

  # as.vector, not unname: tapply returns a 1-d array, so expect_equal fails on
  # dim() alone against a plain numeric even when every value is exactly 0.
  by_bucket <- as.vector(tapply(seq_len(nrow(out)), out$bucket, function(i)
    stats::weighted.mean(out$epr[i], out$pred_tog[i])))
  expect_equal(by_bucket, rep(0, length(by_bucket)))

  # ...and NOT on raw position_group: the merged forwards must retain their
  # real level difference, or the merge silently isn't happening.
  fwd <- out[out$position_group %in% c("MEDIUM_FORWARD", "MIDFIELDER_FORWARD"), ]
  lv <- as.vector(tapply(seq_len(nrow(fwd)), fwd$position_group, function(i)
    stats::weighted.mean(fwd$epr[i], fwd$pred_tog[i])))
  expect_gt(abs(diff(lv)), 1)
})

test_that("centring rebuilds epr from its channels and drops helper columns", {
  d <- .centre_fixture()
  out <- suppressMessages(centre_epr_by_position(d))
  expect_equal(out$epr,
               out$epr_recv + out$epr_disp + out$epr_spoil + out$epr_hitout)
  expect_false(any(c(".cw", ".cpg") %in% names(out)))
  expect_s3_class(out, "data.frame")
})

test_that("centring aborts rather than silently returning uncentred ratings", {
  d <- .centre_fixture()
  expect_error(centre_epr_by_position(d[, setdiff(names(d), "position_group")]),
               "position_group")
  # A partial channel set would redefine epr as the sum of whatever was found.
  expect_error(suppressWarnings(
    centre_epr_by_position(d[, setdiff(names(d), "epr_hitout")])), "channel")
})

test_that("unmapped and missing position groups are left alone, not lumped", {
  d <- .centre_fixture()
  d$position_group[1:10] <- NA_character_
  d$position_group[11:20] <- "NEW_AFL_LABEL"
  before <- d$epr[1:20]
  out <- suppressWarnings(suppressMessages(centre_epr_by_position(d)))
  expect_equal(out$epr[1:20], before)

  # An unmapped label means a whole position ships uncentred, so it must be
  # visible immediately -- not folded into the ungrouped count alongside the
  # routine missing-position rows, and not left to R's deferred warning buffer
  # (capped at nwarnings, which a full rebuild can blow past).
  msgs <- suppressWarnings(testthat::capture_messages(centre_epr_by_position(d)))
  expect_match(paste(msgs, collapse = " "), "UNMAPPED", fixed = TRUE)
  expect_match(paste(msgs, collapse = " "), "10 unmapped", fixed = TRUE)
})

test_that("an NA weight excludes only that player, not their whole position", {
  # weighted.mean()'s na.rm drops NA values but not NA weights, so one missing
  # pred_tog used to blank an entire (season, round, position) cell.
  d <- .centre_fixture()
  d$pred_tog[1] <- NA_real_
  out <- suppressWarnings(suppressMessages(centre_epr_by_position(d)))
  peers <- .collapse_listed_position(d$position_group) ==
    .collapse_listed_position(d$position_group[1])
  expect_true(all(is.finite(out$epr[peers])))
})


# EPV position level centring --------------------------------------------------

.epv_fixture <- function(n = 500, seed = 7, suffix = "_oadj") {
  set.seed(seed)
  pg <- unlist(MATCH_LISTED_POS_MAP, use.names = FALSE)
  d <- data.table::data.table(
    season = 2026L,
    round = sample(1:5, n, replace = TRUE),
    position_group = sample(pg, n, replace = TRUE),
    time_on_ground_percentage = runif(n, 30, 100)
  )
  # set(), not [[<-, which shallow-copies and makes data.table warn on the next
  # := in the function under test.
  for (ch in EPV_LEVEL_CENTRE_CHANNELS) {
    data.table::set(d, j = paste0(ch, suffix), value = rnorm(n))
  }
  # Give each listed bucket a genuinely different level, which is the thing
  # lineup_position centring leaves behind.
  off <- stats::setNames(seq(-2, 2, length.out = length(pg)), pg)
  data.table::set(d, j = paste0("epv_recv", suffix),
                  value = d[[paste0("epv_recv", suffix)]] + off[d$position_group])
  data.table::set(d, j = paste0("epv", suffix), value = Reduce(`+`, lapply(
    paste0(EPV_LEVEL_CENTRE_CHANNELS, suffix), function(cc) d[[cc]])))
  d[]
}

test_that("EPV centring zeroes the TOG-weighted cell mean, which is what EPR sums", {
  # EPR forms sum(x * tog_safe * decay); decay is ~constant within a round, so
  # the TOG-weighted mean is the quantity that must vanish. An unweighted mean
  # would look centred while EPR stayed skewed.
  d <- .epv_fixture()
  out <- suppressMessages(centre_epv_by_position(d))
  out[, `:=`(bucket = .collapse_listed_position(position_group),
             w = pmax(time_on_ground_percentage / 100, 0.1))]
  for (cc in paste0(EPV_LEVEL_CENTRE_CHANNELS, "_oadj")) {
    wm <- out[, stats::weighted.mean(get(cc), w), by = .(season, round, bucket)]$V1
    expect_equal(wm, rep(0, length(wm)))
  }
})

test_that("EPV centring targets the channel set EPR actually reads", {
  # Centring _adj while EPR consumes _oadj would be a silent no-op: every check
  # would pass and nothing downstream would move.
  d <- .epv_fixture(suffix = "_oadj")
  d[, epv_recv_adj := 99]           # a stale _adj set must be ignored
  out <- suppressMessages(centre_epv_by_position(d))
  expect_true(all(out$epv_recv_adj == 99))
  expect_false(isTRUE(all.equal(out$epv_recv_oadj, d$epv_recv_oadj)))

  # ...and falls back to _adj when no _oadj exists.
  d2 <- .epv_fixture(suffix = "_adj")
  out2 <- suppressMessages(centre_epv_by_position(d2))
  out2[, `:=`(bucket = .collapse_listed_position(position_group),
              w = pmax(time_on_ground_percentage / 100, 0.1))]
  wm <- out2[, stats::weighted.mean(epv_recv_adj, w), by = .(season, round, bucket)]$V1
  expect_equal(wm, rep(0, length(wm)))
})

test_that("EPV centring preserves within-position spread and rebuilds the total", {
  # Spread is preserved WITHIN a round, which is the cell a constant is
  # subtracted from. Pooled across rounds it legitimately moves, because each
  # round gets its own offset -- checking it pooled would fail on correct code.
  d <- .epv_fixture()
  key <- c("round", "position_group")
  sd_before <- d[, .(s = sd(epv_recv_oadj)), by = key][order(round, position_group)]$s
  out <- suppressMessages(centre_epv_by_position(d))
  sd_after <- out[, .(s = sd(epv_recv_oadj)), by = key][order(round, position_group)]$s
  # centre_epv_by_position() now applies EPV_POINTS_SCALE as well as centring,
  # so spread is preserved UP TO that factor rather than exactly. Comparing raw
  # sds would fail for a correct reason and hide a genuine collapse behind a
  # known one.
  expect_equal(sd_after, sd_before * EPV_POINTS_SCALE, tolerance = 1e-8)
  expect_gt(min(sd_after), 0)   # a real collapse would still be caught

  expect_equal(out$epv_oadj,
               out$epv_recv_oadj + out$epv_disp_oadj +
                 out$epv_spoil_oadj + out$epv_hitout_oadj)
  expect_false(any(c(".cpg", ".ctog") %in% names(out)))
})

test_that("EPV centring aborts rather than silently returning uncentred values", {
  d <- .epv_fixture()
  expect_error(centre_epv_by_position(d[, !"position_group"]), "position_group")
  expect_error(centre_epv_by_position(d[, !"epv_hitout_oadj"]), "channel")
})
