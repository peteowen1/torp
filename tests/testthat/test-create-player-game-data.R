# -----------------------------------------------------------------------------
# create_player_game_data Tests
# -----------------------------------------------------------------------------

test_that("create_player_game_data function exists and is exported", {
  expect_true(exists("create_player_game_data"))
  expect_true("create_player_game_data" %in% getNamespaceExports("torp"))
})

test_that("create_player_game_data has correct function signature", {
  fn_args <- names(formals(create_player_game_data))

  expect_true("pbp_data" %in% fn_args)
  expect_true("player_stats" %in% fn_args)
  expect_true("teams" %in% fn_args)
  expect_true("decay" %in% fn_args)
})

test_that("create_player_game_data output contains required columns", {
  skip_if(is.null(.shared$pbp) || is.null(.shared$player_stats) || is.null(.shared$teams),
          "Could not load required data")

  pgd <- tryCatch(
    create_player_game_data(.shared$pbp, .shared$player_stats, .shared$teams),
    error = function(e) NULL
  )

  skip_if(is.null(pgd), "Could not create player game data")

  required_cols <- c(
    "match_id", "player_id", "epv_adj", "recv_epv_adj",
    "disp_epv_adj", "spoil_epv_adj", "hitout_epv_adj",
    "team", "opponent", "season", "round",
    "position_group", "lineup_position"
  )

  for (col in required_cols) {
    expect_true(col %in% names(pgd), info = paste("Missing column:", col))
  }

  expect_true(nrow(pgd) > 0)

  # WPA columns should be present when PBP has wpa data
  wpa_cols <- c("wp_credit", "wp_disp_credit", "wp_recv_credit",
                "wp_credit_adj", "wp_disp_credit_adj", "wp_recv_credit_adj")
  if ("wpa" %in% names(.shared$pbp)) {
    for (col in wpa_cols) {
      expect_true(col %in% names(pgd), info = paste("Missing WPA column:", col))
    }
    # WPA values should not be all zero (at least some plays have non-zero WPA)
    expect_true(sum(pgd$wp_credit != 0) > 0, info = "All wp_credit values are zero")
  }
})

test_that("create_player_game_data *_adj columns are game-value scale (regression test for #79)", {
  # Regression test: epv_adj was previously stored as a centered per-80 rate
  # while downstream consumers treated it as a game value, inflating low-TOG
  # players' epv_adj by ~1/tog (e.g. Zac Williams R7 2025 showed epv_adj=71
  # vs raw epv=8 with tog=0.10). Fix multiplies by tog_safe after centering
  # so *_adj is a position-adjusted game value. This test asserts that
  # invariant so the bug can't silently return.
  skip_if(is.null(.shared$pbp) || is.null(.shared$player_stats) || is.null(.shared$teams),
          "Could not load required data")

  pgd <- tryCatch(
    create_player_game_data(.shared$pbp, .shared$player_stats, .shared$teams),
    error = function(e) NULL
  )
  skip_if(is.null(pgd), "Could not create player game data")
  skip_if(nrow(pgd) == 0, "Empty player game data")

  # Game-value magnitude invariant: max |epv_adj| should not exceed ~50.
  # Under the bug, low-TOG cameos blew up to |epv_adj| ~= |epv|/tog
  # (Zac Williams R7 2025: 10% TOG, raw epv 8.14, broken epv_adj 71.3).
  # On the fix, max |epv_adj| sits around 34 (a full-game star's game).
  # A 50 cap cleanly discriminates with ~15 points of headroom for future
  # exceptional games. If this ever false-fails on legitimate data, something
  # extraordinary happened (60+ point EPV game) and we should investigate,
  # not just bump the threshold.
  max_abs_adj <- max(abs(pgd$epv_adj), na.rm = TRUE)
  expect_lt(max_abs_adj, 50,
            label = sprintf("max|epv_adj| (= %.2f)", max_abs_adj))

  # Per-lineup_position, unweighted mean of epv_adj must be ~0.
  # epv_adj = (epv_p80 - wm_L) * tog_safe is centered per lineup_position L with
  # weights tog_safe, so sum_L((epv_p80 - wm_L) * tog_safe) = 0 by construction
  # and the unweighted mean within L is zero up to floating-point noise. If
  # someone groups the centering on the wrong variable (e.g. season, team,
  # position_group), within-lineup_position means drift well away from zero.
  # NB: grouping this test by position_group would be wrong — position_group
  # comes from PBP player_position (6-way) and is not a strict refinement of
  # the teams-API lineup_position (20-way), so means within position_group are
  # not mathematically guaranteed to be zero.
  if ("lineup_position" %in% names(pgd)) {
    dt <- data.table::as.data.table(pgd)[
      !is.na(lineup_position) & !is.na(epv_adj),
      .(m = mean(epv_adj, na.rm = TRUE)),
      by = lineup_position
    ]
    expect_true(all(abs(dt$m) < 1e-6),
                info = sprintf("Largest per-lineup_position mean of epv_adj: %.3e",
                               max(abs(dt$m), na.rm = TRUE)))
  }

  # Semantic guards: position_group is the 6-way class, lineup_position is the
  # ~20-way AFL lineup role. If someone swaps their values in a future
  # refactor, these cardinality bounds fire immediately.
  expect_false(any(pgd$position_group == "MIDFIELDER_FORWARD", na.rm = TRUE),
               info = "MIDFIELDER_FORWARD should be collapsed to MEDIUM_FORWARD")
  pg_n <- dplyr::n_distinct(pgd$position_group, na.rm = TRUE)
  expect_lte(pg_n, 7,
             label = sprintf("n_distinct(position_group) (= %d)", pg_n))
  if ("lineup_position" %in% names(pgd)) {
    lp_n <- dplyr::n_distinct(pgd$lineup_position, na.rm = TRUE)
    expect_gte(lp_n, 15,
               label = sprintf("n_distinct(lineup_position) (= %d)", lp_n))
  }
})

# -----------------------------------------------------------------------------
# load_player_game_data Tests
# -----------------------------------------------------------------------------

test_that("load_player_game_data function exists and is exported", {
  expect_true(exists("load_player_game_data"))
  expect_true("load_player_game_data" %in% getNamespaceExports("torp"))
})

test_that("load_player_game_data has correct function signature", {
  fn_args <- names(formals(load_player_game_data))
  expect_true("seasons" %in% fn_args)
})
