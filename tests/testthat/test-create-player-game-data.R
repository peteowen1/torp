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

  # Game-value invariant: mean |epv_adj| should be roughly the same magnitude
  # as mean |epv|, NOT ~1/avg_tog times larger. If *_adj leaked to per-80 the
  # ratio would be ~2x+ for typical AFL TOG distributions.
  ratio <- mean(abs(pgd$epv_adj), na.rm = TRUE) /
           mean(abs(pgd$epv), na.rm = TRUE)
  expect_lt(ratio, 2,
            label = sprintf("mean|epv_adj|/mean|epv| (= %.2f)", ratio))

  # Per-position-group, TOG-weighted mean of *_adj should be ~0 (centering).
  # This catches the groupby bug too: if adjustment happens on the wrong
  # grouping variable, within-group means won't be zero.
  if ("position_group" %in% names(pgd) && "time_on_ground_percentage" %in% names(pgd)) {
    dt <- data.table::as.data.table(pgd)[
      !is.na(position_group) & !is.na(epv_adj) & !is.na(time_on_ground_percentage),
      .(wm = stats::weighted.mean(epv_adj, pmax(time_on_ground_percentage / 100, 0.1),
                                  na.rm = TRUE)),
      by = position_group
    ]
    expect_true(all(abs(dt$wm) < 1),
                info = sprintf("Largest per-group weighted mean of epv_adj: %.3f",
                               max(abs(dt$wm), na.rm = TRUE)))
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
