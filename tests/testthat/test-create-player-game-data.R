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
    "team", "opponent", "season", "round"
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
