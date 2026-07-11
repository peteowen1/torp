# torp H1 / ECOSYSTEM-FIX-PLAN.md T6: save_injury_data() must never treat a
# 0-row (or errored) read of existing history as "safe to overwrite". Only a
# positively-confirmed-absent release asset justifies a fresh upload. No
# network -- load_injury_data/vb_confirm_absent/save_to_release are mocked.

.new_injury_row <- function(player = "New Player", team = "Richmond") {
  data.frame(
    player = player, team = team, injury = "Hamstring",
    estimated_return = "Round 5", updated = "2026-07-01",
    player_norm = tolower(player), source = "weekly",
    stringsAsFactors = FALSE
  )
}

test_that("save_injury_data refuses to overwrite when 0-row existing is not confirmed absent", {
  local_mocked_bindings(
    load_injury_data = function(seasons = get_afl_season(), columns = NULL) data.frame(),
    vb_confirm_absent = function(repo, tag, name) FALSE,  # asset IS present -- 0-row read was ambiguous
    save_to_release = function(...) stop("save_to_release must not be called")
  )

  expect_error(
    save_injury_data(.new_injury_row(), 2026),
    "not confirmed absent"
  )
})

test_that("save_injury_data proceeds fresh only when the release asset is confirmed absent", {
  uploaded <- NULL
  local_mocked_bindings(
    load_injury_data = function(seasons = get_afl_season(), columns = NULL) data.frame(),
    vb_confirm_absent = function(repo, tag, name) TRUE,  # genuinely absent (first-ever publish)
    save_to_release = function(df, file_name, release_tag, also_csv = FALSE, prev_rows_floor = NULL) {
      uploaded <<- df
      invisible(NULL)
    }
  )

  save_injury_data(.new_injury_row(), 2026)
  expect_equal(nrow(uploaded), 1)
})

test_that("save_injury_data aborts loudly (never silently NULLs) on a transient load error", {
  local_mocked_bindings(
    load_injury_data = function(seasons = get_afl_season(), columns = NULL) {
      stop(structure(
        class = c("vb_error_transient", "vb_error", "error", "condition"),
        list(message = "simulated network blip", call = NULL)
      ))
    },
    save_to_release = function(...) stop("save_to_release must not be called")
  )

  expect_error(
    save_injury_data(.new_injury_row(), 2026),
    "aborting to avoid wiping season history"
  )
})

test_that("save_injury_data merges into existing season history instead of replacing it", {
  existing_df <- data.frame(
    player = "Old Player", team = "Carlton", injury = "Knee",
    estimated_return = "Round 3", updated = "2026-06-01",
    player_norm = "old player", source = "weekly",
    scraped_at = as.POSIXct("2026-06-01", tz = "UTC"),
    scraped_date = as.Date("2026-06-01"),
    stringsAsFactors = FALSE
  )

  uploaded <- NULL
  local_mocked_bindings(
    load_injury_data = function(seasons = get_afl_season(), columns = NULL) existing_df,
    save_to_release = function(df, file_name, release_tag, also_csv = FALSE, prev_rows_floor = NULL) {
      uploaded <<- df
      invisible(NULL)
    }
  )

  save_injury_data(.new_injury_row(), 2026)

  expect_equal(nrow(uploaded), 2)
  expect_setequal(uploaded$player, c("Old Player", "New Player"))
})
