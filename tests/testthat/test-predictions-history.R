# torp issue #303: every publish of the locked predictions also lands as a NEW
# dated file on predictions-history, and a published file is never overwritten.
# No network: vb_list_assets() and save_to_release() are mocked, so nothing is
# uploaded.

HISTORY_REQUIRED_COLS <- c("verse", "event", "market", "selection",
                           "probability", "published_utc", "model_version")

.mock_history_combined <- function() {
  data.frame(
    season = 2026L,
    week = 5L,
    match_id = c("M1", "M2"),
    home_team = c("Footscray", "GWS Giants"),
    away_team = c("Sydney", "Sydney Swans"),
    pred_win = c(0.62, 0.35),
    utc_start_time = c("2026-11-05T09:30:00.000+0000", "2026-11-05T11:45:00.000+0000"),
    generated_utc = c("2026-11-04T23:00:00Z", "2026-11-04T23:00:00Z"),
    stringsAsFactors = FALSE
  )
}

.hist_listing <- function(names = character(0)) {
  data.frame(
    name = names,
    size = numeric(length(names)),
    updated_at = character(length(names)),
    id = integer(length(names)),
    stringsAsFactors = FALSE
  )
}

.hist_condition <- function(class, msg) {
  stop(structure(class = c(class, "vb_error", "error", "condition"),
                 list(message = msg, call = NULL)))
}

test_that("history has the required columns and two rows per match", {
  hist <- .build_prediction_history(
    .mock_history_combined(), as.POSIXct("2026-11-05 07:30:15", tz = "UTC"), "epv-v4/rating-v17"
  )
  expect_true(all(HISTORY_REQUIRED_COLS %in% names(hist)))
  expect_equal(nrow(hist), 4)
  expect_equal(unique(hist$verse), "afl")
  expect_equal(unique(hist$market), "match_winner")
  expect_equal(unique(hist$published_utc), "2026-11-05T07:30:15Z")
  expect_equal(unique(hist$model_version), "epv-v4/rating-v17")
})

test_that("each match has a home and an away row whose probabilities sum to 1", {
  hist <- .build_prediction_history(
    .mock_history_combined(), as.POSIXct("2026-11-05 07:30:15", tz = "UTC"), "v"
  )
  per_event <- split(hist, hist$event)
  expect_equal(names(per_event), c("M1", "M2"))
  for (ev in per_event) expect_equal(sum(ev$probability), 1)

  # pred_win is the HOME win probability; the away row is 1 - pred_win.
  m1 <- hist[hist$event == "M1", ]
  expect_equal(m1$probability[m1$selection == "Western Bulldogs"], 0.62)
  expect_equal(m1$probability[m1$selection == "Sydney Swans"], 0.38)
})

test_that("selection is the normalised home or away team of its match", {
  hist <- .build_prediction_history(
    .mock_history_combined(), as.POSIXct("2026-11-05 07:30:15", tz = "UTC"), "v"
  )
  # "Footscray" and "Sydney" are raw aliases; selection must carry the canonical name.
  expect_setequal(hist$selection[hist$event == "M1"], c("Western Bulldogs", "Sydney Swans"))
  expect_setequal(hist$selection[hist$event == "M2"], c("GWS Giants", "Sydney Swans"))
})

test_that("history refuses to build without the match identity or the probability", {
  bad <- .mock_history_combined()
  bad$pred_win <- NULL
  expect_error(
    .build_prediction_history(bad, as.POSIXct("2026-11-05 07:30:15", tz = "UTC"), "v"),
    "pred_win"
  )
})

test_that("the dated name is predictions_<UTC to the second>.parquet on predictions-history", {
  rec <- new.env()
  rec$calls <- list()
  local_mocked_bindings(
    vb_list_assets = function(repo, tag) .hist_listing(),
    save_to_release = function(df, file_name, release_tag, also_csv = FALSE, prev_rows_floor = NULL) {
      rec$calls[[length(rec$calls) + 1L]] <- list(
        file_name = file_name, release_tag = release_tag,
        rows = nrow(df), also_csv = also_csv
      )
      invisible(NULL)
    }
  )

  .publish_prediction_history(
    .mock_history_combined(), published_at = as.POSIXct("2026-11-05 07:30:15", tz = "UTC"),
    model_version = "v", repo = "peteowen1/torpdata"
  )

  expect_length(rec$calls, 1)
  expect_equal(rec$calls[[1]]$file_name, "predictions_20261105T073015Z")
  expect_equal(rec$calls[[1]]$release_tag, "predictions-history")
  expect_equal(rec$calls[[1]]$rows, 4)
  expect_false(rec$calls[[1]]$also_csv)
  expect_match(paste0(rec$calls[[1]]$file_name, ".parquet"),
               "^predictions_\\d{8}T\\d{6}Z\\.parquet$")
})

test_that("publish refuses to overwrite an existing dated file and writes nothing", {
  rec <- new.env()
  rec$saves <- 0L
  local_mocked_bindings(
    vb_list_assets = function(repo, tag) .hist_listing("predictions_20261105T073015Z.parquet"),
    save_to_release = function(...) {
      rec$saves <- rec$saves + 1L
      invisible(NULL)
    }
  )

  expect_error(
    .publish_prediction_history(
      .mock_history_combined(), published_at = as.POSIXct("2026-11-05 07:30:15", tz = "UTC"),
      model_version = "v", repo = "peteowen1/torpdata"
    ),
    "already exists"
  )
  expect_equal(rec$saves, 0L)
})

test_that("a transient listing failure aborts the publish; it is never read as an empty tag", {
  rec <- new.env()
  rec$saves <- 0L
  local_mocked_bindings(
    vb_list_assets = function(repo, tag) .hist_condition("vb_error_transient", "simulated 500"),
    save_to_release = function(...) {
      rec$saves <- rec$saves + 1L
      invisible(NULL)
    }
  )

  expect_error(
    .publish_prediction_history(
      .mock_history_combined(), published_at = as.POSIXct("2026-11-05 07:30:15", tz = "UTC"),
      model_version = "v", repo = "peteowen1/torpdata"
    ),
    "simulated 500"
  )
  expect_equal(rec$saves, 0L)
})

test_that("an absent history tag counts as empty, so the first publish creates the tag's first file", {
  rec <- new.env()
  rec$calls <- list()
  local_mocked_bindings(
    vb_list_assets = function(repo, tag) .hist_condition("vb_error_absent", "404 not found"),
    save_to_release = function(df, file_name, release_tag, also_csv = FALSE, prev_rows_floor = NULL) {
      rec$calls[[length(rec$calls) + 1L]] <- list(file_name = file_name, release_tag = release_tag)
      invisible(NULL)
    }
  )

  .publish_prediction_history(
    .mock_history_combined(), published_at = as.POSIXct("2026-11-05 07:30:15", tz = "UTC"),
    model_version = "v", repo = "peteowen1/torpdata"
  )

  expect_length(rec$calls, 1)
  expect_equal(rec$calls[[1]]$release_tag, "predictions-history")
})

test_that("a publish with no rows writes no history file and does not touch the listing", {
  rec <- new.env()
  rec$saves <- 0L
  local_mocked_bindings(
    vb_list_assets = function(repo, tag) stop("listing should not be read for an empty publish"),
    save_to_release = function(...) {
      rec$saves <- rec$saves + 1L
      invisible(NULL)
    }
  )

  suppressMessages(.publish_prediction_history(
    .mock_history_combined()[0, ], published_at = as.POSIXct("2026-11-05 07:30:15", tz = "UTC"),
    model_version = "v", repo = "peteowen1/torpdata"
  ))
  expect_equal(rec$saves, 0L)
})
