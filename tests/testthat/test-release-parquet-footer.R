# save_to_release()'s parquet writer drops R's metadata dump from the footer for
# player_game and score_events (issue #261). These tests call the writer
# directly, so nothing is uploaded.

footer_bytes <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  seek(con, file.size(path) - 8)
  readBin(con, "integer", n = 1, size = 4)
}

footer_keys <- function(path) {
  names(arrow::read_parquet(path, as_data_frame = FALSE)$metadata)
}

# An np_breakdown attribute is what bloats the live footer; epv_engine is a
# column, as on the published frame.
make_pgd <- function(n = 60000) {
  dt <- data.table::data.table(
    player_id = sprintf("p%05d", seq_len(n)),
    epv_engine = "v4",
    epv = seq_len(n) / 7
  )
  attr(dt, "np_breakdown") <- data.frame(
    play_type = rep(c("kick", "handball", "mark"), length.out = n),
    net_points = seq_len(n) / 3
  )
  dt
}

test_that("player_game and score_events stems are written without the R metadata dump", {
  df <- make_pgd()
  baseline <- tempfile(fileext = ".parquet")
  arrow::write_parquet(df, baseline)
  # The fixture must really carry the dump, or the checks below prove nothing.
  expect_gt(footer_bytes(baseline), 500000)

  for (stem in c("player_game_2025", "player_game_2025_v14",
                 "score_events_2025", "score_events_2025_v14")) {
    path <- tempfile(fileext = ".parquet")
    .write_release_parquet(df, path, stem)
    expect_false("r" %in% footer_keys(path), info = stem)
    expect_true(footer_bytes(path) < 65536, info = stem)
  }
})

test_that("other assets keep the R metadata", {
  df <- make_pgd()
  for (stem in c("np_breakdown_2025", "match_items_2025")) {
    path <- tempfile(fileext = ".parquet")
    .write_release_parquet(df, path, stem)
    expect_true("r" %in% footer_keys(path), info = stem)
  }
})

test_that("a stripped player_game file reads back with its values and epv_engine stamp", {
  df <- make_pgd()
  path <- tempfile(fileext = ".parquet")
  .write_release_parquet(df, path, "player_game_2025")
  back <- arrow::read_parquet(path)
  data.table::setDT(back)
  expect_equal(back$player_id, df$player_id)
  expect_equal(back$epv, df$epv)
  expect_true(all(back$epv_engine == "v4"))
  restored <- .restore_epv_engine_attr(back)
  expect_identical(attr(restored, "epv_engine"), "v4")
})
