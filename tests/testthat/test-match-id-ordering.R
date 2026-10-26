# -----------------------------------------------------------------------------
# match_id text order vs play order
#
# calculate_epr_stats() / .build_team_ratings_df() decide which matches count as
# "before" a round by comparing match_id strings (`match_id <= match_ref`, with
# match_ref = "CD_M{year}014{RR}"). That is only valid while text order of the
# IDs agrees with round order across seasons, regular rounds and finals.
# Format: CD_M{YYYY}{CCC}{RR}{GG} -- round and game are zero-padded to 2 digits
# and finals continue the round count (2021: R24 = Finals Week 1, R27 = Grand
# Final).
#
# Fixture provenance: the IDs follow that real format (most of them exist in the
# torpdata `results-data` release, results_2021/2022.parquet; CD_M20220140001 is
# an invented round-00 sentinel and is not a real match). The start times are
# ILLUSTRATIVE, written to be consistent with round order; they were not copied
# from the release and several differ from the real kick-offs.
#
# Within a round, IDs are NOT in kick-off order. Checked on the release's 1,280
# matches (2021-2026): order(match_id) differs from order(utc_start_time) for
# 1,111 pairs, all inside a single round (e.g. 2021 R10 game 01 kicked off after
# game 02). Across rounds there are no overlaps. The rating code only relies on
# round-level order (match_ref is a round prefix), so that is what is asserted
# here; do not read these tests as promising kick-off order within a round.
# -----------------------------------------------------------------------------

match_id_fixture <- function() {
  data.frame(
    match_id = c(
      "CD_M20210140101", "CD_M20210140901", "CD_M20210141001", "CD_M20210141002",
      "CD_M20210142301", "CD_M20210142401", "CD_M20210142404", "CD_M20210142501",
      "CD_M20210142601", "CD_M20210142701",
      "CD_M20220140001", "CD_M20220140101", "CD_M20220142701"
    ),
    start = as.POSIXct(c(
      "2021-03-18 08:25", "2021-05-14 09:50", "2021-05-21 09:50", "2021-05-22 05:20",
      "2021-08-20 09:50", "2021-08-27 09:50", "2021-08-29 05:20", "2021-09-03 09:50",
      "2021-09-10 09:50", "2021-09-25 09:15",
      "2022-03-16 08:25", "2022-03-24 08:25", "2022-09-24 09:30"
    ), tz = "UTC"),
    stringsAsFactors = FALSE
  )
}

test_that("match_id text order equals date order across seasons, rounds and finals", {
  d <- match_id_fixture()
  # Shuffle so the assertion is not satisfied by construction.
  d <- d[c(9, 3, 13, 1, 6, 11, 2, 10, 5, 8, 12, 4, 7), ]

  expect_identical(d$match_id[order(d$match_id)], d$match_id[order(d$start, d$match_id)])
})

test_that("two-digit round codes keep round 9 before round 10 and finals after the home-and-away rounds", {
  ids <- c("CD_M20210141001", "CD_M20210140901", "CD_M20210142701", "CD_M20210142301")
  expect_identical(
    sort(ids),
    c("CD_M20210140901", "CD_M20210141001", "CD_M20210142301", "CD_M20210142701")
  )
})

test_that("round-level match_ref cutoff selects exactly the earlier rounds", {
  d <- match_id_fixture()
  # Same construction as ratings_build.R: paste0("CD_M", year, "014", sprintf("%02d", round))
  ref <- paste0("CD_M", 2021, "014", sprintf("%02d", 10))
  before <- d$match_id[d$match_id <= ref]

  # Round 10 itself is excluded (prefix is shorter than a full ID), earlier rounds
  # are in, later rounds and finals and the 2022 season are out.
  expect_identical(before, c("CD_M20210140101", "CD_M20210140901"))
  expect_true(all(d$start[d$match_id %in% before] < min(d$start[d$match_id > ref & d$match_id < "CD_M2022"])))

  finals_ref <- paste0("CD_M", 2021, "014", sprintf("%02d", 27))
  expect_true(all(c("CD_M20210142601", "CD_M20210142401") %in% d$match_id[d$match_id <= finals_ref]))
  expect_false("CD_M20210142701" %in% d$match_id[d$match_id <= finals_ref])
})
