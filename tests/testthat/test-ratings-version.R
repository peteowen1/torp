# Rating vintage provenance (decision D-DEF3, RATING-VERSIONING-PLAN.md)

test_that("canonical vintage keeps the historical filename", {
  # Consumers that pass nothing must keep receiving exactly today's file.
  expect_equal(.rating_vintage_file(NULL), "torp_ratings.parquet")
})

test_that("candidate vintages get a suffixed filename", {
  expect_equal(.rating_vintage_file("v2"), "torp_ratings_v2.parquet")
  expect_equal(.rating_vintage_file("v10"), "torp_ratings_v10.parquet")
})

test_that("an unrecognised vintage aborts rather than falling back", {
  # A silent fallback to canonical would serve a different vintage than the
  # caller asked for -- the precise failure D-DEF3 exists to prevent.
  expect_error(.rating_vintage_file("latest"), "Unrecognised rating vintage")
  expect_error(.rating_vintage_file("2"), "Unrecognised rating vintage")
  expect_error(.rating_vintage_file(c("v1", "v2")), "single non-NA string")
  expect_error(.rating_vintage_file(NA_character_), "single non-NA string")
})

test_that("defining constants are read from the live constants", {
  dc <- .rating_defining_constants()
  # If these ever drift from the actual constants the manifest becomes a lie,
  # so assert identity rather than specific values.
  expect_identical(dc$EPV_POSITION_STANDARDISE, EPV_POSITION_STANDARDISE)
  expect_identical(dc$EPV_STANDARDISE_CHANNELS, EPV_STANDARDISE_CHANNELS)
  expect_identical(dc$TORP_EPR_WEIGHT, TORP_EPR_WEIGHT)
  expect_equal(dc$LINEUP_POSITION_GROUP_MAP$CHF, "KEY_FORWARD")
})

test_that("a vintage entry records provenance and row count", {
  e <- .build_rating_vintage_entry(n_rows = 1234, version = "v2",
                                   generated_utc = "2026-07-27T00:00:00Z")
  expect_equal(e$file, "torp_ratings_v2.parquet")
  expect_equal(e$rows, 1234L)
  expect_equal(e$generated_utc, "2026-07-27T00:00:00Z")
  expect_true(nzchar(e$torp_version))
  expect_true(e$defining_constants$EPV_POSITION_STANDARDISE)
})

test_that("the v1 entry points at the canonical filename", {
  e <- .build_rating_vintage_entry(n_rows = 1, version = "v1",
                                   generated_utc = "2026-07-27T00:00:00Z")
  expect_equal(e$file, "torp_ratings.parquet")
})

test_that("merging a vintage never changes which one is canonical", {
  # Promotion must be a deliberate act; a pipeline run that could promote would
  # reintroduce the in-place-overwrite risk.
  e <- .build_rating_vintage_entry(1, "v2", "2026-07-27T00:00:00Z")
  m <- .merge_rating_manifest(NULL, "v2", e)
  expect_equal(m$canonical, "v1")
  expect_named(m$vintages, "v2")

  m2 <- .merge_rating_manifest(m, "v3", .build_rating_vintage_entry(2, "v3", "x"))
  expect_equal(m2$canonical, "v1")
  expect_setequal(names(m2$vintages), c("v2", "v3"))
})

test_that("re-writing the same vintage replaces its entry in place", {
  m <- .merge_rating_manifest(NULL, "v2", .build_rating_vintage_entry(1, "v2", "a"))
  m <- .merge_rating_manifest(m, "v2", .build_rating_vintage_entry(99, "v2", "b"))
  expect_length(m$vintages, 1L)
  expect_equal(m$vintages$v2$rows, 99L)
})

test_that("vintage stems match what save_to_release expects", {
  expect_equal(.rating_vintage_stem(NULL), "torp_ratings")
  expect_equal(.rating_vintage_stem("v2"), "torp_ratings_v2")
  # stem and file must stay in lockstep, since the pipeline writes with one
  # and verifies with the other
  expect_equal(paste0(.rating_vintage_stem("v2"), ".parquet"),
               .rating_vintage_file("v2"))
})

test_that("RATING_VINTAGE tracks the adopted constants", {
  # v2 is defined as "standardisation on"; if someone turns it off without
  # bumping the label the manifest would misdescribe the output.
  expect_match(RATING_VINTAGE, "^v[0-9]+$")
  if (isTRUE(EPV_POSITION_STANDARDISE)) expect_false(RATING_VINTAGE == "v1")
})
