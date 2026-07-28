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

test_that("vintage label and filename are independent", {
  # The switch-straight-away path writes torp_ratings.parquet under v2
  # constants. Deriving the label from the filename would record the new data
  # as v1 -- labelling it as the data it replaced.
  e <- .build_rating_vintage_entry(10, version = "v2",
                                   file = "torp_ratings.parquet",
                                   generated_utc = "x")
  expect_equal(e$file, "torp_ratings.parquet")
  expect_true(e$defining_constants$EPV_POSITION_STANDARDISE)
  m <- .merge_rating_manifest(NULL, "v2", e)
  expect_named(m$vintages, "v2")
  expect_equal(m$vintages$v2$file, "torp_ratings.parquet")
})

test_that("only an explicit set_canonical changes canonical", {
  # .merge_rating_manifest never promotes; publish_ratings_manifest promotes
  # only when the run actually wrote canonical.
  expect_false(isTRUE(formals(publish_ratings_manifest)$set_canonical))
  e <- .build_rating_vintage_entry(1, "v2", "torp_ratings.parquet", "x")
  expect_equal(.merge_rating_manifest(NULL, "v2", e)$canonical, "v1")
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

test_that("preserve_rating_vintage defaults to dry_run", {
  # A function that writes to a release by default is one that writes to a
  # release by accident. Asserted rather than trusted, because the default is
  # the only thing standing between a typo and an overwritten vintage.
  expect_true(isTRUE(formals(preserve_rating_vintage)$dry_run))
})

test_that("preserve_rating_vintage validates the label before touching anything", {
  # Label validation must happen before any network call, so a bad label is a
  # fast local error rather than a partial release operation.
  expect_error(preserve_rating_vintage("latest"), "Unrecognised rating vintage")
})

test_that("RATING_VINTAGE tracks the adopted constants", {
  # v2 is defined as "standardisation on"; if someone turns it off without
  # bumping the label the manifest would misdescribe the output.
  expect_match(RATING_VINTAGE, "^v[0-9]+$")
  if (isTRUE(EPV_POSITION_STANDARDISE)) expect_false(RATING_VINTAGE == "v1")
})

test_that("an unreadable existence probe aborts rather than assuming 'absent'", {
  # The guard this function provides is worthless if an unreadable probe is
  # treated as "vintage doesn't exist yet, safe to write". A confirmed 404
  # returns 0 rows WITHOUT erroring, so anything that throws here is unexpected
  # -- and must abort, matching versebus.R's rule that uncertain classification
  # resolves to abort, never to overwrite.
  testthat::local_mocked_bindings(
    load_torp_ratings = function(...) stop("simulated network failure")
  )
  expect_error(
    preserve_rating_vintage("v1", dry_run = FALSE),
    "Could not determine whether"
  )
})

test_that("preserve_rating_vintage aborts when the preserved copy fails to verify", {
  # The caller's next step is overwriting canonical, so a normal return means
  # "the old vintage is safe, go ahead". A console print would let a scripted
  # caller proceed on a preservation that did not work.
  canon <- data.frame(player_id = c("a", "b"), epr = c(1, 2))
  calls <- 0
  testthat::local_mocked_bindings(
    load_torp_ratings = function(version = NULL, ...) {
      calls <<- calls + 1
      if (calls == 1) return(canon[0, ])          # target vintage absent
      if (calls == 2) return(canon)               # current canonical
      canon[1, , drop = FALSE]                    # verify: wrong row count
    },
    save_to_release = function(...) invisible(NULL)
  )
  expect_error(
    preserve_rating_vintage("v1", dry_run = FALSE),
    "Verification failed"
  )
})

test_that("a preserved vintage records NO defining_constants", {
  # By preservation time the loaded constants describe the INCOMING vintage.
  # Stamping them against the outgoing data would misattribute it, and a
  # manifest that lies is worse than one with a gap.
  entry <- .build_rating_vintage_entry(100, version = "v1",
                                       file = "torp_ratings_v1.parquet",
                                       defining_constants = NULL)
  expect_null(entry$defining_constants)
  expect_equal(entry$rows, 100L)
  expect_equal(entry$file, "torp_ratings_v1.parquet")

  # ...while a normally-generated vintage still captures them.
  live <- .build_rating_vintage_entry(100, version = "v2")
  expect_false(is.null(live$defining_constants))
})
