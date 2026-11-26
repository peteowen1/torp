# check_vintage_alignment()'s fourth check: the EP/WP models.
#
# Why this file exists. A retrain of the EP, WP or WP-calibration model
# re-scores every play the ratings are built from, but changes no constant, so
# the defining-constants check cannot see it. Pete (2026-11-27): a retrain must
# force a vintage bump, and the rebuild stays a hand step. Each vintage entry
# records the model id; the check refuses a canonical write when the published
# models differ from it.

mk_models_manifest <- function(entry_id = "modelsA") {
  entry <- .build_rating_vintage_entry(n_rows = 10, version = RATING_VINTAGE,
                                       file = "torp_ratings.parquet",
                                       generated_utc = "2026-11-27T00:00:00Z",
                                       pbp_model_id = entry_id)
  j <- jsonlite::toJSON(list(canonical = RATING_VINTAGE,
                             vintages = stats::setNames(list(entry), RATING_VINTAGE)),
                        auto_unbox = TRUE, null = "null")
  jsonlite::fromJSON(j, simplifyVector = FALSE)
}

test_that("an entry records the model id, and NA records none", {
  e <- .build_rating_vintage_entry(1, "v2", "torp_ratings.parquet", "x", pbp_model_id = "abc123")
  expect_identical(e$pbp_model_id, "abc123")
  e0 <- .build_rating_vintage_entry(1, "v2", "torp_ratings.parquet", "x", pbp_model_id = NA_character_)
  expect_false("pbp_model_id" %in% names(e0))
  # The default reads the lookup (the test setup pins it).
  expect_identical(.build_rating_vintage_entry(1, "v2", "torp_ratings.parquet", "x")$pbp_model_id,
                   getOption("torp.pbp_model_id"))
})

test_that("matching models pass", {
  res <- check_vintage_alignment(strict = TRUE, manifest = mk_models_manifest("modelsA"),
                                 pbp_model_id = "modelsA")
  expect_true(res$aligned)
})

test_that("a retrain refuses the canonical write", {
  expect_error(
    check_vintage_alignment(strict = TRUE, manifest = mk_models_manifest("modelsA"),
                            pbp_model_id = "modelsB"),
    class = "torp_error_vintage_models_drift")
})

test_that("a candidate write is not judged on canonical's models", {
  res <- check_vintage_alignment(strict = TRUE, manifest = mk_models_manifest("modelsA"),
                                 candidate = "v99", pbp_model_id = "modelsB")
  expect_identical(res$candidate, "v99")
})

test_that("an entry recorded before the check existed passes with a warning", {
  m <- mk_models_manifest(NA_character_)
  expect_null(m$vintages[[RATING_VINTAGE]]$pbp_model_id)
  expect_message(res <- check_vintage_alignment(strict = TRUE, manifest = m, pbp_model_id = "modelsB",
                                                stale_seasons = integer(0)),
                 "does not record the EP/WP models")
  expect_true(res$aligned)
})

test_that("with no recorded models, stale play-by-play stamps refuse the write under strict", {
  m <- mk_models_manifest(NA_character_)
  expect_error(check_vintage_alignment(strict = TRUE, manifest = m, pbp_model_id = "modelsB",
                                       stale_seasons = 2025L),
               class = "torp_error_vintage_models_drift")
})

test_that("unreadable published models abort under strict and warn otherwise", {
  m <- mk_models_manifest("modelsA")
  expect_error(check_vintage_alignment(strict = TRUE, manifest = m, pbp_model_id = NA_character_),
               class = "torp_error_vintage_models_unreadable")
  expect_warning(res <- check_vintage_alignment(strict = FALSE, manifest = m, pbp_model_id = NA_character_),
                 "skipping the model check")
  expect_true(is.na(res$aligned))
})

test_that("an unknown id never erases the recorded one on a manifest write", {
  old <- mk_models_manifest("modelsA")
  local_mocked_bindings(safe_release_upload = function(...) invisible(TRUE), .package = "torp")
  local_mocked_bindings(pb_list = function(...) data.frame(file_name = "ratings_manifest.json"),
                        .package = "piggyback")
  m <- suppressMessages(publish_ratings_manifest(10, version = RATING_VINTAGE, manifest = old,
                                                 pbp_model_id = NA_character_))
  expect_identical(m$vintages[[RATING_VINTAGE]]$pbp_model_id, "modelsA")
  # A known id replaces it (the ratings run after a passing check 4).
  m2 <- publish_ratings_manifest(10, version = RATING_VINTAGE, manifest = old, pbp_model_id = "modelsA2")
  expect_identical(m2$vintages[[RATING_VINTAGE]]$pbp_model_id, "modelsA2")
  # Nothing recorded and nothing known: still nothing, not a made-up id.
  m3 <- publish_ratings_manifest(10, version = "v99", manifest = old, pbp_model_id = NA_character_)
  expect_null(m3$vintages[["v99"]]$pbp_model_id)
})

test_that("the lookup honours the override and turns a failed read into NA", {
  withr::local_options(torp.pbp_model_id = "pinned")
  expect_identical(.lookup_pbp_model_id(), "pinned")
  withr::local_options(torp.pbp_model_id = NULL)
  local_mocked_bindings(.current_pbp_model_id = function(...) stop("no manifest"), .package = "torp")
  expect_message(id <- .lookup_pbp_model_id(), "Could not read the published EP/WP model id")
  expect_true(is.na(id))
})
