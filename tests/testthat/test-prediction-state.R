# build_prediction_state() -- the seam split out of run_predictions_pipeline()
# on 2026-08-11 so that building a prediction no longer implies publishing one.
#
# These are contract and structure tests. They do not run the pipeline: it
# needs the AFL API, the torpdata releases, and a GAM + XGBoost train. What
# they do cover is the class of mistake the split actually risks -- the
# uploader reaching for a piece of state the builder forgot to hand back,
# which today would surface as "object not found" in production.

# NOTE: nothing in this file calls build_prediction_state(). It hits the AFL
# API, the torpdata releases, and publishes the season's results -- running it
# from a test would upload.

test_that("production behaviour is unchanged: refresh_results defaults to TRUE", {
  # The one side effect inside the state half is the results refresh, which
  # publishes to the results-data release. run_predictions_pipeline() must
  # still do it; only a deliberate read-only caller opts out. Compared as a
  # literal rather than eval()'d -- the default is the constant TRUE.
  expect_identical(formals(build_prediction_state)$refresh_results, TRUE)
})

test_that("run_predictions_pipeline() takes the same arguments as before the split", {
  expect_equal(names(formals(run_predictions_pipeline)),
               c("week", "weeks", "season"))
})

test_that("every piece of state the uploader reads is one the builder returns", {
  # The real failure mode of this refactor. run_predictions_pipeline() unpacks
  # `state$x`; build_prediction_state() returns a fixed list. If someone adds a
  # variable to the state half and the uploader starts using it without it
  # being returned, nothing fails until a production run.
  src <- .r_source_lines("match_model.R")
  skip_if(is.null(src), "R/ source tree not present (installed build)")

  # What the builder promises: the names in the .prediction_state() list.
  ctor_start <- grep("^\\s*\\.prediction_state <- function", src)
  expect_length(ctor_start, 1)
  ctor_end <- ctor_start + which(trimws(src[ctor_start:length(src)]) == "}")[1] - 1
  ctor <- src[ctor_start:ctor_end]
  provided <- trimws(sub("\\s*=.*$", "", grep("^\\s+[a-z_.]+\\s*=", ctor, value = TRUE)))
  provided <- provided[nzchar(provided)]
  expect_true(length(provided) >= 8)

  # What the uploader consumes: every `state$x` in the file.
  consumed <- unique(gsub(".*state\\$([A-Za-z_.]+).*", "\\1",
                          grep("state\\$[A-Za-z_.]+", src, value = TRUE)))
  consumed <- consumed[nzchar(consumed)]
  expect_true(length(consumed) >= 3)

  expect_equal(
    setdiff(consumed, provided), character(0),
    info = paste("run_predictions_pipeline() reads state fields",
                 "build_prediction_state() does not return")
  )
})

test_that("run_predictions_pipeline() no longer loads or builds anything itself", {
  # The point of the split. If loading creeps back into the wrapper, the
  # duplication this was meant to retire starts growing again.
  src <- .r_source_lines("match_model.R")
  skip_if(is.null(src), "R/ source tree not present (installed build)")

  start <- grep("^run_predictions_pipeline <- function", src)
  expect_length(start, 1)
  wrapper <- src[start:length(src)]
  end <- grep("^}", wrapper)[1]
  wrapper <- wrapper[seq_len(end)]
  code <- wrapper[!grepl("^\\s*#", wrapper)]

  expect_true(any(grepl("build_prediction_state\\(", code)),
              info = "the wrapper must delegate to the state builder")

  builders <- c("\\.build_fixtures_df\\(", "\\.build_team_ratings_df\\(",
                "\\.build_match_features\\(", "\\.build_team_mdl_df\\(",
                "\\.build_week_ratings\\(", "\\.train_match_gams\\(",
                "\\.train_match_xgb\\(", "load_torp_ratings\\(",
                "get_all_injuries\\(")
  found <- builders[vapply(builders, function(p) any(grepl(p, code)), logical(1))]
  expect_equal(gsub("\\\\", "", found), character(0),
               info = "state-building leaked back into the upload half")
})

test_that("the results refresh is the only publish inside the state half", {
  # If a second side effect appears in the builder, a read-only caller stops
  # being read-only and nothing says so.
  src <- .r_source_lines("match_model.R")
  skip_if(is.null(src), "R/ source tree not present (installed build)")

  start <- grep("^build_prediction_state <- function", src)
  end <- grep("^run_predictions_pipeline <- function", src)
  expect_length(start, 1)
  expect_length(end, 1)
  builder <- src[start:(end - 1)]
  code <- builder[!grepl("^\\s*#", builder)]

  writes <- grep("save_to_release\\(|pb_upload\\(", code, value = TRUE)
  expect_length(writes, 1)
  expect_match(writes[1], "results_", fixed = FALSE)

  # ...and it is gated.
  gate <- grep("refresh_results", code, value = TRUE)
  expect_true(any(grepl("isTRUE\\(refresh_results\\)", gate)))
})
