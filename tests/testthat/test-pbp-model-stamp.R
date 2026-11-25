shas3 <- c(ep = strrep("a", 64), wp = strrep("b", 64), wp_calibration = strrep("c", 64))

test_that("model id is deterministic, 12 characters, and NA when a sha is missing", {
  id <- torp:::.pbp_model_id_from_shas(shas3)
  expect_type(id, "character")
  expect_equal(nchar(id), 12L)
  expect_identical(id, torp:::.pbp_model_id_from_shas(rev(shas3)))          # input order does not matter
  expect_identical(id, torp:::.pbp_model_id_from_shas(toupper(shas3)))      # case does not matter
  expect_false(identical(id, torp:::.pbp_model_id_from_shas(replace(shas3, "wp", strrep("d", 64)))))
  expect_true(is.na(torp:::.pbp_model_id_from_shas(replace(shas3, "wp_calibration", NA))))
  expect_true(is.na(torp:::.pbp_model_id_from_shas(shas3[c("ep", "wp")])))
})

test_that("the loaded id hashes the cached files, and is NA when a model did not load", {
  dir <- withr::local_tempdir()
  for (f in torp:::PBP_MODEL_FILES) writeLines(f, file.path(dir, f))
  all_loaded <- function(m) list(m)
  shas <- vapply(file.path(dir, torp:::PBP_MODEL_FILES), torp:::vb_sha256, character(1), USE.NAMES = FALSE)
  names(shas) <- names(torp:::PBP_MODEL_FILES)
  expect_identical(torp:::.loaded_pbp_model_id(dir, load = all_loaded), torp:::.pbp_model_id_from_shas(shas))

  no_calib <- function(m) if (m == "wp_calibration") NULL else list(m)
  expect_true(is.na(torp:::.loaded_pbp_model_id(dir, load = no_calib)))

  unlink(file.path(dir, "ep_model.rds"))
  expect_true(is.na(torp:::.loaded_pbp_model_id(dir, load = all_loaded)))
})

test_that("the current id comes from the manifest, and a bad manifest errors rather than reading as stale", {
  man <- list(artifacts = list(
    ep_model.rds = list(sha256 = shas3[["ep"]]),
    wp_model.rds = list(sha256 = shas3[["wp"]]),
    wp_calibration.rds = list(sha256 = shas3[["wp_calibration"]])))
  expect_identical(torp:::.current_pbp_model_id(man), torp:::.pbp_model_id_from_shas(shas3))
  man$artifacts$wp_calibration.rds <- NULL
  expect_error(torp:::.current_pbp_model_id(man), "wp_calibration")
  expect_error(torp:::.current_pbp_model_id(list()), "no artifacts")
})

test_that("stamping adds the id to every row, and an unknown id is stamped as unstamped", {
  pbp <- data.frame(match_id = c("m1", "m1", "m2"))
  expect_identical(torp:::.stamp_pbp_models(pbp, id = "abc123def456")$pbp_model_id, rep("abc123def456", 3))
  expect_identical(suppressMessages(torp:::.stamp_pbp_models(pbp, id = NA_character_))$pbp_model_id,
                   rep("unstamped", 3))
})

test_that("a season's ids treat missing and blank stamps as unstamped", {
  expect_identical(torp:::.pbp_model_ids_in(data.frame(x = 1:2)), "unstamped")
  expect_identical(torp:::.pbp_model_ids_in(data.frame(pbp_model_id = c("b", NA, "a", "", "a"))),
                   c("a", "b", "unstamped"))
})

test_that("a season is stale unless every row is on the current models", {
  stamps <- list(`2023` = "cur", `2024` = c("cur", "unstamped"), `2025` = "old")
  expect_identical(torp:::.stale_pbp_seasons("cur", stamps, 2022:2025), c(2022L, 2024L, 2025L))
  expect_identical(torp:::.stale_pbp_seasons("cur", list(`2026` = "cur"), 2026L), integer(0))
  expect_identical(torp:::.stale_pbp_seasons("cur", list(), 2026:2025), c(2025L, 2026L))
})

test_that("recording a season round-trips through the stamps file and keeps other seasons", {
  store <- withr::local_tempdir()
  published <- character()
  local_mocked_bindings(
    get_torp_data_repo = function() "owner/repo",
    vb_list_assets = function(repo, tag) data.frame(name = list.files(store)),
    vb_download = function(repo, tag, name, dest, ...) {
      # Must read without the strict-mode manifest check (the nightly sets
      # VERSEBUS_STRICT=1 and the manifest commit is best effort).
      if (!identical(list(...)$require_manifest, FALSE)) stop("strict read: would refuse an uncommitted asset")
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      file.copy(file.path(store, name), dest, overwrite = TRUE)
    },
    safe_release_upload = function(path, repo, tag, name) file.copy(path, file.path(store, name), overwrite = TRUE),
    .publish_bus_manifest = function(tag, name, local_path, ...) published <<- c(published, name)
  )
  expect_identical(torp:::.read_pbp_model_stamps(), list())               # not published yet
  expect_true(suppressMessages(torp:::.record_pbp_model_stamps(2025L, data.frame(pbp_model_id = "old"))))
  expect_true(suppressMessages(torp:::.record_pbp_model_stamps(2026L, data.frame(pbp_model_id = c("cur", "cur")))))
  expect_true(suppressMessages(torp:::.record_pbp_model_stamps(2024L, data.frame(x = 1))))   # unstamped rows
  s <- torp:::.read_pbp_model_stamps()
  expect_identical(s, list(`2024` = "unstamped", `2025` = "old", `2026` = "cur"))
  expect_identical(torp:::.stale_pbp_seasons("cur", s, 2024:2026), c(2024L, 2025L))
  expect_identical(published, rep("pbp_model_stamps.json", 3))
})

test_that("a failed stamps write warns and never errors", {
  local_mocked_bindings(.read_pbp_model_stamps = function() stop("network down"))
  expect_false(suppressMessages(torp:::.record_pbp_model_stamps(2026L, data.frame(pbp_model_id = "cur"))))
})

test_that("the report never fails on stale seasons, and fails only when the check cannot run", {
  expect_identical(
    suppressMessages(torp:::.report_stale_pbp_models(2026L, current_id = "cur", stamps = list(`2026` = "old"))),
    character())
  expect_identical(
    suppressMessages(torp:::.report_stale_pbp_models(2022L, current_id = "cur",
                                                     stamps = list(`2021` = "cur", `2022` = "cur"))),
    character())
  local_mocked_bindings(.current_pbp_model_id = function(...) stop("manifest unreachable"))
  expect_identical(suppressMessages(torp:::.report_stale_pbp_models(2026L, stamps = list())), "pbp_model_check")
})

test_that(".score_pbp runs the seven steps in order and stamps only when asked", {
  steps <- c("clean_pbp", "clean_model_data_epv", "clean_shots_data", "add_shot_vars",
             "add_epv_vars", "clean_model_data_wp", "add_wp_vars")
  mk <- function(nm) function(df, ...) { attr(df, "trail") <- c(attr(df, "trail"), nm); df }
  local_mocked_bindings(clean_pbp = mk("clean_pbp"), clean_model_data_epv = mk("clean_model_data_epv"),
                        clean_shots_data = mk("clean_shots_data"), add_shot_vars = mk("add_shot_vars"),
                        add_epv_vars = mk("add_epv_vars"), clean_model_data_wp = mk("clean_model_data_wp"),
                        add_wp_vars = mk("add_wp_vars"),
                        .stamp_pbp_models = function(pbp, id = NULL) { pbp$pbp_model_id <- "test-id"; pbp },
                        .package = "torp")
  x <- data.frame(a = 1:3)
  s <- .score_pbp(x)
  expect_identical(attr(s, "trail"), steps)
  expect_identical(s$pbp_model_id, rep("test-id", 3))
  u <- .score_pbp(x, stamp = FALSE)
  expect_identical(attr(u, "trail"), steps)
  expect_false("pbp_model_id" %in% names(u))
})
