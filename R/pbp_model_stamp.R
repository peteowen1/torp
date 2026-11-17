# Which EP/WP models scored a season's play-by-play ----
#
# EP and WP are computed INTO the season play-by-play (`add_epv_vars()`,
# `add_wp_vars()`) when each round is processed, and nothing recomputes them
# when a model is re-uploaded. On 2026-11-17 the 2021-2025 files were found on
# models older than the 2026-07-22 uploads: stored `exp_pts` differed from a
# fresh build on 14,554 of 14,771 rows of 2025 round 10
# (docs/plans/WPA-REFRESH-GUARD.md). WP reads EP output (`xpoints_diff`,
# `pos_lead_prob`), so the two are fingerprinted together.
#
# Each PBP row now carries `pbp_model_id`: a short hash of the sha256s of the
# EP model, WP model and WP calibration files the build actually loaded. A
# small `pbp_model_stamps.json` on the pbp-data release lists the distinct ids
# per season, so the nightly can tell a stale season without downloading it.

PBP_MODEL_FILES <- c(ep = "ep_model.rds", wp = "wp_model.rds",
                     wp_calibration = "wp_calibration.rds")
PBP_MODEL_STAMPS_ASSET <- "pbp_model_stamps.json"
# Rows written before stamping existed, or by a build that could not say which
# models it loaded.
PBP_MODEL_UNSTAMPED <- "unstamped"

#' Fingerprint of an EP / WP / WP-calibration model set
#'
#' @param shas Named character vector of sha256s, names `ep`, `wp`,
#'   `wp_calibration`.
#' @return 12-character id, or `NA` when any sha is missing.
#' @keywords internal
.pbp_model_id_from_shas <- function(shas) {
  shas <- shas[names(PBP_MODEL_FILES)]
  if (length(shas) != length(PBP_MODEL_FILES) || anyNA(shas) || any(!nzchar(shas))) {
    return(NA_character_)
  }
  key <- paste(names(PBP_MODEL_FILES), tolower(shas), sep = "=", collapse = ";")
  substr(digest::digest(key, algo = "sha256", serialize = FALSE), 1L, 12L)
}

#' Fingerprint of the EP / WP models this session actually scores with
#'
#' Loads the three models through [load_model_with_fallback()] (the same path
#' [add_epv_vars()] and [add_wp_vars()] use), then hashes the cached files they
#' were read from. When the WP calibration is unavailable (WP is then served
#' uncalibrated) or a file cannot be found, the id is `NA`: a build must not
#' claim models it did not use.
#'
#' @param models_dir Directory holding the cached core model files
#'   (torpmodels' cache when `NULL`).
#' @param load Function `(name) -> model or NULL`; injectable for tests.
#' @return 12-character id, or `NA`.
#' @keywords internal
.loaded_pbp_model_id <- function(models_dir = NULL, load = load_model_with_fallback) {
  loaded <- vapply(names(PBP_MODEL_FILES), function(m) !is.null(load(m)), logical(1))
  if (!all(loaded)) return(NA_character_)
  if (is.null(models_dir)) {
    models_dir <- file.path(utils::getFromNamespace("get_models_dir", "torpmodels")(), "core")
  }
  paths <- file.path(models_dir, PBP_MODEL_FILES)
  shas <- vapply(paths, function(p) if (file.exists(p)) vb_sha256(p) else NA_character_,
                 character(1), USE.NAMES = FALSE)
  names(shas) <- names(PBP_MODEL_FILES)
  .pbp_model_id_from_shas(shas)
}

#' Fingerprint of the EP / WP models currently published
#'
#' @param manifest Parsed torpmodels `core-models` `models_manifest.json`
#'   (fetched when `NULL`).
#' @return 12-character id. Errors when the manifest cannot be read or lacks
#'   one of the three models: an unreadable manifest must never read as
#'   "every season is stale".
#' @keywords internal
.current_pbp_model_id <- function(manifest = NULL) {
  if (is.null(manifest)) {
    manifest <- utils::getFromNamespace(".get_models_manifest", "torpmodels")(
      utils::getFromNamespace("get_torpmodels_repo", "torpmodels")(), "core-models", verbose = FALSE)
  }
  arts <- manifest$artifacts
  if (is.null(arts)) cli::cli_abort("torpmodels core-models manifest is missing or has no artifacts")
  shas <- vapply(PBP_MODEL_FILES, function(f) {
    s <- arts[[f]]$sha256
    if (is.null(s)) NA_character_ else as.character(s)
  }, character(1))
  id <- .pbp_model_id_from_shas(shas)
  if (is.na(id)) {
    cli::cli_abort("torpmodels core-models manifest lacks a sha256 for: {names(shas)[is.na(shas)]}")
  }
  id
}

#' Stamp play-by-play rows with the models that scored them
#'
#' @param pbp Play-by-play just built by `add_epv_vars()` / `add_wp_vars()`.
#' @param id Model id (`.loaded_pbp_model_id()` when `NULL`).
#' @return `pbp` with a character `pbp_model_id` column.
#' @keywords internal
.stamp_pbp_models <- function(pbp, id = NULL) {
  if (is.null(id)) {
    # Never let the stamp break a build: any failure just means "unstamped".
    id <- tryCatch(.loaded_pbp_model_id(), error = function(e) {
      cli::cli_alert_warning("Model id lookup failed: {conditionMessage(e)}")
      NA_character_
    })
  }
  if (is.na(id)) {
    cli::cli_alert_warning("Could not tell which EP/WP model files scored this play-by-play; stamping it {.val {PBP_MODEL_UNSTAMPED}}.")
    id <- PBP_MODEL_UNSTAMPED
  }
  pbp$pbp_model_id <- rep(as.character(id), nrow(pbp))
  pbp
}

#' Distinct model ids in a season's play-by-play
#'
#' @param pbp A season's play-by-play.
#' @return Sorted character vector; rows with no stamp count as
#'   `"unstamped"`.
#' @keywords internal
.pbp_model_ids_in <- function(pbp) {
  ids <- if ("pbp_model_id" %in% names(pbp)) as.character(pbp$pbp_model_id) else rep(NA_character_, nrow(pbp))
  ids[is.na(ids) | !nzchar(ids)] <- PBP_MODEL_UNSTAMPED
  sort(unique(ids))
}

#' Seasons whose play-by-play is not wholly on the current models
#'
#' @param current_id Id of the published models ([.current_pbp_model_id()]).
#' @param stamps Named list `season -> character ids` (the stamps file).
#' @param seasons Seasons that should have play-by-play.
#' @return Integer seasons, oldest first. A season missing from `stamps` is
#'   stale.
#' @keywords internal
.stale_pbp_seasons <- function(current_id, stamps, seasons) {
  stale <- vapply(as.character(seasons), function(s) {
    ids <- stamps[[s]]
    is.null(ids) || !identical(sort(unique(as.character(unlist(ids)))), as.character(current_id))
  }, logical(1))
  sort(as.integer(seasons[stale]))
}

#' Read the per-season model stamps file from the pbp-data release
#'
#' @return Named list `season -> character ids`; empty when the file is not
#'   published yet. Errors on any other read failure.
#' @keywords internal
.read_pbp_model_stamps <- function() {
  repo <- get_torp_data_repo()
  if (!PBP_MODEL_STAMPS_ASSET %in% vb_list_assets(repo, "pbp-data")$name) return(list())
  dest <- file.path(tempfile("pbp_stamps_"), PBP_MODEL_STAMPS_ASSET)
  on.exit(unlink(dirname(dest), recursive = TRUE), add = TRUE)
  vb_download(repo, "pbp-data", PBP_MODEL_STAMPS_ASSET, dest)
  s <- jsonlite::read_json(dest, simplifyVector = TRUE)
  lapply(s, as.character)
}

#' Record one season's model ids in the stamps file
#'
#' Called after a season's `pbp_data_<season>_all` is published. Best effort:
#' a failure warns and leaves the season looking stale, which only makes the
#' nightly check report (later: rebuild) it again.
#'
#' @param season Season year.
#' @param pbp The season's play-by-play exactly as published.
#' @return Invisible `TRUE` when written, `FALSE` otherwise.
#' @keywords internal
.record_pbp_model_stamps <- function(season, pbp) {
  ok <- tryCatch({
    stamps <- .read_pbp_model_stamps()
    stamps[[as.character(season)]] <- .pbp_model_ids_in(pbp)
    stamps <- stamps[order(names(stamps))]
    path <- file.path(tempfile("pbp_stamps_up_"), PBP_MODEL_STAMPS_ASSET)
    dir.create(dirname(path), recursive = TRUE)
    on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)
    jsonlite::write_json(stamps, path, auto_unbox = FALSE, pretty = TRUE)
    safe_release_upload(path, repo = get_torp_data_repo(), tag = "pbp-data", name = PBP_MODEL_STAMPS_ASSET)
    cli::cli_inform("Recorded {season} play-by-play models: {stamps[[as.character(season)]]}")
    TRUE
  }, error = function(e) {
    cli::cli_alert_warning("Could not record {season} play-by-play model stamps: {conditionMessage(e)}")
    FALSE
  })
  invisible(ok)
}

#' Report seasons whose play-by-play is on old EP / WP models
#'
#' Report only (WPA-REFRESH-GUARD.md, PR 1): logs the stale seasons and never
#' rebuilds. The 2021-2025 rebuild is part of rating vintage v18 (Pete,
#' 2026-11-17).
#'
#' @param current_season Season year; seasons `AFL_MIN_SEASON` to this one are
#'   checked.
#' @param current_id,stamps Injectable for tests (fetched when `NULL`).
#' @return Character vector of failed steps: `"pbp_model_check"` when the
#'   check itself could not run, otherwise empty. Stale seasons are not a
#'   failure here.
#' @keywords internal
.report_stale_pbp_models <- function(current_season, current_id = NULL, stamps = NULL) {
  res <- tryCatch({
    if (is.null(current_id)) current_id <- .current_pbp_model_id()
    if (is.null(stamps)) stamps <- .read_pbp_model_stamps()
    .stale_pbp_seasons(current_id, stamps, seq.int(AFL_MIN_SEASON, as.integer(current_season)))
  }, error = function(e) {
    cli::cli_alert_danger("Could not check which models scored the published play-by-play: {conditionMessage(e)}")
    e
  })
  if (inherits(res, "error")) return("pbp_model_check")
  if (length(res)) {
    cli::cli_alert_warning(
      "Play-by-play not on the current EP/WP models ({current_id}): {res}. Report only; nothing rebuilt.")
  } else {
    cli::cli_alert_success("Every season's play-by-play is on the current EP/WP models ({current_id}).")
  }
  character()
}
