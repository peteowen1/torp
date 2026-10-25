# v4 season fit bundle: what the nightly priced a season on, reused live
# ============================================================================
#
# The v4 engine FITS three things from the data it is given: the stoppage
# baseline (`.np_stoppage_baseline()`), the disposal-difficulty models
# (`fit_disposal_models()`) and the contest models (`fit_contest_models()`).
# The nightly gives it a season. The live job (`get_player_game_ratings()`)
# gave it one match, and every guard sized for a season then aborted on it
# (torp#250: 26 marked contests against a floor of 200).
#
# So the nightly saves its season fit as a release asset beside
# `player_game_<season>`, and the live path loads it and only SCORES the
# match. The bundle is stamped with the RATING_VINTAGE and torp version that
# built it, and the loader refuses a bundle whose vintage is not the code's:
# a fit from another vintage would price the match on rules the code no
# longer runs, with nothing failing.

# One bundle per season per R session; the live job may score a round match
# by match.
.np_fit_cache <- new.env(parent = emptyenv())

#' Release asset name of a season's fit bundle
#' @keywords internal
.np_fit_asset_name <- function(season) {
  sprintf("np_fit_%d.rds", as.integer(season))
}

#' Extract the season fit bundle from a v4 player-game frame
#'
#' Reads the `np_fit` attribute `create_player_game_data(keep_np_fit = TRUE)`
#' attaches -- the stoppage baseline and the difficulty and contest models
#' that frame was priced on -- and stamps it with what built it.
#'
#' @param pgd Output of `create_player_game_data(..., keep_np_fit = TRUE)`.
#' @param season The season the frame covers.
#' @return A `torp_np_fit` list: `stoppage_baseline`, `disposal_models`,
#'   `contest_models` and `meta` (`rating_vintage`, `torp_version`,
#'   `epv_engine`, `contest_outcomes`, `season`, `n_matches`,
#'   `last_match_id`, `built_at_utc`).
#' @keywords internal
.np_fit_bundle <- function(pgd, season) {
  fit <- attr(pgd, "np_fit")
  if (is.null(fit)) {
    cli::cli_abort(c(
      "The player-game frame carries no {.field np_fit} attribute.",
      "i" = "Build it with {.code create_player_game_data(..., keep_np_fit = TRUE)} under the v4 engine."
    ))
  }
  ids <- unique(as.character(pgd$match_id))
  b <- list(
    stoppage_baseline = data.table::copy(data.table::as.data.table(fit$stoppage_baseline)),
    disposal_models = fit$disposal_models,
    contest_models = fit$contest_models,
    meta = list(
      rating_vintage = RATING_VINTAGE,
      torp_version = as.character(utils::packageVersion("torp")),
      epv_engine = attr(pgd, "epv_engine") %||% EPV_ENGINE,
      contest_outcomes = EPV3_CONTEST_OUTCOMES,
      season = as.integer(season),
      n_matches = length(ids),
      last_match_id = if (length(ids)) max(ids) else NA_character_,
      built_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
  )
  class(b) <- c("torp_np_fit", "list")
  .check_np_fit_bundle(b)
}

#' Refuse a fit bundle that is incomplete or from another vintage
#'
#' @param b A bundle from `.np_fit_bundle()` (or read back from disk).
#' @param vintage The vintage it must carry; the code's `RATING_VINTAGE`.
#' @return `b`, invisibly.
#' @keywords internal
.check_np_fit_bundle <- function(b, vintage = RATING_VINTAGE) {
  if (!is.list(b) || !inherits(b, "torp_np_fit")) {
    cli::cli_abort("Not a v4 season fit bundle (class {.cls {class(b)}}).")
  }
  m <- b$meta
  got <- if (is.null(m$rating_vintage)) NA_character_ else as.character(m$rating_vintage)
  if (!identical(got, vintage)) {
    cli::cli_abort(c(
      "The season fit bundle is vintage {.val {got}}; this code is {.val {vintage}}.",
      "x" = "Pricing a match on another vintage's fit would publish numbers the code no longer produces.",
      "i" = "Rebuild the bundle with the nightly (update_player_game_data()) on this code."
    ))
  }
  if (!identical(m$epv_engine, "v4")) {
    cli::cli_abort("The season fit bundle was built by engine {.val {m$epv_engine}}, not {.val v4}.")
  }
  if (!identical(m$contest_outcomes, EPV3_CONTEST_OUTCOMES)) {
    cli::cli_abort(c(
      "The bundle's contest models are {.val {m$contest_outcomes}}-way; the code runs {.val {EPV3_CONTEST_OUTCOMES}}.",
      "x" = "score_contests() would price the wrong branches without erroring."
    ))
  }
  bl <- b$stoppage_baseline
  need_bl <- c("description", "band", "baseline")
  if (!is.data.frame(bl) || nrow(bl) == 0 || length(setdiff(need_bl, names(bl)))) {
    cli::cli_abort("The bundle's stoppage baseline is missing or lacks {.val {need_bl}}.")
  }
  need_d <- c("p", "ret", "trn")
  need_c <- if (identical(EPV3_CONTEST_OUTCOMES, "three")) {
    c("p_other", "p_att", "att", "def", "oth")
  } else {
    c("p", "att", "def")
  }
  for (piece in list(list("disposal_models", need_d), list("contest_models", need_c))) {
    mods <- b[[piece[[1]]]]
    have <- if (is.list(mods)) names(mods)[vapply(mods, function(x) inherits(x, "gam"), logical(1))] else character()
    miss <- setdiff(piece[[2]], have)
    if (length(miss)) {
      cli::cli_abort("The bundle's {.field {piece[[1]]}} lack fitted model{?s} {.val {miss}}.")
    }
  }
  invisible(b)
}

#' Write a fit bundle to disk atomically
#'
#' @param b Bundle from `.np_fit_bundle()`.
#' @param path Destination `.rds`.
#' @return `path`, invisibly.
#' @keywords internal
.write_np_fit_bundle <- function(b, path) {
  .check_np_fit_bundle(b)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("np_fit_", tmpdir = dirname(path), fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(b, tmp)
  if (!file.rename(tmp, path)) {
    cli::cli_abort("Could not move the fit bundle into place at {.file {path}}.")
  }
  invisible(path)
}

#' Publish a season's fit bundle to the torpdata release
#'
#' Same never-delete-first upload and manifest record as `save_to_release()`,
#' for an `.rds` instead of a parquet. Called by the daily release only.
#'
#' @param b Bundle from `.np_fit_bundle()`.
#' @return The asset name, invisibly.
#' @keywords internal
.publish_np_fit_bundle <- function(b) {
  rlang::check_installed("piggyback", version = "0.1.4", reason = "to upload data to GitHub releases")
  name <- .np_fit_asset_name(b$meta$season)
  path <- file.path(tempfile("np_fit_dir_"), name)
  on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)
  .write_np_fit_bundle(b, path)
  repo <- get_torp_data_repo()
  restore_listing <- .vb_with_fresh_listing()
  on.exit(restore_listing(), add = TRUE)
  .vb_retry(function() safe_release_upload(path, repo = repo, tag = NP_FIT_RELEASE_TAG, name = name),
            times = 3L, delays = c(5, 15),
            should_retry = function(e) inherits(e, "vb_error_transient"))
  invalidate_release_cache(NP_FIT_RELEASE_TAG)
  tryCatch(
    .publish_bus_manifest(NP_FIT_RELEASE_TAG, name, path, rows = NA_integer_),
    error = function(e) {
      cli::cli_warn("Could not update bus_manifest.json for {.val {NP_FIT_RELEASE_TAG}}: {conditionMessage(e)}")
    }
  )
  cli::cli_alert_success(
    "Published {.val {name}} ({round(file.size(path) / 1e6, 1)} MB, vintage {b$meta$rating_vintage}, {b$meta$n_matches} matches)")
  invisible(name)
}

#' Load a season's fit bundle for the live path
#'
#' @param season Season of the match being scored.
#' @param path A local `.rds` to read instead of the release.
#' @return A checked `torp_np_fit` bundle.
#' @keywords internal
.load_np_fit_bundle <- function(season, path = NULL) {
  if (!is.null(path)) {
    if (!file.exists(path)) cli::cli_abort("No fit bundle at {.file {path}}.")
    b <- .check_np_fit_bundle(readRDS(path))
    cli::cli_alert_info(
      "Season fit bundle from {.file {path}}: {b$meta$season}, {b$meta$n_matches} matches, vintage {b$meta$rating_vintage}")
    return(b)
  }
  key <- as.character(as.integer(season))
  hit <- .np_fit_cache[[key]]
  if (!is.null(hit) && identical(hit$meta$rating_vintage, RATING_VINTAGE)) return(hit)
  name <- .np_fit_asset_name(season)
  dest <- file.path(tempdir(), "torp_np_fit", name)
  tryCatch(
    vb_download(get_torp_data_repo(), NP_FIT_RELEASE_TAG, name, dest),
    error = function(e) {
      cli::cli_abort(c(
        "Could not fetch the season fit bundle {.val {name}} from {get_torp_data_repo()}@{NP_FIT_RELEASE_TAG}.",
        "x" = "The live path scores on the nightly's season fit and will not refit on one match (torp#250).",
        "i" = "The daily release publishes it from update_player_game_data(); or pass {.arg np_fit} a local bundle path."
      ), parent = e)
    }
  )
  b <- .check_np_fit_bundle(readRDS(dest))
  cli::cli_alert_info(
    "Season fit bundle {.val {name}}: {b$meta$n_matches} matches to {b$meta$last_match_id}, vintage {b$meta$rating_vintage}, built {b$meta$built_at_utc} by torp {b$meta$torp_version}")
  assign(key, b, envir = .np_fit_cache)
  b
}

#' Resolve the live path's `np_fit` argument to a checked bundle
#'
#' @param np_fit `NULL` (the release asset for `season`), a path to a local
#'   `.rds`, or a bundle already in memory.
#' @param season Season of the match being scored.
#' @keywords internal
.resolve_np_fit <- function(np_fit, season) {
  b <- if (is.null(np_fit)) {
    # Early in a season the nightly has not yet published that season's fit
    # (round 1, or while its first few matches are too few to fit on). Score on
    # the previous season's fit until it has (Pete, 2026-10-26).
    tryCatch(.load_np_fit_bundle(season), error = function(e) {
      prev <- as.integer(season) - 1L
      cli::cli_alert_warning(
        "No {season} season fit yet ({conditionMessage(e)}); falling back to the {prev} fit.")
      .load_np_fit_bundle(prev)
    })
  } else if (is.character(np_fit) && length(np_fit) == 1L) {
    .load_np_fit_bundle(season, path = np_fit)
  } else {
    .check_np_fit_bundle(np_fit)
  }
  if (!identical(as.integer(b$meta$season), as.integer(season))) {
    cli::cli_alert_warning(
      "Scoring a {season} match on the {b$meta$season} season fit.")
  }
  b
}
