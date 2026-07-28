# Rating vintage provenance
# =========================
# Supports decision D-DEF3: an EPR/PSR change ships as a NEW VINTAGE published
# alongside the canonical one, never as an in-place overwrite, so a published
# rating can always be traced to the constants that produced it.
# Design: docs/plans/RATING-VERSIONING-PLAN.md

#' Filename for a rating vintage
#'
#' The canonical vintage always lives at \code{torp_ratings.parquet} and is
#' never renamed, so a consumer that does nothing keeps receiving exactly what
#' it receives today. Candidate vintages are published alongside it under a
#' suffixed name and are strictly opt-in.
#'
#' @param version Vintage label (e.g. \code{"v2"}), or NULL for canonical.
#' @return A parquet filename.
#' @keywords internal
.rating_vintage_file <- function(version = NULL) {
  if (is.null(version)) return("torp_ratings.parquet")
  if (!is.character(version) || length(version) != 1L || is.na(version)) {
    cli::cli_abort("{.arg version} must be a single non-NA string or NULL.")
  }
  if (!grepl("^v[0-9]+$", version)) {
    cli::cli_abort(c(
      "Unrecognised rating vintage {.val {version}}.",
      "i" = "Expected a label like {.val v1} or {.val v2}, or NULL for canonical."
    ))
  }
  sprintf("torp_ratings_%s.parquet", version)
}

#' Release asset stem (no extension) for a rating vintage
#'
#' \code{save_to_release()} takes a stem rather than a filename.
#'
#' @param version Vintage label, or NULL for canonical.
#' @return A character stem.
#' @keywords internal
.rating_vintage_stem <- function(version = NULL) {
  sub("\\.parquet$", "", .rating_vintage_file(version))
}

#' Release asset stem for any vintage-dependent rating artifact
#'
#' A vintage is a property of the **whole rating set**, not of one file.
#' `torp_ratings`, `player_game_ratings`, `player_season_ratings`, `psr` and
#' `team_ratings` all derive from the same constants, so publishing a v2
#' `torp_ratings` beside a v1 `player_game_ratings` would leave two artifacts
#' that disagree with each other and nothing recording why — the exact
#' inconsistency D-DEF3 exists to prevent. Versioning them partially is
#' therefore not a cheaper option, it is a broken one.
#'
#' @param base Asset stem, e.g. `"player_game_ratings_2025"`.
#' @param version Vintage label, or NULL for canonical.
#' @return The stem, suffixed when a vintage is given.
#' @keywords internal
.vintage_asset_stem <- function(base, version = NULL) {
  if (is.null(version)) return(base)
  .rating_vintage_file(version)   # validates the label, aborts on nonsense
  paste0(base, "_", version)
}

#' The constants that define the current rating vintage
#'
#' Generated from the live constants rather than hand-maintained. A
#' hand-written provenance block is the same class of object as the
#' hand-maintained lineup map that turned out to carry three wrong entries
#' (FABLE-DEFENDER-VALUE-PLAN §7.13) — it drifts silently from what the code
#' actually does, which defeats the entire point of recording it.
#'
#' @return A named list of the constants that determine rating values.
#' @keywords internal
.rating_defining_constants <- function() {
  list(
    TORP_EPR_WEIGHT = TORP_EPR_WEIGHT,
    EPV_POSITION_STANDARDISE = EPV_POSITION_STANDARDISE,
    EPV_STANDARDISE_CHANNELS = EPV_STANDARDISE_CHANNELS,
    LINEUP_POSITION_GROUP_MAP = as.list(LINEUP_POSITION_GROUP_MAP),
    EPR_DECAY = list(recv = EPR_DECAY_RECV, disp = EPR_DECAY_DISP,
                     spoil = EPR_DECAY_SPOIL, hitout = EPR_DECAY_HITOUT),
    EPR_PRIOR_GAMES = list(recv = EPR_PRIOR_GAMES_RECV, disp = EPR_PRIOR_GAMES_DISP,
                           spoil = EPR_PRIOR_GAMES_SPOIL, hitout = EPR_PRIOR_GAMES_HITOUT),
    EPR_PRIOR_RATE = list(recv = EPR_PRIOR_RATE_RECV, disp = EPR_PRIOR_RATE_DISP,
                          spoil = EPR_PRIOR_RATE_SPOIL, hitout = EPR_PRIOR_RATE_HITOUT)
  )
}

#' Build a rating-vintage manifest entry
#'
#' @param n_rows Row count of the published ratings frame.
#' @param version Vintage label of the CONSTANTS that produced this data.
#'   Defaults to \code{RATING_VINTAGE}.
#' @param file Filename actually written. Must be passed explicitly: the
#'   vintage label and the filename are independent. Regenerating canonical
#'   under new constants writes \code{torp_ratings.parquet} while the vintage
#'   is \code{"v2"}, and deriving one from the other mislabels the manifest.
#' @param generated_utc Timestamp; defaults to now. Injectable for tests.
#' @return A named list describing this vintage.
#' @keywords internal
#' @param defining_constants The constant values that produced this vintage.
#'   Defaults to the CURRENTLY LOADED constants, which is correct only when the
#'   running code is what generated the data. When preserving an OUTGOING
#'   vintage (`preserve_rating_vintage()`), the code has already moved on, so
#'   the live constants describe the *new* vintage — pass `NULL` there rather
#'   than stamping a definition the preserved data was not built under. A
#'   missing entry is a gap; a wrong one is a lie, and this manifest exists to
#'   be trusted.
.build_rating_vintage_entry <- function(n_rows, version = RATING_VINTAGE,
                                        file = NULL, generated_utc = NULL,
                                        defining_constants = .rating_defining_constants()) {
  if (is.null(generated_utc)) {
    generated_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }
  if (is.null(file)) file <- .rating_vintage_file(if (identical(version, "v1")) NULL else version)
  list(
    file = file,
    torp_version = as.character(utils::packageVersion("torp")),
    generated_utc = generated_utc,
    rows = as.integer(n_rows),
    defining_constants = defining_constants
  )
}

#' Merge a vintage entry into an existing ratings manifest
#'
#' Never changes which vintage is canonical — promotion is a deliberate,
#' separate act (see \code{RATING-VERSIONING-PLAN.md} §2.4). A pipeline run
#' that could silently promote would reintroduce exactly the in-place-overwrite
#' risk D-DEF3 exists to prevent.
#'
#' @param manifest Existing manifest list, or NULL to start one.
#' @param version Vintage label being written.
#' @param entry Output of \code{.build_rating_vintage_entry()}.
#' @return The updated manifest list.
#' @keywords internal
.merge_rating_manifest <- function(manifest, version, entry) {
  if (is.null(manifest)) manifest <- list(canonical = "v1", vintages = list())
  if (is.null(manifest$vintages)) manifest$vintages <- list()
  manifest$vintages[[version]] <- entry
  # canonical is deliberately left untouched
  manifest
}

#' Publish (or refresh) this vintage's entry in the ratings manifest
#'
#' Reads the live manifest, merges this vintage's entry and uploads the result.
#' Deliberately does NOT set \code{canonical} — see
#' \code{.merge_rating_manifest()}.
#'
#' @param n_rows Row count of the ratings frame just published.
#' @param version Vintage label of the constants that produced the data.
#'   Defaults to \code{RATING_VINTAGE}.
#' @param file Filename actually written (see
#'   \code{.build_rating_vintage_entry()}).
#' @param set_canonical If TRUE, record this vintage as canonical. Only ever
#'   TRUE when the run wrote \code{torp_ratings.parquet} itself; a run that
#'   publishes a candidate alongside must leave canonical alone.
#' @return Invisibly, the manifest that was uploaded.
#' @keywords internal
publish_ratings_manifest <- function(n_rows, version = RATING_VINTAGE,
                                     file = NULL, set_canonical = FALSE,
                                     defining_constants = .rating_defining_constants()) {
  entry <- .build_rating_vintage_entry(n_rows, version = version, file = file,
                                       defining_constants = defining_constants)
  manifest <- .merge_rating_manifest(read_ratings_manifest(), version, entry)
  if (isTRUE(set_canonical)) manifest$canonical <- version

  tf <- file.path(tempdir(), "ratings_manifest.json")
  writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = "null"), tf)
  piggyback::pb_upload(tf, repo = get_torp_data_repo(), tag = "ratings-data",
                       overwrite = TRUE)
  cli::cli_alert_success(
    "Published ratings_manifest.json (vintage {.val {version}}, canonical {.val {manifest$canonical}})"
  )
  invisible(manifest)
}

#' Preserve the current canonical ratings under a vintage label
#'
#' The one irreversible-by-omission step in the whole versioning scheme. Once
#' `run_ratings_pipeline.R` overwrites `torp_ratings.parquet` under new
#' constants, the previous vintage is gone unless a copy was taken first —
#' and D-DEF3's entire purpose is that a published number stays traceable to
#' the definition that produced it.
#'
#' Run this BEFORE regenerating when promoting a new vintage directly into
#' canonical (the "switch straight away" path, as opposed to publishing a
#' candidate alongside and promoting after a soak).
#'
#' @param label Vintage label to preserve the current canonical file as,
#'   e.g. `"v1"`.
#' @param repo Data repo. Defaults to `get_torp_data_repo()`.
#' @param dry_run If TRUE (default), report what would happen and upload
#'   nothing. **Deliberately defaults to TRUE**: this function writes to a
#'   release, and a function that publishes by default is one that publishes
#'   by accident.
#' @return Invisibly, a list describing the action.
#' @keywords internal
preserve_rating_vintage <- function(label, repo = get_torp_data_repo(),
                                    dry_run = TRUE) {
  target <- .rating_vintage_file(label)          # validates the label
  current <- .rating_vintage_file(NULL)

  # Force strict loading for every read in this function.
  #
  # Without this the abort below is decorative: outside VERSEBUS_STRICT=1,
  # parquet_from_url() deliberately does NOT rethrow non-404 failures (network
  # blip, CDN 5xx, auth, malformed parquet). It warns and returns a 0-row
  # data.table tagged skip_reason = "transient" (load_engines.R). So the
  # existence probe would see 0 rows, conclude "vintage absent, safe to write",
  # and overwrite a real preserved vintage -- precisely the failure this
  # function exists to prevent.
  #
  # That leniency is right for casual interactive/multi-season loads. It is
  # wrong here: this is the one irreversible-by-omission step in the scheme,
  # and it is invoked manually, so it does not inherit a pipeline's strict env.
  withr::local_envvar(c(VERSEBUS_STRICT = "1"))

  # A genuinely-absent vintage does NOT error: parquet_from_url_cached()
  # returns a 0-row frame on a confirmed 404, which the nrow() check below
  # handles. So anything that DOES throw here is unexpected -- network, auth,
  # a malformed parquet -- and mapping that to "absent, safe to write" would
  # invert the codebase's own rule (versebus.R: when classification is
  # uncertain the answer is transient/abort, never absent/overwrite). Given
  # this function exists to stop an irreversible overwrite, an unreadable
  # probe must abort, not wave us through.
  existing <- tryCatch(
    load_torp_ratings(version = label),
    error = function(e) {
      # NB: interpolate `target`, not a call to `.rating_vintage_file()` --
      # a dot-prefixed name inside cli's braces collides with its own
      # `{.val}`-style markup and hard-errors.
      cli::cli_abort(c(
        "Could not determine whether {.file {target}} already exists: {conditionMessage(e)}",
        "i" = "Refusing to proceed -- an unreadable probe cannot be treated as 'absent', and writing over a preserved vintage is irreversible."
      ), parent = e)
    }
  )
  # Second line of defence: even under strict mode, a 0-row result carrying a
  # non-"not_found" skip_reason means the probe degraded rather than confirmed
  # absence. Only a confirmed 404 licenses "safe to write".
  probe_skip <- attr(existing, "skip_reason")
  if (!is.null(probe_skip) && !identical(probe_skip, "not_found")) {
    cli::cli_abort(c(
      "Existence probe for {.file {target}} degraded ({.val {probe_skip}}) instead of confirming absence.",
      "i" = "Refusing to proceed -- only a confirmed 404 means the vintage is genuinely absent."
    ))
  }

  if (!is.null(existing) && nrow(existing) > 0) {
    cli::cli_abort(c(
      "{.file {target}} already exists in {.val ratings-data} ({nrow(existing)} rows).",
      "i" = "Refusing to overwrite a preserved vintage -- pick an unused label."
    ))
  }

  canon <- tryCatch(load_torp_ratings(), error = function(e) NULL)
  if (is.null(canon) || nrow(canon) == 0) {
    cli::cli_abort("Could not read the current canonical ratings; refusing to proceed.")
  }

  cli::cli_inform(c(
    "Preserve plan:",
    "*" = "copy {.file {current}} ({nrow(canon)} rows) -> {.file {target}}",
    "*" = "{.file {current}} itself is left untouched"
  ))
  if (dry_run) {
    cli::cli_alert_info("dry_run = TRUE -- nothing uploaded. Re-run with dry_run = FALSE to apply.")
    # Same keys as the applied path, so a programmatic caller reading
    # $manifest_recorded gets FALSE rather than NULL in dry-run.
    return(invisible(list(rows = nrow(canon), target = target, applied = FALSE,
                          manifest_recorded = FALSE)))
  }

  save_to_release(canon, .rating_vintage_stem(label), "ratings-data")

  # Verification failure must ABORT, not print. This function's whole purpose
  # is to make the next step (regenerating canonical) safe, so callers treat a
  # normal return as "the old vintage is preserved, go ahead". A cli_alert_danger
  # is a console print: not catchable, invisible to a scripted caller, and it
  # does not fail a CI exit code -- so the pipeline would proceed to overwrite
  # canonical on the strength of a preservation that did not actually work.
  verify <- tryCatch(load_torp_ratings(version = label), error = function(e) NULL)
  if (is.null(verify) || nrow(verify) != nrow(canon)) {
    cli::cli_abort(c(
      "Verification failed: {.file {target}} did not read back at {nrow(canon)} rows (got {if (is.null(verify)) 'unreadable' else nrow(verify)}).",
      "x" = "Do NOT regenerate canonical until this is resolved -- the previous vintage may not be preserved."
    ))
  }
  cli::cli_alert_success("Preserved {nrow(canon)} rows as {.file {target}}")

  # Record the preserved vintage in the manifest -- otherwise the file sits on
  # the release with nothing describing it, and the provenance trail D-DEF3
  # exists to provide does not cover the vintage just preserved.
  #
  # defining_constants = NULL deliberately: by the time an outgoing vintage is
  # preserved, the loaded constants are the INCOMING vintage's. Stamping those
  # against this data would misattribute it. `set_canonical = FALSE` because
  # preserving is never promotion.
  manifest_ok <- TRUE
  tryCatch(
    publish_ratings_manifest(n_rows = nrow(canon), version = label,
                             file = target, set_canonical = FALSE,
                             defining_constants = NULL),
    error = function(e) {
      manifest_ok <<- FALSE
      # Include the condition class: this plumbing is new, so on its first real
      # use a defect in it must be distinguishable from routine upload
      # flakiness rather than reading as the same "could not record" either way.
      cli::cli_warn(c(
        "Preserved {.file {target}} but could not record it in ratings_manifest.json: {conditionMessage(e)}",
        "i" = "Error class: {.val {paste(class(e), collapse = '/')}} -- a non-network class here suggests a defect in the manifest path, not flakiness.",
        "i" = "The data is safe; the provenance entry is missing. Re-run publish_ratings_manifest(n_rows = {nrow(canon)}, version = {.val {label}}, file = {.val {target}}, set_canonical = FALSE, defining_constants = NULL)."
      ))
    }
  )

  invisible(list(rows = nrow(canon), target = target, applied = TRUE,
                 manifest_recorded = manifest_ok))
}

#' Read the ratings manifest from the data release
#'
#' @return A manifest list, or NULL when the tag carries no manifest yet
#'   (which is the state of every release published before versioning existed).
#' @keywords internal
read_ratings_manifest <- function() {
  url <- paste0("https://github.com/", get_torp_data_repo(),
                "/releases/download/ratings-data/ratings_manifest.json")
  tryCatch(jsonlite::fromJSON(url, simplifyVector = FALSE),
           error = function(e) NULL)
}
