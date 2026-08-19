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
    # The engine leads, because it is the largest rating-defining choice there
    # is and it was absent from this list until 2026-08-18. The drift guard
    # exists to catch "a rating-defining constant changed without bumping
    # RATING_VINTAGE", and while these three were missing it could not see the
    # engine flip from v2 to v3 -- the one change most capable of altering every
    # row in the file. Found staging v3 for promotion.
    EPV_ENGINE = EPV_ENGINE,
    EPV3_CHANNELS = EPV3_CHANNELS,
    EPV3_POINTS_SCALE = as.list(EPV3_POINTS_SCALE),
    EPV_CONT_LOSS_ALLOC = EPV_CONT_LOSS_ALLOC,
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
#' @param defining_constants The constant values that produced this vintage.
#'   Defaults to the currently loaded constants. Pass \code{NULL} when
#'   recording a vintage the running code did NOT generate (preserving an
#'   outgoing vintage), so the entry carries an explicit gap rather than a
#'   definition the data was never built under. See
#'   \code{.build_rating_vintage_entry()}.
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

# Published-vintage vs deployed-code guard
# =========================================
# D-DEF3 stamps and reads a manifest, but until now nothing compared it to the
# running code before a production write. torp 2026-07-27/28: v2 was published
# as canonical while `main` still computed v1, and the daily pipeline rewrote
# 2026 rows with v1 logic into the v2 table -- silently, because publishing a
# vintage and deploying the code that produces it are separate acts and
# nothing checked they agreed. Design: docs/plans/FABLE-VINTAGE-GUARD-PLAN.md.

#' Recursively flatten a nested named list into single-level leaves
#'
#' Leaf names are dotted paths (e.g. `"LINEUP_POSITION_GROUP_MAP.CHF"`), which
#' is what makes a manifest entry that survived a JSON round-trip (see
#' \code{.round_trip_constants()}) comparable to the live constants leaf by
#' leaf, regardless of how deep the nesting is.
#'
#' @param x A (possibly nested) list.
#' @param prefix Internal recursion accumulator.
#' @return A flat named list; every element is a leaf value (which may itself
#'   be \code{NULL} -- a JSON \code{null} round-trips to an explicit NULL
#'   element, not a dropped one, because assignment here always uses
#'   single-bracket `[<-` with a length-1 list on the right, which is the one
#'   assignment form that does not delete a NULL from a list).
#' @keywords internal
.flatten_named_list <- function(x, prefix = "") {
  if (!is.list(x)) {
    out <- list(x)
    names(out) <- if (nzchar(prefix)) prefix else "value"
    return(out)
  }
  nms <- names(x)
  if (is.null(nms)) nms <- rep("", length(x))
  out <- list()
  for (i in seq_along(x)) {
    key <- nms[i]
    if (is.na(key) || !nzchar(key)) key <- as.character(i)
    child_prefix <- if (nzchar(prefix)) paste0(prefix, ".", key) else key
    child <- x[[i]]
    if (is.list(child)) {
      out <- c(out, .flatten_named_list(child, child_prefix))
    } else {
      out[child_prefix] <- list(child)
    }
  }
  out
}

#' Put a constants list through the exact JSON round-trip the manifest uses
#'
#' `read_ratings_manifest()` parses with `simplifyVector = FALSE`, and
#' `publish_ratings_manifest()` writes with `auto_unbox = TRUE, null = "null"`.
#' Comparing live constants to a manifest entry is only meaningful if BOTH
#' sides went through the identical coercion -- integers becoming doubles,
#' `NA_character_` becoming JSON `null` becoming R `NULL`, a named atomic
#' vector becoming a list of scalars. Skipping this step and comparing raw R
#' structures with `identical()` would false-alarm on every run.
#'
#' @param x A (possibly nested) list.
#' @return The same structure after `toJSON()` then `fromJSON()`.
#' @keywords internal
.round_trip_constants <- function(x) {
  raw <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
  jsonlite::fromJSON(raw, simplifyVector = FALSE)
}

#' Render one flattened leaf value as a stable, comparable string
#'
#' Two leaves that already went through the SAME round-trip (see
#' `.round_trip_constants()`) are equal iff their JSON text is equal --
#' `digits = NA` keeps full numeric precision, and `NULL` is handled
#' explicitly because `toJSON(NULL)` renders `"{}"`, not `"null"`.
#'
#' @param v A leaf value.
#' @return A length-1 character string.
#' @keywords internal
.normalise_constant_leaf <- function(v) {
  if (is.null(v)) return("null")
  as.character(jsonlite::toJSON(v, auto_unbox = TRUE, null = "null", digits = NA))
}

#' Diff a manifest vintage's `defining_constants` against the live constants
#'
#' Both sides are flattened and every leaf normalised through the same JSON
#' text form, so a leaf present on only one side reports as `"<absent>"`
#' rather than crashing, and a leaf whose VALUE differs (a decay constant
#' edited without a vintage bump) is reported by name with both values.
#'
#' @param manifest_constants The canonical vintage's `defining_constants`
#'   entry, already JSON-round-tripped by virtue of having come from
#'   `read_ratings_manifest()` (or an equivalently round-tripped test fixture).
#' @param live_constants The currently loaded constants. Defaults to
#'   `.rating_defining_constants()`; injectable for tests.
#' @return A named list of differing leaves, each `list(manifest = ..., live = ...)`.
#'   Empty when every leaf agrees.
#' @keywords internal
.diff_defining_constants <- function(manifest_constants,
                                     live_constants = .rating_defining_constants()) {
  live_rt <- .round_trip_constants(live_constants)
  manifest_flat <- .flatten_named_list(manifest_constants)
  live_flat <- .flatten_named_list(live_rt)
  all_names <- union(names(manifest_flat), names(live_flat))

  diffs <- list()
  for (nm in all_names) {
    mv <- if (nm %in% names(manifest_flat)) .normalise_constant_leaf(manifest_flat[[nm]]) else "<absent>"
    lv <- if (nm %in% names(live_flat)) .normalise_constant_leaf(live_flat[[nm]]) else "<absent>"
    if (!identical(mv, lv)) diffs[[nm]] <- list(manifest = mv, live = lv)
  }
  diffs
}

#' Refuse a ratings write when the running code disagrees with what is
#' published as canonical
#'
#' The border check `preserve_rating_vintage()` and `publish_ratings_manifest()`
#' set up but never enforced: this is the choke point that would have stopped
#' the 2026-07-27/28 incident on day one. Three checks, each a distinct failure
#' mode:
#'
#' 1. The manifest itself must be readable (or the run must be non-strict and
#'    accept the grandfather case of a pre-manifest release).
#' 2. The manifest's `canonical` label must match the running code's
#'    `RATING_VINTAGE` -- a mismatch means a write would silently relabel rows
#'    (incident 1, exactly).
#' 3. The canonical vintage's recorded `defining_constants` must match the
#'    live constants after a JSON round-trip -- catching the subtler failure
#'    where a rating-defining constant was edited without bumping
#'    `RATING_VINTAGE`, so the label still matches while the maths changed.
#'
#' @param strict If TRUE, an unreadable manifest or an undefined canonical
#'   vintage aborts rather than warning. Defaults to `VERSEBUS_STRICT=1` via
#'   `.strict_mode()`, matching every other pipeline entry point's convention
#'   (versebus §1.5). Until 2026-08-11 this tested `nzchar()` instead, so it
#'   was the one site where `VERSEBUS_STRICT="0"` still went strict.
#'   Production call sites pass `TRUE` explicitly regardless of the
#'   environment.
#' @param manifest The ratings manifest to check against. Defaults to
#'   `read_ratings_manifest()`; injectable so this function's tests run
#'   offline.
#' @param candidate Vintage label when the run is writing a CANDIDATE vintage
#'   (`torp_ratings_<label>.parquet`) rather than canonical, or `NULL` for a
#'   canonical write. A candidate never touches canonical, so drift from
#'   canonical's published constants is not a reason to refuse it -- checking it
#'   anyway made this guard block the exact remedy its own error message
#'   recommends. Still refused for a candidate: an unreadable manifest, and a
#'   label equal to the canonical one, which is a canonical write wearing a
#'   candidate flag.
#' @return Invisibly, `list(aligned = TRUE, canonical = canon)` on success, or
#'   `list(aligned = NA)` / `list(aligned = NA, canonical = canon)` when a
#'   non-strict run grandfathers a gap it could not verify.
#' @keywords internal
check_vintage_alignment <- function(strict = .strict_mode(),
                                    manifest = read_ratings_manifest(),
                                    candidate = NULL) {
  branch <- Sys.getenv("GITHUB_REF_NAME", "local")

  if (is.null(manifest)) {
    if (isTRUE(strict)) {
      cli::cli_abort(c(
        "ratings_manifest.json is unreadable or absent.",
        "x" = "An unreadable manifest cannot license a production write (branch {.val {branch}})."
      ), class = "torp_error_vintage_manifest_unreadable")
    }
    cli::cli_warn(c(
      "ratings_manifest.json is unreadable or absent.",
      "i" = "Grandfathering a pre-manifest release -- proceeding without an alignment check (branch {.val {branch}})."
    ))
    return(invisible(list(aligned = NA)))
  }

  # CANDIDATE WRITES ARE A DIFFERENT QUESTION. This check exists to stop a run
  # writing CANONICAL ratings with constants that disagree with what canonical
  # was published as -- the 2026-07-27/28 incident. A candidate vintage writes
  # torp_ratings_<label>.parquet and never touches canonical, so drift from
  # canonical is not a reason to refuse it. Refusing it made this guard block the
  # exact remedy its own mismatch error recommends ('publish it as a candidate
  # vintage first'), so a constants change could never be staged at all
  # (found 2026-08-18 staging EPV v3).
  #
  # Still enforced for a candidate: the manifest must be readable (above), and
  # the label must NOT be the canonical one -- a canonical write wearing a
  # candidate label is precisely what must never be waved through.
  if (!is.null(candidate)) {
    if (identical(candidate, manifest$canonical)) {
      cli::cli_abort(c(
        "Refusing to write vintage {.val {candidate}} as a CANDIDATE: it is the canonical vintage.",
        "x" = "That is a canonical write wearing a candidate label (branch {.val {branch}})."
      ), class = "torp_error_vintage_candidate_is_canonical")
    }
    return(invisible(list(aligned = NA, canonical = manifest$canonical,
                          candidate = candidate)))
  }

  canon <- manifest$canonical
  if (!identical(canon, RATING_VINTAGE)) {
    cli::cli_abort(c(
      "Deployed code computes rating vintage {.val {RATING_VINTAGE}}, but the published manifest's canonical vintage is {.val {canon}}.",
      "x" = "Writing now would silently relabel {.val {canon}}-labelled rows with {.val {RATING_VINTAGE}} logic (branch {.val {branch}}) -- this is the 2026-07-27/28 incident.",
      "i" = "Either publish {.val {RATING_VINTAGE}} as a candidate vintage first, or fix RATING_VINTAGE to match {.val {canon}}, before this run proceeds."
    ), class = "torp_error_vintage_mismatch")
  }

  canon_entry <- manifest$vintages[[canon]]
  canon_constants <- if (is.null(canon_entry)) NULL else canon_entry$defining_constants
  if (is.null(canon_constants)) {
    if (isTRUE(strict)) {
      cli::cli_abort(c(
        "The manifest's canonical vintage {.val {canon}} has no recorded defining_constants.",
        "x" = "Canonical-with-no-definition is a provenance gap, not a licence to write (branch {.val {branch}})."
      ), class = "torp_error_vintage_undefined")
    }
    cli::cli_warn(c(
      "The manifest's canonical vintage {.val {canon}} has no recorded defining_constants.",
      "i" = "Proceeding without a defining-constants check (branch {.val {branch}})."
    ))
    return(invisible(list(aligned = NA, canonical = canon)))
  }

  diffs <- .diff_defining_constants(canon_constants)
  if (length(diffs) > 0) {
    lines <- vapply(names(diffs), function(nm) {
      sprintf("%s: manifest %s vs live %s", nm, diffs[[nm]]$manifest, diffs[[nm]]$live)
    }, character(1))
    cli::cli_abort(c(
      "Rating-defining constants have drifted from vintage {.val {canon}}'s published definition without a vintage bump (branch {.val {branch}}).",
      stats::setNames(lines, rep("x", length(lines)))
    ), class = "torp_error_vintage_constants_drift")
  }

  invisible(list(aligned = TRUE, canonical = canon))
}
