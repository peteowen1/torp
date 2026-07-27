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
.build_rating_vintage_entry <- function(n_rows, version = RATING_VINTAGE,
                                        file = NULL, generated_utc = NULL) {
  if (is.null(generated_utc)) {
    generated_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }
  if (is.null(file)) file <- .rating_vintage_file(if (identical(version, "v1")) NULL else version)
  list(
    file = file,
    torp_version = as.character(utils::packageVersion("torp")),
    generated_utc = generated_utc,
    rows = as.integer(n_rows),
    defining_constants = .rating_defining_constants()
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
                                     file = NULL, set_canonical = FALSE) {
  entry <- .build_rating_vintage_entry(n_rows, version = version, file = file)
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

  existing <- tryCatch(load_torp_ratings(version = label),
                       error = function(e) NULL)
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
    return(invisible(list(rows = nrow(canon), target = target, applied = FALSE)))
  }

  save_to_release(canon, .rating_vintage_stem(label), "ratings-data")
  verify <- tryCatch(load_torp_ratings(version = label), error = function(e) NULL)
  if (is.null(verify) || nrow(verify) != nrow(canon)) {
    cli::cli_alert_danger(
      "Verification failed: {.file {target}} did not read back at {nrow(canon)} rows. \\
       Do NOT regenerate canonical until this is resolved.")
  } else {
    cli::cli_alert_success("Preserved {nrow(canon)} rows as {.file {target}}")
  }
  invisible(list(rows = nrow(canon), target = target, applied = TRUE))
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
