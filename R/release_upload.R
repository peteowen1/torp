# Release-asset writes that never delete before they replace
# ============================================================================
#
# `piggyback::pb_upload(overwrite = TRUE)` -- and the default
# `overwrite = "use_timestamps"` whenever the local file is newer -- DELETES
# the existing asset, then uploads. If the upload then fails, the release is
# left without the file. On 2026-09-17 that emptied wheather's cache release
# for three weeks. Here it is worse: the accumulating writers (locked
# predictions, injury history, chains/pbp, ratings) read their asset back, and
# a missing one takes the "first run of the season" branch -- publishing a
# cut-down file over the history. The guard meant to stop that,
# `vb_confirm_absent()`, PASSES after a failed delete-then-upload, because the
# delete itself made the asset absent.
#
# piggyback 0.1.5 also does not raise when the upload POST fails: it retries
# and then only warns (and only with progress on). So "pb_upload() returned"
# has never meant "the file is on the release"; the listing decides here.
#
# Audit: vault/plans/CLOBBER-AUDIT-2026-10-08.md. Same scheme as pannadata's
# scripts/versebus.sh (vb_sh_safe_upload / vb_sh_restore), so the temp names
# match across verses.

RELEASE_TMP_PREFIX <- "vbnew-"

#' List a release's assets including upload state
#'
#' `vb_list_assets()` (shared, vendored) has no `state` column, and a
#' half-finished upload ("starter") must not count as present.
#' @return data.frame(id, name, size, state)
#' @keywords internal
.release_assets_with_state <- function(repo, tag) {
  r <- .vb_split_repo(repo)
  rel <- tryCatch(
    gh::gh("GET /repos/{owner}/{repo}/releases/tags/{tag}",
           owner = r$owner, repo = r$name, tag = tag),
    error = function(e) {
      if (vb_classify_error(e) == "absent") {
        .vb_abort("Release tag {.val {tag}} not found on {.val {repo}}",
                  "vb_error_absent", parent = e)
      }
      .vb_abort("Could not list assets for {repo}@{tag}: {conditionMessage(e)}",
                "vb_error_transient", parent = e)
    }
  )
  a <- if (is.null(rel$assets)) list() else rel$assets
  data.frame(
    id = vapply(a, function(x) as.numeric(x$id), numeric(1)),
    name = vapply(a, function(x) x$name, character(1)),
    size = vapply(a, function(x) as.numeric(x$size), numeric(1)),
    state = vapply(a, function(x) if (is.null(x$state)) NA_character_ else x$state, character(1)),
    stringsAsFactors = FALSE
  )
}

# Real data assets on a tag: not the manifest, not temp copies, not `exclude`.
.release_data_assets <- function(assets, exclude = character()) {
  n <- assets$name
  n[!startsWith(n, RELEASE_TMP_PREFIX) & n != "bus_manifest.json" & !(n %in% exclude)]
}

# Rows that are a temp copy of `name` left by an upload whose final rename
# never happened.
.release_tmp_copies <- function(assets, name) {
  assets[startsWith(assets$name, RELEASE_TMP_PREFIX) &
           endsWith(assets$name, paste0("--", name)), , drop = FALSE]
}

#' Upload a file to a release without ever deleting the old asset first
#'
#' Uploads under a temporary asset name, waits until the listing shows it in
#' state `uploaded` at the local byte size, and only then deletes the old
#' asset (plus temp copies stranded by earlier runs) and renames the new one.
#' A failure before the delete leaves the old asset untouched. A failure of
#' the final rename leaves the data on the release under its temp name, which
#' [confirm_fresh_start()] refuses to treat as absent.
#'
#' @param path Local file to upload.
#' @param repo "owner/name".
#' @param tag Release tag (must exist).
#' @param name Asset name to publish as.
#' @param poll_delays Seconds to wait between listing polls for the upload.
#' @return Invisible asset id.
#' @keywords internal
safe_release_upload <- function(path, repo, tag, name = basename(path),
                                poll_delays = c(2, 3, 5, 10, 15, 30)) {
  size <- as.numeric(file.size(path))
  if (is.na(size)) cli::cli_abort("safe_release_upload: {.file {path}} does not exist")
  r <- .vb_split_repo(repo)
  # Unique per call from the pid and a microsecond clock -- not sample.int(),
  # which would advance .Random.seed and change the draws of a seeded
  # simulation that uploads mid-run.
  tmp_name <- sprintf("%s%s-%s-%d%s--%s", RELEASE_TMP_PREFIX,
                      Sys.getenv("GITHUB_RUN_ID", "local"),
                      Sys.getenv("GITHUB_RUN_ATTEMPT", "0"),
                      Sys.getpid(), sprintf("%.0f", as.numeric(Sys.time()) * 1e6), name)

  # Never serve piggyback a pre-upload listing (same as vb_publish()).
  prev_cache <- Sys.getenv("piggyback_cache_duration", unset = NA)
  Sys.setenv(piggyback_cache_duration = 1)
  on.exit({
    if (is.na(prev_cache)) Sys.unsetenv("piggyback_cache_duration")
    else Sys.setenv(piggyback_cache_duration = prev_cache)
  }, add = TRUE)

  # 1. A fresh name: there is nothing to delete, so nothing can be lost here.
  tryCatch(
    piggyback::pb_upload(path, repo = repo, tag = tag, name = tmp_name,
                         overwrite = FALSE, show_progress = FALSE),
    error = function(e) {
      .vb_abort("Upload of {.val {name}} (as {.val {tmp_name}}) failed; the existing asset is untouched: {conditionMessage(e)}",
                "vb_error_transient", parent = e)
    }
  )

  # 2. The listing, not pb_upload()'s return, decides whether it landed. It
  #    can lag an upload by tens of seconds (torpdata#74), hence the polling.
  new_id <- NA_real_
  assets <- NULL
  last_err <- "none"
  for (d in c(0, poll_delays)) {
    if (d > 0) Sys.sleep(d)
    assets <- tryCatch(.release_assets_with_state(repo, tag), error = function(e) {
      last_err <<- conditionMessage(e)
      NULL
    })
    if (is.null(assets)) next
    hit <- assets[assets$name == tmp_name & assets$state %in% "uploaded" &
                    assets$size == size, , drop = FALSE]
    if (nrow(hit) > 0L) {
      new_id <- hit$id[1L]
      break
    }
  }
  if (is.na(new_id)) {
    .vb_abort("{.val {tmp_name}} never listed as uploaded at {size} bytes (last listing error: {last_err}); the existing {.val {name}} is untouched",
              "vb_error_transient")
  }

  # 3. Only now remove the old asset and any stranded temp copies of it. A
  #    failed delete of the real name stops here: the rename would collide.
  stale <- .release_tmp_copies(assets, name)
  old <- assets[assets$name == name |
                  (assets$id %in% stale$id & assets$name != tmp_name), , drop = FALSE]
  for (i in seq_len(nrow(old))) {
    err <- tryCatch({
      gh::gh("DELETE /repos/{owner}/{repo}/releases/assets/{id}",
             owner = r$owner, repo = r$name, id = format(old$id[i], scientific = FALSE))
      NULL
    }, error = function(e) e)
    if (!is.null(err)) {
      if (identical(old$name[i], name)) {
        .vb_abort("Could not delete the old {.val {name}} ({conditionMessage(err)}); the new copy stays on the release as {.val {tmp_name}}",
                  "vb_error_transient", parent = err)
      }
      cli::cli_warn("Could not delete stale temp asset {.val {old$name[i]}}: {conditionMessage(err)}")
    }
  }

  # 4. Rename into place.
  tryCatch(
    .vb_retry(function() {
      gh::gh("PATCH /repos/{owner}/{repo}/releases/assets/{id}",
             owner = r$owner, repo = r$name, id = format(new_id, scientific = FALSE), name = name)
    }, times = 3L, delays = c(5, 10)),
    error = function(e) {
      .vb_abort("Rename of {.val {tmp_name}} to {.val {name}} failed after the old asset was deleted; the data is on the release as {.val {tmp_name}}: {conditionMessage(e)}",
                "vb_error_integrity", parent = e)
    }
  )
  invisible(new_id)
}

#' Confirm an accumulating asset may be started fresh
#'
#' The guard every "first run / start fresh" branch must pass before
#' publishing a file that is normally read back and appended to. TRUE only
#' when the asset is absent, no temp copy of it is stranded on the release,
#' and the tag's `bus_manifest.json` has never listed it. An asset the
#' manifest lists but the release lacks was LOST, not never-made, and starting
#' fresh would publish a cut-down file over its history -- so that aborts
#' with `vb_error_integrity`, as does a stranded temp copy.
#'
#' Wraps [vb_confirm_absent()] (shared, vendored) rather than changing it.
#' To deliberately rebuild a lost asset from scratch, set the environment
#' variable `TORP_ALLOW_FRESH_START` to its asset name (comma-separate several)
#' or to `1` for all.
#'
#' @param repo "owner/name".
#' @param tag Release tag.
#' @param name Asset name, including extension.
#' @return TRUE when safe to start fresh, FALSE when the asset is present.
#'   Listing and manifest read failures propagate.
#' @keywords internal
confirm_fresh_start <- function(repo, tag, name) {
  if (!isTRUE(vb_confirm_absent(repo, tag, name))) return(FALSE)

  allowed <- trimws(strsplit(Sys.getenv("TORP_ALLOW_FRESH_START", ""), ",")[[1]])
  if ("1" %in% allowed || name %in% allowed) {
    cli::cli_alert_warning("TORP_ALLOW_FRESH_START set: starting {.val {name}} fresh without the lost-asset checks")
    return(TRUE)
  }

  assets <- tryCatch(.release_assets_with_state(repo, tag), vb_error_absent = function(e) NULL)
  if (is.null(assets)) return(TRUE)  # the tag itself does not exist yet

  stranded <- .release_tmp_copies(assets, name)$name
  if (length(stranded) > 0L) {
    stranded <- stranded[1L]
    .vb_abort(c("{.val {name}} is on {repo}@{tag} only as an unswapped upload ({.val {stranded}}).",
                "i" = "Rename it back to {.val {name}} on the release; do not start fresh."),
              "vb_error_integrity")
  }

  # (A half-uploaded asset under its real name never gets this far:
  # vb_confirm_absent() lists it as present.)
  manifest <- vb_read_prev_manifest(repo, tag)
  if (is.null(manifest)) {
    # No commit record at all. On a tag that already holds data that is not
    # "new", it is a lost manifest (or one stranded under a temp name), and
    # the manifest was the only evidence of what else used to be here.
    others <- .release_data_assets(assets, exclude = name)
    if (length(others) > 0L) {
      .vb_abort(c("{repo}@{tag} holds {length(others)} data asset{?s} but no bus_manifest.json, so a missing {.val {name}} cannot be told apart from a new one.",
                  "i" = "Restore bus_manifest.json, or set TORP_ALLOW_FRESH_START={name} if {.val {name}} is genuinely new."),
                "vb_error_integrity")
    }
    return(TRUE)
  }
  if (!is.null(.vb_manifest_entry_for(manifest, name))) {
    .vb_abort(c("{.val {name}} is listed in {repo}@{tag}'s bus_manifest.json but missing from the release: it was LOST, not never made.",
                "x" = "Starting fresh would publish a cut-down file over its history.",
                "i" = "Restore it, or set TORP_ALLOW_FRESH_START={name} to rebuild it from scratch on purpose."),
              "vb_error_integrity")
  }
  TRUE
}
