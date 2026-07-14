# versebus vendored helper — canonical copy: torpverse/torp/R/versebus.R
# VERSEBUS_VERSION below must match across verses; sync by file diff, e.g.:
#   diff torpverse/torp/R/versebus.R pannaverse/panna/R/versebus.R
# Pattern spec: C:\dev\ECOSYSTEM-FIX-PLAN.md §1. Repo-specific glue (repo/tag
# resolution, cache invalidation) stays OUT of this file — callers pass
# repo/tag explicitly and register hooks via options() (see vb_publish).
VERSEBUS_VERSION <- "1.0.0"

# Session state: one-time legacy warnings, manifest-seen memory (for the
# retry-once-on-momentary-absence rule), manifest fetch rate limiting.
.vb_state <- new.env(parent = emptyenv())

.vb_now_utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

.vb_generation_stamp <- function() {
  run_id <- Sys.getenv("GITHUB_RUN_ID", "")
  suffix <- if (nzchar(run_id)) paste0("-r", run_id) else
    paste0("-l", paste(sample(c(0:9, letters[1:6]), 6, replace = TRUE), collapse = ""))
  paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), suffix)
}

.vb_split_repo <- function(repo) {
  parts <- strsplit(repo, "/", fixed = TRUE)[[1]]
  if (length(parts) != 2L || !all(nzchar(parts))) {
    cli::cli_abort("repo must be 'owner/name', got {.val {repo}}",
                   class = c("vb_error", "vb_error_usage"))
  }
  list(owner = parts[[1]], name = parts[[2]])
}

.vb_abort <- function(msg, subclass, ..., .envir = parent.frame()) {
  cli::cli_abort(msg, class = c(subclass, "vb_error"), ..., .envir = .envir)
}

#' Classify an error from a release/network operation
#'
#' The cardinal versebus rule: when classification is uncertain, the answer is
#' "transient" (abort), never "absent" (overwrite). Only a positively
#' confirmed 404 on the tag/asset listing may classify as "absent".
#'
#' @param e a condition object
#' @return one of "absent", "transient"
#' @keywords internal
#' @export
vb_classify_error <- function(e) {
  cls <- class(e)
  if (any(cls == "vb_error_absent")) return("absent")
  if (any(cls %in% c("vb_error_transient", "vb_error_integrity", "vb_error_stale"))) {
    return("transient")
  }
  # gh errors carry http_error_<status> classes; trust them first.
  if (any(grepl("^http_error_404$", cls))) return("absent")
  if (any(grepl("^http_error_(5[0-9]{2}|429)$", cls))) return("transient")
  # piggyback/curl string fallback — DEFAULT IS TRANSIENT (fail-safe).
  "transient"
}

#' sha256 of a file
#' @keywords internal
#' @export
vb_sha256 <- function(path) {
  if (!file.exists(path)) {
    .vb_abort("Cannot hash {.path {path}}: file does not exist", "vb_error_integrity")
  }
  digest::digest(path, algo = "sha256", file = TRUE)
}

#' Build one manifest asset entry for a local file
#' @param rows optional row count for tabular assets (NULL for models etc.)
#' @keywords internal
#' @export
vb_asset_entry <- function(path, rows = NULL) {
  list(
    name = basename(path),
    sha256 = vb_sha256(path),
    bytes = as.numeric(file.size(path)),
    rows = if (is.null(rows)) NA_integer_ else as.integer(rows)
  )
}

#' Producer identity block for the manifest (GHA env or local)
#' @keywords internal
#' @export
vb_producer_info <- function() {
  list(
    repo = Sys.getenv("GITHUB_REPOSITORY", unset = "local"),
    workflow = Sys.getenv("GITHUB_WORKFLOW", unset = "interactive"),
    run_id = Sys.getenv("GITHUB_RUN_ID", unset = ""),
    run_attempt = Sys.getenv("GITHUB_RUN_ATTEMPT", unset = "")
  )
}

#' Write a bus_manifest.json to `path`
#' @param entries list of vb_asset_entry() results (the FULL tag contents)
#' @keywords internal
#' @export
vb_write_manifest <- function(entries, tag, path, producer = vb_producer_info(),
                              notes = "") {
  manifest <- list(
    schema_version = 1L,
    tag = tag,
    generation = .vb_generation_stamp(),
    produced_at_utc = .vb_now_utc(),
    producer = producer,
    assets = entries,
    notes = notes
  )
  vb_atomic_write(
    function(p) jsonlite::write_json(manifest, p, auto_unbox = TRUE, pretty = TRUE),
    path
  )
  invisible(manifest)
}

#' Atomic local write: tempfile in dest's own dir, then rename
#'
#' Same-directory rename is atomic on every filesystem we run on; writing to
#' tempdir() and renaming across devices is not. On any write failure the
#' destination is left untouched.
#' @keywords internal
#' @export
vb_atomic_write <- function(write_fn, dest) {
  tmp <- tempfile(pattern = paste0(".vb_", basename(dest), "_"),
                  tmpdir = dirname(dest))
  ok <- FALSE
  on.exit(if (!ok && file.exists(tmp)) unlink(tmp), add = TRUE)
  write_fn(tmp)
  if (!file.exists(tmp)) {
    .vb_abort("Atomic write produced no file for {.path {dest}}", "vb_error_integrity")
  }
  # Never unlink(dest) before the swap: file.rename() replaces an existing
  # destination in place without needing that, and if it fails (cross-device,
  # a transient lock) file.copy(overwrite=TRUE) still swaps the CONTENT
  # without ever leaving `dest` briefly missing. A pre-emptive unlink would
  # destroy the previous good file the moment the swap itself fails.
  if (!isTRUE(file.rename(tmp, dest)) && !isTRUE(file.copy(tmp, dest, overwrite = TRUE))) {
    .vb_abort("Atomic rename/copy failed for {.path {dest}}", "vb_error_integrity")
  }
  ok <- TRUE
  invisible(dest)
}

#' Row-count floor guard for read-modify-write upserts
#'
#' Call immediately before uploading an accumulated table over an existing
#' one. Shrinkage beyond `floor` means the "existing" read was partial or the
#' merge dropped history — abort rather than wipe.
#' @keywords internal
#' @export
vb_guard_accumulate <- function(existing_df, combined_df, floor = 0.9) {
  n_old <- nrow(existing_df)
  n_new <- nrow(combined_df)
  if (n_old > 0L && n_new < floor * n_old) {
    .vb_abort(
      c("Accumulate guard tripped: combined has {n_new} rows vs {n_old} existing
         (floor {floor * 100}%).",
        "x" = "Refusing to overwrite — the existing read was likely partial."),
      "vb_error_integrity"
    )
  }
  invisible(combined_df)
}

#' List release assets (uncached, typed errors)
#'
#' The positive-confirmation primitive. 404 on the TAG raises
#' `vb_error_absent`; anything else raises `vb_error_transient`.
#' @return data.frame(name, size, updated_at, id)
#' @keywords internal
#' @export
vb_list_assets <- function(repo, tag) {
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
  assets <- if (is.null(rel$assets)) list() else rel$assets
  data.frame(
    name = vapply(assets, function(a) a$name, character(1)),
    size = vapply(assets, function(a) as.numeric(a$size), numeric(1)),
    updated_at = vapply(assets, function(a) a$updated_at, character(1)),
    id = vapply(assets, function(a) as.numeric(a$id), numeric(1)),
    stringsAsFactors = FALSE
  )
}

#' Positively confirm an asset is absent from a release
#'
#' TRUE only when the asset list was fetched successfully AND the name is not
#' in it (or the tag itself is confirmed 404). A listing failure raises
#' `vb_error_transient` — callers MUST NOT catch that into a default. This is
#' THE mandatory guard before any "start fresh / overwrite full-history"
#' branch.
#' @keywords internal
#' @export
vb_confirm_absent <- function(repo, tag, name) {
  assets <- tryCatch(vb_list_assets(repo, tag), error = function(e) {
    if (vb_classify_error(e) == "absent") return(NULL)  # tag 404 => positively absent
    stop(e)
  })
  if (is.null(assets)) return(TRUE)
  !(name %in% assets$name)
}

#' Read a tag's bus_manifest.json (NULL + one-time warning when absent)
#'
#' Applies the momentary-absence rule: if this session has previously seen a
#' manifest on the tag and it now looks absent (piggyback delete-then-upload
#' window), retry once after 10 s before declaring legacy mode. A tag never
#' having a manifest is always legacy mode, never an error — `required` is
#' accepted for caller compatibility but does not abort on absence; a caller
#' that needs to refuse an *uncommitted* asset checks the returned manifest
#' itself (see `vb_download()`'s own require_manifest handling).
#' @keywords internal
#' @export
vb_read_manifest <- function(repo, tag, required = FALSE) {
  key <- paste0(repo, "@", tag)
  fetch <- function() {
    assets <- vb_list_assets(repo, tag)
    if (!("bus_manifest.json" %in% assets$name)) return(NULL)
    tmpdir <- tempfile("vb_manifest_")
    dir.create(tmpdir)
    on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
    piggyback::pb_download("bus_manifest.json", dest = tmpdir,
                           repo = repo, tag = tag, overwrite = TRUE)
    jsonlite::fromJSON(file.path(tmpdir, "bus_manifest.json"),
                       simplifyVector = TRUE, simplifyDataFrame = TRUE)
  }
  m <- tryCatch(fetch(), error = function(e) {
    if (vb_classify_error(e) == "absent") return(NULL)
    stop(e)
  })
  if (is.null(m) && isTRUE(.vb_state[[paste0("seen_", key)]])) {
    Sys.sleep(10)
    m <- tryCatch(fetch(), error = function(e) NULL)
  }
  if (is.null(m)) {
    # A tag with no manifest at all is a bootstrap/legacy state, not an
    # integrity failure — `required` (strict mode) only means "verify
    # sha256 against the manifest when one exists"; it must not treat
    # every not-yet-adopted tag as a hard error, or the very first read of
    # any legacy tag (reference-data, stadium_data, ...) permanently
    # deadlocks strict-mode callers, since nothing re-uploads those tags
    # to ever produce a manifest.
    warn_key <- paste0("warned_", key)
    if (!isTRUE(.vb_state[[warn_key]])) {
      cli::cli_warn("tag {.val {tag}} on {.val {repo}} has no bus_manifest.json — running unverified (legacy mode)")
      .vb_state[[warn_key]] <- TRUE
    }
    return(NULL)
  }
  .vb_state[[paste0("seen_", key)]] <- TRUE
  m
}

#' Previous manifest or NULL on first publish (transient errors propagate)
#' @keywords internal
#' @export
vb_read_prev_manifest <- function(repo, tag) {
  tryCatch(vb_read_manifest(repo, tag, required = FALSE), error = function(e) {
    if (vb_classify_error(e) == "absent") return(NULL)
    stop(e)
  })
}

.vb_manifest_entry_for <- function(manifest, name) {
  if (is.null(manifest) || is.null(manifest$assets)) return(NULL)
  a <- manifest$assets
  if (is.data.frame(a)) {
    hit <- a[a$name == name, , drop = FALSE]
    if (nrow(hit) == 0L) return(NULL)
    return(as.list(hit[1L, ]))
  }
  for (e in a) if (identical(e$name, name)) return(e)
  NULL
}

.vb_check_parquet_magic <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  head <- readBin(con, "raw", 4L)
  sz <- file.size(path)
  if (sz < 8L) return(FALSE)
  seek(con, sz - 4L)
  tail <- readBin(con, "raw", 4L)
  identical(rawToChar(head), "PAR1") && identical(rawToChar(tail), "PAR1")
}

#' Verified, atomic release-asset download
#'
#' Downloads to a tempfile in `dest`'s directory, verifies (parquet magic
#' bytes; sha256 vs manifest when available, size vs asset list otherwise),
#' then atomically renames into place and writes a `<dest>.sha256` sidecar.
#' ON ANY FAILURE the temp is deleted and a pre-existing `dest` is left
#' untouched — but it is NEVER silently served as a fallback: the typed error
#' propagates and the caller must opt in to "serve stale + warn" explicitly.
#' @param manifest pass a manifest to verify sha256; NULL fetches it
#'   (legacy mode when the tag has none)
#' @param require_manifest TRUE in CI/strict mode (VERSEBUS_STRICT=1)
#' @param max_age optional difftime; manifest older than this raises
#'   `vb_error_stale`
#' @keywords internal
#' @export
vb_download <- function(repo, tag, name, dest,
                        manifest = NULL,
                        require_manifest = isTRUE(Sys.getenv("VERSEBUS_STRICT") == "1"),
                        max_age = NULL) {
  if (is.null(manifest)) {
    manifest <- vb_read_manifest(repo, tag, required = require_manifest)
  }
  if (!is.null(manifest) && !is.null(max_age)) {
    produced <- as.POSIXct(manifest$produced_at_utc,
                           format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    if (!is.na(produced) && (Sys.time() - produced) > max_age) {
      .vb_abort("Manifest for {repo}@{tag} produced {.val {manifest$produced_at_utc}}
                 exceeds max_age", "vb_error_stale")
    }
  }
  entry <- .vb_manifest_entry_for(manifest, name)
  if (!is.null(manifest) && is.null(entry)) {
    cli::cli_warn("{.val {name}} is on {repo}@{tag} but not in bus_manifest.json — uncommitted asset")
    if (require_manifest) {
      .vb_abort("Refusing uncommitted asset {.val {name}} in strict mode",
                "vb_error_integrity")
    }
  }

  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  tmpdir <- tempfile(".vb_dl_", tmpdir = dirname(dest))
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  tryCatch(
    piggyback::pb_download(name, dest = tmpdir, repo = repo, tag = tag,
                           overwrite = TRUE),
    error = function(e) {
      .vb_abort("Download of {.val {name}} from {repo}@{tag} failed:
                 {conditionMessage(e)}",
                if (vb_classify_error(e) == "absent") "vb_error_absent"
                else "vb_error_transient",
                parent = e)
    }
  )
  tmp <- file.path(tmpdir, name)
  if (!file.exists(tmp) || file.size(tmp) == 0L) {
    .vb_abort("Download of {.val {name}} produced no/empty file", "vb_error_transient")
  }

  if (grepl("\\.parquet$", name, ignore.case = TRUE) &&
      !.vb_check_parquet_magic(tmp)) {
    .vb_abort("{.val {name}}: parquet magic bytes missing — corrupt download",
              "vb_error_integrity")
  }
  verify_by_size <- function() {
    listed <- tryCatch(vb_list_assets(repo, tag), error = function(e) NULL)
    if (!is.null(listed) && name %in% listed$name) {
      want <- listed$size[listed$name == name][1L]
      if (!isTRUE(all.equal(as.numeric(file.size(tmp)), want))) {
        .vb_abort("{.val {name}}: size {file.size(tmp)} != listed {want}",
                  "vb_error_integrity")
      }
    }
  }
  if (!is.null(entry)) {
    got <- vb_sha256(tmp)
    if (!identical(got, entry$sha256)) {
      # A stale manifest entry (upload succeeded, the LAST manifest publish
      # didn't) is indistinguishable here from real corruption, and is far
      # more common in practice — a hard abort would permanently brick the
      # asset until someone manually republishes the manifest. Downgrade to
      # a warning and fall back to the live-listing size check (the same
      # verification an unmanifested asset already gets); only a genuine
      # corruption signal (parquet magic bytes, checked above) still aborts.
      cli::cli_warn("{.val {name}}: sha256 mismatch vs manifest
                     (got {substr(got, 1, 12)}…, want {substr(entry$sha256, 1, 12)}…)
                     — manifest may be stale, verifying by size instead")
      verify_by_size()
    }
  } else {
    verify_by_size()
  }

  # Never unlink(dest) before the swap — see vb_atomic_write()'s comment;
  # same reasoning applies here to avoid destroying a good cached file if
  # the swap itself fails partway.
  if (!isTRUE(file.rename(tmp, dest)) && !isTRUE(file.copy(tmp, dest, overwrite = TRUE))) {
    .vb_abort("Could not move verified download into {.path {dest}}",
              "vb_error_integrity")
  }
  writeLines(vb_sha256(dest), paste0(dest, ".sha256"))
  invisible(dest)
}

#' Cache validity = sidecar sha matches the manifest entry
#'
#' Trusts the `.sha256` sidecar rather than rehashing `local_path` on every
#' call (rehashing a multi-GB model on every load would defeat the point of
#' caching). As a cheap corroborating check, a `local_path` modified more
#' recently than its sidecar is treated as invalid — the sidecar can only
#' describe content at-or-before its own write time.
#' @keywords internal
#' @export
vb_cache_validate <- function(local_path, manifest_entry) {
  if (is.null(manifest_entry)) return(FALSE)
  if (!file.exists(local_path)) return(FALSE)
  sidecar <- paste0(local_path, ".sha256")
  sha <- if (file.exists(sidecar)) {
    if (file.info(local_path)$mtime > file.info(sidecar)$mtime) return(FALSE)
    trimws(readLines(sidecar, n = 1L, warn = FALSE))
  } else {
    s <- vb_sha256(local_path)
    try(writeLines(s, sidecar), silent = TRUE)
    s
  }
  identical(sha, manifest_entry$sha256)
}

#' Current generation for a tag (manifest, else max asset updated_at)
#' @keywords internal
#' @export
vb_generation <- function(repo, tag) {
  m <- vb_read_prev_manifest(repo, tag)
  if (!is.null(m) && !is.null(m$generation)) return(m$generation)
  assets <- vb_list_assets(repo, tag)
  if (nrow(assets) == 0L) return(NA_character_)
  # per C:\dev\CLAUDE.md: asset updatedAt, never release createdAt
  max(assets$updated_at)
}

#' Manifest-last atomic publish (the versebus producer pattern)
#'
#' Ordered: hash → floor-check → upload data assets (bounded retries, collect
#' failures) → gate (any failure aborts BEFORE the manifest, so consumers
#' keep the last consistent snapshot) → verify live asset list → upload
#' bus_manifest.json LAST → fire the cache-invalidation hook
#' (`options(versebus.on_publish = function(repo, tag) ...)`).
#'
#' @param paths character vector of local files to upload
#' @param rows optional named integer vector: rows per basename
#' @param carry_forward merge with the previous manifest so partial publishes
#'   still describe the whole tag
#' @param min_row_frac optional floor vs previous manifest rows (e.g. 0.9)
#' @param dry_run build + return the manifest without uploading anything
#' @keywords internal
#' @export
vb_publish <- function(paths, repo, tag,
                       rows = NULL,
                       carry_forward = TRUE,
                       min_row_frac = NULL,
                       max_retries = 2,
                       dry_run = FALSE) {
  stopifnot(length(paths) > 0L)
  missing_files <- paths[!file.exists(paths)]
  if (length(missing_files) > 0L) {
    .vb_abort("vb_publish: missing local file(s): {.path {missing_files}}",
              "vb_error_integrity")
  }

  # 1. Hash first.
  entries <- lapply(paths, function(p) {
    row_n <- if (!is.null(rows) && basename(p) %in% names(rows)) rows[[basename(p)]] else NULL
    vb_asset_entry(p, rows = row_n)
  })
  names(entries) <- vapply(entries, `[[`, character(1), "name")

  prev <- if (carry_forward || !is.null(min_row_frac)) {
    vb_read_prev_manifest(repo, tag)
  } else NULL

  # 2. Row-count floor vs the previous manifest.
  if (!is.null(min_row_frac) && !is.null(prev)) {
    for (e in entries) {
      pe <- .vb_manifest_entry_for(prev, e$name)
      if (!is.null(pe) && !is.na(e$rows) && !is.null(pe$rows) && !is.na(pe$rows) &&
          e$rows < min_row_frac * as.numeric(pe$rows)) {
        .vb_abort("{.val {e$name}}: {e$rows} rows < {min_row_frac * 100}% of
                   previous {pe$rows} — refusing to publish", "vb_error_integrity")
      }
    }
  }

  if (dry_run) {
    cli::cli_alert_info("vb_publish dry run: {length(entries)} asset(s) for {repo}@{tag}")
    tmp <- tempfile(fileext = ".json")
    on.exit(unlink(tmp), add = TRUE)
    return(invisible(vb_write_manifest(.vb_merge_entries(prev, entries), tag, tmp)))
  }

  Sys.setenv(piggyback_cache_duration = 1)  # P6: never serve pre-upload listings

  # 3. Upload data assets with bounded exponential backoff; collect failures.
  failures <- character(0)
  for (p in paths) {
    ok <- FALSE
    for (attempt in seq_len(max_retries + 1L)) {
      ok <- tryCatch({
        piggyback::pb_upload(p, repo = repo, tag = tag, overwrite = TRUE)
        TRUE
      }, error = function(e) {
        cli::cli_alert_warning("upload attempt {attempt} failed for {basename(p)}: {conditionMessage(e)}")
        FALSE
      })
      if (ok) break
      if (attempt <= max_retries) Sys.sleep(c(2, 8)[min(attempt, 2L)])
    }
    if (!ok) failures <- c(failures, basename(p))
  }

  # 4. Gate: the manifest is NOT uploaded on any failure — the previous
  # manifest remains the commit record.
  if (length(failures) > 0L) {
    .vb_abort(c("vb_publish: {length(failures)} upload(s) failed for {repo}@{tag}:
                 {.val {failures}}",
                "x" = "bus_manifest.json NOT updated — consumers keep the last consistent snapshot."),
              "vb_error_transient")
  }

  # 5. Verify the live asset list agrees before committing.
  listed <- vb_list_assets(repo, tag)
  for (e in entries) {
    row <- listed[listed$name == e$name, , drop = FALSE]
    if (nrow(row) == 0L) {
      .vb_abort("Post-upload verify: {.val {e$name}} missing from {repo}@{tag}",
                "vb_error_transient")
    }
    if (!isTRUE(all.equal(row$size[1L], e$bytes))) {
      .vb_abort("Post-upload verify: {.val {e$name}} size {row$size[1L]} != local {e$bytes}",
                "vb_error_integrity")
    }
  }

  # 6. Manifest last (merge for partial publishes), with one retry.
  merged <- .vb_merge_entries(if (carry_forward) prev else NULL, entries)
  tmp <- file.path(tempdir(), "bus_manifest.json")
  manifest <- vb_write_manifest(merged, tag, tmp)
  up <- function() piggyback::pb_upload(tmp, repo = repo, tag = tag, overwrite = TRUE)
  tryCatch(up(), error = function(e) { Sys.sleep(5); up() })

  # 7. Own-write cache invalidation hook.
  hook <- getOption("versebus.on_publish")
  if (is.function(hook)) try(hook(repo, tag), silent = TRUE)

  cli::cli_alert_success("vb_publish: {length(entries)} asset(s) committed to {repo}@{tag} (generation {manifest$generation})")
  invisible(manifest)
}

# Merge new entries over a previous manifest's assets (carry forward the rest).
.vb_merge_entries <- function(prev, entries) {
  out <- entries
  if (!is.null(prev) && !is.null(prev$assets)) {
    a <- prev$assets
    prev_list <- if (is.data.frame(a)) {
      lapply(seq_len(nrow(a)), function(i) as.list(a[i, ]))
    } else a
    for (pe in prev_list) {
      nm <- pe$name
      if (!is.null(nm) && !(nm %in% names(out))) {
        out[[nm]] <- pe[intersect(names(pe), c("name", "sha256", "bytes", "rows"))]
      }
    }
  }
  unname(out)
}
