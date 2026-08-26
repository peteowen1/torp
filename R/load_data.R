#' Save a Data Frame to a GitHub Release via Piggyback
#'
#' @description This function is intended for internal use and may be unexported in a future release.
#' Saves a data frame as a `.parquet` file and uploads it to a GitHub release using the `piggyback` package.
#'
#' @param df A data frame to save.
#' @param file_name A string for the file name (without extension).
#' @param release_tag The GitHub release tag to associate with the uploaded file.
#' @param also_csv Logical. If TRUE, also upload a `.csv` copy alongside parquet.
#' @param prev_rows_floor Optional numeric in (0, 1]. When set, aborts BEFORE
#'   uploading if `nrow(df)` is under this fraction of the row count recorded
#'   for this asset in the tag's `bus_manifest.json` (if any). Best-effort:
#'   a manifest read failure or absent manifest never blocks the upload --
#'   this is a secondary safety net, not the primary accumulate guard (use
#'   [vb_guard_accumulate()] against the actually-loaded `existing` data for
#'   that).
#'
#' @return No return value. Used for side effects (file upload).
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' my_df <- data.frame(x = 1:3)
#' save_to_release(my_df, "my_data", "v1.0.0")
#' save_to_release(my_df, "my_data", "v1.0.0", also_csv = TRUE)
#' }
save_to_release <- function(df, file_name, release_tag, also_csv = FALSE, prev_rows_floor = NULL) {
  rlang::check_installed("piggyback", version = "0.1.4", reason = "to upload data to GitHub releases")
  f_name <- paste0(file_name, ".parquet")
  repo <- get_torp_data_repo()
  tf <- tempfile(fileext = ".parquet")
  on.exit(unlink(tf), add = TRUE)

  # Normalise team name values to canonical forms before saving
  if (nrow(df) > 0) df <- .normalise_team_values(df)

  # T12: optional manifest-backed row-count floor, checked before spending an
  # upload attempt. Best-effort -- never blocks on a manifest read failure.
  if (!is.null(prev_rows_floor)) {
    # Swallowing this silently means "floor checked and passed" and "floor never
    # ran" look identical in the log. `vb_read_prev_manifest()` deliberately
    # re-raises anything not classified "absent", so a real fault (auth, a parse
    # bug, a transient) would otherwise take this second-opinion guard dark on
    # exactly the production writes that now depend on it. Still non-fatal: a
    # manifest we cannot read must not block a release.
    prev_manifest <- tryCatch(vb_read_prev_manifest(repo, release_tag), error = function(e) {
      cli::cli_warn(c(
        "prev_rows_floor: could not read the previous manifest for {repo}@{release_tag} ({conditionMessage(e)}).",
        "!" = "The row-count floor did NOT run for {.val {f_name}} -- proceeding unguarded."
      ))
      NULL
    })
    prev_entry <- if (!is.null(prev_manifest)) .vb_manifest_entry_for(prev_manifest, f_name) else NULL
    if (!is.null(prev_entry) && !is.null(prev_entry$rows) && !is.na(prev_entry$rows)) {
      prev_rows <- as.numeric(prev_entry$rows)
      if (prev_rows > 0 && nrow(df) < prev_rows_floor * prev_rows) {
        cli::cli_abort(
          "save_to_release: {.val {f_name}} has {nrow(df)} rows, under {prev_rows_floor * 100}% of the {prev_rows} rows recorded in bus_manifest.json - refusing to upload",
          class = c("vb_error_integrity", "vb_error")
        )
      }
    }
  }

  tryCatch(
    arrow::write_parquet(df, tf),
    error = function(e) {
      cli::cli_abort("Failed to write parquet file {.val {f_name}}: {conditionMessage(e)}")
    }
  )

  # Stamped before the upload so the post-upload verify can tell a listing that
  # simply has not caught up yet (updated_at older than our own write) from one
  # that reflects a DIFFERENT, later write (updated_at newer than ours) -- see
  # the verify block below. Small tolerance applied there for clock skew between
  # this runner and GitHub.
  upload_started_at <- as.POSIXct(Sys.time(), tz = "UTC")

  tryCatch(
    piggyback::pb_upload(tf,
                         repo = repo,
                         tag = release_tag,
                         name = f_name),
    error = function(e) {
      # Retry once on 404/422 — piggyback's delete-then-upload can fail if the
      # asset ID changed between listing and deletion (concurrent upload or
      # stale cache), or if a concurrent create raced us (422). A short
      # backoff then a fresh attempt re-fetches the asset list.
      if (grepl("404|422", conditionMessage(e))) {
        cli::cli_warn("Upload error for {.val {f_name}}, retrying once...")
        Sys.sleep(2)
        # Re-stamp: it is THIS attempt whose result the verify below checks, so
        # the staleness cutoff must reference it and not the abandoned first
        # attempt. Matters most precisely here -- the 404/422 path is the
        # concurrent-upload case the staleness check exists to disambiguate.
        upload_started_at <<- as.POSIXct(Sys.time(), tz = "UTC")
        tryCatch(
          piggyback::pb_upload(tf,
                               repo = repo,
                               tag = release_tag,
                               name = f_name),
          error = function(e2) {
            cli::cli_abort("Failed to upload {.val {f_name}} to release {.val {release_tag}} (after retry): {conditionMessage(e2)}")
          }
        )
      } else {
        cli::cli_abort("Failed to upload {.val {f_name}} to release {.val {release_tag}}: {conditionMessage(e)}")
      }
    }
  )

  # pb_upload() above already returned success -- that's the actual
  # correctness signal. invalidate_release_cache() and the manifest publish
  # below must not be gated on the post-upload verify further down: that
  # verify makes its own network call (vb_list_assets) and a TRANSIENT
  # failure there says nothing about whether the real upload worked, but
  # used to abort save_to_release() entirely, silently skipping both steps
  # even though the data was safely on the release.
  invalidate_release_cache(release_tag)

  # T13: keep bus_manifest.json current for this tag (manifest-last, best
  # effort). The data upload above already succeeded; a manifest publish
  # failure must not undo that or fail the caller -- absent consumers just
  # stay in legacy mode (versebus §1.2) until the next successful publish.
  tryCatch(
    .publish_bus_manifest(release_tag, f_name, tf, rows = if (is.data.frame(df)) nrow(df) else NA_integer_),
    error = function(e) {
      cli::cli_warn("Could not update bus_manifest.json for {.val {release_tag}}: {conditionMessage(e)}")
    }
  )

  # T12: post-upload verify -- confirm the asset is actually on the release
  # with the size we just wrote, via an uncached listing call. Catches races
  # / silent-drop scenarios that a 2xx from pb_upload alone wouldn't surface.
  #
  # torpdata#74, THIRD iteration (2026-07-28). The previous two both assumed
  # "stale listing lag" and responded by widening the retry budget (default
  # ~7s, then 5 attempts / ~20s). Neither worked: the release failed 33 times
  # between 2026-07-14 and 2026-07-27. Re-reading the actual failures showed
  # the earlier diagnosis had the sign backwards:
  #
  #   Post-upload verify: "pbp_data_2026_all.parquet"
  #     listed size 69283638 != local 69283389
  #
  # The listed size is LARGER than local (the previous, bigger asset), where
  # the earlier fix note recorded it as consistently SMALLER. And it is
  # byte-identical on all five attempts -- a lagging listing converges as you
  # wait, a stale read of an older asset never does, which is exactly why more
  # retries changed nothing.
  #
  # FOURTH iteration (2026-08-09). The third kept "listed SMALLER than local"
  # fatal, on the reasoning that truncation makes a file short and that
  # direction is therefore unambiguous. It is not, and the release failed 5 of
  # 8 runs on 2026-08-08 proving it:
  #
  #   Post-upload verify: "pbp_data_2026_all.parquet"
  #     listed size 73694060 < local 74776757 -- possible truncated upload
  #
  # That run was "Updating pbp_data_2026_all with round 22". 73694060 is the
  # PRE-round-22 asset; the live asset now reads 75153518, i.e. every one of
  # those writes landed. The season file GROWS on each round, so a listing that
  # has not caught up serves the previous, SMALLER asset -- the same lagging
  # read the third iteration diagnosed, arriving from the other direction. The
  # third iteration saw LARGER because it was reading same-round re-writes,
  # where a re-encoded parquet differs by a few hundred bytes either way.
  #
  # So size direction carries no information about which failure this is, in
  # EITHER direction, and the decision belongs entirely to the staleness signal
  # the third iteration already introduced -- the listing's own updated_at
  # versus when we started our upload:
  #   updated_at OLDER than us -> the row is a previous asset and the listing
  #                               has not caught up; retry, then warn+proceed
  #   updated_at AT/AFTER us   -> the row IS our write, so the size mismatch is
  #                               real: short means truncation, long means a
  #                               failed replace or a concurrent writer. Fatal.
  #
  # A LARGER listing was never self-evidently a stale read either. It is
  # equally consistent with a FAILED REPLACE (piggyback's delete-then-upload is
  # not atomic -- if the delete loses a race the old, bigger asset stays live
  # while pb_upload() still returns 2xx) or with a CONCURRENT WRITER. Same
  # applies to smaller. That is the whole point: only the timestamp separates
  # them.
  #
  # Note the previous iteration of this comment claimed truncation was
  # "independently guarded by the row-count floor against bus_manifest.json".
  # That was FALSE twice over. `prev_rows_floor` defaults to NULL and none of the
  # ~80 save_to_release() call sites pass it, so it is inert -- and even wired up
  # it would not help, because it compares `nrow(df)` IN MEMORY and aborts before
  # write_parquet(). It guards bad INPUT, not a bad TRANSFER. Do not cite it as a
  # truncation backstop again. The transport-side backstop is
  # `.vb_asset_true_size()`, on the stale-listing path below.
  #
  # The availability motivation still stands -- a whole daily release aborting
  # stops the downstream dispatch to torp and staled its match predictions for
  # two weeks -- but it does not license waving through a write we cannot
  # distinguish from a lost one.
  #
  # A failure to even LIST after retries (vb_error_transient from
  # vb_list_assets itself) only warns -- evidence the verify call flaked, not
  # that the upload did.
  verify_upload <- function() {
    listed <- tryCatch(
      vb_list_assets(repo, release_tag),
      error = function(e) {
        .vb_abort("Post-upload verify could not list {repo}@{release_tag}: {conditionMessage(e)}",
                  c("vb_error_transient", "vb_verify_list_failed"), parent = e)
      }
    )
    row <- listed[listed$name == f_name, , drop = FALSE]
    if (nrow(row) == 0L) {
      # Absent from the listing is the same failure as stale in the listing --
      # the listing is wrong -- so it earns the same treatment: resolve the
      # asset by name on the download path, which cannot answer out of a lagging
      # index. A NEW asset name is what lands here, since there is no prior row
      # for the listing to be stale against, and it is also the slowest to
      # appear. torp_ratings_v3.parquet uploaded fine and then halted the run on
      # this branch, taking Stages 4 and 5 with it, while the asset was in fact
      # live (2026-08-18, first build of the v3 vintage).
      # The extra class marks the ORIGIN. Absent-from-listing and stale-in-
      # listing share the same recovery (ask the download path), but not the
      # same verdict when that path answers 404: with no listing row at all
      # there is no evidence the object was ever written, whereas a stale row
      # is itself evidence of a previous good asset.
      .vb_abort("Post-upload verify: {.val {f_name}} missing from {repo}@{release_tag} listing after upload",
                c("vb_error_transient", "vb_verify_stale_listing",
                  "vb_verify_absent_from_listing"))
    }
    local_bytes <- as.numeric(file.size(tf))
    listed_bytes <- as.numeric(row$size[1L])
    if (!isTRUE(all.equal(listed_bytes, local_bytes))) {
      # Same phrase in every branch below, so the direction is always visible in
      # the log even when the decision did not turn on it.
      direction <- if (listed_bytes < local_bytes) "<" else ">"
      sizes <- paste("listed size", listed_bytes, direction, "local", local_bytes)
      listed_at <- suppressWarnings(
        as.POSIXct(row$updated_at[1L], format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      )
      # An unparseable timestamp means the signal we decide on is untrustworthy,
      # so fail closed -- but say THAT, rather than asserting a temporal
      # relationship we never actually evaluated. An operator chasing the
      # "a different write replaced ours" message would otherwise hunt a
      # phantom concurrent writer instead of an API shape change.
      if (is.na(listed_at)) {
        .vb_abort("Post-upload verify: {.val {f_name}} {sizes}, and updated_at {.val {row$updated_at[1L]}} could not be parsed -- treating the staleness signal as untrusted",
                  "vb_error_integrity")
      }
      # A row stamped before our own upload is a previous asset, whichever way
      # its size falls: the season files grow on each round, so a lagging
      # listing reads SMALLER, and a same-round re-encode reads either way.
      stale_read <- listed_at < (upload_started_at - VB_VERIFY_CLOCK_SKEW_SECS)
      if (stale_read) {
        .vb_abort("Post-upload verify: {.val {f_name}} {sizes}, listing stamped {row$updated_at[1L]} (before our upload) -- lagging listing",
                  c("vb_error_transient", "vb_verify_stale_listing"))
      }
      # Stamped at or after our upload, so this row IS our write and the size
      # mismatch is real.
      if (listed_bytes < local_bytes) {
        .vb_abort("Post-upload verify: {.val {f_name}} {sizes}, listing stamped {row$updated_at[1L]} (at or after our upload) -- our own write is short, i.e. a truncated upload",
                  "vb_error_integrity")
      }
      .vb_abort("Post-upload verify: {.val {f_name}} {sizes}, listing stamped {row$updated_at[1L]} (at or after our upload) -- a different write replaced ours, or the replace failed and the old asset is still live",
                "vb_error_integrity")
    }
    invisible(TRUE)
  }
  # DEFERRED, not raised here. The `also_csv` block below MUST still run even
  # when the parquet verify fails.
  #
  # Observed 2026-07-29 (run 30434581586): the parquet uploaded at 08:51:15, the
  # verify threw, this handler re-raised, and the CSV never uploaded -- leaving
  # predictions_2026.parquet on the new rating vintage and predictions_2026.csv
  # 15 hours stale. squiggle.com.au reads the CSV, so it served the previous
  # round's tips while every internal check looked healthy. The caller's own
  # tryCatch caught the re-raise, but its cli_warn is deferred to end-of-script
  # and was lost when the job hit its timeout, so NOTHING was logged.
  #
  # The parquet is already live by this point, so a verify failure is a reason
  # to shout, not a reason to also skip the copy Squiggle depends on. The error
  # is preserved and re-raised after the CSV attempt.
  verify_err <- NULL
  tryCatch(
    .vb_retry(verify_upload, times = 5L, delays = c(2, 3, 5, 10),
              should_retry = function(e) vb_classify_error(e) != "absent"),
    error = function(e) {
      if (inherits(e, "vb_verify_list_failed")) {
        cli::cli_warn("Post-upload verify could not list {repo}@{release_tag} after retries ({conditionMessage(e)}) -- upload itself already succeeded, proceeding")
      } else if (inherits(e, "vb_verify_stale_listing")) {
        # The listing never caught up, so stop asking it. Until 2026-08-09 this
        # branch warned and moved on, which is the one hole the 4th iteration
        # knowingly left: a genuinely short upload whose listing ALSO lags looks
        # identical here to a lagging read of a good one.
        #
        # `.vb_asset_true_size()` closes it by resolving the asset by NAME on the
        # download path, which cannot return the previous asset the way a stale
        # listing row can. It costs one byte. The old warning text told an
        # operator to "check the asset's real bytes rather than trusting the
        # listing" -- this does that automatically.
        local_bytes <- as.numeric(file.size(tf))
        true_bytes <- .vb_asset_true_size(repo, release_tag, f_name)
        if (is.finite(true_bytes) && isTRUE(all.equal(true_bytes, local_bytes))) {
          cli::cli_alert_success(
            "Post-upload verify: {.val {f_name}} confirmed at {true_bytes} bytes on the download path -- the listing was simply lagging.")
        } else if (inherits(e, "vb_verify_absent_from_listing") &&
                   identical(as.numeric(true_bytes), as.numeric(VB_ASSET_CONFIRMED_ABSENT))) {
          # Decisive ONLY for the absent-from-listing origin: neither route can
          # see the object, and there is no prior listing row suggesting one was
          # ever there, so this is a lost write rather than a lagging index.
          # Without this it fell through to the "unreachable" arm below and
          # warned-and-proceeded, reporting a lost upload as success
          # (2026-08-18 review, on the path this file had just started using).
          #
          # Deliberately NOT applied to a merely-stale listing row: there the
          # row itself is evidence a good asset exists, so a 404 on the download
          # path is one more unreliable read rather than proof of absence.
          verify_err <<- tryCatch(
            cli::cli_abort(
              "Post-upload verify: {.val {f_name}} answers 404/410 on the download path too, after {local_bytes} bytes were written -- a lost write, not a lagging listing",
              class = c("vb_error_integrity", "vb_error")),
            error = function(x) x)
        } else if (is.finite(true_bytes) &&
                   !identical(as.numeric(true_bytes), as.numeric(VB_ASSET_CONFIRMED_ABSENT)) &&
                   true_bytes < local_bytes) {
          # Now this IS evidence, not an ambiguity: the stored object is short.
          # The sentinel is excluded explicitly -- it is negative, so it compares
          # as "short" and would otherwise be reported as a truncated upload when
          # what actually happened is a 404 on a merely-stale listing row.
          verify_err <<- tryCatch(
            cli::cli_abort(
              "Post-upload verify: {.val {f_name}} is {true_bytes} bytes on the download path against {local_bytes} written -- a genuinely short asset, not a lagging listing",
              class = c("vb_error_integrity", "vb_error")),
            error = function(x) x)
        } else {
          # Unreachable answer (private-repo redirect, network, an unexpected
          # status): fall back to exactly the previous behaviour rather than
          # inventing a verdict from a failed measurement.
          cli::cli_warn(c(
            "Post-upload verify still reading an older-stamped asset for {.val {f_name}} after retries ({conditionMessage(e)}) -- proceeding.",
            "i" = "pb_upload() returned success and the listing predates our own upload, so this is a lagging read rather than a lost write.",
            "!" = "The authoritative size read did not return a usable answer either, so this is not a proof of correctness."
          ))
        }
      } else {
        verify_err <<- e
      }
    }
  )

  if (also_csv) {
    csv_name <- paste0(file_name, ".csv")
    tf_csv <- tempfile(fileext = ".csv")
    on.exit(unlink(tf_csv), add = TRUE)

    # The CSV is NOT a convenience mirror for `predictions`: squiggle.com.au
    # reads predictions_<season>.csv to pick up torp's tips. If it fails here
    # while the parquet succeeds, everything downstream looks healthy -- the
    # release is updated, the run goes green, our own loaders (which read
    # parquet) see fresh data -- and Squiggle silently keeps serving the
    # PREVIOUS round's tips. Observed 2026-07-28: parquet at 14:03, CSV still
    # at 05:53, caught only by eyeballing asset timestamps.
    #
    # Still a warning rather than an abort: the parquet upload has already
    # succeeded by this point, and discarding a good data release because a
    # secondary copy failed is the wrong trade. But it must be impossible to
    # miss, and it must name the consequence.
    tryCatch({
      utils::write.csv(df, tf_csv, row.names = FALSE)
      piggyback::pb_upload(tf_csv,
                           repo = repo,
                           tag = release_tag,
                           name = csv_name)
    }, error = function(e) {
      # cli_alert_danger FIRST: it prints immediately. A bare cli_warn is
      # deferred to the end of an Rscript run and dropped past
      # getOption("nwarnings"), which is exactly how the 2026-07-29 CSV
      # divergence left no trace in the log. The verify path above was fixed
      # for that; THIS handler -- the one that fires when the CSV upload itself
      # fails -- was left on the weak path in the same commit.
      cli::cli_alert_danger(
        "CSV copy FAILED for {csv_name} (parquet uploaded fine): {conditionMessage(e)}")
      if (release_tag == "predictions") {
        cli::cli_alert_danger(
          "squiggle.com.au reads this CSV -- it will keep serving the PREVIOUS round's tips until this succeeds.")
      }
      cli::cli_warn(c(
        "!" = "CSV copy FAILED for {.val {csv_name}} (parquet uploaded fine): {conditionMessage(e)}",
        "x" = if (release_tag == "predictions")
          "squiggle.com.au reads this CSV -- it will keep serving the PREVIOUS tips until this succeeds."
        else "Consumers reading the CSV rather than the parquet will see stale data.",
        "i" = "Re-run the pipeline, or re-upload the CSV, before the next round starts."
      ))
    })
  }

  # Also save a local copy if torpdata/data/ is configured
  if (!is.null(get_local_data_dir())) {
    tryCatch(
      save_locally(df, file_name),
      error = function(e) {
        cli::cli_warn("Remote upload succeeded but local save failed: {conditionMessage(e)}")
      }
    )
  }

  # Now re-raise the deferred verify failure, with the CSV already attempted.
  # cli_alert_danger prints IMMEDIATELY; a bare warning would be deferred to
  # end-of-script and is exactly what went missing on 2026-07-29 when the job
  # was killed by its timeout a second after finishing.
  if (!is.null(verify_err)) {
    cli::cli_alert_danger(
      "Post-upload verify FAILED for {.val {f_name}} -- the upload itself succeeded and the CSV copy was still attempted, but treat this asset as unconfirmed.")
    stop(verify_err)
  }
}

#' Publish/refresh `bus_manifest.json` for one uploaded asset (T13)
#'
#' Reads the tag's previous manifest (legacy mode -- NULL -- when absent),
#' replaces this asset's entry, carries every other entry forward unchanged
#' (so the manifest always describes the whole tag, per
#' ECOSYSTEM-FIX-PLAN.md §1.2's partial-tag-publish rule), and uploads the
#' result as `bus_manifest.json` LAST. Called from [save_to_release()] after
#' its own upload has already succeeded and been verified -- this function's
#' only job is to keep the commit record current, never to gate the upload.
#'
#' @param tag Release tag.
#' @param name Asset file name (e.g. `"chains_data_2026_all.parquet"`).
#' @param local_path Local path to the just-uploaded file (still on disk).
#' @param rows Optional row count for the manifest entry.
#' @return Invisible manifest list.
#' @keywords internal
.publish_bus_manifest <- function(tag, name, local_path, rows = NULL) {
  repo <- get_torp_data_repo()
  entry <- vb_asset_entry(local_path, rows = rows)
  entry$name <- name  # local_path is a tempfile; use the canonical asset name

  prev <- tryCatch(vb_read_prev_manifest(repo, tag), error = function(e) NULL)
  merged <- .vb_merge_entries(prev, stats::setNames(list(entry), name))

  tmp <- file.path(tempdir(), paste0(".bus_manifest_", tag, ".json"))
  on.exit(unlink(tmp), add = TRUE)
  manifest <- vb_write_manifest(merged, tag, tmp)

  up <- function() piggyback::pb_upload(tmp, repo = repo, tag = tag, name = "bus_manifest.json", overwrite = TRUE)
  tryCatch(up(), error = function(e) { Sys.sleep(5); up() })

  invisible(manifest)
}

#' Update Player Stats Release
#'
#' Scrapes player stats from the AFL API for a given season, normalises
#' column names, and uploads to the torpdata GitHub release.
#'
#' @param season Season year (numeric).
#' @return Invisible NULL. Called for side effects (upload).
#' @keywords internal
update_player_stats <- function(season) {
  cli::cli_progress_step("Updating player stats for {season}")

  player_stats <- tryCatch({
    get_afl_player_stats(season) |>
      dplyr::select(tidyselect::where(~ dplyr::n_distinct(.) > 1)) |>
      torp_clean_names()
  }, error = function(e) {
    cli::cli_alert_danger("Failed to fetch player stats: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(player_stats) || nrow(player_stats) == 0) {
    cli::cli_alert_warning("No player stats data for {season}")
    return(invisible(NULL))
  }

  # Normalise column names (strips v2 stats_ prefix, renames player/match IDs)
  player_stats <- .normalise_player_stats_columns(player_stats)

  if (!"season" %in% names(player_stats)) {
    player_stats$season <- as.integer(season)
  }

  # torp H7: .fetch_cfs_batch() can silently return a partial batch (token
  # expiry, rate limiting mid-fetch). Compare fetched match coverage against
  # the season's concluded-match fixture count and refuse to upload a
  # player_stats file that's missing more than 5% of matches -- an unattended
  # daily run would otherwise happily re-upload (and re-poison) a partial file.
  if ("match_id" %in% names(player_stats)) {
    fixtures <- tryCatch(get_afl_fixtures(season), error = function(e) NULL)
    if (!is.null(fixtures) && nrow(fixtures) > 0 && "status" %in% names(fixtures)) {
      expected_matches <- sum(fixtures$status == "CONCLUDED", na.rm = TRUE)
      fetched_matches <- dplyr::n_distinct(player_stats$match_id)
      if (expected_matches > 0) {
        missing_frac <- 1 - (fetched_matches / expected_matches)
        if (missing_frac > 0.05) {
          cli::cli_abort(c(
            "Partial player stats fetch for {season}: {fetched_matches} of {expected_matches} concluded matches ({round(missing_frac * 100, 1)}% missing).",
            "x" = "Refusing to upload -- would poison player_stats_{season}.parquet until manually re-run."
          ))
        }
      }
    }
  }

  file_name <- paste0("player_stats_", season)
  save_to_release(df = player_stats, file_name = file_name, release_tag = "player_stats-data")

  cli::cli_inform("Saved player stats: {file_name} ({nrow(player_stats)} rows)")
  invisible(NULL)
}

#' Read a Parquet File from a GitHub Release via Piggyback
#'
#' Downloads and reads a `.parquet` file from a GitHub release. Routes the
#' download through [vb_download()] (verified, atomic write). On any download
#' failure, the error is independently reclassified via [vb_confirm_absent()]
#' -- a real listing call -- rather than trusted from the raw piggyback error:
#' only a positively-confirmed absence is re-signalled as `vb_error_absent`,
#' everything else (including listing failures) propagates as
#' `vb_error_transient` (or the original error). This is the versebus
#' 404-vs-transient discipline (ECOSYSTEM-FIX-PLAN.md §1.3/§1.5) -- callers
#' MUST NOT collapse a caught error here into "file doesn't exist" without
#' checking its class first.
#'
#' @param file_name The base name of the file (without `.parquet` extension).
#' @param release_tag The GitHub release tag the file is associated with.
#'
#' @return A data frame read from the downloaded `.parquet` file.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' df <- file_reader("latest_data", "v1.0.0")
#' }
file_reader <- function(file_name, release_tag) {
  rlang::check_installed("piggyback", version = "0.1.4", reason = "to download data from GitHub releases")
  f_name <- paste0(file_name, ".parquet")
  repo <- get_torp_data_repo()
  tf <- tempfile(fileext = ".parquet")
  on.exit(unlink(tf), add = TRUE)

  tryCatch(
    vb_download(repo, release_tag, f_name, tf),
    vb_error_absent = function(e) stop(e),
    vb_error_integrity = function(e) stop(e),
    error = function(e) {
      # vb_download's own dispatch is fail-safe-transient by default (it has
      # no manifest/listing evidence of its own for a bare piggyback error).
      # Independently confirm absence via a real listing call before
      # re-classifying -- never trust the raw error message alone.
      confirmed_absent <- tryCatch(
        vb_confirm_absent(repo, release_tag, f_name),
        error = function(e2) FALSE
      )
      if (isTRUE(confirmed_absent)) {
        cli::cli_abort(
          "{.val {f_name}} confirmed absent from release {.val {release_tag}}",
          class = c("vb_error_absent", "vb_error"),
          parent = e
        )
      }
      stop(e)
    }
  )

  tryCatch(
    arrow::read_parquet(tf),
    error = function(e) {
      cli::cli_abort("Failed to read {.val {f_name}} after download: {conditionMessage(e)}")
    }
  )
}


# ============================================================================
# Internal helper: load data via API with in-memory cache
# ============================================================================

#' Load data from AFL API with caching
#'
#' Generalised helper that wraps any `get_afl_*()` function with per-season
#' fetching, in-memory caching, and column selection. Used by `load_fixtures()`,
#' `load_results()`, `load_teams()`, `load_player_stats()`, and
#' `load_player_details()`.
#'
#' @param cache_prefix Character. Cache key prefix (e.g. "fixtures", "results").
#' @param seasons Numeric vector of seasons.
#' @param fetch_fn Function that takes a single season year and returns a
#'   data.frame/tibble.
#' @param use_cache Logical. Whether to use in-memory caching.
#' @param cache_ttl Numeric. Cache time-to-live in seconds.
#' @param verbose Logical. Print cache hit/miss info.
#' @param columns Optional character vector of column names to select.
#' @param fetch_all_fn Optional function that takes a vector of seasons and
#'   returns all data in one batch. When provided and `length(seasons) > 1`,
#'   used instead of per-season `lapply(seasons, fetch_fn)` for efficiency
#'   (e.g. one big `curl::multi_download()` instead of N sequential batches).
#' @param use_disk_cache Logical. If TRUE, uses persistent disk cache for
#'   completed past seasons. Default is FALSE.
#' @param refresh Logical. If TRUE, clears all caches and fetches fresh
#'   data from the API. Default is FALSE.
#' @param strict Logical. If TRUE, abort when any requested season fails to
#'   fetch instead of silently returning a partial result. Defaults to
#'   `VERSEBUS_STRICT=1` (set by pipeline entry scripts); interactive
#'   sessions stay lenient. Regardless of `strict`, a result with any failed
#'   season is never written to the in-memory cache (torp H2).
#' @return A tibble.
#' @keywords internal
.load_with_cache <- function(cache_prefix, seasons, fetch_fn,
                             use_cache = TRUE, cache_ttl = 3600,
                             verbose = FALSE, columns = NULL,
                             fetch_all_fn = NULL,
                             use_disk_cache = FALSE,
                             refresh = FALSE,
                             strict = .strict_mode()) {
  cache_key <- paste0(cache_prefix, "_", paste(sort(seasons), collapse = "_"))

  # Force refresh: clear in-memory and disk caches
  if (refresh) {
    if (exists(cache_key, envir = .torp_cache)) {
      rm(list = cache_key, envir = .torp_cache)
    }
    if (use_disk_cache) {
      # Anchored + requires a digit right after the prefix (the season year),
      # so a shorter comp's prefix (e.g. "player_stats") can't match a longer
      # comp-suffixed one ("player_stats_AFLW") as a substring -- an unanchored
      # pattern here would let refreshing AFLM silently wipe AFLW's disk cache.
      clear_disk_cache(pattern = sprintf("^cfs_%s_\\d", cache_prefix))
    }
  }

  # Check in-memory cache
  if (!refresh && use_cache && exists(cache_key, envir = .torp_cache)) {
    cache_entry <- get(cache_key, envir = .torp_cache)
    if (is_cache_valid(cache_entry, cache_ttl)) {
      if (verbose) {
        age_seconds <- as.numeric(difftime(Sys.time(), cache_entry$timestamp, units = "secs"))
        cli::cli_inform("Cache HIT for {cache_prefix} (age: {round(age_seconds, 1)}s)")
      }
      out <- cache_entry$data
      if (!is.null(columns)) {
        keep <- intersect(columns, names(out))
        out <- out[, keep, drop = FALSE]
      }
      return(tibble::as_tibble(out))
    } else {
      if (verbose) cli::cli_inform("Cache EXPIRED for {cache_prefix}, fetching fresh data")
      rm(list = cache_key, envir = .torp_cache)
    }
  } else if (use_cache && verbose) {
    cli::cli_inform("Cache MISS for {cache_prefix}, fetching data")
  }

  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  failed_seasons <- character(0)

  # Fetch data — use bulk fetcher if available and multi-season, else per-season
  if (!is.null(fetch_all_fn) && length(seasons) > 1) {
    results <- list(tryCatch(
      suppressMessages(fetch_all_fn(seasons)),
      error = function(e) {
        cli::cli_warn("Bulk fetch failed for {cache_prefix}: {conditionMessage(e)}")
        failed_seasons <<- as.character(seasons)
        NULL
      }
    ))
  } else {
    results <- lapply(seasons, function(s) {
      # Check per-season disk cache
      if (use_disk_cache) {
        disk_data <- .read_season_disk_cache(cache_prefix, s, current_year)
        if (!is.null(disk_data)) return(disk_data)
      }

      data <- tryCatch(suppressMessages(fetch_fn(s)), error = function(e) {
        cli::cli_warn("Failed to fetch {cache_prefix} for {s}: {conditionMessage(e)}")
        failed_seasons <<- c(failed_seasons, as.character(s))
        NULL
      })

      # Save to per-season disk cache (past seasons only)
      if (use_disk_cache && s < current_year && !is.null(data) && nrow(data) > 0) {
        .write_season_disk_cache(cache_prefix, s, data)
      }

      data
    })
  }
  results <- Filter(Negate(is.null), results)

  # torp H2: failed seasons are dropped silently no more -- surface loudly,
  # and abort outright in strict (pipeline) mode rather than compute
  # downstream ratings/models against a season-shaped hole.
  if (length(failed_seasons) > 0) {
    failed_seasons <- unique(failed_seasons)
    if (strict) {
      cli::cli_abort(
        "{cache_prefix}: failed to fetch season(s) {paste(failed_seasons, collapse = ', ')} - aborting (VERSEBUS_STRICT)",
        class = c("vb_error_transient", "vb_error")
      )
    }
    cli::cli_warn("{cache_prefix}: proceeding with partial data -- season(s) {paste(failed_seasons, collapse = ', ')} failed to fetch")
  }

  if (length(results) == 0) {
    cli::cli_warn("No {cache_prefix} data returned for seasons: {paste(seasons, collapse = ', ')}")
    return(tibble::tibble())
  }

  # rbindlist with fill=TRUE handles differing column sets across seasons
  out <- data.table::rbindlist(lapply(results, data.table::as.data.table), fill = TRUE)
  out <- tibble::as_tibble(out)

  # Store in in-memory cache before column selection -- but never a partial
  # result (torp H2): a season-hole cached for cache_ttl silently poisons
  # every later loader call in the same session/pipeline run.
  if (use_cache && nrow(out) > 0 && length(failed_seasons) == 0) {
    store_in_cache(cache_key, out)
    if (verbose) cli::cli_inform("Stored {cache_prefix} in cache ({nrow(out)} rows)")
  }

  # Apply column selection
  if (!is.null(columns)) {
    keep <- intersect(columns, names(out))
    out <- out[, keep, drop = FALSE]
  }

  out
}

#' Read per-season disk cache for CFS data
#'
#' Only caches past seasons (before current year). Current season data
#' may contain provisional lineups or live stats, so it is always
#' fetched fresh from the API.
#'
#' @param prefix Cache prefix (e.g. "teams", "player_stats")
#' @param season Season year
#' @param current_year Current calendar year
#' @return Data frame if cache hit, NULL if miss
#' @keywords internal
.read_season_disk_cache <- function(prefix, season, current_year) {
  # Never use disk cache for current season — data may be provisional
  if (season >= current_year) return(NULL)

  cache_dir <- get_disk_cache_dir()
  disk_path <- file.path(cache_dir, sprintf("cfs_%s_%d.parquet", prefix, season))

  if (!file.exists(disk_path)) return(NULL)

  tryCatch({
    data <- arrow::read_parquet(disk_path)
    if (nrow(data) > 0) {
      cli::cli_inform("Disk cache HIT for {prefix} {season} ({nrow(data)} rows)")
      return(data)
    }
    NULL
  }, error = function(e) {
    cli::cli_warn("Corrupt disk cache for {prefix} {season} -- deleting and re-fetching: {conditionMessage(e)}")
    unlink(disk_path)
    NULL
  })
}

#' Write per-season disk cache for CFS data
#'
#' @param prefix Cache prefix
#' @param season Season year
#' @param data Data frame to cache
#' @keywords internal
.write_season_disk_cache <- function(prefix, season, data) {
  cache_dir <- get_disk_cache_dir()
  disk_path <- file.path(cache_dir, sprintf("cfs_%s_%d.parquet", prefix, season))
  tryCatch(
    arrow::write_parquet(data, disk_path),
    error = function(e) {
      cli::cli_warn("Failed to write disk cache for {prefix} {season}: {conditionMessage(e)}")
    }
  )
}


#' Load Chains Data
#'
#' @description Loads chains data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param rounds A numeric vector associated with given AFL round - defaults to all rounds. If set to `TRUE`, returns all available rounds in the given season range.
#' @param use_disk_cache Logical. If TRUE, uses persistent disk cache for faster repeated loads. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing chains data.
#' @seealso [load_pbp()], [load_xg()], [load_fixtures()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_chains(2021:2022)
#' })
#' }
#' @export
load_chains <- function(seasons = get_afl_season(), rounds = TRUE, use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)
  rounds <- validate_rounds(rounds)

  urls <- generate_urls("chains-data", "chains_data", seasons, rounds)

  out <- load_from_url(urls, seasons = seasons, rounds = rounds, use_disk_cache = use_disk_cache, columns = columns)

  # Normalise camelCase chains columns (matchId → match_id, etc.)
  if (nrow(out) > 0) {
    if (!data.table::is.data.table(out)) out <- data.table::as.data.table(out)
    .normalise_chains_columns(out)
  }

  if (nrow(out) > 0) out <- .normalise_team_values(out)
  return(out)
}

#' Load Play By Play Data
#'
#' @description Loads play by play seasons from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param rounds A numeric vector associated with given AFL round - defaults to all rounds. If set to `TRUE`, returns all available rounds in the given season range.
#' @param use_disk_cache Logical. If TRUE, uses persistent disk cache for faster repeated loads. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing play by play data.
#' @seealso [load_chains()], [load_xg()], [load_fixtures()], [clean_pbp()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_pbp(2021:2022)
#' })
#' }
#' @export
load_pbp <- function(seasons = get_afl_season(), rounds = TRUE, use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)
  rounds <- validate_rounds(rounds)

  urls <- generate_urls("pbp-data", "pbp_data", seasons, rounds)

  out <- load_from_url(urls, seasons = seasons, rounds = rounds, use_disk_cache = use_disk_cache, columns = columns)

  if (nrow(out) > 0) out <- .normalise_team_values(out)
  return(out)
}

#' Load Expected Goals (xG) Data
#'
#' @description Loads xg data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param rounds A numeric vector of round numbers to filter to. If `NULL` (default), returns all rounds.
#' @param use_disk_cache Logical. If TRUE, uses persistent disk cache for faster repeated loads. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing xG data.
#' @seealso [load_pbp()], [load_chains()], [calculate_match_xgs()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_xg(2021:2022)
#'   load_xg(2026, 2)
#' })
#' }
#' @export
load_xg <- function(seasons = get_afl_season(), rounds = NULL, use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("xg-data", "xg_data", seasons)

  out <- load_from_url(urls, seasons = seasons, use_disk_cache = use_disk_cache, columns = columns)

  # Normalise old column names (home_sG → home_scored_goals, etc.)
  if (nrow(out) > 0) {
    .normalise_columns(out, XG_COL_MAP)
  }

  # Filter by round if requested (round parsed from match_id)
  if (!is.null(rounds) && nrow(out) > 0) {
    if (!data.table::is.data.table(out)) out <- data.table::as.data.table(out)
    out[, round_number := .extract_round_from_match_id(match_id)]
    out <- out[round_number %in% as.integer(rounds)]
  }

  if (nrow(out) > 0) out <- .normalise_team_values(out)
  return(out)
}

#' Load Player Stats Data
#'
#' @description Loads player stats data from the AFL API (cached to disk, not fetched from a torpdata release)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param use_disk_cache Logical. If TRUE (default), caches completed past seasons
#'   to disk so they load instantly on subsequent calls. Current season is always
#'   fetched fresh from the API.
#' @param refresh Logical. If TRUE, clears all caches and fetches fresh data
#'   from the API for all seasons. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#' @param comp Competition: "AFLM" (default) or "AFLW". AFLW's box score is a
#'   narrower stat set (59 vs 84 columns as of 2026-08-24).
#'
#' @return A data frame containing player stats data.
#' @seealso [load_player_details()], [player_game_ratings()], [player_season_ratings()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_player_stats(2021:2022)
#' })
#' }
#' @export
load_player_stats <- function(seasons = get_afl_season(), use_disk_cache = TRUE, refresh = FALSE,
                              columns = NULL, comp = "AFLM") {
  .validate_afl_comp(comp)
  seasons <- validate_seasons(seasons)

  # comp-suffix the cache prefix so an AFLM and AFLW load for the same season
  # never collide in the in-memory cache or the disk cache filename.
  cache_prefix <- if (comp == "AFLM") "player_stats" else paste0("player_stats_", comp)

  out <- .load_with_cache(
    cache_prefix = cache_prefix,
    seasons = seasons,
    fetch_fn = function(s) get_afl_player_stats(s, comp = comp),
    columns = columns,
    use_disk_cache = use_disk_cache,
    refresh = refresh
  )

  # Normalise once after retrieval (handles both fresh API data and stale disk cache)
  if (nrow(out) > 0) out <- tibble::as_tibble(.normalise_player_stats_columns(out))

  if (nrow(out) > 0) out <- .normalise_team_values(out)
  out
}

#' Load Player Game Data
#'
#' @description Loads processed player game data from the [torpdata repository](https://github.com/peteowen1/torpdata).
#' This data contains per-game performance metrics (disposal points, reception points,
#' spoil points, hitout points) adjusted by position, as used by the TORP ratings pipeline.
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param use_disk_cache Logical. If TRUE, uses persistent disk cache for faster repeated loads. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#' @param version Rating vintage label. `NULL` (default) reads the canonical
#'   `player_game_<season>.parquet`; a label reads `player_game_<season>_<label>.parquet`,
#'   matching the names the pipeline's Stage 2 writes for a candidate vintage.
#'
#' @return A data frame containing player game performance data.
#' @seealso [create_player_game_data()], [player_game_ratings()], [calculate_epr()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_player_game_data(2024)
#' })
#' }
#' @export
load_player_game_data <- function(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL,
                                  version = NULL) {
  seasons <- validate_seasons(seasons)

  if (is.null(version)) {
    urls <- generate_urls("player_game-data", "player_game", seasons)
  } else {
    # A candidate vintage has its own per-season assets, written by Stage 2 as
    # .vintage_asset_stem(paste0("player_game_", s), version). Building those
    # names here rather than in generate_urls() keeps the aggregated-file
    # shortcut (which only ever covers canonical) out of the candidate path.
    #
    # This argument exists because Stage 3 called load_player_game_data(TRUE)
    # with no vintage while Stage 2 wrote _v3 files, so a run labelled v3 built
    # its ratings from the canonical v2 player-game data -- a v2 vintage wearing
    # a v3 name, with nothing failing (2026-08-18).
    base_url <- paste0("https://github.com/", get_torp_data_repo(), "/releases/download")
    urls <- paste0(base_url, "/player_game-data/",
                   vapply(seasons,
                          function(s) .vintage_asset_stem(paste0("player_game_", s), version),
                          character(1)),
                   ".parquet")
  }

  # Always read the engine stamp, even when the caller asked for a narrow
  # projection, then drop it again if they did not want it. Without this a
  # `columns=` that omits epv_engine returns an unstamped frame, and an
  # unstamped frame is priced as v2 whatever produced it -- silently, since
  # .frame_epv_engine() only warns when the GLOBAL engine constant reads v3,
  # which is exactly not the case while a v3 candidate is being inspected
  # before promotion. Same read-then-drop shape load_from_url() already uses
  # for the season/round filter columns.
  read_cols <- columns
  if (!is.null(read_cols)) read_cols <- union(read_cols, "epv_engine")

  out <- load_from_url(urls, seasons = seasons, use_disk_cache = use_disk_cache, columns = read_cols)

  # Normalise old abbreviated column names (plyr_nm → player_name, etc.)
  if (nrow(out) > 0) .normalise_columns(out, PLAYER_GAME_COL_MAP)

  if (nrow(out) > 0) out <- .normalise_team_values(out)
  out <- .restore_epv_engine_attr(out)

  # Stamp is on the attribute now, so the column can go if unasked for. setattr
  # rather than a copy: dropping a column must not discard what we just read.
  if (!is.null(columns) && !("epv_engine" %in% columns) && "epv_engine" %in% names(out)) {
    eng <- attr(out, "epv_engine")
    out <- out[, setdiff(names(out), "epv_engine"), drop = FALSE]
    if (!is.null(eng)) data.table::setattr(out, "epv_engine", eng)
  }
  return(out)
}

#' Re-attach the EPV engine stamp after a parquet round-trip
#'
#' @description `create_player_game_data()` tags its frame with an
#'   `epv_engine` attribute, and every downstream pricing decision reads it.
#'   R attributes do not survive parquet, so a frame that goes through the
#'   release comes back unstamped and [.frame_epv_engine()] then prices it as
#'   v2 -- silently, whatever engine actually produced it.
#'
#'   The engine therefore travels as a COLUMN, which does survive, and this
#'   restores it to an attribute on load. Files written before the column
#'   existed are all v2, so an absent column means v2 rather than "unknown".
#'
#'   A frame carrying two different engines is refused outright: it can only
#'   come from mixing vintages across seasons, and averaging two pricing
#'   conventions into one rating is not something to warn about and continue.
#'
#' @param out A loaded player-game frame.
#' @return The frame, stamped.
#' @keywords internal
.restore_epv_engine_attr <- function(out) {
  if (nrow(out) == 0 || !"epv_engine" %in% names(out)) return(out)
  vals <- out[["epv_engine"]]

  # A PARTIALLY stamped frame is the dangerous shape, and na.omit() hid it.
  # Seasons arrive as separate files and are rbindlist(fill = TRUE)'d together,
  # so a season rebuilt under v3 supplies the column while seasons not yet
  # rebuilt supply NA. Dropping those NAs leaves one distinct engine, the frame
  # is stamped with it, and centre_epv_by_position() then applies the v3
  # per-channel scale to every row -- repricing years of v2-computed history,
  # silently, on a clean run. The all-absent case stays fine: no column at all
  # means no season was rebuilt, which is a genuine v2 frame.
  n_missing <- sum(is.na(vals))
  engines <- unique(stats::na.omit(vals))
  if (n_missing > 0 && length(engines) > 0) {
    cli::cli_abort(
      c("Player-game data is only partly stamped: {n_missing} of {length(vals)} row{?s} carry no EPV engine, alongside {.val {engines}}.",
        "x" = "That is seasons from different vintages in one frame. Stamping it from the labelled rows would reprice the unlabelled ones.",
        "i" = "Rebuild the missing seasons, or load a single vintage: {.code load_player_game_data(version = ...)}."),
      class = "torp_error_mixed_epv_engine"
    )
  }
  if (length(engines) > 1) {
    cli::cli_abort(
      c("Player-game data carries {length(engines)} EPV engines: {.val {engines}}.",
        "x" = "These price differently, so one rating built across both is meaningless.",
        "i" = "Load a single vintage: {.code load_player_game_data(version = ...)}."),
      class = "torp_error_mixed_epv_engine"
    )
  }
  if (length(engines) == 1) data.table::setattr(out, "epv_engine", as.character(engines))
  out
}

#' Load AFL Fixture Data
#'
#' @description Loads AFL fixture and schedule data from the AFL API (cached to disk, not fetched from a torpdata release)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param all Deprecated. Use `seasons = TRUE` instead (consistent with other `load_*()` functions).
#' @param use_disk_cache Logical. If TRUE, uses persistent disk caching. Default is FALSE.
#' @param use_cache Deprecated. Use \code{use_mem_cache} instead.
#' @param use_mem_cache Logical. If TRUE, uses in-memory cached data when available to speed up repeated calls. Default is TRUE.
#' @param cache_ttl Numeric. Time-to-live for cached data in seconds. Default is 3600 (1 hour).
#' @param verbose Logical. If TRUE, prints cache hit/miss information.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#' @param comp Competition: "AFLM" (default) or "AFLW".
#'
#' @return A data frame containing AFL fixture and schedule data.
#' @seealso [load_results()], [load_teams()], [load_predictions()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_fixtures(2021:2022)
#'
#'   # Load all fixtures
#'   load_fixtures(seasons = TRUE)
#' })
#' }
#' @export
load_fixtures <- function(seasons = NULL, all = FALSE, use_disk_cache = FALSE,
                          use_mem_cache = TRUE, cache_ttl = 3600, verbose = FALSE,
                          columns = NULL, use_cache = NULL, comp = "AFLM") {
  .validate_afl_comp(comp)

  # Deprecation shim: use_cache → use_mem_cache
  if (!is.null(use_cache)) {
    cli::cli_warn("{.arg use_cache} is deprecated in {.fn load_fixtures}. Use {.arg use_mem_cache} instead.")
    use_mem_cache <- use_cache
  }

  # Process parameters — `all = TRUE` is equivalent to `seasons = TRUE`
  if (all) seasons <- TRUE
  if (is.null(seasons)) {
    seasons <- get_afl_season()
  } else {
    seasons <- validate_seasons(seasons)
  }

  cache_prefix <- if (comp == "AFLM") "fixtures" else paste0("fixtures_", comp)

  out <- .load_with_cache(
    cache_prefix = cache_prefix,
    seasons = seasons,
    fetch_fn = function(s) get_afl_fixtures(s, comp = comp),
    use_cache = use_mem_cache,
    cache_ttl = cache_ttl,
    verbose = verbose,
    columns = columns,
    use_disk_cache = use_disk_cache
  )
  if (nrow(out) > 0) out <- .normalise_team_values(out)
  out
}



#' Load AFL Team and Lineup Data
#'
#' @description Loads AFL team roster and lineup data from the AFL API (cached to disk, not fetched from a torpdata release)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param use_disk_cache Logical. If TRUE (default), caches completed past seasons
#'   to disk so they load instantly on subsequent calls. Current season is always
#'   fetched fresh from the API.
#' @param refresh Logical. If TRUE, clears all caches and fetches fresh data
#'   from the API for all seasons. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#' @param comp Competition: "AFLM" (default) or "AFLW".
#'
#' @return A data frame containing AFL team and player lineup data.
#' @seealso [load_fixtures()], [load_results()], [load_player_details()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_teams(2021:2022)
#' })
#' }
#' @export
load_teams <- function(seasons = get_afl_season(), use_disk_cache = TRUE, refresh = FALSE,
                       columns = NULL, comp = "AFLM") {
  .validate_afl_comp(comp)
  seasons <- validate_seasons(seasons)

  cache_prefix <- if (comp == "AFLM") "teams" else paste0("teams_", comp)

  out <- .load_with_cache(
    cache_prefix = cache_prefix,
    seasons = seasons,
    fetch_fn = function(s) get_afl_lineups(s, comp = comp),
    columns = columns,
    use_disk_cache = use_disk_cache,
    refresh = refresh
  )

  # Derive round_number from match_id if absent or NA (disk-cached data may lack it)
  if (nrow(out) > 0 && "match_id" %in% names(out)) {
    needs_round <- !"round_number" %in% names(out) || anyNA(out$round_number)
    if (needs_round) {
      out$round_number <- .extract_round_from_match_id(out$match_id)
    }
  }

  if (nrow(out) > 0) out <- .normalise_team_values(out)
  out
}

#' Load AFL Match Results Data
#'
#' @description Loads AFL match results and scores from the AFL API (cached to disk, not fetched from a torpdata release)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param use_disk_cache Logical. If TRUE, uses persistent disk cache for faster repeated loads. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#' @param comp Competition: "AFLM" (default) or "AFLW".
#'
#' @return A data frame containing AFL match results and final scores.
#' @seealso [load_fixtures()], [load_predictions()], [load_teams()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_results(2021:2022)
#' })
#' }
#' @export
load_results <- function(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL, comp = "AFLM") {
  .validate_afl_comp(comp)
  seasons <- validate_seasons(seasons)

  cache_prefix <- if (comp == "AFLM") "results" else paste0("results_", comp)

  out <- .load_with_cache(
    cache_prefix = cache_prefix,
    seasons = seasons,
    fetch_fn = function(s) get_afl_results(s, comp = comp),
    columns = columns,
    use_disk_cache = use_disk_cache
  )
  if (nrow(out) > 0) out <- .normalise_team_values(out)
  out
}

#' Load AFL Player Details Data
#'
#' @description Loads AFL player biographical and details data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param use_disk_cache Logical. If TRUE (default), caches completed past seasons
#'   to disk so they load instantly on subsequent calls. Current season is always
#'   fetched fresh from the API.
#' @param refresh Logical. If TRUE, clears all caches and fetches fresh data
#'   from the API for all seasons. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#' @param comp Competition: "AFLM" (default) or "AFLW". AFLW always uses the
#'   per-season fetch path (no batch `fetch_all_fn`) -- `.fetch_all_player_details()`
#'   is AFLM-only internal plumbing not worth complicating for the multi-season
#'   AFLW case, which is rare and fine at per-season speed.
#'
#' @return A data frame containing AFL player biographical details including names, ages, and team affiliations.
#' @seealso [load_player_stats()], [calculate_epr()], [player_game_ratings()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_player_details(2021:2022)
#' })
#' }
#' @export
load_player_details <- function(seasons = get_afl_season(), use_disk_cache = TRUE, refresh = FALSE,
                                columns = NULL, comp = "AFLM") {
  .validate_afl_comp(comp)
  seasons <- validate_seasons(seasons)

  cache_prefix <- if (comp == "AFLM") "player_details" else paste0("player_details_", comp)

  out <- .load_with_cache(
    cache_prefix = cache_prefix,
    seasons = seasons,
    fetch_fn = function(s) get_afl_player_details(s, comp = comp),
    fetch_all_fn = if (comp == "AFLM") .fetch_all_player_details else NULL,
    columns = columns,
    use_disk_cache = use_disk_cache,
    refresh = refresh
  )
  if (nrow(out) > 0) out <- .normalise_team_values(out)
  out
}

#' Load AFL Match Predictions Data
#'
#' @description Loads AFL match predictions and probability data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param rounds A numeric vector associated with given AFL round - defaults to latest round. If set to `TRUE`, returns all available rounds in the given season range.
#' @param use_disk_cache Logical. If TRUE, uses persistent disk cache for faster repeated loads. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing AFL match predictions including win probabilities and expected scores.
#' @seealso [load_fixtures()], [load_results()], [simulate_season()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_predictions(2021:2022)
#' })
#' }
#' @export
load_predictions <- function(seasons = get_afl_season(), rounds = get_afl_week(type = "next"), use_disk_cache = FALSE, columns = NULL) {
  if (isTRUE(seasons) && missing(rounds)) rounds <- TRUE
  seasons <- validate_seasons(seasons)
  rounds <- validate_rounds(rounds)

  urls <- generate_urls("predictions", "predictions", seasons)

  out <- load_from_url(urls, seasons = seasons, rounds = rounds, use_disk_cache = use_disk_cache, columns = columns)

  # Normalise old column names (providerId → match_id, etc.)
  if (nrow(out) > 0) {
    if (!data.table::is.data.table(out)) out <- data.table::as.data.table(out)
    .normalise_predictions_columns(out)
    out <- tibble::as_tibble(out)
  }

  if (nrow(out) > 0) out <- .normalise_team_values(out)
  return(out)
}

#' Load Retrodictions Data
#'
#' @description Loads retrodictions from the [torpdata repository](https://github.com/peteowen1/torpdata).
#' Retrodictions are the current model's predictions for all matches (completed and upcoming),
#' regenerated each pipeline run. Compare with [load_predictions()] which returns locked
#' pre-game predictions frozen at kickoff.
#'
#' @param seasons A numeric vector of 4-digit years - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param rounds A numeric vector of round numbers - defaults to latest round. If set to `TRUE`, returns all available rounds.
#' @param use_disk_cache Logical. If TRUE, uses persistent disk cache. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing retrodictions with the same columns as [load_predictions()].
#' @seealso [load_predictions()], [load_results()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_retrodictions(2026)
#' })
#' }
#' @export
load_retrodictions <- function(seasons = get_afl_season(), rounds = get_afl_week(type = "next"), use_disk_cache = FALSE, columns = NULL) {
  if (isTRUE(seasons) && missing(rounds)) rounds <- TRUE
  seasons <- validate_seasons(seasons)
  rounds <- validate_rounds(rounds)

  urls <- generate_urls("retrodictions", "retrodictions", seasons)

  out <- load_from_url(urls, seasons = seasons, rounds = rounds, use_disk_cache = use_disk_cache, columns = columns)

  if (nrow(out) > 0) {
    if (!data.table::is.data.table(out)) out <- data.table::as.data.table(out)
    .normalise_predictions_columns(out)
    out <- tibble::as_tibble(out)
  }

  if (nrow(out) > 0) out <- .normalise_team_values(out)
  return(out)
}

#' Load TORP Ratings Data
#'
#' @description Loads pre-computed TORP player ratings from the [torpdata repository](https://github.com/peteowen1/torpdata).
#' This data contains per-round TORP ratings for all players, as generated by the ratings pipeline.
#'
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#' @param version Rating vintage to load. `NULL` (default) loads the canonical
#'   ratings, which is what every existing caller gets and what will keep
#'   working unchanged. Pass a label such as `"v2"` to load a candidate vintage
#'   published alongside it (decision D-DEF3 — see
#'   `docs/plans/RATING-VERSIONING-PLAN.md`). An unrecognised label is an error
#'   rather than a silent fall back to canonical, because quietly serving a
#'   different vintage than the caller asked for is the exact failure this
#'   mechanism exists to prevent.
#'
#' @return A data frame containing TORP ratings with columns including
#'   \code{player_id}, \code{player_name}, \code{torp}, \code{season}, \code{round}, and \code{row_id}.
#' @seealso [calculate_epr()], [load_player_game_data()], [player_season_ratings()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_torp_ratings()
#' })
#' }
#' @export
load_torp_ratings <- function(columns = NULL, version = NULL) {
  base_url <- paste0("https://github.com/", get_torp_data_repo(), "/releases/download")
  url <- paste0(base_url, "/ratings-data/", .rating_vintage_file(version))
  out <- load_from_url(url, columns = columns)

  # Normalise old torp_* column names to new *_epr names
  if (nrow(out) > 0) .normalise_columns(out, TORP_RATINGS_COL_MAP)

  if (nrow(out) == 0) {
    cli::cli_warn("No TORP ratings data loaded. The file may not exist yet or the download failed.")
  }
  if (nrow(out) > 0) out <- .normalise_team_values(out)
  out
}

#' Load Player Game Ratings Data
#'
#' @description Loads pre-computed per-game TORP ratings from the
#'   [torpdata repository](https://github.com/peteowen1/torpdata).
#'   This is the output of [player_game_ratings()] — a per-game TORP breakdown
#'   for every player, ready for leaderboards and analysis.
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL
#'   seasons — defaults to latest season. If set to `TRUE`, returns all
#'   available data since 2021.
#' @param rounds A numeric vector of round numbers to filter to, or `TRUE`
#'   (default) for all rounds.
#' @param use_disk_cache Logical. If `TRUE`, uses persistent disk cache for
#'   faster repeated loads. Default is `FALSE`.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing per-game player ratings with columns
#'   including `season`, `round`, `match_id`, `player_id`, `player_name`,
#'   `team`, `total_points`, `recv_points`, `disp_points`, `spoil_points`,
#'   and `hitout_points`.
#' @seealso [player_game_ratings()], [load_player_season_ratings()], [load_torp_ratings()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_player_game_ratings(2024)
#'   load_player_game_ratings(2024, rounds = 1:5)
#' })
#' }
#' @export
load_player_game_ratings <- function(seasons = get_afl_season(), rounds = TRUE, use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)
  rounds <- validate_rounds(rounds)

  urls <- generate_urls("player_game_ratings-data", "player_game_ratings", seasons)

  out <- load_from_url(urls, seasons = seasons, use_disk_cache = use_disk_cache, columns = columns)

  # Post-load round filter (data is stored per-season, not per-round)
  if (!isTRUE(rounds) && "round" %in% names(out)) {
    out <- out[out$round %in% rounds, ]
  }

  # Normalise old display column names (total_points → epv_raw, etc.)
  if (nrow(out) > 0) .normalise_columns(out, PLAYER_GAME_RATINGS_COL_MAP)

  if (nrow(out) > 0) out <- .normalise_team_values(out)
  return(out)
}

#' Load Player Season Ratings Data
#'
#' @description Loads pre-computed season-total TORP ratings from the
#'   [torpdata repository](https://github.com/peteowen1/torpdata).
#'   This is the output of [player_season_ratings()] — season totals and PPG
#'   leaderboards per player.
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL
#'   seasons — defaults to latest season. If set to `TRUE`, returns all
#'   available data since 2021.
#' @param use_disk_cache Logical. If `TRUE`, uses persistent disk cache for
#'   faster repeated loads. Default is `FALSE`.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing season-total player ratings with columns
#'   including `season`, `player_id`, `player_name`, `team`, `position`,
#'   `games`, `season_points`, `season_recv`, `season_disp`, `season_spoil`,
#'   `season_hitout`, and `ppg`.
#' @seealso [player_season_ratings()], [load_player_game_ratings()], [load_torp_ratings()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_player_season_ratings(2024)
#' })
#' }
#' @export
load_player_season_ratings <- function(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("player_season_ratings-data", "player_season_ratings", seasons)

  out <- load_from_url(urls, seasons = seasons, use_disk_cache = use_disk_cache, columns = columns)

  # Normalise old season rating column names
  if (nrow(out) > 0) .normalise_columns(out, PLAYER_GAME_RATINGS_COL_MAP)

  if (nrow(out) > 0) out <- .normalise_team_values(out)
  return(out)
}

#' Load Team Ratings Data
#'
#' @description Loads pre-computed team-level TORP aggregates from the
#'   [torpdata repository](https://github.com/peteowen1/torpdata).
#'   Aggregates are the sum of TORP ratings for each team's top-21 players
#'   (filtered to TORP > 0) per round, with subcategory breakdowns.
#'   Player-level ratings are already centered relative to average, so
#'   team sums are naturally relative to 0.
#'
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing team-level ratings with columns including
#'   `season`, `round`, `team`, `team_torp`, `team_epr_recv`, `team_epr_disp`,
#'   `team_epr_spoil`, `team_epr_hitout`, `top_player`, `top_torp`, and `n_players`.
#' @seealso [load_torp_ratings()], [load_player_game_ratings()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_team_ratings()
#' })
#' }
#' @export
load_team_ratings <- function(columns = NULL) {
  base_url <- paste0("https://github.com/", get_torp_data_repo(), "/releases/download")
  url <- paste0(base_url, "/team_ratings-data/team_ratings.parquet")
  out <- load_from_url(url, columns = columns)

  if (nrow(out) == 0) {
    cli::cli_warn("No team ratings data loaded. The file may not exist yet or the download failed.")
  }
  if (nrow(out) > 0) out <- .normalise_team_values(out)
  out
}

#' Load Injury Data from GitHub Releases
#'
#' Loads historical injury list snapshots from the torpdata repository.
#' Each snapshot is a parquet file saved by [save_injury_data()] during
#' the predictions pipeline.
#'
#' @param seasons A numeric vector of 4-digit years. Defaults to the current
#'   AFL season via [get_afl_season()].
#' @param columns Optional character vector of column names to read.
#' @return A data frame containing injury snapshots with columns including
#'   `player`, `team`, `injury`, `estimated_return`, `updated`,
#'   `player_norm`, `source`, and `scraped_date`.
#' @seealso [get_all_injuries()], [save_injury_data()]
#' @examples
#' \dontrun{
#' try({
#'   load_injury_data(2026)
#' })
#' }
#' @export
load_injury_data <- function(seasons = get_afl_season(), columns = NULL) {
  seasons <- validate_seasons(seasons)
  base_url <- paste0("https://github.com/", get_torp_data_repo(), "/releases/download")
  urls <- paste0(base_url, "/injury-data/injury_list_", seasons, ".parquet")
  out <- load_from_url(urls, seasons = seasons, columns = columns)
  if (nrow(out) > 0) out <- .normalise_team_values(out)
  out
}


#' Load EP/WP Chart Data
#'
#' @description Loads a lightweight subset of play-by-play data optimised for
#'   charting Expected Points (EP) and Win Probability (WP) over a match.
#'   Contains every play but only ~25 columns instead of the full 150+
#'   available from [load_pbp()].
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL
#'   seasons — defaults to latest season. If set to `TRUE`, returns all
#'   available data since 2021.
#' @param rounds A numeric vector associated with given AFL round — defaults to
#'   all rounds. If set to `TRUE`, returns all available rounds in the given
#'   season range.
#' @param use_disk_cache Logical. If `TRUE`, uses persistent disk cache for
#'   faster repeated loads. Default is `FALSE`.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing EP/WP chart data with columns including
#'   `match_id`, `season`, `round_number`, `period`, `total_seconds`,
#'   `home_team_name`, `away_team_name`, `team`, `exp_pts`,
#'   `delta_epv`, `wp`, `wpa`, `description`, `player_name`, `play_type`,
#'   `shot_row`, and `points_shot`.
#' @seealso [load_pbp()], [load_xg()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_ep_wp_charts(2024)
#' })
#' }
#' @export
load_ep_wp_charts <- function(seasons = get_afl_season(), rounds = TRUE, use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)
  rounds <- validate_rounds(rounds)

  urls <- generate_urls("ep_wp_chart-data", "ep_wp_chart", seasons, rounds)

  out <- load_from_url(urls, seasons = seasons, rounds = rounds, use_disk_cache = use_disk_cache, columns = columns)

  if (nrow(out) > 0) out <- .normalise_team_values(out)
  return(out)
}

#' Load Player Stat Ratings Data
#'
#' @description Loads pre-computed Bayesian player stat rating estimates from the
#'   [torpdata repository](https://github.com/peteowen1/torpdata).
#'   Stat ratings are per-stat estimates with credible intervals, produced by
#'   \code{estimate_player_stat_ratings()}.
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL
#'   seasons -- defaults to latest season. If set to `TRUE`, returns all
#'   available data since 2021.
#' @param use_disk_cache Logical. If `TRUE`, uses persistent disk cache for
#'   faster repeated loads. Default is `FALSE`.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing player stat rating estimates with columns
#'   including `player_id`, `player_name`, `pos_group`, `n_games`,
#'   `wt_games`, `ref_date`, and `{stat}_rating`, `{stat}_lower`,
#'   `{stat}_upper` for each estimated stat.
#' @seealso [estimate_player_stat_ratings()], [player_stat_rating_profile()], [load_player_game_ratings()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_player_stat_ratings(2024)
#' })
#' }
#' @export
load_player_stat_ratings <- function(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("player_stat_ratings-data", "player_stat_ratings", seasons)

  out <- load_from_url(urls, seasons = seasons, use_disk_cache = use_disk_cache, columns = columns)

  # Map old *_skill columns to *_rating for backward compat with old parquets
  .normalise_stat_rating_columns(out)

  if (nrow(out) > 0) out <- .normalise_team_values(out)
  return(out)
}

#' @rdname load_player_stat_ratings
#' @export
load_player_skills <- load_player_stat_ratings


#' Load Player Skill Ratings (PSR)
#'
#' @description Loads pre-computed PSR (Player Skill Ratings) from the
#'   [torpdata repository](https://github.com/peteowen1/torpdata).
#'   PSR represents each player's predicted contribution to match margin
#'   based on their skill profile.
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL
#'   seasons — defaults to latest season. If set to `TRUE`, returns all
#'   available data since 2021.
#' @param use_disk_cache Logical. If `TRUE`, uses persistent disk cache for
#'   faster repeated loads. Default is `FALSE`.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing PSR data with columns including
#'   \code{player_id}, \code{player_name}, \code{season}, \code{round},
#'   \code{pos_group}, \code{psr_raw}, and \code{psr}.
#' @seealso [calculate_psr()], [load_player_stat_ratings()], [load_torp_ratings()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_psr(2024)
#'   load_psr(TRUE)  # all seasons
#' })
#' }
#' @export
load_psr <- function(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("psr-data", "psr", seasons)

  out <- load_from_url(urls, seasons = seasons, use_disk_cache = use_disk_cache, columns = columns)

  if (nrow(out) > 0) out <- .normalise_team_values(out)
  return(out)
}


#' Load AFLW Player Skill Ratings (PSR)
#'
#' @description Loads pre-computed AFLW PSR from the
#'   [torpdata repository](https://github.com/peteowen1/torpdata), release tag
#'   `aflw_psr-data`. The AFLW equivalent of [load_psr()], kept as its own
#'   release rather than a comp column on `psr-data` so the men's published
#'   artifact -- which live consumers read -- is never rewritten by an AFLW run.
#'
#' @details Deliberately does NOT call `validate_seasons()`. That helper floors
#'   at `AFL_MIN_SEASON` (2021) because that is where torp's men's chain data
#'   starts, but AFLW's box-score history runs from 2018 -- passing it through
#'   would abort on exactly the three earliest seasons this release exists to
#'   carry.
#'
#' @param seasons A numeric vector of 4-digit years, or `TRUE` for every
#'   available AFLW season (2018 onwards).
#' @param use_disk_cache Logical. If `TRUE`, uses persistent disk cache for
#'   faster repeated loads. Default is `FALSE`.
#' @param columns Optional character vector of column names to read. If NULL
#'   (default), reads all columns.
#'
#' @return A data frame of AFLW PSR with columns including \code{player_id},
#'   \code{player_name}, \code{season}, \code{round}, \code{pos_group},
#'   \code{psr_raw}, and \code{psr}.
#' @seealso [load_psr()], [calculate_psr()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_aflw_psr(2025)
#'   load_aflw_psr(TRUE)  # all AFLW seasons
#' })
#' }
#' @export
load_aflw_psr <- function(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL) {
  seasons <- .validate_aflw_seasons(seasons)

  urls <- generate_urls("aflw_psr-data", "aflw_psr", seasons)

  out <- load_from_url(urls, seasons = seasons, use_disk_cache = use_disk_cache, columns = columns)

  if (nrow(out) > 0) out <- .normalise_team_values(out)
  return(out)
}


#' Validate a season vector against AFLW's own history
#'
#' AFLW's first season is 2018, three years before `AFL_MIN_SEASON`. This
#' mirrors [validate_seasons()] but against the right floor.
#'
#' @param seasons Numeric vector of seasons, or TRUE for all AFLW seasons.
#' @return An integer vector of seasons.
#' @keywords internal
.validate_aflw_seasons <- function(seasons) {
  current_year <- as.integer(format(Sys.Date(), "%Y"))

  if (isTRUE(seasons)) return(AFLW_MIN_SEASON:current_year)

  if (!is.numeric(seasons)) {
    cli::cli_abort("Seasons must be numeric values or TRUE")
  }

  invalid <- seasons[seasons < AFLW_MIN_SEASON | seasons > current_year]
  if (length(invalid) > 0) {
    # Pre-formatted, not interpolated with a {?s} plural marker: cli cannot
    # resolve which quantity to pluralise on when several values are
    # interpolated in one string, and aborts with "Multiple quantities for
    # pluralization" -- turning a clear validation error into a cli internal one.
    bad <- paste(invalid, collapse = ", ")
    cli::cli_abort(paste0(
      "Invalid AFLW season year(s): ", bad,
      ". Must be between ", AFLW_MIN_SEASON, " and ", current_year))
  }

  seasons
}


#' Load Weather Data
#'
#' Downloads historical match weather data from the torpdata GitHub release.
#' Returns a tibble with match-level weather aggregates (temp, wind,
#' humidity, precipitation) derived from Open-Meteo archive data.
#'
#' @return A tibble with columns: match_id, temp_avg, wind_avg,
#'   humidity_avg, precipitation_total, is_roof, and additional metadata.
#' @seealso [save_to_release()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_weather()
#' })
#' }
#' @export
load_weather <- function() {
  file_reader("weather_data", "weather-data")
}
