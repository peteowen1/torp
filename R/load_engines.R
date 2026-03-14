#' Load parquet files from remote URLs
#'
#' @description This function is intended for internal use and may be unexported in a future release.
#' @param url A vector of URLs to load into memory. If more than one URL provided, will row-bind them.
#' @param seasons A numeric vector of years that will be used to filter the dataframe's `season` column. If `TRUE` (default), does not filter.
#' @param rounds A numeric vector of rounds that will be used to filter the dataframe's `round` column. If `TRUE` (default), does not filter.
#' @param use_disk_cache Logical. If TRUE, uses persistent disk cache for faster repeated loads.
#' @param columns Optional character vector of column names to read. If NULL (default),
#'   reads all columns. Filter columns (season, round, round_number, week) are
#'   auto-included when filtering is active.
#' @param ... Additional arguments (currently unused).
#'
#' @return A tibble
#' @export
#' @importFrom data.table rbindlist setDT
#' @importFrom tibble as_tibble
load_from_url <- function(url, ..., seasons = TRUE, rounds = TRUE, use_disk_cache = FALSE, columns = NULL) {
  url <- as.character(url)

  cache_opts <- get_disk_cache_options()
  use_cache <- use_disk_cache && cache_opts$enabled
  max_age <- cache_opts$max_age_days

  # Auto-include filter columns when columns is specified and filtering is active
  read_cols <- columns
  if (!is.null(read_cols)) {
    if (!isTRUE(seasons)) read_cols <- union(read_cols, "season")
    if (!isTRUE(rounds))  read_cols <- union(read_cols, c("round", "round_number", "week"))
  }

  if (length(url) == 1) {
    out <- parquet_from_url_cached(url, use_cache = use_cache, max_age_days = max_age, columns = read_cols)
  } else {
    out <- parquet_from_urls_parallel(url, use_cache = use_cache, max_age_days = max_age, columns = read_cols)
  }

  if (nrow(out) == 0 && length(url) > 0) {
    cli::cli_warn("No data loaded from {length(url)} URL{?s}. Check seasons/rounds or use {.fn clear_skip_markers} to retry previously failed files.")
  }

  # Filter by season/round if specific values requested
  if (!isTRUE(seasons)) {
    stopifnot(is.numeric(seasons))
    if ("season" %in% names(out)) out <- out[out$season %in% seasons, ]
  }
  if (!isTRUE(rounds)) {
    stopifnot(is.numeric(rounds))
    if ("round" %in% names(out) && !all(is.na(out$round))) {
      out <- out[out$round %in% rounds, ]
    } else if ("round_number" %in% names(out) && !all(is.na(out$round_number))) {
      out <- out[out$round_number %in% rounds, ]
    } else if ("week" %in% names(out) && !all(is.na(out$week))) {
      out <- out[out$week %in% rounds, ]
    } else if (nrow(out) > 0 && !setequal(rounds, 0:28)) {
      # Only warn when specific rounds were requested but no round column exists.
      # When rounds = 0:28 (from validate_rounds(TRUE)), filtering is a no-op anyway.
      cli::cli_warn("Round filtering requested but no round column found in data. Returning unfiltered. Available columns: {.val {head(names(out), 10)}}")
    }
  }

  # Drop auto-added filter columns that weren't originally requested
  if (!is.null(columns)) {
    keep <- intersect(names(out), columns)
    if (length(keep) == 0 && nrow(out) > 0) {
      cli::cli_warn("None of the requested columns ({.val {columns}}) found in data. Available: {.val {head(names(out), 10)}}")
    }
    if (length(keep) > 0) out <- out[, ..keep]
  }

  out <- tibble::as_tibble(out)

  return(out)
}

#' Download multiple parquet files in parallel using curl
#'
#' Always checks local `torpdata/data/` first for each URL (with smart
#' staleness), then optionally checks disk cache, then downloads missing
#' files in parallel. Local files are batch-read via `arrow::open_dataset()`
#' for speed. Downloaded data is auto-saved to local storage.
#'
#' @param urls Character vector of URLs
#' @param use_cache Logical. If TRUE, also check/write the `~/.torp/cache/` disk cache.
#' @param max_age_days Maximum disk cache age in days.
#' @param columns Optional character vector of column names to select.
#'
#' @return A data.table with all files combined
#' @keywords internal
#' @importFrom curl multi_download
#' @importFrom data.table rbindlist setDT
parquet_from_urls_parallel <- function(urls, use_cache = FALSE, max_age_days = 7, columns = NULL) {
  n <- length(urls)

  # --- Phase 1: Resolve each URL to a local file path or mark for download ---
  local_paths <- character(n)     # local path if available, "" otherwise
  download_indices <- integer(0)

  n_skipped <- 0L
  for (i in seq_len(n)) {
    # Skip known-bad files (negative cache)
    if (is_download_skippable(urls[i])) {
      n_skipped <- n_skipped + 1L
      next
    }

    # Check local torpdata/data/ (smart staleness)
    local_max_age <- local_max_age_for_url(urls[i])
    if (is_locally_stored(urls[i], local_max_age)) {
      lp <- get_local_path(urls[i])
      if (!is.null(lp) && file.exists(lp) && file.size(lp) >= MIN_PARQUET_BYTES) {
        local_paths[i] <- lp
        next
      }
    }

    # Check disk cache (opt-in)
    if (use_cache && is_disk_cached(urls[i], max_age_days)) {
      cp <- get_disk_cache_path(urls[i])
      if (!is.null(cp) && file.exists(cp) && file.size(cp) >= MIN_PARQUET_BYTES) {
        local_paths[i] <- cp
        next
      }
    }

    download_indices <- c(download_indices, i)
  }

  if (n_skipped > 0) {
    cli::cli_inform("Skipped {n_skipped} previously failed URL{?s}. Use {.fn clear_skip_markers} to retry.")
  }

  # --- Phase 2: Batch-read all local files with open_dataset() ---
  valid_paths <- local_paths[nchar(local_paths) > 0]
  local_dt <- data.table::data.table()

  if (length(valid_paths) > 0) {
    local_dt <- tryCatch({
      ds <- arrow::open_dataset(valid_paths, format = "parquet", unify_schemas = TRUE)
      if (!is.null(columns)) {
        ds <- dplyr::select(ds, dplyr::any_of(columns))
      }
      out <- dplyr::collect(ds)
      data.table::setDT(out)
      out
    }, error = function(e) {
      NULL
    })

    # Sequential fallback (outside tryCatch to avoid nested error handler issues with Arrow)
    if (is.null(local_dt)) {
      parts <- lapply(valid_paths, function(p) {
        tryCatch({
          if (!is.null(columns)) {
            d <- arrow::read_parquet(p, col_select = dplyr::any_of(columns))
          } else {
            d <- arrow::read_parquet(p)
          }
          data.table::setDT(d)
          d
        }, error = function(e) {
          cli::cli_warn("Failed to read local file {.path {basename(p)}}: {conditionMessage(e)}")
          data.table::data.table()
        })
      })
      n_failed <- sum(vapply(parts, function(p) nrow(p) == 0L, logical(1)))
      if (n_failed > 0) {
        cli::cli_warn("{n_failed} of {length(valid_paths)} local file{?s} failed to read. Results may be incomplete.")
      }
      local_dt <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
    }
  }

  # --- Phase 3: Download missing files in parallel ---
  dl_dt <- data.table::data.table()

  if (length(download_indices) > 0) {
    urls_to_dl <- urls[download_indices]
    tmp_files <- vapply(urls_to_dl, function(u) tempfile(fileext = ".parquet"), character(1))

    cli::cli_inform("Downloading {length(urls_to_dl)} file{?s} in parallel...")
    dl <- tryCatch(
      curl::multi_download(urls_to_dl, destfiles = tmp_files),
      error = function(e) {
        if (nrow(local_dt) == 0) {
          cli::cli_abort("Download failed and no local data available: {conditionMessage(e)}")
        }
        cli::cli_warn("Download failed (using local data only): {conditionMessage(e)}")
        NULL
      }
    )

    if (!is.null(dl)) {
      dl_parts <- list()

      for (j in seq_along(download_indices)) {
        idx <- download_indices[j]
        if (isTRUE(dl$success[j]) && file.exists(tmp_files[j]) &&
            file.size(tmp_files[j]) >= MIN_PARQUET_BYTES) {
          tryCatch({
            dt <- arrow::read_parquet(tmp_files[j])
            data.table::setDT(dt)

            if (nrow(dt) > 0) {
              # Apply column selection
              if (!is.null(columns)) {
                cols_present <- intersect(columns, names(dt))
                if (length(cols_present) > 0) {
                  dl_parts[[length(dl_parts) + 1L]] <- dt[, ..cols_present]
                } else {
                  cli::cli_warn("None of the requested columns found in {.url {basename(urls[idx])}}. Available: {.val {head(names(dt), 10)}}")
                }
              } else {
                dl_parts[[length(dl_parts) + 1L]] <- dt
              }

              # Save full data to local storage (best-effort)
              write_local_parquet(urls[idx], dt)

              # Also cache to disk if enabled
              if (use_cache) write_disk_cache(urls[idx], dt)
            }
          }, error = function(e) {
            cli::cli_warn("Failed to read downloaded file {.url {basename(urls[idx])}}: {conditionMessage(e)}")
            # Do NOT mark as skippable -- this was a read error, not a missing file (404)
          })
        } else {
          # Only mark as skippable for HTTP 404, not transient errors
          status <- dl$status_code[j]
          if (!is.na(status) && status == 404) {
            mark_download_skippable(urls[idx])
          }
          if (!isTRUE(dl$success[j])) {
            cli::cli_warn("Failed to download {.url {urls[idx]}}")
          }
        }
      }

      unlink(tmp_files)

      if (length(dl_parts) > 0) {
        dl_dt <- data.table::rbindlist(dl_parts, use.names = TRUE, fill = TRUE)
      }
    }
  }

  # --- Phase 4: Combine local + downloaded ---
  if (nrow(local_dt) == 0 && nrow(dl_dt) == 0) return(data.table::data.table())
  if (nrow(dl_dt) == 0) return(local_dt)
  if (nrow(local_dt) == 0) return(dl_dt)

  data.table::rbindlist(list(local_dt, dl_dt), use.names = TRUE, fill = TRUE)
}

#' Load parquet file from a remote connection with local-first loading
#'
#' Always checks local `torpdata/data/` first (with smart staleness), then
#' optionally checks the disk cache, then downloads. Downloaded data is
#' auto-saved to local storage for next time.
#'
#' @param url A character URL
#' @param use_cache Logical. If TRUE, also check/write the `~/.torp/cache/` disk cache.
#' @param max_age_days Maximum age for disk cache files in days.
#' @param columns Optional character vector of column names to select.
#'
#' @return A data frame
#' @keywords internal
#' @importFrom cli cli_warn cli_abort
#' @importFrom data.table data.table setDT
parquet_from_url_cached <- function(url, use_cache = TRUE, max_age_days = 7, columns = NULL) {
  # 0. Negative cache — skip known-bad URLs
  if (is_download_skippable(url)) {
    cli::cli_warn(c(
      "Skipping previously failed URL: {.url {basename(url)}}",
      "i" = "Run {.fn clear_skip_markers} to retry."
    ))
    return(data.table::data.table())
  }

  # 1. ALWAYS check local torpdata/data/ first (smart staleness per URL)
  local_max_age <- local_max_age_for_url(url)
  if (is_locally_stored(url, local_max_age)) {
    local_data <- read_local_parquet(url, columns = columns)
    if (!is.null(local_data)) {
      data.table::setDT(local_data)
      return(local_data)
    }
  }

  # 2. Check disk cache (opt-in)
  if (use_cache && is_disk_cached(url, max_age_days)) {
    cached_data <- read_disk_cache(url, columns = columns)
    if (!is.null(cached_data)) {
      data.table::setDT(cached_data)
      return(cached_data)
    }
  }

  # 3. Download from URL
  result <- parquet_from_url(url)

  # Mark as skippable only for confirmed 404s (not transient errors)
  if (nrow(result) == 0) {
    if (identical(attr(result, "skip_reason"), "not_found")) {
      mark_download_skippable(url)
    }
    return(result)
  }

  # Auto-save full data to local storage
  write_local_parquet(url, result)

  # Also cache to disk cache if enabled
  if (use_cache) {
    write_disk_cache(url, result)
  }

  # Apply column selection on the downloaded data
  if (!is.null(columns)) {
    cols_present <- intersect(columns, names(result))
    if (length(cols_present) > 0) {
      result <- result[, ..cols_present]
    } else {
      cli::cli_warn("None of the requested columns ({.val {columns}}) found in downloaded data. Available: {.val {head(names(result), 10)}}")
    }
  }

  return(result)
}

#' Load parquet file from a remote connection
#'
#' @description This function is intended for internal use and may be unexported in a future release.
#' @param url A character URL
#'
#' @return A data frame
#' @export
#' @importFrom cli cli_warn cli_abort
#' @importFrom data.table data.table setDT
parquet_from_url <- function(url) {
  # Validate URL format
  if (!is.character(url) || length(url) != 1 || nchar(url) == 0) {
    cli::cli_abort("URL must be a single non-empty character string")
  }

  if (!grepl("^https?://", url)) {
    cli::cli_abort("URL must start with http:// or https://")
  }

  tryCatch({
    # Arrow can read parquet directly from URL
    load <- arrow::read_parquet(url)

    # Validate that we got actual data
    if (is.null(load)) {
      cli::cli_warn("No data returned from {.url {url}}")
      return(data.table::data.table())
    }

    data.table::setDT(load)
    return(load)

  }, error = function(e) {
    error_msg <- conditionMessage(e)
    result <- data.table::data.table()

    if (grepl("404|Not Found", error_msg, ignore.case = TRUE)) {
      cli::cli_warn("Data file not found at {.url {url}} - file may not exist for this season/round combination")
      attr(result, "skip_reason") <- "not_found"
    } else if (grepl("timeout|timed out", error_msg, ignore.case = TRUE)) {
      cli::cli_warn("Connection timeout while downloading from {.url {url}} - please try again")
      attr(result, "skip_reason") <- "transient"
    } else if (grepl("cannot open|connection", error_msg, ignore.case = TRUE)) {
      cli::cli_warn("Failed to connect to {.url {url}} - check internet connection")
      attr(result, "skip_reason") <- "transient"
    } else {
      cli::cli_warn("Failed to load data from {.url {url}}: {error_msg}")
      attr(result, "skip_reason") <- "transient"
    }

    return(result)
  })
}
