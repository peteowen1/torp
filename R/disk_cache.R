# Persistent Disk Cache System for TORP Package
#
# Provides file-based caching for downloaded data to enable
# near-instant repeated loads without network requests.

# Package-level disk cache environment for settings
.torp_disk_cache_env <- new.env(parent = emptyenv())

#' Get Disk Cache Directory
#'
#' Returns the directory used for persistent disk caching.
#' Creates the directory if it doesn't exist.
#'
#' @return Character string of the cache directory path
#' @keywords internal
get_disk_cache_dir <- function() {

  cache_dir <- file.path(Sys.getenv("HOME", path.expand("~")), ".torp", "cache")

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  return(cache_dir)
}

#' Generate Disk Cache Key from URL
#'
#' Creates a unique cache key based on the URL hash.
#'
#' @param url Character URL to generate cache key for
#' @return Character cache key (filename without extension)
#' @keywords internal
#' @importFrom tools file_path_sans_ext
generate_disk_cache_key <- function(url) {
  # Use digest for consistent hashing
  hash <- digest::digest(url, algo = "md5")

  # Extract meaningful parts from URL for human-readable prefix
  # e.g., chains_data_2024_01 from the URL
  url_parts <- basename(url)
  file_prefix <- tools::file_path_sans_ext(url_parts)


  # Combine prefix with hash for unique but identifiable key
  paste0(file_prefix, "_", substr(hash, 1, 8))
}

#' Get Disk Cache Path for URL
#'
#' Returns the full path to the cached file for a given URL.
#'
#' @param url Character URL
#' @return Character path to cache file
#' @keywords internal
get_disk_cache_path <- function(url) {
  cache_dir <- get_disk_cache_dir()
  cache_key <- generate_disk_cache_key(url)
  file.path(cache_dir, paste0(cache_key, ".parquet"))
}

#' Check if URL is Cached on Disk
#'
#' @param url Character URL to check
#' @param max_age_days Maximum age in days for cache to be considered valid.
#'   Default is 7 days. Set to NULL for no expiration.
#' @return Logical indicating if valid cache exists
#' @keywords internal
is_disk_cached <- function(url, max_age_days = 7) {
  cache_path <- get_disk_cache_path(url)

  if (!file.exists(cache_path)) {
    return(FALSE)
  }

  # Check age if max_age_days is specified

  if (!is.null(max_age_days)) {
    file_info <- file.info(cache_path)
    age_days <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "days"))

    if (age_days > max_age_days) {
      return(FALSE)
    }
  }

  return(TRUE)
}

#' Read Data from Disk Cache
#'
#' @param url Character URL to read cached data for
#' @return Data frame if cache exists, NULL otherwise
#' @keywords internal
read_disk_cache <- function(url) {
  cache_path <- get_disk_cache_path(url)

  if (!file.exists(cache_path)) {
    return(NULL)
  }

  tryCatch({
    arrow::read_parquet(cache_path)
  }, error = function(e) {
    cli::cli_warn("Failed to read cache file: {cache_path}")
    # Remove corrupted cache file
    unlink(cache_path)
    return(NULL)
  })
}

#' Write Data to Disk Cache
#'
#' @param url Character URL as cache key
#' @param data Data to cache
#' @return Invisible NULL
#' @keywords internal
write_disk_cache <- function(url, data) {
  cache_path <- get_disk_cache_path(url)

  tryCatch({
    arrow::write_parquet(data, cache_path)
  }, error = function(e) {
    cli::cli_warn("Failed to write cache file: {cache_path}")
  })

  invisible(NULL)
}

#' Clear Disk Cache
#'
#' Removes cached files from the disk cache directory.
#'
#' @param pattern Optional regex pattern to match specific cache files.
#'   If NULL (default), clears all cached files.
#' @param older_than_days Only clear files older than this many days.
#'   If NULL (default), clears regardless of age.
#' @param verbose Logical. If TRUE, prints information about cleared files.
#'
#' @return Invisible count of files removed
#' @export
#'
#' @examples
#' \donttest{
#' # Clear all disk cache
#' clear_disk_cache()
#'
#' # Clear only chains data cache
#' clear_disk_cache(pattern = "chains")
#'
#' # Clear files older than 30 days
#' clear_disk_cache(older_than_days = 30)
#'
#' # Clear with verbose output
#' clear_disk_cache(verbose = TRUE)
#' }
clear_disk_cache <- function(pattern = NULL, older_than_days = NULL, verbose = FALSE) {
  cache_dir <- get_disk_cache_dir()

  if (!dir.exists(cache_dir)) {
    if (verbose) {
      cli::cli_inform("No cache directory exists")
    }
    return(invisible(0L))
  }

  # List cache files
  cache_files <- list.files(cache_dir, pattern = "\\.parquet$", full.names = TRUE)

  if (length(cache_files) == 0) {
    if (verbose) {
      cli::cli_inform("No cache files to clear")
    }
    return(invisible(0L))
  }

  # Filter by pattern if specified

  if (!is.null(pattern)) {
    matching <- grepl(pattern, basename(cache_files), ignore.case = TRUE)
    cache_files <- cache_files[matching]
  }

  # Filter by age if specified
  if (!is.null(older_than_days) && length(cache_files) > 0) {
    file_info <- file.info(cache_files)
    age_days <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "days"))
    cache_files <- cache_files[age_days > older_than_days]
  }

  if (length(cache_files) == 0) {
    if (verbose) {
      cli::cli_inform("No cache files match the specified criteria")
    }
    return(invisible(0L))
  }

  # Remove files
  removed <- unlink(cache_files)

  if (verbose) {
    cli::cli_inform("Cleared {length(cache_files)} cache file{?s}")
  }

  invisible(length(cache_files))
}

#' Get Disk Cache Information
#'
#' Returns information about the current disk cache state.
#'
#' @return A data frame with cache information including file names, sizes, and ages
#' @export
#'
#' @examples
#' \donttest{
#' # Check disk cache status
#' get_disk_cache_info()
#' }
get_disk_cache_info <- function() {
  cache_dir <- get_disk_cache_dir()

  if (!dir.exists(cache_dir)) {
    return(data.frame(
      file = character(0),
      size_mb = numeric(0),
      age_days = numeric(0),
      created = character(0),
      stringsAsFactors = FALSE
    ))
  }

  cache_files <- list.files(cache_dir, pattern = "\\.parquet$", full.names = TRUE)

  if (length(cache_files) == 0) {
    return(data.frame(
      file = character(0),
      size_mb = numeric(0),
      age_days = numeric(0),
      created = character(0),
      stringsAsFactors = FALSE
    ))
  }

  file_info <- file.info(cache_files)

  data.frame(
    file = basename(cache_files),
    size_mb = round(file_info$size / 1024 / 1024, 2),
    age_days = round(as.numeric(difftime(Sys.time(), file_info$mtime, units = "days")), 1),
    created = format(file_info$mtime, "%Y-%m-%d %H:%M"),
    stringsAsFactors = FALSE
  )
}

#' Get Disk Cache Size
#'
#' Returns the total size of the disk cache in megabytes.
#'
#' @return Numeric total cache size in MB
#' @export
#'
#' @examples
#' \donttest{
#' # Get total cache size
#' get_disk_cache_size()
#' }
get_disk_cache_size <- function() {
  cache_dir <- get_disk_cache_dir()

  if (!dir.exists(cache_dir)) {
    return(0)
  }

  cache_files <- list.files(cache_dir, pattern = "\\.parquet$", full.names = TRUE)

  if (length(cache_files) == 0) {
    return(0)
  }

  file_info <- file.info(cache_files)
  round(sum(file_info$size) / 1024 / 1024, 2)
}

#' Set Disk Cache Options
#'
#' Configure disk cache behavior for the current session.
#'
#' @param enabled Logical. Enable or disable disk caching.
#' @param max_age_days Numeric. Maximum age in days for cached files.
#'   Files older than this will be re-downloaded.
#'
#' @return Invisible list of current settings
#' @export
#'
#' @examples
#' \donttest{
#' # Disable disk caching
#' set_disk_cache_options(enabled = FALSE)
#'
#' # Set cache to expire after 30 days
#' set_disk_cache_options(max_age_days = 30)
#' }
set_disk_cache_options <- function(enabled = TRUE, max_age_days = 7) {
  .torp_disk_cache_env$enabled <- enabled
  .torp_disk_cache_env$max_age_days <- max_age_days

  invisible(list(
    enabled = enabled,
    max_age_days = max_age_days
  ))
}

#' Get Disk Cache Options
#'
#' Returns current disk cache settings.
#'
#' @return List with enabled and max_age_days settings
#' @keywords internal
get_disk_cache_options <- function() {
  list(
    enabled = isTRUE(.torp_disk_cache_env$enabled %||% TRUE),
    max_age_days = .torp_disk_cache_env$max_age_days %||% 7
  )
}

