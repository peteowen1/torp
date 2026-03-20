# Cache Management System for TORP Package

# Create package-level cache environments
.torp_cache <- new.env(parent = emptyenv())
.torp_model_cache <- new.env(parent = emptyenv())

#' Clear Data Cache
#'
#' Clears all cached data from memory (fixtures, results, teams,
#' player stats, player details, and AFL API lookups).
#'
#' @param verbose Logical. If TRUE, prints cache clearing information.
#' @return Invisible NULL
#' @export
#' @examples
#' \donttest{
#' clear_data_cache()
#' clear_data_cache(verbose = TRUE)
#' }
clear_data_cache <- function(verbose = FALSE) {
  cache_keys <- ls(.torp_cache)

  if (length(cache_keys) > 0) {
    rm(list = cache_keys, envir = .torp_cache)
    if (verbose) {
      cli::cli_inform("Cleared {length(cache_keys)} data cache entr{?y/ies}")
    }
  } else {
    if (verbose) {
      cli::cli_inform("No data cache entries to clear")
    }
  }

  invisible(NULL)
}


#' Get Cache Information
#'
#' Returns information about all data cached in memory (fixtures, results,
#' teams, player stats, player details, and AFL API lookups).
#'
#' @return A data frame with cache information including keys, timestamps, and data sizes
#' @export
#' @examples
#' \donttest{
#' get_cache_info()
#' }
get_cache_info <- function() {
  cache_keys <- ls(.torp_cache)

  if (length(cache_keys) == 0) {
    return(data.frame(
      cache_key = character(0),
      timestamp = character(0),
      age_seconds = numeric(0),
      data_rows = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  info_list <- lapply(cache_keys, function(key) {
    cache_entry <- get(key, envir = .torp_cache)

    # Cache entries with $data and $timestamp are load_* caches
    if (is.list(cache_entry) && !is.null(cache_entry$timestamp)) {
      age_seconds <- as.numeric(difftime(Sys.time(), cache_entry$timestamp, units = "secs"))
      n_rows <- if (is.data.frame(cache_entry$data)) nrow(cache_entry$data) else NA_integer_
    } else {
      age_seconds <- NA_real_
      n_rows <- NA_integer_
    }

    data.frame(
      cache_key = key,
      timestamp = if (!is.na(age_seconds)) format(cache_entry$timestamp) else NA_character_,
      age_seconds = round(age_seconds, 1),
      data_rows = n_rows,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, info_list)
}

#' Check if cache entry is valid
#'
#' @param cache_entry Cache entry object
#' @param cache_ttl Time-to-live in seconds
#' @return Logical indicating if cache is valid
#' @keywords internal
is_cache_valid <- function(cache_entry, cache_ttl) {
  if (is.null(cache_entry)) {
    return(FALSE)
  }
  
  age_seconds <- as.numeric(difftime(Sys.time(), cache_entry$timestamp, units = "secs"))
  return(age_seconds <= cache_ttl)
}

#' Store data in cache
#'
#' @param cache_key Character cache key
#' @param data Data to cache
#' @return Invisible NULL
#' @keywords internal
store_in_cache <- function(cache_key, data) {
  cache_entry <- list(
    data = data,
    timestamp = Sys.time()
  )
  
  assign(cache_key, cache_entry, envir = .torp_cache)
  invisible(NULL)
}

#' Get data from cache
#'
#' @param cache_key Character cache key
#' @return Cached data or NULL if not found
#' @keywords internal
get_from_cache <- function(cache_key) {
  if (exists(cache_key, envir = .torp_cache)) {
    cache_entry <- get(cache_key, envir = .torp_cache)
    return(cache_entry$data)
  }
  return(NULL)
}


# Model Cache Functions
# ---------------------

#' Clear Model Cache
#'
#' Clears all cached models from memory.
#'
#' @param verbose Logical. If TRUE, prints cache clearing information.
#' @return Invisible NULL
#' @export
#' @examples
#' \donttest{
#' # Clear all cached models
#' clear_model_cache()
#' }
clear_model_cache <- function(verbose = FALSE) {
  cache_keys <- ls(envir = .torp_model_cache)

  if (length(cache_keys) > 0) {
    rm(list = cache_keys, envir = .torp_model_cache)
    if (verbose) {
      cli::cli_inform("Cleared {length(cache_keys)} model cache entr{?y/ies}")
    }
  } else {
    if (verbose) {
      cli::cli_inform("No model cache entries to clear")
    }
  }

  invisible(NULL)
}

#' Clear All Caches
#'
#' Clears all torp caches: in-memory data, in-memory models, and on-disk
#' parquet files. This is the nuclear option — use when you want a completely
#' fresh start (e.g. after a package update that changes column schemas).
#'
#' @param verbose Logical. If TRUE, prints cache clearing information.
#' @return Invisible NULL
#' @export
#' @examples
#' \donttest{
#' clear_all_cache()
#' clear_all_cache(verbose = TRUE)
#' }
clear_all_cache <- function(verbose = FALSE) {
  clear_data_cache(verbose = verbose)
  clear_model_cache(verbose = verbose)
  clear_disk_cache(verbose = verbose)

  if (verbose) {
    cli::cli_inform("All torp caches cleared (data, model, disk)")
  }

  invisible(NULL)
}

#' Get Model Cache Info
#'
#' Returns information about cached models.
#'
#' @return Character vector of cached model names
#' @export
get_model_cache_info <- function() {
  ls(envir = .torp_model_cache)
}