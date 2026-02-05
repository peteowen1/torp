# Cache Management System for TORP Package

# Create package-level cache environments
.torp_cache <- new.env(parent = emptyenv())
.torp_model_cache <- new.env(parent = emptyenv())

#' Clear Fixture Cache
#'
#' Clears the cached fixture data from memory.
#'
#' @param verbose Logical. If TRUE, prints cache clearing information.
#' @return Invisible NULL
#' @export
#' @examples
#' \donttest{
#' # Clear all cached fixtures
#' clear_fixture_cache()
#' 
#' # Clear with verbose output
#' clear_fixture_cache(verbose = TRUE)
#' }
clear_fixture_cache <- function(verbose = FALSE) {
  cache_keys <- ls(.torp_cache, pattern = "^fixtures_")
  
  if (length(cache_keys) > 0) {
    rm(list = cache_keys, envir = .torp_cache)
    if (verbose) {
      cli::cli_inform("Cleared {length(cache_keys)} fixture cache entr{?y/ies}")
    }
  } else {
    if (verbose) {
      cli::cli_inform("No fixture cache entries to clear")
    }
  }
  
  invisible(NULL)
}

#' Get Cache Information
#'
#' Returns information about the current fixture cache state.
#'
#' @return A data frame with cache information including keys, timestamps, and data sizes
#' @export
#' @examples
#' \donttest{
#' # Check current cache status
#' get_cache_info()
#' }
get_cache_info <- function() {
  cache_keys <- ls(.torp_cache, pattern = "^fixtures_")
  
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
    age_seconds <- as.numeric(difftime(Sys.time(), cache_entry$timestamp, units = "secs"))
    
    data.frame(
      cache_key = key,
      timestamp = format(cache_entry$timestamp),
      age_seconds = round(age_seconds, 1),
      data_rows = nrow(cache_entry$data),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, info_list)
}

#' Generate cache key for fixture data
#'
#' @param seasons Seasons parameter
#' @param all All parameter
#' @return Character cache key
#' @keywords internal
generate_fixture_cache_key <- function(seasons, all) {
  if (all) {
    return("fixtures_all")
  } else {
    seasons_str <- paste(sort(seasons), collapse = "_")
    return(paste0("fixtures_", seasons_str))
  }
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

#' Get Model Cache Info
#'
#' Returns information about cached models.
#'
#' @return Character vector of cached model names
#' @export
get_model_cache_info <- function() {
  ls(envir = .torp_model_cache)
}