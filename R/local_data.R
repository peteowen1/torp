# Local Data Storage for torpdata
#
# Provides functions to read/write parquet files locally in the
# torpdata/data/ directory, enabling fast offline access without
# network downloads. Files in torpdata/data/ are gitignored.

#' Get Local Data Directory
#'
#' Returns the path to the local torpdata/data/ directory for storing
#' parquet files locally. Checks the `torp.local_data_dir` option first,
#' then auto-detects based on common workspace layouts.
#'
#' @return Character path to local data directory, or NULL if not found
#' @export
#'
#' @examples
#' \donttest{
#' get_local_data_dir()
#'
#' # Set explicitly
#' set_local_data_dir("path/to/torpdata/data")
#' get_local_data_dir()
#' }
get_local_data_dir <- function() {
  # Explicit option takes precedence
  dir <- getOption("torp.local_data_dir")
  if (!is.null(dir) && dir.exists(dir)) return(normalizePath(dir, winslash = "/"))

  # Auto-detect: check common workspace locations

  candidates <- c(
    file.path(getwd(), "..", "torpdata", "data"),
    file.path(getwd(), "torpdata", "data")
  )

  for (candidate in candidates) {
    if (dir.exists(candidate)) return(normalizePath(candidate, winslash = "/"))
  }

  NULL
}

#' Set Local Data Directory
#'
#' Sets the path to the local torpdata/data/ directory for the current session.
#' Creates the directory if it doesn't exist.
#'
#' @param path Character path to the local data directory
#' @return Invisible normalized path
#' @export
#'
#' @examples
#' \donttest{
#' set_local_data_dir("path/to/torpdata/data")
#' }
set_local_data_dir <- function(path) {
  if (!dir.exists(path)) {
    created <- dir.create(path, recursive = TRUE, showWarnings = FALSE)
    if (!created || !dir.exists(path)) {
      cli::cli_abort("Failed to create local data directory: {path}")
    }
  }
  path <- normalizePath(path, winslash = "/")
  options(torp.local_data_dir = path)
  cli::cli_inform("Local data directory set to: {path}")
  invisible(path)
}

#' Get Local File Path for a URL
#'
#' Maps a GitHub release URL to a local file path in torpdata/data/.
#'
#' @param url Character URL of the remote parquet file
#' @return Character path to the local file, or NULL if local dir not configured
#' @keywords internal
get_local_path <- function(url) {
  local_dir <- get_local_data_dir()
  if (is.null(local_dir)) return(NULL)
  file.path(local_dir, basename(url))
}

#' Check if a File Exists Locally
#'
#' @param url Character URL to check for a local copy
#' @return Logical
#' @keywords internal
is_locally_stored <- function(url) {
  path <- get_local_path(url)
  !is.null(path) && file.exists(path)
}

#' Read a Parquet File from Local Storage
#'
#' @param url Character URL used as the key to find the local file
#' @return Data frame if local file exists, NULL otherwise
#' @keywords internal
read_local_parquet <- function(url) {
  path <- get_local_path(url)
  if (is.null(path) || !file.exists(path)) return(NULL)

  tryCatch({
    arrow::read_parquet(path)
  }, error = function(e) {
    cli::cli_warn("Failed to read local file {path}: {conditionMessage(e)}")
    NULL
  })
}

#' Write a Parquet File to Local Storage
#'
#' @param url Character URL used as the key (filename extracted from it)
#' @param data Data frame to write
#' @return Invisible NULL
#' @keywords internal
write_local_parquet <- function(url, data) {
  path <- get_local_path(url)
  if (is.null(path)) return(invisible(NULL))

  tryCatch({
    arrow::write_parquet(data, path)
  }, error = function(e) {
    cli::cli_warn("Failed to write local file {path}: {conditionMessage(e)}")
  })

  invisible(NULL)
}

#' Save Data Frame Locally in torpdata/data/
#'
#' Saves a data frame as a parquet file in the local torpdata/data/ directory.
#' This is called automatically by [save_to_release()] but can also be
#' used directly.
#'
#' @param df A data frame to save
#' @param file_name A string for the file name (without extension)
#' @return Invisible logical indicating success
#' @export
#'
#' @examples
#' \donttest{
#' my_df <- data.frame(x = 1:3)
#' save_locally(my_df, "test_data")
#' }
save_locally <- function(df, file_name) {
  local_dir <- get_local_data_dir()
  if (is.null(local_dir)) {
    cli::cli_warn("Local data directory not found. Use {.fn set_local_data_dir} to configure.")
    return(invisible(FALSE))
  }

  path <- file.path(local_dir, paste0(file_name, ".parquet"))

  tryCatch({
    arrow::write_parquet(df, path)
    cli::cli_inform("Saved locally: {basename(path)}")
    invisible(TRUE)
  }, error = function(e) {
    cli::cli_warn("Failed to save locally: {conditionMessage(e)}")
    invisible(FALSE)
  })
}
