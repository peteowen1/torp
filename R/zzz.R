# Package startup and configuration
# =================================

#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
  version <- tryCatch(
    as.character(utils::packageVersion("torp")),
    error = function(e) "unknown"
  )

  packageStartupMessage(
    "torp ", version, " - AFL Analytics and Player Rating System\n",
    "Data source: https://github.com/peteowen1/torpdata\n",
    "Report issues: https://github.com/peteowen1/torp/issues"
  )
}

.onLoad <- function(libname, pkgname) {
  # Initialize logging environment if not already done
  if (exists(".torp_logging_env", envir = asNamespace(pkgname))) {
    .torp_logging_env$log_level <- "INFO"
    .torp_logging_env$log_file <- NULL
    .torp_logging_env$console_output <- FALSE
  }

  invisible()
}
