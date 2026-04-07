# Package startup and configuration
# =================================

#' @importFrom utils packageVersion
#' @importFrom withr local_seed
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
    .torp_logging_env$console_output <- FALSE
  }

  # Attach mgcv early so GAM/BAM predict() finds its internal Xbd function.
  # This avoids repeated attachNamespace() calls in get_shot_result_preds().
  if (requireNamespace("mgcv", quietly = TRUE) && !"mgcv" %in% .packages()) {
    tryCatch(
      attachNamespace("mgcv"),
      error = function(e) {
        warning("mgcv namespace attachment failed: ", conditionMessage(e),
                ". Shot model predictions may fail.", call. = FALSE)
      }
    )
  }

  invisible()
}
