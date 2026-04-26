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

  # Attach mgcv early so GAM/BAM predict() finds its internal Xbd C function.
  # mgcv requires its namespace to be ON THE SEARCH PATH (not just loaded) for
  # predict.bam/predict.gam to resolve internal helpers — `loadNamespace()`
  # alone is NOT sufficient. The attach is also done lazily inside
  # `get_shot_result_preds()` (R/add_variables.R) as a defensive fallback;
  # this .onLoad path is purely a perf optimisation that avoids paying the
  # attachNamespace cost on every shot prediction call.
  #
  # If you want to remove this side effect on the user's search path, the
  # cleanest alternative is to move mgcv from Imports to Depends in
  # DESCRIPTION (which makes the dependency truthful AND auto-attaches);
  # the current approach achieves the same runtime behaviour without the
  # CRAN-policy implications of Depends.
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
