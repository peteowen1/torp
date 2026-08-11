# Helpers for tests that read the package's own R/ SOURCE files -- drift
# guards that assert a pattern has not come back, and contract checks that
# compare two places in one file.
#
# Existence of a directory named "R" is NOT enough, and assuming it was made
# the coverage job red on 2026-08-11 while R CMD check stayed green. An
# INSTALLED package also has an R/ -- holding the lazy-load database
# (torp.rdb/.rdx), no .R files at all -- and covr runs the tests right beside
# one. A scan that finds zero files is indistinguishable from a scan that
# finds zero violations, so the locator has to prove it found real sources.
#
# These guards are local-dev instruments. `R CMD check` runs against an
# installed package with no R/ tree beside it, so they skip there and a green
# CI run does not mean they enforced anything.

.r_source_dir <- function() {
  sentinels <- c("versebus.R", "match_model.R", "load_utils.R",
                 "match_data_prep.R")
  for (d in c("R", file.path("..", "..", "R"), file.path("..", "R"))) {
    if (dir.exists(d) && all(file.exists(file.path(d, sentinels)))) {
      return(normalizePath(d))
    }
  }
  NULL
}

# Non-comment lines of every R/ source file, named by basename. NULL rather
# than an empty list when there is nothing to scan -- an empty result must
# reach skip_if(), not sail through the guards as zero violations.
.r_source_code <- function(exclude = character(0)) {
  d <- .r_source_dir()
  if (is.null(d)) return(NULL)
  files <- list.files(d, pattern = "[.]R$", full.names = TRUE)
  files <- files[!basename(files) %in% exclude]
  if (length(files) == 0) return(NULL)
  stats::setNames(
    lapply(files, function(f) {
      lines <- readLines(f, warn = FALSE)
      lines[!grepl("^\\s*#", lines)]
    }),
    basename(files)
  )
}

# All lines (comments included) of one R/ source file, or NULL.
.r_source_lines <- function(basename_) {
  d <- .r_source_dir()
  if (is.null(d)) return(NULL)
  f <- file.path(d, basename_)
  if (!file.exists(f)) return(NULL)
  readLines(f, warn = FALSE)
}
