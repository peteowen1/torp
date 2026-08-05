# Read-or-build cache that refuses to hand back a frame built by different code.
#
# WHY THIS EXISTS. On 2026-08-05 a gate compared a "v3 ship" arm read from
# `epv3_fin_pgd_ship.parquet` (written 08-04 23:22) against arms built from
# current code. Three commits in between had changed the contest population, so
# six columns differed across all 56,576 player-games -- and the gate would have
# attributed that to whatever it was nominally testing. The fitted constants in
# `epv3_finalise_ship.rds` were fitted against the same stale frame.
#
# Nothing failed. The parquet loaded, the columns were all present, the numbers
# were plausible. That is the failure mode this repo keeps hitting: every gate
# checks "is it better", none check "is it the same thing".
#
# The fix is to make a cached frame carry the fingerprint of the code that
# produced it, and to make reuse across a fingerprint change LOUD.
#
# Usage:
#   source("data-raw/04-analysis/cache_guard.R")
#   d <- cached_frame("epv3_fin_pgd_ship", function() {
#     create_player_game_data(pbp, stats_, teams, chains, epv_engine = "v3")
#   })
#
#   cached_frame(..., on_stale = "rebuild")   default: rebuild, and say so
#   cached_frame(..., on_stale = "warn")      hand back the stale frame, loudly
#   cached_frame(..., on_stale = "abort")     for ship gates -- refuse entirely

.cache_dir <- function() "C:/dev/torpverse/torp/data-raw/outputs"

#' Fingerprint of the code that can change a torp-derived frame
#'
#' Content hash of every R source file plus the state of the working tree, so an
#' uncommitted edit counts as a different fingerprint. Committing is not the
#' event that matters here -- running different code is.
code_fingerprint <- function(pkg_dir = "C:/dev/torpverse/torp") {
  files <- sort(list.files(file.path(pkg_dir, "R"), pattern = "\\.[Rr]$", full.names = TRUE))
  if (length(files) == 0) return(NA_character_)
  # Hash contents, not mtimes: a touched-but-unchanged file must not invalidate
  # a cache, and a restored older version must not validate one.
  body <- vapply(files, function(f) paste(readLines(f, warn = FALSE), collapse = "\n"),
                 character(1))
  substr(digest::digest(paste(basename(files), body, collapse = "\n\n"), algo = "sha1"), 1, 12)
}

#' Fingerprint this run pinned to, set on the first cached_frame() call
#' @keywords internal
.run_fingerprint <- local({ v <- NULL; function(set = NULL) {
  if (!is.null(set)) v <<- set
  v
} })

#' Read a parquet without leaving it memory-mapped
#'
#' `arrow::read_parquet()` memory-maps by default, and on Windows a mapped file
#' cannot be overwritten -- "the requested operation cannot be performed on a
#' file with a user-mapped section open". That bites exactly here: reuse a frame
#' early in a session, then rebuild the same tag after a fingerprint change, and
#' the write fails with an IO error that looks nothing like a cache problem.
#' @keywords internal
.read_unmapped <- function(f) {
  con <- arrow::ReadableFile$create(f)
  on.exit(con$close(), add = TRUE)
  arrow::read_parquet(con)
}

cached_frame <- function(tag, builder, on_stale = c("rebuild", "warn", "abort"),
                         dir = .cache_dir()) {
  on_stale <- match.arg(on_stale)
  f <- file.path(dir, paste0(tag, ".parquet"))
  stamp_f <- file.path(dir, paste0(tag, ".fingerprint"))
  now <- code_fingerprint()

  # A GAP THE ORIGINAL VERSION HAD. The stamp check only fires when a cached
  # file already exists, so it protects against reusing a stale frame -- and not
  # at all against a multi-arm run whose arms are BUILT by different code. A
  # three-arm gate takes ~100 minutes; editing anything under torp/R/ while it
  # runs silently gives arm 3 a different engine from arms 1 and 2, every file
  # is freshly built, and nothing above would say a word.
  #
  # So the first call pins the run's fingerprint and every later call must match
  # it. This is a hard abort regardless of `on_stale`: `on_stale` is about
  # trusting old data, and this is about a run contradicting itself.
  pinned <- .run_fingerprint()
  if (is.null(pinned)) {
    .run_fingerprint(now)
  } else if (!identical(pinned, now)) {
    cli::cli_abort(c(
      "Code changed mid-run: this run started on fingerprint {pinned}, '{tag}' is being built on {now}.",
      "x" = "Arms built by different code cannot be compared. Restart the run.",
      "i" = "Editing anything under torp/R/ while a multi-arm gate is running causes this."))
  }

  if (file.exists(f)) {
    was <- if (file.exists(stamp_f)) readLines(stamp_f, warn = FALSE)[1] else NA_character_
    if (!is.na(was) && identical(was, now)) {
      cli::cli_alert_success("Reusing {tag} (code fingerprint {now} matches)")
      return(.read_unmapped(f))
    }
    msg <- if (is.na(was)) {
      paste0("Cached frame '", tag, "' has NO fingerprint -- it predates this guard ",
             "and there is no way to tell what code built it.")
    } else {
      paste0("Cached frame '", tag, "' was built by code fingerprint ", was,
             "; current code is ", now, ".")
    }
    if (on_stale == "abort") {
      cli::cli_abort(c(msg, "x" = "Refusing to reuse it. Delete it or pass on_stale = 'rebuild'."))
    }
    if (on_stale == "warn") {
      cli::cli_warn(c(msg, "!" = "Reusing it anyway -- any comparison against a freshly built arm is CONFOUNDED."))
      return(.read_unmapped(f))
    }
    cli::cli_alert_warning(paste0(msg, " Rebuilding."))
  }

  d <- builder()
  arrow::write_parquet(d, f)
  writeLines(now, stamp_f)
  cli::cli_alert_success("Built {tag} and stamped fingerprint {now}")
  d
}

#' Stamp a frame that already exists, when you know it is current
#'
#' Only for frames built moments ago by the code now in the tree. Using this to
#' silence a real staleness warning defeats the entire point.
stamp_existing <- function(tag, dir = .cache_dir()) {
  writeLines(code_fingerprint(), file.path(dir, paste0(tag, ".fingerprint")))
  invisible(TRUE)
}
