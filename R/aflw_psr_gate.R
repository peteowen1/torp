# AFLW PSR freshness gate
# =======================
# Stage 7 of data-raw/03-ratings/run_ratings_pipeline.R recomputes AFLW PSR for
# every season and re-uploads it on every men's ratings run (~125 s, measured
# on two runs, Nov 2026), although the result can only change when an AFLW
# match is played. This decides whether that work can be skipped.
#
# Anything it cannot establish resolves to RUN, never to skip.

#' Decide whether the AFLW PSR stage can be skipped
#'
#' Pure decision: no network, no clock. Skip only when ALL of these hold:
#' \itemize{
#'   \item every AFLW season with a concluded match has its
#'     \code{aflw_psr_<season>.parquet} on the release;
#'   \item the oldest of those files was uploaded at least \code{grace_days}
#'     after the last concluded match ended, so late stat corrections in the
#'     days after a round have been picked up by a later run;
#'   \item that oldest file is no more than \code{max_age_days} old, so a
#'     change to the coefficients or the code is republished within that
#'     window without anyone remembering to force it;
#'   \item \code{force} is \code{FALSE}.
#' }
#'
#' @param asset_times Named POSIXct vector: upload time of each
#'   \code{aflw_psr_<season>.parquet}, named by file name.
#' @param fixtures AFLW fixtures with \code{season}, \code{status} and
#'   \code{utc_start_time}.
#' @param now POSIXct. The moment the decision is made for.
#' @param grace_days Days after the last match during which the stage keeps
#'   running.
#' @param max_age_days Oldest a published file may be before it is rebuilt
#'   regardless.
#' @param force Logical. \code{TRUE} always runs.
#' @param match_hours Hours from a match's start to its end.
#' @return \code{list(skip = logical(1), reason = character(1))}.
#' @keywords internal
.aflw_psr_gate_decision <- function(asset_times, fixtures, now,
                                    grace_days = 3, max_age_days = 30,
                                    force = FALSE, match_hours = 4) {
  run <- function(why) list(skip = FALSE, reason = why)
  if (isTRUE(force)) return(run("forced (TORP_FORCE_AFLW_PSR)"))
  if (!inherits(now, "POSIXct") || length(now) != 1L || is.na(now)) return(run("no usable clock"))

  fx <- data.table::as.data.table(fixtures)
  if (!nrow(fx) || !all(c("season", "status", "utc_start_time") %in% names(fx))) {
    return(run("AFLW fixtures unreadable"))
  }
  done <- fx[!is.na(status) & status == "CONCLUDED"]
  if (!nrow(done)) return(run("no concluded AFLW match found"))
  start <- .parse_utc_start(done$utc_start_time)
  if (anyNA(start)) return(run("a concluded AFLW match has no start time"))
  last_end <- max(start) + match_hours * 3600
  if (last_end > now) return(run("an AFLW match is finishing now"))

  need <- paste0("aflw_psr_", sort(unique(as.integer(done$season))), ".parquet")
  if (!inherits(asset_times, "POSIXct") || is.null(names(asset_times))) {
    return(run("published AFLW PSR upload times unreadable"))
  }
  missing <- setdiff(need, names(asset_times))
  if (length(missing)) return(run(paste("not published yet:", paste(missing, collapse = ", "))))
  have <- asset_times[need]
  if (anyNA(have)) return(run("a published AFLW PSR file has no upload time"))
  oldest <- min(have)

  fmt <- function(x) format(x, "%Y-%m-%d %H:%M", tz = "UTC")
  if (oldest < last_end + grace_days * 86400) {
    return(run(sprintf(
      "last AFLW match ended %s UTC; oldest published file %s UTC is not %g days after it",
      fmt(last_end), fmt(oldest), grace_days)))
  }
  age_days <- as.numeric(difftime(now, oldest, units = "days"))
  if (age_days > max_age_days) {
    return(run(sprintf("oldest published file is %.0f days old (limit %g)", age_days, max_age_days)))
  }
  list(skip = TRUE, reason = sprintf(
    "no AFLW match since %s UTC; all %d season files published %s UTC or later (%.0f days old)",
    fmt(last_end), length(need), fmt(oldest), age_days))
}

#' Upload times of the published AFLW PSR files
#' @return Named POSIXct vector (UTC), one per \code{aflw_psr_<season>.parquet}
#'   in the \code{uploaded} state.
#' @keywords internal
.aflw_psr_asset_times <- function() {
  r <- .vb_split_repo(get_torp_data_repo())
  rel <- .gh_uncached("GET /repos/{owner}/{repo}/releases/tags/{tag}",
                      owner = r$owner, repo = r$name, tag = "aflw_psr-data")
  a <- Filter(function(x) grepl("^aflw_psr_[0-9]{4}\\.parquet$", x$name) &&
                identical(x$state, "uploaded"), rel$assets)
  out <- as.POSIXct(vapply(a, function(x) x$updated_at, character(1)),
                    format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  names(out) <- vapply(a, function(x) x$name, character(1))
  out
}

#' AFLW PSR gate for the ratings pipeline
#'
#' Fetches what \code{.aflw_psr_gate_decision()} needs and returns its answer. Any
#' error resolves to run. Set the environment variable
#' \code{TORP_FORCE_AFLW_PSR=true} to rebuild regardless (after retraining the
#' AFLW coefficients, for example).
#' @param now POSIXct, defaults to the current time.
#' @return \code{list(skip, reason)}.
#' @keywords internal
.aflw_psr_gate <- function(now = Sys.time()) {
  force <- tolower(Sys.getenv("TORP_FORCE_AFLW_PSR")) %in% c("1", "true", "yes")
  tryCatch(
    .aflw_psr_gate_decision(
      asset_times = if (force) NULL else .aflw_psr_asset_times(),
      fixtures = if (force) NULL else load_fixtures(all = TRUE, comp = "AFLW"),
      now = now, force = force
    ),
    error = function(e) list(skip = FALSE, reason = paste("gate error:", conditionMessage(e)))
  )
}
