#!/usr/bin/env Rscript
# Off-season gate for daily-ratings-predictions.yml (review W6, finding 3.1)
# ==============================================================================
# The four pre-game crons fire every week of the year. Between seasons each run
# spent ~19 minutes retraining and re-uploading ratings and predictions that
# could not change (12 runs from 29 Oct to 15 Nov 2026). This decides, on
# SCHEDULED runs only, whether there is anything to do:
#
#   skip  <=>  no unplayed match starts in the next 14 days (this season or
#              next), AND no match finished after the last ratings upload.
#
# Anything this script cannot establish resolves to RUN, never to skip: an
# unreadable fixture list, a missing upload time, an error of any kind. Manual
# and repository_dispatch runs never reach this script (the workflow passes
# them straight through).
#
# Writes `skip=true|false` and `reason=...` to $GITHUB_OUTPUT (or prints them).
#   RATINGS_UPDATED_AT  updatedAt of torp_ratings.parquet on ratings-data (ISO 8601, UTC)
#   GATE_NOW            optional: decide as of this moment instead of now (testing)
suppressMessages(devtools::load_all(quiet = TRUE))

LOOKAHEAD_DAYS <- 14
MATCH_HOURS    <- 4    # a match is over by start + 4 h

emit <- function(skip, reason) {
  out <- Sys.getenv("GITHUB_OUTPUT")
  lines <- c(sprintf("skip=%s", tolower(as.character(skip))), sprintf("reason=%s", reason))
  if (nzchar(out)) cat(lines, file = out, sep = "\n", append = TRUE)
  cat(lines, sep = "\n"); cat("\n")
  cat(sprintf("::notice::Off-season gate: %s -- %s\n", if (skip) "SKIP" else "RUN", reason))
  quit(save = "no", status = 0)
}

decide <- function(now = Sys.time()) {
  up_raw <- Sys.getenv("RATINGS_UPDATED_AT")
  last_upload <- as.POSIXct(up_raw, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  if (is.na(last_upload)) return(list(FALSE, sprintf("no readable ratings upload time (%s), running", up_raw)))

  s <- get_afl_season()
  fx <- data.table::rbindlist(lapply(c(s, s + 1L), function(y)
    tryCatch(data.table::as.data.table(load_fixtures(y))[, .(season = y, status, utc_start_time)],
             error = function(e) NULL)), fill = TRUE)
  if (nrow(fx) == 0 || !nrow(fx[season == s])) {
    return(list(FALSE, sprintf("no fixtures readable for %d, running", s)))
  }
  fx[, start := .parse_utc_start(utc_start_time)]
  if (anyNA(fx$status) || anyNA(fx$start)) {
    return(list(FALSE, "a fixture has no status or start time, running"))
  }
  horizon <- now + LOOKAHEAD_DAYS * 86400
  # played = concluded AND started before `now`, so a replay at an earlier
  # moment (GATE_NOW) sees that day's fixture list, not today's results
  fx[, played := status == "CONCLUDED" & start <= now]
  soon <- fx[!played & start <= horizon]
  if (nrow(soon)) {
    return(list(FALSE, sprintf("%d unplayed match(es) start by %s UTC (first %s), running",
                               nrow(soon), format(horizon, "%Y-%m-%d"),
                               format(min(soon$start), "%Y-%m-%d %H:%M"))))
  }
  done <- fx[played == TRUE]
  last_end <- if (nrow(done)) max(done$start) + MATCH_HOURS * 3600 else as.POSIXct(NA)
  if (!is.na(last_end) && last_end > last_upload) {
    return(list(FALSE, sprintf("a match finished %s UTC, after the last ratings upload %s UTC, running",
                               format(last_end, "%Y-%m-%d %H:%M"), format(last_upload, "%Y-%m-%d %H:%M"))))
  }
  list(TRUE, sprintf("no match before %s UTC and none finished since the last ratings upload (%s UTC; last match ended %s UTC)",
                     format(horizon, "%Y-%m-%d"), format(last_upload, "%Y-%m-%d %H:%M"),
                     if (is.na(last_end)) "never" else format(last_end, "%Y-%m-%d %H:%M")))
}

# GATE_NOW (ISO 8601, UTC) replays the decision at another moment, for testing.
now <- if (nzchar(Sys.getenv("GATE_NOW"))) as.POSIXct(Sys.getenv("GATE_NOW"), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") else Sys.time()
if (is.na(now)) now <- Sys.time()
res <- tryCatch(decide(now), error = function(e) list(FALSE, paste("gate error, running:", conditionMessage(e))))
emit(res[[1]], gsub("[\r\n]", " ", res[[2]]))
