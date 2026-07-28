# Are the published predictions for the upcoming round built on real team lists?
# ============================================================================
# Rounds 19, 20 and 21 of 2026 were all locked with `players = NA` -- the AFL had
# not published team lists when the pipeline ran, so every player's EPR fell back
# to the position prior. Rounds 13-18 carried 23. Nothing reported it, and the
# cost (~3.2 MAE on rounds 19-20) surfaced only via a paired audit against
# Squiggle's record of our submitted tips, weeks later.
#
# .warn_missing_lineups() now fires at write time, but that only helps if someone
# reads pipeline logs. This answers the question directly and on demand:
#
#   1. Do the PUBLISHED predictions for the next round have real lineups?
#   2. Are team lists available NOW (i.e. would a re-run fix it)?
#   3. How long until first bounce -- after which the lock makes it permanent?
#
# Exit status is 0 when nothing needs doing and 1 when a re-run is required, so
# it can gate a workflow step as well as be read by a human.
#
# Usage:  Rscript torp/data-raw/05-validation/check_prediction_lineups.R [SEASON]

suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)   # the release, never a local mirror

SEASON <- { a <- commandArgs(trailingOnly = TRUE)[1]
            if (is.na(a)) get_afl_season() else as.integer(a) }

# Australian venue-time abbreviations -> offset from UTC in hours. The published
# `start_time` is LOCAL venue time carrying one of these ("2026-08-01 19:10:00
# ACST"); comparing a UTC instant against it unconverted is wrong by up to 11
# hours. `utc_start_time` only exists on rows written after the generated_utc
# change, so this has to keep working without it.
TZ_OFFSET <- c(AWST = 8, ACST = 9.5, ACDT = 10.5, AEST = 10, AEDT = 11, UTC = 0)

.parse_local_start <- function(x) {
  x <- as.character(x)
  abb <- sub("^.*\\s([A-Z]{3,4})$", "\\1", x)
  naive <- suppressWarnings(as.POSIXct(sub("\\s+[A-Z]{3,4}$", "", x),
                                       format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  off <- TZ_OFFSET[abb]
  unknown <- is.na(off) & !is.na(naive)
  if (any(unknown)) {
    cli::cli_abort("Unrecognised timezone abbreviation{?s}: {.val {unique(abb[unknown])}}")
  }
  naive - as.numeric(off) * 3600
}

preds <- as.data.table(load_predictions(SEASON))
if (!"players" %in% names(preds)) {
  cli::cli_abort("predictions_{SEASON} has no `players` column -- cannot audit lineups")
}
setnames(preds, "week", "round", skip_absent = TRUE)

start <- if ("utc_start_time" %in% names(preds)) {
  torp:::.parse_utc_start(preds$utc_start_time)
} else {
  .parse_local_start(preds$start_time)
}
# A check that cannot read start times must not report "all clear" -- that is
# exactly how the first version of this script declared round 21 safe while
# every start time silently parsed to NA.
if (all(is.na(start))) {
  cli::cli_abort("Could not parse any start time -- refusing to report a verdict.")
}
now <- Sys.time()
attr(now, "tzone") <- "UTC"
preds[, `:=`(.start = start, .started = !is.na(start) & start <= now)]

cat(sprintf("predictions_%d: %d rows, rounds %s\n\n", SEASON, nrow(preds),
            paste(range(preds$round), collapse = "-")))

by_round <- preds[, .(matches = .N,
                      no_lineup = sum(is.na(players)),
                      started = sum(.started),
                      first_bounce = if (all(is.na(.start))) as.POSIXct(NA) else
                                       min(.start, na.rm = TRUE)), by = round][order(round)]
cat("=== published predictions by round ===\n")
print(by_round, row.names = FALSE)

# The rounds still fixable: at least one match not yet started, and no lineup.
at_risk <- preds[is.na(players) & !.started]
locked_bad <- preds[is.na(players) & .started]

cat("\n=== verdict ===\n")
if (nrow(locked_bad) > 0) {
  wk <- sort(unique(locked_bad$round))
  cli::cli_alert_danger(
    "{nrow(locked_bad)} already-started match{?es} {?is/are} permanently locked without a team list (round{?s} {.val {as.character(wk)}}). Not fixable.")
}

if (nrow(at_risk) == 0) {
  # Exit 1 when there are already-locked bad rows, even though nothing is
  # fixable. The contract is "0 = all good", not "0 = no action available", and
  # a gate reading only the exit code must not see green on a round that
  # shipped on position priors. Reporting all-clear while a real failure sits
  # one line above is the same false-negative shape this script exists to end.
  if (nrow(locked_bad) > 0) {
    cli::cli_alert_warning(
      "No unstarted prediction is at risk, but {nrow(locked_bad)} already-started one{?s} shipped without a team list. Too late to fix; not a clean run.")
    quit(status = 1)
  }
  cli::cli_alert_success("Every unstarted published prediction has a real team list. Nothing to do.")
  quit(status = 0)
}

wk <- sort(unique(at_risk$round))
deadline <- min(at_risk$.start, na.rm = TRUE)
hrs <- as.numeric(difftime(deadline, now, units = "hours"))
cli::cli_alert_warning(
  "{nrow(at_risk)} unstarted prediction{?s} in round{?s} {.val {as.character(wk)}} {?has/have} NO team list.")
cat(sprintf("First bounce among them: %s UTC (%.1f hours away)\n",
            format(deadline, "%Y-%m-%d %H:%M"), hrs))

# Would a re-run actually help? Only if the AFL has published the lists.
# Keep the message: "the AFL has not published teams yet" and "load_teams() is
# broken / rate-limited / renamed a column" need completely different responses,
# and without conditionMessage they read identically.
teams <- tryCatch(as.data.table(load_teams(TRUE)),
                  error = function(e) { attr(e, ".msg") <- conditionMessage(e); e })
if (inherits(teams, "error")) {
  cli::cli_alert_warning("Could not load team lists to check availability: {conditionMessage(teams)}")
  quit(status = 1)
}
missing_cols <- setdiff(c("season", "round_number", "match_id", "team_id"), names(teams))
if (length(missing_cols) > 0) {
  cli::cli_alert_warning("load_teams() succeeded but is missing {.field {missing_cols}} -- cannot assess availability.")
  quit(status = 1)
}
per_team <- teams[season == SEASON & round_number %in% wk,
                  .(named = .N), by = .(round_number, match_id, team_id)]
# Count a team as ready only if the sheet looks COMPLETE. Presence alone is not
# enough: the AFL sometimes publishes ins/outs before the full side, which gives
# a team one or two named players. Treating that as "published" would tell the
# operator to re-run and produce exactly the degraded predictions this script
# exists to prevent -- with `players` non-NA, so the write-time guard's
# missing-lineup branch would not fire either.
cat("\n=== are team lists available yet? ===\n")
expected <- by_round[round %in% wk, sum(matches) * 2]   # 2 teams per match
if (nrow(per_team) == 0) {
  cli::cli_alert_info("No team lists published for round{?s} {.val {as.character(wk)}} yet. A re-run would NOT help; check back closer to the game.")
} else {
  # Summarise only once there is something to summarise -- min() on an empty
  # set warns and returns Inf.
  per_team[, complete := named >= MIN_PLAUSIBLE_LINEUP]
  avail <- per_team[, .(team_entries = .N, complete = sum(complete),
                        mean_named = round(mean(named), 1),
                        fewest = min(named)), by = round_number]
  print(avail, row.names = FALSE)
  n_complete <- sum(per_team$complete)
  if (n_complete >= expected) {
    cli::cli_alert_success(
      "Team lists ARE published in full ({n_complete}/{expected} team-matches). Re-run the predictions pipeline now -- the started-game lock will replace every unstarted match.")
    cat("  gh workflow run daily-ratings-predictions.yml --ref main --repo peteowen1/torp\n")
  } else {
    cli::cli_alert_info(
      "Only {n_complete}/{expected} team-matches have a COMPLETE sheet ({nrow(per_team)} have any players at all; fewest named {min(per_team$named)}). Re-running now would lock partial lineups -- wait for the rest.")
  }
}
quit(status = 1)
