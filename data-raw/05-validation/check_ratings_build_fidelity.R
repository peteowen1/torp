# Does the offline ratings builder reproduce what production published?
# =====================================================================
# build_ratings_history() exists so a change to the RATING DEFINITION can be
# scored against match MAE before anyone publishes it. That is only worth
# anything if, run with production's own settings, it reproduces production's
# own output. Otherwise every arm built on it measures a harness artifact.
#
# This is the check that every gate we have otherwise skips. The gates ask
# "is the new arm better"; none of them ask "is the thing I scored the same
# thing production runs". That gap has already cost half a headline gain once.
#
# Usage:  Rscript torp/data-raw/05-validation/check_ratings_build_fidelity.R [SEASONS]
# Exit 0 = reproduces published ratings, 1 = does not.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)   # the release, never a local mirror

args <- commandArgs(trailingOnly = TRUE)
SEASONS <- if (length(args) > 0) as.integer(strsplit(args[1], ",")[[1]]) else c(2025L, 2026L)

# Published values are stored to 2dp, so agreement is bounded by rounding, not
# by our arithmetic. A tolerance tighter than the file's own precision fails on
# correct output -- which is worse than no check, because it trains you to
# ignore it.
TOL_MEAN <- 0.02
TOL_COR  <- 0.999
CH <- c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout", "epr")

cat("=== building offline with PRODUCTION settings ===\n")
cat("epv_level_centre = FALSE (as published), epr_position_centre = TRUE\n\n")

built <- as.data.table(build_ratings_history(
  seasons = SEASONS,
  # The published artifact predates EPV level centring. Reproducing it means
  # asking for what production actually ran, not what the constants now say.
  epv_level_centre = FALSE,
  epr_position_centre = TRUE
))

pub <- as.data.table(load_torp_ratings())[season %in% SEASONS]
cat(sprintf("\nbuilt: %d rows | published: %d rows\n", nrow(built), nrow(pub)))

key <- c("player_id", "season", "round")
if (!all(key %in% names(built)) || !all(key %in% names(pub))) {
  cli::cli_abort("Missing join keys: {.val {setdiff(key, intersect(names(built), names(pub)))}}")
}
built[, (key) := lapply(.SD, as.character), .SDcols = key]
pub[,   (key) := lapply(.SD, as.character), .SDcols = key]

m <- merge(built[, c(key, intersect(CH, names(built))), with = FALSE],
           pub[,   c(key, intersect(CH, names(pub))),   with = FALSE],
           by = key, suffixes = c(".new", ".pub"))
cat(sprintf("matched on (player_id, season, round): %d rows\n", nrow(m)))

cover <- nrow(m) / nrow(pub)
cat(sprintf("coverage of published rows: %.1f%%\n\n", 100 * cover))

bad <- FALSE
if (nrow(m) == 0) {
  cli::cli_alert_danger("No rows matched -- the two frames are not comparable.")
  quit(status = 1)
}
if (cover < 0.95) {
  cli::cli_alert_danger("Offline build covers only {round(100*cover,1)}% of published rows.")
  bad <- TRUE
}

cat("=== per-channel agreement ===\n")
res <- rbindlist(lapply(intersect(CH, names(built)), function(cc) {
  a <- m[[paste0(cc, ".new")]]; b <- m[[paste0(cc, ".pub")]]
  ok <- is.finite(a) & is.finite(b)
  data.table(channel = cc, n = sum(ok),
             mean_abs_diff = round(mean(abs(a[ok] - b[ok])), 4),
             max_abs_diff  = round(max(abs(a[ok] - b[ok])), 4),
             cor = round(stats::cor(a[ok], b[ok]), 6))
}))
print(res, row.names = FALSE)

# Two DIFFERENT claims, reported separately, because they license different
# things and only one of them currently holds. Collapsing them into a single
# pass/fail would mean either blocking a usable harness or -- far worse --
# relaxing the tolerance until it went green, which is how a harness that
# doesn't measure production gets blessed.
#
#   STRICT: reproduces production player-for-player. Licenses reading an
#           absolute number off the harness and comparing it to a live metric.
#   PAIRED: same computation, same input distribution, no systematic offset.
#           Licenses A-vs-B arm comparison, where any shared bias cancels.
strict <- !any(res$mean_abs_diff > TOL_MEAN) && !any(res$cor < TOL_COR)

# Systematic offset is the thing that would NOT cancel in a paired comparison,
# so it is what PAIRED has to rule out.
off <- m[is.finite(get(paste0("epr.new"))) & is.finite(get(paste0("epr.pub"))),
         .(o = mean(get("epr.new") - get("epr.pub"))), by = "round"]$o
max_off <- if (length(off)) max(abs(off)) else NA_real_
epr_cor <- res[channel == "epr"]$cor
paired  <- isTRUE(cover > 0.99) && isTRUE(epr_cor >= 0.999) &&
           isTRUE(is.finite(max_off) && max_off < 0.02)

cat(sprintf("\nlargest per-round mean offset on epr: %.4f\n", max_off))

cat("\n=== VERDICT ===\n")
if (strict) {
  cli::cli_alert_success("STRICT: reproduces the published ratings player-for-player.")
} else {
  cli::cli_alert_warning(c(
    "STRICT: does NOT reproduce production player-for-player.",
    "i" = "Worst channel mean |diff| {max(res$mean_abs_diff)}, lowest cor {min(res$cor)}.",
    "!" = "Do not read an absolute MAE off this harness and compare it to a live metric."
  ))
}
if (paired) {
  cli::cli_alert_success(c(
    "PAIRED: safe for A-vs-B arm comparison.",
    "i" = "Coverage {round(100*cover,1)}%, epr cor {epr_cor}, no per-round offset above {signif(max_off,2)} -- a shared bias cancels in the delta."
  ))
} else {
  cli::cli_alert_danger("PAIRED: NOT safe even for arm comparison. Do not score arms with this builder.")
}
if (bad || !paired) quit(status = 1)
quit(status = 0)
