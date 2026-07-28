# Did the published ratings actually come out position-centred?
# ============================================================
# Post-publish verification for EPR_POSITION_CENTRE. The pipeline has its own
# pre-publish guard, but that checks the frame in memory on the latest round
# only. This checks the artifact that consumers actually read, across all of it.
#
# Written because this morning's rating corruption published successfully and
# looked fine for a day. "The run went green" is not evidence that the file is
# right; reading the file is.
#
# Usage:  Rscript torp/data-raw/05-validation/check_position_centring.R
# Exit 0 = centred as claimed, 1 = not.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)   # the release, never a local mirror

# Tolerances are ROUNDING allowances, not slack. The published parquet stores
# values to 2dp, so exact zeros are unachievable no matter how correct the
# pipeline is:
#   - a TOG-weighted mean over ~150 rounded values lands around +/-0.002
#   - epr vs sum(4 channels) accumulates up to 4 * 0.005 = 0.02
# Both sit three orders of magnitude below a real failure (uncentred key
# defenders read 1.87), so this costs no sensitivity. A checker that fails on
# every correct run is worse than none -- it trains you to ignore it.
CENTRE_TOL <- 0.01
SUM_TOL    <- 0.03
CH  <- c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout")

r <- as.data.table(load_torp_ratings())
cat(sprintf("published ratings: %d rows, seasons %s\n\n", nrow(r),
            paste(range(r$season), collapse = "-")))
if (!"position_group" %in% names(r)) {
  cli::cli_abort("No {.field position_group} column -- cannot verify centring.")
}
r[, .w := pmax(dplyr::coalesce(pred_tog, 0.5), 0.01)]
# The 6-way bucket, not raw position_group -- MEDIUM_FORWARD and
# MIDFIELDER_FORWARD are centred together, so each is mean-zero only jointly and
# checking them apart would report a failure on a correct file.
r[, pos_bucket := torp:::.collapse_listed_position(position_group)]

# ---- 1. every (season, round, position) cell, every channel -----------------
have <- intersect(CH, names(r))
worst <- r[!is.na(pos_bucket), c(
    lapply(.SD, function(x) stats::weighted.mean(x, .w, na.rm = TRUE)), .(n = .N)),
    by = .(season, round, pos_bucket), .SDcols = have]
mx <- vapply(have, function(cc) max(abs(worst[[cc]]), na.rm = TRUE), numeric(1))
cat("=== 1. TOG-weighted mean per channel, worst cell across ALL rounds ===\n")
for (cc in have) cat(sprintf("  %-11s max |wmean| = %s\n", cc, format(mx[[cc]], scientific = TRUE)))

# ---- 2. the total, by position, latest round -------------------------------
lat <- r[season == max(season)][round == max(round)]
cat("\n=== 2. latest round, EPR by position ===\n")
print(lat[!is.na(pos_bucket), .(n = .N,
      wmean = round(stats::weighted.mean(epr, .w, na.rm = TRUE), 6),
      median = round(median(epr, na.rm = TRUE), 2),
      sd = round(sd(epr, na.rm = TRUE), 2)), by = pos_bucket][order(-median)],
      row.names = FALSE)

# ---- 2b. which centring regime is this file in? -----------------------------
# The bucket check above CANNOT tell 6-way from 7-way centring: if the two
# forward groups are each mean-zero (7-way) their pooled mean is zero too, so a
# regression from the merged taxonomy back to the split one passes silently.
# Report the two groups separately so a human can see which regime produced the
# file. Informational only -- both regimes are internally consistent, and
# failing here would reject correct historical artifacts.
if (all(c("MEDIUM_FORWARD", "MIDFIELDER_FORWARD") %in% lat$position_group)) {
  fw <- lat[position_group %in% c("MEDIUM_FORWARD", "MIDFIELDER_FORWARD"),
            .(n = .N, wmean = round(stats::weighted.mean(epr, .w, na.rm = TRUE), 4)),
            by = position_group]
  cat("\n=== 2b. forward groups within med_fwd ===\n")
  print(fw, row.names = FALSE)
  cat(if (max(abs(fw$wmean)) < CENTRE_TOL)
        "  both ~0 => centred 7-way (each group separately)\n"
      else
        "  jointly 0, separately not => centred 6-way on the merged bucket (expected since 2026-07-29)\n")
}

# ---- 3. epr must equal the sum of its channels ------------------------------
r[, .chk := rowSums(as.matrix(.SD), na.rm = TRUE), .SDcols = have]
d_epr <- max(abs(r$epr - r$.chk), na.rm = TRUE)
cat(sprintf("\n=== 3. epr vs sum(channels): max |diff| = %s ===\n", format(d_epr, scientific = TRUE)))

# ---- 4. spread must be PRESERVED, not flattened -----------------------------
# Centring subtracts a constant per group, so within-position spread is
# untouched. If it collapsed, something rescaled rather than recentred.
cat("\n=== 4. within-position spread (must be intact, ~2.5-3.3) ===\n")
sp <- lat[!is.na(pos_bucket), .(sd = round(sd(epr, na.rm = TRUE), 2)), by = pos_bucket]
print(sp[order(-sd)], row.names = FALSE)

cat("\n=== VERDICT ===\n")
bad <- max(mx, na.rm = TRUE) > CENTRE_TOL
if (bad) {
  cli::cli_alert_danger("NOT centred: worst channel mean {signif(max(mx), 3)} exceeds {CENTRE_TOL}")
}
if (d_epr > SUM_TOL) {
  cli::cli_alert_danger("epr does not equal the sum of its channels (max diff {signif(d_epr, 3)}, tolerance {SUM_TOL})")
  bad <- TRUE
}
if (min(sp$sd, na.rm = TRUE) < 1) {
  cli::cli_alert_danger("A position's spread collapsed below 1.0 -- recentring should not rescale")
  bad <- TRUE
}
if (bad) quit(status = 1)
cli::cli_alert_success("Published ratings are position-centred, channels sum to epr, spread intact.")
quit(status = 0)
