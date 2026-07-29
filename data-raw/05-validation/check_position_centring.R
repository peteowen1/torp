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
#
# Section 5 history, worth reading before trusting a red result here:
# the PSR per-round centring shipped 2026-07-29 and the full-history rebuild
# that day confirmed it (worst cell 2.09e-04 over 984 cells). Section 5's FIRST
# version still reported a failure at 0.922 against that correct file, because
# it read `psr` off torp_ratings -- the rated-roster SUBSET -- with pred_tog
# weights, while the centring runs over all of psr-data with wt_80s weights.
# Wrong population, wrong weights. It now checks psr-data on its own key, and
# reports the roster-level PSR separately as 5b, informational only.

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

# EPR_POSITION_SHRINK (shipped 2026-07-29) deliberately leaves a residual: it
# subtracts n/(n+prior) of the position mean, not all of it. At prior 5 on the
# measured cell-weight distribution that is ~90% of the correction, so bucket
# means land near 0.1 rather than 1e-17 -- and the strict tolerance above would
# fail on every correct run. A checker that cries wolf gets ignored, which is
# the failure this file's own header warns about.
#
# So the tolerance becomes conditional. When shrinkage is ON we are no longer
# asserting "zero"; we are asserting "centring still did nearly all of its job".
# 0.5 sits well above the ~0.1 expected residual and far below the 1.7-2.9
# UNCENTRED level, so it still catches centring silently not happening at all
# -- which is the thing this section exists to detect.
EPR_SHRINK_ON <- isTRUE(tryCatch(torp:::EPR_POSITION_SHRINK, error = function(e) FALSE))
EPR_TOL <- if (EPR_SHRINK_ON) 0.50 else CENTRE_TOL
if (EPR_SHRINK_ON) {
  cat(sprintf("NOTE: EPR_POSITION_SHRINK is ON (prior %s) -- bucket means are expected to be\n",
              tryCatch(torp:::EPR_POSITION_SHRINK_PRIOR, error = function(e) "?")))
  cat("      NEAR zero, not AT zero. Section 1 tolerance relaxed accordingly.\n\n")
}
# PSR is stored to more decimal places than EPR and is centred over a much
# larger population, so it lands ~2e-04 rather than ~1e-16. Set well above the
# observed value but two orders below the ~0.76 spread an uncentred PSR shows.
PSR_CENTRE_TOL <- 0.01
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
# An empty frame is NOT a pass. max(abs(numeric(0)), na.rm = TRUE) is -Inf, and
# -Inf > CENTRE_TOL is FALSE, so an artifact with no mapped position bucket
# would sail through section 1 and print the success line. Section 5 already
# guards this; section 1 did not.
if (nrow(worst) == 0) {
  cli::cli_alert_danger("No (season, round, position) cell has a mapped bucket -- nothing was checked.")
  quit(status = 1)
}
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

# ---- 5. PSR, the OTHER half of torp -----------------------------------------
# Added 2026-07-29 with the PSR per-round centring. Until then this script
# checked EPR only, so two of the four layers that carry a positional level
# (EPV, EPR, PSV, PSR) shipped with no post-publish verification at all -- and
# this session showed twice that an unverified normalisation can silently skip
# (the PSV guard testing for `round` when the column is `round_number`, and the
# pipeline guard grouping on a taxonomy the centring no longer used).
#
# PSR is centred on pos_group, which has no MIDFIELDER_FORWARD and so already
# matches the 6-way bucket; checking on the bucket is therefore the same
# partition, not a looser one.
psr_bad <- FALSE
psr_rel <- tryCatch(as.data.table(load_psr(TRUE)), error = function(e) NULL)
if (!is.null(psr_rel) && nrow(psr_rel) > 0 && "psr" %in% names(psr_rel)) {
  # Check PSR on the POPULATION IT WAS CENTRED OVER, with the SAME WEIGHTS.
  #
  # The first version of this section read `psr` off torp_ratings and grouped by
  # pos_bucket with pred_tog weights. It reported "worst cell mean 0.922" on a
  # correctly-centred file, for two independent reasons, and I nearly acted on it:
  #
  #   1. WRONG POPULATION. calculate_psr() centres over every player-round in
  #      psr-data (176,055 rows). torp_ratings holds the rated-roster subset
  #      (130,928). A mean-zero set stays mean-zero only under a random subset,
  #      and "was this player rated this round" is emphatically not random --
  #      it is selection. Non-zero group means in the subset are arithmetic,
  #      not a centring failure.
  #   2. WRONG WEIGHTS. Centring is wt_80s-weighted; that check's printed table
  #      was pred_tog-weighted. Re-weighting a weighted mean does not preserve
  #      the zero.
  #
  # So it tested neither the population nor the statistic that the invariant is
  # about. Checking the release on its own key and weights gives 2.09e-04.
  psr_key <- if ("lineup_pos_group" %in% names(psr_rel)) "lineup_pos_group"
             else if ("lineup_position" %in% names(psr_rel)) "lineup_position"
             else if ("pos_group" %in% names(psr_rel)) "pos_group" else NULL
  if (is.null(psr_key)) {
    cli::cli_alert_danger("psr-data carries no position column -- PSR centring cannot be checked.")
    psr_bad <- TRUE
  } else {
    psr_rel[, .psrw := if ("wt_80s" %in% names(psr_rel))
                         pmax(dplyr::coalesce(wt_80s, 1), 0.01) else 1]
    pc <- psr_rel[!is.na(get(psr_key)) & is.finite(psr),
                  .(wmean = stats::weighted.mean(psr, .psrw, na.rm = TRUE), n = .N),
                  by = c("season", "round", psr_key)]
    # Empty is not a pass -- same trap section 1 fell into.
    psr_worst <- if (nrow(pc)) max(abs(pc$wmean), na.rm = TRUE) else NA_real_
    cat(sprintf("\n=== 5. PSR per (season, round, %s), wt_80s-weighted, on psr-data ===\n", psr_key))
    cat(sprintf("  %d rows, %d cells, worst |weighted mean| = %s\n",
                nrow(psr_rel), nrow(pc), format(psr_worst, scientific = TRUE)))
    if (!is.finite(psr_worst) || psr_worst > PSR_CENTRE_TOL) {
      cli::cli_alert_danger("PSR is NOT position-centred per round: worst cell mean {signif(psr_worst, 3)}")
      psr_bad <- TRUE
    }
  }
} else {
  cli::cli_alert_warning("Could not load psr-data -- PSR centring UNVERIFIED (not the same as verified).")
}

# ---- 5b. the PSR level torp_ratings actually inherits (informational) --------
# NOT a pass/fail.
#
# This MUST use wt_80s, joined from psr-data. torp_ratings does not carry
# wt_80s, and an earlier version of this section silently fell back to pred_tog
# -- a different weight vector entirely. That reported a 0.759 positional
# spread on a correctly-centred file and produced a written claim that TORP
# still carried ~0.38 of positional level. Measured properly the answer is:
#
#   full psr population, wt_80s (the invariant)  0.000
#   rated subset,        wt_80s                  0.096   <- the real number
#   full psr population, unweighted              0.306
#   rated subset,        unweighted              0.357
#
# So ~0.10 of the apparent 0.76 was the player set and ~0.66 was the weights.
# The dropped rows are 4.6% of total game time with median wt_80s 0.044 --
# fringe players who barely played -- which is exactly why re-weighting, not
# re-populating, is what moves this number. Re-centring PSR on the rated roster
# would buy ~0.05 in TORP and is not worth a rating-definition change.
if ("psr" %in% names(r) && !is.null(psr_rel) && "wt_80s" %in% names(psr_rel)) {
  wj <- unique(psr_rel[, .(player_id, season, round, wt_80s)])
  lat_w <- merge(lat[!is.na(pos_bucket) & is.finite(psr)], wj,
                 by = c("player_id", "season", "round"), all.x = TRUE,
                 suffixes = c("", ".psr"))
  wcol <- if ("wt_80s.psr" %in% names(lat_w)) "wt_80s.psr" else "wt_80s"
  lat_w[, .ww := pmax(dplyr::coalesce(get(wcol), 0), 1e-9)]
  matched <- sum(!is.na(lat_w[[wcol]]))
  cat(sprintf("\n=== 5b. PSR level within the rated roster, latest round (informational) ===\n"))
  cat(sprintf("  wt_80s-weighted, %d of %d rows matched to psr-data\n", matched, nrow(lat_w)))
  if (matched < 0.9 * nrow(lat_w)) {
    cli::cli_alert_warning("Under 90% of rated rows matched psr-data -- 5b's weighting is unreliable.")
  }
  print(lat_w[, .(n = .N, wmean = round(stats::weighted.mean(psr, .ww, na.rm = TRUE), 4),
                  sd = round(sd(psr, na.rm = TRUE), 2)), by = pos_bucket][order(-wmean)],
        row.names = FALSE)
} else if ("psr" %in% names(r)) {
  cli::cli_alert_warning("Skipping 5b: no wt_80s available to weight it correctly. An unweighted or pred_tog-weighted version of this table is misleading -- see the comment above.")
}

cat("\n=== VERDICT ===\n")
# Each failure reports its OWN cause. Folding PSR into `bad` before this block
# made a PSR failure print the EPR message -- "NOT centred: worst channel mean
# 3.37e-16 exceeds 0.01", a danger alert quoting a number that passes. A
# checker that names the wrong cause is barely better than one that says
# nothing, because the first thing you do is go look at the wrong layer.
epr_bad <- max(mx, na.rm = TRUE) > EPR_TOL
if (epr_bad) {
  cli::cli_alert_danger("EPR NOT centred: worst channel mean {signif(max(mx), 3)} exceeds {EPR_TOL}{if (EPR_SHRINK_ON) ' (shrinkage-adjusted tolerance)' else ''}")
}
bad <- epr_bad || psr_bad
if (d_epr > SUM_TOL) {
  cli::cli_alert_danger("epr does not equal the sum of its channels (max diff {signif(d_epr, 3)}, tolerance {SUM_TOL})")
  bad <- TRUE
}
if (min(sp$sd, na.rm = TRUE) < 1) {
  cli::cli_alert_danger("A position's spread collapsed below 1.0 -- recentring should not rescale")
  bad <- TRUE
}
if (bad) quit(status = 1)
cli::cli_alert_success("Published ratings are position-centred (EPR and PSR), channels sum to epr, spread intact.")
quit(status = 0)
