# What prior should EPV-layer position shrinkage use?
# ==================================================================
# The EPR-layer version of this shrinkage was shipped and reverted the same night
# (2026-07-29): the EPR correction is ~0.002, a backstop, so shrinking it did
# nothing. The EPV layer is where the correction is actually large. This script
# measures the two things needed to pick a prior there, because the units are
# DIFFERENT -- EPV cells are player-GAMES weighted by TOG, EPR cells are
# accumulated rating weight, so the prior 5 chosen for EPR means nothing here.
#
# Measured, per (season, round, listed bucket) cell, BEFORE centring:
#   1. cell weight (sum of TOG) -- how much evidence the "position mean" rests on
#   2. correction size (TOG-weighted cell mean) -- what gets subtracted
#   3. for each candidate prior: how much of the correction survives, and how
#      much positional LEVEL is handed back as a result
#
# (3) is the trade. Shrinkage protects thin cells from having their own noise
# written into every player, but every point of correction withheld is a point of
# positional level returned to the published rating -- the exact thing the whole
# programme removed. A prior is only defensible if it withholds correction where
# the evidence is thin WITHOUT restoring a material level overall.

suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
# NA, not a bogus path: a non-existent dir falls through to the sibling
# auto-detect and silently reads stale local parquets (2026-07-27 incident).
options(torp.local_data_dir = NA)

PRIORS <- c(0, 1, 2, 5, 10, 25)

cli::cli_h1("1. load and opponent-adjust (pre-centring)")
pgd <- adjust_epv_for_opponents(as.data.table(load_player_game_data(TRUE)))
chans <- EPV_LEVEL_CENTRE_CHANNELS
suffix <- if (all(paste0(chans, "_oadj") %in% names(pgd))) "_oadj" else "_adj"
cols <- paste0(chans, suffix)
cli::cli_alert_info("{nrow(pgd)} player-games, channel suffix {.val {suffix}}")
stopifnot(all(cols %in% names(pgd)))

pgd[, pos_bucket := torp:::.collapse_listed_position(position_group)]
pgd[, w := pmax(dplyr::coalesce(time_on_ground_percentage / 100, 0.1), 0.1)]
d <- pgd[!is.na(pos_bucket)]
cli::cli_alert_info("{nrow(d)} rows with a mapped bucket ({nrow(pgd) - nrow(d)} dropped)")

cli::cli_h1("2. cell weight and correction size, per channel")
cells <- rbindlist(lapply(cols, function(cc) {
  d[is.finite(get(cc)),
    .(channel = cc, n_rows = .N, wt = sum(w), cell_mean = weighted.mean(get(cc), w)),
    by = .(season, round, pos_bucket)]
}))

q <- function(x) round(quantile(x, c(0, 0.01, 0.05, 0.25, 0.5, 0.9, 1)), 3)
cli::cli_h2("cell WEIGHT (sum of TOG) -- the evidence behind each position mean")
print(cells[, as.list(q(wt)), by = channel], row.names = FALSE)
cli::cli_h2("|cell mean| -- the correction subtracted from every player in the cell")
print(cells[, as.list(q(abs(cell_mean))), by = channel], row.names = FALSE)

# The total is what a player actually feels: the four channels are summed.
tot <- d[Reduce(`&`, lapply(cols, function(cc) is.finite(d[[cc]])))]
tot[, epv_tot := Reduce(`+`, lapply(cols, function(cc) get(cc)))]
tcells <- tot[, .(n_rows = .N, wt = sum(w), cell_mean = weighted.mean(epv_tot, w)),
              by = .(season, round, pos_bucket)]
cli::cli_h2("TOTAL across the four channels (what a player actually feels)")
cli::cli_alert_info("weight:      {paste(names(q(tcells$wt)), q(tcells$wt), sep='=', collapse='  ')}")
cli::cli_alert_info("|correction|: {paste(names(q(abs(tcells$cell_mean))), q(abs(tcells$cell_mean)), sep='=', collapse='  ')}")
cli::cli_alert_info("{tcells[, sum(wt < 5)]} of {nrow(tcells)} cells carry weight < 5; {tcells[, sum(wt < 2)]} carry < 2")

cli::cli_h2("the thinnest cells -- who they are and what they get subtracted")
print(head(tcells[order(wt), .(season, round, pos_bucket, n_rows,
                              wt = round(wt, 2), correction = round(cell_mean, 3))], 12),
      row.names = FALSE)

cli::cli_h1("3. what each candidate prior does")
# Two numbers per prior, and they pull against each other:
#   kept_at_thin  -- fraction of the correction still applied in the thinnest
#                    cells (LOW is the point of shrinkage)
#   level_back    -- the positional level handed back, as the weighted spread of
#                    the residual bucket means (LOW is what the programme wants)
sweep <- rbindlist(lapply(PRIORS, function(p) {
  tcells[, lam := if (p == 0) 1 else wt / (wt + p)]
  # Residual mean left in each cell after a shrunk correction.
  tcells[, resid := (1 - lam) * cell_mean]
  # Aggregate to a season-level bucket spread, weighted by cell weight -- the
  # shape check_position_centring.R reports, so the numbers are comparable.
  bs <- tcells[, .(bm = weighted.mean(resid, wt)), by = .(season, pos_bucket)
               ][, .(spread = max(bm) - min(bm)), by = season]
  data.table(
    prior          = p,
    kept_median    = round(tcells[, median(lam)], 3),
    kept_p05       = round(tcells[, quantile(lam, 0.05)], 3),
    kept_thinnest  = round(tcells[which.min(wt), lam], 3),
    max_resid      = round(tcells[, max(abs(resid))], 3),
    mean_abs_resid = round(tcells[, weighted.mean(abs(resid), wt)], 4),
    level_back_max = round(max(bs$spread), 3),
    level_back_2026 = round(bs[season == max(season)]$spread, 3)
  )
}))
print(sweep, row.names = FALSE)

cli::cli_alert_info("prior 0 = production (full correction). level_back is the positional")
cli::cli_alert_info("spread RESTORED -- for reference, the v2 fix removed a spread of 2.94.")

saveRDS(list(cells = cells, tcells = tcells, sweep = sweep),
        "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse/92e2b422-0dee-4727-90de-364d23375767/scratchpad/epv_shrink_priors.rds")
cli::cli_alert_success("done")
