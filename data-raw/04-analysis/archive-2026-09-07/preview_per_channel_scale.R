# What would per-channel points constants do to the PUBLISHED ratings?
# ====================================================================
# ws13 measured the per-channel conversion slopes (recv 1.032, disp 0.569, spoil
# 4.024, hitout 7.224). Before any of that becomes a constant, this answers the
# question that actually decides it: who moves, and by how much?
#
# WHY THIS IS NOT MERELY AN INTERPRETABILITY CHANGE, contrary to how it was first
# framed. A UNIFORM rescale (today's single EPV_POINTS_SCALE) is a pure relabel:
# every player's channels scale together, so no ranking moves. A PER-CHANNEL
# rescale changes the relative weighting inside `epr = recv + disp + spoil +
# hitout`, so it changes who is rated highly. It is a rating-definition change and
# belongs behind the #134 gate.
#
# No rebuild needed. Per plan §6.6, the EPR aggregation is linear in per-game
# credit, so scaling a channel's credit by k scales its published channel rating
# by exactly k -- and that is EXACT here rather than approximate, because the
# design scales each EPR_PRIOR_RATE_* by the same factor, so the prior term
# scales with the value term instead of being left behind.

suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)

# ws13's multivariate slopes -- the extra factor on top of today's global 0.919.
K <- c(epr_recv = 1.032, epr_disp = 0.569, epr_spoil = 4.024, epr_hitout = 7.224)

r <- as.data.table(load_torp_ratings())
cli::cli_alert_info("{nrow(r)} rating rows, seasons {min(r$season)}-{max(r$season)}")
stopifnot(all(names(K) %in% names(r)))

# Sanity: the published epr must already be the sum of its channels, or the
# rescale below is not a valid reconstruction.
ok <- Reduce(`&`, lapply(c(names(K), "epr"), function(v) is.finite(r[[v]])))
gap <- max(abs(r[ok]$epr - Reduce(`+`, lapply(names(K), function(v) r[ok][[v]]))))
cli::cli_alert_info("epr - sum(channels): max |gap| = {signif(gap, 3)}")
if (gap > 1e-6) cli::cli_abort("Published epr is not the sum of its channels -- cannot preview a rescale.")

d <- r[ok]
d[, epr_new := Reduce(`+`, lapply(names(K), function(v) get(v) * K[[v]]))]
d[, torp_old := TORP_EPR_WEIGHT * epr + (1 - TORP_EPR_WEIGHT) * psr]
d[, torp_new := TORP_EPR_WEIGHT * epr_new + (1 - TORP_EPR_WEIGHT) * psr]
d <- d[is.finite(torp_old) & is.finite(torp_new)]

cli::cli_h1("1. how big is the move?")
cli::cli_alert_info("mean |d torp| = {round(mean(abs(d$torp_new - d$torp_old)), 4)}; max = {round(max(abs(d$torp_new - d$torp_old)), 3)}")
cli::cli_alert_info("sd(torp) {round(sd(d$torp_old), 3)} -> {round(sd(d$torp_new), 3)}")
cli::cli_alert_info("Spearman correlation of the two rankings: {round(cor(d$torp_old, d$torp_new, method='spearman'), 4)}")

cli::cli_h1("2. per-position mean shift -- the defender question")
pos <- d[!is.na(position_group), .(
  n = .N,
  torp_old = round(mean(torp_old), 3),
  torp_new = round(mean(torp_new), 3),
  shift = round(mean(torp_new) - mean(torp_old), 3)
), by = position_group][order(-shift)]
print(pos, row.names = FALSE)

cli::cli_h1("2b. LEVEL vs SPREAD, because the means above are centred by construction")
# The mean shifts in section 2 are near zero for a reason: EPR is position-centred,
# so each bucket's channel mean is already ~0 and scaling ~0 by 4 leaves ~0. The
# means are therefore NOT where the action is -- dispersion is. This is the same
# distinction the v2 explainer had to make (v2 did not raise defenders' average,
# it widened their range), and getting it backwards is how "spoil x4 raises
# defenders" becomes a claim the numbers do not support.
sp <- d[!is.na(position_group), .(
  n = .N,
  sd_old = round(sd(torp_old), 3),
  sd_new = round(sd(torp_new), 3),
  sd_ratio = round(sd(torp_new) / sd(torp_old), 3),
  p95_old = round(quantile(torp_old, 0.95), 2),
  p95_new = round(quantile(torp_new, 0.95), 2)
), by = position_group][order(-sd_ratio)]
print(sp, row.names = FALSE)

cli::cli_h1("2c. where does the drop in rucks/key defenders come from?")
# Candidate mechanism: EPR_PRIOR_RATE_SPOIL/HITOUT scale with their channel by
# design, so a x4 / x7 rescale also multiplies a NEGATIVE prior pull by 4 and 7.
# Players whose rating leans on spoil/hitout and who have little evidence get
# dragged down. Check it by splitting on evidence (wt_gms) rather than asserting.
if ("wt_gms" %in% names(d)) {
  ev <- d[!is.na(position_group) & is.finite(wt_gms)]
  ev[, ev_band := cut(wt_gms, breaks = quantile(wt_gms, c(0, .25, .5, .75, 1)),
                      include.lowest = TRUE, labels = c("Q1 thin", "Q2", "Q3", "Q4 thick"))]
  print(ev[position_group %in% c("RUCK", "KEY_DEFENDER", "MIDFIELDER"),
           .(n = .N, shift = round(mean(torp_new - torp_old), 3)),
           by = .(position_group, ev_band)][order(position_group, ev_band)],
        row.names = FALSE)
  cli::cli_alert_info("If the drop concentrates in the THIN bands, the amplified negative prior is the mechanism.")
} else {
  cli::cli_alert_warning("No wt_gms column -- cannot test the prior-amplification mechanism here.")
}

cli::cli_h1("3. the served round's top 20 -- does the leaderboard reorder?")
latest <- d[season == max(season)][round == max(round)]
if (nrow(latest) == 0) {
  cli::cli_alert_warning("No rows in the latest season/round -- skipping leaderboard check.")
} else {
  latest <- latest[order(-torp_old)]
  top <- head(latest, 20)
  top[, rank_old := seq_len(.N)]
  lat2 <- latest[order(-torp_new)][, rank_new := seq_len(.N)]
  top[lat2, rank_new := i.rank_new, on = "player_id"]
  nm <- intersect(c("player_name", "player_id"), names(top))[1]
  print(top[, .(player = get(nm), position_group,
                rank_old, rank_new, move = rank_old - rank_new,
                torp_old = round(torp_old, 2), torp_new = round(torp_new, 2))],
        row.names = FALSE)
  cli::cli_alert_info("Spearman over the served round: {round(cor(latest$torp_old, latest$torp_new, method='spearman'), 4)}")
  cli::cli_alert_info("players entering/leaving the top 20: {length(setdiff(head(lat2,20)$player_id, top$player_id))}")
}

cli::cli_h1("4. channel share of the metric, before and after")
shares <- rbindlist(lapply(names(K), function(v) data.table(
  channel = v,
  mean_abs_old = round(mean(abs(d[[v]])), 3),
  mean_abs_new = round(mean(abs(d[[v]] * K[[v]])), 3))))
shares[, `:=`(share_old = round(mean_abs_old / sum(mean_abs_old), 3),
              share_new = round(mean_abs_new / sum(mean_abs_new), 3))]
print(shares, row.names = FALSE)
cli::cli_alert_info("This row of the table IS the change: it is a reweighting of what TORP measures.")
cli::cli_alert_success("done")
