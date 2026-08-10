# What would EPV_RUCK_SWING_SCALE = 3.14 actually do to the published ratings?
# ===========================================================================
# Fix 2 from the swing-allocation audit: the ruck-contest swing is 0.3925 per
# contest, credited to nobody through the chain path (no player_id on Centre Bounce /
# Ball Up Call rows), while the three ruck box weights pay 0.1249 for both rucks --
# 31.8% of it. Scaling the three by 3.14 closes that ledger row.
#
# This previews the effect WITHOUT a rebuild, using the §6.6 linearity shortcut: the
# EPR aggregation is linear in per-game credit, so scaling a channel's credit by k
# scales its published channel rating by k.
#
# THREE REASONS THIS IS AN APPROXIMATION, NOT THE ANSWER. Stated up front because the
# temptation is to read the output as the result:
#  1. `adjust_epv_for_opponents()` forms `.abs_total` as a sum of abs() ACROSS
#     channels, so scaling hitout perturbs the opponent adjustment of every channel.
#     Not linear.
#  2. `.bayesian_shrink()` adds prior_games * prior_rate after the value. The real
#     change scales EPR_PRIOR_RATE_HITOUT too (it is wired to the same constant), and
#     that is reproduced here -- but only approximately, since the published rating
#     does not expose the prior term separately.
#  3. hitout is excluded from EPV_STANDARDISE_CHANNELS, so no standardisation
#     interaction -- this one is in our favour and makes the approximation better than
#     it would be for spoil.
#
# The real number needs a pgd rebuild from PBP; these weights act during pgd
# CONSTRUCTION, upstream of build_ratings_history(), the same reason ws10 could not
# score ROLE_USE_LINEUP_GROUP.

suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)

K <- 3.14
r <- as.data.table(load_torp_ratings())
CH <- c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout")
stopifnot(all(CH %in% names(r)))
ok <- Reduce(`&`, lapply(c(CH, "epr", "psr"), function(v) is.finite(r[[v]])))
d <- r[ok]
cli::cli_alert_info("{nrow(d)} rating rows, seasons {min(d$season)}-{max(d$season)}")

d[, epr_new := epr_recv + epr_disp + epr_spoil + epr_hitout * K]
d[, torp_old := TORP_EPR_WEIGHT * epr + (1 - TORP_EPR_WEIGHT) * psr]
d[, torp_new := TORP_EPR_WEIGHT * epr_new + (1 - TORP_EPR_WEIGHT) * psr]

cli::cli_h1("1. size of the move")
cli::cli_alert_info("mean |d torp| {round(mean(abs(d$torp_new - d$torp_old)), 4)}, max {round(max(abs(d$torp_new - d$torp_old)), 3)}")
cli::cli_alert_info("Spearman of the two rankings: {round(cor(d$torp_old, d$torp_new, method='spearman'), 4)}")
cli::cli_alert_info("hitout channel share of |epr|: {round(100*mean(abs(d$epr_hitout))/mean(abs(d$epr_recv)+abs(d$epr_disp)+abs(d$epr_spoil)+abs(d$epr_hitout)), 2)}% -> {round(100*mean(abs(d$epr_hitout*K))/mean(abs(d$epr_recv)+abs(d$epr_disp)+abs(d$epr_spoil)+abs(d$epr_hitout*K)), 2)}%")

cli::cli_h1("2. per position -- level AND spread, because centring only moves the level")
pos <- d[!is.na(position_group), .(
  n = .N,
  mean_old = round(mean(torp_old), 3), mean_new = round(mean(torp_new), 3),
  shift = round(mean(torp_new) - mean(torp_old), 3),
  sd_old = round(sd(torp_old), 3), sd_new = round(sd(torp_new), 3),
  sd_ratio = round(sd(torp_new) / sd(torp_old), 3)
), by = position_group][order(-sd_ratio)]
print(pos, row.names = FALSE)
cli::cli_alert_info("RUCK sd_ratio is the number that matters -- the whole point is to widen the")
cli::cli_alert_info("range rucks have to be good in. Everyone else should be ~1.000.")

cli::cli_h1("3. does it fix the ruck under-dispersion?")
# Rucks measured second-lowest spread of any position group before the change.
rk <- pos[position_group == "RUCK"]
others <- pos[position_group != "RUCK"]
if (nrow(rk)) {
  cli::cli_alert_info("RUCK spread {rk$sd_old} -> {rk$sd_new} (x{rk$sd_ratio})")
  cli::cli_alert_info("mean of other positions: {round(mean(others$sd_old), 3)} -> {round(mean(others$sd_new), 3)}")
  cli::cli_alert_info("ruck/other spread ratio: {round(rk$sd_old/mean(others$sd_old), 3)} -> {round(rk$sd_new/mean(others$sd_new), 3)}")
  if (rk$sd_new / mean(others$sd_new) > rk$sd_old / mean(others$sd_old)) {
    cli::cli_alert_success("Rucks gain relative spread -- the intended direction.")
  } else {
    cli::cli_alert_danger("Rucks do NOT gain relative spread -- the change is not doing what it is for.")
  }
}

cli::cli_h1("4. the served round's leaderboard")
latest <- d[season == max(season)][round == max(round)][order(-torp_old)]
if (nrow(latest) > 20) {
  top <- head(latest, 20)[, rank_old := seq_len(.N)]
  l2 <- latest[order(-torp_new)][, rank_new := seq_len(.N)]
  top[l2, rank_new := i.rank_new, on = "player_id"]
  nm <- intersect(c("player_name", "player_id"), names(top))[1]
  print(top[, .(player = get(nm), position_group, rank_old, rank_new,
                move = rank_old - rank_new, torp_old = round(torp_old, 2),
                torp_new = round(torp_new, 2))], row.names = FALSE)
  cli::cli_alert_info("entering/leaving the top 20: {length(setdiff(head(l2,20)$player_id, top$player_id))}")
  cli::cli_alert_info("rucks in the top 20 before: {top[position_group == 'RUCK', .N]}, after: {head(l2,20)[position_group == 'RUCK', .N]}")
}

saveRDS(pos, "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse/92e2b422-0dee-4727-90de-364d23375767/scratchpad/ruck_scale_preview.rds")
cli::cli_alert_success("done")
