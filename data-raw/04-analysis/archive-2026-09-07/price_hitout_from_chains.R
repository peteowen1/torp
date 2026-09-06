# What is a hitout WORTH, priced from chain EPV?
# ==============================================
# The loose end from 2026-07-30: epv_hitout converts at 7.22, the worst-looking
# slope of the four, and it was never priced -- I inferred from the spoil result
# (where the production weight turned out already correct) that its slope also is
# not a price, and moved on. This measures it instead of inferring.
#
# Feasibility, checked first: tackles CANNOT be priced this way -- chains carry 56
# "Tackle" rows in a full season against ~9,000 tackles in player_stats, so the
# events are simply not there. Hitouts CAN be, indirectly: chains/pbp carry
# "Gather From Hitout" (3,085 in 2026), which is the possession a hitout to
# advantage created. That is the priceable quantity.
#
# No chains/pbp join is needed -- pbp is cleaned chains and carries both the
# description and delta_epv.
#
# The three production weights and what they are supposed to price:
#   hitout_wt       0.0510  every hitout
#   hitout_adv_wt   0.1748  hitouts to advantage (i.e. ones that created possession)
#   ruck_contest_wt 0.0232  ruck contests attended

suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)

p <- torp:::default_epv_params()
SEASONS <- 2024:2026   # prices were stable to ~1.15x across seasons for spoils
pbp <- as.data.table(load_pbp(SEASONS, rounds = TRUE))
pg  <- as.data.table(load_player_game_data(SEASONS))
cli::cli_alert_info("{nrow(pbp)} pbp rows, {nrow(pg)} player-games, seasons {min(SEASONS)}-{max(SEASONS)}")
stopifnot(all(c("delta_epv", "description", "pos_team") %in% names(pbp)))

cli::cli_h1("1. event counts -- does chain coverage support a price?")
counts <- pbp[, .N, by = description][order(-N)]
gfh <- counts[description == "Gather From Hitout"]$N
stat_tot <- pg[, .(hitouts = sum(hitouts, na.rm = TRUE),
                   adv = sum(hitouts_to_advantage, na.rm = TRUE),
                   contests = sum(ruck_contests, na.rm = TRUE))]
cli::cli_alert_info("player_stats: hitouts {stat_tot$hitouts}, to_advantage {stat_tot$adv}, ruck_contests {stat_tot$contests}")
cli::cli_alert_info("pbp 'Gather From Hitout' rows: {gfh} -- that is {round(100*gfh/stat_tot$adv, 1)}% of hitouts_to_advantage")
if (gfh / stat_tot$adv < 0.25) {
  cli::cli_alert_danger("Coverage under 25% -- a price from this would rest on a small, possibly unrepresentative slice.")
}

cli::cli_h1("2. the EPV a hitout-to-advantage creates")
# Orient delta_epv to the team in possession, exactly as compute_spoil_credit()
# does with its own events, so a positive number means "good for the team that
# gained the ball".
gf <- pbp[description == "Gather From Hitout" & is.finite(delta_epv) & is.finite(pos_team)]
gf[, dv := delta_epv * pos_team]
cli::cli_alert_info("{nrow(gf)} priceable events; mean oriented delta_epv = {round(mean(gf$dv), 4)} (sd {round(sd(gf$dv), 3)})")
cli::cli_alert_info("median {round(median(gf$dv), 4)}; share positive {round(100*mean(gf$dv > 0), 1)}%")

# The ruck does not create all of that value -- the player who gathers does some of
# it. compute_contest_credit()/compute_spoil_credit() use contest_share = 1/3 for a
# contested event, so report the implied weight at 1/3 and at 1/2 rather than
# picking one silently.
for (sh in c(1/3, 1/2, 1)) {
  cli::cli_alert_info("at share {round(sh,3)}: implied hitout_adv_wt = {round(mean(gf$dv) * sh, 4)} (production {p$hitout_adv_wt})")
}
implied_third <- mean(gf$dv) * (1/3)
implied_half  <- mean(gf$dv) * 0.5

cli::cli_h1("3. verdict on hitout_adv_wt")
lo <- min(implied_third, implied_half); hi <- max(implied_third, implied_half)
if (p$hitout_adv_wt >= lo && p$hitout_adv_wt <= hi) {
  cli::cli_alert_success("Production {p$hitout_adv_wt} sits INSIDE the chain-anchored range [{round(lo,4)}, {round(hi,4)}].")
  cli::cli_alert_info("So, as with spoil, the 7.22 slope is NOT evidence of mis-pricing.")
} else {
  ratio <- if (p$hitout_adv_wt < lo) lo / p$hitout_adv_wt else hi / p$hitout_adv_wt
  cli::cli_alert_danger("Production {p$hitout_adv_wt} is OUTSIDE [{round(lo,4)}, {round(hi,4)}] -- off by ~{round(ratio,2)}x.")
  cli::cli_alert_info("Direction: production is {ifelse(p$hitout_adv_wt < lo, 'TOO LOW', 'TOO HIGH')}.")
}
cli::cli_alert_info("For scale, 7.22x the current weight would be {round(7.22 * p$hitout_adv_wt, 3)} points per hitout to advantage.")

cli::cli_h1("4. per-season stability")
st <- gf[, .(n = .N, mean_dv = round(mean(dv), 4),
             implied_third = round(mean(dv) / 3, 4)), by = season][order(season)]
print(st, row.names = FALSE)
if (nrow(st) > 1) {
  cli::cli_alert_info("spread {round(max(st$mean_dv)/min(st$mean_dv), 3)}x across seasons")
}

cli::cli_h1("5. the two weights that CANNOT be priced, and why that matters")
cli::cli_alert_info("hitout_wt ({p$hitout_wt}) covers ALL hitouts including those that create nothing;")
cli::cli_alert_info("ruck_contest_wt ({p$ruck_contest_wt}) covers contests attended. Neither maps to a chain event,")
cli::cli_alert_info("so neither is checkable by this method -- they are participation credit, not outcome credit.")
cli::cli_alert_info("Same blocker as tackle_wt ({p$tackle_wt}), which is 29.1% of epv_spoil and has only 56")
cli::cli_alert_info("chain rows a season. So the chain-pricing method reaches spoils and hitouts-to-advantage,")
cli::cli_alert_info("and CANNOT reach tackles, plain hitouts or ruck contests.")

saveRDS(list(mean_dv = mean(gf$dv), per_season = st, coverage = gfh / stat_tot$adv),
        "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse/92e2b422-0dee-4727-90de-364d23375767/scratchpad/hitout_price.rds")
cli::cli_alert_success("done")
