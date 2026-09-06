# The two measurements that block the duel plan
# =============================================
# From docs/plans/DUEL-CREDIT-PLAN.md §7:
#
#  BLOCKER 1 -- recompute the ruck duel price WITHOUT "Bounce". The 0.8131 figure used
#  event list c("Centre Bounce", "Ball Up Call", "Bounce"), but "Bounce" is a player
#  bouncing the ball while RUNNING (2,856/season, 100% player-attributed in chains) --
#  not a ruck contest. That is ~22% of the population. Blocks EPV_HITOUT_ADV_WT and
#  EPV_RUCK_CONTEST_WT.
#
#  BLOCKER 2 -- verify the KICKER IS DEBITED by the counterfactual amount. Conservation
#  requires the spoiler's gain to be the kicker's debit. Production debits the kicker
#  through epv_disp at REALISED value (`delta_epv * disp_scale`). If that realised debit
#  is much smaller than the counterfactual swing, then raising the spoiler's credit to
#  0.3195 MANUFACTURES VALUE rather than transferring it. Blocks EPV_SPOIL_WT.
#
# The second is the more important of the two: it is the difference between a transfer
# and an invention.

suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)

p <- torp:::default_epv_params()
SEASONS <- 2024:2026
BW <- 10
MARK_RE <- "ted Mark|Mark On"

pbp <- as.data.table(load_pbp(SEASONS, rounds = TRUE))
setorder(pbp, match_id, display_order)
pbp[, V_after := exp_pts + delta_epv]

cli::cli_h1("BLOCKER 1 -- ruck duel price, Centre Bounce + Ball Up Call only")
RUCK_TRUE <- c("Centre Bounce", "Ball Up Call")
RUCK_OLD  <- c("Centre Bounce", "Ball Up Call", "Bounce")

ruck_price <- function(events, label) {
  rk <- pbp[description %in% events & is.finite(x) & is.finite(V_after)]
  rk[, xb := round(x / BW) * BW]
  Vw <- rk[, .(n = .N, Vw = mean(V_after)), by = xb][n >= 100]
  Vw <- merge(Vw, Vw[, .(xb_neg = -xb, Vw_mirror = Vw)], by.x = "xb", by.y = "xb_neg")
  Vw[, credit := 0.5 * (Vw + Vw_mirror)]
  price <- sum(Vw$credit * Vw$n) / sum(Vw$n)
  cli::cli_alert_info("{label}: {nrow(rk)} rows, {nrow(Vw)} bins, duel price = {round(price, 4)}")
  price
}
price_old  <- ruck_price(RUCK_OLD,  "OLD (with running Bounce)")
price_true <- ruck_price(RUCK_TRUE, "CORRECTED (real ruck contests)")
cli::cli_alert_info("change: {round(100*(price_true/price_old - 1), 1)}%")

# Split so it is visible which event was doing what.
for (e in RUCK_OLD) {
  d <- pbp[description == e & is.finite(V_after)]
  cli::cli_alert_info("  {e}: n {nrow(d)}, mean V_after {round(mean(d$V_after), 4)}")
}

tot <- as.data.table(load_player_game_data(SEASONS))[, .(
  hitouts = sum(hitouts, na.rm = TRUE), adv = sum(hitouts_to_advantage, na.rm = TRUE),
  contests = sum(ruck_contests, na.rm = TRUE))]
per_contest <- p$ruck_contest_wt + (tot$hitouts / tot$contests) * p$hitout_wt +
  (tot$adv / tot$contests) * p$hitout_adv_wt
cli::cli_alert_info("production per contest {round(per_contest, 4)}; corrected duel price implies:")
for (sh in c(1/3, 1/2)) {
  cli::cli_alert_info("  share {round(sh,3)} -> {round(price_true*sh, 4)} = {round(price_true*sh/per_contest, 2)}x production")
}

cli::cli_h1("BLOCKER 2 -- is the kicker already debited the counterfactual amount?")
# Build the partition and toss_up exactly as the verified script does.
K <- pbp[description %in% c("Kick", "Ground Kick") & is.finite(x) & is.finite(V_after) &
           !is.na(lead_desc_tot) & pos_team %in% c(-1L, 1L) & is.na(points_shot)]
if ("shot_at_goal" %in% names(K)) K <- K[!(shot_at_goal %in% TRUE)]
K[, outcome := fcase(
  grepl("Spoil", lead_desc_tot),                   "spoiled",
  grepl(MARK_RE, lead_desc_tot) & pos_team ==  1L, "att_mark",
  grepl(MARK_RE, lead_desc_tot) & pos_team == -1L, "def_mark",
  default = "other")]
K[, xb := round(x / BW) * BW]
agg <- K[, .(n = .N, V = mean(V_after)), by = .(xb, outcome)]
tt <- agg[, .(n_tot = sum(n)), by = xb]
agg[tt, p := n / i.n_tot, on = "xb"]
full <- agg[, .N, by = xb][N == 4]$xb
keep <- intersect(full, tt[n_tot >= 300]$xb)
agg <- agg[xb %in% keep]
tu <- agg[, .(toss_up = sum(p * V)), by = xb]
K <- K[xb %in% keep]
K[tu, toss_up := i.toss_up, on = "xb"]

# PRODUCTION's actual credit to the kicker, from its own formula (player_credit.R
# Step 1): sum((delta_epv + offset) * disp_scale). Offsets are 0, and contested rows
# use contest_share rather than disp_scale -- use disp_scale, which is the common path.
K[, prod_kicker := (delta_epv + p$disp_pos_offset) * p$disp_scale]
# COUNTERFACTUAL credit to the kicker: he beat (or missed) expectation by
# V_after - toss_up, times whatever share of the swing he owns.
K[, cf_kicker_full := V_after - toss_up]

cmp <- K[, .(n = .N,
             prod_kicker = round(mean(prod_kicker), 4),
             cf_full = round(mean(cf_kicker_full), 4)), by = outcome][order(outcome)]
cmp[, ratio := round(prod_kicker / cf_full, 3)]
print(cmp, row.names = FALSE)

sp <- cmp[outcome == "spoiled"]
cli::cli_alert_info("On a SPOILED kick: production debits the kicker {sp$prod_kicker}, counterfactual swing is {sp$cf_full}")
cli::cli_alert_info("production covers {round(100*sp$prod_kicker/sp$cf_full, 1)}% of the counterfactual debit")

cli::cli_h1("verdict on conservation")
# The test: if production already debits the kicker by ~the full counterfactual swing,
# then paying the spoiler 0.3195 is a TRANSFER and the ledger balances. If it debits
# much less, the pair does not net to zero and value is created.
spoiler_credit <- 0.3195
kicker_debit <- abs(sp$prod_kicker)
cli::cli_alert_info("spoiler would be credited {spoiler_credit}; kicker is currently debited {round(kicker_debit, 4)}")
imbalance <- spoiler_credit - kicker_debit
cli::cli_alert_info("net created per spoil = {round(imbalance, 4)} points")
if (abs(imbalance) < 0.05) {
  cli::cli_alert_success("Balanced -- the existing kicker debit already matches. Spoil credit can rise as a transfer.")
} else if (imbalance > 0) {
  cli::cli_alert_danger("NOT balanced: paying the spoiler {spoiler_credit} while debiting the kicker only {round(kicker_debit,4)}")
  cli::cli_alert_danger("would CREATE {round(imbalance,4)} points per spoil ({round(imbalance*17964/3)} per season).")
  cli::cli_alert_info("Fix options: (a) share the swing so the spoiler gets only what the kicker loses,")
  cli::cli_alert_info("(b) also move the kicker's debit onto the counterfactual basis, or")
  cli::cli_alert_info("(c) accept non-conservation explicitly and document why.")
} else {
  cli::cli_alert_warning("Kicker is debited MORE than the spoiler would gain -- the reverse imbalance.")
}

saveRDS(list(price_old = price_old, price_true = price_true, per_contest = per_contest,
             kicker_cmp = cmp),
        "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse/92e2b422-0dee-4727-90de-364d23375767/scratchpad/duel_blockers.rds")
cli::cli_alert_success("done")
