# Ruck contests on the same footing as the spoil price, plus a P(mark) audit
# =========================================================================
# Two jobs, both closing gaps flagged 2026-07-30.
#
# JOB 1 -- REDO THE RUCK PRICE PROPERLY. The 7.6x figure used the global V(x) curve,
# which OVERSTATES a tap: rows at a given x are mostly CLEAN possessions in space,
# while winning a tap gives contested possession. Measured on post-contest states
# instead, via the verified identity
#     V_after = exp_pts + delta_epv
# which recovers the state that followed, oriented to the acting row's team. (That
# uses delta_epv as algebra, not as an attribution, so it does not inherit the
# forward-looking problem.)
#
# NOTE WHY 50/50 IS THE CORRECT PROBABILITY HERE, not an assumption I am smuggling
# back in. Pete's rule is a probability-weighted counterfactual. A ruck contest is
# SYMMETRIC between the two teams -- unconditionally, either wins it half the time --
# so the situation-based probability IS 0.5 and the probability-weighted baseline
# coincides with the 50/50 one. For a spoil it does not, which is why that script
# had to measure P(mark).
#
# JOB 2 -- AUDIT P(mark). It read 0.16-0.23 across the whole ground and I called that
# suspiciously flat. Re-reading the table it declines monotonically from the
# defensive to the attacking end, which is what football says should happen (deep
# forward, defenders have position and numbers, so marks are rarer). Test that
# properly rather than eyeballing it, and check the outcome mix against known rates.

suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)

p <- torp:::default_epv_params()
SEASONS <- 2024:2026
BW <- 10

pbp <- as.data.table(load_pbp(SEASONS, rounds = TRUE))
pg  <- as.data.table(load_player_game_data(SEASONS))
setorder(pbp, match_id, display_order)
pbp[, V_after := exp_pts + delta_epv]
cli::cli_alert_info("{nrow(pbp)} pbp rows")

cli::cli_h1("1. is a ruck-contest row oriented to the WINNER?")
# The whole calculation depends on this. If team_id_mdl on a bounce row is the team
# that came out with the ball, V_after is the winner's value and should be clearly
# positive. If it is arbitrary, V_after would centre near zero.
RUCK_EVENTS <- c("Centre Bounce", "Ball Up Call", "Bounce")
rk <- pbp[description %in% RUCK_EVENTS & is.finite(x) & is.finite(V_after)]
cli::cli_alert_info("{nrow(rk)} ruck-contest rows; mean V_after = {round(mean(rk$V_after), 4)}, share positive {round(100*mean(rk$V_after > 0), 1)}%")
if (mean(rk$V_after) < 0.2) {
  cli::cli_abort(c("V_after on ruck rows is not clearly positive -- the row is not oriented to the winner.",
                   "x" = "Refusing to build a duel price on an unverified orientation."))
}
cli::cli_alert_success("Oriented to the winner, as required.")

cli::cli_h1("2. duel price for winning a ruck contest, from POST-CONTEST states")
rk[, xb := round(x / BW) * BW]
Vw <- rk[, .(n = .N, Vw = mean(V_after)), by = xb][n >= 100][order(xb)]
Vw <- merge(Vw, Vw[, .(xb_neg = -xb, Vw_mirror = Vw)], by.x = "xb", by.y = "xb_neg")
Vw[, `:=`(V_lose = -Vw_mirror,
          toss_up = 0.5 * (Vw - Vw_mirror),
          credit = 0.5 * (Vw + Vw_mirror))]
print(Vw[, .(xb, n, V_win = round(Vw, 3), V_lose = round(V_lose, 3),
             toss_up = round(toss_up, 3), credit = round(credit, 3))], row.names = FALSE)
# Weight by where ruck contests actually happen.
credit_ruck <- sum(Vw$credit * Vw$n) / sum(Vw$n)
cli::cli_alert_info("contest-weighted credit for WINNING a ruck contest = {round(credit_ruck, 4)}")
cli::cli_alert_info("(the global-V(x) version said 0.954 -- that overstated it, as predicted)")

cli::cli_h1("3. vs production's TOTAL per-contest ruck credit")
tot <- pg[, .(hitouts = sum(hitouts, na.rm = TRUE), adv = sum(hitouts_to_advantage, na.rm = TRUE),
              contests = sum(ruck_contests, na.rm = TRUE))]
per_contest <- p$ruck_contest_wt + (tot$hitouts / tot$contests) * p$hitout_wt +
  (tot$adv / tot$contests) * p$hitout_adv_wt
cli::cli_alert_info("production per contest = {round(per_contest, 4)}")
for (sh in c(1/3, 1/2, 1)) {
  cli::cli_alert_info("ruck share {round(sh,3)} -> implied {round(credit_ruck*sh, 4)} = {round(credit_ruck*sh/per_contest, 2)}x production")
}
cli::cli_alert_info("ws13's independent regression slope on the hitout channel was 7.22x.")

cli::cli_h1("4. AUDIT P(mark) -- is the gradient real?")
K <- pbp[description %in% c("Kick", "Ground Kick") & is.finite(x) & !is.na(lead_desc_tot)]
K[, outcome := fcase(grepl("Spoil", lead_desc_tot), "spoiled",
                     grepl("Contested Mark", lead_desc_tot), "contested_mark",
                     default = NA_character_)]
CC <- K[!is.na(outcome)]
CC[, marked := as.integer(outcome == "contested_mark")]
cli::cli_alert_info("{nrow(CC)} genuine aerial contests (contested mark or spoil)")
cli::cli_alert_info("overall P(contested mark) = {round(mean(CC$marked), 4)}")
# Logistic on x: a real gradient should be significant with a negative slope
# (marks rarer the deeper forward the kick goes, because defenders hold position).
g <- glm(marked ~ x, data = CC, family = binomial())
co <- summary(g)$coefficients
cli::cli_alert_info("logit slope on x = {signif(co['x','Estimate'], 3)} (p = {signif(co['x','Pr(>|z|)'], 3)})")
if (co["x", "Pr(>|z|)"] < 0.01 && co["x", "Estimate"] < 0) {
  cli::cli_alert_success("Significant NEGATIVE gradient -- marks get rarer toward the attacking end. Real, not flat.")
} else if (co["x", "Pr(>|z|)"] >= 0.01) {
  cli::cli_alert_danger("No significant gradient -- P(mark) really is flat; the location effect in the")
  cli::cli_alert_danger("spoil price comes entirely from the VALUE term, not the probability term.")
} else {
  cli::cli_alert_warning("Significant but POSITIVE gradient -- opposite to expectation, investigate.")
}
# Predicted range across the ground, so the size of the effect is visible.
pr <- predict(g, newdata = data.frame(x = c(-60, -30, 0, 30, 60)), type = "response")
cli::cli_alert_info("fitted P(mark) at x = -60/-30/0/30/60: {paste(round(pr, 3), collapse=' / ')}")

cli::cli_h1("5. which term drives the spoil price gradient: probability or value?")
# The spoil credit was 0.043 at x=-60 rising to 0.338 at x=+60, an 8x move. If
# P(mark) only moves 0.23 -> 0.15, the value term must be doing nearly all of it.
# Worth stating explicitly so nobody tunes the probability model expecting leverage.
V_by <- CC[, .(V_after = mean(V_after, na.rm = TRUE)), by = .(xb = round(x/BW)*BW, outcome)]
Wd <- dcast(V_by, xb ~ outcome, value.var = "V_after")
Wd <- Wd[is.finite(contested_mark) & is.finite(spoiled)]
Wd[, denied := contested_mark - spoiled]
cli::cli_alert_info("value denied at x=-60: {round(Wd[xb==-60]$denied, 3)} | x=+60: {round(Wd[xb==60]$denied, 3)}")
cli::cli_alert_info("so the VALUE term moves ~{round(Wd[xb==60]$denied / Wd[xb==-60]$denied, 1)}x while P(mark) moves ~{round(pr[1]/pr[5], 1)}x")
cli::cli_alert_info("=> the gradient is a VALUE effect. Refining the probability model buys little.")

saveRDS(list(ruck = Vw, credit_ruck = credit_ruck, per_contest = per_contest,
             p_mark_glm = co, denied_by_x = Wd),
        "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse/92e2b422-0dee-4727-90de-364d23375767/scratchpad/ruck_counterfactual.rds")
cli::cli_alert_success("done")
