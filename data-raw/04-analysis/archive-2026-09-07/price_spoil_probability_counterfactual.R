# Spoil value on a PROBABILITY-BASED COUNTERFACTUAL basis
# =======================================================
# Pete's decision, 2026-07-30: credit should be counterfactual, and the baseline
# should be probability-weighted rather than a flat 50/50. That simplifies the
# arithmetic and removes the placeholder that made the first attempt unusable:
#
#   credit_for_spoil(x) = P(mark | contest at x) * [ V_if_marked(x) - V_if_spoiled(x) ]
#
# i.e. the attacker's expected value that the spoil actually denied. No retention
# factor is needed, because V_if_spoiled is MEASURED rather than modelled.
#
# THE PROBABILITIES ARE SITUATION-BASED, NOT PLAYER-CONDITIONED, and that is a
# deliberate choice. Conditioning on the players would measure "did you beat your own
# expectation", under which a dominant key defender scores ~0 for dominating. Value
# over a replacement outcome is what a rating wants.
#
# TWO FIXES over price_duels_ruck_and_spoil.R, which produced 3.6-5.4x on
# approximations too crude to act on:
#
#  FIX 1 -- ORIENTATION. That script used abs(x) as a distance-to-goal proxy, which
#  treats a kick from defensive 50 as if it were forward 50 and inflated
#  V_if_marked to 2.089. Fixed by working entirely inside pbp, whose x IS oriented to
#  the possessing team's attacking direction (verified: cor(x, exp_pts) = 0.871).
#
#  FIX 2 -- THE RETENTION PLACEHOLDER. Gone. V_if_spoiled is read off the data via an
#  identity rather than assumed:
#      xpoints_diff  = points_diff + exp_pts
#      delta_epv     = lead(xpoints_diff) * team_change - xpoints_diff
#  so  lead(xpoints_diff) * team_change = delta_epv + xpoints_diff, and subtracting
#  the (state-independent) points_diff gives the NEXT state's value in exp_pts units,
#  oriented to THIS row's team:
#      V_after = exp_pts + delta_epv
#  This uses delta_epv as algebra, not as an attribution -- it is not crediting the
#  actor's next action, it is reconstructing the state that followed.

suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)

p <- torp:::default_epv_params()
SEASONS <- 2024:2026
BW <- 10   # wider bins than the V(x) fit: these are conditional means on subsets

pbp <- as.data.table(load_pbp(SEASONS, rounds = TRUE))
cli::cli_alert_info("{nrow(pbp)} pbp rows")
stopifnot(all(c("x", "exp_pts", "delta_epv", "description", "lead_desc_tot") %in% names(pbp)))

cli::cli_h1("0. can pbp identify a SPOILED kick at all?")
# Spoil rows are dropped from pbp (it holds possession events), but each row carries
# the FOLLOWING chain description. If spoils appear there, a spoiled kick is
# identifiable without any join.
lead_census <- pbp[grepl("Spoil", lead_desc_tot), .N, by = lead_desc_tot][order(-N)]
print(head(lead_census, 5), row.names = FALSE)
n_sp <- sum(grepl("Spoil", pbp$lead_desc_tot), na.rm = TRUE)
cli::cli_alert_info("{n_sp} pbp rows are followed by a Spoil")
if (n_sp < 5000) {
  cli::cli_abort(c("Too few spoil-followed rows in pbp ({n_sp}) -- the lead_desc_tot route does not work.",
                   "x" = "Fall back to the chains->pbp join on (match_id, display_order)."))
}

cli::cli_h1("1. verify the V_after identity before relying on it")
# If V_after = exp_pts + delta_epv is right, then on rows where possession is
# RETAINED it should closely track the next row's own exp_pts.
setorder(pbp, match_id, display_order)
pbp[, `:=`(nxt_exp = shift(exp_pts, 1L, type = "lead"),
           nxt_team = shift(team_id_mdl, 1L, type = "lead")), by = match_id]
pbp[, V_after := exp_pts + delta_epv]
chk <- pbp[is.finite(V_after) & is.finite(nxt_exp) & !is.na(nxt_team) & !is.na(team_id_mdl) &
             nxt_team == team_id_mdl]
cli::cli_alert_info("on {nrow(chk)} possession-retained rows: cor(V_after, next exp_pts) = {round(cor(chk$V_after, chk$nxt_exp), 4)}")
cli::cli_alert_info("mean |V_after - next exp_pts| = {round(mean(abs(chk$V_after - chk$nxt_exp)), 4)}")
if (cor(chk$V_after, chk$nxt_exp) < 0.9) {
  cli::cli_alert_danger("The identity does NOT reproduce the next state -- do not trust V_if_spoiled below.")
} else {
  cli::cli_alert_success("Identity holds; V_after is the post-event state oriented to this row's team.")
}

cli::cli_h1("2. P(mark | aerial contest at x) -- measured, not assumed")
# The contest population: kicks whose next chain event is either a mark by the
# intended target or a spoil. Both are visible in lead_desc_tot.
K <- pbp[description %in% c("Kick", "Ground Kick") & is.finite(x) & is.finite(exp_pts) &
           is.finite(delta_epv) & !is.na(lead_desc_tot)]
K[, outcome := fcase(
  grepl("Spoil", lead_desc_tot), "spoiled",
  grepl("Contested Mark", lead_desc_tot), "contested_mark",
  grepl("Mark On Lead|Uncontested Mark", lead_desc_tot), "clean_mark",
  default = NA_character_
)]
C <- K[!is.na(outcome)]
C[, xb := round(x / BW) * BW]
cli::cli_alert_info("{nrow(C)} contested aerial situations; outcome mix:")
print(C[, .(n = .N, pct = round(100 * .N / nrow(C), 1)), by = outcome][order(-n)], row.names = FALSE)

# A spoil's counterfactual is the CONTESTED mark -- a clean uncontested mark was
# never the alternative to being spoiled, so including it would overstate the
# denied value. Restrict to the genuine two-outcome contest.
CC <- C[outcome %in% c("spoiled", "contested_mark")]
pm <- CC[, .(n = .N, p_mark = mean(outcome == "contested_mark")), by = xb][n >= 100][order(xb)]
cli::cli_alert_info("P(contested mark | contest) overall = {round(CC[, mean(outcome == 'contested_mark')], 3)}")
print(pm, row.names = FALSE)

cli::cli_h1("3. V_if_marked and V_if_spoiled, both oriented to the ATTACKER")
# Both measured on the same population, at the same oriented x, so the difference is
# the outcome and not the location.
st <- CC[, .(n = .N, V_after = mean(V_after)), by = .(xb, outcome)]
W <- dcast(st, xb ~ outcome, value.var = c("n", "V_after"))
setnames(W, gsub("V_after_", "V_", names(W)))
W <- W[is.finite(V_contested_mark) & is.finite(V_spoiled) &
         n_contested_mark >= 50 & n_spoiled >= 50]
W[pm, p_mark := i.p_mark, on = "xb"]
W <- W[is.finite(p_mark)]
W[, denied := V_contested_mark - V_spoiled]
W[, credit := p_mark * denied]
print(W[, .(xb, n_mark = n_contested_mark, n_spoil = n_spoiled,
            V_marked = round(V_contested_mark, 3), V_spoiled = round(V_spoiled, 3),
            p_mark = round(p_mark, 3), denied = round(denied, 3),
            credit = round(credit, 3))], row.names = FALSE)

# Weight by where spoils actually happen, not by bin count.
sp_dist <- CC[outcome == "spoiled", .N, by = xb]
W[sp_dist, w := i.N, on = "xb"]
W <- W[is.finite(w)]
credit_mean <- sum(W$credit * W$w) / sum(W$w)
cli::cli_alert_info("spoil-weighted mean credit (attacker value DENIED) = {round(credit_mean, 4)} points")

cli::cli_h1("4. implied spoil_wt vs production and vs the realised basis")
for (sh in c(1/3, 1/2, 1)) {
  cli::cli_alert_info("spoiler share {round(sh,3)} -> implied spoil_wt {round(credit_mean * sh, 4)} (production {p$spoil_wt}, {round(credit_mean*sh/p$spoil_wt, 2)}x)")
}
cli::cli_alert_info("Realised-basis bracket measured earlier today: 0.0646 to 0.1185.")
cli::cli_alert_info("Earlier crude counterfactual (abs(x) + 0.5 placeholder) said 0.267-0.400. Superseded.")
cli::cli_alert_info("The original defender-diagnosis claim was 10-20x too low, i.e. {round(10*p$spoil_wt,3)}-{round(20*p$spoil_wt,3)}.")

saveRDS(list(by_x = W, credit_mean = credit_mean,
             p_mark_overall = CC[, mean(outcome == "contested_mark")]),
        "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse/92e2b422-0dee-4727-90de-364d23375767/scratchpad/spoil_counterfactual.rds")
cli::cli_alert_success("done")
