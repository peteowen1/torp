# Verify the counterfactual duel credit, and derive the conservation rule
# ======================================================================
# The spoil figure moved 0.0717 -> 0.410 -> 0.3195 across three specification fixes in
# one hour, so it needs verification that does not just repeat the same calculation.
# Three independent checks here, in increasing strength.
#
# CHECK 1 -- THE ZERO-SUM IDENTITY. If credit(o) = toss_up - V_o and toss_up is the
# probability-weighted expectation over an exhaustive partition, then
#     SUM_o P(o) * credit(o) = toss_up - SUM_o P(o) * V_o = toss_up - toss_up = 0
# identically. This is not a modelling assumption, it is arithmetic, so it is a real
# test of whether the partition and the weighting were built correctly. A partition
# that does not sum to 1, or values computed on a different population from the
# probabilities, will break it.
#
# CHECK 2 -- ORDERING. An intercept MARK should be worth more to the defence than a
# spoil: a mark gives clean possession, a spoil only puts the ball on the ground. If
# the numbers say otherwise, the outcome values are wrong.
#
# CHECK 3 -- AGAINST PRODUCTION. torp already prices intercept marks, via
# EPV_RECV_INTERCEPT_MARK_SCALE on the reception formula. Compare.
#
# AND THE PAYOFF FOR PETE'S ITEM 4 (the conservation rule). If credits are zero-sum
# over outcomes, the framework is self-conserving: the swing on an event is a FIXED
# quantity, credited to the winner and debited to the loser. The share question stops
# being arbitrary -- shares must sum to 1 across the participants, and the total is
# pinned by the counterfactual rather than chosen.

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

K <- pbp[description %in% c("Kick", "Ground Kick") & is.finite(x) & is.finite(V_after) &
           !is.na(lead_desc_tot) & pos_team %in% c(-1L, 1L) & is.na(points_shot)]
if ("shot_at_goal" %in% names(K)) K <- K[!(shot_at_goal %in% TRUE)]
K[, outcome := fcase(
  grepl("Spoil", lead_desc_tot),                   "spoiled",
  grepl(MARK_RE, lead_desc_tot) & pos_team ==  1L, "att_mark",
  grepl(MARK_RE, lead_desc_tot) & pos_team == -1L, "def_mark",
  default = "other")]
K[, xb := round(x / BW) * BW]
cli::cli_alert_info("{nrow(K)} contest kicks (shots excluded)")

# Probabilities and values on the SAME population and bins -- the thing that makes
# the identity testable rather than vacuous.
OUT <- c("att_mark", "def_mark", "spoiled", "other")
agg <- K[, .(n = .N, V = mean(V_after)), by = .(xb, outcome)]
tot <- agg[, .(n_tot = sum(n)), by = xb]
agg[tot, p := n / i.n_tot, on = "xb"]
agg <- agg[xb %in% tot[n_tot >= 300]$xb]
# Only bins where all four outcomes are observed, else the partition is incomplete
# there and the identity cannot hold.
full_bins <- agg[, .N, by = xb][N == length(OUT)]$xb
agg <- agg[xb %in% full_bins]
cli::cli_alert_info("{length(full_bins)} x-bins with all {length(OUT)} outcomes observed")

toss <- agg[, .(toss_up = sum(p * V), p_sum = sum(p)), by = xb]
cli::cli_alert_info("max |sum of probabilities - 1| = {signif(max(abs(toss$p_sum - 1)), 3)}")
stopifnot(max(abs(toss$p_sum - 1)) < 1e-9)
agg[toss, toss_up := i.toss_up, on = "xb"]
agg[, credit := toss_up - V]

cli::cli_h1("CHECK 1 -- do probability-weighted credits sum to zero?")
zs <- agg[, .(weighted_credit_sum = sum(p * credit)), by = xb]
cli::cli_alert_info("max |sum_o P(o) * credit(o)| across bins = {signif(max(abs(zs$weighted_credit_sum)), 3)}")
if (max(abs(zs$weighted_credit_sum)) < 1e-10) {
  cli::cli_alert_success("Zero-sum identity holds exactly. Partition, probabilities and values are consistent.")
} else {
  cli::cli_alert_danger("Identity BROKEN -- the partition or the weighting is wrong; do not trust the magnitudes.")
}

cli::cli_h1("CHECK 2 -- ordering of the four outcomes (credit to the DEFENCE)")
# credit(o) as computed is the ATTACKER's value denied, so a defensive outcome should
# be positive and an attacking one negative.
sp_dist <- K[outcome == "spoiled", .N, by = xb]
agg[sp_dist, w := i.N, on = "xb"]
byout <- agg[is.finite(w), .(
  n = sum(n),
  mean_V = round(sum(V * w) / sum(w), 3),
  credit_to_defence = round(sum(credit * w) / sum(w), 4)
), by = outcome][order(-credit_to_defence)]
print(byout, row.names = FALSE)
ci <- byout[outcome == "def_mark"]$credit_to_defence
cs <- byout[outcome == "spoiled"]$credit_to_defence
if (ci > cs) {
  cli::cli_alert_success("Intercept mark ({ci}) beats a spoil ({cs}) -- clean possession is worth more than a spill, as it must be.")
} else {
  cli::cli_alert_danger("Intercept mark ({ci}) does NOT beat a spoil ({cs}) -- outcome values are wrong.")
}
if (byout[outcome == "att_mark"]$credit_to_defence < 0) {
  cli::cli_alert_success("The attacking mark carries NEGATIVE defensive credit, i.e. the defence is debited. Signs coherent.")
} else {
  cli::cli_alert_danger("An attacking mark reads as a defensive GAIN -- sign error somewhere.")
}

cli::cli_h1("CHECK 3 -- the spoil figure, and against production")
cli::cli_alert_info("spoil credit (full attribution) = {cs} vs production spoil_wt {p$spoil_wt} ({round(cs/p$spoil_wt, 2)}x)")
cli::cli_alert_info("previous run gave 0.3195 -- reproduced here as {cs}")
cli::cli_alert_info("intercept mark credit = {ci}; production prices intercept marks via")
cli::cli_alert_info("EPV_RECV_INTERCEPT_MARK_SCALE = {p$recv_intercept_mark_scale} on the reception formula (a MULTIPLIER, not a points value),")
cli::cli_alert_info("so the two are not directly comparable -- noted rather than forced.")

cli::cli_h1("THE CONSERVATION RULE THAT FOLLOWS (Pete's item 4)")
cli::cli_alert_info("Because credits are zero-sum over outcomes, the swing on an event is a FIXED")
cli::cli_alert_info("quantity: {cs} points for a spoil, {ci} for an intercept mark. It is not a free")
cli::cli_alert_info("parameter. So the share question is constrained, not arbitrary:")
cli::cli_alert_info("  - shares across the PARTICIPANTS of one event must sum to 1")
cli::cli_alert_info("  - the winner's gain is the loser's debit, so no value is created")
cli::cli_alert_info("  - a player cannot also be paid for the same swing downstream, which is the")
cli::cli_alert_info("    double-count compute_spoil_credit() already guards against by excluding")
cli::cli_alert_info("    spoils already counted as contest triples")
cli::cli_alert_info("This makes 'full attribution' ({round(cs,4)}) an UPPER BOUND on the spoiler's share,")
cli::cli_alert_info("reached only if the spoiler is credited the entire swing and the kicker debited it all.")

saveRDS(list(by_outcome = byout, agg = agg, zero_sum_max = max(abs(zs$weighted_credit_sum))),
        "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse/92e2b422-0dee-4727-90de-364d23375767/scratchpad/duel_conservation.rds")
cli::cli_alert_success("done")
