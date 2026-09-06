# Spoil value over the FULL outcome space of an aerial contest
# ===========================================================
# Fixes a specification error Pete caught in price_spoil_probability_counterfactual.R.
# That script classified a kick's outcome as "contested_mark" vs "spoiled" and set
# toss_up = P(mark)*V_marked + (1-P(mark))*V_spoiled. Two things wrong with it:
#
#  1. IT NEVER CHECKED WHICH TEAM MARKED IT. `grepl("Contested Mark", lead_desc_tot)`
#     lumps the attacking target marking (good for the attacker) together with a
#     DEFENDER INTERCEPT MARK (bad for the attacker). Those belong on opposite sides.
#     The codebase already separates them -- `is_intercept_mark` in player_credit.R
#     keys on `pos_team == -1L`, i.e. possession NOT retained.
#  2. THE PROBABILITIES DID NOT SUM OVER THE REAL OUTCOME SPACE. "Not a contested
#     mark" was treated as "spoiled", when it can also be an intercept mark, a spill
#     the attack retains, or the ball going out. So P(mark) = 0.209 was a conditional
#     two-way split inside a subset, not a distribution, and toss_up was biased by an
#     unknown amount in an unknown direction.
#
# Correct form -- a proper expectation over a partition:
#     toss_up(x) = SUM_o P(o | x) * V_o(x)          , SUM_o P(o|x) = 1
#     credit_for_spoil(x) = toss_up(x) - V_spoiled(x)
# i.e. the attacker's EXPECTED value that the spoil actually denied, relative to
# everything that could have happened instead.
#
# Outcome partition, using pos_team (possession retained = +1) as the team key:
#     att_mark   mark in lead_desc_tot AND pos_team == +1   (target marks)
#     def_mark   mark in lead_desc_tot AND pos_team == -1   (intercept mark)
#     spoiled    Spoil in lead_desc_tot
#     other      everything else -- spills, ground balls, out of bounds
# These are mutually exclusive and cover every kick, so they sum to 1 by construction
# and that is asserted below rather than assumed.

suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)

p <- torp:::default_epv_params()
SEASONS <- 2024:2026
BW <- 10

pbp <- as.data.table(load_pbp(SEASONS, rounds = TRUE))
setorder(pbp, match_id, display_order)
pbp[, V_after := exp_pts + delta_epv]

# SHOTS AT GOAL MUST BE EXCLUDED, and this is a third specification issue found on
# the same table. `other` is 57.9% of kicks and rises to 90% in the forward half,
# carrying V_other ~3.2 at x = +60 -- because a kick there that is neither marked nor
# spoiled is usually a SHOT AT GOAL. You cannot spoil a shot, so including them puts
# scoring shots into the counterfactual as though they were an alternative to this
# aerial contest, which inflates toss_up exactly where the gradient is steepest.
# The contest population is kicks TO A TARGET.
shot_col <- intersect(c("shot_at_goal", "is_shot"), names(pbp))
K <- pbp[description %in% c("Kick", "Ground Kick") & is.finite(x) & is.finite(V_after) &
           !is.na(lead_desc_tot) & pos_team %in% c(-1L, 1L)]
n_pre <- nrow(K)
if (length(shot_col)) {
  sc <- shot_col[1]
  K <- K[!(get(sc) %in% TRUE) & is.na(points_shot)]
  cli::cli_alert_info("excluded {n_pre - nrow(K)} shots at goal ({round(100*(n_pre-nrow(K))/n_pre,1)}%) using {.field {sc}} + points_shot")
} else {
  K <- K[is.na(points_shot)]
  cli::cli_alert_warning("no shot flag found; excluded scoring rows on points_shot only")
}
MARK_RE <- "ted Mark|Mark On"   # the same pattern is_intercept_mark uses
K[, outcome := fcase(
  grepl("Spoil", lead_desc_tot),                        "spoiled",
  grepl(MARK_RE, lead_desc_tot) & pos_team ==  1L,      "att_mark",
  grepl(MARK_RE, lead_desc_tot) & pos_team == -1L,      "def_mark",
  default = "other"
)]
K[, xb := round(x / BW) * BW]
cli::cli_alert_info("{nrow(K)} kicks classified")

cli::cli_h1("1. the outcome partition -- does it sum to 1?")
mix <- K[, .(n = .N, pct = round(100 * .N / nrow(K), 2)), by = outcome][order(-n)]
print(mix, row.names = FALSE)
stopifnot(sum(mix$n) == nrow(K))
cli::cli_alert_success("Partition is exhaustive and mutually exclusive ({sum(mix$n)} = {nrow(K)}).")
cli::cli_alert_info("Note def_mark (intercept) is {mix[outcome=='def_mark']$pct}% of kicks -- it was previously")
cli::cli_alert_info("lumped in with att_mark, which is the error being corrected.")

cli::cli_h1("2. how badly did the old lumping distort V_if_marked?")
# Direct check: value to the attacker when the target marks vs when a defender does.
cmp <- K[outcome %in% c("att_mark", "def_mark"),
         .(n = .N, V_to_attacker = round(mean(V_after), 3)), by = outcome]
print(cmp, row.names = FALSE)
cli::cli_alert_info("If these differ in SIGN, lumping them was not a rounding error.")

cli::cli_h1("3. probability-weighted counterfactual over the full partition")
P <- dcast(K[, .N, by = .(xb, outcome)], xb ~ outcome, value.var = "N", fill = 0L)
Vt <- dcast(K[, .(V = mean(V_after)), by = .(xb, outcome)], xb ~ outcome, value.var = "V")
setnames(P,  setdiff(names(P),  "xb"), paste0("n_",  setdiff(names(P),  "xb")))
setnames(Vt, setdiff(names(Vt), "xb"), paste0("V_",  setdiff(names(Vt), "xb")))
M <- merge(P, Vt, by = "xb")
ncols <- grep("^n_", names(M), value = TRUE)
M[, n_tot := rowSums(.SD), .SDcols = ncols]
M <- M[n_tot >= 300 & is.finite(V_spoiled) & is.finite(V_att_mark) &
         is.finite(V_def_mark) & is.finite(V_other)]
for (o in c("att_mark", "def_mark", "spoiled", "other")) {
  M[, (paste0("p_", o)) := get(paste0("n_", o)) / n_tot]
}
# Expectation over the partition, then the value the spoil denied.
M[, toss_up := p_att_mark * V_att_mark + p_def_mark * V_def_mark +
     p_spoiled * V_spoiled + p_other * V_other]
M[, credit := toss_up - V_spoiled]
print(M[, .(xb, n = n_tot,
            p_att = round(p_att_mark, 3), p_def = round(p_def_mark, 3),
            p_sp = round(p_spoiled, 3), p_oth = round(p_other, 3),
            V_att = round(V_att_mark, 2), V_sp = round(V_spoiled, 2),
            toss_up = round(toss_up, 3), credit = round(credit, 3))], row.names = FALSE)

# Weight by where spoils actually occur.
spd <- K[outcome == "spoiled", .N, by = xb]
M[spd, w := i.N, on = "xb"]
M <- M[is.finite(w)]
credit_mean <- sum(M$credit * M$w) / sum(M$w)
cli::cli_alert_info("spoil-weighted credit over the FULL partition = {round(credit_mean, 4)}")
cli::cli_alert_info("the two-outcome version said 0.0717 -- change of {round(100*(credit_mean/0.0717 - 1), 1)}%")

cli::cli_h1("4. implied spoil_wt")
for (sh in c(1/3, 1/2, 1)) {
  cli::cli_alert_info("share {round(sh,3)} -> {round(credit_mean*sh, 4)} vs production {p$spoil_wt} ({round(credit_mean*sh/p$spoil_wt, 2)}x)")
}
cli::cli_alert_info("Realised-basis bracket: 0.0646-0.1185. Original claim was 10-20x = 0.737-1.474.")

cli::cli_h1("5. does the location gradient survive the correction?")
lo <- M[xb == -60]; hi <- M[xb == 60]
if (nrow(lo) && nrow(hi)) {
  cli::cli_alert_info("credit at x=-60: {round(lo$credit,3)} | x=+60: {round(hi$credit,3)} -> {round(hi$credit/lo$credit,1)}x")
  cli::cli_alert_info("(two-outcome version gave 0.043 -> 0.338, ~8x)")
}

saveRDS(list(mix = mix, by_x = M, credit_mean = credit_mean),
        "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse/92e2b422-0dee-4727-90de-364d23375767/scratchpad/spoil_full_partition.rds")
cli::cli_alert_success("done")
