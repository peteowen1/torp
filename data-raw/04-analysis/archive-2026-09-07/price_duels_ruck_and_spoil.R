# Duel pricing, part 2: the CORRECTED ruck comparison, and spoils measured not asserted
# ====================================================================================
# Two fixes to price_contest_from_state_values.R, both from Pete's questions.
#
# FIX 1 -- the ruck comparison was against the wrong denominator. That script priced
# RUCK CONTESTS (Centre Bounce / Ball Up Call / Bounce rows, which are in pbp) and
# then compared the result to `hitout_adv_wt` alone. But production pays a ruck
# through THREE weights, each on a different count:
#     ruck_contest_wt  0.0232  per ruck contest
#     hitout_wt        0.0510  per hitout
#     hitout_adv_wt    0.1748  per hitout to advantage
# The comparable quantity is total ruck credit PER CONTEST, which needs the observed
# ratios of hitouts and hitouts-to-advantage to contests.
#
# FIX 2 -- the spoil claim was asserted, never measured. The argument was that
# compute_spoil_credit() prices a spoil on REALISED value (-delta_epv * 1/3, i.e.
# where the ball ended up) while the counterfactual price is the swing against the
# opponent having marked it, and that the two diverge for defensive acts. That is a
# mechanism, not a number, and it was used to walk back a measured result. Measured
# here instead.
#
# A marking contest is NOT the symmetric two-state duel a ruck contest is. Three
# outcomes:
#     attacker marks  -> attacker has clean possession at x
#     defender marks  -> defender has clean possession at x (rare)
#     ball spills     -> contested, usually still in the attacker's forward half
# So the toss-up baseline must be built from the OBSERVED outcome mix, not from a
# 50/50 mirror. That difference is the whole reason a spoil can be worth much more
# than its realised delta suggests: it converts a likely mark into a spill.

suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)

p <- torp:::default_epv_params()
SEASONS <- 2024:2026
BW <- 5

pbp <- as.data.table(load_pbp(SEASONS, rounds = TRUE))
pg  <- as.data.table(load_player_game_data(SEASONS))
cli::cli_alert_info("{nrow(pbp)} pbp rows, {nrow(pg)} player-games")

cli::cli_h1("0. refit V(x), the state-value surface")
S <- pbp[is.finite(x) & is.finite(exp_pts)]
S[, xb := round(x / BW) * BW]
V <- S[, .(n = .N, V = mean(exp_pts)), by = xb][n >= 200]
V <- merge(V, V[, .(xb_neg = -xb, V_mirror = V)], by.x = "xb", by.y = "xb_neg")
V[, `:=`(toss_up = 0.5 * (V - V_mirror), win_value = 0.5 * (V + V_mirror))]
Vf <- function(xx) {           # V at arbitrary x, by nearest fitted bin
  b <- round(xx / BW) * BW
  V[.(b), V, on = "xb"]
}
cli::cli_alert_info("V(x) fitted over {nrow(V)} bins; V(0) = {round(Vf(0),3)}, V(60) = {round(Vf(60),3)}, V(-60) = {round(Vf(-60),3)}")

cli::cli_h1("1. FIX 1 -- production's TOTAL ruck credit per contest")
tot <- pg[, .(hitouts = sum(hitouts, na.rm = TRUE),
              adv = sum(hitouts_to_advantage, na.rm = TRUE),
              contests = sum(ruck_contests, na.rm = TRUE))]
# ruck_contests is a per-PLAYER count, so both rucks are counted at one contest.
contests_events <- tot$contests / 2
cli::cli_alert_info("hitouts {tot$hitouts}, to_advantage {tot$adv}, ruck_contests (player-count) {tot$contests} -> ~{round(contests_events)} contest events")
per_contest <- p$ruck_contest_wt +
  (tot$hitouts / tot$contests) * p$hitout_wt +
  (tot$adv / tot$contests) * p$hitout_adv_wt
cli::cli_alert_info("production credit to a ruck PER CONTEST = {round(per_contest, 4)}")
cli::cli_alert_info("  = {p$ruck_contest_wt} + {round(tot$hitouts/tot$contests,3)}x{p$hitout_wt} + {round(tot$adv/tot$contests,3)}x{p$hitout_adv_wt}")

RUCK_EVENTS <- c("Centre Bounce", "Ball Up Call", "Bounce")
rk <- pbp[description %in% RUCK_EVENTS & is.finite(x)]
rk[, xb := round(x / BW) * BW]
rk[V, wv := i.win_value, on = "xb"]
duel_ruck <- rk[is.finite(wv), mean(wv)]
cli::cli_alert_info("duel price for WINNING a ruck contest = {round(duel_ruck, 4)}")
for (sh in c(1/3, 1/2)) {
  imp <- duel_ruck * sh
  cli::cli_alert_info("at ruck share {round(sh,3)}: implied {round(imp,4)} vs production {round(per_contest,4)} -> production is {round(imp/per_contest,2)}x too LOW")
}
cli::cli_alert_info("NOTE the earlier '2.3x' compared against hitout_adv_wt alone and was wrong.")

cli::cli_h1("2. FIX 2 -- price a SPOIL against the observed outcome mix")
ch <- as.data.table(load_chains(SEASONS, rounds = TRUE))
detect <- intersect(c("description", "x", "y", "team_id", "display_order", "match_id"), names(ch))
stopifnot(length(detect) == 6)
setorder(ch, match_id, display_order)
ch[, `:=`(nxt_desc = shift(description, 1L, type = "lead"),
          nxt_team = shift(team_id, 1L, type = "lead"),
          nxt_x    = shift(x, 1L, type = "lead"),
          nxt2_desc = shift(description, 2L, type = "lead"),
          nxt2_team = shift(team_id, 2L, type = "lead"),
          nxt2_x    = shift(x, 2L, type = "lead")), by = match_id]

# The kick that a spoil defused: scan back for it, the same way
# compute_spoil_credit() does. What matters here is only the LOCATION and which
# team was attacking, so a 1-row lag is enough for the base rate.
ch[, `:=`(prv_desc = shift(description, 1L, type = "lag"),
          prv_team = shift(team_id, 1L, type = "lag"),
          prv_x    = shift(x, 1L, type = "lag")), by = match_id]

sp <- ch[description == "Spoil" & is.finite(x) & !is.na(team_id) & !is.na(prv_team)]
cli::cli_alert_info("{nrow(sp)} spoil rows in chains")
# Orient x to the ATTACKING team (the one whose kick was spoiled). Chains x is in a
# fixed ground frame, so flip when the attacker is the team going the other way.
# Use the kick's own x as the contest location.
sp[, att_is_prv := TRUE]        # by construction prv_team kicked it
sp[, contest_x := prv_x]
sp <- sp[is.finite(contest_x)]

# Outcome mix AFTER the spoil: who has it two rows later, and where.
sp[, spoiler_team := team_id]
sp[, outcome_team := fifelse(!is.na(nxt2_team), nxt2_team, nxt_team)]
sp[, outcome_x := fifelse(!is.na(nxt2_x), nxt2_x, nxt_x)]
sp <- sp[!is.na(outcome_team) & is.finite(outcome_x)]
sp[, def_regained := outcome_team == spoiler_team]
cli::cli_alert_info("after a spoil, the DEFENDING side has it 2 rows later {round(100*mean(sp$def_regained),1)}% of the time")

# Value the two states from the ATTACKER's perspective, using V(x) in the
# attacker's oriented frame. The attacker attacks toward +x in their own frame; the
# chains frame is fixed, so orient by the attacking team's direction using the sign
# convention that makes the kick's x point at their goal.
# Conservative approach: use |contest_x| as distance-to-goal proxy in the oriented
# frame, taking the attacker as moving toward +x.
sp[, att_x := abs(contest_x)]
sp[, V_if_marked := Vf(att_x)]                    # attacker marks: clean possession there
sp[, V_if_spoiled := fifelse(def_regained, -Vf(-att_x), Vf(att_x) * 0.5)]
# ^ defender regains -> attacker's value is the mirror, negated.
#   ball spills but attack retains -> partial, contested retention. 0.5 is a
#   DELIBERATE PLACEHOLDER, flagged below, not a fitted quantity.

base_mark_rate <- 0.5   # what a contest would have produced absent the spoil
sp[, toss_up := base_mark_rate * V_if_marked + (1 - base_mark_rate) * V_if_spoiled]
sp[, spoil_value := toss_up - V_if_spoiled]        # points of attacker value denied
cli::cli_alert_info("mean V_if_marked {round(mean(sp$V_if_marked),3)}, mean V_if_spoiled {round(mean(sp$V_if_spoiled),3)}")
cli::cli_alert_info("counterfactual spoil value (attacker points denied) = {round(mean(sp$spoil_value),4)}")
for (sh in c(1/3, 1/2)) {
  cli::cli_alert_info("at spoiler share {round(sh,3)}: implied spoil_wt = {round(mean(sp$spoil_value)*sh, 4)} vs production {p$spoil_wt}")
}
cli::cli_alert_info("Realised-value bracket measured earlier: 0.0646 to 0.1185.")
cli::cli_alert_danger("THE 0.5 RETENTION FACTOR ABOVE IS A PLACEHOLDER, NOT A MEASUREMENT.")
cli::cli_alert_info("It sets how much attacking value survives a spill the attack retains. The")
cli::cli_alert_info("spoil number is only as good as that, so treat it as an ORDER OF MAGNITUDE")
cli::cli_alert_info("check on whether the counterfactual differs from realised value -- not a weight.")

saveRDS(list(V = V, per_contest = per_contest, duel_ruck = duel_ruck,
             spoil_value = mean(sp$spoil_value), def_regain_rate = mean(sp$def_regained)),
        "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse/92e2b422-0dee-4727-90de-364d23375767/scratchpad/duels2.rds")
cli::cli_alert_success("done")
