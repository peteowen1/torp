# torp#209: design the floor on real contests, before writing the rule
# =============================================================================
# Pete chose "pay a floor". This script exists to settle WHAT floor, on real
# rows, rather than to pick a number and measure it afterwards -- the standing
# rule after two months of EPV work went in circles doing the reverse.
#
# THE MECHANISM, already settled (np_209_true_mechanism.R, and re-read in the
# code 2026-09-12). The contest models give, per contest:
#
#   p     = p_hat, the modelled chance the DEFENCE wins it
#   V_att = value if the attack wins,  V_def = value if the defence wins
#   Delta = V_att - V_def,             V_pre = (1 - p) * V_att + p * V_def
#
# and the ledger splits the kick into
#
#   decision      = V_pre    - exp_pts     -> the kicker
#   cont_surprise = V_branch - V_pre       -> whoever won the contest
#   ground        = V_after  - V_branch    -> whoever possessed next
#
# so the winner's payment is  (1 - p) * Delta  when the defence wins and
# p * Delta when the attack wins. At p = 1 with the defence winning, V_pre IS
# V_def and the payment is exactly zero: certainty does not divert the credit,
# it destroys the quantity being divided.
#
# WHY A FLOOR ON p AND NOT ON THE PAYMENT. p does not appear anywhere in the
# contested payment split (epv_net_points.R:1161-1178 use only dec_hm, c_hm,
# g_hm, beta and omega). It enters ONLY through V_pre, which carries opposite
# signs into `decision` and `cont_surprise`. So clamping p to [eps, 1 - eps] is
# a pure TRANSFER from the kicker to the contest winner and conservation holds
# by construction -- there is no compensating term to invent. A floor applied
# to the payment itself would have to take the points from somewhere, and
# "somewhere" is exactly the kind of unrecipiented term this ledger has been
# burned by before.
#
# WHAT THIS PRINTS, in the order a decision needs it:
#   1. the real distribution of p and Delta, so the floor is sized in points
#   2. five named contests at high p, before and after each candidate floor
#   3. what each floor costs the kicker and pays the winner, season-wide
#   4. the per-position move, which is the whole reason #209 is open
#
# HONEST ABOUT THE FIT: the contest models here are fitted in-sample on 2026.
# That is fine for this question -- we are reading the ARITHMETIC of a clamp on
# a fixed p, not validating p itself -- but it means the per-position numbers
# below are a design sighting shot, not a ship gate. The ship gate is a full
# leak-safe rebuild, the same one every vintage gets.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_209_floor_design.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
FLOORS <- c(0.00, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30)

pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))
say("pbp ", format(nrow(pbp), big.mark = ","), " rows | chains ",
    format(nrow(ch), big.mark = ","), " rows | EPV_ENGINE ", EPV_ENGINE,
    " | RATING_VINTAGE ", RATING_VINTAGE)

cst <- build_aerial_contests(ch, pbp)
stopifnot(nrow(cst) > 5000)
csc <- as.data.table(score_contests(cst, fit_contest_models(cst)))
csc <- csc[def_win == TRUE | out_desc %chin% EPV3_DUEL_OUT]
csc <- csc[is.finite(p_hat) & is.finite(Delta)]
say("contests scored: ", format(nrow(csc), big.mark = ","),
    "   defence won ", round(100 * mean(csc$def_win), 1), "%")

# --- 1. how big is the quantity a floor would create? -------------------------
# c_cont is the ledger's own signed quantity, V_branch - V_pre, in the ATTACKING
# team's frame: negative when the defence wins, positive when the attack does.
# Keeping the sign matters -- the proof that a floor is a transfer is that the
# change in c_cont is exactly minus the change in the kicker's decision credit,
# and differencing magnitudes instead would break that for the defence-won half
# for reasons of arithmetic rather than design.
#
# won_pts is the same number from the WINNER's point of view, always positive:
# what this player earned for winning the ball.
cnt <- function(p, dw, D) fifelse(dw, -(1 - p) * D, p * D)
csc[, c_cont := cnt(p_hat, def_win, Delta)]
csc[, paid_now := abs(c_cont)]
say("\n=== the raw material: p (chance the DEFENCE wins) and Delta ===")
say("Delta = V_att - V_def, in expected points; it is the whole stake of the")
say("contest. paid_now is the winner's current payment, unsigned. Higher is")
say("more credit changing hands; a payment near zero is the #209 complaint.")
q <- function(x) round(stats::quantile(x, c(.05, .25, .5, .75, .95), na.rm = TRUE), 3)
print(data.table(stat = c("p_hat", "|Delta|", "|paid_now|"),
                 p05 = c(q(csc$p_hat)[1], q(abs(csc$Delta))[1], q(abs(csc$paid_now))[1]),
                 p25 = c(q(csc$p_hat)[2], q(abs(csc$Delta))[2], q(abs(csc$paid_now))[2]),
                 med = c(q(csc$p_hat)[3], q(abs(csc$Delta))[3], q(abs(csc$paid_now))[3]),
                 p75 = c(q(csc$p_hat)[4], q(abs(csc$Delta))[4], q(abs(csc$paid_now))[4]),
                 p95 = c(q(csc$p_hat)[5], q(abs(csc$Delta))[5], q(abs(csc$paid_now))[5])))

say("\n=== where the collapse actually bites ===")
say("Rows are bands of p. n is contests; mean |paid| is the winner's payment in")
say("expected points. The defence-won band at high p is #209 in one line.")
csc[, p_band := cut(p_hat, c(-.01, .1, .3, .5, .7, .9, 1.01),
                    labels = c("0.0-0.1", "0.1-0.3", "0.3-0.5",
                               "0.5-0.7", "0.7-0.9", "0.9-1.0"))]
print(csc[, .(n = .N, mean_abs_paid = round(mean(abs(paid_now)), 4)),
          by = .(p_band, won_by = fifelse(def_win, "defence", "attack"))][order(p_band, won_by)])

# WHAT KIND OF FOOTBALL ACT is saturated? #209 is written as "a defender wins a
# contest he was always going to win". If the saturated rows are not contests at
# all, a floor is paying for the wrong thing and the issue needs reframing
# before any constant moves.
say("\n=== what the saturated rows actually ARE ===")
say("Play types on contests with p above 0.95, most common first. n is contests.")
print(head(csc[p_hat > 0.95, .N, by = out_desc][order(-N)], 8))
say("\nSame for the bulk of the population, p in 0.7-0.9, for comparison:")
print(head(csc[p_hat > 0.7 & p_hat <= 0.9, .N, by = out_desc][order(-N)], 8))

# THE ASYMMETRY, which is a different and much bigger thing than the tail.
say("\n=== the real asymmetry: the defence winning is the EXPECTED outcome ===")
say("Across every contest, not just the saturated tail. mean_paid is what the")
say("winner earns in expected points; share is how often that side wins. If the")
say("defence wins most contests, surprise-based credit pays it less per contest")
say("by construction -- and that is a property of the whole population, which no")
say("clamp on the tail can reach.")
print(csc[, .(n = .N, share = round(.N / nrow(csc), 3),
              mean_paid = round(mean(abs(c_cont)), 3),
              total_paid = round(sum(abs(c_cont)), 0)),
          by = .(won_by = fifelse(def_win, "defence", "attack"))])

# --- 2. five real contests, before and after ---------------------------------
# The point of naming them is that a floor is a claim about a specific football
# act: this defender made this contest look routine and was paid nothing for it.
nm <- unique(pbp[!is.na(player_id), .(pid = as.character(player_id), player = player_name)])
tmn <- unique(pbp[!is.na(team), .(match_id, team)])
ex <- csc[def_win == TRUE][order(-p_hat)][abs(Delta) > 0.5][1:5]
ex[, pid := as.character(out_pid)]
ex <- merge(ex, nm, by = "pid", all.x = TRUE)
say("\n=== five real contests the defence was always expected to win ===")
say("p is the modelled chance the defence wins. Delta is the stake in expected")
say("points. `now` is what the winner is paid; the eps columns are what a floor")
say("of that size would pay him instead. All in expected points, higher is more")
say("credit to the player who won the ball.")
show <- ex[, .(player = fifelse(is.na(player), "(unnamed in pbp)", player),
               contest = out_desc, p = round(p_hat, 3), Delta = round(Delta, 2),
               now = round(abs(cnt(p_hat, def_win, Delta)), 4))]
for (e in FLOORS[-1]) {
  show[[paste0("eps", sub("0\\.", ".", format(e)))]] <-
    round(abs(cnt(pmin(pmax(ex$p_hat, e), 1 - e), ex$def_win, ex$Delta)), 4)
}
print(show)

# --- 3 & 4. what each floor costs and who it moves ---------------------------
# A clamp on p moves value between exactly two recipients, so the season-wide
# accounting is a two-column answer: points off kickers, points onto winners.
say("\n=== what each floor moves, season-wide ===")
say("to_winners is the total extra paid to contest winners across ", SEASON, ",")
say("in expected points; from_kickers is the identical amount taken off the")
say("kickers' decision credit. They must match exactly -- a clamp on p is a")
say("transfer, not a creation, and if these differ the premise is wrong.")
mv <- rbindlist(lapply(FLOORS, function(e) {
  pc <- pmin(pmax(csc$p_hat, e), 1 - e)
  cont_new <- cnt(pc, csc$def_win, csc$Delta)
  vpre_now <- (1 - csc$p_hat) * csc$V_att_hat + csc$p_hat * csc$V_def_hat
  vpre_new <- (1 - pc) * csc$V_att_hat + pc * csc$V_def_hat
  data.table(floor = e,
             n_clamped = sum(csc$p_hat < e | csc$p_hat > 1 - e),
             to_winners = round(sum(cont_new - csc$c_cont), 2),
             from_kickers = round(sum(vpre_new - vpre_now), 2),
             extra_to_winners_abs = round(sum(abs(cont_new)) - sum(csc$paid_now), 1),
             mean_abs_paid = round(mean(abs(cont_new)), 4))
}))
print(mv)
say("\nto_winners and from_kickers are the SIGNED transfer and must be equal and")
say("opposite to the last decimal -- that is the proof a floor creates no value,")
say("it only moves it. extra_to_winners_abs is the same move stated as points of")
say("credit the winning players actually gain, which is the number that matters")
say("to a defender. Any gap in the first two is a defect in this script.")
stopifnot(all(abs(mv$to_winners + mv$from_kickers) < 0.05))

say("\n=== who wins the ball on the clamped rows? ===")
say("Contests where p is outside [", min(FLOORS[-1]), ", ", 1 - min(FLOORS[-1]),
    "], by the winner's listed position.")
say("n is contests won; extra is the total points a floor of 0.10 would add,")
say("which is the per-position size of the #209 fix before any rebuild.")
pg <- as.data.table(load_player_game_ratings(SEASON))
# Modal position per player, not unique() -- position_group here is the PER-MATCH
# lineup listing, so unique() gives 53 of 669 players more than one row and every
# merge on it silently double-counts them (found in the categories artifact the
# same day, 2026-09-12).
pos <- pg[!is.na(position_group), .N, by = .(pid = as.character(player_id), position_group)]
data.table::setorder(pos, pid, -N, position_group)
pos <- pos[, .SD[1], by = pid][, .(pid, position_group)]
stopifnot(!anyDuplicated(pos$pid))
cl <- csc[p_hat < 0.10 | p_hat > 0.90]
cl[, pid := as.character(out_pid)]
cl[, extra := abs(cnt(pmin(pmax(p_hat, 0.10), 0.90), def_win, Delta)) - paid_now]
cl <- merge(cl, pos, by = "pid", all.x = TRUE)
print(cl[!is.na(position_group),
         .(n = .N, extra_points = round(sum(extra), 1),
           per_contest = round(mean(extra), 4)),
         by = position_group][order(-extra_points)])

# SIZE IT AGAINST THE THING IT IS MEANT TO FIX. #209 is open because of the
# forward-defender gap, which is 3.409 points a game on v11. A fix worth a
# hundredth of that is a correctness note, not a lever, and saying so here is
# cheaper than finding it out after a full rebuild.
say("\n=== the same move, per player-game, against the 3.409 gap ===")
say("extra_per_game is the floor's payment divided by that position's")
say("player-games in ", SEASON, ", in the SAME units as the forward-defender gap")
say("(expected points a game). pct_of_gap sizes it against 3.409. Higher means")
say("the floor does more of what #209 was opened to do.")
gcount <- pg[!is.na(position_group), .(player_games = .N), by = position_group]
for (e in FLOORS[FLOORS > 0]) {
  ce <- csc[p_hat < e | p_hat > 1 - e]
  if (!nrow(ce)) next
  ce <- copy(ce)[, pid := as.character(out_pid)]
  ce[, extra := abs(cnt(pmin(pmax(p_hat, e), 1 - e), def_win, Delta)) - abs(c_cont)]
  ce <- merge(ce, pos, by = "pid", all.x = TRUE)
  agg <- ce[!is.na(position_group), .(extra_points = sum(extra)), by = position_group]
  agg <- merge(agg, gcount, by = "position_group")
  agg[, `:=`(floor = e,
             extra_per_game = round(extra_points / player_games, 4),
             pct_of_gap = round(100 * (extra_points / player_games) / 3.409, 2))]
  print(agg[order(-extra_per_game), .(floor, position_group, player_games,
                                      extra_points = round(extra_points, 1),
                                      extra_per_game, pct_of_gap)])
}
say("\nRows with no named winner in chains are excluded from the table above;")
say("chains names the beaten aerial opponent only some of the time, so this is")
say("a lower bound on the move, not the whole of it.")
say("  named winners on clamped rows: ", format(sum(!is.na(cl$position_group)), big.mark = ","),
    " of ", format(nrow(cl), big.mark = ","))
