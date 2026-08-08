# What is WINNING A 50/50 worth? Priced from EPV state values, not event deltas.
# =============================================================================
# Pete's formulation, 2026-07-30, and it is the right one. Every previous attempt to
# price a contested event used a row's own `delta_epv` -- which is FORWARD-looking
# (add_variables.R:66-67: next row's state minus this row's), so it credits the
# actor's NEXT ACTION rather than the value of the event happening. That is what
# invalidated the hitout price: `Gather From Hitout`'s delta belongs to the gatherer,
# while `hitout_adv_wt` pays the ruck. It is also why `Centre Bounce` reads +0.795 at
# 99.7% positive -- a neutral state is always followed by someone gaining the ball.
#
# THIS METHOD USES NO DELTAS AT ALL, so none of that applies. It uses only state
# values:
#
#   V(x)        = expected points differential for the team IN POSSESSION at x
#   V_them(x)   = the same physical spot with the OPPONENT in possession
#               = -V(-x)   (they attack the other way, so their x is mirrored)
#   toss_up(x)  = 0.5*V(x) + 0.5*V_them(x) = 0.5*(V(x) - V(-x))
#   win(x)      = V(x) - toss_up(x)        = 0.5*(V(x) + V(-x))
#
# So the value of WINNING a 50/50 is half the swing between the two possession
# states -- Pete's worked example: us +3, them 0 -> toss-up 1.5 -> winning worth 1.5.
#
# Note the clean property at the centre circle: toss_up(0) = 0 by symmetry, so
# winning a centre bounce is worth exactly V(0). That is a useful correctness check
# on the arithmetic rather than an assumption.

suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)

p <- torp:::default_epv_params()
SEASONS <- 2024:2026
pbp <- as.data.table(load_pbp(SEASONS, rounds = TRUE))
cli::cli_alert_info("{nrow(pbp)} pbp rows, seasons {min(SEASONS)}-{max(SEASONS)}")
stopifnot(all(c("x", "y", "exp_pts", "description") %in% names(pbp)))

cli::cli_h1("1. orientation check -- which way does x point?")
# V(x) must be read with x oriented toward the POSSESSING team's goal. Establish
# that from the data rather than assuming the convention.
S <- pbp[is.finite(x) & is.finite(exp_pts)]
S[, xbin := cut(x, breaks = seq(-90, 90, by = 15))]
prof <- S[, .(n = .N, mean_exp_pts = round(mean(exp_pts), 3)), by = xbin][order(xbin)]
print(prof, row.names = FALSE)
rho <- cor(S$x, S$exp_pts)
cli::cli_alert_info("cor(x, exp_pts) = {round(rho, 3)}")
if (rho < 0.1) {
  cli::cli_abort(c("exp_pts does not rise with x -- the orientation assumption is wrong.",
                   "x" = "Refusing to build V(x) on an unverified orientation."))
}
cli::cli_alert_success("Positive x is the possessing team's attacking direction.")

cli::cli_h1("2. build V(x) -- state value by field position")
# Smooth in x only. y matters (angle), but a ruck contest is a centre-corridor
# event, so x carries almost all of the signal and a 1-D curve keeps this readable.
BW <- 5   # metres per bin
S[, xb := round(x / BW) * BW]
V <- S[, .(n = .N, V = mean(exp_pts)), by = xb][order(xb)][n >= 200]
cli::cli_alert_info("{nrow(V)} bins of {BW}m with >=200 observations, x from {min(V$xb)} to {max(V$xb)}")

# V(-x) by self-join, so win(x) can be formed. Bins without a mirror are dropped
# rather than extrapolated.
V <- merge(V, V[, .(xb_neg = -xb, V_mirror = V)], by.x = "xb", by.y = "xb_neg", all.x = TRUE)
V <- V[is.finite(V_mirror)]
V[, `:=`(toss_up = 0.5 * (V - V_mirror),
         win_value = 0.5 * (V + V_mirror))]
print(V[, .(xb, n, V = round(V, 3), V_them = round(-V_mirror, 3),
            toss_up = round(toss_up, 3), win_value = round(win_value, 3))][seq(1, .N, by = 2)],
      row.names = FALSE)

cli::cli_h1("3. correctness check: toss_up(0) should be 0, win(0) should equal V(0)")
z <- V[xb == 0]
if (nrow(z)) {
  cli::cli_alert_info("at x=0: V {round(z$V,4)}, toss_up {round(z$toss_up,4)}, win_value {round(z$win_value,4)}")
  if (abs(z$toss_up) < 0.02 && abs(z$win_value - z$V) < 0.02) {
    cli::cli_alert_success("Symmetry holds -- the arithmetic is behaving.")
  } else {
    cli::cli_alert_danger("Symmetry FAILS at the centre. V(x) is asymmetric, so x is not centred on the ground's midpoint.")
  }
}

cli::cli_h1("4. price the ruck contests, at their ACTUAL locations")
# Ruck contests are not all at the centre circle: centre bounces are, ball-ups are
# spread. Use the real location distribution rather than evaluating at x=0.
RUCK_EVENTS <- c("Centre Bounce", "Ball Up Call", "Bounce")
rk <- pbp[description %in% RUCK_EVENTS & is.finite(x)]
rk[, xb := round(x / BW) * BW]
rk[V, win_value := i.win_value, on = "xb"]
cli::cli_alert_info("{nrow(rk)} ruck-contest rows; {sum(is.na(rk$win_value))} outside the fitted x range (dropped)")
byev <- rk[is.finite(win_value), .(n = .N, mean_win_value = round(mean(win_value), 4)), by = description][order(-n)]
print(byev, row.names = FALSE)
overall <- rk[is.finite(win_value), mean(win_value)]
cli::cli_alert_info("mean value of WINNING a ruck contest, across all types: {round(overall, 4)} points")

cli::cli_h1("4b. THE HONEST VERSION -- measure both states from the ACTUAL contest outcomes")
# Section 4 uses the global V(x) curve, and that OVERSTATES the value of winning a
# tap. Rows at a given x are mostly players with CLEAN possession in space; winning
# a ruck tap gives CONTESTED, messy possession. Using the clean-possession curve
# priced a centre bounce at ~0.93, which at half credit would pay a ruck ~9 points a
# game against a mean total epv of ~7. Obviously wrong, so it is not used.
#
# Same formula, both states measured empirically instead: for each contest, take the
# state a few rows later, oriented to ONE nominated team, split by who actually won
# the contest. Then
#   V_win  = value to team A when A won it
#   V_lose = value to team A when B won it
#   toss_up = 0.5*(V_win + V_lose)      <- the 50/50 Pete describes
#   credit  = V_win - toss_up = 0.5*(V_win - V_lose)
# This keeps the counterfactual baseline and drops the clean/messy mismatch.
setorder(pbp, match_id, display_order)
pbp[, `:=`(row_i = seq_len(.N)), by = match_id]
LOOK <- 2L   # rows ahead: far enough for possession to be established, close
             # enough that it is still this contest's consequence
pbp[, `:=`(fwd_exp = shift(exp_pts, LOOK, type = "lead"),
           fwd_team = shift(team_id_mdl, LOOK, type = "lead"),
           fwd_desc = shift(description, LOOK, type = "lead")), by = match_id]

# WHO WON CANNOT BE READ BY COMPARING TEAMS FORWARD. A contest row's `team_id_mdl`
# is ALREADY the winner's -- pbp assigns the row to whoever came out with the ball.
# A first attempt compared the contest row's team to the team 2 rows later and got
# "94% won" on centre bounces, which is that artefact, not a contest rate. So V_lose
# is not observable by splitting; it comes from the MIRROR instead:
#
#   V_win(x)  = state value a couple of rows after a contest at x, to the WINNER
#   team B winning at physical x is team A losing there, and by symmetry that is
#   the mirror of A winning at -x, so:  V_lose_to_A(x) = -V_win(-x)
#   toss_up(x) = 0.5*(V_win(x) - V_win(-x))
#   credit(x)  = V_win(x) - toss_up(x) = 0.5*(V_win(x) + V_win(-x))
#
# Same shape as section 2, but V is now measured on POST-CONTEST states only, which
# is what removes the clean-possession overstatement.
price_duel <- function(dt, label) {
  q <- dt[is.finite(fwd_exp) & is.finite(x)]
  if (nrow(q) < 500) { cli::cli_alert_warning("{label}: only {nrow(q)} usable rows -- skipped"); return(NULL) }
  q[, xb := round(x / BW) * BW]
  # fwd_exp is already oriented to the team in possession at that later row; take it
  # to the contest winner via whether possession is still theirs.
  q[, v_win := ifelse(fwd_team == team_id_mdl | is.na(fwd_team), fwd_exp, -fwd_exp)]
  W <- q[, .(n = .N, Vw = mean(v_win)), by = xb][n >= 50]
  if (nrow(W) < 4) { cli::cli_alert_warning("{label}: too few populated bins -- skipped"); return(NULL) }
  W <- merge(W, W[, .(xb_neg = -xb, Vw_mirror = Vw)], by.x = "xb", by.y = "xb_neg")
  W[, `:=`(toss_up = 0.5 * (Vw - Vw_mirror), credit = 0.5 * (Vw + Vw_mirror))]
  # Weight by where the contests actually happen.
  wt <- W$n / sum(W$n)
  data.table(duel = label, n = nrow(q), bins = nrow(W),
             V_win_mean = round(sum(W$Vw * wt), 4),
             toss_up = round(sum(W$toss_up * wt), 4),
             credit_for_winning = round(sum(W$credit * wt), 4))
}

duels <- rbindlist(list(
  price_duel(pbp[description %in% RUCK_EVENTS], "ruck contest (all)"),
  price_duel(pbp[description == "Centre Bounce"], "centre bounce"),
  price_duel(pbp[description %in% c("Ball Up Call", "Bounce")], "ball-up / boundary"),
  price_duel(pbp[description == "Contested Mark"], "contested mark"),
  price_duel(pbp[description == "Spoil"], "spoil"),
  price_duel(pbp[description == "Hard Ball Get"], "hard ball get")
), fill = TRUE)
print(duels, row.names = FALSE)
cli::cli_alert_info("credit_for_winning is HALF the swing between winning and losing -- Pete's 50/50 baseline.")
ruck_credit <- duels[duel == "ruck contest (all)"]$credit_for_winning

cli::cli_h1("5. implied ruck weights vs production")
cli::cli_alert_info("Using the EMPIRICAL duel price (4b), not the global-curve one (4).")
for (sh in c(1/3, 1/2, 1)) {
  cli::cli_alert_info("ruck share {round(sh,3)} -> implied credit per contest WON = {round(ruck_credit * sh, 4)}")
}
cli::cli_alert_info("(the global-curve version would have said {round(overall * 0.5, 4)} at half share -- overstated, see 4b)")
# hitouts_to_advantage is the count of contests the ruck WON in a way that gave his
# team the ball, so it is the count this value attaches to.
cli::cli_alert_info("production hitout_adv_wt = {p$hitout_adv_wt}, hitout_wt = {p$hitout_wt}, ruck_contest_wt = {p$ruck_contest_wt}")
imp_half <- ruck_credit * 0.5
cli::cli_alert_info("At half credit the implied hitout_adv_wt is {round(imp_half, 4)} vs production {p$hitout_adv_wt} ({round(p$hitout_adv_wt/imp_half, 2)}x)")

cli::cli_h1("6. sanity: does win_value behave like football?")
# Winning the ball should be worth MORE the closer you are to your own goal to
# attack -- and the curve should be flattest deep in defence. If it is flat or
# inverted, V(x) is not a value surface and nothing above means anything.
cli::cli_alert_info("win_value at x=-60: {round(V[xb==-60]$win_value, 3)} | x=0: {round(V[xb==0]$win_value, 3)} | x=+60: {round(V[xb==60]$win_value, 3)}")
# NOT a test that win_value rises toward goal -- it CANNOT. win_value(x) =
# 0.5*(V(x) + V(-x)) is an even function of x, so it is symmetric by construction.
# An earlier version of this script asserted it should rise and reported a failure,
# which was the check being wrong, not the data. What the symmetry means is that a
# 50/50 is worth the same at both ends of the ground; the readable fact is that it
# is worth MORE near either goal than at the centre.
if (nrow(V[xb == 60]) && nrow(V[xb == 0]) && V[xb == 60]$win_value > V[xb == 0]$win_value) {
  cli::cli_alert_success("Contests near goal are worth more than contests at the centre, as expected.")
} else {
  cli::cli_alert_danger("Centre contests price ABOVE goal-line contests -- that would be wrong; investigate.")
}

saveRDS(list(V = V, ruck = byev, overall = overall),
        "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse/92e2b422-0dee-4727-90de-364d23375767/scratchpad/contest_state_price.rds")
cli::cli_alert_success("done")
