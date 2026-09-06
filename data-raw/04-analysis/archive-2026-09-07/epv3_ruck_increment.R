# How much does winning the tap actually change who gets the ball?
#
# THE QUESTION THIS CLOSES. The stoppage swing is ~1.32 points (the state is
# worth +0.661 to whoever wins it, and the mirror to whoever does not), and the
# box weights pay a tap-winning ruck 0.0742. That looks like an 18x under-payment
# and it is not, because the tap does not decide possession: of 66,853 resolved
# stoppages, only 24.7% are next touched via `Gather From Hitout`, against
# `Loose Ball Get` 30.5% and `Hard Ball Get` 19.6%. Three quarters are settled on
# the ground.
#
# So the ruck owns the INCREMENT -- how much more often his team gets the ball
# when he wins the tap than when he does not -- times 1.32. Nobody has measured
# that increment, and without it the ruck channel cannot be priced. This is the
# last box-score carve-out in an otherwise chain-native metric.
#
# TWO ROUTES, because neither is clean on its own.
#
#   TEAM ROUTE. Regress clearance differential on hitout differential per match.
#   The slope is "extra clearances per extra hitout". Confounded upward: good
#   midfields both win hitouts (via better centre-bounce setups) and win
#   clearances independently. So it is an UPPER bound.
#
#   STOPPAGE ROUTE. At each stoppage, who touches it next and from what. A
#   `Gather From Hitout` names the gathering team, and a hitout by definition
#   went to a team, so this is the direct conditional -- but it is only
#   observable on the stoppages where the tap was decisive, which biases it. So
#   it is a LOWER bound on the causal effect and an upper bound on the share of
#   stoppages the ruck controls.
#
# Reporting both and their implied point values is the honest answer; a single
# number here would be false precision.
#
# ~3 min.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_ruck_increment.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

SWING <- 1.3225   # measured, epv3_stoppage_swing.R: 2 x 0.6612
PAID  <- EPV_HITOUT_WT + EPV_RUCK_CONTEST_WT

say("=== What is a ruck tap worth? ===")
say("run at ", format(Sys.time()))
say("stoppage swing (measured): ", SWING, " points")
say("paid to a tap-winning ruck by the box weights: ", round(PAID, 4))

# ---------------------------------------------------------------------------
# TEAM ROUTE
# ---------------------------------------------------------------------------
ps <- as.data.table(load_player_stats(TRUE))
need <- c("match_id", "team_id", "hitouts", "clearances")
miss <- setdiff(need, names(ps))
if (length(miss) > 0) {
  say("MISSING columns: ", paste(miss, collapse = ", "))
} else {
  tm <- ps[, .(hitouts = sum(hitouts, na.rm = TRUE),
               clearances = sum(clearances, na.rm = TRUE),
               disposals = sum(disposals, na.rm = TRUE)),
           by = .(match_id, team_id)]
  tm <- tm[, if (.N == 2) .SD else NULL, by = match_id]
  setorder(tm, match_id, team_id)
  d <- tm[, .(d_hitouts = hitouts[1] - hitouts[2],
              d_clear   = clearances[1] - clearances[2],
              d_disp    = disposals[1] - disposals[2],
              tot_hitouts = sum(hitouts)), by = match_id]
  d <- d[is.finite(d_hitouts) & is.finite(d_clear)]
  say("")
  say("=== TEAM ROUTE: clearance differential on hitout differential ===")
  say("matches ", nrow(d), " | mean hitouts per match ",
      round(mean(d$tot_hitouts), 1))
  f <- lm(d_clear ~ d_hitouts, data = d)
  co <- summary(f)$coefficients
  say_dt(data.table(term = rownames(co), coef = round(co[, 1], 4),
                    se = round(co[, 2], 4), t = round(co[, 3], 2)), 4)
  slope <- co["d_hitouts", 1]
  say("")
  say("slope: ", round(slope, 4), " extra clearances per extra hitout")
  say("cor(d_hitouts, d_clear) = ", round(cor(d$d_hitouts, d$d_clear), 3))
  say("")
  say("A hitout differential of 1 buys ", round(slope, 3), " of a clearance.")
  say("A clearance is a possession from a stoppage, so in swing terms one extra")
  say("hitout is worth about ", round(slope * SWING / 2, 4), " points")
  say("(half the swing, because a clearance differential of 1 moves ONE")
  say("stoppage from them to us, not two).")
  say("")
  say("=> implied per-tap value ", round(slope * SWING / 2, 4),
      " against ", round(PAID, 4), " paid  =  ",
      round(slope * SWING / 2 / PAID, 2), "x")
  say("")
  say("*** THIS IS AN UPPER BOUND. *** A good midfield wins hitouts AND wins")
  say("clearances for reasons that have nothing to do with the ruck, so the")
  say("slope absorbs midfield quality. Control for it:")
  if ("d_disp" %in% names(d)) {
    f2 <- lm(d_clear ~ d_hitouts + d_disp, data = d)
    co2 <- summary(f2)$coefficients
    say_dt(data.table(term = rownames(co2), coef = round(co2[, 1], 4),
                      t = round(co2[, 3], 2)), 4)
    s2 <- co2["d_hitouts", 1]
    say("controlled slope ", round(s2, 4), " => per-tap ",
        round(s2 * SWING / 2, 4), "  =  ", round(s2 * SWING / 2 / PAID, 2), "x paid")
  }
}

# ---------------------------------------------------------------------------
# STOPPAGE ROUTE
# ---------------------------------------------------------------------------
say("")
say("=== STOPPAGE ROUTE: what fraction of stoppages does the tap decide? ===")
full <- as.data.table(load_pbp(TRUE))
p <- data.table(match_id = full$match_id, display_order = full$display_order,
                description = full$description, team_id = full$team_id,
                exp_pts = full$exp_pts)
rm(full); invisible(gc())
setorder(p, match_id, display_order)
p[, `:=`(f1_desc = shift(description, 1, type = "lead"),
         f1_tid  = shift(team_id, 1, type = "lead"),
         f2_desc = shift(description, 2, type = "lead"),
         f2_tid  = shift(team_id, 2, type = "lead")), by = match_id]

st <- p[description %chin% c("Centre Bounce", "Ball Up Call")]
st[, `:=`(nxt = f1_desc, ntid = f1_tid)]
st[is.na(ntid), `:=`(nxt = f2_desc, ntid = f2_tid)]
say("stoppages ", format(nrow(st), big.mark = ","))
tab <- st[!is.na(nxt), .(n = .N), by = nxt][order(-n)]
tab[, pct := round(100 * n / sum(n), 1)]
say_dt(tab[1:10], 10)

ho_share <- st[!is.na(nxt), mean(nxt == "Gather From Hitout")]
say("")
say("share of stoppages resolved by a clean tap (`Gather From Hitout`): ",
    round(100 * ho_share, 1), "%")
say("")
say("On those, the tap decided possession by construction. On the rest the ball")
say("was contested on the ground. If the ruck's tap only shifts possession on")
say("the ", round(100 * ho_share, 1), "% it cleanly wins, and a coin-flip would")
say("have given his team half of those anyway, his marginal effect is at most")
say(round(ho_share / 2, 4), " of a possession per stoppage, i.e. ",
    round(ho_share / 2 * SWING, 4), " points -- shared between the two rucks.")
say("")
say("=> per-ruck upper bound from this route: ",
    round(ho_share / 2 * SWING / 2, 4), " points per stoppage against ",
    round(PAID, 4), " paid  =  ", round(ho_share / 2 * SWING / 2 / PAID, 2), "x")

say("")
say("=== WHAT TO DO WITH THIS ===")
say("Both routes are bounds, not estimates, and they bound from opposite sides.")
say("Quote the range. What it already rules out is the 3.14x that the")
say("centre-bounce artifact produced and the 7.4x the regression produced --")
say("if the honest range sits well below those, EPV_RUCK_SWING_SCALE stays at 1")
say("until somebody runs the experiment properly (tap winner joined to the")
say("stoppage, which needs a data source that names the ruck at the contest --")
say("chains does not, 0.0% of Centre Bounce rows carry a player_id).")

close(con)
cat("\nDone\n")
