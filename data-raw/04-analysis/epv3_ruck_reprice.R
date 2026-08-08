# Reprice the stoppage channel from the measured tap values, in the CHANNEL's
# own units.
#
# WHAT IS ALREADY ESTABLISHED (epv3_ruck_to_advantage.txt). With raw taps and
# taps-to-advantage both in the model, per team-match differential:
#
#   d_hitouts              -0.0417  (t -3.5)
#   d_hitouts_to_advantage +0.2029  (t  6.4)     stable OOS: +0.219 / +0.212
#
# THE PARAMETERISATION MATTERS AND THE OBVIOUS READING IS WRONG. A negative
# coefficient on hitouts does not mean winning a tap destroys value. `hitouts`
# appears in both terms, so with `hta` held fixed its coefficient is the effect
# of an UNDIRECTED tap:
#
#   undirected tap   Y = -0.0417 differential  =  -0.0209 per ruck
#   directed tap     X = -0.0417 + 0.2029      =  +0.0806 per ruck
#
# The +0.1015 quoted elsewhere is X - Y, which is the right number to compare
# against EPV_HITOUT_ADV_WT because the channel formula adds it ON TOP of
# EPV_HITOUT_WT. It is not the value of a directed tap.
#
# A CONVERGENCE WORTH CHECKING RATHER THAN CELEBRATING. epv3_ruck_increment.txt
# bounded the ruck's marginal effect at 0.0817 points per stoppage by a
# completely different route: 24.7% of stoppages resolve by a clean tap, a coin
# flip would have given his team half of those anyway. That is within 1.4% of the
# +0.0806 here. Two independent routes agreeing is strong evidence -- IF the
# units match. One is per stoppage and one is per directed tap, and a ruck does
# not win one directed tap per stoppage. This script checks whether they are the
# same quantity before anything is claimed.
#
# WHAT IS PAID TODAY, per ruck (EPV3_STOP_ZERO_SUM = TRUE):
#   epv_cont_stop = ho*HITOUT_WT + hta*HITOUT_ADV_WT + ho*RUCK_CONTEST_WT
#                   - max(0, rc - ho)*RUCK_LOSS_WT
# so a tap pays HITOUT_WT + RUCK_CONTEST_WT = 0.0742 and a directed one adds
# 0.1748.
#
# This does NOT change any constant. It measures the gap per ruck-game in the
# channel's own units and writes the proposed weights out for a gate to test.
#
# ~4 min, box score only.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_ruck_reprice.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

say("=== Repricing the stoppage channel ==="); say("run at ", format(Sys.time()))

RA <- readRDS(file.path(OUT_DIR, "epv3_ruck_to_advantage.rds"))
cf <- RA$coefs
Y_diff <- unname(cf[["d_hitouts"]])
X_diff <- unname(cf[["d_hitouts"]] + cf[["d_hitouts_to_advantage"]])
Y <- Y_diff / 2; X <- X_diff / 2
say(sprintf("measured per ruck: undirected tap %+.4f | directed tap %+.4f | difference %+.4f",
            Y, X, X - Y))
say(sprintf("paid per ruck:     undirected tap %+.4f | directed tap %+.4f | difference %+.4f",
            EPV_HITOUT_WT + EPV_RUCK_CONTEST_WT,
            EPV_HITOUT_WT + EPV_RUCK_CONTEST_WT + EPV_HITOUT_ADV_WT,
            EPV_HITOUT_ADV_WT))

ps <- as.data.table(load_player_stats(TRUE))
ps[, team := fifelse(team_status == "home", home_team_name, away_team_name)]
r <- ps[!is.na(hitouts) & ruck_contests > 0 & hitouts > 0]
say(""); say("ruck-games (ruck_contests > 0 and at least one hitout): ",
             format(nrow(r), big.mark = ","))

r[, `:=`(ho = fcoalesce(as.numeric(hitouts), 0),
         hta = fcoalesce(as.numeric(hitouts_to_advantage), 0),
         rc = fcoalesce(as.numeric(ruck_contests), 0))]
r[, `:=`(
  paid = ho * EPV_HITOUT_WT + hta * EPV_HITOUT_ADV_WT + ho * EPV_RUCK_CONTEST_WT -
         pmax(0, rc - ho) * EPV_RUCK_LOSS_WT,
  measured = (ho - hta) * Y + hta * X)]

say(""); say("=== 1. PER RUCK-GAME, PAID vs MEASURED ===")
say_dt(data.table(
  quantity = c("hitouts", "hitouts to advantage", "ruck contests",
               "PAID (epv_cont_stop)", "MEASURED (tap values)"),
  mean = round(c(mean(r$ho), mean(r$hta), mean(r$rc), mean(r$paid), mean(r$measured)), 3),
  sd = round(c(sd(r$ho), sd(r$hta), sd(r$rc), sd(r$paid), sd(r$measured)), 3)), 5)
say("")
say(sprintf("  ratio of means  paid / measured = %.2fx", mean(r$paid) / mean(r$measured)))
say(sprintf("  ratio of spreads                = %.2fx", sd(r$paid) / sd(r$measured)))
say(sprintf("  cor(paid, measured)             = %.4f", cor(r$paid, r$measured)))
say("")
say("  The CORRELATION is the important one. If the two are highly correlated the")
say("  channel is ranking rucks correctly and only the SCALE is wrong, which one")
say("  constant fixes. If not, the shape is wrong and rescaling will not help.")

say(""); say("=== 2. DOES THE UNIT CONVERGENCE HOLD? ===")
nm <- uniqueN(ps$match_id)
say(sprintf("  directed taps per team-match: %.2f", 2 * sum(r$hta) / (2 * nm)))
say(sprintf("  stoppages per match (from epv3_ruck_increment): 53.9"))
say(sprintf("  so a directed tap is NOT one per stoppage -- %.2f directed taps",
            sum(r$hta) / nm), " per match against 53.9 stoppages")
say("")
say("  the 0.0817/stoppage bound and the +0.0806/directed-tap estimate are")
say("  therefore DIFFERENT QUANTITIES and their agreement is a coincidence of")
say("  magnitude, not independent confirmation. Do not quote it as confirmation.")
say("")
say(sprintf("  total measured ruck value per match, both teams: %.3f points",
            sum(r$measured) / nm))
say(sprintf("  total stoppage value per match (chain, both teams): ~%.1f points",
            2 * 5.58))
say(sprintf("  => the ruck's marginal share of stoppage value: %.1f%%",
            100 * (sum(r$measured) / nm) / (2 * 5.58)))
say("")
say("  Pete's prior was that the ruck carries most of a stoppage's value. On this")
say("  measurement he carries the DIRECTIONAL part, which is a minority of the")
say("  total -- most of a stoppage's value is decided on the ground afterwards.")
say("  The prior is right about the ruck mattering and about WHERE (direction);")
say("  the share is smaller than 'most'.")

say(""); say("=== 3. PROPOSED WEIGHTS ===")
say("Keeping the channel's existing shape and only refitting its constants:")
prop <- list(EPV_HITOUT_WT = round(Y - EPV_RUCK_CONTEST_WT, 4),
             EPV_HITOUT_ADV_WT = round(X - Y, 4))
say_dt(data.table(
  constant = c("EPV_HITOUT_WT", "EPV_HITOUT_ADV_WT", "EPV_RUCK_CONTEST_WT", "EPV_RUCK_LOSS_WT"),
  current = c(EPV_HITOUT_WT, EPV_HITOUT_ADV_WT, EPV_RUCK_CONTEST_WT, EPV_RUCK_LOSS_WT),
  proposed = c(prop$EPV_HITOUT_WT, prop$EPV_HITOUT_ADV_WT,
               EPV_RUCK_CONTEST_WT, EPV_RUCK_LOSS_WT)), 4)
say("")
say("EPV_RUCK_CONTEST_WT and EPV_RUCK_LOSS_WT are left alone deliberately: they")
say("are the win/loss ATTENDANCE ledger, a different quantity from what a tap is")
say("worth, and nothing here measures them. EPV_HITOUT_WT absorbs the difference")
say("so the per-tap total lands on the measured value.")
say("")
say("CAVEAT THAT GATES THIS. The measured side is stoppage EPV in the chain")
say("frame; the paid side feeds epv_cont_stop, which then passes through")
say("EPV3_SUB_SCALE and EPV3_POINTS_SCALE before reaching a rating. Both are")
say("nominally expected points so the comparison is meaningful, but these weights")
say("must be gated on rating quality and MAE, not adopted from this table.")

saveRDS(list(Y = Y, X = X, proposed = prop,
             cor_paid_measured = cor(r$paid, r$measured)),
        file.path(OUT_DIR, "epv3_ruck_reprice.rds"))
say(""); say("done ", format(Sys.time())); close(con); cat("\nDone\n")
