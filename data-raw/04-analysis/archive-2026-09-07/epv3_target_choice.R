# Which target should an optimiser minimise error against: margin, or xscore
# margin? Measured rather than argued.
#
# The load_xg() frame is already one row per match with home_xscore/away_xscore
# and xscore_diff, so no aggregation is needed -- the earlier screen looked for
# shot-level column names that do not exist here and reported "NONE".

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT <- "C:/dev/torpverse/torp/data-raw/outputs/epv3_target_choice.txt"
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

xg <- as.data.table(load_xg(TRUE))
say("=== Target choice: margin vs xscore margin ===")
say("xg rows ", nrow(xg))

d <- xg[is.finite(score_diff) & is.finite(xscore_diff)]
say("matches with both: ", nrow(d))
say("")
say("sd(score_diff)   = ", round(sd(d$score_diff), 3))
say("sd(xscore_diff)  = ", round(sd(d$xscore_diff), 3))
say("cor              = ", round(cor(d$score_diff, d$xscore_diff), 4))
say("sd(score - xscore) = ", round(sd(d$score_diff - d$xscore_diff), 3),
    "   <- conversion luck: real, and no rating should be asked to predict it")
say("")
r2 <- cor(d$score_diff, d$xscore_diff)^2
say("xscore_diff explains ", round(100 * r2, 1), "% of margin variance.")
say("So ", round(100 * (1 - r2), 1), "% of what an optimiser targeting MARGIN")
say("would be chasing is orthogonal to expected scoring.")

# stats-discipline rule 3, applied to the target choice: the gain from a
# lower-noise target is roughly the ratio of irreducible noise.
say("")
say("--- what does the quieter target buy in statistical power? ---")
say("Effective sample size scales like 1/noise-variance for the same signal.")
say("Ratio sd(margin)^2 / sd(xmargin)^2 = ",
    round(sd(d$score_diff)^2 / sd(d$xscore_diff)^2, 3))
say("i.e. one match measured against xscore margin is worth roughly that many")
say("matches measured against margin, for the SAME underlying effect.")

say("")
say("--- and is xscore margin itself predictable from ratings at all? ---")
say("(if xscore margin were pure noise too, the swap buys nothing)")
say("sd of the two targets is the headline; the screen in")
say("epv3_optimiser_headroom.R reports R2 against each.")

say("")
say("=== RECOMMENDATION ===")
say("Optimise the inner loop against XSCORE MARGIN, gate on REAL MARGIN.")
say("Reasons, in order:")
say(" 1. it is the quieter target by the ratio above, so every match carries")
say("    more information about the parameter being tuned")
say(" 2. conversion luck is explicitly not a player-rating property -- a player")
say("    who created five good chances that missed did not play worse")
say(" 3. the production match model ALREADY passes through xscore as an")
say("    intermediate GAM step (total_xpoints -> xscore_diff -> score_diff),")
say("    so aligning the rating to xscore is coherent with the architecture,")
say("    not a detour")
say(" 4. but what SHIPS is margin/bits/Brier, so the winner must clear the")
say("    full ws17 gate on those before it counts. Optimising and gating on the")
say("    same quantity is how a metric-forcing fix gets through.")

close(con)
cat("\nWrote ", OUT, "\n")
