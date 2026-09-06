# Where does TOG enter, and what does the ranking look like at each stage?
#
# HOW IT WORKS NOW. .position_adjust() touches time-on-ground TWICE:
#
#   1. p80  = epv / TOG          divide out minutes to get a per-80 RATE
#   2. adj  = (p80 - m) * TOG    multiply minutes back in at the end
#
# Algebraically that is  adj = epv - m*TOG: raw output minus what a typical
# player in your cell would have produced in YOUR minutes. Which is sensible,
# and is NOT double-counting.
#
# THE SUSPICION. Under volume conditioning the leaderboard filled with
# part-timers and cor(adj, tog) hit -0.519, and the natural suspect is step 1.
# A ruck who plays 38% of the game rucks for nearly all of it, so dividing his
# output by 0.38 gives a large RATE that no full-timer can match -- full-timers
# rest and rotate forward.
#
# So this prints the ranking at every stage, with TOG alongside, and lets the
# data say which step does the damage:
#
#   A. raw                    no TOG anywhere
#   B. p80                    after dividing by TOG
#   C. residual               p80 minus the volume curve  (pre multiply-back)
#   D. adj = residual * TOG   the full thing
#   E. raw - E[raw | rc]      conditioning WITHOUT any per-80 step at all

suppressMessages({
  library(data.table); library(arrow); library(mgcv)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "tog_stages.txt"), split = TRUE)
cat("=== Where TOG enters, and the ranking at each stage ===\nrun at",
    format(Sys.time()), "\n")

d <- as.data.table(read_parquet(file.path(OUT_DIR, "v2_schemec_pgd.parquet")))
d[, tog_safe := pmax(fcoalesce(time_on_ground_percentage / 100, 0.1), 0.1)]
d[, rc := fcoalesce(as.numeric(ruck_contests), 0)]
d[, p80 := epv_hitout / tog_safe]
S <- max(d$season, na.rm = TRUE)

cat("\n########## THE TWO PLACES TOG IS USED ##########\n")
cat("  step 1  p80 = epv / TOG        -> a per-80-minute RATE\n")
cat("  step 2  adj = (p80 - m) * TOG  -> minutes multiplied back in\n")
cat("  net     adj = epv - m*TOG      -> raw minus what a typical player in\n")
cat("                                    your cell makes in YOUR minutes\n")

# The two curves: one on the RATE, one on RAW output.
fit_rate <- bam(p80 ~ s(rc, k = 20), data = d, weights = d$tog_safe, discrete = TRUE)
fit_raw  <- bam(epv_hitout ~ s(rc, k = 20), data = d, discrete = TRUE)
d[, exp_rate := as.numeric(predict(fit_rate, newdata = d))]
d[, exp_raw  := as.numeric(predict(fit_raw,  newdata = d))]
d[, resid_rate := p80 - exp_rate]          # C: pre multiply-back
d[, adj_vol := resid_rate * tog_safe]      # D: full
d[, adj_noper80 := epv_hitout - exp_raw]   # E: no per-80 at all

pl <- d[season == S, .(gm = .N,
                       tog = round(mean(tog_safe) * 100, 0),
                       rc = round(mean(rc), 0),
                       A_raw = round(mean(epv_hitout, na.rm = TRUE), 2),
                       B_p80 = round(mean(p80, na.rm = TRUE), 2),
                       C_resid = round(mean(resid_rate, na.rm = TRUE), 3),
                       D_adj = round(mean(adj_vol, na.rm = TRUE), 3),
                       E_noper80 = round(mean(adj_noper80, na.rm = TRUE), 3),
                       schemeC = round(mean(epv_hitout_adj, na.rm = TRUE), 3)),
        by = player_name][gm >= 6]

show <- function(col, lbl) {
  # setorder() takes literal column names; setorderv() takes a variable.
  data.table::setorderv(pl, col, order = -1L)
  cat("\n--", lbl, "--\n")
  print(pl[1:10, .(player_name, tog, rc, A_raw, B_p80, C_resid, D_adj, E_noper80)],
        nrows = 12)
}
show("A_raw",     "A. RAW output (no TOG anywhere)")
show("B_p80",     "B. after DIVIDING by TOG (the per-80 rate)")
show("C_resid",   "C. rate minus the volume curve -- PRE multiply-back")
show("D_adj",     "D. residual x TOG -- the full volume scheme")
show("E_noper80", "E. raw minus E[raw | contests] -- NO per-80 step at all")

cat("\n########## WHICH STAGE FAVOURS PART-TIMERS? ##########\n")
cat("correlation of each stage with TOG. Near 0 is what we want; negative means\n")
cat("the stage rewards playing FEWER minutes.\n\n")
act <- pl[A_raw > quantile(pl$A_raw, 0.75, na.rm = TRUE)]
st <- data.table(
  stage = c("A raw", "B per-80 rate", "C residual (pre xTOG)", "D residual x TOG",
            "E no per-80 at all", "scheme C bucket"),
  cor_with_tog = round(c(cor(act$A_raw, act$tog), cor(act$B_p80, act$tog),
                         cor(act$C_resid, act$tog), cor(act$D_adj, act$tog),
                         cor(act$E_noper80, act$tog), cor(act$schemeC, act$tog)), 3),
  tracks_production = round(c(1, cor(act$A_raw, act$B_p80), cor(act$A_raw, act$C_resid),
                              cor(act$A_raw, act$D_adj), cor(act$A_raw, act$E_noper80),
                              cor(act$A_raw, act$schemeC)), 3))
print(st)

cat("\n########## READ ##########\n")
cat("If B is strongly negative with TOG, the per-80 DIVISION is what favours\n")
cat("part-timers, and the multiply-back in D only partly undoes it.\n")
cat("If E is near zero on TOG while still tracking production, then dropping\n")
cat("the per-80 step entirely -- conditioning raw output on contests directly --\n")
cat("is the version worth building.\n")

saveRDS(list(players = pl, stages = st), file.path(OUT_DIR, "tog_stages.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
