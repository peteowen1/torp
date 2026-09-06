# Split the ruck's value three ways: attending, winning, winning to advantage.
#
# WHY THIS EXISTS. `epv3_ruck_to_advantage.R` regressed stoppage EPV on
# `d_hitouts` and `d_hitouts_to_advantage` and NEVER INCLUDED ATTENDANCE. Its
# -0.0417 on raw taps is therefore the effect of an extra tap *without*
# controlling for how many contests the ruck was in -- and winning more and
# attending more are strongly correlated, so that one coefficient is carrying
# both effects. Reading it as "winning a tap is worth less than nothing" was my
# error, and it is the number I was about to write into the constants.
#
# Pete's structure, which this tests: attendance NEGATIVE (a contest you might
# lose hands the opposition the ball), a won tap SLIGHTLY POSITIVE, a tap to
# advantage LARGELY POSITIVE. That is also exactly the ledger EPV3_STOP_ZERO_SUM
# already implements for v3 -- pay for wins, debit for losses -- so if it holds,
# v2's pay-to-attend formula is the odd one out rather than the baseline.
#
# THE IDENTIFICATION QUESTION, asked before the answer is read. Contests, wins
# and wins-to-advantage are nested (rc >= h >= hta), so they are collinear by
# construction. If the VIFs are large the three-way split is not identified and
# the coefficients will be unstable noise dressed as structure -- exactly the
# trap the two-way fit fell into from the other direction. The VIF table and an
# out-of-sample split are printed BEFORE the verdict for that reason, and a
# failure there is a real answer: it would mean the data cannot separate
# attending from winning and the constants must come from somewhere else.
#
# Data build is copied verbatim from epv3_ruck_to_advantage.R so the two are
# comparable coefficient for coefficient.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_ruck_three_way.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 45) for (l in capture.output(print(utils::head(x, n)))) say(l)
tid <- function(f) { s <- summary(f); data.table(term = rownames(s$coefficients),
  coef = round(s$coefficients[, 1], 4), t = round(s$coefficients[, 3], 2)) }

say("=== Attending, winning, winning to advantage: three coefficients ===")
say("run at ", format(Sys.time()))

ps <- as.data.table(load_player_stats(TRUE))
agg_cols <- intersect(c("hitouts", "hitouts_to_advantage", "ruck_contests",
                        "centre_clearances", "stoppage_clearances", "clearances",
                        "disposals"), names(ps))
ps[, team := data.table::fifelse(team_status == "home", home_team_name, away_team_name)]
tm <- ps[!is.na(team), lapply(.SD, function(v) sum(v, na.rm = TRUE)), .SDcols = agg_cols,
         by = .(match_id = as.character(match_id), team)]

ch <- as.data.table(load_chains(TRUE)); pbp <- as.data.table(load_pbp(TRUE))
setorder(ch, match_id, display_order)
narrow <- data.table(match_id = ch$match_id, display_order = ch$display_order,
                     description = ch$description, team_id = ch$team_id)
rm(ch); invisible(gc())
narrow[, `:=`(nxt_d = shift(description, 1, type = "lead"),
              nxt_t = shift(team_id, 1, type = "lead")), by = match_id]
stp <- narrow[description %chin% c("Centre Bounce", "Ball Up Call") & !is.na(nxt_t)]
stp <- merge(stp, pbp[, .(match_id, display_order = display_order + 1L, nxt_exp = exp_pts)],
             by = c("match_id", "display_order"), all.x = TRUE)
stp <- stp[is.finite(nxt_exp)]
tid_map <- unique(ps[!is.na(team), .(team_id = as.character(team_id), team)])
sv <- stp[, .(stop_epv = sum(nxt_exp)), by = .(match_id, team_id = as.character(nxt_t))]
sv <- merge(sv, tid_map, by = "team_id")[, .(match_id, team, stop_epv)]
tm <- merge(tm, sv, by = c("match_id", "team"), all.x = TRUE)
tm[is.na(stop_epv), stop_epv := 0]

res <- as.data.table(load_results(TRUE))
tg <- res[, .(match_id = as.character(match_id), home = home_team_name,
              away = away_team_name, margin = home_score - away_score)][is.finite(margin)]
h <- merge(tg, tm, by.x = c("match_id", "home"), by.y = c("match_id", "team"))
a <- merge(tg, tm, by.x = c("match_id", "away"), by.y = c("match_id", "team"))
d <- merge(h, a, by = "match_id", suffixes = c("_h", "_a"))
for (cc in c(agg_cols, "stop_epv")) d[, (paste0("d_", cc)) := get(paste0(cc, "_h")) - get(paste0(cc, "_a"))]
if (!"margin" %in% names(d) && "margin_h" %in% names(d)) d[, margin := margin_h]
d <- d[is.finite(d_hitouts) & is.finite(d_ruck_contests)]
say("matches ", nrow(d))

say(""); say("########## 0. IS THE THREE-WAY SPLIT IDENTIFIED? ##########")
say("rc >= h >= hta by construction, so these are collinear. Large VIFs mean the")
say("data cannot separate attending from winning and the numbers below are noise.")
X <- d[, .(d_ruck_contests, d_hitouts, d_hitouts_to_advantage)]
say_dt(data.table(pair = c("rc~h", "rc~hta", "h~hta"),
                  cor = round(c(cor(X$d_ruck_contests, X$d_hitouts),
                                cor(X$d_ruck_contests, X$d_hitouts_to_advantage),
                                cor(X$d_hitouts, X$d_hitouts_to_advantage)), 3)))
vif <- sapply(names(X), function(v) {
  f <- stats::lm(stats::as.formula(paste(v, "~ .")), data = X)
  1 / (1 - summary(f)$r.squared)
})
say_dt(data.table(term = names(vif), VIF = round(vif, 2)))
say(sprintf("  worst VIF %.2f -- %s", max(vif),
            if (max(vif) < 10) "identified (conventional cut-off 10)" else
              "NOT IDENTIFIED, treat everything below as unreliable"))

say(""); say("########## 1. THE THREE-WAY FIT ##########")
f3 <- stats::lm(d_stop_epv ~ d_ruck_contests + d_hitouts + d_hitouts_to_advantage, data = d)
say_dt(tid(f3))
say("")
say("For comparison, the two-way fit that produced the constants I nearly wrote:")
say_dt(tid(stats::lm(d_stop_epv ~ d_hitouts + d_hitouts_to_advantage, data = d)))

say(""); say("########## 2. WITH A TEAM-STRENGTH CONTROL ##########")
f3c <- stats::lm(d_stop_epv ~ d_ruck_contests + d_hitouts + d_hitouts_to_advantage + d_disposals, data = d)
say_dt(tid(f3c))

say(""); say("########## 3. DOES IT HOLD OUT OF SAMPLE? ##########")
say("A three-way split of collinear regressors is exactly where an in-sample fit")
say("flatters itself. If the halves disagree in SIGN, the structure is not real.")
cut <- floor(nrow(d) / 2)
c1 <- coef(stats::lm(d_stop_epv ~ d_ruck_contests + d_hitouts + d_hitouts_to_advantage, data = d[1:cut]))
c2 <- coef(stats::lm(d_stop_epv ~ d_ruck_contests + d_hitouts + d_hitouts_to_advantage, data = d[(cut + 1):nrow(d)]))
say_dt(data.table(term = names(c1), first_half = round(c1, 4), second_half = round(c2, 4),
                  same_sign = sign(c1) == sign(c2)))

say(""); say("########## 4. VERDICT AGAINST PETE'S PREDICTION ##########")
say("Predicted: attendance NEGATIVE, won tap SLIGHTLY POSITIVE, to advantage LARGELY POSITIVE.")
cf <- coef(f3c)
per <- function(x) x / 2   # per-ruck = half the differential
rows <- data.table(
  term      = c("attendance (ruck_contests)", "won tap (hitouts)", "to advantage (hta)"),
  paid_now  = c(EPV_RUCK_CONTEST_WT, EPV_HITOUT_WT, EPV_HITOUT_ADV_WT),
  measured  = round(per(c(cf[["d_ruck_contests"]], cf[["d_hitouts"]], cf[["d_hitouts_to_advantage"]])), 4),
  predicted = c("negative", "slightly +", "largely +"))
rows[, matches_prediction := c(measured[1] < 0, measured[2] > 0, measured[3] > 0)]
say_dt(rows)
ok <- all(rows$matches_prediction)
say("")
if (ok && max(vif) < 10 && all(sign(c1) == sign(c2))) {
  say("  ALL THREE MATCH, identified, and stable out of sample.")
  say("  Pete's structure is right and these are the constants to ship.")
} else if (!ok) {
  say("  DOES NOT match the prediction on every term. Do not force it -- read")
  say("  the signs above and decide from them, not from what was expected.")
} else {
  say("  Signs match but identification or stability failed. Not shippable as is.")
}
say(""); say("The v2 formula is  hitouts*W_H + hta*W_ADV + ruck_contests*W_C,  so these")
say("three map one-to-one onto EPV_HITOUT_WT, EPV_HITOUT_ADV_WT and")
say("EPV_RUCK_CONTEST_WT. No further arithmetic is needed to use them.")

saveRDS(list(three_way = coef(f3), controlled = cf, vif = vif,
             oos = list(c1, c2), rows = rows),
        file.path(OUT_DIR, "epv3_ruck_three_way.rds"))
say(""); say("done ", format(Sys.time())); close(con); cat("\nDone\n")
