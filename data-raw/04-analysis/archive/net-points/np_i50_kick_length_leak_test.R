# Can Kick Inside 50 Result give a LEAK-FREE kick length? (#210)
# =============================================================================
# Pete: "does this mean we can use it in the kick model without leakage! (and in
# the contest model) ... does it give us x and y of the target as well? if so we
# get kick distance info as well!"
#
# The geometry says yes. In the traces:
#   1987 Kick                   WB  Jedd Busslinger   2  18   <- origin
#   1988 Kick Into F50          WB  Jedd Busslinger   2  18   <- same as origin
#   1989 Kick Inside 50 Result  WB  Louis Emmett     46  -7   <- destination
# so Kick Into F50 marks the kick and Kick Inside 50 Result marks the arrival,
# and the distance between them is a kick length. Both rows precede the outcome.
#
# BUT THIS IS THE EXACT REASONING THAT PRODUCED #210. There I argued out_x/out_y
# was a legitimate landing point "chosen by the kicker" and it was contaminated:
# kick_len ran to 115m on the confident rows because after a turnover the next
# recorded event is intrinsically distant. Preceding in display_order does not
# prove a field was not written retrospectively.
#
# So the same tests that caught #210, verdict rules written first:
#
#  (1) PHYSICAL PLAUSIBILITY -- the test #210 failed. A real kick is at most
#      ~60m. COUNTS, not rounded percentages (the 0.0% / 143.3m lesson).
#        LEAK if a material share exceeds ~70m.
#  (2) OUTCOME INDEPENDENCE -- for kicks from the same origin cell, does the
#      recorded arrival differ by whether the kick was RETAINED or TURNED OVER?
#      If the arrival encodes the outcome, this gap is large.
#        LEAK if median implied length differs sharply by outcome within cells.
#  (3) SATURATION -- does a model using it produce p_hat at 1? That is the
#      symptom that started #210.
#  (4) VALUE -- out-of-fold log loss against the clean baseline, on the covered
#      subset only. Coverage matters as much as lift: this row exists only for
#      kicks inside 50, so it can help a minority of disposals at best.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_i50_kick_length_leak_test.R"'
suppressMessages({library(data.table); library(mgcv); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
set.seed(20260911)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
ch  <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]
pbp <- as.data.table(load_pbp(SEASON));    pbp[, match_id := as.character(match_id)]
setorder(ch, match_id, display_order)
for (s in c("description", "player_id", "team_id", "x", "y")) {
  ch[, (paste0("n1_", s)) := shift(get(s), 1, type = "lead"), by = match_id]
  ch[, (paste0("n2_", s)) := shift(get(s), 2, type = "lead"), by = match_id]
}

# A kick, then Kick Into F50 (origin), then Kick Inside 50 Result (arrival).
k <- ch[description == "Kick" & n1_description == "Kick Into F50" &
          n2_description == "Kick Inside 50 Result"]
say("kicks with the full Into-F50 / I50-Result pair: ", format(nrow(k), big.mark = ","))
say("all kicks in chains: ", format(ch[description == "Kick", .N], big.mark = ","),
    "   coverage ", round(100 * nrow(k) / ch[description == "Kick", .N], 1), "%")

k[, `:=`(i50_len = sqrt((n2_x - x)^2 + (n2_y - y)^2),
         i50_fwd = n2_x - x)]
k <- k[is.finite(i50_len)]
say("with finite geometry: ", format(nrow(k), big.mark = ","))

say("\n=== (1) PHYSICAL PLAUSIBILITY -- counts, not percentages ===")
say("A real kick is at most ~60m. #210's leaky kick_len hit 115m medians.")
for (thr in c(50, 60, 70, 80, 100)) {
  n <- sum(k$i50_len > thr)
  say("  implied length > ", formatC(thr, width = 3), "m : ", formatC(n, width = 6),
      "   (", format(round(100 * n / nrow(k), 3), nsmall = 3), "%)")
}
say("  median ", round(median(k$i50_len), 1), "m   p95 ",
    round(quantile(k$i50_len, .95), 1), "m   max ", round(max(k$i50_len), 1), "m")

# the outcome: who has the ball after the I50 result resolves
ch[, rn := .I]
k[, out_desc := n2_description]
res <- ch[, .(match_id, display_order, r_desc = description, r_team = team_id)]
k[, res_do := display_order + 3L]
k <- merge(k, res, by.x = c("match_id", "res_do"), by.y = c("match_id", "display_order"),
           all.x = TRUE)
k <- k[!is.na(r_team)]
k[, turnover := as.integer(r_team != team_id)]
say("\nresolvable kicks: ", format(nrow(k), big.mark = ","),
    "   turnover rate ", round(100 * mean(k$turnover), 1), "%")

say("\n=== (2) OUTCOME INDEPENDENCE: same origin, different outcome ===")
say("If the recorded arrival encodes the outcome, implied length differs sharply")
say("between retained and turned-over kicks from the same part of the ground.")
k[, ox := cut(x, seq(-100, 100, by = 20))]
k[, oy := cut(y, seq(-80, 80, by = 40))]
g <- k[, .(n = .N, med = median(i50_len)), by = .(ox, oy, turnover)]
w <- dcast(g, ox + oy ~ turnover, value.var = c("n", "med"))
setnames(w, c("n_0", "n_1", "med_0", "med_1"), c("n_ret", "n_tov", "med_ret", "med_tov"),
         skip_absent = TRUE)
w <- w[!is.na(n_ret) & !is.na(n_tov) & n_ret >= 30 & n_tov >= 30]
w[, diff_m := med_tov - med_ret]
say("  origin cells with 30+ of each: ", nrow(w))
if (nrow(w)) {
  say("  implied length difference (turnover - retained):")
  say("    mean ", round(mean(w$diff_m), 2), "m   median ", round(median(w$diff_m), 2),
      "m   range ", round(min(w$diff_m), 1), " to ", round(max(w$diff_m), 1), "m")
  say("  (#210's leaky feature differed by tens of metres; a real kick length")
  say("   should differ only slightly -- longer kicks are genuinely harder)")
}

say("\n=== (3)+(4) does it saturate, and what is it worth? ===")
k[, `:=`(abs_y = abs(y), y_ = turnover)]
half <- as.numeric(quantile(abs(ch$x), 0.995, na.rm = TRUE))
k[, goal_dist := sqrt(pmax(0, half - x)^2 + abs_y^2)]
k <- merge(k, pbp[, .(match_id, display_order, exp_pts)],
           by = c("match_id", "display_order"), all.x = TRUE)
k <- k[is.finite(exp_pts)]
say("with exp_pts: ", format(nrow(k), big.mark = ","))
mids <- unique(k$match_id)
k <- merge(k, data.table(match_id = mids, fold = sample(rep_len(1:5, length(mids)))),
           by = "match_id")
FORMS <- list(
  CLEAN = y_ ~ s(x, abs_y) + s(goal_dist) + s(exp_pts),
  PLUS_I50 = y_ ~ s(x, abs_y) + s(goal_dist) + s(exp_pts) + s(i50_len) + s(i50_fwd))
ll <- function(y, p) { p <- pmin(pmax(p, 1e-15), 1 - 1e-15)
  -mean(y * log(p) + (1 - y) * log(1 - p)) }
R <- rbindlist(lapply(1:5, function(f) {
  o <- data.table(y = k[fold == f, y_])
  for (nm in names(FORMS)) {
    m <- mgcv::bam(FORMS[[nm]], data = droplevels(k[fold != f]),
                   family = stats::binomial(), discrete = TRUE)
    o[, (nm) := as.numeric(stats::predict(m, newdata = k[fold == f], type = "response"))]
  }
  o
}))
b <- ll(R$y, rep(mean(R$y), nrow(R)))
say("\nlogloss, lower is better. intercept only ", round(b, 5))
for (nm in names(FORMS)) {
  p <- R[[nm]]
  say("  ", formatC(nm, width = -9), round(ll(R$y, p), 5),
      "   rows p>=0.99: ", formatC(sum(p >= 0.99), width = 5),
      "   max p ", round(max(p), 5))
}
say("\n  gain from adding the I50 geometry: ",
    round(ll(R$y, R$CLEAN) - ll(R$y, R$PLUS_I50), 5), " log loss")

say("\n=== VERDICT ===")
over70 <- sum(k$i50_len > 70)
say("  implied lengths over 70m: ", over70, " of ", format(nrow(k), big.mark = ","))
if (over70 / nrow(k) < 0.01 && max(R$PLUS_I50) < 0.99) {
  say("  LOOKS CLEAN. Physically plausible, no saturation. Pete is right that this")
  say("  is a leak-free route to kick distance -- but note the COVERAGE ceiling:")
  say("  it exists only for kicks inside 50.")
} else {
  say("  NOT CLEAN on these tests. Do not wire it into the kick model.")
}
