# Is the ruck channel worth 7.4 points per unit, or is it proxying team quality?
#
# THE STAKE. The stoppage channel's fitted points constant is the single biggest
# lever in v3. At 7.38 the contest category carries 13% of the rating and Sean
# Darcy is the 3rd best player in the competition on ruck taps alone; at the
# ledger route's 3.14 it carries about 6% and he is not.
#
# TWO ROUTES DISAGREE BY MORE THAN 2x AND BOTH ARE ON FILE:
#   7.38  this session's EPR-layer regression (t 4.56); ws13 got 7.22 the same way
#   3.14  audit_swing_allocation.R: the measured swing per ruck contest (0.3925)
#         against what the three box weights actually pay for it (0.1249)
#
# The regression is the one that can be wrong for a reason the accounting cannot:
# a team with a dominant ruck is not a random team. Hitout share correlates with
# clearance and midfield quality, so `epr_hitout` can be paid for team strength
# it merely travels with. The accounting route has no such channel.
#
# THE TEST. Put team strength in the regression explicitly and see what survives.
# If most of the 7.38 is the ruck, the coefficient barely moves. If a large part
# of it is team quality, it collapses -- and the honest constant is nearer the
# ledger's 3.14.
#
# Controls, weakest to strongest:
#   1  the other three EPR channels          (already in the base fit)
#   2  + PSR difference                      independent box-score team strength
#   3  + Elo difference                      pure team strength, no player data
#   4  Elo difference ALONE as the reference: how much does the ruck channel add?
#
# Elo is the sharpest control because it is built only from results -- it cannot
# contain ruck information except through the wins ruck dominance produced, which
# is precisely the confound.
#
# Reads cached ratings. ~2 min, no rebuild.

suppressMessages({
  library(dplyr); library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_ruck_multiplier_check.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

teams <- load_teams(TRUE)
shared_fixtures <- load_fixtures(TRUE)
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)),
                   error = function(e) NULL)
res <- as.data.table(load_results(TRUE)); xg <- as.data.table(load_xg(TRUE))
tgt <- merge(res[, .(match_id = as.character(match_id), margin = home_score - away_score)],
             xg[, .(match_id = as.character(match_id), xmargin = xscore_diff)],
             by = "match_id")[is.finite(margin) & is.finite(xmargin)]

rt <- as.data.table(read_parquet(file.path(OUT_DIR, "epv3_cal_rt_4ch.parquet")))
SUB <- c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout")

tr <- as.data.table(.build_team_ratings_df(teams, as.data.frame(rt), psr_df))
keep <- c("match_id", SUB, intersect(c("psr", "elo", "xelo"), names(tr)))
h <- tr[team_type == "home", ..keep]; a <- tr[team_type == "away", ..keep]
m <- merge(h, a, by = "match_id", suffixes = c("_h", "_a"))
for (v in setdiff(keep, "match_id")) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
m <- merge(m, tgt, by = "match_id")
say("=== Is the 7.38 ruck multiplier the ruck, or the team? ===")
say("run at ", format(Sys.time()))
say("matches ", nrow(m), " | controls available: ",
    paste(intersect(c("psr", "elo", "xelo"), names(tr)), collapse = ", "))

show <- function(f, label) {
  co <- summary(f)$coefficients
  r <- data.table(term = rownames(co), coef = round(co[, 1], 4), t = round(co[, 3], 2))
  say(""); say("--- ", label, "   (R2 ", round(summary(f)$r.squared, 4), ") ---")
  say_dt(r, 10)
  r[term == "d_epr_hitout", coef]
}

base_f <- lm(as.formula(paste("xmargin ~ 0 +", paste0("d_", SUB, collapse = " + "))), data = m)
k0 <- show(base_f, "1. four EPR channels only")

k1 <- NA_real_
if ("d_psr" %in% names(m)) {
  k1 <- show(lm(as.formula(paste("xmargin ~ 0 +", paste0("d_", SUB, collapse = " + "), "+ d_psr")),
                data = m), "2. + PSR difference (box-score team strength)")
}
k2 <- NA_real_
elo_col <- intersect(c("d_elo", "d_xelo"), names(m))
if (length(elo_col) > 0) {
  k2 <- show(lm(as.formula(paste("xmargin ~ 0 +", paste0("d_", SUB, collapse = " + "),
                                 "+", paste(elo_col, collapse = " + "))), data = m),
             "3. + Elo difference (pure team strength)")
}

say("")
say("=== WHAT SURVIVES ===")
say_dt(data.table(
  control = c("EPR channels only", "+ PSR", "+ Elo"),
  ruck_coef = round(c(k0, k1, k2), 4),
  pct_of_uncontrolled = round(100 * c(k0, k1, k2) / k0, 1)), 5)
say("")
say("The ledger route's independent answer is 3.14x the current box weights,")
say("which in these units is the same question asked without a regression at")
say("all: measured swing per ruck contest 0.3925 against 0.1249 actually paid.")

say("")
say("=== how much of the ruck channel IS team strength? ===")
if (length(elo_col) > 0) {
  for (v in c(paste0("d_", SUB), elo_col)) {
    if (v %in% names(m)) say(sprintf("  cor(%-14s, %s) %+.3f", v, elo_col[1],
                                     cor(m[[v]], m[[elo_col[1]]], use = "complete.obs")))
  }
}
if ("d_psr" %in% names(m)) {
  say("")
  for (v in paste0("d_", SUB)) {
    say(sprintf("  cor(%-14s, d_psr) %+.3f", v, cor(m[[v]], m$d_psr, use = "complete.obs")))
  }
}

say("")
say("=== the same question on ACTUAL margin, not xmargin ===")
bm <- lm(as.formula(paste("margin ~ 0 +", paste0("d_", SUB, collapse = " + "))), data = m)
say_dt({co <- summary(bm)$coefficients
        data.table(term = rownames(co), coef = round(co[, 1], 4), t = round(co[, 3], 2))}, 6)

say("")
say("=== does the ruck channel survive OUT of sample? ===")
say("Fitted on the earlier half of the matches, scored on the later half. An")
say("in-sample coefficient that does not reproduce out of sample is a fit, not")
say("a constant -- the trap that overturned five conclusions in one session.")
setorder(m, match_id)
if ("season" %in% names(m)) setorder(m, season, match_id)
cut <- floor(nrow(m) / 2)
f_tr <- lm(as.formula(paste("xmargin ~ 0 +", paste0("d_", SUB, collapse = " + "))), data = m[1:cut])
f_te <- lm(as.formula(paste("xmargin ~ 0 +", paste0("d_", SUB, collapse = " + "))), data = m[(cut + 1):nrow(m)])
say_dt(data.table(term = SUB,
                  first_half = round(coef(f_tr), 4),
                  second_half = round(coef(f_te), 4),
                  full = round(coef(base_f), 4)), 6)

close(con)
cat("\nDone\n")
