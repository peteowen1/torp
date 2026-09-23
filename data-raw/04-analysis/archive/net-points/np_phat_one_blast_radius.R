# How big is the p_hat == 1 degenerate class?
# =============================================================================
# Found while tracing a contest-kick example: on Mitch Lewis's shot at goal the
# difficulty model returns p_hat = 1.000, so surprise = 0 and the defence's
# share -- (1 - p) * surprise -- is exactly 0. Nick Vlastuin took the ball and
# was paid 0.000 for it; the whole swing landed on the kicker as `decision`.
#
# That may be correct (a shot at goal genuinely does end the chain, and the
# shooter is genuinely responsible for a miss) or it may be the neighbour of the
# already-fixed "behinds were classed as turnovers" defect. Either way the
# question is the same one that decides whether it matters: HOW MUCH VALUE
# lands this way, and on whom.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_phat_one_blast_radius.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]
tm  <- fread("data-raw/outputs/np_difficulty_terms_2025_2026.csv")
tm[, match_id := as.character(match_id)]
tm  <- tm[substr(match_id, 5, 8) == as.character(SEASON)]

say("=== distribution of p_hat over ", format(nrow(tm), big.mark = ","), " scored disposals ===")
print(tm[, .(min = round(min(p_hat), 4), p25 = round(quantile(p_hat, .25), 3),
             median = round(median(p_hat), 3), p75 = round(quantile(p_hat, .75), 3),
             max = round(max(p_hat), 4))])

for (thr in c(1, 0.999, 0.99, 0.95)) {
  n <- tm[p_hat >= thr, .N]
  say("p_hat >= ", thr, ": ", format(n, big.mark = ","), " rows (",
      round(100 * n / nrow(tm), 2), "%),  |surprise| total ",
      round(tm[p_hat >= thr, sum(abs(surprise))], 1),
      ",  |decision| total ", round(tm[p_hat >= thr, sum(abs(decision))], 1))
}

# What ARE these rows? Join the chains description of what the disposal became.
hot <- tm[p_hat >= 0.999]
nxt <- ch[, .(match_id, display_order, description)]
lab <- merge(hot[, .(match_id, display_order, decision, surprise)],
             nxt, by = c("match_id", "display_order"), all.x = TRUE)
# what the NEXT chains row says happened
for (k in 1:3) {
  f <- ch[, .(match_id, display_order = display_order - k, nd = description)]
  lab <- merge(lab, f, by = c("match_id", "display_order"), all.x = TRUE)
  setnames(lab, "nd", paste0("next", k))
}
say("\n=== what the p_hat >= 0.999 rows actually are (this row's description) ===")
print(head(lab[, .N, by = description][order(-N)], 8))
say("\n=== and what happened next (first following chains row) ===")
print(head(lab[, .N, by = next1][order(-N)], 8))

say("\n=== how much does the defence lose by getting (1-p)*surprise = 0? ===")
say("On these rows the defence's credit is zero BY CONSTRUCTION, whatever")
say("happened. Total |decision| charged to kickers on them: ",
    round(lab[, sum(abs(decision))], 1), " points over ",
    format(nrow(lab), big.mark = ","), " rows.")
say("For scale, the whole 2026 ledger's receiver value is ~20,945 points.")
