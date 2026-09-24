# Three-outcome contest model: is the OFF switch inert, and what does ON do?
# =============================================================================
# Two questions, in this order, and the first one gates the second.
#
# 1. WITH THE CONSTANTS AT THEIR DEFAULTS, does the new code reproduce v11
#    bit-for-bit? The three-way model is behind EPV3_CONTEST_OUTCOMES and
#    EPV3_CONTEST_EXCLUDE_SHOTS, both defaulting to the shipped behaviour, so
#    anything that moves is a bug I introduced in the refactor rather than the
#    design. `identical()`, not `all.equal()` -- a tolerance would hide exactly
#    the small drift a refactor causes.
#
# 2. WITH IT ON, what happens to the things this was built to fix: the
#    defensive win rate, the mispricing of an intercept mark, and the
#    forward-defender gap.
#
# This is a design measurement on 2026, fitted in-sample. It is NOT the ship
# gate -- that is a full leak-safe rebuild over all history, the same one every
# vintage gets.
#
#   powershell.exe -Command 'Rscript "data-raw/05-validation/check_three_way_contest.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))
say("pbp ", format(nrow(pbp), big.mark = ","), " | chains ",
    format(nrow(ch), big.mark = ","),
    " | EPV3_CONTEST_OUTCOMES ", EPV3_CONTEST_OUTCOMES,
    " | EPV3_CONTEST_EXCLUDE_SHOTS ", EPV3_CONTEST_EXCLUDE_SHOTS)

# --- 1. the OFF switch must be inert -----------------------------------------
say("\n=== 1. is the new code inert with the constants at their defaults? ===")
if (!identical(EPV3_CONTEST_OUTCOMES, "two") ||
    !identical(EPV3_CONTEST_EXCLUDE_SHOTS, FALSE)) {
  say("  SKIPPED -- the constants are not at their shipped defaults, so this")
  say("  run cannot answer the inertness question. Re-run on a clean checkout.")
} else {
  cst <- as.data.table(build_aerial_contests(ch, pbp))
  say("  contests built: ", format(nrow(cst), big.mark = ","))
  set.seed(1)
  csc <- as.data.table(score_contests(cst, fit_contest_models(cst)))
  # The reference: the numbers this file recorded when the two-way path was the
  # only path (measured 2026-09-12 on v11, before the three-way code existed).
  REF <- list(n_contests = 17571L, n_scored = 16081L, def_win_pct = 78.7)
  got <- list(n_contests = nrow(cst),
              n_scored = nrow(csc[def_win == TRUE | out_desc %chin% EPV3_DUEL_OUT]),
              def_win_pct = round(100 * mean(cst$def_win), 1))
  say("  contests   expected ", REF$n_contests, "   got ", got$n_contests,
      if (identical(REF$n_contests, got$n_contests)) "   OK" else "   *** MOVED ***")
  say("  after gate2 expected ", REF$n_scored, "   got ", got$n_scored,
      if (identical(REF$n_scored, got$n_scored)) "   OK" else "   *** MOVED ***")
  say("  def_win %  expected ", REF$def_win_pct, "   got ", got$def_win_pct,
      if (isTRUE(all.equal(REF$def_win_pct, got$def_win_pct))) "   OK" else "   *** MOVED ***")
  say("  Population unchanged is necessary but not sufficient -- the fitted")
  say("  values matter too, and those are checked by the full rebuild's psr")
  say("  control, not here.")
}

# --- 2. what turning it on does ----------------------------------------------
# Flip the constants in this session only. They are package-level bindings, so
# this needs assignInNamespace rather than a plain assignment, and it is exactly
# the kind of thing that must never appear outside a measurement script.
say("\n=== 2. turning the three-way model ON ===")
ns <- asNamespace("torp")
old <- list(o = get("EPV3_CONTEST_OUTCOMES", ns), s = get("EPV3_CONTEST_EXCLUDE_SHOTS", ns))
on.exit({
  utils::assignInNamespace("EPV3_CONTEST_OUTCOMES", old$o, ns = "torp")
  utils::assignInNamespace("EPV3_CONTEST_EXCLUDE_SHOTS", old$s, ns = "torp")
}, add = TRUE)
utils::assignInNamespace("EPV3_CONTEST_OUTCOMES", "three", ns = "torp")
utils::assignInNamespace("EPV3_CONTEST_EXCLUDE_SHOTS", TRUE, ns = "torp")

cst3 <- as.data.table(build_aerial_contests(ch, pbp))
say("contests built: ", format(nrow(cst3), big.mark = ","))
say("\n--- the three branches ---")
say("n is contests; pct is share of the population. This replaces the single")
say("86% figure the two-way model produced.")
b <- cst3[, .(n = .N), by = out3][order(-n)]
b[, pct := round(100 * n / sum(n), 1)]
print(b)

m3 <- fit_contest_models(cst3)
csc3 <- as.data.table(score_contests(cst3, m3))

say("\n--- does the model now SEE the difference? ---")
say("mean fitted probability of each branch, by what actually happened. Under")
say("the two-way model these were 0.787 / 0.786 / 0.791 -- identical, i.e.")
say("blind. Each row should now peak on its own branch.")
print(csc3[, .(n = .N,
               p_att = round(mean(p_att_hat), 3),
               p_def = round(mean(p_def_hat), 3),
               p_other = round(mean(p_other_hat), 3)), by = out3][order(-n)])

say("\n--- is an intercept mark priced properly now? ---")
say("mean_V_actual is what the outcome is really worth (attacking frame,")
say("negative is good for the defence). mean_V_branch is what the model prices")
say("that branch at. Under the two-way model mark_def read +0.270 against a")
say("true -0.722, about one point understated.")
print(csc3[, .(n = .N,
               mean_V_actual = round(mean(V_after, na.rm = TRUE), 3),
               mean_V_branch = round(mean(V_branch_hat, na.rm = TRUE), 3),
               mean_V_pre    = round(mean(V_pre, na.rm = TRUE), 3)),
           by = out3][order(-n)])

say("\n--- what the winner is paid, by branch ---")
say("The contest payment is |V_branch - V_pre| in expected points. A branch")
say("that is the expected outcome should pay little; a rare one should pay a")
say("lot. That is the design, stated so it can be checked rather than assumed.")
csc3[, paid := abs(V_branch_hat - V_pre)]
print(csc3[, .(n = .N, mean_paid = round(mean(paid), 3),
               total_paid = round(sum(paid), 0)), by = out3][order(-n)])
say("\nUnder the two-way model the defence was paid 0.311 a contest against the")
say("attack's 1.265, over 13,827 defensive wins and 2,254 attacking ones.")

say("\n--- the same thing by who is actually PAID, not by branch ---")
say("def_win is the routing variable: TRUE means the payment goes to the")
say("defending side. This is the row comparable to the 0.311 vs 1.265 above.")
print(csc3[, .(n = .N, mean_paid = round(mean(paid), 3),
               total_paid = round(sum(paid), 0)),
           by = .(paid_to = data.table::fifelse(def_win, "defence", "attack"))])

say("\n--- how much signal does each probability model actually have? ---")
say("AUC of each branch model against what happened. 0.5 is a coin flip, so a")
say("value near 0.5 means the features cannot see that outcome coming and the")
say("branch is being priced by its base rate rather than by the situation.")
auc <- function(score, lab) {
  lab <- as.logical(lab)
  if (length(unique(lab)) < 2) return(NA_real_)
  r <- rank(score)
  n1 <- sum(lab); n0 <- sum(!lab)
  (sum(r[lab]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
print(data.table(
  model = c("P(nobody marked it)", "P(attack marked it | somebody did)"),
  auc = c(round(auc(csc3$p_other_hat, csc3$out3 == "other"), 3),
          round(auc(csc3[out3 != "other"]$p_att_hat,
                    csc3[out3 != "other"]$out3 == "mark_att"), 3))))
