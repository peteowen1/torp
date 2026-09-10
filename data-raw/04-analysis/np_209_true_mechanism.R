# The REAL mechanism behind #209, after two wrong ones (#209, #210)
# =============================================================================
# Third attempt, and the first two are worth stating because the errors were
# mine and the same shape twice.
#
#   WRONG #1  "the defence is paid (1 - p_hat) * surprise, so p_hat = 1 pays
#             whoever won the ball nothing." That term is recv_hm on the
#             RETAINED branch -- the receiving teammate -- and the line is dead
#             anyway (every retained row is overwritten at :1093/:1102/:1125).
#             Caught by a code reviewer.
#
#   WRONG #2  "so #209's premise is wrong, the winner IS paid." That came from
#             this script's own earlier verdict, which tested
#             sum(|contest_winner payments|) > 1e-6 and passed on a total of
#             0.005 POINTS ACROSS 17 ACTS. A gate blind to the defect it was
#             written to detect.
#
# The symptom in #209 is REAL: the contest winner is paid ~0.0003 points an act
# on the saturated rows. What was never right was the mechanism. Reading the
# code, it should be this:
#
#   the contest model's  V_pre = (1 - p) * V_att_hat + p * V_def_hat
#   the ledger's         c_cont = V_branch - V_pre
#   with                 V_branch = V_def_hat  when def_win is TRUE
#
# so at p = 1 and def_win TRUE, V_pre IS V_def_hat and c_cont collapses to
# EXACTLY ZERO. Then cede_c_hm = (1 - beta) * 0 = 0, and the winner's share of
# nothing is nothing. Certainty does not divert the payment; it destroys the
# quantity being divided.
#
# That predicts something specific and falsifiable, which is the point of
# writing it down before measuring: cont_surprise on the saturated rows should
# be ~0 while ground_surprise stays ordinary. If cont_surprise is NOT ~0, this
# third explanation is wrong too and the thing to do is stop theorising and
# print the rows.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_209_true_mechanism.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))
tm  <- as.data.table(np_difficulty_terms_for_season(SEASON, pbp_data = pbp, chains = ch))

con <- tm[contested == TRUE]
hot <- tm[p_hat >= 0.99]

say("\n=== the prediction: cont_surprise ~ 0 on saturated rows ===")
say("cont_surprise = V_branch - V_pre. At p_hat = 1 with def_win TRUE these are")
say("the same number, so the contest surprise should vanish while the ground")
say("surprise carries on as normal.")
print(rbind(
  hot[, .(group = "p_hat >= 0.99", n = .N,
          mean_abs_cont = round(mean(abs(cont_surprise), na.rm = TRUE), 5),
          max_abs_cont  = round(max(abs(cont_surprise), na.rm = TRUE), 5),
          mean_abs_ground = round(mean(abs(ground_surprise), na.rm = TRUE), 3))],
  con[p_hat < 0.99, .(group = "other contests", n = .N,
          mean_abs_cont = round(mean(abs(cont_surprise), na.rm = TRUE), 5),
          max_abs_cont  = round(max(abs(cont_surprise), na.rm = TRUE), 5),
          mean_abs_ground = round(mean(abs(ground_surprise), na.rm = TRUE), 3))]))

say("\n=== is it a knife edge, or a smooth collapse? ===")
say("This decides whether #209 is 18 rows or a gradient the 18 merely sit at the")
say("end of. mean |cont_surprise| by how certain the contest model was:")
con[, pband := cut(p_hat, c(-Inf, .5, .8, .9, .95, .99, .999, Inf),
                   labels = c("<.5", ".5-.8", ".8-.9", ".9-.95", ".95-.99",
                              ".99-.999", ">=.999"))]
print(con[, .(n = .N,
              def_win_pct = round(100 * mean(def_win), 1),
              mean_abs_cont = round(mean(abs(cont_surprise), na.rm = TRUE), 4),
              mean_abs_ground = round(mean(abs(ground_surprise), na.rm = TRUE), 3)),
          by = pband][order(pband)])

say("\n=== and the mirror case, which the theory also predicts ===")
say("At p_hat ~ 0 the ATTACK is certain to win, so an attacking win should have")
say("cont_surprise ~ 0 too -- the same collapse, other end. If this holds, the")
say("issue is not about defenders at all; it is about certainty in either")
say("direction paying the winner nothing.")
print(con[, .(n = .N,
              mean_abs_cont = round(mean(abs(cont_surprise), na.rm = TRUE), 4)),
          by = .(certain = data.table::fcase(
                   p_hat >= 0.99, "def certain (p>=.99)",
                   p_hat <= 0.01, "att certain (p<=.01)",
                   default = "uncertain"),
                 def_win)][order(certain, def_win)])

say("\n=== VERDICT ===")
mc <- hot[, mean(abs(cont_surprise), na.rm = TRUE)]
mo <- con[p_hat < 0.99, mean(abs(cont_surprise), na.rm = TRUE)]
say("  mean |cont_surprise|, saturated : ", signif(mc, 3))
say("  mean |cont_surprise|, other     : ", signif(mo, 3))
say("  ratio                           : ", signif(mc / mo, 3))
if (is.finite(mc) && mc < 0.01 * mo) {
  say("\n  CONFIRMED. The contest surprise collapses to ~0 exactly where the")
  say("  model is certain, so there is nothing left to pay the winner. #209 is")
  say("  real, and the mechanism is the SURPRISE VANISHING, not a share being")
  say("  set to zero. A clamp on p_hat fixes it only if applied BEFORE")
  say("  cont_surprise is computed -- clamping it in the terms table, as tested")
  say("  earlier, changes nothing at all.")
} else {
  say("\n  NOT CONFIRMED. Third explanation also wrong. Stop theorising and")
  say("  print the 18 rows with V_pre, V_att_hat, V_def_hat side by side.")
}
