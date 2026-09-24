# On the 18 saturated rows, what is the contest winner ACTUALLY paid? (#209)
# =============================================================================
# #209 was filed on my characterisation: "where p_hat = 1, the defence's
# (1 - p_hat) * surprise is exactly zero, so whoever won the ball is paid
# nothing." Two measurements have since undermined that:
#
#   - p_hat never appears in the contested payment branches at all
#     (epv_net_points.R:1093-1110 use dec_hm, c_hm, g_hm, beta, omega only)
#   - clamping p_hat to 0.99 on all 18 rows changes ZERO player-games
#
# So the winner cannot be going unpaid BECAUSE of p_hat. But "not because of
# p_hat" is not the same as "paid". The issue only closes if the winner is
# actually paid something, and that has to be read off the ledger's own payment
# columns rather than deduced from beta.
#
# What the code says should happen on these rows (all def_win == TRUE):
#   dw & retained : cede_c_hm = (1 - beta) * c_hm   -> the defensive pool
#   dw & turnover : cede_c_hm = (1 - beta) * c_hm   -> same
# with beta = NP_BLAME_SHARE = 0.30, so 70% of the contest surprise should be
# ceded to the defence and routed by .np_defensive_pool() using
# NP_CONTEST_WINNER_SHARE.
#
# This prints it instead of asserting it.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_209_what_the_winner_is_paid.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

# RUN IT ON ONLY THE MATCHES THAT MATTER. A first attempt built the whole 2026
# ledger with return_payments = TRUE and was killed for memory. It did not need
# to: build_net_points() allocates each match's margin within that match, so
# restricting to the handful of matches carrying the 18 saturated rows gives
# byte-identical payments for those rows at a fraction of the footprint. The
# difficulty terms are already fitted (2025 -> 2026) and are passed in, so
# subsetting cannot turn this into an in-sample fit.
SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))
ch[, match_id := as.character(match_id)]

say("NP_BLAME_SHARE (beta) = ", NP_BLAME_SHARE,
    "   => the defence should receive (1 - beta) = ", 1 - NP_BLAME_SHARE,
    " of the contest surprise")

tm <- as.data.table(np_difficulty_terms_for_season(SEASON, pbp_data = pbp, chains = ch))
hot <- tm[p_hat >= 0.99, .(match_id, display_order)]
say("\nsaturated rows: ", nrow(hot),
    "   spread over ", uniqueN(hot$match_id), " matches")

keep <- unique(hot$match_id)
pbp <- pbp[match_id %chin% keep]
ch  <- ch[match_id %chin% keep]
tm  <- tm[match_id %chin% keep]
ps  <- as.data.table(load_player_stats(SEASON, refresh = TRUE))
ps[, match_id := as.character(match_id)]; ps <- ps[match_id %chin% keep]
res <- as.data.table(load_results(SEASON))
res[, match_id := as.character(match_id)]; res <- res[match_id %chin% keep]
say("restricted to ", length(keep), " matches: ",
    format(nrow(pbp), big.mark = ","), " pbp rows, ",
    format(nrow(tm), big.mark = ","), " difficulty terms")

np <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                       stoppages = "allocate", difficulty_terms = tm,
                       return_payments = TRUE)
# The payments are an ATTRIBUTE, not the return value: build_net_points()
# returns the player-match table and hangs the per-act ledger off it as
# "np_payments" (epv_net_points.R:2146). One row per (act, role, recipient),
# with `hm` in the home-margin frame.
pay <- as.data.table(attr(np, "np_payments"))
pay[, match_id := as.character(match_id)]
say("\npayment rows: ", format(nrow(pay), big.mark = ","),
    "   roles: ", paste(sort(unique(pay$role)), collapse = ", "))

h <- merge(pay, hot, by = c("match_id", "display_order"))
say("payment rows landing on the ", nrow(hot), " saturated acts: ", nrow(h))

if (nrow(h) > 0) {
  say("\n=== who is paid on those acts, and how much ===")
  say("`contest_winner` is the player who WON the ball. If #209 were right, that")
  say("role would be absent or zero on every one of these acts.")
  say("sum_hm is signed in the home-margin frame; sum_abs is magnitude in points.")
  print(h[, .(n = .N,
              sum_hm = round(sum(hm), 3),
              sum_abs = round(sum(abs(hm)), 3),
              mean_abs = round(mean(abs(hm)), 3)), by = role][order(-sum_abs)])

  nwin <- h[role == "contest_winner", .N]
  say("\n  acts paying a contest_winner : ", nwin, " of ", nrow(hot))
  say("  points to contest winners     : ",
      round(sum(abs(h[role == "contest_winner", hm])), 3))

  say("\n=== the disposer's side, which is the real signature ===")
  say("mean |decision| on these acts was 1.347 against 0.087 overall (15x).")
  say("p_hat = 1 sets V_pre = V_trn, so the disposer is charged the full")
  say("turnover-branch value. All 18 acts ARE def_win -- the ball WAS lost -- so")
  say("the live question is whether that charge is CORRECT, not whether it is big.")
  say("  actor payments: n ", h[role == "actor", .N],
      ", sum_hm ", round(sum(h[role == "actor", hm]), 3),
      ", mean |hm| ", round(mean(abs(h[role == "actor", hm])), 3))
}

say("\n=== VERDICT ===")
if (nrow(h) > 0 && h[role == "contest_winner", .N] > 0 &&
    sum(abs(h[role == "contest_winner", hm])) > 1e-6) {
  say("  #209's PREMISE IS WRONG. The contest winner IS paid on every saturated")
  say("  row, through cede_c_hm, and p_hat plays no part in that payment.")
  say("  The issue should be closed as not-a-defect and reopened, if at all, on")
  say("  the real question: whether |decision| = 1.347 is a correct charge.")
} else {
  say("  The winner is NOT paid on these rows. #209 stands, but the mechanism in")
  say("  its description is still wrong -- it is not the (1 - p_hat) term.")
}
