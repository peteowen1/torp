# What does p_hat ACTUALLY change in a payment? (issues #209, #210)
# =============================================================================
# A code review caught me stating, in NEWS.md, DECISIONS.md, two issue comments
# and a commit message, that "the defence is paid (1 - p_hat) * surprise, so
# p_hat = 1 pays whoever won the ball nothing."
#
# Reading .np_credit_terms() says that is wrong twice over:
#
#   epv_net_points.R:1059-1063  retained: recv_hm = (1-omega)*(1-p_hat)*sur_hm
#                               -> that is the RECEIVER, a teammate, not the defence
#   epv_net_points.R:1064-1068  turnover: own_hm = dec_hm + beta*sur_hm
#                               cede_hm = (1-beta)*sur_hm   <- p_hat ABSENT
#
# and worse, line 1061 looks DEAD, because every retained row is overwritten
# further down:
#
#   1093 / 1102  contested retained  -> recv_hm = (1-omega)*g_hm
#   1125         uncontested retained -> recv_hm = (1-omega)*u*sur_hm, u = 0.5
#                (only when NP_UNCONTESTED_RECEIVER_SHARE is finite -- it is, 0.5)
#
# If that is right, p_hat is NEVER a payment share on a live branch. It would
# still matter, but through a different route entirely: V_pre = (1-p)*V_ret +
# p*V_trn, so p_hat shapes `decision` and `surprise` themselves.
#
# THAT IS A READING, AND READING IS WHAT PRODUCED THE ORIGINAL ERROR. This
# script measures it instead. Three questions:
#
#   (1) How many scored rows actually keep line 1061's p_hat split? The `cs`
#       mask needs finite c_hm/g_hm, so a contested row with a non-finite
#       contest term falls through BOTH overwrites and keeps it.
#   (2) On the 18 rows at p_hat >= 0.99, which branch pays, and what changes?
#   (3) Counterfactual: re-run the ledger with p_hat clamped to 0.99 and see
#       which players' payments actually move. That answers "what does
#       saturation cost" without any theory about which term is responsible.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_phat_payment_path_audit.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))
ps  <- as.data.table(load_player_stats(SEASON, refresh = TRUE))
res <- as.data.table(load_results(SEASON))

say("NP_UNCONTESTED_RECEIVER_SHARE = ", NP_UNCONTESTED_RECEIVER_SHARE,
    "  (finite => the line-1125 overwrite is LIVE)")

tm <- as.data.table(np_difficulty_terms_for_season(SEASON, pbp_data = pbp, chains = ch))

say("\n=== (1) which rows could still keep the line-1061 p_hat split? ===")
say("A row keeps it only if it is retained, contested, and its contest terms are")
say("NOT finite -- otherwise 1093/1102 (contested) or 1125 (uncontested) overwrite.")
say("  contested rows                        : ", format(tm[contested == TRUE, .N], big.mark = ","))
say("  contested with non-finite cont_surprise: ",
    tm[contested == TRUE & !is.finite(cont_surprise), .N])
say("  contested with non-finite ground_surprise: ",
    tm[contested == TRUE & !is.finite(ground_surprise), .N])
say("  contested with def_win NA             : ", tm[contested == TRUE & is.na(def_win), .N])

say("\n=== (2) the saturated rows ===")
hot <- tm[p_hat >= 0.99]
say("  rows at p_hat >= 0.99 : ", nrow(hot))
say("  all contested         : ", all(hot$contested))
say("  def_win TRUE          : ", hot[def_win == TRUE, .N], " of ", nrow(hot))
say("  mean |decision|       : ", round(mean(abs(hot$decision)), 3),
    "   vs ", round(mean(abs(tm$decision)), 3), " overall")
say("  mean |surprise|       : ", round(mean(abs(hot$surprise)), 3),
    "   vs ", round(mean(abs(tm$surprise)), 3), " overall")
say("\n  p_hat = 1 makes V_pre = V_trn, so `decision` is the turnover branch's")
say("  value minus exp_pts. If |decision| is inflated on these rows, the DISPOSER")
say("  is being charged for a certain turnover that did not happen.")

# --- (3) the counterfactual, which needs no theory at all ---------------------
run <- function(terms, label) {
  say("\n--- ledger: ", label, " ---")
  np <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                         stoppages = "allocate", difficulty_terms = terms,
                         return_payments = TRUE)
  fin <- as.data.table(torp:::.np_team_margin(np, pbp, ps, res))
  fin[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
  fin[, .(match_id, player_id, net_points)]
}

say("\n=== (3) counterfactual: clamp p_hat to 0.99 and re-run the whole ledger ===")
tm_clamped <- copy(tm)
n_clamp <- tm_clamped[p_hat > 0.99, .N]
tm_clamped[p_hat > 0.99, p_hat := 0.99]
say("clamped ", n_clamp, " rows")

r0 <- run(tm, "as-is")
r1 <- run(tm_clamped, "p_hat clamped at 0.99")

cmp <- merge(r0, r1, by = c("match_id", "player_id"), suffixes = c("_asis", "_clamp"))
cmp[, delta := net_points_clamp - net_points_asis]
say("\nplayer-games compared : ", format(nrow(cmp), big.mark = ","))
say("  any change at all    : ", cmp[abs(delta) > 1e-9, .N])
say("  max |change|         : ", round(max(abs(cmp$delta)), 4), " points")
say("  sum |change|         : ", round(sum(abs(cmp$delta)), 4), " points")
say("\nThis is what the saturation actually costs, measured end to end, with no")
say("assumption about which term carries it.")
