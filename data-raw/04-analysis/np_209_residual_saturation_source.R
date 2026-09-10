# Where does the REMAINING p_hat saturation come from? (issues #209, #210)
# =============================================================================
# #210 removed the disposal model's leak and cut the saturated rows from 768 to
# 18 (p_hat >= 0.99, production regime, fit 2025 score 2026). Those 18 are why
# #209's defensive clamp stays: the defence is paid (1 - p_hat) * surprise, so
# p_hat = 1 pays whoever won the ball exactly nothing.
#
# A HINT, NOT YET A FINDING. The aerial-contest model, checked separately in
# np_contest_model_leak_check.R, produced 18 rows at p >= 0.99 with max 1.0
# out-of-fold on its own 50,050 contests. The end-to-end pipeline then produced
# 18 as well. Since .np_difficulty_terms() lets the contest model OVERWRITE
# p_hat on contested rows (epv_net_points.R:879), those two 18s lining up
# suggests the entire residual is the contest model's, and the disposal model
# now contributes zero.
#
# That is a coincidence of counts until it is checked directly, and "it lines up"
# is exactly the kind of reasoning that has been wrong here before. The check is
# one column: `contested`.
#
# It matters for where #209 gets fixed properly. If every saturated row is a
# contest, the clamp belongs in the contest path and the disposal path needs
# nothing; if they are mixed, the clamp has to stay general.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_209_residual_saturation_source.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))

tm <- as.data.table(np_difficulty_terms_for_season(SEASON, pbp_data = pbp, chains = ch))
say("\nrows: ", format(nrow(tm), big.mark = ","),
    "   contested: ", format(tm[contested == TRUE, .N], big.mark = ","),
    " (", round(100 * mean(tm$contested), 1), "%)")

hot <- tm[p_hat >= 0.99]
say("\n=== the ", nrow(hot), " rows at p_hat >= 0.99 ===")
say("If the hint is right, contested is TRUE on all of them.")
print(hot[, .(n = .N,
              median_p = round(median(p_hat), 5),
              max_p = round(max(p_hat), 5),
              mean_abs_surprise = round(mean(abs(surprise)), 3)), by = contested])

say("\n=== base rates, for comparison ===")
say("A contested row being over-represented among the saturated rows only means")
say("something if contests are not already saturating everywhere.")
print(tm[, .(n = .N,
             pct_of_all = round(100 * .N / nrow(tm), 1),
             median_p = round(median(p_hat), 3),
             p99_9 = round(quantile(p_hat, .999), 4),
             n_at_p99 = sum(p_hat >= 0.99)), by = contested])

say("\n=== what the defence loses on those rows ===")
say("payment is (1 - p_hat) * surprise, so this is what the clamp is protecting:")
say("  sum |surprise| on saturated rows : ", round(sum(abs(hot$surprise)), 2), " points")
say("  mean (1 - p_hat) there           : ", signif(mean(1 - hot$p_hat), 3))
say("  so the defence is paid           : ",
    round(sum((1 - hot$p_hat) * abs(hot$surprise)), 4), " points across all ", nrow(hot), " rows")

say("\n=== VERDICT ===")
n_con <- hot[contested == TRUE, .N]
if (nrow(hot) > 0 && n_con == nrow(hot)) {
  say("  CONFIRMED: all ", nrow(hot), " saturated rows are contests.")
  say("  The disposal path now contributes ZERO saturation, and #209's clamp")
  say("  can be scoped to the contest path.")
} else if (n_con == 0) {
  say("  REFUTED: none of the ", nrow(hot), " saturated rows is a contest.")
  say("  The hint was a coincidence of counts. The clamp stays general.")
} else {
  say("  MIXED: ", n_con, " of ", nrow(hot), " are contests.")
  say("  The hint was a coincidence of counts. The clamp stays general.")
}
