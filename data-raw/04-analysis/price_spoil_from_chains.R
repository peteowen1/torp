# What is a spoil WORTH, priced from chain EPV rather than fitted?
# ================================================================
# ws13 measured epv_spoil converting at 4.02 points instead of 1, and
# decompose_epv_channels.R found why it could: the channel has NO chain-EPV term,
# so nothing ever put it in points units. Two attempts to fix that by changing its
# magnitude have now failed (§6.6's global multiplier sweep; refit_spoil_weights.R's
# 8-parameter refit, +0.017 on the valid forward target).
#
# This asks the question those attempts skipped: what does the CHAIN DATA say a
# spoil is worth? That is how epv_recv and epv_disp get their units -- they are
# `delta_epv x share` on real possessions -- and those are the two channels that
# already convert at ~1. Anchoring rather than rescaling.
#
# compute_spoil_credit() already computes exactly this: for each spoil it finds the
# kick it defused and credits `-delta_epv * contest_share` (share = 1/3, matching
# compute_contest_credit()), signed, excluding spoils already priced as contest
# triples so nothing double-counts. Published per player-game as `spoil_epv_ctx`
# with `spoils_priced`.
#
# NOTE what is and is not being reused. §6.5 measured this quantity as a
# replacement MEASURE of spoiling and it lost to the flat count on both
# predictive-validity tests -- so the flat count stays. But "which measure predicts
# defence better" and "what is one spoil worth in points" are different questions,
# and only the first was answered. This answers the second and leaves the measure
# alone.

suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)

p <- torp:::default_epv_params()
d <- as.data.table(load_player_game_data(TRUE))
need <- c("spoils", "spoils_priced", "spoil_epv_ctx")
stopifnot(all(need %in% names(d)))

cli::cli_h1("1. coverage -- what fraction of spoils can chains price?")
cov <- d[, .(spoils = sum(spoils, na.rm = TRUE),
             priced = sum(spoils_priced, na.rm = TRUE),
             ctx_total = sum(spoil_epv_ctx, na.rm = TRUE)), by = season][order(season)]
cov[, `:=`(coverage = round(priced / spoils, 3),
           # (a) unpriced spoils are like priced ones -- the per-priced-spoil mean
           price_if_representative = round(ctx_total / priced, 4),
           # (b) unpriced spoils are worth nothing -- total measured points spread
           #     over ALL spoils, which is what a flat weight multiplies
           price_if_unpriced_zero = round(ctx_total / spoils, 4))]
print(cov, row.names = FALSE)

lo <- mean(cov$price_if_unpriced_zero)
hi <- mean(cov$price_if_representative)
cli::cli_alert_info("Chain-anchored price per spoil is bracketed: {round(lo, 4)} to {round(hi, 4)}")
cli::cli_alert_info("Production spoil_wt = {p$spoil_wt}")

cli::cli_h1("2. verdict on the weight")
# The bracket is the honest form of the answer: (b) assumes an unpriced spoil is
# worth zero, which is too harsh -- a spoil is unpriced when the 5-row scan could
# not find its kick, not because it did nothing. (a) assumes unpriced spoils match
# priced ones. The truth is between.
inside <- p$spoil_wt >= lo && p$spoil_wt <= hi
if (inside) {
  cli::cli_alert_success("Production {p$spoil_wt} sits INSIDE the chain-anchored bracket [{round(lo,4)}, {round(hi,4)}].")
  cli::cli_alert_info("So the spoil PRICE is already defensible on EPV grounds. ws13's 4.02 slope is")
  cli::cli_alert_info("NOT evidence the weight is 4x too low -- a regression slope on team aggregates")
  cli::cli_alert_info("and a per-act expected-points price are different quantities.")
} else {
  ratio <- if (p$spoil_wt < lo) lo / p$spoil_wt else hi / p$spoil_wt
  cli::cli_alert_danger("Production {p$spoil_wt} is OUTSIDE the bracket -- off by ~{round(ratio, 2)}x.")
}
cli::cli_alert_info("ratio to midpoint: {round(mean(c(lo,hi)) / p$spoil_wt, 3)}x")

cli::cli_h1("3. is the price stable across seasons?")
# A constant is only defensible if the underlying price does not wander. The totals
# calibration already wanders (EPR 0.87-1.02), so check this one too.
cli::cli_alert_info("price_if_representative range: {round(min(cov$price_if_representative),4)} to {round(max(cov$price_if_representative),4)} (spread {round(max(cov$price_if_representative)/min(cov$price_if_representative),3)}x)")
cli::cli_alert_info("price_if_unpriced_zero  range: {round(min(cov$price_if_unpriced_zero),4)} to {round(max(cov$price_if_unpriced_zero),4)} (spread {round(max(cov$price_if_unpriced_zero)/min(cov$price_if_unpriced_zero),3)}x)")

cli::cli_h1("4. what the 10-20x claim implies, for comparison")
# The original defender diagnosis claimed the spoil channel was weighted 10-20x too
# low. Stated in these units so the two claims can be compared directly.
cli::cli_alert_info("10x would mean a spoil is worth {round(10 * p$spoil_wt, 3)} points; 20x -> {round(20 * p$spoil_wt, 3)}")
cli::cli_alert_info("The chain data says {round(lo, 4)}-{round(hi, 4)}.")
if (10 * p$spoil_wt > hi) {
  cli::cli_alert_danger("The 10-20x claim is REFUTED by the chain-anchored price, by a factor of {round(10 * p$spoil_wt / hi, 1)}x or more.")
}

cli::cli_h1("5. sanity: does the signed credit behave as documented?")
# The roxygen says ~40% of spoils earn NEGATIVE credit (a spoil on a kick that was
# still good for the attackers). If that is not visible, the column is not what it
# claims and everything above is void.
pg <- d[spoils_priced > 0]
cli::cli_alert_info("{nrow(pg)} player-games with a priced spoil; mean credit per game {round(mean(pg$spoil_epv_ctx), 4)}")
cli::cli_alert_info("player-games with NEGATIVE total contextual credit: {round(100*mean(pg$spoil_epv_ctx < 0), 1)}%")
cli::cli_alert_info("(roxygen claims ~40% of individual SPOILS are negative; per-game totals net off, so expect less)")

saveRDS(cov, "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse/92e2b422-0dee-4727-90de-364d23375767/scratchpad/spoil_price.rds")
cli::cli_alert_success("done")
