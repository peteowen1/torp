# Fix 1, stage 1: what does folding spoil_epv_ctx in do at the PLAYER-GAME level?
# =============================================================================
# The swing-allocation audit found two defects in the spoil channel:
#   LEVEL      the spoiler gets a flat 0.0737 against an unclaimed 0.1597 (2.17x)
#   DISPERSION the available swing has sd 0.4032 and IQR -0.478 to +0.181 -- the SIGN
#              varies -- while a flat weight has sd 0 by construction
# A bigger constant fixes only the first. `compute_spoil_credit()` already computes
# `-delta_epv * contest_share` per spoil and publishes it as `spoil_epv_ctx`, which
# fixes both -- and it has never been folded into `epv_spoil`.
#
# Stage 1 (this script, cheap): does the swap actually add discrimination at the
# player-game level? If the answer is no, there is no point rebuilding ratings.
# Stage 2 (only if this passes): push it through build_ratings_history() for the real
# rating effect.
#
# THE SHARE AND THE FALLBACK, both stated rather than buried:
#  - `spoil_epv_ctx` uses contest_share = 1/3. The conserving share is 0.5, matching
#    the kicker's `disp_scale`, so it is scaled by 1.5 here.
#  - it prices only ~47-57% of spoils (the scan looks back 5 rows for the kick that
#    was defused). Unpriced spoils are filled at the MEAN priced value -- measured
#    dispersion where it is measurable, no bias where it is not. Zero-filling would
#    silently delete real value.

suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)

p <- torp:::default_epv_params()
SEASONS <- 2021:2026
d <- as.data.table(load_player_game_data(SEASONS))
need <- c("spoils", "spoils_priced", "spoil_epv_ctx", "epv_spoil")
stopifnot(all(need %in% names(d)))
cli::cli_alert_info("{nrow(d)} player-games, seasons {min(d$season)}-{max(d$season)}")

SHARE_SCALE <- 1.5   # 1/3 -> 1/2, to match disp_scale and close the ledger

# THE FALLBACK BASE IS NOT (spoils - spoils_priced). Measured in
# audit_spoil_pricing_dropouts.R, spoils leave compute_spoil_credit() for four
# reasons and they do not mean the same thing:
#   contest triple, already priced via contest_epv   28.0% of all spoils
#   same-team kick (chain-logging artifact)          15.0%
#   no kick within 5 rows                             0.0%  (14 spoils in 3 seasons)
#   priced                                           56.9%
# Contest triples are 65.1% of the unpriced group and are ALREADY PAID, in the RECV
# channel. Filling them here pays them twice. Only the same-team/no-kick group --
# 34.9% of the unpriced group -- is a genuine gap.
# (Also note the 5-row scan is NOT the coverage limit, so widening it buys nothing.)
FALLBACK_FRACTION <- 0.349
OTHER <- c(tackles = "tackle_wt", pressure_acts = "pressure_wt",
           def_half_pressure_acts = "def_pressure_wt", intercepts = "intercepts_wt",
           one_percenters = "one_percenters_wt", rebound50s = "rebound50s_wt",
           frees_against = "frees_against_wt")

cli::cli_h1("1. coverage and the fallback value")
cov <- d[, .(spoils = sum(spoils, na.rm = TRUE), priced = sum(spoils_priced, na.rm = TRUE),
             ctx = sum(spoil_epv_ctx, na.rm = TRUE))]
per_priced <- cov$ctx / cov$priced
cli::cli_alert_info("coverage {round(100*cov$priced/cov$spoils, 1)}%; mean ctx per priced spoil {round(per_priced, 4)} (share 1/3)")
cli::cli_alert_info("at the conserving share 1/2: {round(per_priced * SHARE_SCALE, 4)} vs production flat {p$spoil_wt}")
FALLBACK <- per_priced * SHARE_SCALE

cli::cli_h1("2. rebuild the channel from published columns")
d[, other_box := Reduce(`+`, lapply(names(OTHER), function(s) get(s) * p[[OTHER[[s]]]]))]
d[, spoils_unpriced := pmax(spoils - spoils_priced, 0) * FALLBACK_FRACTION]
d[, spoil_term_old := spoils * p$spoil_wt]
d[, spoil_term_new := spoil_epv_ctx * SHARE_SCALE + spoils_unpriced * FALLBACK]
d[, epv_spoil_new := other_box + spoil_term_new]
# Reconstruction check: the old rebuild must reproduce the published column exactly,
# or the term list is wrong and everything below is wrong with it.
d[, epv_spoil_rebuilt := other_box + spoil_term_old]
gap <- max(abs(d$epv_spoil_rebuilt - d$epv_spoil), na.rm = TRUE)
cli::cli_alert_info("reconstruction of published epv_spoil: max |gap| = {signif(gap, 3)}")
if (gap > 1e-8) cli::cli_abort("Cannot reproduce published epv_spoil -- term list wrong, aborting.")
cli::cli_alert_success("Published channel reproduced exactly; the swap below is apples-to-apples.")

cli::cli_h1("3. the SPOIL TERM itself: does it gain discrimination?")
s <- d[spoils > 0]
cli::cli_alert_info("{nrow(s)} player-games with at least one spoil")
cli::cli_alert_info("OLD term: mean {round(mean(s$spoil_term_old), 4)}, sd {round(sd(s$spoil_term_old), 4)}")
cli::cli_alert_info("NEW term: mean {round(mean(s$spoil_term_new), 4)}, sd {round(sd(s$spoil_term_new), 4)}")
cli::cli_alert_info("level ratio {round(mean(s$spoil_term_new)/mean(s$spoil_term_old), 3)}x, dispersion ratio {round(sd(s$spoil_term_new)/sd(s$spoil_term_old), 3)}x")
cli::cli_alert_info("share of player-games where the NEW term is negative: {round(100*mean(s$spoil_term_new < 0), 1)}% (old: 0% by construction)")

cli::cli_h1("4. THE KEY TEST -- does it discriminate WITHIN a spoil count?")
# A flat weight gives every player with n spoils the same credit. If the new term
# varies within a fixed n, it is adding information a constant cannot carry. If it
# does not, the fold-in is pointless.
within <- s[spoils %in% 1:6, .(n_games = .N,
                               mean_new = round(mean(spoil_term_new), 3),
                               sd_new = round(sd(spoil_term_new), 3),
                               old = round(mean(spoil_term_old), 3)), by = spoils][order(spoils)]
print(within, row.names = FALSE)
cli::cli_alert_info("sd_new > 0 within a fixed spoil count IS the added discrimination; old sd is exactly 0.")
r2 <- summary(lm(spoil_term_new ~ factor(spoils), data = s))$r.squared
cli::cli_alert_info("R^2 of the new term on spoil COUNT alone = {round(r2, 3)}")
cli::cli_alert_info("=> {round(100*(1-r2), 1)}% of the new term's variance is information the count does not carry.")

cli::cli_h1("5. does it reach the CHANNEL, or is it drowned by the other 7 terms?")
cli::cli_alert_info("epv_spoil: old sd {round(sd(d$epv_spoil, na.rm=TRUE), 4)} -> new sd {round(sd(d$epv_spoil_new, na.rm=TRUE), 4)} (x{round(sd(d$epv_spoil_new, na.rm=TRUE)/sd(d$epv_spoil, na.rm=TRUE), 3)})")
cli::cli_alert_info("epv_spoil: old mean {round(mean(d$epv_spoil, na.rm=TRUE), 4)} -> new mean {round(mean(d$epv_spoil_new, na.rm=TRUE), 4)}")
cli::cli_alert_info("cor(old, new) = {round(cor(d$epv_spoil, d$epv_spoil_new, use='complete.obs'), 4)}")
cli::cli_alert_info("spoil term as share of |channel|: {round(100*mean(abs(s$spoil_term_old))/mean(abs(s$epv_spoil)), 1)}% -> {round(100*mean(abs(s$spoil_term_new))/mean(abs(s$epv_spoil_new)), 1)}%")

cli::cli_h1("6. by position -- who gains spread?")
KEY <- if ("position_group" %in% names(d)) "position_group" else "lineup_position"
pos <- d[!is.na(get(KEY)) & is.finite(epv_spoil) & is.finite(epv_spoil_new),
         .(n = .N, sd_old = round(sd(epv_spoil), 3), sd_new = round(sd(epv_spoil_new), 3),
           ratio = round(sd(epv_spoil_new) / sd(epv_spoil), 3),
           mean_old = round(mean(epv_spoil), 3), mean_new = round(mean(epv_spoil_new), 3)),
         by = c(KEY)][n >= 500][order(-ratio)]
print(pos, row.names = FALSE)
cli::cli_alert_info("A key-defender ratio above the others' is the signal Fix 1 exists to produce.")

saveRDS(list(within = within, pos = pos, fallback = FALLBACK, r2_on_count = r2),
        "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse/92e2b422-0dee-4727-90de-364d23375767/scratchpad/spoil_fold_preview.rds")
cli::cli_alert_success("done")
