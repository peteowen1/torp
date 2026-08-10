# Is every EPV swing fully allocated, and are the box-score substitutes sized right?
# ================================================================================
# The unifying frame, arrived at 2026-07-30 after the duel work collapsed into
# "delta_epv already IS the counterfactual swing":
#
#   The chain path splits each swing 0.5 to the disposer (disp_scale) and 0.5 to the
#   receiver (recv_scale), so it allocates exactly 1.0 of the swing BY CONSTRUCTION.
#   That is why aerial mark contests conserve without anyone tuning them.
#
#   Spoils and ruck taps are different: the counterparty is a BOX-SCORE event with no
#   chain row to credit. `epv_spoil` and `epv_hitout` are flat-weight SUBSTITUTES
#   standing in for chain credit that cannot be attributed.
#
# So there is one question for both, and it is checkable:
#   IS THE SUBSTITUTE THE SAME SIZE AS THE SWING IT STANDS IN FOR?
#
# Three tests:
#   A. confirm the 0.5 + 0.5 exhaustion on events where BOTH sides are chain rows
#   B. spoils   -- swing available to the spoiler vs EPV_SPOIL_WT
#   C. ruck taps -- the Centre Bounce / Ball Up swing is credited to NOBODY (no
#      player_id on those rows), so the whole thing is unallocated. Compare it to what
#      the three ruck box weights actually pay.

suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)

p <- torp:::default_epv_params()
SEASONS <- 2024:2026
MARK_RE <- "ted Mark|Mark On"
pbp <- as.data.table(load_pbp(SEASONS, rounds = TRUE))
setorder(pbp, match_id, display_order)

cli::cli_h1("A. does the chain path allocate exactly 1.0 of each swing?")
# The disposer's share and the receiver's share are separate constants; if they sum to
# 1 the allocation is exhaustive by construction rather than by luck.
cli::cli_alert_info("disp_scale {p$disp_scale} + recv_scale {p$recv_scale} = {p$disp_scale + p$recv_scale}")
cli::cli_alert_info("contest_share (3-way aerial) {round(p$contest_share %||% (1/3), 4)} x 3 = {round(3*(p$contest_share %||% (1/3)), 4)}")
if (abs((p$disp_scale + p$recv_scale) - 1) < 1e-9) {
  cli::cli_alert_success("Exhaustive: a 2-player chain event allocates 100% of its swing.")
} else {
  cli::cli_alert_danger("NOT exhaustive -- 2-player chain events allocate {round(100*(p$disp_scale+p$recv_scale))}% of the swing.")
}

K <- pbp[description %in% c("Kick", "Ground Kick") & is.finite(delta_epv) &
           !is.na(lead_desc_tot) & pos_team %in% c(-1L, 1L) & is.na(points_shot)]
if ("shot_at_goal" %in% names(K)) K <- K[!(shot_at_goal %in% TRUE)]
K[, outcome := fcase(
  grepl("Spoil", lead_desc_tot),                   "spoiled",
  grepl(MARK_RE, lead_desc_tot) & pos_team ==  1L, "att_mark",
  grepl(MARK_RE, lead_desc_tot) & pos_team == -1L, "def_mark",
  default = "other")]
# The two chain shares of the same swing.
K[, `:=`(disposer_share = delta_epv * p$disp_scale,
         receiver_share = delta_epv * p$recv_scale,
         swing = delta_epv)]
alloc <- K[, .(n = .N,
               swing = round(mean(swing), 4),
               disposer = round(mean(disposer_share), 4),
               receiver = round(mean(receiver_share), 4),
               allocated = round(mean(disposer_share) + mean(receiver_share), 4)), by = outcome]
alloc[, pct_allocated := round(100 * allocated / swing, 1)]
print(alloc[order(outcome)], row.names = FALSE)
cli::cli_alert_info("For att_mark and def_mark BOTH sides are chain rows, so 100% is expected.")
cli::cli_alert_info("For `spoiled` the receiver side does NOT exist -- there is no chain row for the spoiler.")

cli::cli_h1("B. SPOILS -- the swing available to the spoiler vs what he is paid")
sp <- K[outcome == "spoiled"]
avail <- mean(sp$receiver_share)     # the unclaimed half of the swing
cli::cli_alert_info("{nrow(sp)} spoiled kicks; mean swing {round(mean(sp$swing), 4)}")
cli::cli_alert_info("disposer (kicker) is debited {round(mean(sp$disposer_share), 4)}")
cli::cli_alert_info("the RECEIVER half -- {round(avail, 4)} -- has no chain row to land on; the spoiler is the counterparty")
cli::cli_alert_info("production pays the spoiler a FLAT EPV_SPOIL_WT = {p$spoil_wt}")
cli::cli_alert_info("=> ratio {round(abs(avail)/p$spoil_wt, 2)}x  (production {ifelse(abs(avail) > p$spoil_wt, 'UNDER', 'OVER')}pays)")

# The other half of the story: a flat weight cannot vary, and the swing does.
cli::cli_alert_info("dispersion of the available swing: sd {round(sd(sp$receiver_share), 4)}, ")
cli::cli_alert_info("  IQR {paste(round(quantile(sp$receiver_share, c(.25,.75)), 3), collapse=' to ')}, ")
cli::cli_alert_info("  a flat weight has sd 0 by definition -- that is the part no constant can fix.")

cli::cli_h1("C. RUCK TAPS -- an entirely UNALLOCATED swing")
# Centre Bounce and Ball Up Call rows carry no player_id (verified in chains), so
# NEITHER side of their swing is credited to anyone through the chain path. The three
# ruck box weights are the substitute for the whole thing.
rk <- pbp[description %in% c("Centre Bounce", "Ball Up Call") & is.finite(delta_epv)]
cli::cli_alert_info("{nrow(rk)} ruck-contest rows; mean swing {round(mean(rk$delta_epv), 4)}")
cli::cli_alert_info("credited through the chain path: 0 (no player_id on these rows)")
pg <- as.data.table(load_player_game_data(SEASONS))
tot <- pg[, .(hitouts = sum(hitouts, na.rm = TRUE), adv = sum(hitouts_to_advantage, na.rm = TRUE),
              contests = sum(ruck_contests, na.rm = TRUE))]
# Per ruck contest, both rucks counted (ruck_contests is a per-player stat).
per_contest_per_ruck <- p$ruck_contest_wt +
  (tot$hitouts / tot$contests) * p$hitout_wt +
  (tot$adv / tot$contests) * p$hitout_adv_wt
both_rucks <- per_contest_per_ruck * 2
cli::cli_alert_info("production pays per contest: {round(per_contest_per_ruck, 4)} per ruck, {round(both_rucks, 4)} for both")
swing_r <- mean(rk$delta_epv)
cli::cli_alert_info("=> production allocates {round(100*both_rucks/swing_r, 1)}% of the ruck-contest swing")
cli::cli_alert_info("   ratio {round(swing_r/both_rucks, 2)}x")
if (both_rucks < 0.5 * swing_r) {
  cli::cli_alert_danger("Under half the swing is allocated -- the ruck substitute is UNDERSIZED by {round(swing_r/both_rucks, 1)}x.")
} else if (both_rucks > 1.5 * swing_r) {
  cli::cli_alert_danger("More than 1.5x the swing is allocated -- the substitute is OVERSIZED.")
} else {
  cli::cli_alert_success("Within 0.5-1.5x of the swing -- the substitute is roughly the right size.")
}

cli::cli_h1("D. summary ledger, per event type")
led <- data.table(
  event = c("aerial mark (att)", "aerial mark (intercept)", "spoil", "ruck tap"),
  swing = c(alloc[outcome == "att_mark"]$swing, alloc[outcome == "def_mark"]$swing,
            mean(sp$swing), swing_r),
  chain_allocated = c(alloc[outcome == "att_mark"]$allocated, alloc[outcome == "def_mark"]$allocated,
                      mean(sp$disposer_share), 0),
  box_substitute = c(0, 0, p$spoil_wt, both_rucks)
)
led[, total_allocated := chain_allocated + fifelse(event == "spoil", -box_substitute, box_substitute)]
led[, pct := round(100 * abs(total_allocated) / abs(swing), 1)]
print(led[, .(event, swing = round(swing, 4), chain_allocated = round(chain_allocated, 4),
              box_substitute = round(box_substitute, 4), pct_of_swing_allocated = pct)],
      row.names = FALSE)
cli::cli_alert_info("Where pct is ~100 the ledger balances. Where it is far off, the box substitute is mis-sized.")

saveRDS(list(alloc = alloc, spoil_available = avail, ruck_swing = swing_r,
             ruck_paid = both_rucks, ledger = led),
        "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse/92e2b422-0dee-4727-90de-364d23375767/scratchpad/swing_allocation.rds")
cli::cli_alert_success("done")
