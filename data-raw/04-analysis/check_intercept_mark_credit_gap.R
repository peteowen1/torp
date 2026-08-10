# Does production ALREADY credit an intercept mark, and how much is actually missing?
# ==================================================================================
# The counterfactual says an intercept mark is worth 1.5368 points to the defence --
# 4.8x a spoil -- and EPV_RECV_INTERCEPT_MARK_SCALE is 1, i.e. no premium. That looks
# like a large gap, but the same shape of argument has evaporated under measurement
# before (§6.5: contextual spoil credit looked obviously better and predicted worse).
# So this measures what production ALREADY pays an intercept mark before claiming
# anything is missing.
#
# How production credits it (player_credit.R, Step 2):
#   is_intercept_mark = pos_team == -1L & grepl("ted Mark|Mark On", lead_desc_tot)
#   epv_recv contribution = ((recv_neg_mult * delta_epv * pos_team) + recv_neg_offset)
#                            * recv_intercept_mark_scale
# with recv_neg_mult 1, recv_neg_offset 0, recv_intercept_mark_scale 1, so it reduces
# to `delta_epv * pos_team` -- the REALISED forward value, on the intercepting team's
# orientation. The counterfactual value is a different quantity, so the gap is
# (counterfactual credit) - (what the realised path already pays).
#
# THE COMPARISON MUST BE ON THE SAME ROWS. The 1.5368 was computed on the KICK rows
# (whose lead was an intercept mark), because that is where the contest and its
# alternatives are visible. Production pays the MARKER, on the mark row. Same event,
# two different rows, so both quantities are computed per event here.

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

cli::cli_h1("1. what production pays, per intercept mark")
# Reproduce production's own formula on its own rows, rather than re-deriving it.
ip <- pbp[pos_team == -1L & grepl(MARK_RE, lead_desc_tot) & is.finite(delta_epv)]
cli::cli_alert_info("{nrow(ip)} intercept-mark events (production's own predicate)")
ip[, prod_credit := ((p$recv_neg_mult * delta_epv * pos_team) + p$recv_neg_offset) *
     p$recv_intercept_mark_scale * p$recv_scale]
cli::cli_alert_info("production credit per intercept mark: mean {round(mean(ip$prod_credit), 4)}, median {round(median(ip$prod_credit), 4)}")
cli::cli_alert_info("  (formula reduces to delta_epv * pos_team * recv_scale, recv_scale = {p$recv_scale})")
cli::cli_alert_info("share negative: {round(100*mean(ip$prod_credit < 0), 1)}%")

cli::cli_h1("2. the SAME events, valued counterfactually")
# The intercept mark's counterfactual credit was computed on the kick row. Those are
# exactly the rows in `ip` -- production's predicate keys on the kick's lead_desc_tot,
# so `ip` IS the kick population. Recompute the counterfactual on it.
pbp[, V_after := exp_pts + delta_epv]
K <- pbp[description %in% c("Kick", "Ground Kick") & is.finite(x) & is.finite(V_after) &
           !is.na(lead_desc_tot) & pos_team %in% c(-1L, 1L) & is.na(points_shot)]
if ("shot_at_goal" %in% names(K)) K <- K[!(shot_at_goal %in% TRUE)]
K[, outcome := fcase(
  grepl("Spoil", lead_desc_tot),                   "spoiled",
  grepl(MARK_RE, lead_desc_tot) & pos_team ==  1L, "att_mark",
  grepl(MARK_RE, lead_desc_tot) & pos_team == -1L, "def_mark",
  default = "other")]
K[, xb := round(x / 10) * 10]
agg <- K[, .(n = .N, V = mean(V_after)), by = .(xb, outcome)]
tot <- agg[, .(n_tot = sum(n)), by = xb]
agg[tot, p := n / i.n_tot, on = "xb"]
full <- agg[, .N, by = xb][N == 4]$xb
agg <- agg[xb %in% full & xb %in% tot[n_tot >= 300]$xb]
tu <- agg[, .(toss_up = sum(p * V)), by = xb]
agg[tu, toss_up := i.toss_up, on = "xb"]
agg[, credit := toss_up - V]
cf <- agg[outcome == "def_mark", .(xb, cf_credit = credit, n_def = n)]

# Join both quantities per x-bin so like is compared with like.
ip[, xb := round(x / 10) * 10]
prod_by_x <- ip[xb %in% cf$xb, .(n_prod = .N, prod_credit = mean(prod_credit)), by = xb]
cmp <- merge(cf, prod_by_x, by = "xb")
cmp[, gap := cf_credit - prod_credit]
print(cmp[order(xb), .(xb, n_def, cf_credit = round(cf_credit, 3),
                       prod_credit = round(prod_credit, 3), gap = round(gap, 3))],
      row.names = FALSE)
w <- cmp$n_def / sum(cmp$n_def)
cli::cli_alert_info("event-weighted: counterfactual {round(sum(cmp$cf_credit*w), 4)}, production {round(sum(cmp$prod_credit*w), 4)}, GAP {round(sum(cmp$gap*w), 4)}")
cli::cli_alert_info("ratio: production pays {round(sum(cmp$prod_credit*w)/sum(cmp$cf_credit*w), 3)} of the counterfactual value")

cli::cli_h1("3. is the gap real, or does the realised path already track it?")
cr <- cor(cmp$cf_credit, cmp$prod_credit)
cli::cli_alert_info("cor(counterfactual, production) across x-bins = {round(cr, 3)}")
if (abs(sum(cmp$gap * w)) < 0.1) {
  cli::cli_alert_success("Gap under 0.1 points -- production already pays roughly the counterfactual value. NOT a lead.")
} else if (cr > 0.8) {
  cli::cli_alert_warning("Large gap but HIGH correlation: production tracks the shape and misses the LEVEL,")
  cli::cli_alert_warning("which a single scale constant could fix -- EPV_RECV_INTERCEPT_MARK_SCALE is exactly that dial.")
} else {
  cli::cli_alert_danger("Large gap AND low correlation: production mis-ranks intercept marks, not just mis-scales them.")
}

cli::cli_h1("4. how much would closing it move a key defender?")
# Bound the effect before recommending anything: intercept marks per player-game, at
# the measured gap, against the mean |epv| of the recv channel.
pg <- as.data.table(load_player_game_data(SEASONS))
if ("intercepts" %in% names(pg)) {
  im_per_game <- nrow(ip) / uniqueN(paste(pbp$match_id)) / 44   # rough per player-game
  cli::cli_alert_info("~{round(nrow(ip)/uniqueN(pbp$match_id), 1)} intercept marks per MATCH ({round(nrow(ip)/uniqueN(pbp$match_id)/2, 1)} per team)")
  cli::cli_alert_info("mean |epv_recv| per player-game = {round(mean(abs(pg$epv_recv), na.rm=TRUE), 3)}")
  cli::cli_alert_info("a key defender taking 2 intercept marks would gain ~{round(2*sum(cmp$gap*w), 3)} points of epv_recv")
}
cli::cli_alert_info("Compare: EPV_RECV_INTERCEPT_MARK_SCALE would need to be ~{round(sum(cmp$cf_credit*w)/sum(cmp$prod_credit*w), 2)} to close it,")
cli::cli_alert_info("against its current value of {p$recv_intercept_mark_scale}.")

saveRDS(cmp, "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse/92e2b422-0dee-4727-90de-364d23375767/scratchpad/intercept_gap.rds")
cli::cli_alert_success("done")
