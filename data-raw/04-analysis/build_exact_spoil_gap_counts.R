# Exact per-player-match spoil gap counts, replacing the biased global fraction
# ============================================================================
# The spoil fold filled unpriced spoils using a GLOBAL fraction (34.9% genuine gaps,
# 65.1% contest triples already paid via contest_epv). Measured by position, that
# constant is wrong by up to 24x:
#
#   KEY_DEFENDER    0.039     <- almost all their unpriced spoils are contest triples
#   MEDIUM_DEFENDER 0.046
#   MIDFIELDER      0.468
#   RUCK            0.617
#   MEDIUM_FORWARD  0.882
#   KEY_FORWARD     0.934
#
# So a global 0.349 OVER-fills key defenders ~9x, crediting them a second time for
# spoils already paid in the RECV channel -- and key defenders are exactly the players
# the fix is aimed at, so the reported +38% spread was inflated by my own
# approximation.
#
# The fix is exact rather than a better constant: classify every spoil from chains,
# aggregate the GENUINE GAPS per (player_id, match_id), and join that in. No fraction.
#
# Output: a parquet of player_id, match_id, spoils_gap, spoils_triple, spoils_priced_chk
# for reuse by the preview and the gate. Production would instead expose these from
# compute_spoil_credit() directly -- noted in the plan -- but that needs a pgd rebuild
# to reach player_game, whereas this is usable now.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)

SEASONS <- 2021:2026
OUT <- "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse/92e2b422-0dee-4727-90de-364d23375767/scratchpad/spoil_gap_counts.parquet"

target_descs <- torp:::CHAINS_CONTEST_TARGET_DESCS
inflight <- torp:::CHAINS_INFLIGHT_DESCS
kick_descs <- c("Kick", "Ground Kick")

res <- rbindlist(lapply(SEASONS, function(s) {
  ch <- as.data.table(load_chains(s, rounds = TRUE))
  if (!nrow(ch)) return(NULL)
  data.table::setorder(ch, match_id, display_order)
  ch[, `:=`(
    .prev_desc = shift(description, 1L), .prev_tid = shift(team_id, 1L),
    .prev_x = shift(x, 1L), .prev_y = shift(y, 1L),
    .l1 = shift(description, 1L), .l2 = shift(description, 2L), .l3 = shift(description, 3L),
    .l4 = shift(description, 4L), .l5 = shift(description, 5L),
    .t1 = shift(team_id, 1L), .t2 = shift(team_id, 2L), .t3 = shift(team_id, 3L),
    .t4 = shift(team_id, 4L), .t5 = shift(team_id, 5L)
  ), by = match_id]
  sp <- ch[description == "Spoil" & !is.na(player_id)]
  if (!nrow(sp)) return(NULL)
  sp[, is_triple := { v <- .prev_desc %chin% target_descs & x == .prev_x & y == .prev_y &
                        !is.na(.prev_tid) & team_id != .prev_tid; v[is.na(v)] <- FALSE; v }]
  sp[, `:=`(
    .c2 = .l1 %chin% inflight,
    .c3 = .l1 %chin% inflight & .l2 %chin% inflight,
    .c4 = .l1 %chin% inflight & .l2 %chin% inflight & .l3 %chin% inflight,
    .c5 = .l1 %chin% inflight & .l2 %chin% inflight & .l3 %chin% inflight & .l4 %chin% inflight)]
  sp[, .kick_lag := fcase(
    .l1 %chin% kick_descs, 1L,
    .c2 & .l2 %chin% kick_descs, 2L,
    .c3 & .l3 %chin% kick_descs, 3L,
    .c4 & .l4 %chin% kick_descs, 4L,
    .c5 & .l5 %chin% kick_descs, 5L, default = NA_integer_)]
  sp[, kick_tid := fcase(.kick_lag == 1L, .t1, .kick_lag == 2L, .t2, .kick_lag == 3L, .t3,
                         .kick_lag == 4L, .t4, .kick_lag == 5L, .t5)]
  sp[, class := fcase(
    is_triple, "triple",
    is.na(.kick_lag), "gap",
    is.na(kick_tid) | team_id == kick_tid, "gap",
    default = "priced")]
  out <- sp[, .(spoils_gap = sum(class == "gap"),
                spoils_triple = sum(class == "triple"),
                spoils_priced_chk = sum(class == "priced")),
            by = .(player_id = as.character(player_id), match_id = as.character(match_id))]
  cli::cli_alert_info("{s}: {nrow(sp)} spoils -> {nrow(out)} player-matches (gap {sum(out$spoils_gap)}, triple {sum(out$spoils_triple)}, priced {sum(out$spoils_priced_chk)})")
  out
}))

cli::cli_h1("validation")
cli::cli_alert_info("{nrow(res)} player-match rows")
tot <- res[, .(gap = sum(spoils_gap), triple = sum(spoils_triple), priced = sum(spoils_priced_chk))]
cli::cli_alert_info("gap {tot$gap} | triple {tot$triple} | priced {tot$priced}")
cli::cli_alert_info("implied global fraction = {round(tot$gap/(tot$gap+tot$triple), 4)} (the constant that was in use: 0.349)")

# Cross-check the priced count against the published spoils_priced, which is what
# compute_spoil_credit() actually produced. If these disagree badly the classification
# here does not match production's and the exact counts are not usable.
pg <- as.data.table(load_player_game_data(SEASONS))[, .(player_id = as.character(player_id),
                                                        match_id = as.character(match_id),
                                                        spoils, spoils_priced)]
m <- merge(pg, res, by = c("player_id", "match_id"), all.x = TRUE)
m[is.na(spoils_gap), `:=`(spoils_gap = 0L, spoils_triple = 0L, spoils_priced_chk = 0L)]
cli::cli_alert_info("cor(published spoils_priced, reclassified priced) = {round(cor(m$spoils_priced, m$spoils_priced_chk), 4)}")
cli::cli_alert_info("mean |difference| = {round(mean(abs(m$spoils_priced - m$spoils_priced_chk)), 4)}")
agree <- mean(m$spoils_priced == m$spoils_priced_chk)
cli::cli_alert_info("exact agreement on {round(100*agree, 1)}% of player-matches")
if (agree < 0.9) {
  cli::cli_alert_danger("Under 90% agreement -- this classification does NOT reproduce compute_spoil_credit().")
  cli::cli_alert_danger("Do not use these counts until the discrepancy is understood.")
} else {
  cli::cli_alert_success("Reproduces production's priced count -- the gap counts are trustworthy.")
}

arrow::write_parquet(res, OUT)
cli::cli_alert_success("wrote {OUT}")
