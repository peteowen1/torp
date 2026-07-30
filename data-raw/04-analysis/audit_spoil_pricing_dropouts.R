# WHY are 45.5% of spoils unpriced -- and is my Fix 1 fallback double-counting?
# ===========================================================================
# Fix 1 fills `spoils - spoils_priced` at the mean priced value. Reading
# compute_spoil_credit() properly, spoils drop out of pricing for FOUR different
# reasons, and they do not all mean the same thing:
#
#   1. no player_id                      -- genuinely unattributable
#   2. CONTEST TRIPLE, already priced by compute_contest_credit() via `contest_epv`
#                                        -- NOT unpriced. Filling these DOUBLE-PAYS.
#   3. no Kick/Ground Kick within 5 rows back through in-flight rows only
#                                        -- a scan limit; widening might recover them
#   4. the kick was the SAME team (~16%, described in the code as chain-logging
#      artifacts)                        -- deliberately dropped, should stay dropped
#
# Only (3) is a coverage problem worth fixing, and only (1)+(3)+(4) are safe to fill.
# If (2) is a material share, the Fix 1 fallback as currently written is wrong.
#
# This counts each stage on real chains data.

suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)

SEASONS <- 2024:2026
ch <- as.data.table(load_chains(SEASONS, rounds = TRUE))
data.table::setorder(ch, match_id, display_order)
target_descs <- torp:::CHAINS_CONTEST_TARGET_DESCS
inflight <- torp:::CHAINS_INFLIGHT_DESCS
kick_descs <- c("Kick", "Ground Kick")

ch[, `:=`(
  .prev_desc = shift(description, 1L), .prev_tid = shift(team_id, 1L),
  .prev_x = shift(x, 1L), .prev_y = shift(y, 1L),
  .l1 = shift(description, 1L), .l2 = shift(description, 2L), .l3 = shift(description, 3L),
  .l4 = shift(description, 4L), .l5 = shift(description, 5L),
  .t1 = shift(team_id, 1L), .t2 = shift(team_id, 2L), .t3 = shift(team_id, 3L),
  .t4 = shift(team_id, 4L), .t5 = shift(team_id, 5L)
), by = match_id]

all_sp <- ch[description == "Spoil"]
cli::cli_alert_info("{nrow(all_sp)} Spoil rows, seasons {min(SEASONS)}-{max(SEASONS)}")

cli::cli_h1("stage-by-stage dropout")
n0 <- nrow(all_sp)
s1 <- all_sp[!is.na(player_id)]
cli::cli_alert_info("1. no player_id:            -{n0 - nrow(s1)} ({round(100*(n0-nrow(s1))/n0, 1)}%)")

is_triple <- s1[, .prev_desc %chin% target_descs & x == .prev_x & y == .prev_y &
                  !is.na(.prev_tid) & team_id != .prev_tid]
is_triple[is.na(is_triple)] <- FALSE
n_triple <- sum(is_triple)
s2 <- s1[!is_triple]
cli::cli_alert_info("2. CONTEST TRIPLE (already priced via contest_epv): -{n_triple} ({round(100*n_triple/n0, 1)}%)")

s2[, `:=`(
  .c2 = .l1 %chin% inflight,
  .c3 = .l1 %chin% inflight & .l2 %chin% inflight,
  .c4 = .l1 %chin% inflight & .l2 %chin% inflight & .l3 %chin% inflight,
  .c5 = .l1 %chin% inflight & .l2 %chin% inflight & .l3 %chin% inflight & .l4 %chin% inflight
)]
s2[, .kick_lag := fcase(
  .l1 %chin% kick_descs, 1L,
  .c2 & .l2 %chin% kick_descs, 2L,
  .c3 & .l3 %chin% kick_descs, 3L,
  .c4 & .l4 %chin% kick_descs, 4L,
  .c5 & .l5 %chin% kick_descs, 5L,
  default = NA_integer_)]
n_nokick <- s2[is.na(.kick_lag), .N]
cli::cli_alert_info("3. no kick within 5 rows:   -{n_nokick} ({round(100*n_nokick/n0, 1)}%)")
s3 <- s2[!is.na(.kick_lag)]
s3[, kick_tid := fcase(.kick_lag == 1L, .t1, .kick_lag == 2L, .t2, .kick_lag == 3L, .t3,
                       .kick_lag == 4L, .t4, .kick_lag == 5L, .t5)]
n_same <- s3[is.na(kick_tid) | team_id == kick_tid, .N]
cli::cli_alert_info("4. same-team kick (artifact): -{n_same} ({round(100*n_same/n0, 1)}%)")
s4 <- s3[!is.na(kick_tid) & team_id != kick_tid]
cli::cli_alert_info("=> PRICED: {nrow(s4)} ({round(100*nrow(s4)/n0, 1)}%)")

cli::cli_h1("VERDICT on the Fix 1 fallback")
cli::cli_alert_info("Fix 1 currently fills (spoils - spoils_priced) = {round(100*(n0-nrow(s4))/n0, 1)}% of spoils at the mean priced value.")
cli::cli_alert_info("Of those, CONTEST TRIPLES are {round(100*n_triple/(n0-nrow(s4)), 1)}% -- and they are ALREADY paid via contest_epv.")
if (n_triple / n0 > 0.02) {
  cli::cli_alert_danger("DOUBLE-COUNT: the fallback pays contest triples a second time.")
  cli::cli_alert_info("Fix: fill only (no-kick + same-team + no-player) spoils, i.e. exclude triples from the fallback base.")
  cli::cli_alert_info("Correct fallback base = {round(100*(n_nokick + n_same + (n0-nrow(s1)))/n0, 1)}% of spoils, not {round(100*(n0-nrow(s4))/n0, 1)}%.")
} else {
  cli::cli_alert_success("Contest triples are under 2% of spoils -- the double-count is immaterial.")
}

cli::cli_h1("could a WIDER scan recover the no-kick group?")
# If those spoils have a kick at 6-10 rows back they are recoverable; if the scan is
# blocked by a possession event, the kick belongs to a different play and widening
# would attribute a spoil to a kick it never touched.
nk <- s2[is.na(.kick_lag)]
blocked <- nk[, mean(!(.l1 %chin% inflight), na.rm = TRUE)]
cli::cli_alert_info("{nrow(nk)} no-kick spoils; {round(100*blocked, 1)}% are BLOCKED at lag 1 by a non-in-flight row")
cli::cli_alert_info("(blocked means the preceding row is a possession event, so a further-back kick")
cli::cli_alert_info(" belongs to a different play -- widening the scan would MIS-attribute, not recover.)")
if (blocked > 0.7) {
  cli::cli_alert_info("Mostly blocked => widening the scan is NOT the fix; the fallback is.")
} else {
  cli::cli_alert_info("A meaningful share is not blocked => widening to 6-8 rows may raise coverage.")
}
print(head(nk[, .N, by = .l1][order(-N)], 8), row.names = FALSE)

cli::cli_alert_success("done")
