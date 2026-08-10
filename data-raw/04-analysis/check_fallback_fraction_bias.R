# Is the 0.349 fallback fraction biased by position?
# ==================================================
# The spoil fold fills genuinely-unpriced spoils at the mean priced value, using a
# GLOBAL fraction: 34.9% of (spoils - spoils_priced) are real gaps, the other 65.1%
# being contest triples already paid via contest_epv in the RECV channel.
#
# That global constant is only safe if the triple share is roughly the same for every
# position. It might well not be: a contest triple is an aerial contest logged as a
# 3-player event, so positions that contest more marks -- key defenders and key
# forwards, precisely the players this fix is aimed at -- should generate more of them.
# If so, the global fraction OVER-fills key defenders (crediting them a fallback for
# spoils they were already paid for elsewhere) and the +38% spread result is partly an
# artifact of my own approximation.
#
# This is the kind of thing that has bitten repeatedly this session, so measure it
# rather than assume the constant travels.

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

sp <- ch[description == "Spoil" & !is.na(player_id)]
cli::cli_alert_info("{nrow(sp)} attributed spoils; position column: {.field player_position}")

# Classify each spoil the way compute_spoil_credit() does.
sp[, is_triple := {
  v <- .prev_desc %chin% target_descs & x == .prev_x & y == .prev_y &
    !is.na(.prev_tid) & team_id != .prev_tid
  v[is.na(v)] <- FALSE
  v
}]
sp[, `:=`(
  .c2 = .l1 %chin% inflight,
  .c3 = .l1 %chin% inflight & .l2 %chin% inflight,
  .c4 = .l1 %chin% inflight & .l2 %chin% inflight & .l3 %chin% inflight,
  .c5 = .l1 %chin% inflight & .l2 %chin% inflight & .l3 %chin% inflight & .l4 %chin% inflight
)]
sp[, .kick_lag := fcase(
  .l1 %chin% kick_descs, 1L,
  .c2 & .l2 %chin% kick_descs, 2L,
  .c3 & .l3 %chin% kick_descs, 3L,
  .c4 & .l4 %chin% kick_descs, 4L,
  .c5 & .l5 %chin% kick_descs, 5L, default = NA_integer_)]
sp[, kick_tid := fcase(.kick_lag == 1L, .t1, .kick_lag == 2L, .t2, .kick_lag == 3L, .t3,
                       .kick_lag == 4L, .t4, .kick_lag == 5L, .t5)]
sp[, class := fcase(
  is_triple, "triple_already_paid",
  is.na(.kick_lag), "gap_no_kick",
  is.na(kick_tid) | team_id == kick_tid, "gap_same_team",
  default = "priced")]

cli::cli_h1("1. overall -- reproduce the 0.349")
tot <- sp[, .N, by = class][order(-N)]
tot[, pct := round(100 * N / sum(N), 1)]
print(tot, row.names = FALSE)
unpriced <- sp[class != "priced", .N]
gaps <- sp[class %in% c("gap_no_kick", "gap_same_team"), .N]
cli::cli_alert_info("global fallback fraction = {round(gaps / unpriced, 4)} (the constant in use is 0.349)")

cli::cli_h1("2. THE TEST -- does it vary by position?")
POS <- "player_position"
if (!POS %in% names(sp)) {
  cli::cli_alert_danger("No {POS} in chains -- cannot test the bias here.")
} else {
  by_pos <- sp[!is.na(get(POS)), .(
    spoils = .N,
    priced = sum(class == "priced"),
    triples = sum(class == "triple_already_paid"),
    gaps = sum(class %in% c("gap_no_kick", "gap_same_team"))
  ), by = c(POS)][spoils >= 200]
  by_pos[, unpriced := triples + gaps]
  by_pos[, frac := round(gaps / unpriced, 3)]
  setorder(by_pos, frac)
  print(by_pos[, .(pos = get(POS), spoils, priced, triples, gaps, frac)], row.names = FALSE)
  rng <- range(by_pos$frac)
  cli::cli_alert_info("fallback fraction ranges {rng[1]} to {rng[2]} across positions (global 0.349)")

  # The players this fix is aimed at.
  defish <- grep("FB|CHB|BP|KEY_DEF", unique(as.character(by_pos[[POS]])), value = TRUE)
  if (length(defish)) {
    dfrac <- by_pos[get(POS) %in% defish, sum(gaps) / sum(unpriced)]
    ofrac <- by_pos[!get(POS) %in% defish, sum(gaps) / sum(unpriced)]
    cli::cli_alert_info("defender-like positions {paste(defish, collapse=', ')}")
    cli::cli_alert_info("fallback fraction: defenders {round(dfrac, 3)} vs others {round(ofrac, 3)}")
    if (dfrac < 0.349 * 0.85) {
      cli::cli_alert_danger("BIASED: defenders have FEWER genuine gaps than the global constant assumes,")
      cli::cli_alert_danger("so a global 0.349 OVER-fills them and inflates the +38% spread result.")
      cli::cli_alert_info("Fix: expose the triple count per player-game from compute_spoil_credit()")
      cli::cli_alert_info("(a `spoils_contested` column) and use it instead of a global fraction.")
    } else if (dfrac > 0.349 * 1.15) {
      cli::cli_alert_danger("BIASED the other way: defenders have MORE genuine gaps than assumed,")
      cli::cli_alert_danger("so the global constant UNDER-fills them and the result is conservative.")
    } else {
      cli::cli_alert_success("Within 15% of the global constant for defenders -- the approximation travels.")
    }
  }
}

cli::cli_h1("3. how much does it matter, in points?")
# Bound the error: the fallback value is ~0.1779 per filled spoil, so a wrong fraction
# misprices by (frac_error x unpriced spoils x 0.1779) per player-game.
cli::cli_alert_info("fallback value per filled spoil ~0.1779; unpriced spoils are {round(100*unpriced/nrow(sp), 1)}% of all spoils")
cli::cli_alert_info("a 0.05 error in the fraction misprices ~{round(0.05 * 0.1779, 4)} points per unpriced spoil")

cli::cli_alert_success("done")
