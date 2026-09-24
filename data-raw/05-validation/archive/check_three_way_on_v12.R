# The three-outcome contest model, measured on top of v12 (not v11)
# =============================================================================
# The earlier figure (+0.235 on the gap) was measured against v11, which still
# had the asymmetric filter. v12 fixed that, so the old number no longer
# describes what shipping the three-way model would now do -- the two changes
# overlapped, because the three-way path skips gate 2 entirely and v12's
# symmetric filter is itself close to a no-op at that point (it re-asks the
# question build_aerial_contests() already answered).
#
# So this measures the MARGINAL effect against the shipped v12 baseline, where
# what remains is almost purely the pricing change plus the shot and ruck-tap
# exclusion.
#
# NOT a ship gate: 2026 only, contest models fitted in-sample on both arms
# (identically, so the comparison is fair). The gate is a full-history rebuild.
#
#   powershell.exe -Command 'Rscript "data-raw/05-validation/check_three_way_on_v12.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
ORD <- c("KEY_FORWARD", "MEDIUM_FORWARD", "MIDFIELDER", "RUCK",
         "MEDIUM_DEFENDER", "KEY_DEFENDER")

pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))
ps  <- as.data.table(load_player_stats(SEASON, refresh = TRUE))
res <- as.data.table(load_results(SEASON))
say("RATING_VINTAGE ", RATING_VINTAGE,
    " | NP_CONTEST_FILTER_SYMMETRIC ", NP_CONTEST_FILTER_SYMMETRIC,
    " | EPV3_CONTEST_OUTCOMES ", EPV3_CONTEST_OUTCOMES)
if (!identical(RATING_VINTAGE, "v12")) {
  say("\n  NOTE: this is meant to run on the v12 baseline. RATING_VINTAGE reads ",
      RATING_VINTAGE, ", so the 'baseline' arm below is not what production ships.")
}

pg <- as.data.table(load_player_game_ratings(SEASON))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
pos <- pg[!is.na(position_group), .N, by = .(player_id, position_group)]
setorder(pos, player_id, -N, position_group)
pos <- pos[, .SD[1], by = player_id][, .(player_id, position_group)]
stopifnot(!anyDuplicated(pos$player_id))

ns <- asNamespace("torp")
old <- list(o = get("EPV3_CONTEST_OUTCOMES", ns), s = get("EPV3_CONTEST_EXCLUDE_SHOTS", ns))
set_arm <- function(o, s) {
  utils::assignInNamespace("EPV3_CONTEST_OUTCOMES", o, ns = "torp")
  utils::assignInNamespace("EPV3_CONTEST_EXCLUDE_SHOTS", s, ns = "torp")
}
on.exit(set_arm(old$o, old$s), add = TRUE)

run <- function(label, outcomes, shots) {
  say("\n>>> arm: ", label, "  (outcomes=", outcomes, ", exclude_shots=", shots, ")")
  set_arm(outcomes, shots)
  tm <- as.data.table(np_difficulty_terms_for_season(SEASON, pbp_data = pbp, chains = ch))
  say("  contested rows: ", format(sum(tm$contested, na.rm = TRUE), big.mark = ","))
  np <- as.data.table(build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                                       stoppages = "allocate", difficulty_terms = tm))
  np[, player_id := as.character(player_id)]
  merge(np, pos, by = "player_id", all.x = TRUE)
}

a <- run("v12 as shipped (two outcomes)", "two", FALSE)
b <- run("three outcomes, shots excluded", "three", TRUE)

say("\n=== per-position mean Net Points, points a game ===")
say("Higher is better for that position. The gap is KEY_FORWARD minus")
say("KEY_DEFENDER; smaller is more balanced. Raw ledger frame, so the LEVELS")
say("are not comparable to the published position-adjusted ones -- the gap is.")
agg <- function(d, lab) d[!is.na(position_group),
                          .(v = round(mean(net_points, na.rm = TRUE), 3)),
                          by = position_group][, setnames(.SD, "v", lab)]
cmp <- merge(agg(a, "v12"), agg(b, "three_way"), by = "position_group")[match(ORD, position_group)]
cmp[, change := round(three_way - v12, 3)]
print(cmp)
g <- function(col) cmp[position_group == "KEY_FORWARD"][[col]] -
                   cmp[position_group == "KEY_DEFENDER"][[col]]
say("\n  FWD-DEF gap   v12 ", round(g("v12"), 3),
    "   three-way ", round(g("three_way"), 3),
    "   change ", round(g("three_way") - g("v12"), 3))
say("  Measured against v11 earlier, the three-way model cost +0.235. That")
say("  figure included the filter fix that v12 has since shipped separately,")
say("  so the number above is the one that describes shipping it NOW.")

say("\n=== how much moves, and where ===")
say("pct_moved counts player-games changing by more than 1e-9.")
m <- merge(a[, .(match_id, player_id, a = net_points)],
           b[, .(match_id, player_id, b = net_points)], by = c("match_id", "player_id"))
say("  player-games ", format(nrow(m), big.mark = ","),
    "   pct_moved ", round(100 * mean(abs(m$b - m$a) > 1e-9), 1), "%",
    "   mean |move| ", round(mean(abs(m$b - m$a)), 4),
    "   max |move| ", round(max(abs(m$b - m$a)), 3))
COLS <- intersect(c("np_direct", "np_defensive_won", "np_contest_won", "np_stoppage",
                    "np_ceded", "np_defensive", "np_team", "np_residual"), names(a))
ca <- a[!is.na(position_group), lapply(.SD, mean, na.rm = TRUE), .SDcols = COLS, by = position_group]
cb <- b[!is.na(position_group), lapply(.SD, mean, na.rm = TRUE), .SDcols = COLS, by = position_group]
d <- merge(ca, cb, by = "position_group", suffixes = c("", ".3"))
for (cc in COLS) d[[paste0("d_", cc)]] <- round(d[[paste0(cc, ".3")]] - d[[cc]], 3)
print(d[match(ORD, position_group), c("position_group", paste0("d_", COLS)), with = FALSE])
say("\nNote np_contest_won counts contests won OFF THE OPPOSITION only; a")
say("same-team contest winner is booked into np_direct (epv_net_points.R:1265).")
