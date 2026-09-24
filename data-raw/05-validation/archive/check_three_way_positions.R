# Three-outcome contest model: what does it do to the forward-defender gap?
# =============================================================================
# The contest-level numbers (check_three_way_contest.R) say the defence's share
# of contest credit falls from 60.1% to 48.3%, because the two-way path was
# discarding 1,414 attacking wins through an asymmetric filter. That predicts
# the gap WIDENS. This measures it on the real ledger instead of predicting it.
#
# Runs build_net_points() twice on 2026 -- once with the constants as shipped
# (v11) and once with the three-way model on -- and compares per-position mean
# Net Points. Everything else is held identical, including the difficulty terms
# source, so the only thing that differs is the contest branch.
#
# NOT a ship gate: one season, contest models fitted in-sample on each arm
# (the arms are treated identically, so the comparison is fair, but neither arm
# is the leak-safe production fit). The ship gate is a full-history rebuild.
#
#   powershell.exe -Command 'Rscript "data-raw/05-validation/check_three_way_positions.R"'
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
say("pbp ", format(nrow(pbp), big.mark = ","), " | chains ",
    format(nrow(ch), big.mark = ","), " | RATING_VINTAGE ", RATING_VINTAGE)

pg <- as.data.table(load_player_game_ratings(SEASON))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
# Modal position per player: position_group here is the PER-MATCH lineup
# listing, so unique() gives 53 of 669 players more than one row and any merge
# on it double-counts them.
pos <- pg[!is.na(position_group), .N, by = .(player_id, position_group)]
data.table::setorder(pos, player_id, -N, position_group)
pos <- pos[, .SD[1], by = player_id][, .(player_id, position_group)]
stopifnot(!anyDuplicated(pos$player_id))

ns <- asNamespace("torp")
set_arm <- function(outcomes, shots) {
  utils::assignInNamespace("EPV3_CONTEST_OUTCOMES", outcomes, ns = "torp")
  utils::assignInNamespace("EPV3_CONTEST_EXCLUDE_SHOTS", shots, ns = "torp")
}
old <- list(o = get("EPV3_CONTEST_OUTCOMES", ns), s = get("EPV3_CONTEST_EXCLUDE_SHOTS", ns))
on.exit(set_arm(old$o, old$s), add = TRUE)

run_arm <- function(label, outcomes, shots) {
  say("\n>>> arm: ", label, "  (EPV3_CONTEST_OUTCOMES = ", outcomes,
      ", EXCLUDE_SHOTS = ", shots, ")")
  set_arm(outcomes, shots)
  tm <- as.data.table(np_difficulty_terms_for_season(SEASON, pbp_data = pbp, chains = ch))
  np <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                         stoppages = "allocate", difficulty_terms = tm)
  np <- as.data.table(np)
  np[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
  np <- merge(np, pos, by = "player_id", all.x = TRUE)
  np[]
}

a <- run_arm("v11 as shipped", "two", FALSE)
b <- run_arm("three-way", "three", TRUE)

say("\n\n=== CONSERVATION: does each arm still allocate the whole margin? ===")
say("Net Points must sum to the match margin per match. mean |gap| in points;")
say("anything above about 1e-6 means the ledger stopped conserving.")
chk <- function(d, lab) {
  s <- d[, .(tot = sum(net_points, na.rm = TRUE)), by = match_id]
  say("  ", lab, ": matches ", nrow(s), "   mean |sum| ",
      signif(mean(abs(s$tot)), 3), "   max |sum| ", signif(max(abs(s$tot)), 3))
}
chk(a, "v11      ")
chk(b, "three-way")
say("  (Net Points is a zero-sum allocation of the margin across BOTH teams,")
say("   so the per-match total is 0, not the margin.)")

say("\n=== per-position mean Net Points, points a game ===")
say("Higher is better for that position. The forward-defender gap is")
say("KEY_FORWARD minus KEY_DEFENDER; smaller is a more balanced rating.")
agg <- function(d, lab) d[!is.na(position_group),
                          .(v = round(mean(net_points, na.rm = TRUE), 3)),
                          by = position_group][, setnames(.SD, "v", lab)]
A <- agg(a, "v11"); B <- agg(b, "three_way")
cmp <- merge(A, B, by = "position_group")
cmp <- cmp[match(ORD, position_group)]
cmp[, change := round(three_way - v11, 3)]
print(cmp)

g <- function(col) cmp[position_group == "KEY_FORWARD"][[col]] -
                   cmp[position_group == "KEY_DEFENDER"][[col]]
say("\n  FWD-DEF gap   v11 ", round(g("v11"), 3),
    "   three-way ", round(g("three_way"), 3),
    "   change ", round(g("three_way") - g("v11"), 3))
say("  Negative change = the gap NARROWED (better for the defender program).")
say("  Positive change = it widened, which the contest-level numbers predicted.")

say("\n=== where the change lands, by ledger column ===")
say("Each column is a component of Net Points and they sum to it exactly.")
say("This says WHICH part of the ledger moved, not just that the total did.")
COLS <- intersect(c("np_direct", "np_defensive_won", "np_contest_won", "np_stoppage",
                    "np_ceded", "np_defensive", "np_team", "np_residual"), names(a))
ca <- a[!is.na(position_group), lapply(.SD, function(x) mean(x, na.rm = TRUE)),
        .SDcols = COLS, by = position_group]
cb <- b[!is.na(position_group), lapply(.SD, function(x) mean(x, na.rm = TRUE)),
        .SDcols = COLS, by = position_group]
d <- merge(ca, cb, by = "position_group", suffixes = c("", ".3"))
for (cc in COLS) d[[paste0("d_", cc)]] <- round(d[[paste0(cc, ".3")]] - d[[cc]], 3)
dd <- d[, c("position_group", paste0("d_", COLS)), with = FALSE]
dd <- dd[match(ORD, position_group)]
print(dd)
say("\nd_ columns are three-way minus v11, points a game. The contest fix should")
say("show up in d_np_contest_won above all; anything large elsewhere means it")
say("moved something it was not supposed to.")
