# torp#220: what does the symmetric contest filter do on its own?
# =============================================================================
# The three-way contest model drops the asymmetric filter as part of a much
# bigger change. This measures the filter fix BY ITSELF, with the two-way model
# otherwise untouched, so it can ship independently and be attributed cleanly.
#
# The defect: `csc[def_win == TRUE | out_desc %chin% EPV3_DUEL_OUT]` keeps every
# defensive win unconditionally but an attacking win only when the outcome is in
# the narrower duel list. Its own stated intent, one line above, is symmetric.
#
# Expect the gap to WIDEN. The defence's 60.1% share of contest credit is
# inflated by rows this filter discards, so correcting it costs defenders. That
# is stated up front so a widening result is not mistaken for a regression.
#
# NOT a ship gate: 2026 only, contest models fitted in-sample on both arms
# (identically, so the comparison is fair). The gate is a full-history rebuild.
#
#   powershell.exe -Command 'Rscript "data-raw/05-validation/check_symmetric_filter.R"'
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
say("RATING_VINTAGE ", RATING_VINTAGE, " | NP_CONTEST_FILTER_SYMMETRIC ",
    NP_CONTEST_FILTER_SYMMETRIC, " | EPV3_CONTEST_OUTCOMES ", EPV3_CONTEST_OUTCOMES)

pg <- as.data.table(load_player_game_ratings(SEASON))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
pos <- pg[!is.na(position_group), .N, by = .(player_id, position_group)]
setorder(pos, player_id, -N, position_group)
pos <- pos[, .SD[1], by = player_id][, .(player_id, position_group)]
stopifnot(!anyDuplicated(pos$player_id))

ns <- asNamespace("torp")
old <- get("NP_CONTEST_FILTER_SYMMETRIC", ns)
on.exit(utils::assignInNamespace("NP_CONTEST_FILTER_SYMMETRIC", old, ns = "torp"), add = TRUE)

run <- function(label, symmetric) {
  say("\n>>> arm: ", label, "  (NP_CONTEST_FILTER_SYMMETRIC = ", symmetric, ")")
  utils::assignInNamespace("NP_CONTEST_FILTER_SYMMETRIC", symmetric, ns = "torp")
  tm <- as.data.table(np_difficulty_terms_for_season(SEASON, pbp_data = pbp, chains = ch))
  say("  contested rows in the terms table: ",
      format(sum(tm$contested, na.rm = TRUE), big.mark = ","))
  np <- as.data.table(build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                                       stoppages = "allocate", difficulty_terms = tm))
  np[, player_id := as.character(player_id)]
  merge(np, pos, by = "player_id", all.x = TRUE)
}

a <- run("v11 as shipped (asymmetric)", FALSE)
b <- run("symmetric filter", TRUE)

say("\n=== per-position mean Net Points, points a game ===")
say("Higher is better for that position. The forward-defender gap is")
say("KEY_FORWARD minus KEY_DEFENDER; smaller is more balanced.")
agg <- function(d, lab) d[!is.na(position_group),
                          .(v = round(mean(net_points, na.rm = TRUE), 3)),
                          by = position_group][, setnames(.SD, "v", lab)]
cmp <- merge(agg(a, "asymmetric"), agg(b, "symmetric"), by = "position_group")
cmp <- cmp[match(ORD, position_group)]
cmp[, change := round(symmetric - asymmetric, 3)]
print(cmp)
g <- function(col) cmp[position_group == "KEY_FORWARD"][[col]] -
                   cmp[position_group == "KEY_DEFENDER"][[col]]
say("\n  FWD-DEF gap   asymmetric ", round(g("asymmetric"), 3),
    "   symmetric ", round(g("symmetric"), 3),
    "   change ", round(g("symmetric") - g("asymmetric"), 3))

say("\n=== where it lands, by ledger column ===")
say("d_ columns are symmetric minus asymmetric, points a game. A filter that")
say("only adds attacking-win contests should move np_contest_won and the")
say("channels it feeds, and little else.")
COLS <- intersect(c("np_direct", "np_defensive_won", "np_contest_won", "np_stoppage",
                    "np_ceded", "np_defensive", "np_team", "np_residual"), names(a))
ca <- a[!is.na(position_group), lapply(.SD, mean, na.rm = TRUE), .SDcols = COLS, by = position_group]
cb <- b[!is.na(position_group), lapply(.SD, mean, na.rm = TRUE), .SDcols = COLS, by = position_group]
d <- merge(ca, cb, by = "position_group", suffixes = c("", ".s"))
for (cc in COLS) d[[paste0("d_", cc)]] <- round(d[[paste0(cc, ".s")]] - d[[cc]], 3)
print(d[match(ORD, position_group), c("position_group", paste0("d_", COLS)), with = FALSE])

say("\n=== how much did each player's rating actually move? ===")
say("pct_moved counts player-games changing by more than 1e-9. A filter this")
say("small should move a minority of rows, not all of them.")
m <- merge(a[, .(match_id, player_id, a = net_points)],
           b[, .(match_id, player_id, b = net_points)],
           by = c("match_id", "player_id"))
say("  player-games ", format(nrow(m), big.mark = ","),
    "   pct_moved ", round(100 * mean(abs(m$b - m$a) > 1e-9), 1), "%",
    "   mean |move| ", round(mean(abs(m$b - m$a)), 4),
    "   max |move| ", round(max(abs(m$b - m$a)), 3))
