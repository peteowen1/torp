# Every play type: in chains? in PBP? paid by Net Points? (issues #209, #210)
# =============================================================================
# Pete asked for two things.
#
# 1. More context on the Shape C and Shape D traces -- extra rows either side,
#    who kicked, and who the contest was actually with.
#
# 2. "A table of all play types and whether they're in the chains / pbp / net
#    points algo at the moment and number of rows."
#
# On (2), the distinction that makes the table worth reading: THE LEDGER RUNS ON
# PBP, NOT CHAINS. build_net_points() builds its ledger `l` from pbp rows and
# reads chains only to resolve who ended up with the ball. So a description that
# exists in chains but not in pbp is INVISIBLE to payment -- it can never be
# credited or blamed, no matter how important the act.
#
# That is exactly Pete's point about Mark Fumbled: he does not think it is a
# contest, but he does think someone should be BLAMED for it. Those are two
# different questions with two different answers, and the table separates them.
#
# How the ledger classifies a pbp row (epv_net_points.R:947-968):
#   is_stoppage                         -> "stoppage"  (repriced to a baseline)
#   description in NP_DISPOSAL_DESCS    -> "retained" / "turnover" / "terminal"
#   any other act, next team differs    -> "turnover"  (NP_TURNOVER_ON_ALL_ACTS)
#   any other act, same team            -> "act"       (100% to the actor)
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_play_type_inventory.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
set.seed(20260911)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
ch  <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]
pbp <- as.data.table(load_pbp(SEASON));    pbp[, match_id := as.character(match_id)]
setorder(ch, match_id, display_order)
ch[, rn := .I]
ch[, who := trimws(paste(player_name_given_name, player_name_surname))]
ch[who %chin% c("NA NA", "NA"), who := ""]
ch[, side := fifelse(team_id == home_team_id, home_team_team_abbr, away_team_team_abbr)]
ch[, prev_desc := shift(description, 1), by = match_id]

# ============================================================================
# PART 1 -- wider traces
# ============================================================================
DISPOSALS <- c("Kick", "Handball", "Ground Kick", "Kickin short", "Kickin long",
               "Kickin play on", "Shot At Goal")

trace <- function(i, label, note, back = 9L, fwd = 5L) {
  m <- ch$match_id[i]
  w <- ch[max(1L, i - back):min(nrow(ch), i + fwd)][match_id == m]
  say("\n", strrep("=", 82))
  say(label, "   |   ", note)
  say("match ", m, "  Q", ch$period[i], "  ",
      sprintf("%d:%02d", ch$period_seconds[i] %/% 60, ch$period_seconds[i] %% 60))
  say(strrep("=", 82))
  say(sprintf("  %-5s %-24s %-5s %-21s %5s %5s", "ord", "description", "team", "player", "x", "y"))
  for (k in seq_len(nrow(w))) {
    tag <- ""
    if (w$display_order[k] == ch$display_order[i]) tag <- "  <<< OUTCOME"
    else if (w$description[k] %chin% c("Contest Target")) tag <- "  <-- target named"
    else if (w$description[k] %chin% DISPOSALS &&
             w$display_order[k] < ch$display_order[i]) tag <- "  <-- the kick"
    say(sprintf("  %-5d %-24s %-5s %-21s %5s %5s%s",
                w$display_order[k], substr(w$description[k], 1, 24),
                ifelse(is.na(w$side[k]), "-", w$side[k]),
                substr(ifelse(w$who[k] == "", "-", w$who[k]), 1, 21),
                ifelse(is.na(w$x[k]), "-", as.character(w$x[k])),
                ifelse(is.na(w$y[k]), "-", as.character(w$y[k])), tag))
  }
  # who kicked it, and who the contest was between
  bk <- ch[rn <= i & match_id == m & description %chin% DISPOSALS]
  bk <- if (nrow(bk)) bk[.N] else NULL
  tgt <- ch[rn < i & rn >= i - 4 & match_id == m & description == "Contest Target"]
  say("  ---")
  if (!is.null(bk)) say("  kicked by  : ", bk$who, " (", bk$side, ") from (", bk$x, ", ", bk$y, ")")
  if (nrow(tgt)) say("  target     : ", tgt$who[1], " (", tgt$side[1], ")")
  else           say("  target     : none logged")
  say("  resolved by: ", ch$who[i], " (", ch$side[i], ") at (", ch$x[i], ", ", ch$y[i], ")")
  if (!is.null(bk)) {
    say("  => ", if (!is.na(ch$side[i]) && !is.na(bk$side) && ch$side[i] != bk$side)
      "OPPOSITION won the ball (def_win TRUE)" else "KICKING side kept it (def_win FALSE)")
  }
  say("  in pbp?    : ",
      if (pbp[match_id == m & display_order == ch$display_order[i], .N] > 0)
        "YES -- the ledger sees this row" else "NO -- invisible to Net Points")
}

pick <- function(idx, n) { idx <- idx[!is.na(idx)]
  if (!length(idx)) integer(0) else sort(sample(idx, min(n, length(idx)))) }

say("################ PART 1: WIDER TRACES ################")
for (i in pick(ch[description == "Contested Mark" & prev_desc == "Contested Mark", rn], 2))
  trace(i, "SHAPE C: Contested Mark -> Contested Mark", "1,576 rows -- frame duplicate?")
for (i in pick(ch[description == "Uncontested Mark" & prev_desc == "Kick Inside 50 Result", rn], 2))
  trace(i, "SHAPE D: Kick Inside 50 Result -> Uncontested Mark", "30,069 rows -- contest or reception?")
for (i in pick(ch[description == "Mark Fumbled", rn], 2))
  trace(i, "SHAPE E: Mark Fumbled", "1,840 rows -- blame, even if not a contest?")

# ============================================================================
# PART 2 -- the inventory
# ============================================================================
say("\n\n################ PART 2: PLAY TYPE INVENTORY ################")
cn <- ch[, .(chains_rows = .N), by = description]
pn <- pbp[, .(pbp_rows = .N), by = description]
inv <- merge(cn, pn, by = "description", all = TRUE)
inv[is.na(chains_rows), chains_rows := 0L][is.na(pbp_rows), pbp_rows := 0L]

inv[, np_role := fcase(
  description %chin% NP_STOPPAGE_DESCS,        "STOPPAGE (repriced to baseline)",
  description %chin% NP_DISPOSAL_DESCS,        "DISPOSAL (split disposer/receiver)",
  pbp_rows == 0L,                              "not in PBP -- INVISIBLE to ledger",
  default =                                    "ACT (turnover if next team differs)")]
inv[, contest_pop := fcase(
  description %chin% EPV3_AERIAL_OUT,          "yes (current)",
  description %chin% c("Mark Fumbled", "Mark Dropped", "Dropped Mark"), "PETE: add",
  default =                                    "no")]
inv[, inflight := description %chin% CHAINS_INFLIGHT_DESCS]
setorder(inv, -chains_rows)

show <- function(dt, title) {
  say("\n--- ", title, " (", nrow(dt), " types) ---")
  if (!nrow(dt)) { say("  none"); return(invisible()) }
  say(sprintf("  %-26s %8s %8s  %-34s %s", "play type", "chains", "pbp", "net points role", "contest pop"))
  for (k in seq_len(nrow(dt))) say(sprintf("  %-26s %8s %8s  %-34s %s",
      substr(dt$description[k], 1, 26), format(dt$chains_rows[k], big.mark = ","),
      format(dt$pbp_rows[k], big.mark = ","), dt$np_role[k], dt$contest_pop[k]))
}

show(inv[np_role %like% "DISPOSAL|STOPPAGE"], "A. DISPOSALS AND STOPPAGES -- the ledger's core")
show(inv[description %chin% c("Contested Mark","Uncontested Mark","Mark On Lead",
        "Pack Mark (P)","Pack Mark (O)","Spoil","Spoil gaining possession",
        "Spoil ineffective","Mark Fumbled","Mark Dropped","Dropped Mark",
        "Contest Target","Kick Into F50","Kick Inside 50 Result")],
     "B. MARKS, SPOILS AND CONTEST ANNOTATIONS")
ERR_RX <- "Error|Clanger|Out On Full|Out of Bounds|Free Against|Dispossessed|Knock On|Fumbl|Dropped|Smother|Debit|Rushed|Shark|Tackle"
errish <- inv[grepl(ERR_RX, description)]
show(errish, "C. ERRORS, TURNOVERS AND LOSSES -- Pete: should these be blamed?")
show(inv[chains_rows > 0 & pbp_rows == 0], "D. IN CHAINS BUT NOT IN PBP -- INVISIBLE to Net Points")
show(inv[pbp_rows > 0 & chains_rows == 0], "E. IN PBP BUT NOT IN CHAINS")
show(inv[!description %chin% c(errish$description) & chains_rows > 0 & pbp_rows > 0 &
         !np_role %like% "DISPOSAL|STOPPAGE" &
         !description %chin% c("Contested Mark","Uncontested Mark","Mark On Lead",
           "Pack Mark (P)","Pack Mark (O)","Spoil","Spoil gaining possession",
           "Spoil ineffective","Contest Target","Kick Into F50","Kick Inside 50 Result")],
     "F. EVERYTHING ELSE (in both, paid as ACT)")

say("\n=== totals ===")
say("  distinct play types in chains : ", inv[chains_rows > 0, .N])
say("  distinct play types in pbp    : ", inv[pbp_rows > 0, .N])
say("  in chains but NOT pbp         : ", inv[chains_rows > 0 & pbp_rows == 0, .N],
    "  (", format(inv[chains_rows > 0 & pbp_rows == 0, sum(chains_rows)], big.mark = ","), " rows)")
say("  NP_TURNOVER_ON_ALL_ACTS = ", NP_TURNOVER_ON_ALL_ACTS,
    "  (TRUE => any act losing the ball is blamed, not just kicks/handballs)")
