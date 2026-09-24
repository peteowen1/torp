# Row-by-row trace of ONE contest kick, for the design session
# =============================================================================
# The sharpest version of the contest-kick question: Lachie Schultz kicks
# inside 50, his OWN teammate Charlie West takes a contested mark -- a good
# outcome, the row is worth +1.134 -- and Schultz is still charged -1.784.
#
# Shows the passage either side of it, the difficulty model's own terms for the
# kick (p_hat, decision, surprise), and every payment. The decision/surprise
# split is why the kicker is charged on a row that went well, so it has to be
# visible for the rule to be argued about honestly.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_schultz_row_trace.R"'
# Env: TRACE_MATCH, TRACE_ROW, TRACE_PAD
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

MATCH <- Sys.getenv("TRACE_MATCH", "CD_M20260142402")
ROW   <- as.integer(Sys.getenv("TRACE_ROW", "1012"))
PAD   <- as.integer(Sys.getenv("TRACE_PAD", "4"))
LO <- ROW - PAD; HI <- ROW + PAD

SEASON <- as.integer(substr(MATCH, 5, 8))
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]
ps  <- as.data.table(load_player_stats(SEASON, refresh = TRUE))
res <- as.data.table(load_results(SEASON))
tm  <- fread("data-raw/outputs/np_difficulty_terms_2025_2026.csv")
tm[, match_id := as.character(match_id)]
tm  <- tm[substr(match_id, 5, 8) == as.character(SEASON)]

np <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                       stoppages = "allocate", difficulty_terms = tm,
                       return_payments = TRUE)
pay <- as.data.table(attr(np, "np_payments")); pay[, match_id := as.character(match_id)]
ha  <- unique(pbp[!is.na(team), .(match_id, team, home_away)])
pay <- merge(pay, ha, by = c("match_id", "team"), all.x = TRUE)
pay[, own := hm * fifelse(home_away == "Home", 1, -1)]
nm  <- unique(pbp[!is.na(player_id), .(player_id = as.character(player_id), player_name)])
pay[, player_id := as.character(player_id)]
pay <- merge(pay, nm, by = "player_id", all.x = TRUE)
pay[is.na(player_id), player_name := "(team pool)"]

hm_map <- unique(pbp[match_id == MATCH & !is.na(team), .(team, home_away)])
say("match ", MATCH, "   ", paste(hm_map$team, hm_map$home_away, collapse = "  vs  "))
say("(values below are in each player's OWN frame: + is good for him)\n")

# --- the passage, chains as the spine (PBP is a subset of it) ---------------
seg <- ch[match_id == MATCH & display_order >= LO & display_order <= HI,
          .(display_order, description, team_id, player_id = as.character(player_id))]
seg <- merge(seg, nm, by = "player_id", all.x = TRUE)
pv  <- pbp[match_id == MATCH & display_order >= LO & display_order <= HI,
           .(display_order, exp_pts, delta_epv, x, y, pteam = team)]
seg <- merge(seg, pv, by = "display_order", all.x = TRUE)
setorder(seg, display_order)

for (i in seq_len(nrow(seg))) {
  r <- seg[i]
  mark <- if (r$display_order == ROW) "  <<< THE KICK" else ""
  say("--------------------------------------------------------------")
  say("#", r$display_order, "  ", r$description,
      if (!is.na(r$player_name)) paste0("  --  ", r$player_name) else "",
      if (!is.na(r$pteam)) paste0("  [", r$pteam, "]") else "", mark)
  if (!is.na(r$exp_pts))
    say("    exp_pts ", formatC(r$exp_pts, format = "f", digits = 3),
        "   delta_epv ", formatC(r$delta_epv, format = "f", digits = 3),
        "   at x=", round(r$x, 0), " y=", round(r$y, 0))
  else
    say("    (chains-only row: carries no value of its own)")

  t <- tm[match_id == MATCH & display_order == r$display_order]
  if (nrow(t) == 1) {
    say("    difficulty model:  p_hat ", formatC(t$p_hat, format = "f", digits = 3),
        "   decision ", formatC(t$decision, format = "f", digits = 3),
        "   surprise ", formatC(t$surprise, format = "f", digits = 3))
  }
  p <- pay[match_id == MATCH & display_order == r$display_order]
  if (nrow(p)) {
    setorder(p, -own)
    for (j in seq_len(nrow(p)))
      say("      ", formatC(p$role[j], width = -16), " ",
          formatC(p$player_name[j], width = -20), " ",
          formatC(p$own[j], format = "f", digits = 3, width = 8),
          "   [", p$team[j], "]")
    say("      ", formatC("SUM", width = -16), " ",
        formatC("", width = -20), " ",
        formatC(sum(p$own), format = "f", digits = 3, width = 8))
  }
}

say("\n==============================================================")
say("THE QUESTION, on this row:")
say("  Schultz kicks into a contest. The difficulty model says a kick from")
say("  there is unlikely to be retained -- that is the `decision` term, and it")
say("  is charged to him whether or not the contest is won.")
say("  West then WINS it, which is the `surprise`, and he is paid for it.")
say("  So a successful inside-50 kick to a one-on-one still debits the kicker.")
say("  Is that right? If not: should the decision term shrink when the target")
say("  is a genuine contest, or should winning the contest refund the kicker?")
