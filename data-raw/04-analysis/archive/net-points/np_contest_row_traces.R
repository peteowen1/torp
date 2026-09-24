# Real chain sequences, for Pete to say which ones are a contest (#209, #210)
# =============================================================================
# The population question cannot be settled by me. build_aerial_contests()
# defines a contest from the OUTCOME description (EPV3_AERIAL_OUT); chains
# appears to record contests STRUCTURALLY -- a Contest Target row, or an
# opponent logged at the same coordinates, or the outcome doubled across two
# rows. The two disagree badly: 50,050 rows in the population, only 2,497 with a
# logged Contest Target, and 69.8% of the population are receptions whose
# def_win rate is 13.1% against 100% for spoils.
#
# So: print real sequences, row by row, and let Pete say which are contests.
# Nothing is decided in this script. It only shows.
#
# Six shapes, chosen because each stresses a different part of the definition:
#
#   A  Contest Target -> Contested Mark     the clearest contest chains logs
#   B  Contest Target -> Spoil              a contest the defence won
#   C  Contested Mark -> Contested Mark     the SAME outcome on two rows
#                                           (1,576 of 5,336) -- both players?
#   D  Kick Inside 50 Result -> Uncontested Mark   in the population, but is it
#                                           a contest or just a reception?
#   E  Mark Fumbled -> Mark Dropped         EXCLUDED today (1,846 / 1,477 rows),
#                                           but 589 have an opponent adjacent
#   F  Contest Target -> Free For           EXCLUDED today (556 rows) -- a
#                                           contest that ended in a free kick
#
# Each trace runs from the kick that started it through to the next possession,
# showing team, player, position on the ground, and what the engine currently
# does with the row.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_contest_row_traces.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
set.seed(20260911)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
ch <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]
setorder(ch, match_id, display_order)
ch[, rn := .I]
ch[, who := trimws(paste(player_name_given_name, player_name_surname))]
ch[who == "NA NA" | who == "NA", who := ""]
ch[, side := fifelse(team_id == home_team_id, home_team_team_abbr, away_team_team_abbr)]
ch[, prev_desc := shift(description, 1), by = match_id]

# The kick that started this passage: walk back to the nearest disposal row.
KICKS <- c("Kick", "Ground Kick", "Handball", "Kickin short", "Kickin long",
           "Kickin play on", "Shot At Goal")

show_trace <- function(i, label, note) {
  m <- ch$match_id[i]
  lo <- max(1L, i - 6L); hi <- min(nrow(ch), i + 3L)
  w <- ch[lo:hi][match_id == m]
  say("\n", strrep("-", 78))
  say(label)
  say("  ", note)
  say("  match ", m, "   quarter ", ch$period[i], "   ",
      sprintf("%d:%02d", ch$period_seconds[i] %/% 60, ch$period_seconds[i] %% 60))
  say(strrep("-", 78))
  say(sprintf("  %-5s %-26s %-5s %-22s %6s %6s", "ord", "description", "team", "player", "x", "y"))
  for (k in seq_len(nrow(w))) {
    mark <- if (w$display_order[k] == ch$display_order[i]) " <<<" else ""
    say(sprintf("  %-5d %-26s %-5s %-22s %6s %6s%s",
                w$display_order[k], substr(w$description[k], 1, 26),
                ifelse(is.na(w$side[k]), "-", w$side[k]),
                substr(ifelse(w$who[k] == "", "-", w$who[k]), 1, 22),
                ifelse(is.na(w$x[k]), "-", as.character(w$x[k])),
                ifelse(is.na(w$y[k]), "-", as.character(w$y[k])), mark))
  }
  inpop <- ch$description[i] %chin% EPV3_AERIAL_OUT
  say("  -> engine: outcome row is ",
      if (inpop) "IN the contest population" else "NOT in the contest population",
      " (EPV3_AERIAL_OUT)")
}

pick <- function(idx, n = 3) {
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0) return(integer(0))
  sort(sample(idx, min(n, length(idx))))
}

say("=== SIX SHAPES. For each: is this a contest? ===")
say("Ordinary rows above and below are shown for context; '<<<' marks the row")
say("the engine keys on. 'team' is the team the ROW belongs to.")

A <- pick(ch[description == "Contested Mark" & prev_desc == "Contest Target", rn], 3)
for (i in A) show_trace(i, "SHAPE A -- Contest Target -> Contested Mark",
  "1,003 rows. The clearest contest chains logs: a named target, then a contested mark.")

B <- pick(ch[description == "Spoil" & prev_desc == "Contest Target", rn], 3)
for (i in B) show_trace(i, "SHAPE B -- Contest Target -> Spoil",
  "1,727 rows. A named target, then the defence spoils it.")

C <- pick(ch[description == "Contested Mark" & prev_desc == "Contested Mark", rn], 3)
for (i in C) show_trace(i, "SHAPE C -- Contested Mark -> Contested Mark",
  "1,576 rows. The SAME outcome on two rows. Both contestants, or a duplicate?")

D <- pick(ch[description == "Uncontested Mark" & prev_desc == "Kick Inside 50 Result", rn], 3)
for (i in D) show_trace(i, "SHAPE D -- Kick Inside 50 Result -> Uncontested Mark",
  "914 rows share x/y. IN the population today. Contest, or just a reception?")

E <- pick(ch[description == "Mark Fumbled", rn], 2)
for (i in E) show_trace(i, "SHAPE E -- Mark Fumbled (-> Mark Dropped)",
  "1,846 rows, EXCLUDED today. 95 have a Contest Target, 589 an adjacent opponent.")

F <- pick(ch[description == "Free For" & prev_desc == "Contest Target", rn], 2)
for (i in F) show_trace(i, "SHAPE F -- Contest Target -> Free For",
  "556 rows, EXCLUDED today. A contest that ended in a free kick.")

say("\n\n", strrep("=", 78))
say("THE QUESTION FOR EACH SHAPE: is it a contest that should be in the model?")
say(strrep("=", 78))
say("Current population (EPV3_AERIAL_OUT), 2026 chains counts:")
for (d in EPV3_AERIAL_OUT) say("  IN   ", formatC(d, width = -26), format(ch[description == d, .N], big.mark = ","))
for (d in c("Mark Fumbled", "Mark Dropped", "Dropped Mark")) {
  say("  OUT  ", formatC(d, width = -26), format(ch[description == d, .N], big.mark = ","))
}
say("  OUT  ", formatC("Free For (after Contest Target)", width = -26),
    format(ch[description == "Free For" & prev_desc == "Contest Target", .N], big.mark = ","))
