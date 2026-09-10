# What does chains ACTUALLY call a contest? (issues #209, #210)
# =============================================================================
# Pete: "Isn't a contest defined by if there are two rows of players in the
# chains? Like there's a play type called marking contest or something? I think
# you're defining it wrong and that could be causing the issues?"
#
# Worth taking seriously rather than defending. build_aerial_contests() defines
# the population by the OUTCOME description --
#
#   EPV3_AERIAL_OUT <- c("Contested Mark", "Uncontested Mark", "Mark On Lead",
#                        "Pack Mark (P)", "Pack Mark (O)", "Spoil",
#                        "Spoil gaining possession", "Spoil ineffective")
#
# -- i.e. it INFERS "a contest happened" from how the ball was resolved. If
# chains instead RECORDS a contest as its own event, with the participants on
# their own rows, then the right population is the recorded one and the inferred
# one is a proxy that will be wrong in both directions: contests chains logged
# that we miss, and non-contests we count.
#
# That would matter a lot. A population defined by the outcome is exactly the
# shape that produces the two things already measured and unexplained:
#   - every surviving spoil is def_win == TRUE (22.5% of rows, deterministic)
#   - the model is overconfident from 0.8 to 0.99 by up to 13.6 points
#
# So: print the vocabulary. No inference, no derived helper -- raw chains, every
# description, with counts, and the per-chain structure around a marking event.
# The standing rule is to read raw chains for metric work, because the helpers
# collapse and filter.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_chains_contest_vocabulary.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
ch <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]
say("chains rows: ", format(nrow(ch), big.mark = ","))
say("columns: ", paste(names(ch), collapse = ", "))

say("\n=== EVERY description in chains, by frequency ===")
tab <- ch[, .N, by = description][order(-N)]
tab[, pct := round(100 * N / nrow(ch), 2)]
print(tab, nrows = 200)

say("\n=== anything naming a contest, a mark, a spoil or a duel ===")
print(tab[grepl("contest|mark|spoil|pack|ruck|hit|tackle|bounce|throw|ball up",
                description, ignore.case = TRUE)][order(-N)], nrows = 100)

say("\n=== what the engine currently treats as the contest population ===")
say("EPV3_AERIAL_OUT (outcome-inferred): ", paste(EPV3_AERIAL_OUT, collapse = ", "))
say("EPV3_DUEL_OUT: ", paste(EPV3_DUEL_OUT, collapse = ", "))
say("EPV3_CONTEST_TARGET_DESCS: ", paste(EPV3_CONTEST_TARGET_DESCS, collapse = ", "))
say("CHAINS_INFLIGHT_DESCS: ", paste(CHAINS_INFLIGHT_DESCS, collapse = ", "))

say("\n=== does chains put the PARTICIPANTS on their own rows? ===")
say("Pete's hypothesis. If a marking contest is logged as an event with each")
say("player on a row, consecutive rows should share a display_order neighbourhood")
say("and name different players at the same coordinates.")
setorder(ch, match_id, display_order)
ch[, `:=`(nx_desc = shift(description, 1, type = "lead"),
          nx_pid = shift(player_id, 1, type = "lead"),
          nx_tid = shift(team_id, 1, type = "lead"),
          nx_x = shift(x, 1, type = "lead"), nx_y = shift(y, 1, type = "lead"))]
marks <- ch[grepl("Mark|Spoil", description)]
say("rows whose description mentions Mark or Spoil: ", format(nrow(marks), big.mark = ","))
say("\nwhat FOLLOWS each such row (top 15):")
print(marks[, .N, by = .(description, nx_desc)][order(-N)][1:15])

say("\n=== same-coordinate consecutive pairs: the signature of a logged contest ===")
say("If two players contest the same ball, chains would plausibly log them at the")
say("SAME x,y on adjacent rows. Counting exact coordinate matches:")
same <- ch[!is.na(nx_x) & x == nx_x & y == nx_y & !is.na(player_id) & !is.na(nx_pid) &
             player_id != nx_pid]
say("  adjacent rows, same x/y, different players: ", format(nrow(same), big.mark = ","),
    " (", round(100 * nrow(same) / nrow(ch), 2), "% of chains)")
if (nrow(same) > 0) {
  say("  of those, how many are opposing teams: ",
      round(100 * same[team_id != nx_tid, .N] / nrow(same), 1), "%")
  say("\n  the description pairs that do this (top 20):")
  print(same[, .N, by = .(description, nx_desc)][order(-N)][1:20])
}
