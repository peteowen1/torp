# Every play type around a contested mark / spoil, with counts (issues #209, #210)
# =============================================================================
# Pete: "What are all the play types associated with a contested mark / spoil
# and how many rows do they have each?"
#
# Straight enumeration off raw chains -- no derived helper, no filtering, no
# inference. The standing rule is to read raw chains for metric work because the
# helpers collapse and filter, and the whole reason this question is live is that
# build_aerial_contests() defines its population from the OUTCOME description
# rather than from what chains actually logs.
#
# Four views:
#   1. the aerial outcome rows themselves, with counts
#   2. what immediately PRECEDES each (the in-flight annotation)
#   3. what immediately FOLLOWS each (how the ball is next touched)
#   4. the full run of rows from the kick to the outcome -- the "shape" of a
#      contest as chains records it
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_contest_play_types.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
ch <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]
setorder(ch, match_id, display_order)
say("chains rows ", SEASON, ": ", format(nrow(ch), big.mark = ","))

AERIAL <- c("Contested Mark", "Pack Mark (P)", "Pack Mark (O)", "Uncontested Mark",
            "Mark On Lead", "Mark Fumbled", "Mark Dropped", "Dropped Mark",
            "Spoil", "Spoil gaining possession", "Spoil ineffective")

say("\n=== 1. every mark/spoil play type in chains, with counts ===")
t1 <- ch[description %chin% AERIAL, .N, by = description][order(-N)]
t1[, pct_of_chains := round(100 * N / nrow(ch), 3)]
t1[, in_EPV3_AERIAL_OUT := description %chin% EPV3_AERIAL_OUT]
t1[, in_EPV3_DUEL_OUT := description %chin% EPV3_DUEL_OUT]
print(t1)
say("\nEPV3_AERIAL_OUT is what build_aerial_contests() uses as the population.")
say("EPV3_DUEL_OUT is the narrower 'duel' set (EPV3_CONTEST_POPULATION is \"",
    EPV3_CONTEST_POPULATION, "\", so the WIDE set is live).")
say("Rows in chains matching EPV3_AERIAL_OUT: ",
    format(ch[description %chin% EPV3_AERIAL_OUT, .N], big.mark = ","))

for (s in c("description", "player_id", "team_id", "x", "y")) {
  ch[, (paste0("p1_", s)) := shift(get(s), 1), by = match_id]
  ch[, (paste0("p2_", s)) := shift(get(s), 2), by = match_id]
  ch[, (paste0("n1_", s)) := shift(get(s), 1, type = "lead"), by = match_id]
}

FOCUS <- c("Contested Mark", "Pack Mark (P)", "Pack Mark (O)", "Spoil",
           "Spoil gaining possession", "Spoil ineffective", "Mark Fumbled",
           "Mark Dropped")

say("\n\n=== 2. what IMMEDIATELY PRECEDES each contested outcome ===")
say("This is the in-flight annotation. `Contest Target` is the row that names")
say("the intended player -- the structural marker of a contest.")
for (f in c("Contested Mark", "Spoil")) {
  say("\n--- ", f, "  (n = ", format(ch[description == f, .N], big.mark = ","), ") ---")
  p <- ch[description == f, .N, by = p1_description][order(-N)]
  p[, pct := round(100 * N / sum(N), 1)]
  print(head(p, 12))
}

say("\n--- the smaller ones, preceding row ---")
for (f in setdiff(FOCUS, c("Contested Mark", "Spoil"))) {
  n <- ch[description == f, .N]
  if (n == 0) next
  p <- ch[description == f, .N, by = p1_description][order(-N)]
  say("  ", formatC(f, width = -26), " n=", formatC(n, width = 5),
      "   top preceding: ",
      paste(sprintf("%s (%d)", head(p$p1_description, 3), head(p$N, 3)), collapse = ", "))
}

say("\n\n=== 3. what IMMEDIATELY FOLLOWS each ===")
for (f in c("Contested Mark", "Spoil")) {
  say("\n--- ", f, " ---")
  p <- ch[description == f, .N, by = n1_description][order(-N)]
  p[, pct := round(100 * N / sum(N), 1)]
  print(head(p, 10))
}

say("\n\n=== 4. is a Contest Target logged, and does the pair share coordinates? ===")
say("Two players contesting the same ball should appear at the same x,y.")
for (f in FOCUS) {
  d <- ch[description == f]
  if (nrow(d) == 0) next
  ct <- d[p1_description %chin% EPV3_CONTEST_TARGET_DESCS, .N]
  ct2 <- d[p2_description %chin% EPV3_CONTEST_TARGET_DESCS, .N]
  samexy <- d[!is.na(p1_x) & x == p1_x & y == p1_y, .N]
  opp <- d[!is.na(p1_team_id) & team_id != p1_team_id, .N]
  say("  ", formatC(f, width = -26), " n=", formatC(nrow(d), width = 5),
      "  ContestTarget at -1: ", formatC(ct, width = 5),
      "  at -2: ", formatC(ct2, width = 5),
      "  same x/y as prev: ", formatC(samexy, width = 5),
      "  prev is opponent: ", formatC(opp, width = 5))
}

say("\n\n=== 5. Contest Target itself: what does it resolve INTO? ===")
say("n = ", format(ch[description %chin% EPV3_CONTEST_TARGET_DESCS, .N], big.mark = ","))
p <- ch[description %chin% EPV3_CONTEST_TARGET_DESCS, .N, by = n1_description][order(-N)]
p[, pct := round(100 * N / sum(N), 1)]
print(head(p, 15))
say("\nEPV3_CONTEST_TARGET_DESCS = ", paste(EPV3_CONTEST_TARGET_DESCS, collapse = ", "))
