# Who actually loses a duel, given who won it?
#
# THE IDEA. Chains names the beaten player in 31.3% of genuine duels. That is a
# biased sample but it is a DIRECTLY OBSERVED one, so the conditional
# distribution P(loser position | winner position, zone) can be measured rather
# than assumed -- and then used as the allocation key for the 69% of debits that
# carry no name.
#
# This replaces the flat "spread it across all 22" rule, which is the single
# most destructive thing in the metric: it costs the contest channel 0.924 ->
# 0.384 in conversion to margin. Pete's framing is that the positional mirror
# should take a high share but not all, with the shares over positions summing
# to 100% of the team's debit for that event. That is exactly a conditional
# distribution, so it is estimable.
#
# WHAT TO CHECK BEFORE TRUSTING IT:
#   1. is the matrix actually diagonal? If a key forward's duels are lost by key
#      defenders far more than chance, the mirror idea is real. If it is flat,
#      position tells us nothing and the flat rule was right after all.
#   2. how biased is the named sample? Named rate is 35.7% on spoils and 19.8%
#      on contested marks, so the sample is spoil-heavy -- i.e. tilted toward
#      DEFENCE wins. The conditional is therefore better estimated within
#      outcome type than pooled.
#   3. does it survive the zone split? A duel in the forward 50 is a different
#      matchup from one in the midfield.
#
# ~4 min, cached frames only.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_mirror_weights.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 60) for (l in capture.output(print(utils::head(x, n)))) say(l)

cst <- as.data.table(read_parquet(file.path(OUT_DIR, "epv3_contest_table.parquet")))
pgd <- as.data.table(read_parquet(file.path(OUT_DIR, "epv3_fin_pgd_ship.parquet")))
pos <- unique(pgd[, .(match_id, player_id, pos = position_group)])

DUEL <- c("Contested Mark", "Pack Mark (P)", "Pack Mark (O)",
          "Spoil", "Spoil gaining possession", "Spoil ineffective")
cst[, genuine := out_desc %chin% DUEL]
say("=== Who loses a duel, given who won it? ===")
say("run at ", format(Sys.time()))
say("contests ", format(nrow(cst), big.mark = ","),
    " | genuine duels ", format(sum(cst$genuine), big.mark = ","))

d <- cst[genuine == TRUE]
d <- merge(d, pos, by.x = c("match_id", "out_pid"), by.y = c("match_id", "player_id"), all.x = TRUE)
setnames(d, "pos", "winner_pos")
d <- merge(d, pos, by.x = c("match_id", "loser_pid"), by.y = c("match_id", "player_id"), all.x = TRUE)
setnames(d, "pos", "loser_pos")

named <- d[!is.na(loser_pid) & !is.na(winner_pos) & !is.na(loser_pos)]
say("duels with BOTH positions known: ", format(nrow(named), big.mark = ","),
    " (", round(100 * nrow(named) / nrow(d), 1), "% of duels)")

# ---------------------------------------------------------------------------
say("")
say("=== 1. IS IT DIAGONAL? P(loser position | winner position), row %% ===")
say("Rows are the WINNER's position, columns the LOSER's. A strong diagonal")
say("means the mirror is real.")
tab <- named[, .N, by = .(winner_pos, loser_pos)]
tab[, pct := round(100 * N / sum(N), 1), by = winner_pos]
w <- dcast(tab, winner_pos ~ loser_pos, value.var = "pct", fill = 0)
say_dt(w, 10)
say("")
say("counts behind it:")
say_dt(dcast(tab, winner_pos ~ loser_pos, value.var = "N", fill = 0), 10)

# The baseline: what share would each position take under the FLAT rule?
say("")
say("=== 2. AGAINST THE FLAT BASELINE ===")
say("The flat rule gives every player on the losing team an equal share, so the")
say("expected share of a position is its share of the team's players.")
base <- pgd[!is.na(position_group), .N, by = position_group]
base[, flat_pct := round(100 * N / sum(N), 1)]
setnames(base, "position_group", "loser_pos")
say_dt(base[order(-flat_pct), .(loser_pos, flat_pct)], 10)
say("")
say("LIFT = observed share / flat share. Above 1 means this matchup happens more")
say("than the flat rule assumes; that lift IS the information the flat rule")
say("throws away.")
lift <- merge(tab, base[, .(loser_pos, flat_pct)], by = "loser_pos")
lift[, lift := round(pct / flat_pct, 2)]
say_dt(dcast(lift, winner_pos ~ loser_pos, value.var = "lift", fill = NA), 10)

# ---------------------------------------------------------------------------
say("")
say("=== 3. HOW BIASED IS THE NAMED SAMPLE? ===")
say("Named rate by outcome -- a spoil-heavy sample is tilted toward DEFENCE")
say("wins, so the conditional should be estimated within outcome type.")
say_dt(d[, .(n = .N, named_pct = round(100 * mean(!is.na(loser_pid)), 1)),
         by = out_desc][order(-n)], 8)
say("")
say("--- the same matrix, split by who won ---")
for (side in c(FALSE, TRUE)) {
  s <- named[def_win == side]
  if (nrow(s) < 200) next
  say(""); say("--- ", if (side) "DEFENCE won (spoils etc.)" else "ATTACK retained",
               "  n = ", format(nrow(s), big.mark = ","), " ---")
  t2 <- s[, .N, by = .(winner_pos, loser_pos)]
  t2[, pct := round(100 * N / sum(N), 1), by = winner_pos]
  say_dt(dcast(t2, winner_pos ~ loser_pos, value.var = "pct", fill = 0), 10)
}

# ---------------------------------------------------------------------------
say("")
say("=== 4. DOES ZONE MATTER? ===")
half <- 78
d[, zone := fcase(att_x > half - 50, "forward 50", att_x > 0, "att midfield",
                  att_x > -(half - 50), "def midfield", default = "defensive 50")]
nz <- d[!is.na(loser_pid) & !is.na(winner_pos) & !is.na(loser_pos)]
for (z in c("forward 50", "att midfield", "def midfield", "defensive 50")) {
  s <- nz[zone == z]
  if (nrow(s) < 300) next
  say(""); say("--- ", z, "  n = ", format(nrow(s), big.mark = ","), " ---")
  t3 <- s[, .N, by = .(winner_pos, loser_pos)]
  t3[, pct := round(100 * N / sum(N), 1), by = winner_pos]
  say_dt(dcast(t3, winner_pos ~ loser_pos, value.var = "pct", fill = 0), 10)
}

say("")
say("=== VERDICT INPUTS ===")
say("A usable key needs the matrix to be clearly non-flat (part 1 vs part 2) and")
say("stable enough across outcome and zone (parts 3-4) that one matrix can be")
say("applied. If the diagonal is strong and stable, allocating the unnamed debit")
say("by this conditional replaces the flat smear with a measured one -- Pete's")
say("'high share to the mirror, but not all, summing to 100%'.")

fwrite(dcast(tab, winner_pos ~ loser_pos, value.var = "pct", fill = 0),
       file.path(OUT_DIR, "epv3_mirror_weights.csv"))
saveRDS(tab, file.path(OUT_DIR, "epv3_mirror_weights.rds"))
close(con)
cat("\nDone\n")
