# Does the finer LISTED position sharpen the matchup, or just thin the cells?
#
# The matchup weights are currently estimated on `player_position` -- 7 groups.
# Pete's question: does `lineup_position` (21 slots: FB, CHB, HBFL, WR, C, CHF,
# FF, ...) carry more? FB-on-FF and CHB-on-CHF are natural mirrors that the
# 7-group key blurs together, so in principle yes. Against that, 21x21 is 441
# cells estimated from ~29k named duels, so thin cells are the obvious risk.
#
# The two things that decide it:
#   CONCENTRATION -- how much of a winner's debit the top matchup takes. A finer
#     key is only worth having if it points somewhere more specific.
#   SUPPORT -- how many cells carry enough observations to be worth trusting
#     after shrinkage. A key that is sharper on paper and empty in practice is
#     worse than the coarse one.
#
# Reported, not concluded: this measures which key looks better, and the arm
# build afterwards is what actually tests whether it helps.
#
# ~3 min, cached frames only.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_mirror_granularity.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 50) for (l in capture.output(print(utils::head(x, n)))) say(l)

cst <- as.data.table(read_parquet(file.path(OUT_DIR, "epv3_contest_table.parquet")))
pgd <- as.data.table(read_parquet(file.path(OUT_DIR, "epv3_fin_pgd_ship.parquet")))

DUEL <- c("Contested Mark", "Pack Mark (P)", "Pack Mark (O)",
          "Spoil", "Spoil gaining possession", "Spoil ineffective")
d <- cst[out_desc %chin% DUEL & !is.na(loser_pid) & def_win == TRUE]
say("=== Matchup key: 7 position groups against 21 listed slots ===")
say("run at ", format(Sys.time()))
say("named defence-win duels: ", format(nrow(d), big.mark = ","))

analyse <- function(key_col, label) {
  pos <- unique(pgd[!is.na(get(key_col)), .(match_id, player_id, pos = get(key_col))])
  x <- merge(d, pos, by.x = c("match_id", "out_pid"),
             by.y = c("match_id", "player_id"), all.x = TRUE)
  setnames(x, "pos", "wpos")
  x <- merge(x, pos, by.x = c("match_id", "loser_pid"),
             by.y = c("match_id", "player_id"), all.x = TRUE)
  setnames(x, "pos", "lpos")
  x <- x[!is.na(wpos) & !is.na(lpos)]

  tab <- x[, .N, by = .(wpos, lpos)]
  tab[, tot := sum(N), by = wpos]
  tab[, share := N / tot]
  top <- tab[, .(n_winner = tot[1], top_share = round(max(share), 3),
                 top_loser = lpos[which.max(share)],
                 top2_share = round(sum(sort(share, decreasing = TRUE)[1:2]), 3)),
             by = wpos]

  say(""); say("=== ", label, " ===")
  say("keys: ", uniqueN(x$wpos), " winner x ", uniqueN(x$lpos), " loser = ",
      uniqueN(x$wpos) * uniqueN(x$lpos), " cells, from ",
      format(nrow(x), big.mark = ","), " duels")
  say("cells with >= 200 observations (the shrinkage prior): ",
      nrow(tab[N >= 200]), " of ", nrow(tab),
      "  (", round(100 * nrow(tab[N >= 200]) / nrow(tab), 1), "%)")
  say("share of all duels sitting in those cells: ",
      round(100 * sum(tab[N >= 200]$N) / sum(tab$N), 1), "%")
  say("")
  say("concentration -- how much of a winner's debit the top matchup takes:")
  setorder(top, -n_winner)
  say_dt(top, 25)
  say("")
  say("mean top-1 share (weighted by volume): ",
      round(weighted.mean(top$top_share, top$n_winner), 3),
      "   mean top-2: ", round(weighted.mean(top$top2_share, top$n_winner), 3))
  invisible(list(top = top, tab = tab, n = nrow(x)))
}

g <- analyse("position_group", "7 position groups")
l <- analyse("lineup_position", "21 listed lineup slots")

say("")
say("=== SIDE BY SIDE ===")
# NOTE `key` is a reserved argument of data.table() -- naming a column that
# way throws "some columns are not in the data.table".
say_dt(data.table(
  key_used = c("position_group (7)", "lineup_position (21)"),
  duels = c(g$n, l$n),
  cells = c(nrow(g$tab), nrow(l$tab)),
  cells_ge_200 = c(nrow(g$tab[N >= 200]), nrow(l$tab[N >= 200])),
  pct_duels_in_thick_cells = c(
    round(100 * sum(g$tab[N >= 200]$N) / sum(g$tab$N), 1),
    round(100 * sum(l$tab[N >= 200]$N) / sum(l$tab$N), 1)),
  mean_top1_share = c(round(weighted.mean(g$top$top_share, g$top$n_winner), 3),
                      round(weighted.mean(l$top$top_share, l$top$n_winner), 3))), 3)

say("")
say("HOW TO READ IT. A finer key earns its place only if mean_top1_share rises")
say("MATERIALLY while pct_duels_in_thick_cells stays high. If concentration")
say("barely moves and support collapses, the extra granularity is buying noise")
say("-- and shrinkage would pull it back toward the coarse answer anyway, so it")
say("would be complexity for nothing.")
say("")
say("Either way this only says which key LOOKS better. Whether it helps is what")
say("the built arm measures, via the contest channel's conversion to margin.")

close(con)
cat("\nDone\n")
