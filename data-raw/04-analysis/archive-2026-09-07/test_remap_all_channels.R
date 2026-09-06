# Does the bench remap help recv, disp and spoil too -- or only hitout?
#
# THE QUESTION BEHIND THIS. Scheme C fixed hitout by celling on "does this
# player ruck", i.e. on the job rather than the starting slot. The obvious next
# move is to do the same for the other three, and it is worth being careful
# about why that is NOT a simple copy:
#
#   * hitout has a BINARY participation split. You ruck or you do not, and
#     `ruck_contests` measures it directly. Everyone in the cell does the same
#     job.
#   * recv, disp and spoil are scored by EVERY player. There is no "did you do
#     this" line to draw, only a continuum of how much and from where.
#   * and celling finely on ACTIVITY would be circular: compare a player with
#     others who had a similar number of receptions and the channel can no
#     longer reward getting more receptions. That is the count-dependence trap
#     wearing a new hat.
#
# But one part of today's work DOES apply to all four, and has never been
# measured for three of them: the bench remap. `INT` is 21% of player-games and
# is not a role, so every channel centred on the starting slot has the same
# contamination -- it was only ever DIAGNOSED on hitout.
#
# So: compare the adjustment-layer view with and without the remap, on all four
# channels. If recv/disp/spoil improve, the cheap fix already helps them and no
# channel-specific key is needed. If they do not move, they need their own
# answer -- or none.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_suite.R")

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "remap_all_channels.txt"), split = TRUE)
cat("=== Does the bench remap help every channel, or only hitout? ===\nrun at",
    format(Sys.time()), "\n")

res <- load_results(TRUE)
none  <- as.data.table(read_parquet(file.path(OUT_DIR, "v2v3_pgd_v2.parquet")))
remap <- as.data.table(read_parquet(file.path(OUT_DIR, "v2_benchremap_pgd.parquet")))
schmc <- as.data.table(read_parquet(file.path(OUT_DIR, "v2_schemec_pgd.parquet")))

cat("\nthree frames, identical raw channels, differing only in the centring key:\n")
cat("  none   lineup slot as-is, INT included\n")
cat("  remap  bench slots replaced by the role actually filled\n")
cat("  schmc  remap + hitout celled on ruck involvement\n")
for (nm in c("epv_recv", "epv_disp", "epv_spoil", "epv_hitout")) {
  cat(sprintf("  raw %s identical across all three: %s\n", nm,
              isTRUE(all.equal(none[[nm]], remap[[nm]])) &&
              isTRUE(all.equal(none[[nm]], schmc[[nm]]))))
}

get_adj <- function(x, lbl) {
  a <- .bm_adj(x)
  if (is.null(a)) return(NULL)
  a[, arm := lbl][]
}
all <- rbindlist(list(get_adj(none, "1 none"), get_adj(remap, "2 remap"),
                      get_adj(schmc, "3 remap+schemeC")))

cat("\n########## cor(adj, raw) -- did the ordering survive? ##########\n")
print(dcast(all, channel ~ arm, value.var = "cor_adj_raw"))
cat("\n########## cor(adj, tog) -- is it a minutes artefact? ##########\n")
print(dcast(all, channel ~ arm, value.var = "cor_adj_tog"))
cat("\n########## top-5 overlap with the raw leaders ##########\n")
print(dcast(all, channel ~ arm, value.var = "top5_overlap"))

cat("\n########## READ ##########\n")
cat("For HITOUT a rise in cor(adj, raw) is good: the channel is ruck-exclusive,\n")
cat("so everyone in it does the same job and the ordering should track output.\n")
cat("\nFor RECV, DISP and SPOIL it is NOT that simple. Every position scores in\n")
cat("them at different rates, so the adjustment is SUPPOSED to reorder -- a\n")
cat("rise in cor(adj, raw) there could mean the positional correction is doing\n")
cat("LESS, which is not obviously an improvement. Judge those on cor(adj, tog)\n")
cat("moving toward zero, which is unambiguous: no channel should be a function\n")
cat("of how many minutes a player was on the ground.\n")

cat("\n########## HOW MUCH OF EACH CHANNEL SITS IN THE BENCH CELL ##########\n")
cat("The remap can only help a channel to the extent its value is scored by\n")
cat("players who START on the bench.\n\n")
S <- max(none$season, na.rm = TRUE)
b <- none[season == S & lineup_position %chin% c("INT", "SUB", "EMERG")]
tot <- none[season == S]
print(rbindlist(lapply(c("epv_recv", "epv_disp", "epv_spoil", "epv_hitout"), function(c)
  data.table(channel = c,
             pct_of_value_from_bench_starters =
               round(100 * sum(abs(b[[c]]), na.rm = TRUE) / sum(abs(tot[[c]]), na.rm = TRUE), 1)))))

saveRDS(all, file.path(OUT_DIR, "remap_all_channels.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
