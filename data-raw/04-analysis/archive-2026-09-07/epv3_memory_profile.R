# Where does the v3 rebuild's memory actually go?
#
# run_ratings_pipeline.R Stage 2 crashed Rscript.exe mid-2024 (Windows
# Application Error, 2026-08-18 15:22) with 4.9 GB free of 31.5 GB. No R-level
# error, which is the signature of the process being killed rather than failing.
#
# The suspect is the log line "Batch loading PBP, player_stats, teams for 6
# seasons": if all six seasons are resident before the per-season loop starts,
# peak memory is ~6x one season's frames even though the loop only ever needs
# one. This measures rather than assumes.
suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

sz <- function(x) format(object.size(x), units = "GB", standard = "SI")
hdr <- function(s) cat("\n==== ", s, " ====\n", sep = "")

gc(reset = TRUE)
base_mb <- sum(gc()[, 2])
cat("R baseline after load_all: ", round(base_mb / 1024, 2), " GB\n", sep = "")

hdr("one season of each input")
pbp1 <- load_pbp(seasons = 2024)
cat("pbp 2024        rows ", format(nrow(pbp1), big.mark = ","), "  size ", sz(pbp1), "\n", sep = "")
st1 <- load_player_stats(seasons = 2024)
cat("player_stats    rows ", format(nrow(st1), big.mark = ","), "  size ", sz(st1), "\n", sep = "")
tm1 <- load_teams(seasons = 2024)
cat("teams           rows ", format(nrow(tm1), big.mark = ","), "  size ", sz(tm1), "\n", sep = "")
ch1 <- load_chains(seasons = 2024)
cat("chains 2024     rows ", format(nrow(ch1), big.mark = ","), "  size ", sz(ch1), "\n", sep = "")

one <- sum(vapply(list(pbp1, st1, tm1, ch1), function(x) as.numeric(object.size(x)), numeric(1)))
cat("\nONE season resident: ", round(one / 1024^3, 2), " GB\n", sep = "")
cat("SIX seasons if all batch-loaded: ~", round(6 * one / 1024^3, 2), " GB\n", sep = "")
cat("(plus whatever create_player_game_data allocates on top)\n")

hdr("peak while building ONE season")
gc(reset = TRUE)
before <- sum(gc()[, 2])
pg <- create_player_game_data(pbp1, st1, tm1, ch1, epv_engine = "v3")
after_max <- sum(gc()[, 6])   # max used since reset
cat("player-game rows: ", format(nrow(pg), big.mark = ","), "\n", sep = "")
cat("peak R memory during one-season build: ", round(after_max / 1024, 2), " GB\n", sep = "")
cat("resident before build: ", round(before / 1024, 2), " GB\n", sep = "")

hdr("verdict")
cat("If one season peaks near ", round(after_max / 1024, 2),
    " GB, six seasons resident plus a build is the crash.\n", sep = "")
cat("The fix is to load per season inside the loop, not batch up front.\n")
