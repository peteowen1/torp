# Where does a ratings rebuild actually spend its time?
#
# I asserted twice that the cost sat in one place and was wrong both times:
# first that .prepare_final_dataframe's 169 calls dominated, then that the batch
# stage did. Reading the code says .prepare_final_dataframe joins ~800 rows
# against ~700 per call, which should be milliseconds. So neither story fits a
# 12-minute build and the honest move is to instrument it.
#
# This matters beyond my gate: .build_epr_season() runs in the DAILY production
# ratings pipeline, so anything quadratic or repeated in here costs every night.
#
# Read-only. Times one season with per-stage instrumentation, then the pieces
# inside the per-round loop.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_profile_ratings.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

tic <- function() Sys.time()
toc <- function(t0) as.numeric(difftime(Sys.time(), t0, units = "secs"))

say("=== Profiling a ratings rebuild ===")

t0 <- tic()
pgd <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_player_game_v2.parquet")))
say(sprintf("%-46s %7.1fs", "read player_game parquet", toc(t0)))

t0 <- tic(); pgd <- adjust_epv_for_opponents(pgd)
t_oadj <- toc(t0); say(sprintf("%-46s %7.1fs", "adjust_epv_for_opponents()", t_oadj))

t0 <- tic(); if (isTRUE(EPV_LEVEL_CENTRE)) pgd <- centre_epv_by_position(pgd)
t_centre <- toc(t0); say(sprintf("%-46s %7.1fs", "centre_epv_by_position()", t_centre))

t0 <- tic(); fixtures <- load_fixtures(TRUE)
say(sprintf("%-46s %7.1fs", "load_fixtures()", toc(t0)))

t0 <- tic(); stat_ratings <- get_player_stat_ratings(current = FALSE)
say(sprintf("%-46s %7.1fs", "get_player_stat_ratings()", toc(t0)))

# ---- one season, stage by stage --------------------------------------------
YEAR <- 2025L
rounds <- 0:28
say("")
say("--- one season (", YEAR, ", ", length(rounds), " requested rounds) ---")

t0 <- tic(); plyr_tm_df <- load_player_details(YEAR)
t_pd <- toc(t0)
say(sprintf("%-46s %7.1fs   (x6 seasons = %.0fs)", "load_player_details()", t_pd, t_pd * 6))

fix_dt <- as.data.table(fixtures)
fix_dates <- fix_dt[season == YEAR & round_number %in% rounds,
                    .(date_val = lubridate::as_date(min(utc_start_time))),
                    by = .(round_val = round_number)]
round_info <- data.table(round_val = rounds,
                         match_ref = paste0("CD_M", YEAR, "014", sprintf("%02d", rounds)))
round_info <- round_info[fix_dates, on = "round_val", nomatch = NULL]
say("rounds resolved: ", nrow(round_info))

t0 <- tic(); batch <- calculate_epr_stats_batch(pgd, round_info)
t_batch <- toc(t0)
say(sprintf("%-46s %7.1fs   (x6 = %.0fs)", "calculate_epr_stats_batch()", t_batch, t_batch * 6))
say("  batch rows: ", format(nrow(batch), big.mark = ","))

# attach the stat-rating columns exactly as .build_epr_season does
batch[, `:=`(pred_tog = NA_real_, pred_selection = NA_real_, pred_cond_tog = NA_real_)]
sr <- as.data.table(stat_ratings)
batch[sr, `:=`(pred_selection = i.squad_selection_rating,
               pred_cond_tog = i.cond_tog_rating), on = "player_id"]
batch[is.na(pred_selection), pred_selection := 0]
batch[is.na(pred_cond_tog), pred_cond_tog := 0]
batch[, pred_tog := pred_selection * pred_cond_tog]

fix_summary <- fixtures |>
  dplyr::group_by(season = season, round = round_number) |>
  dplyr::summarise(ref_date = lubridate::as_date(min(utc_start_time)), .groups = "drop")

t0 <- tic()
per_round <- numeric(nrow(round_info))
for (i in seq_len(nrow(round_info))) {
  rv <- round_info$round_val[i]
  s <- tic()
  rd <- batch[round_val == rv]
  fd <- .prepare_final_dataframe(plyr_tm_df, rd, YEAR, rv, fixtures, fix_summary = fix_summary)
  per_round[i] <- toc(s)
}
t_loop <- toc(t0)
say(sprintf("%-46s %7.1fs   (x6 = %.0fs)", "per-round loop TOTAL", t_loop, t_loop * 6))
say(sprintf("  per call: mean %.3fs  max %.3fs  n = %d",
            mean(per_round), max(per_round), length(per_round)))

say("")
say("--- inside one .prepare_final_dataframe call ---")
rv <- round_info$round_val[ceiling(nrow(round_info) / 2)]
rd <- batch[round_val == rv]
say("round ", rv, " | batch rows for it: ", nrow(rd),
    " | player details rows: ", nrow(plyr_tm_df))
t0 <- tic(); for (k in 1:10) invisible(.prepare_final_dataframe(
  plyr_tm_df, rd, YEAR, rv, fixtures, fix_summary = fix_summary))
say(sprintf("%-46s %7.3fs", "10 calls, so per call", toc(t0) / 10))

# Is the batch subset the cost rather than the join? batch is ~24x bigger than
# one round, and `batch[round_val == rv]` scans it every iteration.
t0 <- tic(); for (k in 1:50) invisible(batch[round_val == rv])
say(sprintf("%-46s %7.4fs", "batch[round_val == rv] subset, per call", toc(t0) / 50))
say("  batch is ", format(nrow(batch), big.mark = ","), " rows and is scanned once per round.")
say("  A key or split() would make this a lookup instead of a scan.")

say("")
say("=== WHERE THE TIME GOES (one season) ===")
tot <- t_pd + t_batch + t_loop
say(sprintf("  load_player_details        %6.1fs  %4.1f%%", t_pd, 100 * t_pd / tot))
say(sprintf("  calculate_epr_stats_batch  %6.1fs  %4.1f%%", t_batch, 100 * t_batch / tot))
say(sprintf("  per-round loop             %6.1fs  %4.1f%%", t_loop, 100 * t_loop / tot))
say(sprintf("  season total               %6.1fs", tot))
say("")
say(sprintf("  ONE-OFF per rebuild: adjust_epv_for_opponents %.1fs + centre %.1fs",
            t_oadj, t_centre))
say(sprintf("  PROJECTED 6-season rebuild: %.0fs = %.1f min",
            tot * 6 + t_oadj + t_centre, (tot * 6 + t_oadj + t_centre) / 60))

close(con)
cat("\nWrote ", OUT, "\n")
