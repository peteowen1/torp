# Before/after top 40, for the ship decision on the three centring fixes.
#
# Both arms use IDENTICAL rating constants -- the only difference is the
# player-game frame underneath, so every movement here comes from the centring
# work and nothing else:
#
#   before  production today: lineup slot as the cell, INT included
#   after   bench remap + hitout celled on ruck involvement + blended reference
#
# What to look for, and none of it is a number the panel can give you:
#   * do the names at the top look right
#   * is every position still represented
#   * are the big movers explicable, or does someone appear from nowhere the way
#     Sean Darcy did at 5th and Mason Cox at 79th

suppressMessages({
  library(dplyr); library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "centring_leaderboard.txt"), split = TRUE)
cat("=== Before / after the centring fixes: top 40 ===\nrun at", format(Sys.time()), "\n")

set_const <- function(l) for (nm in names(l)) assignInNamespace(nm, l[[nm]], ns = "torp")
with_const <- function(l, expr) {
  old <- lapply(names(l), function(nm) get(nm, envir = asNamespace("torp")))
  names(old) <- names(l); set_const(l); on.exit(set_const(old), add = TRUE); force(expr)
}
V2 <- list(EPV_ENGINE = "v2", EPV3_CHANNELS = 3L,
           EPV3_SUB_SCALE = c(cont_aerial = 1, cont_stop = 1),
           EPV3_STOP_ZERO_SUM = FALSE,
           EPV_STANDARDISE_CHANNELS = c("recv", "disp", "spoil"),
           EPV3_POINTS_SCALE = c(recv = 1, disp = 1, cont_aerial = 1, cont_stop = 1),
           EPV_PER_CHANNEL_POINTS_SCALE = FALSE, EPV_POINTS_SCALE = 0.919,
           EPR_PRIOR_RATE_RECV = -0.7 * 0.919, EPR_PRIOR_RATE_DISP = -0.7 * 0.919,
           EPR_PRIOR_RATE_SPOIL = -0.3 * 0.919, EPR_PRIOR_RATE_HITOUT = -0.3 * 0.919,
           EPR_PRIOR_GAMES_RECV = 3, EPR_PRIOR_GAMES_DISP = 3,
           EPR_PRIOR_GAMES_SPOIL = 3, EPR_PRIOR_GAMES_HITOUT = 3)

teams <- load_teams(TRUE)
shared_stat_ratings <- get_player_stat_ratings(current = FALSE)
shared_fixtures <- load_fixtures(TRUE)
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)),
                   error = function(e) NULL)

build <- function(pgd_file, tag) {
  f <- file.path(OUT_DIR, paste0("centring_rt_", tag, ".parquet"))
  if (file.exists(f)) { cli::cli_alert_info("Reusing ratings {tag}")
    return(as.data.table(read_parquet(f))) }
  pgd <- as.data.table(read_parquet(file.path(OUT_DIR, pgd_file)))
  setattr(pgd, "epv_engine", "v2")
  d <- adjust_epv_for_opponents(copy(pgd)); setattr(d, "epv_engine", "v2")
  if (isTRUE(EPV_LEVEL_CENTRE)) d <- centre_epv_by_position(d)
  out <- rbindlist(lapply(sort(unique(d$season)), function(s) {
    sr <- if (s >= 2024) 0 else 1
    mr <- if (s == get_afl_season()) get_afl_week(type = "next") else 28
    torp:::.build_epr_season(s, sr:mr, d, shared_stat_ratings, shared_fixtures)
  }), use.names = TRUE, fill = TRUE)
  if (isTRUE(EPR_POSITION_CENTRE)) out <- centre_epr_by_position(out)
  if (!is.null(psr_df) && nrow(psr_df) > 0 && "psr" %in% names(psr_df)) out <- calculate_torp(out, psr_df)
  out <- as.data.table(out); write_parquet(out, f); out
}
before <- with_const(V2, build("v2v3_pgd_v2.parquet",  "before"))
after  <- with_const(V2, build("v2_blend_pgd.parquet", "after"))

latest <- function(x) {
  s <- max(x$season, na.rm = TRUE)
  y <- x[season == s][, .SD[which.max(round)], by = player_id]
  y[is.finite(epr)]
}
b <- latest(before); a <- latest(after)
m <- merge(b[, .(player_id, player_name, position_group, epr_b = epr,
                 hb = epr_hitout, sb = epr_spoil)],
           a[, .(player_id, epr_a = epr, ha = epr_hitout, sa = epr_spoil)], by = "player_id")
m[, `:=`(rk_b = frank(-epr_b), rk_a = frank(-epr_a))]
m[, move := as.integer(rk_b - rk_a)]
cat("\nplayers rated:", nrow(m), "| season", max(a$season, na.rm = TRUE), "\n")

cat("\n########## TOP 40 AFTER ##########\n")
setorder(m, rk_a)
print(m[1:40, .(rk = as.integer(rk_a), player = player_name,
                pos = substr(position_group, 1, 14),
                epr = round(epr_a, 2), was = as.integer(rk_b), move)], nrows = 45)

cat("\n########## POSITION MIX IN THE TOP 40 ##########\n")
pm <- merge(m[rk_b <= 40, .(before = .N), by = position_group],
            m[rk_a <= 40, .(after = .N), by = position_group],
            by = "position_group", all = TRUE)
pm[is.na(before), before := 0L][is.na(after), after := 0L]
print(pm[order(-after)])

cat("\n########## BIGGEST MOVERS ##########\n")
mv <- m[rk_b <= 150 | rk_a <= 150]
setorder(mv, -move); cat("\nrisen:\n")
print(mv[1:8, .(player = player_name, pos = substr(position_group, 1, 14),
                was = as.integer(rk_b), now = as.integer(rk_a),
                hitout = round(ha, 2), spoil = round(sa, 2))])
setorder(mv, move); cat("\nfallen:\n")
print(mv[1:8, .(player = player_name, pos = substr(position_group, 1, 14),
                was = as.integer(rk_b), now = as.integer(rk_a),
                hitout = round(ha, 2), spoil = round(sa, 2))])

cat(sprintf("\nSpearman %.4f | mean |rank change| %.1f of %d players\n",
            cor(m$rk_b, m$rk_a, method = "spearman"), mean(abs(m$move)), nrow(m)))

cat("\n########## THE RUCKS SPECIFICALLY ##########\n")
setorder(m, -ha)
print(m[1:10, .(player = player_name, pos = substr(position_group, 1, 14),
                hitout_before = round(hb, 2), hitout_after = round(ha, 2),
                epr_rank = as.integer(rk_a))])

saveRDS(m, file.path(OUT_DIR, "centring_leaderboard.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
