#!/usr/bin/env Rscript
# EPR layer for v4: raw net points, prior of a few games toward the channel mean
# ==============================================================================
# Pete, 2026-09-06: "if we're guessing Chad Warner's next EPV we wouldn't guess
# 2.3, we'd guess about 6." Measured on the six-season v4 frame: with 25+
# weighted games, 91% of a player's decay-weighted raw mean carries into his
# next game (81% of the above-position mean); the best prior is ~4 games toward
# the league mean for raw net points, ~10 games at -0.25 above position. v3's
# priors (14/24/11 games at about -1) were fitted for v3's units and over-shrink
# v4 by a third. This script builds the v4 rating under the measured priors on
# the cached frames and gates it beside v3 and the v4-through-v3-layer arm.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/run_epr_gate_v4layer.R"'
suppressMessages({ library(data.table); library(arrow); devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE) })
options(torp.local_data_dir = NA)
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_epr_gate.R")
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_suite.R")
OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "epr_gate_v4layer.txt"), split = TRUE)
cat("=== EPR layer for v4: measured priors, raw points ===\nrun at", format(Sys.time()), "\n")
set_const <- function(...) { vals <- list(...); for (nm in names(vals)) assignInNamespace(nm, vals[[nm]], ns = "torp") }
rd <- function(f) as.data.table(read_parquet(file.path(OUT_DIR, f)))
pgd3 <- rd("v3v4_pgd_v3.parquet"); setattr(pgd3, "epv_engine", "v3")
pgd4 <- rd("v3v4_pgd_v4.parquet"); setattr(pgd4, "epv_engine", "v4")
rt3 <- rd("v3v4_rt_v3.parquet"); rt4 <- rd("v3v4_rt_v4.parquet")
res <- as.data.table(load_results(TRUE))
shared_stat_ratings <- get_player_stat_ratings(current = FALSE); shared_fixtures <- load_fixtures(TRUE)
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)), error = function(e) NULL)

# the variant: adjusted columns ARE the raw channels (no per-80, no centring,
# no standardising); priors of K games toward each channel's league mean
K <- as.numeric(Sys.getenv("NP_PRIOR_GAMES", unset = "4"))
p4 <- copy(pgd4)
p4[, `:=`(epv_recv_adj = epv_recv, epv_disp_adj = epv_disp, epv_spoil_adj = epv_spoil, epv_hitout_adj = 0, epv_adj = epv)]
mu <- p4[, .(recv = mean(epv_recv), disp = mean(epv_disp), spoil = mean(epv_spoil))]
cat(sprintf("channel means (prior rates): won %.3f  own %.3f  pools %.3f; prior games %g\n", mu$recv, mu$disp, mu$spoil, K))
set_const(EPV_ENGINE = "v4", EPV_LEVEL_CENTRE = FALSE, EPR_POSITION_CENTRE = FALSE,
          EPR_PRIOR_GAMES_RECV = K, EPR_PRIOR_GAMES_DISP = K, EPR_PRIOR_GAMES_SPOIL = K, EPR_PRIOR_GAMES_HITOUT = K,
          EPR_PRIOR_RATE_RECV = mu$recv, EPR_PRIOR_RATE_DISP = mu$disp, EPR_PRIOR_RATE_SPOIL = mu$spoil, EPR_PRIOR_RATE_HITOUT = 0,
          EPR_LOADING_DEFAULT = 1)
d <- p4  # no opponent adjustment for the variant: the raw ledger is the object being rated
setattr(d, "epv_engine", "v4")
rt4r <- rbindlist(lapply(sort(unique(d$season)), function(s) {
  sr <- if (s >= 2024) 0 else 1; mr <- if (s == get_afl_season()) get_afl_week(type = "next") else 28
  torp:::.build_epr_season(s, sr:mr, d, shared_stat_ratings, shared_fixtures)
}), use.names = TRUE, fill = TRUE)
if (!is.null(psr_df) && nrow(psr_df) > 0 && "psr" %in% names(psr_df)) rt4r <- calculate_torp(rt4r, psr_df)
rt4r <- as.data.table(rt4r); write_parquet(rt4r, file.path(OUT_DIR, paste0("v3v4_rt_v4raw_k", K, ".parquet")))
set_const(EPV_ENGINE = "v3")

snap <- function(rt) { rt <- as.data.table(rt)[!is.na(epr)]; last <- rt[season == max(season), max(round)]; rt[season == max(season) & round == last] }
s <- snap(rt4r)
cat("\nTOP 20 by EPR, v4 raw-points layer (own / won / pools):\n")
print(s[order(-epr)][1:20, .(player_name, position_group, epr = round(epr, 2), own = round(epr_disp, 2), won = round(epr_recv, 2), pools = round(epr_spoil, 2))])
cat("\nEPR by position group (mean, top 40 count):\n"); print(s[, .(mean = round(mean(epr), 2), n_top40 = sum(rank(-epr) <= 40)), by = position_group][order(-mean)])

# does it guess next games? player-level: rating before the round vs the game's net points
lu <- .bm_lineup(p4, rt4r, "epr")
cat(sprintf("\nnext-game check: %d player-games; cor(rating, next net points) = %.3f; slope = %.3f (1.0 = a calibrated guess)\n",
            nrow(lu), cor(lu$v, p4[lu, on = .(match_id, player_id)]$epv), coef(lm(p4[lu, on = .(match_id, player_id)]$epv ~ lu$v))[2]))
lu3 <- .bm_lineup(pgd4, rt4, "epr")
cat(sprintf("   same for v4 through v3's layer: cor = %.3f; slope = %.3f\n", cor(lu3$v, pgd4[lu3, on = .(match_id, player_id)]$epv_adj), coef(lm(pgd4[lu3, on = .(match_id, player_id)]$epv_adj ~ lu3$v))[2]))

cat("\n==== FAST EPR GATE ====\n")
g3 <- bm_epr_gate(pgd3, rt3, res, "v3 production"); g4 <- bm_epr_gate(pgd4, rt4, res, "v4 via v3 layer"); g4r <- bm_epr_gate(p4, rt4r, res, paste0("v4 raw layer k", K))
print(g4r); cat("\n-- v3 vs v4 raw layer --\n"); compare_epr_gates(g3, g4r); cat("\n-- v4 via v3 layer vs v4 raw layer --\n"); compare_epr_gates(g4, g4r)
cat("\n==== FACE VALIDITY, v3 -> v4 raw layer ====\n"); print(tryCatch(face_validity(snap(rt3), s), error = function(e) conditionMessage(e)))
saveRDS(list(g3 = g3, g4 = g4, g4r = g4r), file.path(OUT_DIR, paste0("epr_gate_v4layer_k", K, ".rds")))
cat("\ndone", format(Sys.time()), "\n"); sink()
