# Where does spoil's +0.197 TOG correlation come from?
#
# CONTEXT. Every other channel sits near zero on cor(adj, tog): recv +0.009,
# disp +0.020, hitout -0.147 against a raw -0.301. spoil is +0.197 and it got
# there BECAUSE of the bench remap -- it read +0.025 before. Doubling the cell
# sizes changed it by 0.002, so cell-size noise is ruled out.
#
# TWO DIFFERENCES FROM HITOUT, and the diagnosis has to distinguish them rather
# than assume by analogy -- assuming by analogy is the error this review already
# records twice:
#
#   1. spoil IS standardised (divided by the cell sd); hitout is not.
#   2. spoil is an eight-component box aggregate --
#        spoils, tackles, pressure_acts, def_half_pressure_acts, intercepts,
#        one_percenters, rebound50s, frees_against
#      -- and THREE of those weights are negative, so components partly cancel
#      and the net can be either sign for a given player.
#
# SCOPE, stated because it changes how much this is worth. Under v3 `epv_spoil`
# is an alias for the aerial CONTEST channel computed from chains, a completely
# different quantity, so this specific defect is v2-only. It still matters
# because v3 costs +0.367 dMAE and is unapproved, so v2 is what ships -- and
# because the centring METHOD is shared, so if standardisation is the culprit
# the finding transfers to v3 regardless.
#
# Reconstruction check runs FIRST and the rest is not to be read if it fails.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "spoil_tog.txt"), split = TRUE)
cat("=== Where does spoil's TOG correlation come from? ===\nrun at",
    format(Sys.time()), "\n")

d <- as.data.table(read_parquet(file.path(OUT_DIR, "v2_blend_pgd.parquet")))
d[, tog_safe := pmax(fcoalesce(time_on_ground_percentage / 100, 0.1), 0.1)]
d[, p80 := epv_spoil / tog_safe]
d[, rk := torp:::.remap_bench_role(as.character(lineup_position), player_id,
                                   season, position_group)]
S <- max(d$season, na.rm = TRUE)
cat("spoil standardised:", "spoil" %in% EPV_STANDARDISE_CHANNELS, "\n")

cat("\n########## 0. RECONSTRUCTION CHECK ##########\n")
cells <- d[, .(m = weighted.mean(p80, tog_safe, na.rm = TRUE),
               s = torp:::.wtd_sd(p80, tog_safe)), by = .(cell = rk)]
pooled <- torp:::.wtd_sd(d$p80, d$tog_safe)
g <- merge(d[, .(player_name, season, cell = rk, tog_safe, p80, act = epv_spoil_adj)],
           cells, by = "cell")
g[, recon := (p80 - m) / s * pooled * tog_safe]
imp <- g[season == S, .(implied = sum(act, na.rm = TRUE) / sum((p80 - m) / s * tog_safe, na.rm = TRUE),
                        n = .N), by = player_name][n >= 10]
cat(sprintf("  pooled from the saved frame %.4f | implied by the data %.4f (IQR %.3f-%.3f)\n",
            pooled, median(imp$implied, na.rm = TRUE),
            quantile(imp$implied, .25, na.rm = TRUE), quantile(imp$implied, .75, na.rm = TRUE)))
cat("  A tight implied constant means the FORM is right even if the pooled sd\n")
cat("  differs -- .pooled_sd is computed on a wider population than the frame\n")
cat("  that ships, which is a known and separate issue.\n")
k <- median(imp$implied, na.rm = TRUE) / pooled
g[, recon := recon * k]
cat(sprintf("  worst |recon - actual| after rescaling: %.4f\n",
            g[season == S, max(abs(recon - act), na.rm = TRUE)]))

cat("\n########## 1. TOG CORRELATION AT EACH STAGE ##########\n")
g[, centred := (p80 - m)]
g[, standardised := centred / s]
pl <- g[season == S, .(n = .N, tog = mean(tog_safe),
                       raw = mean(p80 * tog_safe), p80 = mean(p80),
                       centred = mean(centred), std = mean(standardised),
                       adj = mean(act, na.rm = TRUE)), by = player_name][n >= 8]
act_ <- pl[raw > quantile(pl$raw, 0.75, na.rm = TRUE)]
st <- data.table(
  stage = c("A raw", "B per-80", "C centred", "D standardised (/cell sd)",
            "E x TOG = adj"),
  cor_with_tog = round(c(cor(act_$raw, act_$tog), cor(act_$p80, act_$tog),
                         cor(act_$centred, act_$tog), cor(act_$std, act_$tog),
                         cor(act_$adj, act_$tog)), 3))
print(st)
cat("\nThe stage where this jumps is the culprit. If it is D, standardising is\n")
cat("the cause and hitout escapes only because it is not standardised.\n")

cat("\n########## 2. DOES CELL SD TRACK TOG? ##########\n")
cat("Standardising divides by the cell sd, so if the cells whose players play\n")
cat("MORE happen to have SMALLER sds, those players get inflated.\n\n")
cc <- d[, .(n = .N, mean_tog = round(weighted.mean(tog_safe, rep(1, .N)), 3),
            cell_sd = round(torp:::.wtd_sd(p80, tog_safe), 3)), by = .(cell = rk)][n > 500]
setorder(cc, -mean_tog); print(cc)
cat(sprintf("\n  cor(cell mean TOG, cell sd) = %.3f\n", cor(cc$mean_tog, cc$cell_sd)))
cat("  Negative means high-minutes cells have tight spreads, so dividing by the\n")
cat("  sd inflates exactly the players who play most.\n")

cat("\n########## 3. WHICH COMPONENT CARRIES IT? ##########\n")
comp <- c("spoils", "tackles", "pressure_acts", "def_half_pressure_acts",
          "intercepts", "one_percenters", "rebound50s", "frees_against")
wts <- c(EPV_SPOIL_WT, EPV_TACKLE_WT, EPV_PRESSURE_WT, EPV_DEF_PRESSURE_WT,
         EPV_INTERCEPTS_WT, EPV_ONE_PERCENTERS_WT, EPV_REBOUND50S_WT,
         EPV_FREES_AGAINST_WT)
cur <- d[season == S]
print(rbindlist(lapply(seq_along(comp), function(i) {
  v <- cur[[comp[i]]]
  data.table(component = comp[i], weight = round(wts[i], 4),
             points = round(mean(v, na.rm = TRUE) * wts[i], 3),
             cor_with_tog = round(cor(v, cur$tog_safe, use = "complete.obs"), 3))
}))[order(-abs(points))])
cat("\nA component with a big point contribution AND a strong TOG correlation\n")
cat("pushes the whole channel; a negative weight flips the direction.\n")

saveRDS(list(stages = st, cells = cc), file.path(OUT_DIR, "spoil_tog.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
