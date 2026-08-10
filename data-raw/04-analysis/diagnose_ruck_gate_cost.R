# Why does the ruck repricing cost MAE and bits?
#
# THE PUZZLE. The match model is known to absorb pure rescales of a rating --
# it consumes every channel diff and reweights them itself, so calibration
# changes gate neutral and only changes to WHAT IS MEASURED move MAE. The
# repriced arm has a Spearman of 0.9991 on total EPR, which looks like almost a
# pure monotone transform, and yet dMAE is +0.2194 with the CI on zero.
#
# Those two facts do not sit together, so one of them is being misread. The most
# likely candidate: 0.9991 is the Spearman on TOTAL epr across all players, and
# the ruck channel is a small part of that for most of the league. The channel
# itself may have moved far more, and the channel diff is what the model sees.
#
# WHAT THIS MEASURES, at the level the match model actually consumes:
#   1. how much epr_hitout_diff itself changed, not total epr
#   2. how well each version correlates with the margin it is meant to help
#      predict -- if the old one simply correlates better, that IS the answer
#   3. whether the change is a rescale (absorbable) or a reordering (not)
#   4. what the channel is made of before and after, since a channel that has
#      become mostly win-RATE is a different measurement from one that was
#      mostly VOLUME, and the model cannot recover volume if we removed it

suppressMessages({
  library(dplyr); library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "ruck_gate_cost.txt"), split = TRUE)
cat("=== Why does the ruck repricing cost prediction? ===\nrun at", format(Sys.time()), "\n")

teams <- load_teams(TRUE)
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)),
                   error = function(e) NULL)
res <- as.data.table(load_results(TRUE))
tg <- res[, .(match_id = as.character(match_id),
              margin = home_score - away_score)][is.finite(margin)]

CH <- c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout", "epr")
diffs <- function(tag) {
  rt <- as.data.frame(read_parquet(file.path(OUT_DIR, paste0("ruck_rt_", tag, ".parquet"))))
  tr <- as.data.table(.build_team_ratings_df(teams, rt, psr_df))
  h <- tr[team_type == "home"]; a <- tr[team_type == "away"]
  m <- merge(h[, c("match_id", CH), with = FALSE],
             a[, c("match_id", CH), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  for (v in CH) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  merge(m[, c("match_id", paste0("d_", CH)), with = FALSE], tg, by = "match_id")
}
A <- diffs("shipped"); B <- diffs("repriced")
j <- merge(A, B, by = "match_id", suffixes = c("_s", "_r"))
# merge suffixed `margin` on both sides (identical values). Restore the plain
# name every fit below uses -- same trap as epv3_ruck_to_advantage.R hit.
if (!"margin" %in% names(j) && "margin_s" %in% names(j)) j[, margin := margin_s]
cat("\nmatches with both arms:", nrow(j), "\n")

cat("\n########## 1. HOW MUCH DID THE CHANNEL THE MODEL SEES CHANGE? ##########\n")
cat("Total epr moved barely at all (Spearman 0.9991). The question is whether\n")
cat("the HITOUT channel diff did, because that is a separate model input.\n\n")
rows <- rbindlist(lapply(CH, function(v) {
  s <- j[[paste0("d_", v, "_s")]]; r <- j[[paste0("d_", v, "_r")]]
  data.table(channel = v,
             sd_shipped = round(sd(s, na.rm = TRUE), 4),
             sd_repriced = round(sd(r, na.rm = TRUE), 4),
             sd_ratio = round(sd(r, na.rm = TRUE) / sd(s, na.rm = TRUE), 3),
             pearson = round(cor(s, r, use = "complete.obs"), 4),
             spearman = round(cor(s, r, method = "spearman", use = "complete.obs"), 4))
}))
print(rows)
cat("\npearson near 1 => a rescale, which the model absorbs.\n")
cat("spearman well below 1 => a REORDERING, which it cannot.\n")

cat("\n########## 2. WHICH VERSION ACTUALLY PREDICTS THE MARGIN? ##########\n")
cat("The direct question. If the shipped channel simply carries more margin\n")
cat("signal, the repricing removed information and the gate is right.\n\n")
pm <- rbindlist(lapply(CH, function(v) {
  s <- j[[paste0("d_", v, "_s")]]; r <- j[[paste0("d_", v, "_r")]]
  data.table(channel = v,
             cor_margin_shipped = round(cor(s, j$margin, use = "complete.obs"), 4),
             cor_margin_repriced = round(cor(r, j$margin, use = "complete.obs"), 4))
}))
pm[, delta := round(cor_margin_repriced - cor_margin_shipped, 4)]
print(pm)

cat("\n########## 3. IS IT THE CHANNEL, OR THE TOTAL IT FEEDS? ##########\n")
cat("epr is built from the channels, so a channel change moves the total too.\n")
cat("Single-feature regressions on margin, R-squared:\n\n")
r2 <- function(x) summary(stats::lm(j$margin ~ x))$r.squared
print(data.table(
  feature = c("d_epr_hitout", "d_epr (total)"),
  shipped  = round(c(r2(j$d_epr_hitout_s), r2(j$d_epr_s)), 5),
  repriced = round(c(r2(j$d_epr_hitout_r), r2(j$d_epr_r)), 5)))

cat("\n########## 4. INCREMENTAL: DOES HITOUT ADD ANYTHING OVER THE REST? ##########\n")
cat("The model has all four channels. What matters is what hitout adds ON TOP\n")
cat("of the others, not its standalone correlation.\n\n")
inc <- function(sfx) {
  base <- stats::lm(margin ~ d_epr_recv + d_epr_disp + d_epr_spoil,
                    data = setnames(copy(j)[, c("margin", paste0(c("d_epr_recv","d_epr_disp","d_epr_spoil","d_epr_hitout"), sfx)), with = FALSE],
                                    c("margin","d_epr_recv","d_epr_disp","d_epr_spoil","d_epr_hitout")))
  full <- stats::lm(margin ~ d_epr_recv + d_epr_disp + d_epr_spoil + d_epr_hitout,
                    data = setnames(copy(j)[, c("margin", paste0(c("d_epr_recv","d_epr_disp","d_epr_spoil","d_epr_hitout"), sfx)), with = FALSE],
                                    c("margin","d_epr_recv","d_epr_disp","d_epr_spoil","d_epr_hitout")))
  c(base = summary(base)$r.squared, full = summary(full)$r.squared,
    added = summary(full)$r.squared - summary(base)$r.squared,
    coef_hitout = unname(coef(full)["d_epr_hitout"]),
    t_hitout = unname(summary(full)$coefficients["d_epr_hitout", 3]))
}
ia <- inc("_s"); ib <- inc("_r")
print(data.table(quantity = names(ia), shipped = round(ia, 5), repriced = round(ib, 5)))
cat("\n'added' is the R-squared the hitout channel contributes beyond the other\n")
cat("three. If it falls, the repricing removed signal the model was using.\n")

cat("\n########## 5. VERDICT ##########\n")
da <- ia[["added"]]; db <- ib[["added"]]
cat(sprintf("  incremental R-squared from hitout: %.5f -> %.5f  (%+.5f)\n", da, db, db - da))
if (db < da) {
  cat("  The repriced channel carries LESS margin signal than the shipped one.\n")
  cat("  That is the MAE cost, and it is a real information loss rather than a\n")
  cat("  scaling artefact the model should have absorbed.\n")
} else {
  cat("  The repriced channel carries AS MUCH OR MORE margin signal, so the MAE\n")
  cat("  cost is NOT explained by this channel losing information. Look elsewhere:\n")
  cat("  the GAM smooths, the shrinkage priors, or the total it feeds.\n")
}

saveRDS(list(changed = rows, margin = pm, incremental = list(shipped = ia, repriced = ib)),
        file.path(OUT_DIR, "ruck_gate_cost.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
