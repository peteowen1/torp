# Score the gate's own predictions before believing its dMAE.
#
# Per C:/dev/CLAUDE.md: after fitting any model, score it back on the data and
# hunt outlier predictions before reporting a single metric. Aggregate metrics
# cannot see per-row leakage, and dMAE between two arms is an aggregate of
# aggregates -- if both arms leak, the comparison still looks clean.
#
# The check that works is "find a state where the model cannot legitimately know
# much, and see whether it claims to." For a match-margin model that state is
# ROUND 1 of a season: no current-season form exists, so predictions should be
# visibly weaker there than late in a season. If round 1 is as sharp as round 20,
# a rating is carrying information from the match it is predicting.
suppressPackageStartupMessages({ library(data.table); library(arrow) })

OUT <- "C:/dev/torpverse/torp/data-raw/outputs"
pick <- function(stem) {
  f <- file.path(OUT, paste0(stem, ".parquet"))
  g <- file.path(OUT, paste0(stem, "_414match.parquet"))
  if (file.exists(f)) return(as.data.table(read_parquet(f)))
  if (file.exists(g)) { cat("(using archived 414-match", stem, ")\n"); return(as.data.table(read_parquet(g))) }
  NULL
}
arms <- list(v2 = pick("epv3_4ch_v2_production"),
             v3 = pick("epv3_4ch_v3_final"))
arms <- arms[!vapply(arms, is.null, logical(1))]
stopifnot(length(arms) > 0)

for (nm in names(arms)) {
  p <- arms[[nm]][is.finite(pred_margin) & is.finite(margin)]
  cat("\n==============================", nm, "==============================\n")
  cat("n =", nrow(p), " MAE =", round(mean(abs(p$pred_margin - p$margin)), 3), "\n")

  # 1. IMPLAUSIBLE PREDICTIONS. AFL margins are heavy-tailed but bounded in
  #    practice; a predicted margin far outside the observed range is a tell.
  cat("\n-- prediction range vs actual --\n")
  cat(sprintf("  pred_margin  min %.1f  max %.1f  sd %.1f\n",
              min(p$pred_margin), max(p$pred_margin), sd(p$pred_margin)))
  cat(sprintf("  actual       min %.1f  max %.1f  sd %.1f\n",
              min(p$margin), max(p$margin), sd(p$margin)))
  wild <- p[abs(pred_margin) > max(abs(p$margin))]
  cat("  predictions outside the actual range:", nrow(wild), "\n")

  # 2. THE COLD-START TEST. Round 1 has no current-season form. Correlation
  #    there should be clearly WEAKER than late season. Similar or better is the
  #    signature of a rating that already knows the result.
  if (all(c("season", "round") %in% names(p))) {
    p[, phase := fifelse(round <= 1, "round 1 (cold start)",
                 fifelse(round >= 15, "round 15+ (form known)", "mid season"))]
    cat("\n-- cold-start test: does round 1 predict as well as late season? --\n")
    print(p[, .(n = .N,
                MAE = round(mean(abs(pred_margin - margin)), 2),
                cor = round(cor(pred_margin, margin), 3)), by = phase][order(phase)])
    r1 <- p[phase == "round 1 (cold start)"]; late <- p[phase == "round 15+ (form known)"]
    if (nrow(r1) > 5 && nrow(late) > 5) {
      cat(sprintf("  round1 cor %.3f vs late cor %.3f -> %s\n",
                  cor(r1$pred_margin, r1$margin), cor(late$pred_margin, late$margin),
                  if (cor(r1$pred_margin, r1$margin) >= cor(late$pred_margin, late$margin))
                    "SUSPICIOUS: cold start predicts as well as late season" else "expected shape"))
    }
  } else {
    cat("\n(no season/round columns -- cold-start test skipped)\n")
  }

  # 3. PERFECT-SEPARATION TELL. Bucket the predictions and compare actual means.
  #    A monotone ramp is calibration; near-perfect separation is the answer
  #    leaking into a feature.
  cat("\n-- decile calibration (monotone ramp = healthy, perfect split = leak) --\n")
  p[, dec := cut(pred_margin, breaks = quantile(pred_margin, 0:10/10), include.lowest = TRUE, labels = FALSE)]
  print(p[, .(n = .N, mean_pred = round(mean(pred_margin), 1),
              mean_actual = round(mean(margin), 1)), by = dec][order(dec)])
}
cat("\nNOTE: this audits the PREDICTIONS, not the ratings pipeline. A leak shared\n")
cat("by both arms would inflate both MAEs and could still leave dMAE looking sane.\n")
