# Paired v2-vs-v3 comparison for the FOUR-channel gate.
#
# ws30_epv3_4ch_gate.R completed all three arms and wrote their per-match
# predictions, then died in its final block (the file was edited while the job was
# running -- Rscript had NOT fully parsed it up front, which is worth remembering).
# The expensive work is done and saved, so the paired numbers are recovered here
# rather than by re-running 90 minutes.
suppressPackageStartupMessages({ library(data.table); library(arrow) })

OUT <- "C:/dev/torpverse/torp/data-raw/outputs"
rd <- function(f) as.data.table(read_parquet(file.path(OUT, f)))
preds <- list(
  "v2 production"  = rd("epv3_4ch_v2_production.parquet"),
  "v3 final"       = rd("epv3_4ch_v3_final.parquet"),
  "v3 + shrinkage" = rd("epv3_4ch_v3_shrinkage.parquet")
)
for (nm in names(preds)) preds[[nm]] <- preds[[nm]][is.finite(pred_margin) & is.finite(margin)]

.bits <- function(pw, hw) mean(ifelse(hw == 1, 1 + log2(pw),
                              ifelse(hw == 0, 1 + log2(1 - pw), 1 + 0.5 * log2(pw * (1 - pw)))))

cat("=== headline (all matches each arm scored) ===\n")
tab <- rbindlist(lapply(names(preds), function(nm) {
  p <- preds[[nm]]
  data.table(arm = nm, n = nrow(p),
             MAE  = round(mean(abs(p$pred_margin - p$margin)), 4),
             RMSE = round(sqrt(mean((p$pred_margin - p$margin)^2)), 4),
             bits = round(.bits(pmin(pmax(p$pred_win, 1e-6), 1 - 1e-6), p$home_win), 4),
             tips = sum((p$pred_margin > 0) == (p$margin > 0)))
}))
print(tab)

common <- Reduce(intersect, lapply(preds, function(p) p$match_id))
ba <- preds[["v2 production"]][match_id %chin% common][order(match_id)]
cat("\n=== paired against v2 production, ", length(common), " common matches ===\n", sep = "")
cat("(POSITIVE dMAE = v3 is WORSE; this is the number the ship decision needs)\n")
for (nm in c("v3 final", "v3 + shrinkage")) {
  q <- preds[[nm]][match_id %chin% common][order(match_id)]
  dd <- abs(q$pred_margin - q$margin) - abs(ba$pred_margin - ba$margin)
  dd <- dd[is.finite(dd)]
  tt <- t.test(dd)
  cat(sprintf("  %-16s dMAE %+.4f  95%% CI [%+.4f, %+.4f]  p %.4f\n",
              nm, mean(dd), tt$conf.int[1], tt$conf.int[2], tt$p.value))
}

cat("\n=== per-season dMAE (v3 final - v2), to check it is not one bad year ===\n")
q <- preds[["v3 final"]][match_id %chin% common][order(match_id)]
if ("season" %in% names(q)) {
  s <- data.table(season = q$season,
                  d = abs(q$pred_margin - q$margin) - abs(ba$pred_margin - ba$margin))
  print(s[is.finite(d), .(n = .N, dMAE = round(mean(d), 4)), by = season][order(season)])
} else {
  cat("  (no season column in the saved predictions)\n")
}

cat("\nCONTEXT: the 3-channel gate reported dMAE +1.109 on 1,203 matches.\n")
cat("That is a DIFFERENT sample from this one, so the two are not directly\n")
cat("comparable -- read the CI here, not the difference of the two point estimates.\n")
