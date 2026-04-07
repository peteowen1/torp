# EP Model Variable Importance
# Check which of the 19 features actually matter

library(xgboost)
devtools::load_all()

cat("\n== EP Model Variable Importance ==\n\n")

# Load the model
ep_model <- load_model_with_fallback("ep")

# Get feature names (same as training)
chains <- load_chains(TRUE, TRUE)
pbp <- clean_pbp(chains)
model_data <- clean_model_data_epv(pbp)
epv_vars <- select_epv_model_vars(model_data)
feature_names <- colnames(stats::model.matrix(~ . + 0, data = epv_vars[1, ]))

cat("Feature names (", length(feature_names), "):\n")
cat(paste(feature_names, collapse = ", "), "\n\n")

# Get importance (all 3 metrics)
imp_gain <- xgb.importance(feature_names = feature_names, model = ep_model)
print(imp_gain)

cat("\n-- Sorted by Gain (predictive contribution) --\n")
imp_gain <- imp_gain[order(-imp_gain$Gain), ]
for (i in seq_len(nrow(imp_gain))) {
  cat(sprintf("  %2d. %-30s Gain=%.4f  Cover=%.4f  Freq=%.4f\n",
    i, imp_gain$Feature[i], imp_gain$Gain[i], imp_gain$Cover[i], imp_gain$Frequency[i]))
}

cat("\n-- Features with <1% Gain --\n")
low <- imp_gain[imp_gain$Gain < 0.01, ]
if (nrow(low) > 0) {
  for (i in seq_len(nrow(low))) {
    cat(sprintf("  %s (Gain=%.4f)\n", low$Feature[i], low$Gain[i]))
  }
} else {
  cat("  None — all features contribute >=1%\n")
}

cat("\nDone.\n")
