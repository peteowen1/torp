# Match Predictions Pipeline ----
#
# Thin entrypoint for weekly AFL match predictions.
# The pipeline function lives in torp/R/match_model.R.
#
# Usage:
#   Rscript data-raw/02-models/build_match_predictions.R
#
# Or interactively after devtools::load_all():
#   run_predictions_pipeline()              # auto-detect next week
#   run_predictions_pipeline(week = 5)      # specific week
#   run_predictions_pipeline(weeks = "all") # all fixture weeks
#   run_predictions_pipeline(weeks = 1:5)   # specific weeks

devtools::load_all()

result <- run_predictions_pipeline()
