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

# versebus §1.5: pipeline entry points run strict + disable piggyback's
# asset-list memoisation (torp P6) + clear negative-cache markers.
Sys.setenv(VERSEBUS_STRICT = "1")
Sys.setenv(piggyback_cache_duration = 1)
clear_skip_markers()

if (sys.nframe() == 0) {
  result <- run_predictions_pipeline()
  result$predictions %>% filter(season == get_afl_season(), round == get_afl_week('next'))
}

# sim_results <- simulate_afl_season(get_afl_season(), n_sims = 5000, seed = 42, n_cores = parallel::detectCores() - 2)
# print(sim_results)
