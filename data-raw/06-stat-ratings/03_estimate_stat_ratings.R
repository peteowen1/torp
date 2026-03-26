# 03_estimate_stat_ratings.R
# ==========================
# Run stat rating estimation for all seasons/rounds.
#
# Input:  cache-stat-ratings/01_stat_rating_data.rds, optional 02_optimized_params.rds
# Output: cache-stat-ratings/03_player_stat_ratings.rds

# Setup ----
devtools::load_all()

# Config ----
cache_dir <- file.path("data-raw", "cache-stat-ratings")

# Load data ----
cli::cli_h1("Loading stat rating data")

stat_rating_data <- readRDS(file.path(cache_dir, "01_stat_rating_data.rds"))
cli::cli_inform("Loaded {nrow(stat_rating_data)} rows")

# Load optimized params if available, otherwise use defaults
params_path <- file.path(cache_dir, "02_optimized_params.rds")
if (file.exists(params_path)) {
  params <- readRDS(params_path)
  cli::cli_inform("Using optimized parameters")
} else {
  params <- default_stat_rating_params()
  cli::cli_inform("Using default parameters")
}

# Load fixtures for round dates ----
fixtures <- load_fixtures(all = TRUE, use_disk_cache = TRUE)
fixtures_dt <- data.table::as.data.table(fixtures)

# Build ref_date map from fixtures ----
cli::cli_h1("Estimating stat ratings by round")

seasons <- sort(unique(stat_rating_data$season))

# Collect all ref_dates: one per season-round (= first match of that round)
ref_date_map <- fixtures_dt[
  season %in% seasons,
  .(ref_date = min(as.Date(utc_start_time), na.rm = TRUE)),
  by = .(season, round = round_number)
]
ref_date_map <- ref_date_map[!is.na(ref_date) & is.finite(ref_date)]
data.table::setorder(ref_date_map, ref_date)
cli::cli_inform("Processing {nrow(ref_date_map)} season-round combinations")

# Batch estimation (no CI needed for per-round snapshots) ----
t0 <- proc.time()
batch_results <- .estimate_stat_ratings_batch(
  stat_rating_data,
  ref_dates = ref_date_map$ref_date,
  params = params,
  compute_ci = FALSE
)
elapsed <- (proc.time() - t0)[["elapsed"]]
cli::cli_inform("Batch estimation completed in {round(elapsed, 1)}s")

# Attach season/round metadata ----
all_results <- vector("list", nrow(ref_date_map))
counter <- 0

for (i in seq_len(nrow(ref_date_map))) {
  rd_key <- as.character(ref_date_map$ref_date[i])
  if (rd_key %in% names(batch_results)) {
    res <- batch_results[[rd_key]]
    if (nrow(res) > 0) {
      res[, `:=`(season = ref_date_map$season[i], round = ref_date_map$round[i])]
      counter <- counter + 1
      all_results[[counter]] <- res
    }
  }
}

n_failures <- nrow(ref_date_map) - counter
for (szn in seasons) {
  n_rnds <- ref_date_map[season == szn, .N]
  n_ok <- sum(vapply(all_results[seq_len(counter)], function(r) {
    any(r$season == szn)
  }, logical(1)))
  cli::cli_inform("Completed {szn} ({n_ok}/{n_rnds} rounds)")
}

# Estimate for future fixture seasons (first round only) ----
future_seasons <- setdiff(sort(unique(fixtures_dt$season)), seasons)

for (szn in future_seasons) {
  fix_szn <- fixtures_dt[season == szn]
  if (nrow(fix_szn) == 0) next

  rnd <- min(fix_szn$round_number, na.rm = TRUE)
  ref_date <- min(as.Date(fix_szn$utc_start_time[fix_szn$round_number == rnd]),
                  na.rm = TRUE)

  if (is.na(ref_date) || is.infinite(ref_date)) next

  result <- tryCatch(
    estimate_player_stat_ratings(stat_rating_data, ref_date = ref_date, params = params,
                           compute_ci = FALSE),
    error = function(e) {
      cli::cli_warn("Failed for {szn} R{rnd}: {conditionMessage(e)}")
      n_failures <<- n_failures + 1
      NULL
    }
  )

  if (!is.null(result) && nrow(result) > 0) {
    result[, `:=`(season = szn, round = rnd)]
    counter <- counter + 1
    all_results[[counter]] <- result
  }

  cli::cli_inform("Completed {szn} R{rnd} [fixture]")
}

# Check failure rate ----
if (counter == 0) {
  cli::cli_abort("All round estimations failed ({n_failures} errors). Aborting to prevent empty output.")
}
if (n_failures > 0) {
  cli::cli_inform("{n_failures}/{counter + n_failures} rounds skipped (too few games before ref_date to meet min_games threshold)")
}

# Combine ----
cli::cli_h1("Combining results")

all_stat_ratings <- data.table::rbindlist(all_results[seq_len(counter)], fill = TRUE)
all_seasons <- sort(unique(all_stat_ratings$season))
cli::cli_inform("Total: {nrow(all_stat_ratings)} player-round rows across {length(all_seasons)} seasons")

# Save ----
saveRDS(all_stat_ratings, file.path(cache_dir, "03_player_stat_ratings.rds"))
cli::cli_alert_success("Saved to {file.path(cache_dir, '03_player_stat_ratings.rds')}")
