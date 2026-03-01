# 03_estimate_skills.R
# ====================
# Run skill estimation for all seasons/rounds.
#
# Input:  cache-skills/01_skill_data.rds, optional 02_optimized_params.rds
# Output: cache-skills/03_player_skills.rds

# Setup ----
devtools::load_all()

# Config ----
cache_dir <- file.path("data-raw", "cache-skills")

# Load data ----
cli::cli_h1("Loading skill data")

skill_data <- readRDS(file.path(cache_dir, "01_skill_data.rds"))
cli::cli_inform("Loaded {nrow(skill_data)} rows")

# Load optimized params if available, otherwise use defaults
params_path <- file.path(cache_dir, "02_optimized_params.rds")
if (file.exists(params_path)) {
  params <- readRDS(params_path)
  cli::cli_inform("Using optimized parameters")
} else {
  params <- default_skill_params()
  cli::cli_inform("Using default parameters")
}

# Load fixtures for round dates ----
fixtures <- load_fixtures(all = TRUE, use_disk_cache = TRUE)

# Compute skills at end of each round ----
cli::cli_h1("Estimating skills by round")

seasons <- sort(unique(skill_data$season))
all_results <- list()
counter <- 0

for (szn in seasons) {
  # Get round dates from fixtures
  fix_szn <- fixtures[fixtures$compSeason.year == szn, ]
  if (nrow(fix_szn) == 0) next

  rounds <- sort(unique(fix_szn$round.roundNumber))

  for (rnd in rounds) {
    # Reference date = first match of this round
    ref_date <- min(as.Date(fix_szn$utcStartTime[fix_szn$round.roundNumber == rnd]),
                    na.rm = TRUE)

    if (is.na(ref_date) || is.infinite(ref_date)) next

    result <- tryCatch(
      estimate_player_skills(skill_data, ref_date = ref_date, params = params),
      error = function(e) {
        cli::cli_warn("Failed for {szn} R{rnd}: {conditionMessage(e)}")
        NULL
      }
    )

    if (!is.null(result) && nrow(result) > 0) {
      result[, `:=`(season = szn, round = rnd)]
      counter <- counter + 1
      all_results[[counter]] <- result
    }
  }

  cli::cli_inform("Completed {szn} ({length(rounds)} rounds)")
}

# Estimate for future fixture seasons (first round only) ----
future_seasons <- setdiff(sort(unique(fixtures$compSeason.year)), seasons)

for (szn in future_seasons) {
  fix_szn <- fixtures[fixtures$compSeason.year == szn, ]
  if (nrow(fix_szn) == 0) next

  rnd <- min(fix_szn$round.roundNumber, na.rm = TRUE)
  ref_date <- min(as.Date(fix_szn$utcStartTime[fix_szn$round.roundNumber == rnd]),
                  na.rm = TRUE)

  if (is.na(ref_date) || is.infinite(ref_date)) next

  result <- tryCatch(
    estimate_player_skills(skill_data, ref_date = ref_date, params = params),
    error = function(e) {
      cli::cli_warn("Failed for {szn} R{rnd}: {conditionMessage(e)}")
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

# Combine ----
cli::cli_h1("Combining results")

all_skills <- data.table::rbindlist(all_results, fill = TRUE)
all_seasons <- sort(unique(all_skills$season))
cli::cli_inform("Total: {nrow(all_skills)} player-round rows across {length(all_seasons)} seasons")

# Save ----
saveRDS(all_skills, file.path(cache_dir, "03_player_skills.rds"))
cli::cli_alert_success("Saved to {file.path(cache_dir, '03_player_skills.rds')}")
