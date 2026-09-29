# AFL 2026 re-release, step 3: incremental 2026 ratings on v15 (docs/plans/AFL-2026-RERELEASE.md).
# Same config as daily-ratings-predictions.yml's partial-season run. REBUILD_ALL_RATINGS
# MUST be FALSE: TRUE replaces the whole torp_ratings.parquet with SEASONS only, which
# with SEASONS = 2026 would drop 2021-2025.
options(torp.local_data_dir = NA)   # the local torpdata/data holds stale 2026 files
suppressMessages(devtools::load_all(".", quiet = TRUE))
stopifnot(is.null(get_local_data_dir()), identical(RATING_VINTAGE, "v15"))
SEASONS <- 2026
REFRESH_UPSTREAM <- FALSE
REBUILD_PLAYER_GAME <- TRUE
REBUILD_ALL_RATINGS <- FALSE
cat("ratings start:", format(Sys.time()), "\n")
source("data-raw/03-ratings/run_ratings_pipeline.R")
cat("ratings end:", format(Sys.time()), "\n")
