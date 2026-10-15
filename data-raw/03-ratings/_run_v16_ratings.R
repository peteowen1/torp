# Rating vintage v16 (2026-10-11): full-history ratings rebuild after published PSR
# moved to the season-listing key (Thread G, torp#251; PSR_PUBLISHED_CENTRE).
#
# PSR moves in EVERY season, so this rebuilds all of them: SEASONS = TRUE with
# REBUILD_ALL_RATINGS = TRUE, which replaces torp_ratings.parquet with the full
# 2021-2026 history (the partial-rebuild guard aborts if any season fails).
# player_game is unchanged by v16, so it is reused, not rebuilt; upstream API
# data is not refreshed. The manifest was promoted v15 -> v16 first
# (promote_rating_vintage.R; v15 kept byte-identical as torp_ratings_v15.parquet).
#
# PROVENANCE ONLY -- it already ran (2026-10-11, 64 min). Re-running rewrites the
# published v16 in place; the guards below stop it once v16 is no longer current.
#
# Run from torp/:  Rscript data-raw/03-ratings/_run_v16_ratings.R
options(torp.local_data_dir = NA)   # never let a stale local torpdata/data shadow the releases
suppressMessages(devtools::load_all(".", quiet = TRUE))
stopifnot(is.null(get_local_data_dir()),
          identical(RATING_VINTAGE, "v16"),
          identical(PSR_PUBLISHED_CENTRE, "listed"),
          identical(read_ratings_manifest()$canonical, "v16"))
SEASONS <- TRUE
REFRESH_UPSTREAM <- FALSE
REBUILD_PLAYER_GAME <- FALSE
REBUILD_ALL_RATINGS <- TRUE
cat("ratings start:", format(Sys.time()), "\n")
source("data-raw/03-ratings/run_ratings_pipeline.R")
cat("ratings end:", format(Sys.time()), "\n")
print(tictoc::tic.log(format = TRUE))
