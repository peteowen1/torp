# Build the v3 rating vintage as a CANDIDATE, alongside canonical.
#
# WHY THIS WRAPPER EXISTS. The point of this run is to see the real
# All-Australian squad under v3 BEFORE anything the site reads changes. TOPV is
# 0.5*EPV + 0.5*PSV, so the engine can move the 46 names, and four posts naming
# them are already merged to main behind draft: true. A rating-proxy check put
# the churn at 6 of 46 (out: Dangerfield, Darcy, Impey, Curtis, Coleman,
# Naughton), but that compared per-player RATINGS with no quotas -- the published
# squad uses cumulative season TOPV with 14/14/4/14 quotas and a 12-game floor,
# so the real answer needs real game-logs.
#
# RATINGS_VINTAGE = "v3" writes torp_ratings_v3.parquet and leaves canonical
# torp_ratings.parquet alone, so the live site keeps serving v2 throughout.
# Promotion is a separate, deliberate act and this run must not do it.
RATINGS_VINTAGE   <- "v3"

# Defaults, stated rather than inherited, so the run is reproducible from this
# file alone.
SEASONS           <- TRUE   # all seasons
REFRESH_UPSTREAM  <- TRUE   # pull current player_stats/teams
REBUILD_PLAYER_GAME <- TRUE # v3 changes the player-game frame, so it must rebuild
REBUILD_ALL_RATINGS <- TRUE

# The pipeline calls devtools::load_all() itself, but the guards below read the
# constants BEFORE that, so load first. Without this the run dies instantly on
# "object 'EPV_ENGINE' not found" -- harmless, but it wastes a launch.
suppressMessages(devtools::load_all(quiet = TRUE))

cat("=== v3 CANDIDATE vintage run ===\n")
cat("engine   : ", torp:::EPV_ENGINE, "\n", sep = "")
cat("channels : ", torp:::EPV3_CHANNELS, "\n", sep = "")
cat("scale    : ",
    paste(names(torp:::EPV3_POINTS_SCALE), torp:::EPV3_POINTS_SCALE,
          sep = "=", collapse = ", "), "\n", sep = "")
cat("vintage  : ", RATINGS_VINTAGE, "  -> torp_ratings_v3.parquet\n", sep = "")
cat("canonical torp_ratings.parquet is NOT touched by this run\n\n")

# Guard: if the engine is not v3 this run would publish a v2 vintage under a v3
# label, which is worse than not running at all.
stopifnot(identical(torp:::EPV_ENGINE, "v3"),
          identical(torp:::EPV3_CHANNELS, 4L))

source(here::here("data-raw/03-ratings/run_ratings_pipeline.R"))
