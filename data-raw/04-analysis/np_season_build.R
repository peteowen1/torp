# The season build both net points page generators need, cached.
# =============================================================================
# build_np_categories_artifact.R and np_first_goal_walkthrough.R each spent
# about 15 minutes recomputing the same thing: the season's play-by-play,
# chains, player stats, results, difficulty terms and build_net_points(). This
# builds it once and reuses it until something it depends on changes.
#
# The key is the rating vintage, the engine, the season, and a hash of the two
# files that decide the ledger (R/epv_net_points.R, R/constants_ratings.R), so a
# code or constant change rebuilds it rather than serving stale numbers -- a
# stale cached input once put a superseded model behind every number on the
# walkthrough page. .np_team_margin() is NOT cached: it is quick, and it is the
# step most often being changed.
#
#   source("data-raw/04-analysis/np_season_build.R"); b <- np_season_build(2026)
np_season_build <- function(season, cache_dir = "data-raw/outputs") {
  key <- paste(RATING_VINTAGE, EPV_ENGINE, season,
               unname(tools::md5sum(c("R/epv_net_points.R", "R/constants_ratings.R"))),
               collapse = " ")
  path <- file.path(cache_dir, sprintf("np_season_build_%s.rds", season))
  if (file.exists(path)) {
    b <- readRDS(path)
    if (identical(b$key, key)) {
      message("np_season_build: cached (", path, ")")
      return(b)
    }
    message("np_season_build: cache key changed, rebuilding")
  }
  t0 <- Sys.time()
  pbp <- data.table::as.data.table(load_pbp(season)); pbp[, match_id := as.character(match_id)]
  ch  <- data.table::as.data.table(load_chains(season)); ch[, match_id := as.character(match_id)]
  ps  <- data.table::as.data.table(load_player_stats(season, refresh = TRUE))
  res <- data.table::as.data.table(load_results(season)); res[, match_id := as.character(match_id)]
  tm  <- data.table::as.data.table(np_difficulty_terms_for_season(season, pbp_data = pbp, chains = ch))
  tm[, match_id := as.character(match_id)]
  tm  <- tm[substr(match_id, 5, 8) == as.character(season)]
  np  <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                          stoppages = "allocate", difficulty_terms = tm,
                          return_payments = TRUE)
  b <- list(key = key, pbp = pbp, ch = ch, ps = ps, res = res, tm = tm, np = np)
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(b, path)
  message("np_season_build: built in ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1),
          " min, cached to ", path)
  b
}
