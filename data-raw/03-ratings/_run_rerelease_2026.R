# AFL 2026 re-release on the current EP/WP models (docs/plans/AFL-2026-RERELEASE.md).
# Phase 3 (play-by-play) and Phase 6 (derived data) of rebuild_everything.R, for 2026
# only, WITHOUT its Phase 8 (full-history ratings) and Phase 9 (retrains match GAMs).
# The incremental ratings update runs after this, from run_ratings_pipeline.R.
#
# Local torpdata/data is switched OFF (NA): it holds a stale old-model 2026 pbp.
options(torp.local_data_dir = NA)
suppressMessages({ library(dplyr); library(data.table); devtools::load_all(".", quiet = TRUE) })
if (file.exists("../torpmodels/DESCRIPTION")) suppressMessages(devtools::load_all("../torpmodels", quiet = TRUE))
stopifnot(is.null(get_local_data_dir()), identical(RATING_VINTAGE, "v15"))
say <- function(...) { cat(format(Sys.time(), "%H:%M:%S "), ..., "\n", sep = ""); flush.console() }
season <- 2026L

say("play-by-play: loading chains")
chains <- load_chains(seasons = season, rounds = TRUE)
say("chains ", nrow(chains), " rows, ", uniqueN(chains$match_id), " matches")
stopifnot(nrow(chains) > 100000)
pbp <- chains |> clean_pbp() |> clean_model_data_epv() |> clean_shots_data() |>
  add_shot_vars() |> add_epv_vars() |> clean_model_data_wp() |> add_wp_vars()
say("pbp ", nrow(pbp), " rows")
arrow::write_parquet(pbp, "data-raw/logs/pbp_data_2026_all.rerelease.parquet")   # local copy for the checks
save_to_release(pbp, paste0("pbp_data_", season, "_all"), "pbp-data")
say("pbp published")
rm(chains, pbp); gc()

start_rd <- if (season >= 2024) 0L else 1L
end_rd <- .afl_last_round(season)
say("xG ", start_rd, "-", end_rd)
xg_df <- calculate_match_xgs(season, start_rd:end_rd)
save_to_release(xg_df, paste0("xg_data_", season), "xg-data")
say("xg ", nrow(xg_df), " rows published")

clear_all_cache()
pbp <- load_pbp(season, rounds = TRUE)
say("reloaded pbp from the release: ", nrow(pbp), " rows")
chains <- load_chains(season, rounds = TRUE)
pgd <- create_player_game_data(pbp, load_player_stats(season), load_teams(season), chains = chains)
np_bd <- attr(pgd, "np_breakdown")   # read before save_to_release() touches pgd (as daily_release.R)
save_to_release(pgd, paste0("player_game_", season), "player_game-data")
say("player_game ", nrow(pgd), " rows published")
stopifnot(!is.null(np_bd), nrow(np_bd) > 0)
save_to_release(as.data.frame(np_bd), paste0("np_breakdown_", season), "player_game-data")
say("np_breakdown ", nrow(np_bd), " rows published")

chart_cols <- c("match_id", "season", "round_number", "period", "period_seconds", "total_seconds", "display_order",
                "home_team_name", "away_team_name", "team", "home", "pos_team_points", "opp_team_points", "points_diff",
                "home_points", "away_points", "exp_pts", "delta_epv", "wp", "wpa", "description", "player_name",
                "play_type", "shot_row", "points_shot")
save_to_release(pbp[, intersect(chart_cols, names(pbp)), drop = FALSE], paste0("ep_wp_chart_", season, "_all"), "ep_wp_chart-data")
say("ep_wp_chart published. DONE")
