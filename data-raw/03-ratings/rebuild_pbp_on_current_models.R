# Re-score published play-by-play on the current EP/WP models -- the hand step
# after an EP, WP or WP-calibration retrain (Pete 2026-11-27: a retrain forces a
# rating vintage bump; the nightly never rebuilds history by itself).
#
# check_vintage_alignment() refuses every ratings run once the published models
# differ from the ones the canonical vintage recorded
# (torp_error_vintage_models_drift). This script is the first half of the way
# out. It is the recipe that published v18 on 2026-11-27, in one file.
#
# Usage, from the torp package root (PowerShell, arrow needs it):
#   Rscript data-raw/03-ratings/rebuild_pbp_on_current_models.R 2021 2022 ... 2026
#     DRY RUN (default): scores each season from the released chains into
#     $env:PBP_REBUILD_DIR (default <tempdir>/pbp_rebuild), skipping seasons
#     already there, and compares each with the published file. Uploads nothing.
#   $env:PBP_REBUILD_PUBLISH='1'; Rscript ... (same seasons)
#     uploads exactly the files the dry run checked (no re-scoring), first
#     saving the published ones to <dir>/backup, then records pbp_model_stamps.json
#     and rebuilds xg-data and ep_wp_chart-data from the new play-by-play.
#
# Read the dry run before publishing. What "checks out" meant for v18:
#   - row count and (match_id, display_order) keys equal the published file;
#   - every row stamped with the published model id;
#   - a season already on the current models comes out IDENTICAL (the control:
#     if it does not, something other than the models moved -- stop);
#   - no column newly 100% NA.
# Measured 2026-11-27: under 1 minute a season to score, ~100 s to upload.
#
# Then the rating vintage, in this order (memory: vintage flip needs a
# full-history rebuild):
#   1. bump RATING_VINTAGE (R/constants_ratings.R) + NEWS on a branch, review;
#   2. promote_rating_vintage.R (PROMOTE_FROM / PROMOTE_TO), which preserves the
#      outgoing canonical and records the new models on the new entry;
#   3. merge to main, then check_vintage_alignment(strict = TRUE) on main;
#   4. gh workflow run daily-ratings-predictions.yml --ref main -f seasons=TRUE
#      (rebuilds player_game data, ratings, PSR and stat ratings, then the blog).

args <- commandArgs(trailingOnly = TRUE)
seasons <- as.integer(args)
stopifnot("pass the seasons to rebuild" = length(seasons) > 0, !anyNA(seasons))
publish <- identical(Sys.getenv("PBP_REBUILD_PUBLISH"), "1")
out_dir <- Sys.getenv("PBP_REBUILD_DIR", file.path(tempdir(), "pbp_rebuild"))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

suppressMessages({
  library(data.table)
  devtools::load_all(".", quiet = TRUE)
  if (dir.exists("../torpmodels")) devtools::load_all("../torpmodels", quiet = TRUE)
})
options(torp.local_data_dir = NA)
# Each published season is ~85 MB; R's default 60 s download timeout failed on
# one at a slow moment (2026-11-27).
options(timeout = max(600, getOption("timeout")))
stopifnot(is.null(get_local_data_dir()))
Sys.setenv(VERSEBUS_STRICT = "1")
say <- function(...) cat(..., "\n", sep = "")

cur_id <- .current_pbp_model_id()
say("published EP/WP model id: ", cur_id, " | ", if (publish) "PUBLISH" else "DRY RUN", " | dir ", out_dir)
f_new <- function(s) file.path(out_dir, sprintf("pbp_data_%d_all.parquet", s))
key <- c("match_id", "display_order")

if (!publish) {
  for (s in seasons) {
    if (!file.exists(f_new(s))) {
      t0 <- Sys.time()
      chains <- load_chains(seasons = s, rounds = TRUE)
      stopifnot(nrow(chains) > 0)
      pbp <- .score_pbp(chains)
      stopifnot(identical(unique(pbp$pbp_model_id), cur_id))
      arrow::write_parquet(pbp, paste0(f_new(s), ".tmp"))
      file.rename(paste0(f_new(s), ".tmp"), f_new(s))  # a killed run leaves no half file
      say(s, ": scored ", nrow(pbp), " rows in ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")
      rm(chains, pbp); gc()
    }
    new <- as.data.table(arrow::read_parquet(f_new(s)))
    old <- as.data.table(load_pbp(s, rounds = TRUE))
    all_na <- function(d) names(d)[vapply(d, function(x) all(is.na(x)), logical(1))]
    k_new <- unique(new[, ..key]); k_old <- unique(old[, ..key])
    say("\n=== ", s, ": new ", nrow(new), " rows | published ", nrow(old))
    say("  keys only new / only published: ", nrow(fsetdiff(k_new, k_old)), " / ", nrow(fsetdiff(k_old, k_new)),
        " | duplicate keys new: ", anyDuplicated(new, by = key))
    say("  columns only new: ", paste(setdiff(names(new), names(old)), collapse = ","),
        " | only published: ", paste(setdiff(names(old), names(new)), collapse = ","))
    say("  newly 100% NA: ", paste(setdiff(all_na(new), all_na(old)), collapse = ","))
    m <- merge(new, old, by = key, suffixes = c(".n", ".o"))
    for (cc in intersect(c("exp_pts", "delta_epv", "wp", "wpa", "xscore"), intersect(names(new), names(old)))) {
      d <- abs(m[[paste0(cc, ".n")]] - m[[paste0(cc, ".o")]])
      say(sprintf("  %-9s differ %d of %d | max %.4f | mean %.5f", cc,
                  sum(d > 1e-9, na.rm = TRUE), nrow(m), max(d, na.rm = TRUE), mean(d, na.rm = TRUE)))
    }
    rm(new, old, m); gc()
  }
  say("\nDRY RUN done: nothing uploaded. Publish with $env:PBP_REBUILD_PUBLISH='1'.")
  quit(status = 0)
}

# PUBLISH ----
missing <- seasons[!file.exists(vapply(seasons, f_new, ""))]
if (length(missing)) stop("no checked file for ", paste(missing, collapse = ","), "; run the dry run first")
bak <- file.path(out_dir, "backup"); dir.create(bak, showWarnings = FALSE)
for (s in seasons) {
  name <- sprintf("pbp_data_%d_all.parquet", s)
  if (!file.exists(file.path(bak, name))) {
    system2("gh", c("release", "download", "pbp-data", "-R", get_torp_data_repo(), "-p", name, "-D", bak, "--clobber"))
  }
  pbp <- arrow::read_parquet(f_new(s))
  stopifnot(identical(unique(pbp$pbp_model_id), cur_id))
  save_to_release(pbp, paste0("pbp_data_", s, "_all"), "pbp-data")
  stopifnot(isTRUE(.record_pbp_model_stamps(s, pbp)))
  say(s, ": published ", nrow(pbp), " rows + stamp")
  rm(pbp); gc()
}
chart_cols <- c("match_id", "season", "round_number", "period", "period_seconds",
  "total_seconds", "display_order", "home_team_name", "away_team_name", "team", "home",
  "pos_team_points", "opp_team_points", "points_diff", "home_points", "away_points",
  "exp_pts", "delta_epv", "wp", "wpa", "description", "player_name", "play_type",
  "shot_row", "points_shot")
for (s in seasons) {
  xg_df <- calculate_match_xgs(s, .afl_first_round(s):.afl_max_round(s))
  save_to_release(xg_df, paste0("xg_data_", s), "xg-data")
  pbp <- load_pbp(s, rounds = TRUE)
  stopifnot(identical(unique(pbp$pbp_model_id), cur_id))  # the read-back is the new file
  save_to_release(pbp[, intersect(chart_cols, names(pbp)), drop = FALSE],
                  paste0("ep_wp_chart_", s, "_all"), "ep_wp_chart-data")
  say(s, ": xg ", nrow(xg_df), " matches, chart ", nrow(pbp), " rows")
  rm(xg_df, pbp); gc()
}
stale <- .stale_pbp_seasons(cur_id, .read_pbp_model_stamps(), seasons)
if (length(stale)) stop("seasons still stale after publishing: ", paste(stale, collapse = ","))
say("PUBLISHED. Previous play-by-play kept in ", bak, ". Next: the rating vintage (header, steps 1-4).")
