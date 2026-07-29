# Ratings Pipeline
#
# End-to-end script for computing all TORP ratings:
#   Stage 1: Refresh upstream data (player_stats, teams) from AFL API
#   Stage 2: Build player game data from PBP + player_stats + teams
#   Stage 3: Compute EPR ratings per season/round and release
#   Stage 4: Compute team ratings
#   Stage 5: Player game & season ratings (EPV + PSV → torp_value)
#   Stage 6: Compute & release PSR
#
# Usage:
#   Rscript data-raw/03-ratings/run_ratings_pipeline.R
#   Or: source("data-raw/03-ratings/run_ratings_pipeline.R")
#
# CI Usage (from GitHub Actions):
#   Set config variables before sourcing:
#     SEASONS <- NULL; REBUILD_PLAYER_GAME <- TRUE
#   Then source this file - it will skip setting defaults if they already exist.

# Setup ----

library(dplyr)
library(cli)
library(tictoc)
library(piggyback)

devtools::load_all()

# versebus §1.5: pipeline entry points run strict + disable piggyback's
# asset-list memoisation (torp P6) + clear negative-cache markers so a
# previous run's transient failure can't suppress this run's re-check.
Sys.setenv(VERSEBUS_STRICT = "1")
Sys.setenv(piggyback_cache_duration = 1)
clear_skip_markers()

# Source daily_release.R into a local env to get update_player_stats() and
# update_teams() without leaking .release_cache and other globals.
.daily_release_env <- new.env(parent = globalenv())
source(here::here("data-raw/01-data/daily_release.R"), local = .daily_release_env)
update_player_stats <- .daily_release_env$update_player_stats
update_teams <- .daily_release_env$update_teams
get_start_round <- .daily_release_env$get_start_round
get_max_round <- .daily_release_env$get_max_round

# Configuration ----
# These defaults are only set if not already defined (allows CI to override)

# Which seasons to process:
#   NULL          = current season only
#   numeric vector = specific seasons (e.g. 2024:2025)
#   TRUE          = all seasons 2021+
if (!exists("SEASONS", envir = .GlobalEnv)) SEASONS <- TRUE

# Whether to re-fetch player_stats + teams from AFL API
if (!exists("REFRESH_UPSTREAM", envir = .GlobalEnv)) REFRESH_UPSTREAM <- TRUE

# Whether to rebuild player game tables from PBP
if (!exists("REBUILD_PLAYER_GAME", envir = .GlobalEnv)) REBUILD_PLAYER_GAME <- TRUE

# Full rebuild vs incremental (only configured seasons)
if (!exists("REBUILD_ALL_RATINGS", envir = .GlobalEnv)) REBUILD_ALL_RATINGS <- TRUE

# Which rating vintage this run publishes (decision D-DEF3, see
# docs/plans/RATING-VERSIONING-PLAN.md). NULL = the canonical
# torp_ratings.parquet, i.e. exactly today's behaviour and the safe default.
# Set to a label such as "v2" to publish a CANDIDATE vintage alongside
# canonical without touching it. Promotion is a separate, deliberate act --
# this script must never promote.
if (!exists("RATINGS_VINTAGE", envir = .GlobalEnv)) RATINGS_VINTAGE <- NULL
# NOTE: these must NOT be dot-prefixed. A variable named `.foo` interpolated
# into cli::cli_abort()/cli_warn() collides with cli's own `{.val}`-style
# markup and hard-errors at runtime -- it crashed a real production run once.
vintage_file <- torp:::.rating_vintage_file(RATINGS_VINTAGE)
vintage_stem <- torp:::.rating_vintage_stem(RATINGS_VINTAGE)
if (!is.null(RATINGS_VINTAGE)) {
  cli::cli_alert_info(
    "Publishing CANDIDATE rating vintage {.val {RATINGS_VINTAGE}} to {.file {vintage_file}} -- canonical torp_ratings.parquet is untouched"
  )
}

# Resolve seasons ----

resolve_seasons <- function(seasons) {
  if (is.null(seasons)) return(get_afl_season())
  if (isTRUE(seasons)) return(2021:get_afl_season())
  seasons
}

seasons <- resolve_seasons(SEASONS)

cli::cli_h1("EPR Ratings Pipeline")
cli::cli_inform("Seasons: {paste(seasons, collapse = ', ')}")
cli::cli_inform("Refresh upstream: {REFRESH_UPSTREAM}")
cli::cli_inform("Rebuild player game: {REBUILD_PLAYER_GAME}")
cli::cli_inform("Rebuild all ratings: {REBUILD_ALL_RATINGS}")

# Stage 1: Refresh Upstream Data ----

if (REFRESH_UPSTREAM) {
  cli::cli_h2("Stage 1: Refresh Upstream Data")
  tictoc::tic("stage_1_upstream")

  for (s in seasons) {
    tryCatch({
      cli::cli_progress_step("Refreshing player_stats and teams for {s}")
      update_player_stats(s)
      update_teams(s)
    }, error = function(e) {
      cli::cli_alert_danger("Failed to refresh upstream for {s}: {conditionMessage(e)}")
    })
  }

  tictoc::toc(log = TRUE)
} else {
  cli::cli_alert_info("Skipping Stage 1 (REFRESH_UPSTREAM = FALSE)")
}

# Stage 2: Build Player Game Data + Release ----

stage2_failed_seasons <- character()

if (REBUILD_PLAYER_GAME) {
  cli::cli_h2("Stage 2: Build Player Game Data")
  tictoc::tic("stage_2_player_game")

  # Batch load all seasons at once (parallel download via curl::multi_download)
  cli::cli_progress_step("Batch loading PBP, player_stats, teams for {length(seasons)} seasons")
  all_pbp <- load_pbp(seasons, rounds = TRUE)
  all_chains <- load_chains(seasons, rounds = TRUE)
  all_pstats <- load_player_stats(seasons)
  all_teams <- load_teams(seasons)
  cli::cli_inform("  Loaded: PBP {nrow(all_pbp)} | chains {nrow(all_chains)} | player_stats {nrow(all_pstats)} | teams {nrow(all_teams)}")

  for (s in seasons) {
    tryCatch({
      cli::cli_progress_step("Building player game data for {s}")

      pbp <- all_pbp[all_pbp$season == s, ]
      chains <- all_chains[all_chains$season == s, ]
      pstats <- all_pstats[all_pstats$season == s, ]
      teams_data <- all_teams[all_teams$season == s, ]

      cli::cli_inform("  PBP: {nrow(pbp)} rows | player_stats: {nrow(pstats)} rows | teams: {nrow(teams_data)} rows")
      if (nrow(pbp) == 0) {
        cli::cli_alert_danger("No PBP data for {s} - skipping")
        stage2_failed_seasons <- c(stage2_failed_seasons, as.character(s))
        next
      }

      pgd <- create_player_game_data(pbp, pstats, teams_data, chains = chains)
      cli::cli_inform("  Player game data: {nrow(pgd)} rows")

      file_name <- paste0("player_game_", s)
      save_to_release(pgd, torp:::.vintage_asset_stem(file_name, RATINGS_VINTAGE), "player_game-data")
      cli::cli_alert_success("Released {file_name} ({nrow(pgd)} rows)")
    }, error = function(e) {
      cli::cli_alert_danger("Failed to build player game data for {s}: {conditionMessage(e)}")
      stage2_failed_seasons <<- c(stage2_failed_seasons, as.character(s))
    })
  }

  if (length(stage2_failed_seasons) > 0) {
    cli::cli_alert_danger("Stage 2 failed for seasons: {paste(stage2_failed_seasons, collapse = ', ')} - Stage 3 will use stale player game data for these")
  }

  tictoc::toc(log = TRUE)
} else {
  cli::cli_alert_info("Skipping Stage 2 (REBUILD_PLAYER_GAME = FALSE)")
}

# Stage 3: Compute EPR Ratings + Release ----

cli::cli_h2("Stage 3: Compute EPR Ratings")
tictoc::tic("stage_3_ratings")

# Reload player game data fresh (Stage 2 may have uploaded new files with
# additional columns like WPA; the in-memory data from Stage 2's load_pbp
# is stale relative to the new player_game releases)
cli::cli_progress_step("Loading all player game data")
clear_all_cache()

# Stage 2 just wrote these files to the release, so Stage 3 must read what it
# wrote. load_player_game_data() prefers a local torpdata/data sibling when one
# exists (get_local_data_dir()), which on a developer machine can be months
# stale for some seasons -- and a stale season silently destroys its ratings
# (see adjust_epv_for_opponents()'s guard, and the 2026-07-27 incident where
# local 2022/2025 were three months old and both seasons' ratings deflated).
#
# Point the loader at the release for the duration of this stage.
# Wrapped in a function deliberately: on.exit() only attaches to a function
# frame. Registered at script top level it never fires, so an error inside the
# load would leave torp.local_data_dir pointing somewhere wrong for the rest of
# the session -- silently disabling local data for every later call, with
# nothing pointing at the cause. This script is documented as source()-able, so
# that leak is a realistic path.
#
# NA is the explicit disable sentinel (see get_local_data_dir()). Setting a
# non-existent path does NOT work: it falls through to the sibling auto-detect
# and re-finds the same directory.
.load_pgd_from_release <- function() {
  local_dir <- get_local_data_dir()
  if (!is.null(local_dir)) {
    cli::cli_alert_info(
      "Ignoring local data dir {.path {local_dir}} for Stage 3 -- reading the release Stage 2 just wrote")
  }
  withr::local_options(list(torp.local_data_dir = NA))
  stopifnot(is.null(get_local_data_dir()))   # assert the bypass actually took
  load_player_game_data(TRUE)
}
all_pgd <- .load_pgd_from_release()
cli::cli_inform("Player game data loaded: {nrow(all_pgd)} rows, {ncol(all_pgd)} cols")

# Schema-consistency check across seasons: a mixed-vintage load (some seasons
# pre-rename, some post) is exactly what produced the 2026-07-27 incident, and
# it is invisible in row counts.
.adj_cols <- grep("_adj$", names(all_pgd), value = TRUE)
if (length(.adj_cols) > 0 && "season" %in% names(all_pgd)) {
  .cov <- data.table::as.data.table(all_pgd)[
    , lapply(.SD, function(x) mean(!is.na(x))), by = "season", .SDcols = .adj_cols]
  .mixed <- .adj_cols[vapply(.adj_cols, function(cc) {
    v <- .cov[[cc]]; any(v < 0.01) && any(v > 0.99)
  }, logical(1))]
  if (length(.mixed) > 0) {
    # Assumption worth knowing if this ever false-positives: today every
    # _adj-suffixed column is produced together in one mutate() block in
    # create_player_game_data(), so none is legitimately season-limited. If a
    # metric is ever added that only exists from some season onward, this
    # generic threshold would abort every rebuild and needs an allowlist.
    cli::cli_abort(c(
      "Mixed-vintage player game data: column{?s} {.val {.mixed}} {?is/are} populated for some seasons and empty for others.",
      "x" = "Rebuilding on this would silently deflate the empty seasons' ratings.",
      "i" = "Per-season coverage:\n{paste(utils::capture.output(print(.cov)), collapse = '\n')}"
    ), class = c("torp_error_stale_input", "vb_error_integrity"))
  }
}

# Convert to keyed data.table once — avoids full copy on every round call
data.table::setDT(all_pgd)

# Apply EPV opponent adjustment — adds _oadj columns used by EPR
cli::cli_progress_step("Applying EPV opponent adjustment")
all_pgd <- adjust_epv_for_opponents(all_pgd)

# Positional LEVEL correction, applied here and not earlier. It has to run
# after the opponent adjustment (which would otherwise reintroduce a level on
# top of it) and before both consumers -- EPR aggregation below and
# .compute_player_game_ratings() in stage 5 -- so a single call covers the
# published ratings and the per-game displays.
if (isTRUE(EPV_LEVEL_CENTRE)) {
  cli::cli_progress_step("Centring EPV on listed-position levels")
  all_pgd <- centre_epv_by_position(all_pgd)

  # Verify the invariant that makes this worth doing: the TOG-weighted sum per
  # (season, round, bucket) is what EPR's numerator accumulates, so it is the
  # thing that must vanish -- not the unweighted mean, which would look centred
  # while EPR stayed skewed.
  .lc <- if (all(paste0(EPV_LEVEL_CENTRE_CHANNELS, "_oadj") %in% names(all_pgd)))
    "_oadj" else "_adj"
  .chk <- data.table::as.data.table(all_pgd)
  .chk[, `:=`(pos_bucket = torp:::.collapse_listed_position(position_group),
              w = pmax(dplyr::coalesce(time_on_ground_percentage / 100, 0.1), 0.1))]
  .chk <- .chk[!is.na(pos_bucket)]
  # Fail CLOSED. chk_worst starts at 0 and is only ever RAISED, so if every cell
  # is empty -- position_group missing or renamed, or every value unmapped --
  # the loop never runs, chk_worst stays 0, the abort below is skipped, and the
  # script reports "verified (max |cell mean| = 0)" having verified nothing.
  # That is the same shape as the bugs this guard exists to catch, one level up:
  # the GUARD degrading to a no-op rather than the normalisation.
  #
  # NOTE the names here are deliberately NOT dot-prefixed. cli >= 3.4.0 treats
  # `{.name}` as style markup, so a bare `{.checked}` in any cli_* string is a
  # hard error ("Invalid cli literal: starts with a dot") -- it took this
  # pipeline down on 2026-07-29 AFTER the centring had already succeeded, i.e.
  # the guard crashed on reporting a pass. Same bug previously hit
  # check_manifest_sync(). Bare `{signif(chk_worst, 3)}` is safe either way
  # because it starts with a function call, which is exactly why the abort paths
  # survived and only the success path blew up -- the failure mode is invisible
  # until the happy path runs.
  if (nrow(.chk) == 0) {
    cli::cli_abort(c(
      "Cannot verify EPV level centring: no row has a mapped position bucket.",
      "x" = "Refusing to build ratings on EPV whose centring cannot be checked."
    ))
  }
  chk_worst <- 0
  chk_cells <- 0L
  for (cc in paste0(EPV_LEVEL_CENTRE_CHANNELS, .lc)) {
    m <- .chk[is.finite(get(cc)),
              .(wm = stats::weighted.mean(get(cc), w)),
              by = .(season, round, pos_bucket)]
    if (nrow(m) > 0) { chk_worst <- max(chk_worst, max(abs(m$wm), na.rm = TRUE)); chk_cells <- chk_cells + nrow(m) }
  }
  if (chk_cells == 0L) {
    cli::cli_abort(c(
      "Cannot verify EPV level centring: zero cells had a finite channel value.",
      "x" = "An empty check is not a pass."
    ))
  }
  if (!is.finite(chk_worst) || chk_worst > 1e-8) {
    cli::cli_abort(c(
      "EPV level centring did not take: max |TOG-weighted cell mean| = {signif(chk_worst, 3)}",
      "x" = "Refusing to build ratings on EPV that is not centred as claimed."
    ))
  }
  cli::cli_alert_success("EPV level centring verified across {chk_cells} cell{?s} (max |cell mean| = {signif(chk_worst, 3)})")
  rm(.chk, chk_worst, chk_cells, .lc)
}

data.table::setkey(all_pgd, match_id)

# Pre-load shared data once — avoids ~145 redundant loads per full rebuild
cli::cli_progress_step("Pre-loading shared reference data")
shared_stat_ratings <- tryCatch(get_player_stat_ratings(current = FALSE), error = function(e) {
  cli::cli_alert_danger("Could not load stat ratings: {conditionMessage(e)}")
  NULL
})
# Backwards compat: rename time_on_ground_rating → cond_tog_rating if needed
if (!is.null(shared_stat_ratings)) {
  if (!"cond_tog_rating" %in% names(shared_stat_ratings) && "time_on_ground_rating" %in% names(shared_stat_ratings)) {
    shared_stat_ratings$cond_tog_rating <- shared_stat_ratings$time_on_ground_rating
  }
  # Also handle old _skill column names during transition
  if (!"cond_tog_rating" %in% names(shared_stat_ratings) && "cond_tog_skill" %in% names(shared_stat_ratings)) {
    shared_stat_ratings$cond_tog_rating <- shared_stat_ratings$cond_tog_skill
  }
  if (!"squad_selection_rating" %in% names(shared_stat_ratings)) {
    if ("squad_selection_skill" %in% names(shared_stat_ratings)) {
      shared_stat_ratings$squad_selection_rating <- shared_stat_ratings$squad_selection_skill
    } else {
      cli::cli_alert_danger("Stat ratings missing squad_selection_rating; using cond_tog_rating alone as pred_tog")
      shared_stat_ratings$squad_selection_rating <- 1
    }
  }
}
shared_fixtures <- load_fixtures(TRUE)

# Stage 3's per-season builder now lives in the package as
# torp:::.build_epr_season() so it can be called WITHOUT publishing --
# see R/ratings_build.R and build_ratings_history(). It was defined here,
# inside a script that writes to GitHub Releases, which meant the only way
# to get a ratings history was to publish one.

torp_season_list <- list()
failed_seasons <- character()
empty_seasons <- character()

for (s in seasons) {
  tryCatch({
    start_round <- if (s >= 2024) 0 else 1
    max_round <- if (s == get_afl_season()) {
      get_afl_week(type = "next")
    } else {
      28
    }

    cli::cli_h3("Computing ratings for {s} (rounds {start_round}-{max_round})")
    tictoc::tic(paste0("ratings_", s))

    torp_df <- torp:::.build_epr_season(s, start_round:max_round, all_pgd, shared_stat_ratings, shared_fixtures)
    cli::cli_inform("  {s}: {nrow(torp_df)} rating rows")

    if (nrow(torp_df) == 0) {
      empty_seasons <<- c(empty_seasons, as.character(s))
    }

    torp_season_list[[as.character(s)]] <- torp_df

    tictoc::toc(log = TRUE)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to compute ratings for {s}: {conditionMessage(e)}")
    failed_seasons <<- c(failed_seasons, as.character(s))
  })
}

torp_new <- dplyr::bind_rows(torp_season_list)
cli::cli_inform("New ratings computed: {nrow(torp_new)} rows")

if (length(failed_seasons) > 0) {
  cli::cli_alert_danger("Failed seasons: {paste(failed_seasons, collapse = ', ')}")
}
if (length(empty_seasons) > 0) {
  cli::cli_alert_info("Empty seasons (no data available yet): {paste(empty_seasons, collapse = ', ')}")
}
if (length(failed_seasons) == length(seasons)) {
  cli::cli_abort("All seasons failed to compute - aborting pipeline")
}
if (nrow(torp_new) == 0 && length(failed_seasons) == 0) {
  cli::cli_alert_info("No new ratings computed (pre-season or no games played yet) - exiting gracefully")
}

if (nrow(torp_new) > 0) {
  # Load existing full-history ratings unconditionally (needed both for the
  # incremental upsert AND as a floor-guard reference when REBUILD_ALL_RATINGS
  # is set). A transient read failure must never be treated as "no existing
  # ratings" -- torp P1/P8: that collapses to overwriting full-history
  # ratings-data with just the seasons computed this run.
  existing <- tryCatch(
    load_torp_ratings(version = RATINGS_VINTAGE),
    vb_error_absent = function(e) NULL,
    error = function(e) {
      cli::cli_abort("Could not load existing torp_ratings ({conditionMessage(e)}) - aborting to avoid overwriting full-history ratings-data", parent = e)
    }
  )
  if (!is.null(existing) && nrow(existing) == 0) existing <- NULL

  if (!REBUILD_ALL_RATINGS) {
    cli::cli_progress_step("Incremental update: upserting into existing ratings")

    if (!is.null(existing)) {
      # Deduplicate both sides by row_id (keeps first occurrence)
      n_dup_existing <- sum(duplicated(existing$row_id))
      n_dup_new <- sum(duplicated(torp_new$row_id))
      if (n_dup_existing > 0) {
        cli::cli_alert_danger("Existing ratings had {n_dup_existing} duplicate row_id{?s} (keeping first)")
      }
      if (n_dup_new > 0) {
        cli::cli_alert_danger("New ratings had {n_dup_new} duplicate row_id{?s} (keeping first)")
      }
      existing <- dplyr::distinct(existing, row_id, .keep_all = TRUE)
      torp_new <- dplyr::distinct(torp_new, row_id, .keep_all = TRUE)
      torp_df_total <- existing |>
        dplyr::rows_upsert(torp_new, by = "row_id")
      if (nrow(torp_df_total) < nrow(existing)) {
        cli::cli_abort("Row count decreased after upsert ({nrow(existing)} -> {nrow(torp_df_total)}) - possible data corruption")
      }
      cli::cli_inform("Upserted into existing: {nrow(torp_df_total)} total rows (was {nrow(existing)})")
    } else {
      # Confirmed absent (first-ever publish) -- fresh build is legitimate.
      is_absent <- tryCatch(
        # Per-VINTAGE, not per-tag: publishing a candidate must neither trip
        # the guard protecting canonical nor bypass its own.
        vb_confirm_absent(get_torp_data_repo(), "ratings-data", vintage_file),
        error = function(e) {
          cli::cli_abort("Could not verify {vintage_file} is absent before a fresh upload: {conditionMessage(e)}")
        }
      )
      if (!isTRUE(is_absent)) {
        cli::cli_abort("Refusing fresh ratings upload: {vintage_file} was not confirmed absent from ratings-data.")
      }
      torp_df_total <- torp_new
    }
  } else {
    # REBUILD_ALL_RATINGS: torp_new is expected to already cover full history
    # (SEASONS == TRUE). Floor-guard against the existing release regardless
    # -- a shrink here means the just-computed "full" set is actually partial.
    torp_df_total <- torp_new
  }

  if (!is.null(existing)) {
    vb_guard_accumulate(existing, torp_df_total, floor = 0.9)
  }

  # Blend PSR into ratings so the release has torp/psr/osr/dsr columns.
  # torp_df_total is always the full historical table (upserted into the
  # existing release above), so PSR must cover ALL seasons too -- otherwise
  # historical rows find no per-round match in calculate_torp() and fall back
  # to the current snapshot, leaving psr/osr/dsr flat across history (#88).
  psr_df <- tryCatch({
    stat_ratings <- load_player_stat_ratings(TRUE)
    .compute_psr_from_stat_ratings(stat_ratings)
  }, error = function(e) {
    cli::cli_warn("Could not compute PSR for release: {e$message}")
    NULL
  })
  # Position-centre EPR BEFORE blending, so `torp` is built from the same
  # centred channels it will be published alongside. Centring afterwards would
  # leave torp derived from uncentred EPR and silently inconsistent with it.
  if (isTRUE(EPR_POSITION_CENTRE)) {
    torp_df_total <- centre_epr_by_position(torp_df_total)

    # Check EVERY (season, round, position) cell, not just the latest. Centring
    # runs across all history, so sampling one round would let an earlier round
    # fail silently -- and it is the historical rounds that feed model training.
    #
    # This checks the weighted mean of `epr` (the total) while centring is
    # applied per CHANNEL. Those agree exactly only because the four channels
    # are finite together or NA together (verified 2026-07-28: 107,448 rows all
    # finite, 23,480 all NA, ZERO partial), so every channel's mean is taken
    # over the same players and their sum is the total's mean. That invariant
    # holds by data, not by contract. If partial-NA rows ever appear the check
    # fails loud rather than passing something wrong -- the skew would exceed
    # the tolerance below -- but the message would be misleading, so start here.
    # Group by the SAME collapsed bucket centre_epr_by_position() used. Keying
    # this on raw position_group while centring uses the 6-way map would fail
    # every run for the two merged forward groups -- each is only mean-zero
    # jointly. A guard that groups differently from the code it guards is not a
    # guard.
    chk <- data.table::as.data.table(torp_df_total)
    chk[, pos_bucket := torp:::.collapse_listed_position(position_group)]
    chk <- chk[
      !is.na(pos_bucket),
      .(wmean = stats::weighted.mean(epr, pmax(pred_tog, 0.01), na.rm = TRUE),
        n = .N, n_rated = sum(is.finite(epr))),
      by = .(season, round, pos_bucket)]

    # A cell where NOBODY is rated yet has nothing to centre and no mean to
    # check -- that is the start of the dataset, not a failure. 23,480 rows
    # (~18%) carry an NA channel and therefore an NA epr, which is pre-existing
    # published behaviour; in early 2021 a few whole position-rounds are NA.
    # Skip those, but COUNT them, so "nothing was checkable" can never be
    # mistaken for "everything checked out".
    unrated <- chk[n_rated == 0]
    if (nrow(unrated) > 0) {
      cli::cli_inform(
        "Position centring: {nrow(unrated)} cell{?s} have no rated players (earliest: season {unrated$season[1]} round {unrated$round[1]} {unrated$pos_bucket[1]}) -- nothing to verify there")
    }
    # Known, accepted limitation: a cell with n_rated == 1 passes vacuously --
    # the weighted mean of one point IS that point, so subtracting it leaves
    # exactly 0 whatever the grouping logic did. Early rounds are where such
    # cells live, so the guard's power is weakest precisely where this filter
    # newly admits cells. Left as-is because the failure this guard exists to
    # catch (a whole taxonomy or channel not centring) shows up across many
    # cells at once, not in a single-player one.
    chk <- chk[n_rated > 0]
    # Fail CLOSED. An empty check is not a pass: zero rows here means nothing
    # had a position group, which is exactly the state in which centring cannot
    # have happened.
    if (nrow(chk) == 0) {
      cli::cli_abort(c(
        "Cannot verify EPR position centring: no position bucket has a single rated player.",
        "x" = "Refusing to publish ratings whose centring cannot be checked."
      ))
    }
    if (!all(is.finite(chk$wmean))) {
      bad <- chk[!is.finite(wmean)]
      cli::cli_abort(c(
        "EPR position centring produced {nrow(bad)} non-finite cell mean{?s}.",
        "i" = "First: season {bad$season[1]} round {bad$round[1]} {bad$pos_bucket[1]}"
      ))
    }
    worst <- max(abs(chk$wmean))
    if (worst > 1e-6) {
      b <- chk[which.max(abs(wmean))]
      cli::cli_abort(c(
        "EPR position centring did not take: max |weighted mean| = {signif(worst, 3)}",
        "i" = "Worst cell: season {b$season} round {b$round} {b$pos_bucket} (n = {b$n})",
        "x" = "Refusing to publish ratings whose positions are not centred as claimed."
      ))
    }
    cli::cli_alert_success(
      "Position centring verified across {nrow(chk)} (season, round, position) cell{?s}")
  }

  if (!is.null(psr_df) && nrow(psr_df) > 0 && "psr" %in% names(psr_df)) {
    torp_df_total <- calculate_torp(torp_df_total, psr_df)
    cli::cli_alert_success("Blended PSR into ratings ({sum(!is.na(torp_df_total$torp))} rows with torp)")
  }

  save_to_release(torp_df_total, vintage_stem, "ratings-data")

  uploaded <- tryCatch(load_torp_ratings(version = RATINGS_VINTAGE), error = function(e) NULL)
  if (is.null(uploaded) || nrow(uploaded) != nrow(torp_df_total)) {
    cli::cli_alert_danger("Upload verification failed - piggyback cache delay may be the cause")
  }
  cli::cli_alert_success("Released {vintage_stem} ({nrow(torp_df_total)} rows)")

  # Provenance: record which constants produced this vintage. Never sets
  # `canonical` -- promotion is deliberate and separate.
  tryCatch(
    # The vintage label comes from the CONSTANTS (RATING_VINTAGE), not from the
    # filename. Regenerating canonical under new constants writes
    # torp_ratings.parquet while the vintage is "v2" -- deriving the label from
    # the filename would record that file as v1, i.e. label the new data as the
    # data it replaced. canonical is set only when this run wrote canonical.
    torp:::publish_ratings_manifest(
      nrow(torp_df_total),
      version = torp:::RATING_VINTAGE,
      file = vintage_file,
      set_canonical = is.null(RATINGS_VINTAGE)
    ),
    error = function(e) cli::cli_warn("Could not publish ratings manifest: {conditionMessage(e)}")
  )
}

tictoc::toc(log = TRUE)

# Stage 4: Compute Team Ratings ----

cli::cli_h2("Stage 4: Compute Team Ratings")
tictoc::tic("stage_4_team_ratings")

tryCatch({
  # Use the just-released torp_ratings (or the one we built above)
  ratings_for_teams <- if (exists("torp_df_total") && nrow(torp_df_total) > 0) {
    torp_df_total
  } else {
    load_torp_ratings()
  }

  cli::cli_inform("Building team ratings from {nrow(ratings_for_teams)} player rating rows")

  team_ratings <- ratings_for_teams |>
    dplyr::filter(!is.na(.data$epr)) |>
    dplyr::group_by(.data$season, .data$round, .data$team) |>
    dplyr::mutate(
      # Scale pred_tog to sum to 18 per team (18 full-game equivalents)
      team_tog_sum = sum(.data$pred_tog, na.rm = TRUE),
      tog_wt = dplyr::if_else(.data$team_tog_sum > 0,
                               .data$pred_tog * 18 / .data$team_tog_sum, 0)
    ) |>
    dplyr::summarise(
      team_epr     = round(sum(.data$epr * .data$tog_wt, na.rm = TRUE), 2),
      team_epr_recv    = round(sum(.data$epr_recv * .data$tog_wt, na.rm = TRUE), 2),
      team_epr_disp    = round(sum(.data$epr_disp * .data$tog_wt, na.rm = TRUE), 2),
      team_epr_spoil   = round(sum(.data$epr_spoil * .data$tog_wt, na.rm = TRUE), 2),
      team_epr_hitout  = round(sum(.data$epr_hitout * .data$tog_wt, na.rm = TRUE), 2),
      top_player   = .data$player_name[which.max(.data$epr)],
      top_epr      = round(max(.data$epr, na.rm = TRUE), 2),
      n_players    = sum(.data$pred_tog > 0),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$season, .data$round, -.data$team_epr)

  save_to_release(team_ratings, torp:::.vintage_asset_stem("team_ratings", RATINGS_VINTAGE), "team_ratings-data")
  cli::cli_alert_success("Released team_ratings ({nrow(team_ratings)} rows)")
}, error = function(e) {
  cli::cli_alert_danger("Failed to compute team ratings: {conditionMessage(e)}")
})

tictoc::toc(log = TRUE)

# Stage 5: Compute Player Game & Season Ratings ----

cli::cli_h2("Stage 5: Player Game & Season Ratings")
tictoc::tic("stage_5_derived_ratings")

# all_pstats is only batch-loaded in Stage 2 (REBUILD_PLAYER_GAME); load it
# here too so Stage 5 doesn't fail when that stage was skipped.
if (!exists("all_pstats")) {
  all_pstats <- load_player_stats(seasons)
}

for (s in seasons) {
  tryCatch({
    start_round <- get_start_round(s)
    max_round <- get_max_round(s)

    pgd <- all_pgd[all_pgd$season == s, ]
    if (nrow(pgd) == 0) next

    # Player game ratings (EPV-based)
    pgr <- .compute_player_game_ratings(pgd, s, start_round:max_round)

    # Add PSV columns from box-score stats
    pstats_season <- data.table::as.data.table(all_pstats[all_pstats$season == s, ])
    if (nrow(pstats_season) > 0) {
      # Ensure tog column exists (PSV expects fraction 0-1)
      if (!"tog" %in% names(pstats_season) && "time_on_ground_percentage" %in% names(pstats_season)) {
        pstats_season[, tog := pmax(time_on_ground_percentage / 100, 0.1)]
      }
      # Carry position_group from EPV so PSV can center by position
      if (!"position_group" %in% names(pstats_season) && "position_group" %in% names(pgr)) {
        pg_map <- unique(data.table::as.data.table(pgr)[, .(player_id, match_id, position_group)])
        pstats_season <- merge(pstats_season, pg_map,
                               by = intersect(c("player_id", "match_id"), names(pstats_season)),
                               all.x = TRUE)
      }
      # Apply per-game stat opponent adjustment before PSV
      pstats_season <- tryCatch({
        adjust_stats_for_opponents(pstats_season)
      }, error = function(e) {
        cli::cli_warn("Stat opponent adjustment failed for {s}: {conditionMessage(e)}")
        pstats_season
      })
      psv_result <- tryCatch({
        .compute_psv(pstats_season)
      }, error = function(e) {
        cli::cli_warn("PSV computation failed for {s}: {conditionMessage(e)}")
        NULL
      })
      if (!is.null(psv_result)) {
        # Per-game (psv/osv/dsv) + centered per-80 (psv_p80/osv_p80/dsv_p80),
        # both supplied directly by calculate_psv() (issue #80).
        psv_cols <- intersect(
          c("psv", "osv", "dsv", "psv_p80", "osv_p80", "dsv_p80"),
          names(psv_result)
        )
        if (length(psv_cols) > 0 && "player_id" %in% names(psv_result) &&
            "match_id" %in% names(psv_result)) {
          psv_slim <- psv_result[, c("player_id", "match_id", psv_cols), with = FALSE]
          pgr <- merge(pgr, psv_slim, by = c("player_id", "match_id"), all.x = TRUE)
          # psv is per-game (psv_p80 * tog), on the same scale as epv
          cli::cli_inform("  Added PSV columns to game ratings ({sum(!is.na(pgr$psv))} matched)")
        }
      }
    }

    # Compute per-game TORP value: 50% EPV + 50% PSV (parallels career TORP = 50% EPR + 50% PSR)
    if (all(c("epv", "psv") %in% names(pgr))) {
      pgr$torp_value <- round(
        TORP_EPR_WEIGHT * pgr$epv + (1 - TORP_EPR_WEIGHT) * pgr$psv,
        1
      )
      pgr$torp_value_p80 <- round(pgr$torp_value / pgr$tog, 1)
    }

    file_name <- paste0("player_game_ratings_", s)
    save_to_release(pgr, torp:::.vintage_asset_stem(file_name, RATINGS_VINTAGE), "player_game_ratings-data")
    cli::cli_alert_success("Released {file_name} ({nrow(pgr)} rows)")

    # Player season ratings
    psr <- .compute_player_season_ratings(pgr)
    file_name <- paste0("player_season_ratings_", s)
    save_to_release(psr, torp:::.vintage_asset_stem(file_name, RATINGS_VINTAGE), "player_season_ratings-data")
    cli::cli_alert_success("Released {file_name} ({nrow(psr)} rows)")
  }, error = function(e) {
    cli::cli_alert_danger("Failed derived ratings for {s}: {conditionMessage(e)}")
  })
}

tictoc::toc(log = TRUE)

# Stage 6: Compute & Release PSR ----

cli::cli_h2("Stage 6: Compute & Release PSR")
tictoc::tic("stage_6_psr")

tryCatch({
  stat_ratings <- load_player_stat_ratings(TRUE)
  psr_coef_path <- file.path("data-raw", "cache-stat-ratings", "psr_coefficients.csv")
  if (!file.exists(psr_coef_path)) {
    psr_coef_path <- system.file("extdata", "psr_coefficients.csv", package = "torp")
  }
  if (file.exists(psr_coef_path) && nchar(psr_coef_path) > 0) {
    coef_df <- utils::read.csv(psr_coef_path)
    psr_all <- calculate_psr(stat_ratings, coef_df)
    cli::cli_inform("PSR computed for {nrow(psr_all)} player-rounds across {length(unique(psr_all$season))} seasons")

    for (s in sort(unique(psr_all$season))) {
      psr_season <- psr_all[psr_all$season == s, ]
      file_name <- paste0("psr_", s)
      save_to_release(psr_season, torp:::.vintage_asset_stem(file_name, RATINGS_VINTAGE), "psr-data")
      cli::cli_alert_success("Released {file_name} ({nrow(psr_season)} rows)")
    }
  } else {
    cli::cli_warn("PSR coefficient file not found - skipping PSR release")
  }
}, error = function(e) {
  cli::cli_alert_danger("Failed to compute/release PSR: {conditionMessage(e)}")
})

tictoc::toc(log = TRUE)

# Summary ----

cli::cli_h2("Pipeline Complete")
timings <- tictoc::tic.log(format = TRUE)
for (t in timings) {
  cli::cli_inform(t)
}
tictoc::tic.clearlog()
