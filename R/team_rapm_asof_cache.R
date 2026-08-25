# Disk-backed memoization for the as-of RAPM/SPM engine (team_rapm_asof.R).
# =========================================================================
# Added 2026-08-25 after the decay-xRAPM investigation re-ran the same
# ~13s/snapshot AFLM backfill roughly six times in one night (standalone
# halflife sweep, full backfill, three-way gate x2, cadence-matched control).
# The original design (AFL-DECAY-XRAPM-PLAN.md sec1) specified resume-safe
# caching, mirroring panna's driver -- this file is that piece, built after
# the fact. See AFL-DECAY-XRAPM-PLAN.md sec13 for the measured speedup.
#
# What is cached: the two expensive, always-repeated units per checkpoint --
# (1) build_team_rapm_asof() + fit_team_rapm_asof() + extract_team_rapm_ratings(),
# and (2) fit_team_spm_asof() -- not the cheap helper functions around them.
#
# CACHE KEY, and its one real limitation: the key hashes every call argument
# that affects the result PLUS a cheap data-freshness signal (row count and
# max match date of the relevant comp's results), so a stale data pull or a
# different halflife/season/etc. cannot silently return the wrong answer.
# It does NOT hash the fitting functions' own source code -- editing the
# logic inside build_team_rapm_asof()/fit_team_rapm_asof()/fit_team_spm_asof()
# and re-running with the same arguments WOULD silently serve a pre-edit
# cached result. .TEAM_RAPM_ASOF_CACHE_VERSION below is the explicit guard
# for that: bump it whenever those functions' logic changes. This is a
# deliberate, visible gate rather than an automatic one because hashing R
# function bodies reliably (deparsed source, srcref-independent) is more
# machinery than a sandbox/investigation cache needs -- but an un-bumped
# version after a real logic change is exactly the kind of silent-staleness
# bug this session spent all night hunting elsewhere, so do not skip it.

.TEAM_RAPM_ASOF_CACHE_VERSION <- 1L

# .team_rapm_asof_cache_dir ----

#' Local, gitignored cache directory for as-of RAPM/SPM snapshots.
#' Override with the TORP_ASOF_CACHE_DIR env var (e.g. a temp dir in tests).
#' @keywords internal
.team_rapm_asof_cache_dir <- function() {
  dir <- Sys.getenv("TORP_ASOF_CACHE_DIR", file.path("data-raw", "cache-team-rapm-asof"))
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

# .team_rapm_asof_cache_enabled ----

#' Whether as-of caching is active. TORP_ASOF_CACHE=0 (or "false"/"no")
#' forces every call to recompute -- e.g. while actively editing the fitting
#' logic, so a stale hit can't be mistaken for a real result.
#' @keywords internal
.team_rapm_asof_cache_enabled <- function() {
  !(tolower(Sys.getenv("TORP_ASOF_CACHE", "1")) %in% c("0", "false", "no"))
}

# .team_rapm_asof_cache_key ----

#' Build a stable, content-addressed cache key: every argument that affects
#' the result, the cache-version guard, and a cheap data-freshness signal
#' (match count + max match date for the comp) so a source-data refresh
#' invalidates old entries without a manual clear.
#' @keywords internal
.team_rapm_asof_cache_key <- function(fn_name, comp, ...) {
  match_dates <- .team_rapm_match_dates(TRUE, comp = comp)
  freshness <- list(
    n_matches = nrow(match_dates),
    max_date = if (nrow(match_dates) > 0) as.character(max(match_dates$match_date)) else NA_character_
  )
  key_list <- c(
    list(cache_version = .TEAM_RAPM_ASOF_CACHE_VERSION, fn = fn_name, comp = comp, freshness = freshness),
    list(...)
  )
  hash <- digest::digest(key_list, algo = "md5")
  paste0(fn_name, "_", comp, "_", substr(hash, 1, 16))
}

# .team_rapm_asof_cache_path ----

#' @keywords internal
.team_rapm_asof_cache_path <- function(key) {
  file.path(.team_rapm_asof_cache_dir(), paste0(key, ".rds"))
}

# fit_team_rapm_asof_cached ----

#' Cached as-of RAPM snapshot: memoizes \code{build_team_rapm_asof()} +
#' \code{fit_team_rapm_asof()} + \code{extract_team_rapm_ratings()} (filtered
#' to \code{rating_type == "individual"}) as ONE unit, keyed on every argument
#' that affects the result. This is the single most repeated, most expensive
#' call in the as-of pipeline (~13s on AFLM) -- callers that previously called
#' the three functions directly (e.g.
#' \code{data-raw/03-ratings/build_team_rapm_asof_snapshots.R}) should call
#' this instead; the uncached functions are unchanged and still available for
#' anything that needs the full \code{design}/\code{fit} objects (this wrapper
#' returns only the ratings, with \code{n_train_matches}/\code{p}/\code{cv_r2}
#' carried as attributes for diagnostics).
#'
#' @inheritParams build_team_rapm_asof
#' @param nfolds,seed Passed to \code{fit_team_rapm_asof()}.
#' @return \code{extract_team_rapm_ratings()}'s "individual" rows (data.table),
#'   or \code{NULL} if \code{build_team_rapm_asof()} returns \code{NULL}
#'   (too few matches before \code{ref_date} -- see its own docs). Identical,
#'   cache hit or miss, to calling the three underlying functions directly.
#' @keywords internal
fit_team_rapm_asof_cached <- function(ref_date, comp = "AFLM", halflife_days = 365,
                                      seasons = TRUE, exposure = NULL, threshold = NULL,
                                      unit = NULL, game_minutes = NULL,
                                      min_train_matches = 50L, nfolds = 10, seed = 20260825) {
  ref_date <- as.Date(ref_date)
  key <- .team_rapm_asof_cache_key(
    "rapm_asof", comp,
    ref_date = as.character(ref_date), halflife_days = halflife_days,
    seasons = if (isTRUE(seasons)) "ALL" else paste(sort(seasons), collapse = ","),
    exposure = exposure, threshold = threshold, unit = unit, game_minutes = game_minutes,
    min_train_matches = min_train_matches, nfolds = nfolds, seed = seed
  )
  path <- .team_rapm_asof_cache_path(key)

  if (.team_rapm_asof_cache_enabled() && file.exists(path)) {
    return(readRDS(path))
  }

  design <- build_team_rapm_asof(ref_date, comp = comp, halflife_days = halflife_days,
                                 seasons = seasons, exposure = exposure, threshold = threshold,
                                 unit = unit, game_minutes = game_minutes,
                                 min_train_matches = min_train_matches)
  if (is.null(design)) {
    return(NULL)
  }
  fit <- fit_team_rapm_asof(design, nfolds = nfolds, seed = seed)
  ratings <- extract_team_rapm_ratings(design, fit)
  ratings <- ratings[rating_type == "individual"]
  data.table::setattr(ratings, "n_train_matches", design$n_train_matches)
  data.table::setattr(ratings, "p", design$p)
  data.table::setattr(ratings, "cv_r2", fit$cv_r2)

  if (.team_rapm_asof_cache_enabled()) {
    saveRDS(ratings, path)
  }
  ratings
}

# fit_team_spm_asof_cached ----

#' Cached as-of SPM fit: memoizes \code{fit_team_spm_asof()}. The RAPM
#' ratings passed in are content-hashed into the cache key (not assumed
#' reproducible from \code{ref_date} alone), so a cache hit is only served
#' when both the SPM call's own arguments AND the RAPM ratings it was given
#' are unchanged.
#'
#' @inheritParams fit_team_spm_asof
#' @return Identical, cache hit or miss, to calling \code{fit_team_spm_asof()}
#'   directly.
#' @keywords internal
fit_team_spm_asof_cached <- function(ref_date, rapm_asof_ratings, comp = "AFLM", seasons = TRUE,
                                     alpha = 0.5, nfolds = 10, seed = 20260825, prior_games = 10) {
  ref_date <- as.Date(ref_date)
  ratings_hash <- digest::digest(rapm_asof_ratings, algo = "md5")
  key <- .team_rapm_asof_cache_key(
    "spm_asof", comp,
    ref_date = as.character(ref_date),
    seasons = if (isTRUE(seasons)) "ALL" else paste(sort(seasons), collapse = ","),
    alpha = alpha, nfolds = nfolds, seed = seed, prior_games = prior_games,
    ratings_hash = ratings_hash
  )
  path <- .team_rapm_asof_cache_path(key)

  if (.team_rapm_asof_cache_enabled() && file.exists(path)) {
    return(readRDS(path))
  }

  out <- fit_team_spm_asof(ref_date, rapm_asof_ratings, comp = comp, seasons = seasons,
                           alpha = alpha, nfolds = nfolds, seed = seed, prior_games = prior_games)

  if (.team_rapm_asof_cache_enabled() && !is.null(out)) {
    saveRDS(out, path)
  }
  out
}

# clear_team_rapm_asof_cache ----

#' Delete cached as-of snapshots. Needed after: a source-data refresh that
#' the freshness signal didn't happen to change (e.g. a corrected historical
#' row with the same match count and max date -- rare but possible), or any
#' edit to the fitting logic that didn't bump \code{.TEAM_RAPM_ASOF_CACHE_VERSION}.
#' @param comp Optional -- restrict to one comp's files. \code{NULL} (default)
#'   clears every cached file.
#' @return Invisibly, the number of files removed.
#' @keywords internal
clear_team_rapm_asof_cache <- function(comp = NULL) {
  dir <- .team_rapm_asof_cache_dir()
  pattern <- if (is.null(comp)) "\\.rds$" else sprintf("_%s_.*\\.rds$", comp)
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  unlink(files)
  cli::cli_inform("Cleared {length(files)} as-of cache file{?s} from {dir}.")
  invisible(length(files))
}
