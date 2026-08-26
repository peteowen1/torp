# Package Configuration

#' Is this a strict pipeline run?
#'
#' Single parse rule for `VERSEBUS_STRICT`, the environment variable that turns
#' fetch/manifest problems from warnings into aborts. It is set to `"1"` by the
#' three pipeline entry scripts (`data-raw/01-data/daily_release.R`,
#' `02-models/build_match_predictions.R`, `03-ratings/run_ratings_pipeline.R`)
#' and by the release workflows in this repo and torpdata; nothing sets it to
#' any other value.
#'
#' Exists because the rule was written out at five call sites and one of them
#' disagreed: `check_vintage_alignment()` tested `nzchar()`, so
#' `VERSEBUS_STRICT="0"` went strict there and lenient at the other four,
#' despite its own roxygen claiming it matched "every other pipeline entry
#' point's convention".
#'
#' `R/versebus.R` deliberately keeps its own inline copy: that file is vendored
#' into torpmodels and guarded function-by-function by `test-versebus-sync.R`,
#' so it cannot call a torp-local helper without the sibling repo gaining one
#' first.
#'
#' The rule is fail-open: only the exact string `"1"` enables strict mode, so
#' a plausible-looking `VERSEBUS_STRICT=true` would silently leave every abort
#' path as a warning. That is the convention four of the five call sites
#' already used and it is not being changed here, but a set-but-unrecognised
#' value now warns rather than being read as "off" in silence -- the whole
#' point of the flag is that a bad read must not pass quietly.
#'
#' @return `TRUE` when `VERSEBUS_STRICT` is exactly `"1"`.
#' @noRd
.strict_mode <- function() {
  raw <- Sys.getenv("VERSEBUS_STRICT")
  if (!identical(raw, "") && !identical(raw, "1")) {
    cli::cli_warn(c(
      "{.envvar VERSEBUS_STRICT} is set to {.val {raw}}, which is not {.val 1}.",
      "x" = "Strict mode is OFF -- fetch and manifest problems will warn, not abort.",
      "i" = "Use {.code VERSEBUS_STRICT=1} to enable it."
    ))
  }
  identical(raw, "1")
}

#' Get TORP Data Repository
#'
#' Returns the repository used for TORP data downloads. Can be configured
#' via the TORP_DATA_REPO environment variable or package options.
#'
#' @return Character string of the repository in format "owner/repo"
#' @keywords internal
get_torp_data_repo <- function() {
  getOption("torp.data.repo", "peteowen1/torpdata")
}

#' Set TORP Data Repository
#'
#' Sets the repository used for TORP data downloads in the current session.
#'
#' @param repo Character string of the repository in format "owner/repo"
#' @export
set_torp_data_repo <- function(repo) {
  if (!is.character(repo) || length(repo) != 1 || nchar(repo) == 0) {
    cli::cli_abort("Repository must be a single non-empty character string")
  }

  if (!grepl("^[^/]+/[^/]+$", repo)) {
    cli::cli_abort("Repository must be in format 'owner/repo'")
  }

  options(torp.data.repo = repo)
  cli::cli_inform("TORP data repository set to: {repo}")
}

# Release asset cache (session-scoped, short TTL)
.torp_release_cache <- new.env(parent = emptyenv())

# Records why the last get_release_assets() call for a tag returned NULL.
# NULL entry = the fetch succeeded (the release genuinely has no matching
# assets); a character entry = the fetch itself failed. See
# .last_release_fetch_error().
.torp_release_fetch_error <- new.env(parent = emptyenv())

#' Why the last release-asset fetch for a tag returned nothing
#'
#' `get_release_assets()` returns `NULL` both when a release genuinely has no
#' assets and when the API call fails, and its callers cannot tell those apart.
#' That matters wherever "no prior data" and "could not check for prior data"
#' should lead to different behaviour -- notably a publish step that skips its
#' regression check when it believes nothing was published before, which would
#' otherwise skip that check on precisely the flaky-network run where a bad
#' capture is most likely.
#'
#' @param release_tag Character. The release tag name.
#' @return The error message from the last failed fetch for this tag, or `NULL`
#'   if the last fetch succeeded (or none has been attempted this session).
#' @keywords internal
.last_release_fetch_error <- function(release_tag) {
  if (!exists(release_tag, envir = .torp_release_fetch_error)) {
    return(NULL)
  }
  get(release_tag, envir = .torp_release_fetch_error)
}

#' Get filenames available in a GitHub release
#'
#' Queries the GitHub API via piggyback for the list of assets attached to a
#' release tag. Results are cached per tag with a short TTL
#' (`RELEASE_ASSET_CACHE_TTL_SECS`) so repeated calls (e.g. across multiple
#' `load_*()` calls) don't hit the network, while still picking up assets
#' uploaded elsewhere during a long-lived session.
#'
#' @param release_tag Character. The release tag name (e.g. "predictions-data").
#' @return Character vector of filenames, or NULL if the query fails.
#' @keywords internal
get_release_assets <- function(release_tag) {
  if (exists(release_tag, envir = .torp_release_cache)) {
    cached <- get(release_tag, envir = .torp_release_cache)
    age_secs <- as.numeric(difftime(Sys.time(), cached$time, units = "secs"))
    if (age_secs < RELEASE_ASSET_CACHE_TTL_SECS) {
      return(cached$assets)
    }
  }

  repo_parts <- strsplit(get_torp_data_repo(), "/")[[1]]

  assets <- tryCatch({
    resp <- gh::gh(
      "GET /repos/{owner}/{repo}/releases/tags/{tag}",
      owner = repo_parts[1],
      repo = repo_parts[2],
      tag = release_tag
    )
    assign(release_tag, NULL, envir = .torp_release_fetch_error)
    vapply(resp$assets, function(a) a$name, character(1))
  }, error = function(e) {
    # NULL is deliberately kept as the return: generate_urls() treats it as
    # "listing unavailable, use the heuristic filter", and changing that
    # contract would alter every loader's fallback. But NULL alone cannot tell
    # a caller "this release has no assets" from "the network just failed",
    # and a caller gating a regression check on the difference needs to know.
    # The message is recorded here for .last_release_fetch_error() to report.
    assign(release_tag, conditionMessage(e), envir = .torp_release_fetch_error)
    cli::cli_warn("Could not fetch release assets for {.val {release_tag}}: {e$message}")
    NULL
  })

  if (!is.null(assets)) {
    assign(release_tag, list(assets = assets, time = Sys.time()), envir = .torp_release_cache)
  }

  assets
}

#' Invalidate the cached release-asset listing for a tag
#'
#' Call after uploading to a release so the next `get_release_assets()` call
#' sees the new asset instead of a pre-upload snapshot.
#'
#' @param release_tag Character. The release tag name.
#' @return Invisible NULL.
#' @keywords internal
invalidate_release_cache <- function(release_tag) {
  if (exists(release_tag, envir = .torp_release_cache)) {
    rm(list = release_tag, envir = .torp_release_cache)
  }
  invisible(NULL)
}

# Helper functions

#' Validate rounds
#'
#' @param rounds A numeric vector or TRUE
#'
#' @return Validated rounds vector
#' @keywords internal
validate_rounds <- function(rounds) {
  if (isTRUE(rounds)) rounds <- 0:28

  if (!is.numeric(rounds)) {
    cli::cli_abort("Rounds must be numeric values or TRUE")
  }

  invalid_rounds <- rounds[rounds < 0 | rounds > 28]
  if (length(invalid_rounds) > 0) {
    cli::cli_abort("Invalid round numbers: {paste(invalid_rounds, collapse = ', ')}. Rounds must be between 0 and 28")
  }

  return(rounds)
}

#' Validate seasons
#'
#' @param seasons A numeric vector or TRUE
#'
#' @return A validated numeric vector of seasons
#' @keywords internal
validate_seasons <- function(seasons) {
  current_year <- as.integer(format(Sys.Date(), "%Y"))

  if (isTRUE(seasons)) seasons <- AFL_MIN_SEASON:current_year

  if (!is.numeric(seasons)) {
    cli::cli_abort("Seasons must be numeric values or TRUE")
  }

  invalid_seasons <- seasons[seasons < AFL_MIN_SEASON | seasons > current_year]
  if (length(invalid_seasons) > 0) {
    cli::cli_abort("Invalid season years: {paste(invalid_seasons, collapse = ', ')}. Seasons must be between {AFL_MIN_SEASON} and {current_year}")
  }

  return(seasons)
}

#' Validate seasons for a specific competition
#'
#' \code{validate_seasons()} floors at \code{AFL_MIN_SEASON} (2021 -- where the
#' men's CHAIN data starts) regardless of competition. AFLW's own history runs
#' from 2018, and it is box-score based, so that floor is wrong for it in both
#' directions: \code{seasons = TRUE} silently resolves to 2021+ (dropping
#' AFLW's first three seasons with no warning), and an explicit
#' \code{load_player_stats(2019, comp = "AFLW")} aborts outright even though
#' the data exists.
#'
#' Routing on \code{comp} rather than widening \code{AFL_MIN_SEASON} keeps
#' men's behaviour bit-identical -- the men's floor is load-bearing elsewhere
#' (chain-derived pipelines genuinely have no pre-2021 data to read).
#'
#' @param seasons Numeric vector of seasons, or TRUE for all available.
#' @param comp Competition: "AFLM" (default) or "AFLW".
#' @return A validated numeric vector of seasons.
#' @keywords internal
.validate_seasons_comp <- function(seasons, comp = "AFLM") {
  if (identical(comp, "AFLW")) return(.validate_aflw_seasons(seasons))
  validate_seasons(seasons)
}

#' Generate URLs for data download
#'
#' @param data_type Type of data (e.g., "chain-data", "pbp-data")
#' @param file_prefix Prefix for the file name
#' @param seasons A numeric vector of seasons
#' @param rounds A numeric vector of rounds (optional)
#' @param prefer_aggregated Logical. If TRUE, prefer aggregated seasonal files
#'   when loading all rounds for a season. Default is TRUE (aggregated files
#'   are available on torpdata). Set via option `torp.use_aggregated_files`.
#'
#' @return A character vector of URLs
#' @keywords internal
#' @importFrom tools file_path_sans_ext
generate_urls <- function(data_type, file_prefix, seasons, rounds = NULL, prefer_aggregated = NULL) {
  base_url <- paste0("https://github.com/", get_torp_data_repo(), "/releases/download")

  # Determine if we should try aggregated files
  # Default to TRUE - aggregated files are available on torpdata
  # Can be disabled via option: options(torp.use_aggregated_files = FALSE)
  if (is.null(prefer_aggregated)) {
    prefer_aggregated <- getOption("torp.use_aggregated_files", TRUE)
  }

  current_season <- get_afl_season()
  current_round <- NULL

  if (is.null(rounds)) {
    combinations <- expand.grid(seasons = seasons)

    urls <- paste0(base_url, "/", data_type, "/", file_prefix, "_", combinations$seasons, ".parquet")
    urls <- sort(urls)
  }

  if (!is.null(rounds)) {
    # These data types only have _all files (no per-round files on GitHub)
    # Always load _all and filter by round_number in load_from_url()
    aggregated_only_types <- c("chains-data", "pbp-data", "ep_wp_chart-data")

    if (data_type %in% aggregated_only_types) {
      urls <- paste0(base_url, "/", data_type, "/", file_prefix, "_", seasons, "_all.parquet")
      return(as.character(urls))
    }

    current_season <- get_afl_season()
    current_round <- get_afl_week()

    # Check if we're loading all rounds for any season (rounds 0-28 or TRUE was passed)
    all_rounds <- 0:28
    loading_all_rounds <- length(rounds) >= length(all_rounds) &&
      all(all_rounds %in% rounds)

    if (prefer_aggregated && loading_all_rounds) {
      # Use aggregated files for complete seasons
      urls <- paste0(base_url, "/", data_type, "/", file_prefix, "_", seasons, "_all.parquet")
      return(as.character(urls))
    }

    # Use per-round files (default behavior for other data types)
    rounds_02d <- sprintf("%02d", rounds)
    combinations <- expand.grid(seasons = seasons, rounds = rounds_02d)

    urls <- paste0(base_url, "/", data_type, "/", file_prefix, "_", combinations$seasons, "_", combinations$rounds, ".parquet")
    urls <- sort(urls)
  }

  # Filter URLs against actual release assets (ground truth).
  # Falls back to heuristic future-URL filter if the API call fails.
  assets <- get_release_assets(data_type)

  if (!is.null(assets)) {
    requested_files <- basename(urls)
    keep <- requested_files %in% assets
    n_dropped <- sum(!keep)
    if (n_dropped > 0) {
      cli::cli_warn("Filtered {n_dropped} URL{?s} not found in {.val {data_type}} release.")
    }
    return(as.character(urls[keep]))
  }

  # Fallback: filter out future URLs by parsing season/round from filenames
  n_before <- length(urls)
  keep <- vapply(urls, function(u) {
    fname <- tools::file_path_sans_ext(basename(u))
    parts <- regmatches(fname, gregexpr("[0-9]+", fname))[[1]]
    if (length(parts) == 0) return(TRUE)
    file_season <- as.numeric(parts[length(parts) - (length(parts) > 1)])
    if (is.na(file_season) || file_season < 2020) {
      cli::cli_warn("Could not parse season from URL: {.url {u}}")
      return(TRUE)
    }
    if (file_season < current_season) return(TRUE)
    if (file_season > current_season) return(FALSE)
    if (data_type == "fixtures-data") return(TRUE)
    if (length(parts) >= 2) {
      file_round <- as.numeric(parts[length(parts)])
      if (grepl("_all\\.parquet$", u)) return(TRUE)
      if (is.null(current_round)) current_round <<- get_afl_week()
      return(file_round <= current_round)
    }
    TRUE
  }, logical(1), USE.NAMES = FALSE)
  urls <- urls[keep]
  n_dropped <- n_before - length(urls)

  if (n_dropped > 0) {
    cli::cli_inform("Filtered {n_dropped} future URL{?s} (current season: {current_season}, round: {current_round}).")
  }

  return(as.character(urls))
}
