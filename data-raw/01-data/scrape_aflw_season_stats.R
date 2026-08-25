# AFLW statspro season stats backfill ----
# Fetches statspro/playersStats/seasons/{providerId} for every AFLW season
# (2018-current) -- the 28 "extended" fields (spoils, pressure_acts, and 26
# others) that CFS's playerStats/match extendedStats block returns genuinely
# empty for AFLW. See AFL-API-REFERENCE.md's "Endpoint family: statspro" for
# the full field-by-field audit against get_afl_player_stats(). Season-total
# granularity only -- confirmed the finest available for these fields (no
# as-at-round parameter exists; the round-level sibling endpoint doesn't
# carry them at all).
#
# One HTTP call per season (~580 players each), not one-per-player -- cheap.
# Run: powershell.exe -Command 'Rscript "data-raw/01-data/scrape_aflw_season_stats.R"'

devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

AFLW_SEASON_STATS_FIRST_YEAR <- 2018L # AFLW's inaugural season; 2017 has no comp-season provider id (confirmed)

seasons <- AFLW_SEASON_STATS_FIRST_YEAR:get_afl_season()
cat(sprintf("Scraping AFLW season stats for %d seasons (%d-%d)\n",
            length(seasons), min(seasons), max(seasons)))

results <- list()
for (yr in seasons) {
  d <- tryCatch(
    get_afl_player_season_stats(yr, comp = "AFLW"),
    error = function(e) {
      cli::cli_warn("Season {yr}: fetch failed -- {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(d) || nrow(d) == 0) {
    cli::cli_warn("Season {yr}: 0 rows, skipping.")
    next
  }
  results[[as.character(yr)]] <- d
  cat(sprintf("  %d: %d players\n", yr, nrow(d)))
}

if (length(results) == 0) {
  cli::cli_abort("No AFLW season stats fetched for any season -- aborting, not publishing.")
}

# Validate before publishing (real, hard-to-reverse action below) ----
# Known spot-check: Jodie Hicks (CD_I1001681), 2025, matches the value
# documented in AFL-API-REFERENCE.md exactly.
if ("2025" %in% names(results)) {
  hicks <- results[["2025"]][results[["2025"]]$player_id == "CD_I1001681", ]
  stopifnot(
    "Jodie Hicks spot-check failed -- spoils" = nrow(hicks) == 1 && hicks$spoils == 5,
    "Jodie Hicks spot-check failed -- intercepts" = hicks$intercepts == 18,
    "Jodie Hicks spot-check failed -- pressure_acts" = hicks$pressure_acts == 66
  )
  cat("Spot-check passed: Jodie Hicks 2025 (spoils=5, intercepts=18, pressure_acts=66).\n")
}
for (yr_chr in names(results)) {
  d <- results[[yr_chr]]
  if (any(is.na(d$player_id))) cli::cli_abort("Season {yr_chr}: NA player_id present -- aborting.")
  if (nrow(d) < 100) cli::cli_warn("Season {yr_chr}: only {nrow(d)} rows -- unusually low for a full AFLW season, check before trusting.")
}
cat("Validation passed for all seasons. Publishing to torpdata release 'aflw_season_stats-data'.\n")

# Publish, one parquet per season ----
for (yr_chr in names(results)) {
  save_to_release(
    df = results[[yr_chr]],
    file_name = paste0("aflw_season_stats_", yr_chr),
    release_tag = "aflw_season_stats-data"
  )
}

cat("Done.\n")
