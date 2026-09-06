# Confirm: are EPV standardisation constants per-season, and does that make
# seasons land on different scales?
# =========================================================================
# HYPOTHESIS (2026-07-28). run_ratings_pipeline.R calls
# create_player_game_data() once per season, on that season's data alone, so
# .position_adjust()'s rescale denominator (the within-position weighted SD)
# and .pooled_sd are computed PER SEASON. Each season is therefore normalised
# to its own dispersion. EPR then mixes seasons with a 273-630 day decay, so a
# player's rating is a decayed blend of differently-scaled quantities -- which
# drags dispersion smoothly across a season as the blend shifts from last
# season's scale to this one's.
#
# Observed and unexplained without this: published v2 epr SD collapses 2.60 ->
# 1.01 through 2022 and 2.51 -> 1.17 through 2025, climbs 0.85 -> 2.33 through
# 2023, while v1 (which only CENTRES -- a location shift, harmless to mix) sits
# at ~1.8-2.0 in every season.
#
# TEST: build per-game data for 2021+2022 two ways and compare the scale each
# season lands on.
#   ARM A (current): one create_player_game_data() call per season
#   ARM B (fixed):   one call over both seasons, so constants are pooled
# If A gives the two seasons materially different epv_adj dispersion and B
# gives them the same, the mechanism is confirmed and the fix is to compute the
# constants once over all seasons rather than per season.
#
# Run: powershell.exe -Command 'Rscript "<this file>"'

suppressMessages({
  library(dplyr); library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

SEASONS <- 2021:2022
OUT <- "C:/dev/torpverse/torpmodels/data-raw/04-match-model/experiments/results/standardisation_scope.rds"

cli::cli_h1("Loading {SEASONS} source data")
t0 <- Sys.time()
pbp    <- load_pbp(SEASONS)
pstats <- load_player_stats(SEASONS)
teams  <- load_teams(SEASONS)      # API, not the local parquet (lineup_position gap)
chains <- load_chains(SEASONS)
cli::cli_inform("loaded in {round(difftime(Sys.time(), t0, units='mins'), 1)} min")
for (nm in c("pbp", "pstats", "teams", "chains")) {
  d <- get(nm); cli::cli_inform("{nm}: {nrow(d)} rows")
}
stopifnot("lineup_position" %in% names(teams))
cli::cli_inform("teams lineup_position non-NA: {sum(!is.na(teams$lineup_position))}")

.sub <- function(d, s) d[d$season == s, ]

cli::cli_h2("ARM A -- current behaviour: one call per season")
a <- rbindlist(lapply(SEASONS, function(s) {
  cli::cli_progress_step("season {s}")
  as.data.table(create_player_game_data(.sub(pbp, s), .sub(pstats, s),
                                        .sub(teams, s), chains = .sub(chains, s)))
}), fill = TRUE)

cli::cli_h2("ARM B -- fixed: one call over both seasons (pooled constants)")
b <- as.data.table(create_player_game_data(pbp, pstats, teams, chains = chains))

adj <- c("epv_adj", "epv_recv_adj", "epv_disp_adj", "epv_spoil_adj")
adj <- intersect(adj, intersect(names(a), names(b)))

summ <- function(d, lab) {
  d <- d[!is.na(epv_adj)]
  out <- d[, c(list(arm = lab, n = .N),
               lapply(.SD, function(x) round(sd(x, na.rm = TRUE), 4))),
           by = season, .SDcols = adj]
  out[order(season)]
}
resA <- summ(a, "A per-season"); resB <- summ(b, "B pooled")

cli::cli_h1("Per-game epv_adj dispersion, by season")
print(rbind(resA, resB), row.names = FALSE)

ratio <- function(r) {
  x <- r[["epv_adj"]]
  if (length(x) == 2) round(max(x) / min(x), 3) else NA_real_
}
cat("\n=== between-season scale gap (max/min of epv_adj SD) ===\n")
cat(sprintf("ARM A (per-season constants): %.3f\n", ratio(resA)))
cat(sprintf("ARM B (pooled constants)    : %.3f\n", ratio(resB)))
cat("\nVERDICT:\n")
if (!is.na(ratio(resA)) && !is.na(ratio(resB))) {
  if (ratio(resA) > 1.15 && ratio(resB) < ratio(resA) * 0.8) {
    cat("  CONFIRMED -- per-season constants put the two seasons on different\n")
    cat("  scales, and pooling brings them together. Mixing them via EPR's\n")
    cat("  cross-season decay is what drags dispersion through a season.\n")
  } else if (ratio(resA) <= 1.15) {
    cat("  NOT CONFIRMED -- the two seasons already land on the same scale\n")
    cat("  under current behaviour, so scope is not the mechanism. Look elsewhere.\n")
  } else {
    cat("  INCONCLUSIVE -- pooling did not materially close the gap. Report both.\n")
  }
}

saveRDS(list(arm_a = resA, arm_b = resB, seasons = SEASONS), OUT)
cli::cli_alert_success("Saved {OUT}")
