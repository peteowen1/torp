# ARM 3: does the standardisation itself drive the within-season dispersion drift?
# ================================================================================
# Context. Published v2 epr SD collapses 2.60 -> 1.01 through 2022 and 2.51 ->
# 1.17 through 2025, climbs 0.85 -> 2.33 through 2023, while published v1 sits
# at ~1.8-2.0 every season. Two hypotheses have already been killed:
#   - position amplification: the v2/v1 ratio is uniform across all seven
#     position groups, and between-position means are only 5-7% of variance;
#   - per-season constant scope (confirm_standardisation_scope.R): pooling the
#     constants across seasons barely moves the per-game scale (gap 1.048 ->
#     1.029), and per-game epv_adj SD is stable at ~8.2-8.6 either way.
#
# So the per-game inputs are fine and the drift is downstream, in the EPR
# aggregation -- or the published v1/v2 comparison is confounded, because those
# two files were produced by different pipeline invocations at different times
# rather than as a controlled pair.
#
# THIS SCRIPT REMOVES THAT CONFOUND. It builds both arms itself, from identical
# source data, in one process, differing ONLY in EPV_POSITION_STANDARDISE:
#   ARM ON  = v2 behaviour (recentre AND rescale)
#   ARM OFF = v1 behaviour (recentre only)
# then runs the real calculate_epr() over 2022 for each and compares the SD
# trajectory round by round.
#
# READING IT:
#   OFF flat, ON collapsing  -> the rescale interacts with the decay/shrinkage
#                               aggregation; the bug is real and is v2's.
#   both flat                -> the drift is NOT reproducible from code+data,
#                               so it is an artifact of how the published files
#                               were generated (mixed vintages/partial reruns),
#                               not of the standardisation at all.
#   both collapsing          -> the drift predates v2 entirely and published
#                               v1's flatness is itself the artifact.
#
# Run: powershell.exe -Command 'Rscript "<this file>"'

suppressMessages({
  library(dplyr); library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

SEASONS   <- 2021:2022
TEST_SEAS <- 2022
OUT <- "C:/dev/torpverse/torpmodels/data-raw/04-match-model/experiments/results/standardisation_arm3.rds"

cli::cli_h1("Loading {SEASONS}")
pbp    <- load_pbp(SEASONS)
pstats <- load_player_stats(SEASONS)
teams  <- load_teams(SEASONS)
chains <- load_chains(SEASONS)
cli::cli_inform("pbp {nrow(pbp)} | pstats {nrow(pstats)} | teams {nrow(teams)} | chains {nrow(chains)}")

# Build per-game data per season, exactly as run_ratings_pipeline.R does, under
# a given standardisation setting.
build_pgd <- function(standardise) {
  old <- get("EPV_POSITION_STANDARDISE", envir = asNamespace("torp"))
  assignInNamespace("EPV_POSITION_STANDARDISE", standardise, ns = "torp")
  on.exit(assignInNamespace("EPV_POSITION_STANDARDISE", old, ns = "torp"), add = TRUE)
  stopifnot(identical(get("EPV_POSITION_STANDARDISE", envir = asNamespace("torp")), standardise))

  rbindlist(lapply(SEASONS, function(s) {
    as.data.table(create_player_game_data(
      pbp[pbp$season == s, ], pstats[pstats$season == s, ],
      teams[teams$season == s, ], chains = chains[chains$season == s, ]))
  }), fill = TRUE)
}

cli::cli_h2("Building per-game data: standardise ON")
pgd_on  <- build_pgd(TRUE)
cli::cli_h2("Building per-game data: standardise OFF")
pgd_off <- build_pgd(FALSE)

cat("\nper-game epv_adj SD by season:\n")
print(rbind(
  pgd_on[!is.na(epv_adj),  .(arm = "ON",  sd = round(sd(epv_adj), 3)), by = season],
  pgd_off[!is.na(epv_adj), .(arm = "OFF", sd = round(sd(epv_adj), 3)), by = season]
)[order(season, arm)], row.names = FALSE)

# Run the real EPR aggregation round by round through the test season.
rounds <- sort(unique(pbp$round_number[pbp$season == TEST_SEAS]))
rounds <- rounds[rounds >= 1]
cli::cli_h2("Running calculate_epr over {TEST_SEAS}, rounds {min(rounds)}-{max(rounds)}")

traj <- function(pgd, lab) {
  rbindlist(lapply(rounds, function(r) {
    e <- tryCatch(
      calculate_epr(season_val = TEST_SEAS, round_val = r,
                    player_game_data = pgd, skills = FALSE),
      error = function(err) { cli::cli_warn("R{r} {lab}: {conditionMessage(err)}"); NULL })
    if (is.null(e)) return(NULL)
    e <- as.data.table(e)
    e <- e[!is.na(epr) & !is.na(wt_gms) & as.numeric(wt_gms) >= 3]
    data.table(arm = lab, round = r, n = nrow(e), sd = sd(e$epr), mean = mean(e$epr))
  }), fill = TRUE)
}
t_on  <- traj(pgd_on,  "ON")
t_off <- traj(pgd_off, "OFF")

cli::cli_h1("EPR SD trajectory through {TEST_SEAS}")
for (lab in c("ON", "OFF")) {
  x <- if (lab == "ON") t_on else t_off
  if (nrow(x)) cat(sprintf("%-4s: %s\n", lab, paste(sprintf("%.2f", x$sd), collapse = " ")))
}

fmt <- function(x) if (!nrow(x)) NA_real_ else round(x$sd[nrow(x)] / x$sd[1], 3)
cat("\n=== end/start SD ratio (published v2 for 2022 was 1.01/2.60 = 0.39) ===\n")
cat(sprintf("ARM ON  : %.3f\n", fmt(t_on)))
cat(sprintf("ARM OFF : %.3f\n", fmt(t_off)))

saveRDS(list(on = t_on, off = t_off,
             pgd_sd_on = pgd_on[!is.na(epv_adj), .(sd = sd(epv_adj)), by = season],
             pgd_sd_off = pgd_off[!is.na(epv_adj), .(sd = sd(epv_adj)), by = season]), OUT)
cli::cli_alert_success("Saved {OUT}")
