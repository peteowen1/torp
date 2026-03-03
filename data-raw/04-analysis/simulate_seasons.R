# AFL Season Simulation Examples ----
# Usage patterns for simulate_afl_season() and related functions.

devtools::load_all()

# Set to TRUE to also run mid-season and custom rating examples
RUN_ALL <- FALSE
N_CORES <- pmax(parallel::detectCores(logical = FALSE) - 2,1)

# Pre-season simulation (injury-aware) ----
# Loads preseason injury list, excludes injured players from team ratings,
# uses reduced per-round noise (SIM_INJURY_SD_KNOWN = 2 vs default 3).
injuries <- get_all_injuries(2026, scrape = FALSE)
cli::cli_alert_info("Loaded {nrow(injuries)} preseason injuries")

results_pre <- simulate_afl_season(2026, n_sims = 3000, seed = 420, verbose = TRUE,
                                   n_cores = N_CORES, injuries = injuries)
print(results_pre)

# Full summary table with starting TORP ratings
summary_pre <- summarise_simulations(results_pre)
ratings <- results_pre$original_ratings[, .(torp = mean(torp)), by = team]
summary_pre <- merge(summary_pre, ratings, by = "team", all.x = TRUE)
data.table::setorder(summary_pre, -avg_wins)
summary_pre[, .(team, torp, avg_wins, avg_pf_pg, avg_pa_pg, top_1_pct, top_4_pct, top_8_pct, made_gf_pct, won_gf_pct, last_pct)]

# Injury impact comparison ----
# Run without injuries to quantify the effect of excluding known absences.
if (RUN_ALL) {
  results_no_inj <- simulate_afl_season(2026, n_sims = 3000, seed = 420, verbose = TRUE, n_cores = N_CORES)
  sum_inj <- summarise_simulations(results_pre)[, .(team, wins_inj = avg_wins, gf_inj = won_gf_pct)]
  sum_std <- summarise_simulations(results_no_inj)[, .(team, wins_std = avg_wins, gf_std = won_gf_pct)]
  impact <- merge(sum_inj, sum_std, by = "team")
  impact[, `:=`(win_delta = round(wins_inj - wins_std, 2), gf_delta = round(gf_inj - gf_std, 3))]
  data.table::setorder(impact, win_delta)
  cat("\n--- Injury Impact (injury-aware minus standard) ---\n")
  print(impact[, .(team, wins_inj, wins_std, win_delta, gf_inj, gf_std, gf_delta)])
}

# Mid-season simulation ----
if (RUN_ALL) {
  # Auto-detects played games from fixture scores; simulates remaining rounds.
  # Pass injuries for injury-aware mid-season sims (scrapes live + preseason).
  mid_injuries <- get_all_injuries(2026, scrape = TRUE)
  results_mid <- simulate_afl_season(2026, n_sims = 500, seed = 1, injuries = mid_injuries)
  print(results_mid)
}

# Custom ratings (what-if scenarios) ----
if (RUN_ALL) {
  custom_ratings <- data.table::data.table(
    team = c("Adelaide Crows", "Brisbane Lions", "Carlton", "Collingwood",
             "Essendon", "Fremantle", "Geelong Cats", "Gold Coast Suns",
             "GWS Giants", "Hawthorn", "Melbourne", "North Melbourne",
             "Port Adelaide", "Richmond", "St Kilda", "Sydney Swans",
             "West Coast Eagles", "Western Bulldogs"),
    torp = c(55, 75, 60, 65, 50, 62, 68, 58,
             63, 72, 52, 40, 64, 45, 48, 70,
             38, 56)
  )

  results_custom <- simulate_afl_season(
    2026, n_sims = 500,
    team_ratings = custom_ratings,
    seed = 7
  )

  summary_custom <- summarise_simulations(results_custom)
  summary_custom[, .(team, avg_wins, top_4_pct, won_gf_pct)]
}

# Query: position distribution ----
pos_dist <- results_pre$ladders[, .N, by = .(team, rank)]
pos_dist[, pct := N / sum(N), by = team]

# Query: avg wins at each ladder position ----
results_pre$ladders[, .(avg_wins = mean(wins), avg_pct = mean(percentage)), by = rank]

# Query: GF probability for a specific team ----
results_pre$finals[team == "Brisbane Lions", mean(made_gf)]

# Most improved / biggest dropoff analysis ----
# Load 2025 actual results and calculate wins
fixtures_2025 <- load_fixtures(seasons = 2025)
fix_2025_dt <- data.table::as.data.table(fixtures_2025)

# Build game results for calculate_ladder() — regular season only
games_2025 <- data.table::data.table(
  roundnum   = as.integer(fix_2025_dt$round.roundNumber),
  home_team  = fix_2025_dt$home.team.name,
  away_team  = fix_2025_dt$away.team.name,
  home_score = fix_2025_dt$home.score.totalScore,
  away_score = fix_2025_dt$away.score.totalScore
)
max_round_2025 <- AFL_REGULAR_SEASON_ROUNDS[as.character(2025)]
if (is.na(max_round_2025)) max_round_2025 <- 24L
games_2025 <- games_2025[roundnum <= max_round_2025 & !is.na(home_score) & !is.na(away_score)]
games_2025[, result := home_score - away_score]

if (requireNamespace("fitzRoy", quietly = TRUE)) {
  games_2025[, home_team := fitzRoy::replace_teams(home_team)]
  games_2025[, away_team := fitzRoy::replace_teams(away_team)]
}

ladder_2025 <- calculate_ladder(games_2025)
# Count draws as half a win
actual_wins_2025 <- ladder_2025[, .(team, wins_2025 = wins + draws * 0.5)]

# Compute improvement in each simulation (draws as half wins for consistency)
ladders_with_improvement <- merge(
  results_pre$ladders, actual_wins_2025,
  by = "team", all.x = TRUE
)
ladders_with_improvement[, `:=`(
  adj_wins = wins + draws * 0.5,
  improvement = (wins + draws * 0.5) - wins_2025
)]

# Find most improved and biggest dropoff per sim
most_improved <- ladders_with_improvement[,
  .(team = team[which.max(improvement)], max_improvement = max(improvement)),
  by = sim_id
]
biggest_dropoff <- ladders_with_improvement[,
  .(team = team[which.min(improvement)], max_dropoff = min(improvement)),
  by = sim_id
]

# Odds of being most improved
cat("\n--- Most Improved Team Odds ---\n")
mi_odds <- most_improved[, .N, by = team][order(-N)]
mi_odds[, pct := sprintf("%.1f%%", N / nrow(most_improved) * 100)]
print(mi_odds)

# Odds of biggest dropoff
cat("\n--- Biggest Dropoff Team Odds ---\n")
bd_odds <- biggest_dropoff[, .N, by = team][order(-N)]
bd_odds[, pct := sprintf("%.1f%%", N / nrow(biggest_dropoff) * 100)]
print(bd_odds)

# Average improvement per team
cat("\n--- Average Improvement (wins vs 2025) ---\n")
avg_improvement <- ladders_with_improvement[, .(
  wins_2025 = wins_2025[1L],
  avg_wins_2026 = round(mean(adj_wins), 1),
  avg_improvement = round(mean(improvement), 1)
), by = team][order(-avg_improvement)]
print(avg_improvement)

# Visualization: win distribution ----
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  # Win distribution histogram for top 4 teams by avg wins
  top4_teams <- summary_pre[order(-avg_wins)][1:4, team]
  ggplot(results_pre$ladders[team %in% top4_teams],
         aes(x = wins, fill = team)) +
    geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
    labs(title = "Win Distribution - Top 4 Favourites",
         x = "Wins", y = "Frequency") +
    theme_minimal()

  # Ladder position boxplot
  ggplot(results_pre$ladders, aes(x = reorder(team, rank), y = rank)) +
    geom_boxplot(fill = "steelblue", alpha = 0.6) +
    coord_flip() +
    scale_y_reverse() +
    labs(title = "Ladder Position Distribution",
         x = NULL, y = "Ladder Position") +
    theme_minimal()
}
