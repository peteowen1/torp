# defensive_credit_reprice.R ----------------------------------------------
# Can the key-defender under-dispersion be fixed at the MECHANISM -- how much
# EPV credit each defensive action earns -- rather than by a post-hoc
# per-position multiplier?
#
# WHY THIS IS A DIFFERENT LEVER FROM THE ONE THAT FAILED
# ------------------------------------------------------
# calibration_harness.R swept a multiplier on the WHOLE spoil channel and made
# calibration worse (spread 2.98x -> 5.53x): every position has a spoil
# channel, so scaling it moves everyone, and key vs medium defenders need it
# moved in opposite directions. But the channel's COMPOSITION is
# position-specific -- a key defender's epv_spoil is mostly spoils, intercepts
# and one-percenters; a midfielder's is mostly tackles and pressure acts.
# Reweighting the individual stats WITHIN the channel can therefore move
# defenders without moving midfielders. That is what this script optimises.
#
# THE LINEARITY THAT MAKES IT CHEAP
# ---------------------------------
#   epv_spoil       = sum_s w_s * stat_s                      (player_credit.R:208)
#   epv_spoil_adj   = (p80 - wmean(p80 | lineup_position)) * tog   -- linear in w
#   spoil_sum       = sum over prior games of adj * tog * decay    -- linear in w
#   epr_spoil       = (loading*spoil_sum + k*prior)/(wt_gms + k)   -- linear in w
#                     (.bayesian_shrink, player_ratings.R:222)
# So aggregate ONCE PER STAT and any candidate weight vector is a linear
# combination -- no pipeline rebuild per candidate.
#
#   epr_spoil(w) = sum_s w_s * (L_s - B) + B
# where L_s is the aggregation run with stat s alone at unit weight and B is
# the pure-prior term (the aggregation run with a zero channel).
#
# Point-in-time throughout: a player's rating for match m uses only matches
# strictly before m, with the production decay (EPR_DECAY_SPOIL) and priors.
#
#   powershell.exe -Command 'Rscript "torp/data-raw/04-analysis/defensive_credit_reprice.R"'
# -------------------------------------------------------------------------

suppressMessages({
  library(arrow); library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

DATA_DIR <- "C:/dev/torpverse/torpdata/data/"
SEASONS  <- 2021:2026
BUCKETS  <- c("KEY_DEFENDER", "MEDIUM_DEFENDER", "MIDFIELDER", "RUCK",
              "MEDIUM_FORWARD", "KEY_FORWARD")

# The eight stats that make up epv_spoil, with their shipped weights.
STATS <- c("spoils", "tackles", "pressure_acts", "def_half_pressure_acts",
           "intercepts", "one_percenters", "rebound50s", "frees_against")
W0 <- c(spoils = EPV_SPOIL_WT, tackles = EPV_TACKLE_WT,
        pressure_acts = EPV_PRESSURE_WT,
        def_half_pressure_acts = EPV_DEF_PRESSURE_WT,
        intercepts = EPV_INTERCEPTS_WT, one_percenters = EPV_ONE_PERCENTERS_WT,
        rebound50s = EPV_REBOUND50S_WT, frees_against = EPV_FREES_AGAINST_WT)

cat("Shipped defensive credit weights:\n")
print(round(W0, 4))

# ---- per-game stats, position-adjusted exactly as create_player_game_data ----
pg <- rbindlist(lapply(SEASONS, function(s)
  as.data.table(read_parquet(file.path(DATA_DIR, sprintf("player_game_%d.parquet", s))))),
  use.names = TRUE, fill = TRUE)
pg[, round := as.numeric(round)]
pg[, tog_safe := pmax(fifelse(is.na(time_on_ground_percentage), 100,
                              time_on_ground_percentage) / 100, 0.1)]
for (s in STATS) set(pg, which(is.na(pg[[s]])), s, 0)

# Step 7 of create_player_game_data: per-80 normalise, then centre within
# lineup_position, then re-scale by tog.
for (s in STATS) {
  p80 <- pg[[s]] / pg$tog_safe
  set(pg, j = paste0(".p80_", s), value = p80)
}
pg[, .grp := lineup_position]
for (s in STATS) {
  col <- paste0(".p80_", s)
  pg[, (paste0(".adj_", s)) :=
       (get(col) - stats::weighted.mean(get(col), tog_safe, na.rm = TRUE)) * tog_safe,
     by = .grp]
}

# ---- point-in-time decayed aggregation, once per stat --------------------
setorder(pg, player_id, utc_start_time)
pg[, .date := as.Date(utc_start_time)]
lam_spoil <- EPR_DECAY_SPOIL
K <- EPR_PRIOR_GAMES_SPOIL
PRIOR <- EPR_PRIOR_RATE_SPOIL
LOAD <- EPR_LOADING_DEFAULT

# Running decayed sums using the exponential recursion:
#   S_i = S_{i-1} * exp(-dt/decay) + x_i     (value BEFORE game i is S_{i-1})
prior_decayed <- function(x, dates) {
  n <- length(x); out <- numeric(n); s <- 0; prev <- dates[1]
  for (i in seq_len(n)) {
    s <- s * exp(-as.numeric(dates[i] - prev) / lam_spoil)
    prev <- dates[i]
    out[i] <- s          # strictly prior
    s <- s + x[i]
  }
  out
}
cat("\nAggregating decayed prior sums (", length(STATS), "stats + denominator)...\n")
pg[, .den := prior_decayed(tog_safe, .date), by = player_id]
for (s in STATS) {
  pg[, (paste0(".S_", s)) :=
       prior_decayed(get(paste0(".adj_", s)) * tog_safe, .date), by = player_id]
}

# epr_spoil contribution of stat s at unit weight, and the pure-prior term.
denom <- pg$.den + K
pg[, .B := (K * PRIOR) / denom]
for (s in STATS) {
  pg[, (paste0(".A_", s)) := (LOAD * get(paste0(".S_", s))) / denom]
}

# ---- validate against the published spoil_epr ----------------------------
epr_spoil_of <- function(w) {
  v <- pg$.B
  for (s in STATS) v <- v + w[[s]] * pg[[paste0(".A_", s)]]
  v
}
pg[, .recon := epr_spoil_of(W0)]

tr <- as.data.table(read_parquet(file.path(DATA_DIR, "torp_ratings.parquet")))
tr[, round := as.numeric(round)]
chk <- merge(pg[, .(player_id, season, round, .recon, .den)],
             tr[, .(player_id, season, round, spoil_epr, recv_epr, disp_epr,
                    hitout_epr, psr, torp)],
             by = c("player_id", "season", "round"))
chk <- chk[!is.na(spoil_epr) & .den > 1]
cat(sprintf("\n--- reconstruction check vs published spoil_epr (n = %s) ---\n",
            format(nrow(chk), big.mark = ",")))
cat(sprintf("  cor  = %.4f\n", cor(chk$.recon, chk$spoil_epr)))
cat(sprintf("  mean recon %.3f vs published %.3f\n",
            mean(chk$.recon), mean(chk$spoil_epr)))
cat(sprintf("  sd   recon %.3f vs published %.3f\n",
            sd(chk$.recon), sd(chk$spoil_epr)))
cat("  (Production uses opponent-adjusted _oadj inputs where available; this\n")
cat("   reconstruction uses _adj, so an exact match is not expected. A high\n")
cat("   correlation means the linear machinery is right.)\n")

saveRDS(list(pg = pg[, c("player_id", "season", "round", "match_id", "team_id",
                          "position_group", "lineup_position", "tog_safe", ".B", ".den",
                          paste0(".A_", STATS)), with = FALSE],
             STATS = STATS, W0 = W0),
        file.path(DATA_DIR, "..", "reprice_cache.rds"))
cat("\nCached per-stat channel components for the optimisation stage.\n")
