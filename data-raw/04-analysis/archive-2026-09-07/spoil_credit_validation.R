# spoil_credit_validation.R -----------------------------------------------
# WS2a validation: does compute_spoil_credit() do what it was built to do?
#
# Checks, in order:
#   1. Coverage    -- share of spoils that now get a contextual price
#                     (baseline: compute_contest_credit() reaches 27.8%).
#   2. Spread      -- the flat weight is a constant; how much signed variation
#                     does the contextual measure recover, and for whom?
#   3. Reliability -- split-half by position bucket. Extra spread is only worth
#                     having if it is signal. If contextual reliability lands
#                     below the flat count's, the variance is noise and WS2b
#                     must not proceed.
#   4. Team-level  -- does the contextual total track margin better than the
#                     flat count? Reported for completeness, NOT a gate: this
#                     is the game-state-confounded direction the diagnosis
#                     warns about (defensive volume rises when losing).
#
#   powershell.exe -Command 'Rscript "torp/data-raw/04-analysis/spoil_credit_validation.R"'
# -------------------------------------------------------------------------

suppressMessages({
  library(arrow)
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

DATA_DIR <- "C:/dev/torpverse/torpdata/data/"
SEASONS  <- 2022:2025
FLAT_WT  <- EPV_SPOIL_WT

load_stub <- function(stub, season) {
  fs <- list.files(DATA_DIR, pattern = sprintf("^%s_%d_\\d+\\.parquet$", stub, season),
                   full.names = TRUE)
  rbindlist(lapply(fs, function(f) as.data.table(read_parquet(f))),
            use.names = TRUE, fill = TRUE)
}

cat("Computing contextual spoil credit,", paste(range(SEASONS), collapse = "-"), "\n")
ctx_list <- list(); con_list <- list(); counts <- list()
for (s in SEASONS) {
  ch <- load_stub("chains_data", s)
  pb <- load_stub("pbp_data", s)
  pid <- intersect(c("playerId", "player_id"), names(ch))[1]
  n_spoils <- nrow(ch[description == "Spoil" & !is.na(get(pid))])

  ctx <- compute_spoil_credit(ch, pb)
  con <- compute_contest_credit(ch, pb)
  ctx[, season := s]
  ctx_list[[as.character(s)]] <- ctx
  con_list[[as.character(s)]] <- con[, .(player_id, match_id, contest_epv)]
  counts[[as.character(s)]] <- data.table(
    season = s, spoils = n_spoils, priced = sum(ctx$spoils_priced))
  cat(sprintf("  %d: %s spoils, %s newly priced\n", s,
              format(n_spoils, big.mark = ","),
              format(sum(ctx$spoils_priced), big.mark = ",")))
}
ctx <- rbindlist(ctx_list); con <- rbindlist(con_list)
cnt <- rbindlist(counts)

# --- 1. coverage ---------------------------------------------------------
cat("\n========== 1. COVERAGE ==========\n")
cnt[, new_pct := round(100 * priced / spoils, 1)]
print(cnt)
cat(sprintf("\nPooled: %s of %s spoils newly priced (%.1f%%)\n",
            format(sum(cnt$priced), big.mark = ","),
            format(sum(cnt$spoils), big.mark = ","),
            100 * sum(cnt$priced) / sum(cnt$spoils)))
cat("compute_contest_credit() already priced ~27.8% (the excluded triples),\n")
cat("so combined contextual coverage is the sum of the two.\n")

# --- join to per-game data for position + flat comparison ----------------
pg <- rbindlist(lapply(SEASONS, function(s)
  as.data.table(read_parquet(file.path(DATA_DIR, sprintf("player_game_%d.parquet", s))))),
  use.names = TRUE, fill = TRUE)

d <- merge(pg[, .(player_id, match_id, season, round, team_id, position_group,
                  tog = time_on_ground_percentage / 100, spoils)],
           ctx[, .(player_id, match_id, spoil_epv_ctx, spoils_priced)],
           by = c("player_id", "match_id"), all.x = TRUE)
d <- merge(d, con, by = c("player_id", "match_id"), all.x = TRUE)
for (v in c("spoil_epv_ctx", "spoils_priced", "contest_epv")) {
  set(d, which(is.na(d[[v]])), v, 0)
}
# The two contextual streams are disjoint by construction, so they sum.
d[, ctx_total := spoil_epv_ctx + contest_epv]
d[, flat_total := spoils * FLAT_WT]

# --- 2. spread -----------------------------------------------------------
cat("\n========== 2. SPREAD (per player-game) ==========\n")
cat("flat = spoils x EPV_SPOIL_WT; ctx = contextual credit for the same acts\n\n")
print(d[position_group != "" & !is.na(position_group),
        .(n = .N,
          spoils = round(mean(spoils, na.rm = TRUE), 1),
          flat_mean = round(mean(flat_total), 3),
          flat_sd = round(sd(flat_total), 3),
          ctx_mean = round(mean(ctx_total), 3),
          ctx_sd = round(sd(ctx_total), 3),
          pct_ctx_negative = round(100 * mean(ctx_total < 0), 1)),
        by = position_group][order(-spoils)])

# --- 3. reliability ------------------------------------------------------
cat("\n========== 3. SPLIT-HALF RELIABILITY (Spearman-Brown, full season) ==========\n")
cat("Is the extra spread signal or noise? ctx must not land below flat.\n\n")
P <- d[tog > 0.4 & !is.na(position_group) & position_group != ""]
setorder(P, player_id, season, round)
P[, half := seq_len(.N) %% 2, by = .(player_id, season)]
rel <- function(col) {
  s <- P[, .(v = mean(get(col), na.rm = TRUE), n = .N),
         by = .(player_id, season, position_group, half)][n >= 6]
  w <- dcast(s, player_id + season + position_group ~ half, value.var = "v")
  setnames(w, c("0", "1"), c("a", "b"))
  w <- w[!is.na(a) & !is.na(b) & is.finite(a) & is.finite(b)]
  w[, .(n_players = .N, r = cor(a, b)), by = position_group
    ][, .(position_group, n_players, r_full = round(2 * r / (1 + r), 3))]
}
rf <- rel("flat_total"); setnames(rf, "r_full", "r_flat")
rc <- rel("ctx_total");  setnames(rc, "r_full", "r_ctx")
rr <- merge(rf, rc, by = c("position_group", "n_players"))
rr[, delta := round(r_ctx - r_flat, 3)]
print(rr[order(-r_ctx)])

# --- 4. team-level margin association ------------------------------------
cat("\n========== 4. TEAM-LEVEL MARGIN ASSOCIATION (not a gate) ==========\n")
resd <- rbindlist(lapply(SEASONS, function(s)
  as.data.table(read_parquet(file.path(DATA_DIR, sprintf("results_%d.parquet", s))))),
  use.names = TRUE, fill = TRUE)
resd <- resd[!is.na(home_score) & !is.na(away_score),
             .(match_id, home_team_id, away_team_id, margin = home_score - away_score)]
tm <- d[, .(flat = sum(flat_total), ctx = sum(ctx_total)), by = .(match_id, team_id)]
m <- merge(resd, tm, by.x = c("match_id", "home_team_id"),
           by.y = c("match_id", "team_id"))
a <- merge(resd[, .(match_id, away_team_id)], tm,
           by.x = c("match_id", "away_team_id"), by.y = c("match_id", "team_id"))
setnames(a, c("flat", "ctx"), c("a_flat", "a_ctx"))
m <- merge(m, a, by = "match_id")
m[, `:=`(d_flat = flat - a_flat, d_ctx = ctx - a_ctx)]
cat(sprintf("n = %d matches\n", nrow(m)))
cat(sprintf("  cor(margin, flat spoil diff) = %+.3f\n", cor(m$margin, m$d_flat)))
cat(sprintf("  cor(margin, ctx  spoil diff) = %+.3f\n", cor(m$margin, m$d_ctx)))
cat("\nA more negative flat correlation is the game-state confound the\n")
cat("diagnosis documents (losing teams spoil more). The contextual measure\n")
cat("should be less negative -- it knows which spoils mattered.\n")
