# spoil_predictive_validity.R ---------------------------------------------
# The replacement gate for WS2b (plan §6.4). The split-half reliability gate
# was retired because the flat spoil count scores well on it by being a stable
# measure of the WRONG thing (team defensive workload). These two tests are
# built so that a confounded-but-stable measure cannot win them.
#
#   TEST A -- points conceded. Team pre-match defensive spoil rating (built
#     point-in-time, identical decay for both schemes) predicting the points
#     the team actually concedes, controlling for opponent attacking strength
#     and venue. Target is a match outcome, not any rating, so nothing is
#     circular. Both measures are z-scored so coefficients compare directly.
#
#   TEST B -- across-season persistence. Split-half within a season holds team,
#     role and teammates constant, which is exactly why the confounded measure
#     won there. Season t -> t+1 breaks some of that, and the subset of players
#     who CHANGED TEAMS breaks most of it. A measure of the player should
#     survive a move; a measure of his old team's workload should not.
#
#   powershell.exe -Command 'Rscript "torp/data-raw/04-analysis/spoil_predictive_validity.R"'
# -------------------------------------------------------------------------

suppressMessages({
  library(arrow)
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

DATA_DIR  <- "C:/dev/torpverse/torpdata/data/"
CACHE     <- file.path(tempdir(), "spoil_pv_cache.rds")
CACHE_ALT <- "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse/acdf8436-9151-40da-a881-13b2d9ee4247/scratchpad/spoil_pv_cache.rds"
SEASONS   <- 2021:2025
HALFLIFE  <- 10   # games; identical for both schemes so the comparison is fair

load_stub <- function(stub, season) {
  fs <- list.files(DATA_DIR, pattern = sprintf("^%s_%d_\\d+\\.parquet$", stub, season),
                   full.names = TRUE)
  rbindlist(lapply(fs, function(f) as.data.table(read_parquet(f))),
            use.names = TRUE, fill = TRUE)
}

build_credits <- function() {
  out <- list()
  for (s in SEASONS) {
    ch <- load_stub("chains_data", s); pb <- load_stub("pbp_data", s)
    ctx <- compute_spoil_credit(ch, pb)
    con <- compute_contest_credit(ch, pb)
    pg <- as.data.table(read_parquet(file.path(DATA_DIR, sprintf("player_game_%d.parquet", s))))
    d <- pg[, .(player_id, player_name, match_id, season, round = as.numeric(round),
                team_id, team, position_group,
                tog = time_on_ground_percentage / 100, spoils,
                utc_start_time)]
    d <- merge(d, ctx[, .(player_id, match_id, spoil_epv_ctx)],
               by = c("player_id", "match_id"), all.x = TRUE)
    d <- merge(d, con[, .(player_id, match_id, contest_epv)],
               by = c("player_id", "match_id"), all.x = TRUE)
    for (v in c("spoil_epv_ctx", "contest_epv")) set(d, which(is.na(d[[v]])), v, 0)
    out[[as.character(s)]] <- d
    cat(sprintf("  %d done\n", s))
  }
  rbindlist(out, use.names = TRUE, fill = TRUE)
}

if (file.exists(CACHE_ALT)) {
  cat("Loading cached per-game credits\n")
  d <- readRDS(CACHE_ALT)
} else {
  cat("Building per-game credits (chains + PBP,", paste(range(SEASONS), collapse = "-"), ")\n")
  d <- build_credits()
  saveRDS(d, CACHE_ALT)
}

d[, tog := pmax(pmin(tog, 1.2), 0.1)]
d[, `:=`(flat_p80 = (spoils * EPV_SPOIL_WT) / tog,
         ctx_p80  = (spoil_epv_ctx + contest_epv) / tog)]

# --- point-in-time player rating: EWMA of PRIOR games only ----------------
setorder(d, player_id, utc_start_time)
lam <- log(2) / HALFLIFE
ewma_prior <- function(x) {
  n <- length(x)
  out <- rep(NA_real_, n)
  num <- 0; den <- 0
  for (i in seq_len(n)) {
    out[i] <- if (den > 0) num / den else NA_real_
    num <- num * exp(-lam) + x[i]
    den <- den * exp(-lam) + 1
  }
  out
}
d[, `:=`(flat_rt = ewma_prior(flat_p80),
         ctx_rt  = ewma_prior(ctx_p80)), by = player_id]

##########################################################################
cat("\n\n==================  TEST A: PREDICTING POINTS CONCEDED  ==================\n")
##########################################################################
res <- rbindlist(lapply(SEASONS, function(s)
  as.data.table(read_parquet(file.path(DATA_DIR, sprintf("results_%d.parquet", s))))),
  use.names = TRUE, fill = TRUE)
res <- res[!is.na(home_score) & !is.na(away_score)]

# One row per team-match: what this team conceded.
long <- rbindlist(list(
  res[, .(match_id, team_id = home_team_id, opp_id = away_team_id,
          conceded = away_score, home = 1L)],
  res[, .(match_id, team_id = away_team_id, opp_id = home_team_id,
          conceded = home_score, home = 0L)]
))

tm <- d[!is.na(flat_rt) & !is.na(ctx_rt),
        .(flat = sum(flat_rt * tog), ctx = sum(ctx_rt * tog), n_players = .N),
        by = .(match_id, team_id)]
long <- merge(long, tm, by = c("match_id", "team_id"))
# Opponent attacking strength control: their pre-match team TORP sum.
tr <- as.data.table(read_parquet(file.path(DATA_DIR, "torp_ratings.parquet")))
tr[, round := as.numeric(round)]
att <- merge(d[, .(player_id, match_id, season, round, team_id, tog)],
             tr[, .(player_id, season, round, torp)],
             by = c("player_id", "season", "round"))[
             , .(att_str = sum(torp * tog, na.rm = TRUE)), by = .(match_id, team_id)]
long <- merge(long, att, by.x = c("match_id", "opp_id"),
              by.y = c("match_id", "team_id"))
long <- long[n_players >= 15]
cat(sprintf("n = %d team-matches\n", nrow(long)))

zs <- function(x) (x - mean(x)) / sd(x)
long[, `:=`(z_flat = zs(flat), z_ctx = zs(ctx), z_att = zs(att_str))]

cat("\nA better defensive measure should carry a NEGATIVE coefficient:\n")
cat("more pre-match defensive value -> fewer points conceded.\n\n")
m_flat <- lm(conceded ~ z_flat + z_att + home, data = long)
m_ctx  <- lm(conceded ~ z_ctx  + z_att + home, data = long)
m_both <- lm(conceded ~ z_flat + z_ctx + z_att + home, data = long)
sm <- function(m, term, lab) {
  s <- summary(m)$coefficients
  cat(sprintf("  %-28s beta = %+6.3f  (se %.3f, t = %+5.2f)   RMSE %.3f\n",
              lab, s[term, 1], s[term, 2], s[term, 3],
              sqrt(mean(residuals(m)^2))))
}
sm(m_flat, "z_flat", "flat spoil count alone")
sm(m_ctx,  "z_ctx",  "contextual credit alone")
cat("\n  both entered together (which survives?):\n")
sm(m_both, "z_flat", "  flat")
sm(m_both, "z_ctx",  "  contextual")

##########################################################################
cat("\n\n==================  TEST B: ACROSS-SEASON PERSISTENCE  ==================\n")
##########################################################################
ps <- d[tog > 0.5, .(gms = .N, team = data.table::last(team),
                     flat = mean(flat_p80), ctx = mean(ctx_p80)),
        by = .(player_id, season, position_group)][gms >= 12]
nxt <- copy(ps)[, season := season - 1L]
setnames(nxt, c("flat", "ctx", "team", "gms"), c("flat2", "ctx2", "team2", "gms2"))
pair <- merge(ps, nxt[, .(player_id, season, position_group, flat2, ctx2, team2, gms2)],
              by = c("player_id", "season", "position_group"))
cat(sprintf("n = %d consecutive-season player pairs\n", nrow(pair)))

report_pairs <- function(dt, lab) {
  if (nrow(dt) < 30) { cat(sprintf("\n%s: n=%d, too few\n", lab, nrow(dt))); return(invisible()) }
  cat(sprintf("\n%s (n = %d)\n", lab, nrow(dt)))
  cat(sprintf("  flat  season t -> t+1 : r = %+.3f\n", cor(dt$flat, dt$flat2)))
  cat(sprintf("  ctx   season t -> t+1 : r = %+.3f\n", cor(dt$ctx, dt$ctx2)))
}
report_pairs(pair, "ALL players")
report_pairs(pair[position_group %in% c("KEY_DEFENDER", "MEDIUM_DEFENDER")], "defenders only")
movers <- pair[team != team2]
report_pairs(movers, "TEAM CHANGERS (confound partially broken)")
report_pairs(movers[position_group %in% c("KEY_DEFENDER", "MEDIUM_DEFENDER")],
             "TEAM-CHANGING DEFENDERS (the sharpest test)")

cat("\nRead: within-season split-half favoured the flat count because team\n")
cat("context is held fixed. If the flat count's advantage shrinks or reverses\n")
cat("for team changers, its within-season reliability was the confound.\n")
