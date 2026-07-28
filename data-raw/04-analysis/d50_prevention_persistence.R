# Plan §7.20 — Track D2: does defensive PREVENTION persist as a team trait?
#
# THE GATE, AND WHY IT IS THE WHOLE POINT. Pete's answer to O2 (2026-07-27) was
# "a shutdown defender rates highly only if suppression persists" — which turns
# the largest build in the program into an empirical question. This is that
# question, asked at TEAM level first, because if a team's prevention residual
# has no year-to-year persistence there is no player trait underneath it to
# attribute and the expensive matchup build (D1) never starts.
#
# THE DECOMPOSITION. Points conceded splits into two skills:
#   (a) TERRITORY  — how many inside-50 entries you concede
#   (b) PREVENTION — how few points you concede PER entry, once conceded
# (b) is the suppression quantity. (a) is largely midfield/possession and is
# already priced elsewhere in TORP. Testing (b) separately is essential: a
# "prevention persists" result that is really just "good teams are good" would
# restate what the metric already knows and justify nothing.
#
# BENCHMARKS, so a persistence number can be read. Raw points conceded is the
# ceiling — if prevention persists far below it, prevention is mostly noise
# riding on general team strength. Territory is the sibling skill.
suppressMessages({library(arrow); library(data.table)})

DD <- "C:/dev/torpverse/torpdata/data/"
SEASONS <- 2021:2026
rdp <- function(pat, ss) rbindlist(lapply(ss, function(s) {
  f <- file.path(DD, sprintf(pat, s)); if (!file.exists(f)) return(NULL)
  as.data.table(read_parquet(f)) }), use.names=TRUE, fill=TRUE)

pg <- rdp("player_game_%d.parquet", SEASONS)
stopifnot("inside50s" %in% names(pg))
ent <- pg[!is.na(team_id), .(entries = sum(inside50s, na.rm = TRUE)),
          by = .(match_id, season, team_id)]

res <- rdp("results_%d.parquet", SEASONS)
res <- res[!is.na(home_score) & !is.na(away_score),
           .(match_id, season, home_team_id, away_team_id, home_score, away_score)]
# one row per team-side: what this team conceded, and how many entries the
# OPPOSITION had against it
sides <- rbind(
  res[, .(match_id, season, team_id = home_team_id, opp_id = away_team_id,
          conceded = away_score)],
  res[, .(match_id, season, team_id = away_team_id, opp_id = home_team_id,
          conceded = home_score)])
sides <- merge(sides, ent[, .(match_id, opp_id = team_id, opp_entries = entries)],
               by = c("match_id", "opp_id"))
sides <- sides[!is.na(opp_entries) & opp_entries > 0]
cat(sprintf("team-matches: %s across %s seasons\n",
            format(nrow(sides), big.mark=","), uniqueN(sides$season)))

# --- expected points conceded given entries allowed -------------------------
fit <- lm(conceded ~ opp_entries, data = sides)
cat(sprintf("\nexpected points conceded = %.2f + %.3f x entries allowed  (R2 %.3f)\n",
            coef(fit)[1], coef(fit)[2], summary(fit)$r.squared))
sides[, exp_conceded := predict(fit, sides)]
# positive = conceded FEWER points than the entries implied = prevented more
sides[, prevention := exp_conceded - conceded]

# --- season-level team aggregates -------------------------------------------
ts <- sides[, .(games = .N,
                conceded_pg = mean(conceded),
                entries_allowed_pg = mean(opp_entries),
                prevention_pg = mean(prevention),
                pts_per_entry = sum(conceded) / sum(opp_entries)),
            by = .(team_id, season)][games >= 15]
cat(sprintf("team-seasons with >=15 games: %d\n", nrow(ts)))

# --- year-to-year persistence -----------------------------------------------
nxt <- copy(ts)[, season := season - 1L]
setnames(nxt, setdiff(names(nxt), c("team_id","season")),
         paste0("next_", setdiff(names(nxt), c("team_id","season"))))
pair <- merge(ts, nxt, by = c("team_id","season"))
cat(sprintf("consecutive team-season pairs: %d\n\n", nrow(pair)))

persist <- function(col) {
  x <- pair[[col]]; y <- pair[[paste0("next_", col)]]
  ok <- !is.na(x) & !is.na(y)
  ct <- suppressWarnings(cor.test(x[ok], y[ok]))
  c(r = unname(ct$estimate), lo = ct$conf.int[1], hi = ct$conf.int[2], n = sum(ok))
}
cat("=== YEAR-TO-YEAR PERSISTENCE (team level) ===\n")
rows <- list(
  c("conceded per game        [benchmark: general defensive strength]", "conceded_pg"),
  c("entries allowed per game [skill (a): territory]                 ", "entries_allowed_pg"),
  c("points per entry allowed [skill (b): PREVENTION, raw]           ", "pts_per_entry"),
  c("prevention residual      [skill (b): PREVENTION, entry-adjusted]", "prevention_pg"))
for (r in rows) {
  p <- persist(r[2])
  cat(sprintf("  %s  r = %+.3f [%+.3f, %+.3f]  n = %d\n",
              r[1], p["r"], p["lo"], p["hi"], p["n"]))
}

# --- does prevention persist BEYOND general defensive strength? -------------
# The decisive test. If next season's prevention is predicted by this season's
# prevention only through this season's overall concession, there is no
# separate trait -- just team quality wearing a different name.
cat("\n=== IS PREVENTION A SEPARATE TRAIT? ===\n")
m1 <- lm(next_prevention_pg ~ conceded_pg, data = pair)
m2 <- lm(next_prevention_pg ~ conceded_pg + prevention_pg, data = pair)
an <- anova(m1, m2)
cat(sprintf("  next prevention ~ conceded            : R2 %.3f\n", summary(m1)$r.squared))
cat(sprintf("  next prevention ~ conceded + prevention: R2 %.3f\n", summary(m2)$r.squared))
cat(sprintf("  incremental prevention coefficient %+.3f (p = %.3f)\n",
            coef(m2)[["prevention_pg"]], an$`Pr(>F)`[2]))

# --- split-half reliability within season: the ceiling ----------------------
# How much of prevention is even measurable within a season? A trait cannot
# persist across years more strongly than it repeats within one.
set.seed(20260727)
sides[, half := ifelse(seq_len(.N) %% 2 == 0, "A", "B"), by = .(team_id, season)]
sh <- dcast(sides[, .(v = mean(prevention), n = .N), by = .(team_id, season, half)],
            team_id + season ~ half, value.var = "v")
sh <- sh[!is.na(A) & !is.na(B)]
r_half <- cor(sh$A, sh$B)
cat(sprintf("\n=== WITHIN-SEASON SPLIT-HALF (measurement ceiling) ===\n"))
cat(sprintf("  split-half r = %+.3f  ->  Spearman-Brown reliability %+.3f  (n = %d)\n",
            r_half, 2*r_half/(1+r_half), nrow(sh)))
cat("  A trait cannot persist across seasons more strongly than it repeats\n")
cat("  within one. If this is near zero, prevention is not measurable at all\n")
cat("  at team level, let alone attributable to individual players.\n")
