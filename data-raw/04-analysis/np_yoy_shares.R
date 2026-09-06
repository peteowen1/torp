#!/usr/bin/env Rscript
# Year-over-year repeatability of Net Points under each credit rule and share
# ==============================================================================
# D17 of docs/plans/EPV-V4-CREDIT-RULES.md: the shares are not identifiable from
# conservation (the identity holds for every value), so they are chosen by
# whether a player's net points REPEAT from one season to the next. Skill
# persists, noise does not. This script scores the rule set and each (Y) share
# on that criterion over two seasons, fitting the difficulty models leak-safe
# (2026 scored on models fitted to 2025; 2025 in-sample, and it says so).
#
# Metric: correlation of per-game net points between seasons, over players with
# at least MIN_G games in both, overall and within position band. Positional
# means are reported too, so a share that "wins" by re-levelling positions is
# visible for what it is.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_yoy_shares.R"'

suppressMessages(library(data.table))
options(torp.local_data_dir = NA)
devtools::load_all(quiet = TRUE)
stopifnot(is.null(get_local_data_dir()))
SEASONS <- c(2025L, 2026L)
MIN_G <- 10L
OUT <- "data-raw/outputs"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
say <- function(...) cat(..., "\n", sep = "")
t0 <- Sys.time()

pbp <- as.data.table(load_pbp(SEASONS)); ch <- as.data.table(load_chains(SEASONS))
ps <- as.data.table(load_player_stats(SEASONS, refresh = TRUE)); res <- as.data.table(load_results(SEASONS))
pbp[, season := as.integer(substr(match_id, 5, 8))]
say("pbp ", nrow(pbp), " rows over ", uniqueN(pbp$match_id), " matches; by season:")
print(pbp[, .(matches = uniqueN(match_id), rows = .N), by = season])
say("chains ", nrow(ch), "; player_stats ", nrow(ps), "; results ", nrow(res))
say("loaded in ", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min")

terms <- .np_difficulty_terms(pbp, ch, leak_safe = TRUE)
pairs <- .np_contest_pairs(ch)
fwrite(terms, file.path(OUT, "np_difficulty_terms_2025_2026.csv"))
say("terms in ", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min")

band <- function(p) fcase(p %chin% c("FF", "CHF", "FPL", "FPR"), "forward",
                          p %chin% c("FB", "CHB", "BPL", "BPR", "HBFL", "HBFR"), "defender",
                          p %chin% c("C", "WL", "WR", "R", "RR", "RK"), "midfield/ruck",
                          p %chin% c("HFFL", "HFFR"), "half-forward", default = "bench")
pos <- ps[, .(match_id, player_id = as.character(player_id), band = band(position))]

score <- function(np, label) {
  np <- merge(np, pos, by = c("match_id", "player_id"))
  np[, season := as.integer(substr(match_id, 5, 8))]
  # a player's band for the season: his modal band
  pb <- np[, .N, by = .(player_id, season, band)][order(-N)][, .SD[1], by = .(player_id, season)]
  s <- np[, .(pg = mean(net_points), g = .N), by = .(player_id, season)]
  s <- merge(s, pb[, .(player_id, season, band)], by = c("player_id", "season"))
  w <- dcast(s[g >= MIN_G], player_id ~ season, value.var = c("pg", "band"))
  w <- w[!is.na(pg_2025) & !is.na(pg_2026)]
  r_all <- cor(w$pg_2025, w$pg_2026)
  # within-band: same band both seasons, band-demeaned
  wb <- w[band_2025 == band_2026]
  wb[, `:=`(d25 = pg_2025 - mean(pg_2025), d26 = pg_2026 - mean(pg_2026)), by = band_2025]
  r_within <- cor(wb$d25, wb$d26)
  r_band <- wb[, .(r = round(cor(pg_2025, pg_2026), 3), n = .N), by = band_2025][order(band_2025)]
  lev <- np[season == 2026, .(m = round(mean(net_points), 2)), by = band][order(band)]
  data.table(label = label, n_players = nrow(w), r_yoy = round(r_all, 4), r_within_band = round(r_within, 4),
             r_def = r_band[band_2025 == "defender"]$r, r_fwd = r_band[band_2025 == "forward"]$r,
             r_mid = r_band[band_2025 == "midfield/ruck"]$r,
             lvl_def = lev[band == "defender"]$m, lvl_fwd = lev[band == "forward"]$m,
             lvl_mid = lev[band == "midfield/ruck"]$m,
             sd_pg = round(sd(np$net_points), 3))
}

run <- function(label, ...) {
  say("\n== ", label)
  np <- suppressMessages(build_net_points(pbp, ps, res, chains = ch, ...))
  check_net_points_conservation(np)
  fwrite(np[, .(match_id, player_id, net_points)], file.path(OUT, paste0("np_yoy_", gsub("[^a-z0-9]+", "_", tolower(label)), ".csv")))
  score(np, label)
}

grid <- list(
  list("flat", credit = "flat"),
  list("difficulty, matchup", credit = "difficulty", difficulty_terms = terms, spread = "matchup"),
  list("difficulty, context", credit = "difficulty", difficulty_terms = terms, spread = "context", contest_pairs = pairs),
  list("difficulty, tog", credit = "difficulty", difficulty_terms = terms, spread = "tog"),
  list("difficulty, defensive_acts", credit = "difficulty", difficulty_terms = terms, spread = "defensive_acts"),
  list("blame 0.15", credit = "difficulty", difficulty_terms = terms, blame_share = 0.15),
  list("blame 0.50", credit = "difficulty", difficulty_terms = terms, blame_share = 0.50),
  list("blame 0.00 (decision only)", credit = "difficulty", difficulty_terms = terms, blame_share = 0.00),
  list("offence pool 0.00", credit = "difficulty", difficulty_terms = terms, offence_pool_share = 0.00),
  list("offence pool 0.20", credit = "difficulty", difficulty_terms = terms, offence_pool_share = 0.20)
)
results <- rbindlist(lapply(grid, function(g) do.call(run, g)))
say("\n\n==== YEAR-OVER-YEAR REPEATABILITY (2025 -> 2026, players with >= ", MIN_G, " games both seasons) ====")
print(results)
fwrite(results, file.path(OUT, "np_yoy_shares.csv"))
say("\ndone in ", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min")
