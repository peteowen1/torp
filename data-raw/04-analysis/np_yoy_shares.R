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
# Metric: correlation of per-game net points between consecutive seasons, over
# players with at least MIN_G games in both, overall and within position band,
# averaged across season pairs through Fisher's z (2021 played 16-minute
# quarters, so pairs are never pooled raw). A per-80-minutes rate is reported
# beside per-game. Positional means are reported too, so a share that "wins" by
# re-levelling positions is visible for what it is.
#
#   NP_SEASONS=2021:2026 powershell.exe -Command 'Rscript "data-raw/04-analysis/np_yoy_shares.R"'
#
# Two seasons run in minutes; six take longer and hold two seasons of chains
# at once, so wait for the box to be free.

suppressMessages(library(data.table))
options(torp.local_data_dir = NA)
devtools::load_all(quiet = TRUE)
stopifnot(is.null(get_local_data_dir()))
# NP_SEASONS is an R expression typed by whoever runs this local script (e.g.
# 2021:2026); it is not user-facing input, so parsing it is fine here.
SEASONS <- eval(parse(text = Sys.getenv("NP_SEASONS", unset = "c(2025L, 2026L)")))
SEASONS <- as.integer(SEASONS)
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

# leak-safe within the loaded seasons; the earliest is fitted in-sample and says so
terms <- .np_difficulty_terms(pbp, ch, leak_safe = TRUE)
pairs <- .np_contest_pairs(ch)
fwrite(terms, file.path(OUT, "np_difficulty_terms_2025_2026.csv"))
say("terms in ", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min")

band <- function(p) fcase(p %chin% c("FF", "CHF", "FPL", "FPR"), "forward",
                          p %chin% c("FB", "CHB", "BPL", "BPR", "HBFL", "HBFR"), "defender",
                          p %chin% c("C", "WL", "WR", "R", "RR", "RK"), "midfield/ruck",
                          p %chin% c("HFFL", "HFFR"), "half-forward", default = "bench")
pos <- ps[, .(match_id, player_id = as.character(player_id), band = band(position))]

tog_tbl <- ps[, .(match_id, player_id = as.character(player_id), tog = pmax(time_on_ground_percentage, 1) / 100)]
fz <- function(r, n) { z <- atanh(r); w <- n - 3; tanh(sum(z * w) / sum(w)) }  # Fisher-z average, weighted

score_pair <- function(s, s1, s2) {
  # s: per (player, season) pg / p80 / band / g
  w <- merge(s[season == s1 & g >= MIN_G], s[season == s2 & g >= MIN_G],
             by = "player_id", suffixes = c("_a", "_b"))
  if (nrow(w) < 30) return(NULL)
  wb <- w[band_a == band_b]
  wb[, `:=`(da = pg_a - mean(pg_a), db = pg_b - mean(pg_b),
            ea = p80_a - mean(p80_a), eb = p80_b - mean(p80_b)), by = band_a]
  rb <- wb[, .(r = cor(pg_a, pg_b), n = .N), by = band_a]
  data.table(pair = paste0(s1, "-", s2), n = nrow(w),
             r_all = cor(w$pg_a, w$pg_b), r_within = cor(wb$da, wb$db),
             r_within_p80 = cor(wb$ea, wb$eb),
             r_def = rb[band_a == "defender"]$r, n_def = rb[band_a == "defender"]$n,
             r_fwd = rb[band_a == "forward"]$r, n_fwd = rb[band_a == "forward"]$n,
             r_mid = rb[band_a == "midfield/ruck"]$r, n_mid = rb[band_a == "midfield/ruck"]$n)
}

score <- function(np, label) {
  np <- merge(np, pos, by = c("match_id", "player_id"))
  np[tog_tbl, on = .(match_id, player_id), tog := i.tog]
  np[is.na(tog), tog := 0.75]
  np[, season := as.integer(substr(match_id, 5, 8))]
  # a player's band for the season: his modal band
  pb <- np[, .N, by = .(player_id, season, band)][order(-N)][, .SD[1], by = .(player_id, season)]
  s <- np[, .(pg = mean(net_points), p80 = mean(net_points / tog), g = .N), by = .(player_id, season)]
  s <- merge(s, pb[, .(player_id, season, band)], by = c("player_id", "season"))
  seasons <- sort(unique(s$season))
  pairs <- rbindlist(lapply(seq_len(length(seasons) - 1), function(i) score_pair(s, seasons[i], seasons[i + 1])))
  if (is.null(pairs) || nrow(pairs) == 0) cli::cli_abort("No season pair with 30+ players for {label}.")
  last <- max(seasons)
  lev <- np[season == last, .(m = round(mean(net_points), 2)), by = band][order(band)]
  out <- data.table(label = label, pairs = nrow(pairs), n_players = sum(pairs$n),
                    r_yoy = round(fz(pairs$r_all, pairs$n), 4),
                    r_within_band = round(fz(pairs$r_within, pairs$n), 4),
                    r_within_p80 = round(fz(pairs$r_within_p80, pairs$n), 4),
                    r_def = round(fz(pairs$r_def, pairs$n_def), 3),
                    r_fwd = round(fz(pairs$r_fwd, pairs$n_fwd), 3),
                    r_mid = round(fz(pairs$r_mid, pairs$n_mid), 3),
                    lvl_def = lev[band == "defender"]$m, lvl_fwd = lev[band == "forward"]$m,
                    lvl_mid = lev[band == "midfield/ruck"]$m,
                    sd_pg = round(sd(np$net_points), 3))
  attr(out, "pairs") <- pairs[, label := label]
  out
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
  list("offence pool 0.20", credit = "difficulty", difficulty_terms = terms, offence_pool_share = 0.20),
  list("stoppages allocated (v4 engine)", credit = "difficulty", difficulty_terms = terms, stoppages = "allocate"),
  list("stoppages, loser share 0.30", credit = "difficulty", difficulty_terms = terms, stoppages = "allocate", stoppage_loser_share = 0.30),
  list("stoppages, loser share 0.70", credit = "difficulty", difficulty_terms = terms, stoppages = "allocate", stoppage_loser_share = 0.70)
)
scored <- lapply(grid, function(g) do.call(run, g))
results <- rbindlist(scored)
pairs_all <- rbindlist(lapply(scored, attr, "pairs"))
say("\n\n==== YEAR-OVER-YEAR REPEATABILITY (", paste(range(SEASONS), collapse = " -> "), ", consecutive pairs, players with >= ", MIN_G, " games in both; Fisher-z averages) ====")
print(results)
say("\nper pair:")
print(pairs_all[, .(label, pair, n, r_all = round(r_all, 3), r_within = round(r_within, 3), r_within_p80 = round(r_within_p80, 3))])
fwrite(results, file.path(OUT, "np_yoy_shares.csv"))
fwrite(pairs_all, file.path(OUT, "np_yoy_shares_pairs.csv"))
say("\ndone in ", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min")
