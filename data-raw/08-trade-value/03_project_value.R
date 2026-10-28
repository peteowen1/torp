# Projected 5-season value for every player rated at the end of 2026 (plan step 4,
# player side). Decisions (docs/DECISIONS.md, 2026-10-28):
#   (a) value = games x (TORP - replacement), no floor
#   (b) 5 seasons, 10% a year discount; 3- and 8-season sensitivity
#   (c) list survival modelled explicitly
# plus draft pick blended into the rating step for a player's first three
# seasons (02_age_survival.R: pick 1 vs pick 20 ~ +0.8 TORP at equal rating).
#
# Simulation: each season a player stays on a list with the survival model's
# probability; if he stays, his rating moves by the aging model plus a draw of
# its residual, and he plays the games model's expected games.
#
# Output: torpdata/data/raw/trade_value/03_projected_value_<as_of>.csv (gitignored)
#
# `Rscript 03_project_value.R 2023` projects from the end of 2023 instead of
# 2026, for grading past trades as they looked at the time
# (10_grade_past_trades.R). Only the players' starting point moves: the aging,
# games, TOG and survival models are still fitted on every season, so an
# as-at-2023 forecast uses what later seasons taught the model about aging in
# general, but nothing about these players after 2023.

suppressMessages({ library(data.table); library(splines) })
invisible(capture.output(source("data-raw/08-trade-value/02_age_survival.R")))
args <- commandArgs(trailingOnly = TRUE)
as_of <- if (length(args)) as.integer(args[1]) else 2026L
stopifnot(as_of %in% 2021:2026)
set.seed(as_of + 1L)

# ---- 1. Models ----
p[, early := !is.na(pick) & (season - draft_year) <= 3]
p[, lpick := fifelse(early, log(pick), 0)]
# Last season's rating as well as this one's: one-step carry-over compounded
# over eight seasons shrank steady players too fast (07_persistence_check.R:
# 4-season carry-over 0.54 actual vs 0.45 compounded; top-100 players keep
# 61% of their edge after 5 seasons vs 37%). A player with no rating the
# season before uses this season's as both.
prev <- end[!is.na(torp), .(player_id, season = season + 1L, torp_prev = torp)]
p <- merge(p, prev, by = c("player_id", "season"), all.x = TRUE)
p[is.na(torp_prev), torp_prev := torp]
age_fit <- lm(torp_next ~ torp * ns(age, 3) + torp_prev + early + lpick, data = p[survived == TRUE])
res_sd <- sigma(age_fit)
games_fit <- lm(games_next ~ ns(age, 3) + torp + games, data = p[survived == TRUE])
# Mean time on ground next season, for players who play; this season's TOG is
# set to the league median for anyone who did not play.
tog_med <- median(p$tog, na.rm = TRUE)
p[, tog_now := fifelse(is.na(tog), tog_med, tog)]
tog_fit <- lm(tog_next ~ ns(age, 3) + torp + tog_now, data = p[survived == TRUE & games_next > 0])
cat("TOG model residual sd:", round(sigma(tog_fit), 3), " median TOG:", round(tog_med, 3), "
")
cat("aging model residual sd:", round(res_sd, 3), " pick term:", round(coef(age_fit)[["lpick"]], 3), "\n")

# ---- 2. Replacement level ----
# Players picked up as mature pickups (no National Draft pick on record, aged
# 22+ in their first rated season, first season 2022 or later): the kind of
# player a club can get for nothing. Mean of their first-season end TORP.
first <- end[!is.na(torp), .SD[which.min(season)], by = player_id]
fw_names <- unique(fread("C:/dev/torpverse/torpdata/data/raw/footywire_drafts.csv")$player)
pick_ups <- first[season >= 2022 & age >= 22 & !(player_name %in% fw_names)]
repl <- mean(pick_ups$torp)
# 04_pick_curve.R reads this, so the pick curve and the ev_ columns share one level.
writeLines(format(repl, digits = 15), file.path(out_dir, "03_replacement_level.txt"))
# Sensitivity: the 40th-best rating at each club at the end of the as-of season.
cur_team <- r[season == as_of, .SD[.N], by = player_id][, .(player_id, team)]
e26 <- merge(end[season == as_of & !is.na(torp)], cur_team, by = "player_id")
repl_40 <- e26[order(-torp), .SD[40], by = team][, mean(torp, na.rm = TRUE)]
cat("replacement TORP: mature pickups", round(repl, 2), "(n", nrow(pick_ups), ")",
    " | 40th at each club", round(repl_40, 2), "\n")

# ---- 3. Simulate ----
n_sims <- 2000L; horizon <- 8L; disc <- 0.9
start <- e26[, .(player_id, player_name, team, age, torp, games, tog_now = fifelse(is.na(tog), tog_med, tog), draft_year, pick)]
start <- merge(start, end[season == as_of - 1L & !is.na(torp), .(player_id, torp_prev = torp)], by = "player_id", all.x = TRUE)
start[is.na(torp_prev), torp_prev := torp]
sim <- start[rep(seq_len(.N), each = n_sims)][, sim := rep(seq_len(n_sims), nrow(start))]
sim[, `:=`(alive = TRUE, season = as_of, torp_start = torp)]
val <- matrix(0, nrow(sim), horizon)
torp_path <- matrix(NA_real_, nrow(sim), horizon)
for (k in seq_len(horizon)) {
  ps <- predict(surv, newdata = sim, type = "response")
  sim[, alive := alive & (runif(.N) < ps)]
  sim[, early := !is.na(pick) & (season - draft_year) <= 3]
  sim[, lpick := fifelse(early, log(pick), 0)]
  mu <- predict(age_fit, newdata = sim)
  g  <- pmin(pmax(predict(games_fit, newdata = sim), 0), 25)
  tg <- pmin(pmax(predict(tog_fit, newdata = sim), 0.2), 1)
  sim[, `:=`(torp_prev = torp, torp = mu + rnorm(.N, 0, res_sd), games = g, tog_now = tg, age = age + 1, season = season + 1L)]
  # value = games x TOG x (TORP - replacement): TORP and replacement are per full game
  val[, k] <- fifelse(sim$alive, sim$games * sim$tog_now * (sim$torp - repl), 0) * disc^(k - 1)
  torp_path[, k] <- fifelse(sim$alive, sim$torp, NA_real_)
  if (k == 1) sim[, alive_1 := alive]
}
sim[, `:=`(v3 = rowSums(val[, 1:3]), v5 = rowSums(val[, 1:5]), v8 = rowSums(val[, 1:8]),
           seasons5 = rowSums(!is.na(torp_path[, 1:5])), torp27 = torp_path[, 1])]

# Check: k-season carry-over in the simulation (survivors, rating in season k
# on the 2026 rating) against what ratings actually did (07_persistence_check.R:
# 0.82, 0.71, 0.61, 0.54, 0.46 for k = 1..5).
carry <- sapply(1:5, function(k) {
  ok <- !is.na(torp_path[, k])
  coef(lm(torp_path[ok, k] ~ sim$torp_start[ok]))[2]
})
cat("simulated carry-over k = 1..5:", round(carry, 2), "(actual 0.82 0.71 0.61 0.54 0.46)
")

out <- sim[, .(v5_mean = mean(v5), v5_p10 = quantile(v5, .1), v5_p90 = quantile(v5, .9),
               v3_mean = mean(v3), v8_mean = mean(v8),
               seasons_on_list_5 = mean(seasons5), torp_2027 = mean(torp27, na.rm = TRUE)),
           by = player_id]
# Expected UNdiscounted value of each future season (2027 = k 1), so a caller
# can discount from a different start year (04_pick_curve.R values draftees
# whose first season was 2021 or 2022).
ev <- as.data.table(val %*% diag(1 / disc^(seq_len(horizon) - 1)))
setnames(ev, paste0("ev_", as_of + seq_len(horizon)))
ev[, player_id := sim$player_id]
ev <- ev[, lapply(.SD, mean), by = player_id]
out <- merge(out, ev, by = "player_id")
# Chance he is still on a list each season, so 05 can charge his salary only
# for the seasons he is being paid.
onl <- as.data.table(!is.na(torp_path) * 1)
setnames(onl, paste0("on_list_", as_of + seq_len(horizon)))
onl[, player_id := sim$player_id]
onl <- onl[, lapply(.SD, mean), by = player_id]
out <- merge(out, onl, by = "player_id")
out <- merge(start[, .(player_id, player_name, team, age = round(age, 1), torp_2026 = torp, games_2026 = games, draft_year, pick)],
             out, by = "player_id")
num <- names(out)[sapply(out, is.double)]
out[, (num) := lapply(.SD, round, 2), .SDcols = num]
setorder(out, -v5_mean)
out[, rank_v5 := seq_len(.N)]
setnames(out, c("torp_2026", "games_2026", "torp_2027"), paste0(c("torp_", "games_", "torp_"), as_of + c(0L, 0L, 1L)))
fwrite(out, file.path(out_dir, sprintf("03_projected_value_%d.csv", as_of)))
cat("players projected:", nrow(out), " sims each:", n_sims, "\n")
if (as_of != 2026L) {
  # The anchors below name 2026 players; earlier seasons just print the top.
  cat("\ntop 10 by projected 5-season value, as at", as_of, ":\n")
  print(out[1:10, .SD, .SDcols = c("rank_v5", "player_name", "age", paste0("torp_", as_of), "v5_mean", "v8_mean")])
  quit(save = "no")
}

# ---- 4. Anchors (chosen before looking) ----
a <- function(nm) out[player_name == nm]
anchors <- c(
  watson_top5   = a("Nick Watson")$rank_v5 <= 5,
  blakey_over_greene = a("Nick Blakey")$v5_mean > a("Toby Greene")$v5_mean,
  duursma_over_same_rating_undrafted = TRUE  # checked below against a matched peer
)
peer <- out[is.na(pick) & abs(age - a("Willem Duursma")$age) < 1.5 &
              abs(torp_2026 - a("Willem Duursma")$torp_2026) < 0.5]
anchors["duursma_over_same_rating_undrafted"] <- nrow(peer) == 0 || a("Willem Duursma")$v5_mean > mean(peer$v5_mean)
print(anchors)

cols <- c("rank_v5", "player_name", "age", "torp_2026", "torp_2027", "seasons_on_list_5",
          "v5_mean", "v5_p10", "v5_p90", "v3_mean", "v8_mean")
cat("\nexample players:\n")
print(out[grepl("Willem Duursma|Levi Ashcroft|Harley Reid|Colby McKercher|Nick Watson|Tom Brown|Mitch Georgiades|Nick Blakey|Toby Greene|Lachie Neale", player_name), ..cols][order(rank_v5)])
cat("\ntop 15 by projected 5-season value:\n"); print(out[1:15, ..cols])
cat("\nvalue distribution: share with v5_mean > 0:", round(mean(out$v5_mean > 0), 3),
    " median:", median(out$v5_mean), "\n")
stopifnot(all(anchors))
