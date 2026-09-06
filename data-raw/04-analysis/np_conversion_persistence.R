# Does a player's goal-kicking conversion credit repeat year to year?
#
# Under v4 a scoring kick is a terminal row: the kicker takes (1 - omega) of the
# whole swing (score minus exp_pts at the kick), the team pool the rest. That is
# the conversion surprise, and it is where key forwards earn much of their +8
# per game. If it does not repeat, it is luck paid to forwards and belongs in
# the team pool; if it repeats like the rest of the ledger, it is skill and
# stays. Persistence separates the two (2025 -> 2026, min 10 games each).
suppressMessages(library(data.table))
options(torp.local_data_dir = NA)
devtools::load_all(quiet = TRUE)
OUT <- "data-raw/outputs"
MIN_G <- 10L
say <- function(...) cat(..., "\n", sep = "")
terms_all <- fread(file.path(OUT, "np_difficulty_terms_2025_2026.csv"))
terms_all[, match_id := as.character(match_id)]

per_season <- function(s) {
  t0 <- Sys.time()
  pbp <- as.data.table(load_pbp(s)); ch <- as.data.table(load_chains(s))
  ps <- as.data.table(load_player_stats(s, refresh = TRUE)); res <- as.data.table(load_results(s))
  tm <- terms_all[substr(match_id, 5, 8) == as.character(s)]
  np <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty", stoppages = "allocate",
                         difficulty_terms = tm, return_payments = TRUE)
  pay <- attr(np, "np_payments")
  # scoring kicks: the running score moves on the next play-by-play row
  a <- pbp[, .(match_id = as.character(match_id), display_order, description, team, home_away,
               home_points, away_points)]
  setorder(a, match_id, display_order)
  a[, tot := home_points + away_points]
  a[, ntot := shift(tot, -1L), by = match_id]
  a[, shot := description %chin% NP_DISPOSAL_DESCS & !is.na(ntot) & ntot != tot]
  say("  ", s, ": ", sum(a$shot), " scoring kicks in ", uniqueN(a$match_id), " matches")
  pay[, match_id := as.character(match_id)]
  pay <- merge(pay, a[, .(match_id, display_order, shot, actor_team = team)], by = c("match_id", "display_order"), all.x = TRUE)
  pay[is.na(shot), shot := FALSE]
  # player frame: positive when it helps the recipient's team
  home <- unique(pbp[, .(match_id = as.character(match_id), team, home_away)])
  pay <- merge(pay, home, by = c("match_id", "team"), all.x = TRUE)
  pay[, pts := hm * fifelse(home_away == "Home", 1, -1)]
  conv <- pay[shot == TRUE & role == "actor" & !is.na(player_id), .(conv = sum(pts)), by = .(match_id, player_id)]
  pool_from_shots <- pay[shot == TRUE & role == "attack_pool", sum(abs(hm))]
  say("  conversion paid to kickers ", round(sum(abs(conv$conv))), " pts gross; pooled from shots ", round(pool_from_shots))
  np <- as.data.table(np)[, .(match_id, player_id, team, net_points)]
  d <- merge(np, conv, by = c("match_id", "player_id"), all.x = TRUE)
  d[is.na(conv), conv := 0]
  d[, rest := net_points - conv]
  d[, season := s]
  say("  built in ", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min")
  rm(pbp, ch, pay, a); gc()
  d
}
d <- rbindlist(lapply(c(2025L, 2026L), per_season))
fwrite(d, file.path(OUT, "np_conversion_by_game.csv"))

pgd <- as.data.table(arrow::read_parquet(file.path(OUT, "v3v4_pgd_v4.parquet"), col_select = c("match_id", "player_id", "position_group", "player_name")))
pos <- pgd[, .(pos = names(which.max(table(position_group))), name = player_name[1]), by = player_id]
ps <- merge(d[, .(g = .N, total = mean(net_points), conv = mean(conv), rest = mean(rest)), by = .(player_id, season)], pos, by = "player_id")
w <- dcast(ps[g >= MIN_G], player_id + pos + name ~ season, value.var = c("total", "conv", "rest", "g"))
w <- w[!is.na(total_2025) & !is.na(total_2026)]
say("\nPlayers with ", MIN_G, "+ games in both seasons: ", nrow(w))
say("\nPer-game means 2026 by position (10+ games): total, conversion, rest")
print(ps[season == 2026 & g >= MIN_G, .(n = .N, total = round(mean(total), 2), conv = round(mean(conv), 2), rest = round(mean(rest), 2)), by = pos][order(-total)])
r <- function(x, y) round(cor(x, y), 3)
say("\nYear-over-year correlation 2025 -> 2026 (per game):")
tab <- rbind(
  w[, .(group = "all", n = .N, total = r(total_2025, total_2026), conv = r(conv_2025, conv_2026), rest = r(rest_2025, rest_2026))],
  w[, .(n = .N, total = r(total_2025, total_2026), conv = r(conv_2025, conv_2026), rest = r(rest_2025, rest_2026)), by = .(group = pos)][n >= 15]
)
print(tab)
say("\nKey forwards: does 2025 conversion predict 2026 total beyond 2025 rest?")
kf <- w[pos == "KEY_FORWARD"]
print(summary(lm(total_2026 ~ rest_2025 + conv_2025, kf))$coefficients)
say("\nAll: same regression")
print(summary(lm(total_2026 ~ rest_2025 + conv_2025, w))$coefficients)
say("\nTop 10 conversion per game 2026 (10+ games):")
print(ps[season == 2026 & g >= MIN_G][order(-conv)][1:10, .(name, pos, g, conv = round(conv, 2), rest = round(rest, 2), total = round(total, 2))])
