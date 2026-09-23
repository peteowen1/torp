# Is rucks' stoppage credit the right size? (Pete, 2026-09-23)
# =============================================================================
# Rucks average +2.65 a game from stoppages (v14): +0.72 from hitouts and +1.93
# from first possessions. A ruck's job IS stoppages, so the question is size.
# Test: a team's stoppage value per game with its most-used ruck playing vs in
# games he missed, beside what was paid to whoever rucked in each. If the team
# drop is bigger than the pay drop, rucks are not overpaid.
#
# Result (2026-09-23): team drop 1.98 (2025, n=38 missed) and 2.31 (2026, n=60),
# ruck pay drop 1.02 and 1.55 -- rucks are paid at most what they are worth.
# (A per-team-game "team stoppage vs ruck pay" comparison is uninformative:
# both sides of every stoppage are booked, so both average exactly zero.)
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_ruck_check.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")
source("data-raw/04-analysis/np_season_build.R")

one <- function(season) {
  b <- np_season_build(season)
  fin <- torp:::.np_team_margin(b$np, b$pbp, b$ps, b$res)
  paid <- as.data.table(attr(fin, "np_team_margin_payments"))
  prow <- as.data.table(attr(fin, "np_team_margin_pool_rows"))
  for (d in list(paid, prow)) d[, match_id := as.character(match_id)]
  st <- unique(b$pbp[description %in% c("Centre Bounce", "Ball Up Call", "Throw In", "Boundary Throw In") |
                       (is.na(player_id) & grepl("Bounce|Ball Up|Throw", description)),
                     .(match_id = as.character(match_id), display_order)])
  book <- rbind(merge(paid, st, by = c("match_id", "display_order"))[, .(match_id, team, v = paid)],
                merge(prow, st, by = c("match_id", "display_order"))[, .(match_id, team, v = pool)])
  team_st <- book[, .(team_stoppage = sum(v)), by = .(match_id, team)]
    pg <- as.data.table(load_player_game_ratings(season))
  pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
  rk <- pg[position_group == "RUCK", .N, by = .(player_id, team)][order(team, -N)][, .SD[1], by = team]
  # every stoppage-row payment to whoever lined up as a ruck that match (the
  # replacement included), so "with" and "without" compare like with like
  rp <- merge(merge(paid, st, by = c("match_id", "display_order"))[, player_id := as.character(player_id)],
              unique(pg[position_group == "RUCK", .(match_id, player_id)]), by = c("match_id", "player_id"))
  ruck_pos <- rp[, .(ruck_pos_pay = sum(paid)), by = .(match_id, team)]
  list(season = season, team_st = team_st, first_ruck = rk, ruck_pos = ruck_pos,
       played = unique(pg[, .(match_id, player_id)]), n_st = nrow(st))
}
for (s in c(2025, 2026)) {
  r <- one(s)
  say("\n=== ", s, ": ", r$n_st, " stoppage rows, ", uniqueN(r$team_st$match_id), " matches")
  fr <- merge(r$team_st, r$first_ruck[, .(team, player_id)], by = "team")
  fr[, with := paste(match_id, player_id) %in% r$played[, paste(match_id, player_id)]]
  fr <- merge(fr, r$ruck_pos, by = c("match_id", "team"), all.x = TRUE)
  fr[is.na(ruck_pos_pay), ruck_pos_pay := 0]
  w <- fr[, .(games = .N, team_stoppage = round(mean(team_stoppage), 2), se = round(sd(team_stoppage) / sqrt(.N), 2),
              paid_to_rucks = round(mean(ruck_pos_pay), 2)), by = with]
  say("2. with / without the team's first ruck:"); print(w)
}
