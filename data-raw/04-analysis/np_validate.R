# np_validate.R -- does torp's Net Points hold its identity, and do the numbers
# read like football?
#
# The same validation panna's net goals ledger gets, run against torp, so the
# two can be compared on the same terms:
#   1. does each team's players sum to that team's own margin?
#   2. top and bottom players, and the spread within each position
#   3. where the points come from, by channel and by play type
#   4. how much is paid to a NAMED player versus spread into a pool by proxy
#
# Question 4 is the honest one. torp's own scoping doc
# (docs/plans/NET-POINTS-TEAM-SUM-CONVENTION.md section 4) put the pool share at
# 17% before the team-sum convention and predicted ~55% after it, because AFL
# has no per-moment on-ground data and the whole defensive half must be proxied.
# It shipped anyway. This measures what the share actually came out at.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_validate.R"'

suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)

SEASON   <- 2026
MIN_GMS  <- 8

say <- function(...) cat(..., "\n", sep = "")
say("constants: EPV_ENGINE ", EPV_ENGINE, " | vintage ", RATING_VINTAGE,
    " | pool by ", NP_TEAM_MARGIN_POOL_BY, " | blame pool ", NP_BLAME_POOL,
    " | siren to pool ", NP_SIREN_TO_POOL)

pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))
ps  <- as.data.table(load_player_stats(SEASON, refresh = TRUE))
res <- as.data.table(load_results(SEASON))
tm  <- as.data.table(np_difficulty_terms_for_season(SEASON, pbp_data = pbp, chains = ch))

np <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                       stoppages = "allocate", difficulty_terms = tm,
                       return_payments = TRUE)
pay <- as.data.table(attr(np, "np_payments"))
fin <- as.data.table(torp:::.np_team_margin(np, pbp, ps, res))
fin[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]

# ---- 1. TEAM TOTALS --------------------------------------------------------
say("\n==== 1. DOES EACH TEAM SUM TO ITS OWN MARGIN? ====")
say("Winners of a 63-point win should sum to +63, losers to -63, match to zero.")
say("cor and slope nearer 1.000 are better; lower error is better.\n")

# `fin$team` is the full club name ("Adelaide Crows"), so join on the name
# column, not the abbreviation. Team naming is not canonical across sources
# here (see torp/CLAUDE.md on AFL_TEAM_ALIASES), so the join is ASSERTED below
# rather than assumed: an unmatched team silently yields a margin of the wrong
# sign and a correlation of zero, which reads as a broken ledger.
r <- as.data.table(res)[, .(match_id = as.character(match_id),
                            home_team = home_team_name,
                            away_team = away_team_name,
                            home_score, away_score)]
tt <- fin[, .(own_total = sum(net_points, na.rm = TRUE)), by = .(match_id, team)]
tt <- merge(tt, r, by = "match_id")
tt[, side := fifelse(team == home_team, "home",
                     fifelse(team == away_team, "away", NA_character_))]
if (anyNA(tt$side)) {
  bad <- sort(unique(tt[is.na(side)]$team))
  stop(sprintf("%d team-match rows matched neither side. Unmatched team names: %s",
               sum(is.na(tt$side)), paste(utils::head(bad, 6), collapse = ", ")))
}
tt[, own_margin := fifelse(side == "home", home_score - away_score,
                           away_score - home_score)]
tt[, err := own_total - own_margin]
say(sprintf("team-matches            : %d", nrow(tt)))
say(sprintf("cor(total, own margin)  : %.4f", cor(tt$own_total, tt$own_margin)))
say(sprintf("slope                   : %.4f", coef(lm(own_margin ~ own_total, tt))[2]))
say(sprintf("median |error|          : %.3f points", median(abs(tt$err))))
say(sprintf("max |error|             : %.3f points", max(abs(tt$err))))
z <- tt[, .(s = sum(own_total)), by = match_id]
say(sprintf("the two sides cancel to : %.2e", max(abs(z$s))))

say("\nthe biggest wins:")
print(head(tt[order(-abs(own_margin)), .(team, own_margin,
                                         own_total = round(own_total, 3))], 8),
      row.names = FALSE)

# ---- 2. PLAYERS AND POSITIONS ---------------------------------------------
pg <- fin[, .(np = sum(net_points, na.rm = TRUE), gms = .N), by = player_id]
# ONE row per player. `ps` has a row per player-MATCH, so its `position` is the
# slot he filled that day; unique() over it gives a player as many rows as he
# had slots and the merge below duplicates his season total once per slot.
# (Nick Watson appeared three times at 188.6 before this.) Take the modal slot.
nm <- ps[, .N, by = .(player_id = as.character(player_id), player_name, position)]
setorder(nm, player_id, -N)
nm <- nm[, .SD[1], by = player_id][, .(player_id, player_name, position)]
pg <- merge(pg, nm, by = "player_id", all.x = TRUE)[gms >= MIN_GMS]
pg[, per_game := np / gms]

say("\n==== 2. TOP 20 BY NET POINTS (min ", MIN_GMS, " games) ====")
print(head(pg[order(-np), .(player_name, position, gms,
                            net = round(np, 1), per_game = round(per_game, 2))], 20),
      row.names = FALSE)
say("\n---- bottom 10 ----")
print(head(pg[order(np), .(player_name, position, gms,
                           net = round(np, 1), per_game = round(per_game, 2))], 10),
      row.names = FALSE)

say("\n==== 2b. SPREAD BY POSITION (per player-game) ====")
say("A larger sd means the metric separates players inside that position more.\n")
pgm <- merge(fin[, .(np = sum(net_points, na.rm = TRUE)), by = .(player_id, match_id)],
             nm, by = "player_id", all.x = TRUE)
a <- pgm[!is.na(position), .(player_games = .N,
                             mean = round(mean(np), 3), sd = round(sd(np), 3),
                             p5 = round(quantile(np, .05), 2),
                             p95 = round(quantile(np, .95), 2)),
         by = position][order(-mean)]
print(a, row.names = FALSE)
say(sprintf("\nall positions pooled: mean %.3f, sd %.3f (n = %d player-games)",
            mean(pgm$np), sd(pgm$np), nrow(pgm)))

# ---- 3. WHERE THE POINTS COME FROM ----------------------------------------
# The payment table carries `role` and `hm` (the home-frame value); it has no
# play_type column, so the breakdown is by role only.
say("
==== 3. BY ROLE ====")
say("total = season sum in points; share_abs = share of all absolute value.
")
val <- if ("hm" %in% names(pay)) "hm" else "value"
tot_abs <- sum(abs(pay[[val]]), na.rm = TRUE)
b <- pay[, .(n = .N, total = round(sum(get(val), na.rm = TRUE), 1),
             share_abs = round(100 * sum(abs(get(val)), na.rm = TRUE) / tot_abs, 1)),
         by = role][order(-share_abs)]
print(b, row.names = FALSE)

# ---- 4. NAMED vs PROXY -----------------------------------------------------
say("
==== 4. NAMED vs PROXY ====")
say("A named payment goes to the player the feed identified; a proxy payment is")
say("spread into a pool because nobody was. torp's scoping doc predicted ~55%")
say("proxy under this convention, against 17% before it.
")
pay[, kind := fifelse(grepl("pool", role, ignore.case = TRUE), "proxy (pool)", "named")]
k <- pay[, .(abs_value = round(sum(abs(get(val)), na.rm = TRUE), 1)), by = kind]
k[, pct := round(100 * abs_value / sum(abs_value), 1)]
print(k[order(-abs_value)], row.names = FALSE)
