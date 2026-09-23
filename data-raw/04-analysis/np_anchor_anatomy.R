# What is the team-margin anchor actually made of?
# =============================================================================
# The play-type page (build_np_categories_artifact.R) now splits every player's
# net points exactly into named payments + team pool share + anchor. On 2026 the
# anchor is 36% of all absolute value, about -4.2 a player-game, where panna's
# equivalent step moves 7.6%. Hypothesis: on rows where a team CONCEDES (above
# all, the opponent's scores), no payment is booked for the conceding side, so
# .np_team_margin() never charges it and the whole debit lands in `recon`.
#
# Tested per team-match: recon total vs minus the opponent's score, and how
# much of the value on rows with no conceding-side payment explains it.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_anchor_anatomy.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")
SEASON <- 2026
CACHE <- "data-raw/outputs/np_anchor_inputs_2026.rds"

if (file.exists(CACHE)) {
  x <- readRDS(CACHE)
} else {
  pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
  ch  <- as.data.table(load_chains(SEASON))
  ps  <- as.data.table(load_player_stats(SEASON, refresh = TRUE))
  res <- as.data.table(load_results(SEASON))
  tm  <- as.data.table(np_difficulty_terms_for_season(SEASON, pbp_data = pbp, chains = ch))
  np  <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                          stoppages = "allocate", difficulty_terms = tm,
                          return_payments = TRUE)
  fin <- torp:::.np_team_margin(np, pbp, ps, res)
  x <- list(pay = as.data.table(attr(np, "np_payments")),
            parts = as.data.table(attr(fin, "np_team_margin_parts")),
            res = res,
            ha = unique(pbp[!is.na(team), .(match_id, team, home_away)]),
            desc = unique(pbp[, .(match_id, display_order, description)]))
  saveRDS(x, CACHE)   # so the next question costs seconds, not a rebuild
}
pay <- x$pay; parts <- x$parts; res <- x$res; ha <- x$ha
pay[, match_id := as.character(match_id)]; parts[, match_id := as.character(match_id)]
res[, match_id := as.character(match_id)]
say("payments ", nrow(pay), " | player-matches ", nrow(parts), " | matches ", uniqueN(parts$match_id))
stopifnot(nrow(pay) > 0, nrow(parts) > 0)

# 1. recon per team-match against the opponent's score
rt <- parts[, .(recon = sum(recon), named = sum(named), share = sum(share)), by = .(match_id, team)]
rt <- merge(rt, ha, by = c("match_id", "team"))
rt <- merge(rt, res[, .(match_id, home_score, away_score)], by = "match_id")
rt[, `:=`(own_score = fifelse(home_away == "Home", home_score, away_score),
          opp_score = fifelse(home_away == "Home", away_score, home_score))]
say("\n1. Per team-match (n = ", nrow(rt), "), points:")
say("   named  mean ", round(mean(rt$named), 1), "  | own score mean ", round(mean(rt$own_score), 1),
    "  | corr(named, own score) ", round(cor(rt$named, rt$own_score), 3))
say("   recon  mean ", round(mean(rt$recon), 1), "  | opp score mean ", round(mean(rt$opp_score), 1),
    "  | corr(recon, -opp score) ", round(cor(rt$recon, -rt$opp_score), 3))

# 2. rows where one side has no payment at all
pay <- merge(pay, ha, by = c("match_id", "team"), all.x = TRUE)
if (!"doubled" %in% names(pay)) pay[, doubled := FALSE]
rows <- pay[, .(v = sum(hm[doubled == FALSE]), n_teams = uniqueN(team[!is.na(team)])),
            by = .(match_id, display_order)]
rows <- merge(rows, x$desc, by = c("match_id", "display_order"), all.x = TRUE)
one <- rows[n_teams == 1]
say("\n2. Rows paid to ONE side only: ", nrow(one), " of ", nrow(rows),
    " (", round(100 * nrow(one) / nrow(rows), 1), "%), carrying ",
    round(sum(abs(one$v))), " of ", round(sum(abs(rows$v))), " points of |value| (",
    round(100 * sum(abs(one$v)) / sum(abs(rows$v)), 1), "%)")
say("   by description (top 10 by |value|):")
print(one[, .(rows = .N, abs_value = round(sum(abs(v)))), by = description][order(-abs_value)][1:10])
