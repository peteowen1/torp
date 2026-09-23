# v10: what the two flips do, separately and together (#209, #210)
# =============================================================================
# Pete decided both on 2026-09-11:
#   EPV3_CONTEST_POPULATION  "all" -> "evidence"   (duel evidence required)
#   NP_ERROR_BLAME_SHARE     0     -> 1.0          (the whole debit to the fumbler)
#
# They ship as ONE vintage because they are independent of each other -- the
# error blame rule fires on disposal turnovers, the population rule on the
# contest table -- so a single rebuild is honest here. But "independent" is a
# claim, and this script is where it gets checked: if the combined effect is not
# close to the sum of the parts, they interact and the attribution story is
# wrong.
#
# THAT IS THE WHOLE POINT OF MEASURING ALL FOUR ARMS rather than just the
# combined one. This session has repeatedly produced a confident causal story
# that only came apart when someone measured the pieces.
#
# Four arms:
#   BASE   population "all",      blame 0     <- what is published today (v9)
#   POP    population "evidence", blame 0
#   ERR    population "all",      blame 1.0
#   BOTH   population "evidence", blame 1.0   <- the proposed v10
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_v10_combined_effect.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))
ps  <- as.data.table(load_player_stats(SEASON, refresh = TRUE))
res <- as.data.table(load_results(SEASON))
pg  <- as.data.table(load_player_game_ratings(SEASON))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
pos <- unique(pg[, .(match_id, player_id, position_group)])
ORD <- c("KEY_FORWARD", "MEDIUM_FORWARD", "MIDFIELDER", "RUCK",
         "MEDIUM_DEFENDER", "KEY_DEFENDER")

run <- function(population, blame) {
  assignInNamespace("EPV3_CONTEST_POPULATION", population, ns = "torp")
  assignInNamespace("NP_ERROR_BLAME_SHARE", blame, ns = "torp")
  tm <- as.data.table(np_difficulty_terms_for_season(SEASON, pbp_data = pbp, chains = ch))
  np <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                         stoppages = "allocate", difficulty_terms = tm,
                         return_payments = TRUE)
  fin <- as.data.table(torp:::.np_team_margin(np, pbp, ps, res))
  fin[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
  merge(fin[, .(match_id, player_id, net_points)], pos,
        by = c("match_id", "player_id"), all.x = TRUE)
}

arms <- list(BASE = c("all","0"), POP = c("evidence","0"),
             ERR = c("all","1"), BOTH = c("evidence","1"))
out <- list()
for (nm in names(arms)) {
  say("\n=== arm ", nm, "  (population ", arms[[nm]][1],
      ", blame ", arms[[nm]][2], ") ===")
  out[[nm]] <- run(arms[[nm]][1], as.numeric(arms[[nm]][2]))
}
assignInNamespace("EPV3_CONTEST_POPULATION", "all", ns = "torp")
assignInNamespace("NP_ERROR_BLAME_SHARE", 0, ns = "torp")

sm <- function(d) d[!is.na(position_group), .(m = mean(net_points)), by = position_group]
tab <- Reduce(function(a,b) merge(a,b,by="position_group"),
              lapply(names(out), function(n) setnames(sm(out[[n]]), "m", n)))
tab <- tab[match(ORD, position_group)]
for (n in names(out)) tab[[n]] <- round(tab[[n]], 3)
say("\n=== published net points a game, by position ===")
print(tab)

g <- function(col) tab[position_group == "KEY_FORWARD"][[col]] -
                   tab[position_group == "KEY_DEFENDER"][[col]]
say("\nKEY_FORWARD - KEY_DEFENDER gap:")
for (n in names(out)) say("  ", formatC(n, width = -5), round(g(n), 3))

say("\n=== do the two changes INTERACT? ===")
say("If independent, BOTH - BASE should equal (POP - BASE) + (ERR - BASE).")
d <- data.table(position_group = tab$position_group,
                pop_alone = tab$POP - tab$BASE,
                err_alone = tab$ERR - tab$BASE,
                both      = tab$BOTH - tab$BASE)
d[, sum_of_parts := round(pop_alone + err_alone, 3)]
d[, interaction := round(both - sum_of_parts, 3)]
print(d)
mx <- max(abs(d$interaction))
say("\n  largest interaction term: ", round(mx, 4), " points a game")
if (mx < 0.02) {
  say("  NEGLIGIBLE -- the two are independent, so quoting them separately is")
  say("  honest and one rebuild covers both.")
} else {
  say("  NOT NEGLIGIBLE -- they interact, so the separate figures cannot be")
  say("  quoted as if they add. Report the combined number only.")
}

say("\n=== per-player movement, BASE -> BOTH ===")
m <- merge(out$BASE[, .(match_id, player_id, a = net_points)],
           out$BOTH[, .(match_id, player_id, b = net_points)],
           by = c("match_id", "player_id"))
m[, dd := b - a]
say("  player-games     : ", format(nrow(m), big.mark = ","))
say("  moved            : ", format(m[abs(dd) > 1e-9, .N], big.mark = ","))
say("  mean |move|      : ", round(mean(abs(m$dd)), 3))
say("  max  |move|      : ", round(max(abs(m$dd)), 3))
say("  moving > 1 point : ", format(m[abs(dd) > 1, .N], big.mark = ","))
