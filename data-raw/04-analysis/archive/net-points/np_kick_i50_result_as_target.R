# Can `Kick Inside 50 Result` serve as a contest-target marker? (#209, #210)
# =============================================================================
# Pete, on finding that the beaten contestant was named on the Kick Inside 50
# Result row: "this is great news!!!! we can use this in the contest model right"
#
# Probably -- and constants_data.R:132 already says so:
#   CHAINS_CONTEST_TARGET_DESCS <- c("Contest Target", "Kick Inside 50 Result")
# while the contest engine scans only
#   EPV3_CONTEST_TARGET_DESCS   <- c("Contest Target")
#
# But two traces show DIFFERENT behaviour and the difference matters:
#
#   1989 Kick Inside 50 Result  WB   Louis Emmett     46  -7
#   1990 Contested Mark         SYD  Jai Serong       46  -7    <- target BEATEN
#
#   1403 Kick Inside 50 Result  COLL Patrick Lipinski 41  -6
#   1404 Uncontested Mark       COLL Patrick Lipinski 41  -6    <- target WON it
#
# So it names the KICKING side's intended target either way, which is the right
# semantics. That is a hypothesis from two rows though, and some Kick Inside 50
# Result rows have no player at all ("-" in the traces).
#
# FOUR THINGS THAT MUST HOLD before it can be used as a target marker:
#   (1) COVERAGE      -- how often is a player actually named?
#   (2) TEAM          -- is the named player on the KICKING side? If it sometimes
#                        names the winner instead, using it would debit the wrong
#                        player, which is the 2026-07 interceptor bug again.
#   (3) GEOMETRY      -- does it sit at the resolution coordinates?
#   (4) GAIN          -- how many more contests get a named opponent?
#
# Verdict rule set first: usable only if the named player is on the kicking side
# in the large majority of rows AND coverage is materially better than the 2,497
# contests Contest Target currently reaches.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_kick_i50_result_as_target.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
ch <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]
setorder(ch, match_id, display_order)
for (s in c("description", "player_id", "team_id", "x", "y")) {
  ch[, (paste0("n1_", s)) := shift(get(s), 1, type = "lead"), by = match_id]
}

k <- ch[description == "Kick Inside 50 Result"]
say("Kick Inside 50 Result rows: ", format(nrow(k), big.mark = ","))
say("Contest Target rows       : ",
    format(ch[description == "Contest Target", .N], big.mark = ","))

say("\n=== (1) COVERAGE: is a player named? ===")
say("  with a player_id : ", format(k[!is.na(player_id), .N], big.mark = ","),
    "  (", round(100 * mean(!is.na(k$player_id)), 1), "%)")
say("  with a team_id   : ", format(k[!is.na(team_id), .N], big.mark = ","),
    "  (", round(100 * mean(!is.na(k$team_id)), 1), "%)")

say("\n=== (2) TEAM: is the named player on the KICKING side? ===")
say("chain_team_id is the team whose chain this is, i.e. the kicking side.")
kk <- k[!is.na(team_id) & !is.na(chain_team_id)]
say("  named player is on the CHAIN (kicking) team : ",
    format(kk[team_id == chain_team_id, .N], big.mark = ","),
    "  (", round(100 * mean(kk$team_id == kk$chain_team_id), 1), "%)")
say("  named player is on the OPPOSITION           : ",
    format(kk[team_id != chain_team_id, .N], big.mark = ","),
    "  (", round(100 * mean(kk$team_id != kk$chain_team_id), 1), "%)")
say("\nIf this were mostly OPPOSITION, the row would be naming the WINNER, and")
say("using it as the target would debit the wrong player.")

say("\n=== how it relates to the row that follows ===")
kn <- k[!is.na(player_id) & !is.na(n1_player_id)]
say("  next row is the SAME player : ",
    round(100 * mean(kn$player_id == kn$n1_player_id), 1), "%   (target won it)")
say("  next row is a DIFFERENT player: ",
    round(100 * mean(kn$player_id != kn$n1_player_id), 1), "%   (target beaten)")
say("  of the different-player cases, opposition: ",
    round(100 * kn[player_id != n1_player_id & !is.na(n1_team_id),
                   mean(n1_team_id != team_id)], 1), "%")

say("\n=== (3) GEOMETRY: same coordinates as the resolving row? ===")
kg <- k[!is.na(x) & !is.na(n1_x)]
say("  exact same x AND y : ", round(100 * mean(kg$x == kg$n1_x & kg$y == kg$n1_y), 1), "%")
say("  within 2m          : ",
    round(100 * mean(sqrt((kg$x - kg$n1_x)^2 + (kg$y - kg$n1_y)^2) <= 2), 1), "%")

say("\n=== (4) GAIN: what does the next row resolve to? ===")
r <- k[!is.na(player_id), .N, by = n1_description][order(-N)]
r[, pct := round(100 * N / sum(N), 1)]
print(head(r, 12))

say("\n=== how many contests would gain a named target? ===")
DUEL <- c("Contested Mark", "Pack Mark (P)", "Pack Mark (O)", "Spoil",
          "Spoil gaining possession", "Spoil ineffective", "Mark Fumbled",
          "Mark Dropped")
now <- ch[description == "Contest Target" & n1_description %chin% DUEL, .N]
add <- k[!is.na(player_id) & n1_description %chin% DUEL, .N]
say("  named by Contest Target today        : ", format(now, big.mark = ","))
say("  additionally named by Kick I50 Result: ", format(add, big.mark = ","))
say("  combined                              : ", format(now + add, big.mark = ","))

say("\n=== VERDICT ===")
own <- mean(kk$team_id == kk$chain_team_id)
cov <- mean(!is.na(k$player_id))
say("  coverage ", round(100 * cov, 1), "%   named-player-is-kicking-side ",
    round(100 * own, 1), "%")
if (own > 0.9 && cov > 0.5) {
  say("  USABLE. It names the kicking side's intended target, with real coverage.")
  say("  Adding it to EPV3_CONTEST_TARGET_DESCS would name the beaten opponent on")
  say("  ", format(add, big.mark = ","), " more contests than Contest Target reaches alone.")
} else if (own <= 0.9) {
  say("  NOT SAFE AS-IS. It names the opposition too often, so it is not reliably")
  say("  the intended target -- using it would debit the wrong player.")
} else {
  say("  COVERAGE TOO THIN to be worth wiring.")
}
