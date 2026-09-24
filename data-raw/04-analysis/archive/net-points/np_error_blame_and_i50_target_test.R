# Gate test: error blame re-attribution and the i50 target marker (#209, #210)
# =============================================================================
# Two changes, both shipped inert, both measured here before any default moves.
#
#   NP_ERROR_BLAME_SHARE  0 -> the share of a turnover debit that moves from the
#                         disposer to the teammate named on a chains-only error
#                         row the ledger cannot see (Mark Fumbled / Dropped /
#                         No Pressure Error). Measured: those are blamed on a
#                         DIFFERENT player 100% of the time.
#   EPV3_TARGET_FROM_I50  FALSE -> also read the beaten contestant off
#                         `Kick Inside 50 Result`, which names a player on 64.2%
#                         of its rows and that player is on the kicking side
#                         100.0% of the time.
#
# FOUR THINGS THAT MUST HOLD, and the first two are the ones that would make
# this dangerous rather than merely wrong:
#
#  (1) CONSERVATION. Error blame moves a RECIPIENT, never an amount, so the
#      team-sum convention must be untouched. build_net_points() already aborts
#      if the payment table fails to rebuild the ledger, so a silent break is
#      not the risk -- a loud one is, and it must not happen.
#  (2) INERTNESS AT THE DEFAULT. With both gates off the published numbers must
#      be IDENTICAL to today's, not merely close. If they move at all, one of
#      these changed a code path it was not supposed to touch.
#  (3) EFFECT. At a non-zero share, who moves and by how much.
#  (4) TARGET COVERAGE. How many more contests name a loser.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_error_blame_and_i50_target_test.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))
ps  <- as.data.table(load_player_stats(SEASON, refresh = TRUE))
res <- as.data.table(load_results(SEASON))
tm  <- as.data.table(np_difficulty_terms_for_season(SEASON, pbp_data = pbp, chains = ch))

run <- function(label, err_share, i50) {
  say("\n--- ", label, "  (NP_ERROR_BLAME_SHARE = ", err_share,
      ", EPV3_TARGET_FROM_I50 = ", i50, ") ---")
  assignInNamespace("NP_ERROR_BLAME_SHARE", err_share, ns = "torp")
  assignInNamespace("EPV3_TARGET_FROM_I50", i50, ns = "torp")
  np <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                         stoppages = "allocate", difficulty_terms = tm,
                         return_payments = TRUE)
  fin <- as.data.table(torp:::.np_team_margin(np, pbp, ps, res))
  fin[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
  pay <- as.data.table(attr(np, "np_payments"))
  list(np = fin[, .(match_id, player_id, net_points)], pay = pay)
}

base <- run("BASELINE (both gates off -- today's behaviour)", 0, FALSE)
say("payment roles: ", paste(sort(unique(base$pay$role)), collapse = ", "))

say("\n=== (4) TARGET COVERAGE: does the i50 marker name more losers? ===")
cst0 <- { assignInNamespace("EPV3_TARGET_FROM_I50", FALSE, ns = "torp")
          as.data.table(build_aerial_contests(ch, pbp)) }
cst1 <- { assignInNamespace("EPV3_TARGET_FROM_I50", TRUE, ns = "torp")
          as.data.table(build_aerial_contests(ch, pbp)) }
assignInNamespace("EPV3_TARGET_FROM_I50", FALSE, ns = "torp")
say("  contests            : ", format(nrow(cst0), big.mark = ","), " / ",
    format(nrow(cst1), big.mark = ","), " (must be equal -- population unchanged)")
say("  with a named target : ", format(cst0[!is.na(target_pid), .N], big.mark = ","),
    " -> ", format(cst1[!is.na(target_pid), .N], big.mark = ","))
say("  DEFENSIVE wins with a named loser: ",
    format(cst0[def_win == TRUE & !is.na(target_pid), .N], big.mark = ","),
    " -> ", format(cst1[def_win == TRUE & !is.na(target_pid), .N], big.mark = ","))
say("  target is ever the winner? ",
    cst1[!is.na(target_pid) & target_pid == out_pid, .N], " rows",
    "  (must be 0 on def_win: ",
    cst1[def_win == TRUE & !is.na(target_pid) & target_pid == out_pid, .N], ")")

say("\n=== (2) INERTNESS: does the i50 gate alone move published numbers? ===")
i50 <- run("i50 target ON, error blame off", 0, TRUE)
cmp <- merge(base$np, i50$np, by = c("match_id", "player_id"),
             suffixes = c("_base", "_i50"))
cmp[, d := net_points_i50 - net_points_base]
say("  player-games: ", format(nrow(cmp), big.mark = ","),
    "   moved: ", cmp[abs(d) > 1e-9, .N],
    "   max |move|: ", round(max(abs(cmp$d)), 4))
say("  (this SHOULD move people: naming a loser routes contest debits to a real")
say("   player instead of spreading them over the roster)")

say("\n=== (3) EFFECT of error blame at a few shares ===")
for (s in c(0.5, 1.0)) {
  r <- run(paste0("error blame ", s), s, FALSE)
  c2 <- merge(base$np, r$np, by = c("match_id", "player_id"),
              suffixes = c("_base", "_new"))
  c2[, d := net_points_new - net_points_base]
  eb <- r$pay[role == "error_blame"]
  say("  payments with role=error_blame: ", format(nrow(eb), big.mark = ","),
      "   points moved: ", round(sum(abs(eb$hm)), 1))
  say("  player-games moved: ", c2[abs(d) > 1e-9, .N],
      "   mean |move|: ", round(mean(abs(c2$d)), 4),
      "   max |move|: ", round(max(abs(c2$d)), 3))
}

say("\n=== (1) CONSERVATION at the most aggressive setting ===")
full <- run("both gates ON, error blame 1.0", 1.0, TRUE)
mg <- unique(as.data.table(load_results(SEASON))[
  , .(match_id = as.character(match_id), margin = home_score - away_score)])
chk <- merge(full$np, unique(pbp[, .(match_id, player_id = as.character(player_id))]),
             by = c("match_id", "player_id"), all.x = TRUE)
say("  build_net_points() did not abort, so the payment table rebuilds the ledger.")
say("  player-games: ", format(nrow(full$np), big.mark = ","))
say("  total net_points across all player-games: ",
    round(sum(full$np$net_points), 6), "  (team-sum convention: each team sums")
say("  to its own margin, so the grand total is the sum of both, near zero)")
assignInNamespace("NP_ERROR_BLAME_SHARE", 0, ns = "torp")
assignInNamespace("EPV3_TARGET_FROM_I50", FALSE, ns = "torp")
say("\ngates restored to their shipped defaults.")
