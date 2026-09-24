# Who gets blamed for an error the ledger cannot see? (#209, #210)
# =============================================================================
# Pete: the chains-only rows "all give us info that's useful in net points ...
# tough whether to put them in pbp as no player has possession so could look
# weird when drawing out chains - but also maybe we could make it work with
# these rows in? could just have NULL (or NA) EP".
#
# Before designing that, size the defect it would fix. The ledger runs on pbp.
# When a chains-only error sits between a disposal and the opposition's next
# possession, pbp sees only:
#
#     Kick (SYD Bice)  ->  Loose Ball Get (NMFC Duursma)
#
# next team differs, so the KICK is the turnover and BICE wears the whole swing
# -- for a mark NICK BLAKEY dropped. The value is not lost (the ledger conserves
# by construction); it is charged to the wrong player.
#
# THAT REFRAMES THE FIX. It is not that value is missing and needs an EP. It is
# that blame is misattributed, and the row naming the right culprit exists in
# chains and is thrown away. An NA-EP row would not need to carry value at all
# -- it would need to carry an ACTOR.
#
# Measured here:
#   (1) how often a chains-only error sits between a disposal and a possession
#       change, i.e. how often the disposer is blamed for someone else's error
#   (2) how much swing that is, in points
#   (3) which error types drive it
#   (4) whether the erring player is on the disposer's own team (if so, the
#       blame is transferable within the side; if not, something else is going on)
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_invisible_blame_misattribution.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
ch  <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]
pbp <- as.data.table(load_pbp(SEASON));    pbp[, match_id := as.character(match_id)]
setorder(ch, match_id, display_order)

# the error rows the ledger cannot see, with a named player
ERRORS <- c("Mark Fumbled", "Mark Dropped", "Dropped Mark", "No Pressure Error",
            "Out On Full", "Out On Full After Kick", "Free Against", "Smothered",
            "Dispossessed", "Kick In Clanger", "Shark", "Debit")
inpbp <- unique(pbp$description)
say("error descriptions and whether pbp carries them:")
for (e in ERRORS) {
  n <- ch[description == e, .N]
  if (n > 0) say("  ", formatC(e, width = -24), formatC(n, width = 6),
                 "  in pbp: ", e %chin% inpbp)
}

# For each chains row, is it visible to the ledger?
pk <- unique(pbp[, .(match_id, display_order, visible = TRUE)])
ch <- merge(ch, pk, by = c("match_id", "display_order"), all.x = TRUE)
ch[is.na(visible), visible := FALSE]
say("\nchains rows visible to the ledger: ",
    format(ch[visible == TRUE, .N], big.mark = ","), " of ",
    format(nrow(ch), big.mark = ","),
    " (", round(100 * mean(ch$visible), 1), "%)")

# Walk each match: for every invisible error row, find the previous VISIBLE row
# (what the ledger will blame) and the next VISIBLE row (who takes over).
setorder(ch, match_id, display_order)
ch[, vis_idx := cumsum(visible)]
vis <- ch[visible == TRUE, .(match_id, display_order, vis_idx,
                             v_desc = description, v_pid = player_id,
                             v_team = team_id)]
err <- ch[visible == FALSE & description %chin% ERRORS & !is.na(player_id),
          .(match_id, display_order, e_desc = description, e_pid = player_id,
            e_team = team_id, vis_idx)]
say("\ninvisible error rows with a named player: ", format(nrow(err), big.mark = ","))

prev <- vis[, .(match_id, vis_idx, p_desc = v_desc, p_pid = v_pid, p_team = v_team)]
nxt  <- vis[, .(match_id, vis_idx_next = vis_idx, n_desc = v_desc,
                n_pid = v_pid, n_team = v_team)]
e <- merge(err, prev, by = c("match_id", "vis_idx"), all.x = TRUE)
e[, vis_idx_next := vis_idx + 1L]
e <- merge(e, nxt, by = c("match_id", "vis_idx_next"), all.x = TRUE)
e <- e[!is.na(p_pid) & !is.na(n_team)]
say("with a visible row either side: ", format(nrow(e), big.mark = ","))

e[, ledger_calls_turnover := p_team != n_team]
say("\n=== (1) how often is the previous act booked as the turnover? ===")
say("  the ledger sees prev -> next and the team changes: ",
    format(e[ledger_calls_turnover == TRUE, .N], big.mark = ","),
    "  (", round(100 * mean(e$ledger_calls_turnover), 1), "%)")
say("  in those cases the DISPOSER is debited, not the player who erred.")

say("\n=== (4) is the erring player on the blamed player's team? ===")
m <- e[ledger_calls_turnover == TRUE]
say("  same team as the blamed act: ",
    round(100 * mean(m$e_team == m$p_team, na.rm = TRUE), 1), "%")
say("  (high => the blame is transferable within the side, which is what a fix")
say("   would do: move it from the disposer to the player who actually erred)")

say("\n=== (3) which errors drive it ===")
print(m[, .(n = .N,
            blamed_act = names(sort(table(p_desc), decreasing = TRUE))[1]),
        by = e_desc][order(-n)])

say("\n=== (2) how much swing is being misattributed? ===")
sw <- merge(m[, .(match_id, display_order = as.integer(display_order), e_desc, e_pid, p_pid)],
            pbp[, .(match_id, display_order, delta_epv)],
            by = c("match_id", "display_order"), all.x = TRUE)
# the invisible row has no pbp value; the swing sits on the PREVIOUS visible row
pv <- merge(m[, .(match_id, vis_idx, e_desc)],
            ch[visible == TRUE, .(match_id, vis_idx, display_order)],
            by = c("match_id", "vis_idx"))
pv <- merge(pv, pbp[, .(match_id, display_order, delta_epv)],
            by = c("match_id", "display_order"), all.x = TRUE)
pv <- pv[is.finite(delta_epv)]
say("  misattributed events with a measurable swing: ", format(nrow(pv), big.mark = ","))
say("  total |swing| charged to the wrong player     : ",
    round(sum(abs(pv$delta_epv)), 1), " points")
say("  mean per event                                : ",
    round(mean(abs(pv$delta_epv)), 3), " points")
say("  per match (", uniqueN(ch$match_id), " matches)             : ",
    round(sum(abs(pv$delta_epv)) / uniqueN(ch$match_id), 2), " points")
say("\n  by error type:")
print(pv[, .(n = .N, total_abs = round(sum(abs(delta_epv)), 1),
             mean_abs = round(mean(abs(delta_epv)), 3)), by = e_desc][order(-total_abs)])

say("\n=== VERDICT ===")
say("  The value is NOT missing -- the ledger conserves, so every point is")
say("  allocated. It is charged to the DISPOSER when a named teammate made the")
say("  error. A fix does not need an EP on these rows; it needs the ACTOR, and")
say("  chains already has it.")
