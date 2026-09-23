#!/usr/bin/env Rscript
# Prove the chains-aware Net Points ledger allocates IDENTICALLY to the PBP one
# ==============================================================================
# Step 1 of docs/plans/EPV-V4-CREDIT-RULES.md section 7: move the ledger's
# sequence from PBP to chains so spoils and contest targets are visible,
# without moving a single point. This script is the proof, on a full season,
# with identical() rather than all.equal() -- match the proof to the claim.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_chains_ledger_equivalence.R"'

suppressMessages(library(data.table))
options(torp.local_data_dir = NA)
devtools::load_all(quiet = TRUE)
stopifnot(is.null(get_local_data_dir()))
SEASON <- as.integer(Sys.getenv("NP_SEASON", unset = "2026"))
say <- function(...) cat(..., "\n", sep = "")

pbp <- as.data.table(load_pbp(SEASON))
ch  <- as.data.table(load_chains(SEASON))
ps  <- as.data.table(load_player_stats(SEASON, refresh = TRUE))
res <- as.data.table(load_results(SEASON))
say("season ", SEASON, ": pbp ", nrow(pbp), " rows, chains ", nrow(ch), " rows, ",
    uniqueN(pbp$match_id), " matches")

a <- build_net_points(pbp, ps, res)
b <- build_net_points(pbp, ps, res, chains = ch)
setorder(a, match_id, player_id); setorder(b, match_id, player_id)
# the np_params attribute records whether chains were supplied, so strip it:
# the DATA is what must be bit-identical
strip <- function(x) { x <- as.data.frame(x); attr(x, "np_params") <- NULL; x }
same <- identical(strip(a), strip(b))
say("\nallocation identical with and without chains: ", same)
if (!same) {
  d <- merge(a, b, by = c("match_id", "player_id"), suffixes = c(".pbp", ".chains"))
  say("max |net_points| difference: ", max(abs(d$net_points.pbp - d$net_points.chains)))
  stop("chains ledger moved value; step 1 is not done")
}

l0 <- .np_build_ledger(pbp)
l1 <- .np_build_ledger(pbp, ch)
setorder(l0, match_id, display_order); setorder(l1, match_id, display_order)
stopifnot(identical(l0[, .(match_id, display_order, hm, next_team, next_player)],
                    l1[, .(match_id, display_order, hm, next_team, next_player)]))
say("ledger rows, value and adjacency identical: TRUE (", nrow(l1), " rows)")

disp <- l1[description %chin% NP_DISPOSAL_DESCS]
disp[, kind := fcase(is.na(next_team), "terminal", next_team == team, "retained",
                     default = "turnover")]
say("\nWhat disposals resolve into (chains), by possession outcome:")
tab <- disp[, .N, by = .(kind, resolve_desc)][order(kind, -N)]
tab[, share := round(100 * N / sum(N), 1), by = kind]
print(tab[, head(.SD, 8), by = kind])

# The D8 fact: a spoil is a contest win, not a turnover. How often does the
# ATTACKING team hold the next state after a spoil?
sp <- disp[resolve_desc == "Spoil"]
say("\nspoils: ", nrow(sp), "; attack regathers (next PBP state is the kicking team): ",
    round(100 * mean(sp$kind == "retained"), 1), "%; defence wins possession: ",
    round(100 * mean(sp$kind == "turnover"), 1), "%; terminal: ",
    round(100 * mean(sp$kind == "terminal"), 1), "%")
say("spoiler is the next PBP actor: ",
    round(100 * mean(sp$resolve_player == sp$next_player, na.rm = TRUE), 1), "%")
# The scoring rule: a row after which the score moved is terminal. With chains
# we can see what those rows resolved into; they must be scores and restarts,
# never something that reads as a possession change.
term <- disp[kind == "terminal", .N, by = resolve_desc][order(-N)]
say("\nterminal disposals by resolution (scores and restarts expected):")
print(head(term, 8))
stopifnot(disp[resolve_desc == "Behind" & kind == "turnover", .N] == 0)
say("behinds classed as turnovers: 0 (asserted)")
say("\ndone")
