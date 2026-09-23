# Net points edge cases, checked on a real season
# =============================================================================
# The AFL twin of panna's ng_scenarios.R (vault/plans/NET-LEDGER-PARITY.md):
# the awkward real cases on 2026, each reported with how many rows it covered
# so a check with nothing to test fails rather than passing vacuously.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_scenarios.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
source("data-raw/04-analysis/np_season_build.R")
b <- np_season_build(2026)
fin <- torp:::.np_team_margin(b$np, b$pbp, b$ps, b$res)
paid <- as.data.table(attr(fin, "np_team_margin_payments"))
prow <- as.data.table(attr(fin, "np_team_margin_pool_rows"))
parts <- as.data.table(attr(fin, "np_team_margin_parts"))
fin <- as.data.table(fin); res <- b$res
res_ <- list()
chk <- function(name, n, ok, detail = "") {
  res_[[length(res_) + 1]] <<- data.table(check = name, n = n,
    result = if (n == 0) "NO DATA" else if (isTRUE(ok)) "pass" else "FAIL", detail = detail)
}
for (d in list(paid, prow, parts, fin)) d[, match_id := as.character(match_id)]

# 1. Double entry: per row, the two sides' bookings are equal and opposite.
side <- rbind(paid[, .(match_id, display_order, team, v = paid)],
              prow[, .(match_id, display_order, team, v = pool)])[, .(v = sum(v)), by = .(match_id, display_order, team)]
rw <- side[, .(n_sides = .N, net = sum(v)), by = .(match_id, display_order)]
chk("every row books both sides", nrow(rw), all(rw$n_sides == 2),
    sprintf("%d rows one-sided", rw[n_sides != 2, .N]))
chk("the two sides are equal and opposite", nrow(rw), max(abs(rw$net)) < 1e-6,
    sprintf("worst %.1e", max(abs(rw$net))))

# 2. Every team on its own margin; draws sum to 0 per side.
ha <- unique(b$pbp[!is.na(team) & !is.na(home_away), .(match_id, team, home_away)])
tt <- merge(merge(fin[, .(got = sum(net_points)), by = .(match_id, team)], ha, by = c("match_id", "team")),
            res[, .(match_id, m = home_score - away_score)], by = "match_id")
tt[, want := fifelse(home_away == "Home", m, -m)]
chk("every team lands on its own margin", nrow(tt), max(abs(tt$got - tt$want)) < 1e-6)
chk("draws sum to 0 per side", tt[m == 0, .N], tt[m == 0, max(abs(got))] < 1e-6)

# 3. The parts rebuild each player's net points exactly.
rb <- merge(paid[, .(named = sum(paid)), by = .(match_id, player_id = as.character(player_id))],
            parts[, .(match_id, player_id = as.character(player_id), share, recon)],
            by = c("match_id", "player_id"), all = TRUE)
rb[is.na(named), named := 0]
rb <- merge(rb, fin[, .(match_id, player_id = as.character(player_id), net_points)], by = c("match_id", "player_id"))
chk("named + pool share + anchor = net points", nrow(rb),
    max(abs(rb$named + rb$share + rb$recon - rb$net_points)) < 1e-6)

# 4. The anchor stays small now every row is booked (41.9% before v14).
a <- sum(abs(parts$recon)) / (sum(abs(parts$named)) + sum(abs(parts$share)) + sum(abs(parts$recon)))
chk("anchor under 5% of absolute value", nrow(parts), a < 0.05, sprintf("%.1f%%", 100 * a))

# 5. Stoppage rows (no acting player) are still booked both sides.
st <- unique(b$pbp[is.na(player_id) & !is.na(match_id), .(match_id = as.character(match_id), display_order)])
stb <- merge(rw, st, by = c("match_id", "display_order"))
chk("stoppage rows book both sides", nrow(stb), all(stb$n_sides == 2))

# 6. Quarter ends: the last row of each quarter is still booked and conserves.
qe <- b$pbp[!is.na(period), .(display_order = max(display_order)), by = .(match_id = as.character(match_id), period)]
qb <- merge(rw, qe, by = c("match_id", "display_order"))
chk("quarter-end rows conserve", nrow(qb), max(abs(qb$net)) < 1e-6)

# 7. Plausibility: player-match net points and single payments within bounds.
chk("no player-match beyond +/-40 points", nrow(fin), max(abs(fin$net_points)) < 40,
    sprintf("range %.1f to %.1f", min(fin$net_points), max(fin$net_points)))
chk("no single named payment beyond 7 points (a goal is 6)", nrow(paid), max(abs(paid$paid)) < 7,
    sprintf("max %.2f", max(abs(paid$paid))))

# 8. Players with no time on ground on record (the 0.75 default).
nt <- fin[, .(player_id = as.character(player_id), match_id)][!paste(match_id, player_id) %in%
        b$ps[, paste(as.character(match_id), as.character(player_id))]]
chk("paid players missing a time-on-ground row", nrow(fin), nrow(nt) < 0.001 * nrow(fin),
    sprintf("%d of %d player-matches", nrow(nt), nrow(fin)))

out <- rbindlist(res_)
print(out, row.names = FALSE)
cat("\n", sum(out$result == "pass"), "of", nrow(out), "passed;", sum(out$result == "FAIL"), "failed;",
    sum(out$result == "NO DATA"), "had nothing to test\n")
