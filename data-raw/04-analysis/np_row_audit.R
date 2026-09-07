# A row-level audit of where every point of value goes
# =============================================================================
# Pete's structure: put the shares on the rows themselves, so a row shows how
# its value split and a player's total is a sum you can read rather than a
# number you have to reconstruct. Written 2026-09-07 after three failed attempts
# to explain a 5.3-point discrepancy on one player by reasoning about the code.
#
# One row per (play-by-play row, recipient). Columns:
#   row_value     what the row was worth, home-margin frame
#   side          did this recipient's team GAIN or CONCEDE on this row
#   role          how he earned it: actor, receiver, ball_winner, ...
#   raw           what the shipped ledger pays him
#   raw_share     his share of the row under the shipped ledger
#   conv_full     his share after the convention rescales his side to the full
#                 row value
#   conv_named    what he keeps after the named/pool split
#   conv_share    conv_named as a share of the row
#
# Reading it: under the shipped ledger the shares across BOTH teams sum to 1 on
# every row. Under the convention each side separately sums to 1, because the
# row is booked twice.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_row_audit.R"'
#
# Env: NP_AUDIT_MATCH (default the first 2026 match), NP_AUDIT_PLAYER (optional
# player_id to filter to), NP_AUDIT_SEASON (default 2026).
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
stopifnot(is.null(get_local_data_dir()))
S <- "data-raw/outputs"
SEASON <- as.integer(Sys.getenv("NP_AUDIT_SEASON", "2026"))
MATCH  <- Sys.getenv("NP_AUDIT_MATCH", "CD_M20260140001")
WHO    <- Sys.getenv("NP_AUDIT_PLAYER", "")
NS     <- NP_TEAM_MARGIN_NAMED_SHARE
say <- function(...) cat(..., "\n", sep = "")

pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))
ps  <- as.data.table(load_player_stats(SEASON, refresh = TRUE))
res <- as.data.table(load_results(SEASON))
tm  <- fread(file.path(S, "np_difficulty_terms_2025_2026.csv"))
tm[, match_id := as.character(match_id)]
tm <- tm[substr(match_id, 5, 8) == as.character(SEASON)]

np <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                       stoppages = "allocate", difficulty_terms = tm,
                       return_payments = TRUE)
pay <- as.data.table(attr(np, "np_payments")); pay[, match_id := as.character(match_id)]
ha <- unique(pbp[, .(match_id, team, home_away)])
pay <- merge(pay, ha, by = c("match_id", "team"), all.x = TRUE)
pay[, own := hm * fifelse(home_away == "Home", 1, -1)]
pay[, row_value := sum(hm), by = .(match_id, display_order)]
pay <- pay[abs(row_value) > 1e-12]
pay[, gain_home := row_value > 0]
pay[, side := fifelse((home_away == "Home") == gain_home, "gain", "concede")]
pay[, side_sum := sum(own), by = .(match_id, display_order, side)]
pay[, target := fifelse(side == "gain", abs(row_value), -abs(row_value))]
pay[, conv_full := fifelse(abs(side_sum) > 0.05 * abs(row_value),
                           own * target / side_sum, NA_real_)]
pay[, named_tot := sum(conv_full, na.rm = TRUE),
    by = .(match_id, display_order, team)]
pay[, conv_named := fifelse(!is.na(conv_full) & abs(named_tot) > 1e-12,
                            conv_full * NS * target / named_tot, NA_real_)]
pay[, `:=`(raw_share = own / row_value,
           conv_share = conv_named / target)]

desc <- unique(rbind(pbp[, .(match_id, display_order, description)],
                     ch[, .(match_id = as.character(match_id), display_order, description)]))
nm <- unique(pbp[!is.na(player_id), .(player_id = as.character(player_id), player_name)])
aud <- merge(pay, desc, by = c("match_id", "display_order"), all.x = TRUE)
aud[, player_id := as.character(player_id)]
aud <- merge(aud, nm, by = "player_id", all.x = TRUE)
aud <- aud[, .(match_id, display_order, description, team, side, role,
               who = fifelse(is.na(player_name), "(team pool)", player_name),
               player_id, row_value = round(row_value, 3),
               raw = round(own, 3), raw_share = round(raw_share, 3),
               conv_full = round(conv_full, 3), conv_named = round(conv_named, 3),
               conv_share = round(conv_share, 3))]
setorder(aud, match_id, display_order, -raw)
fwrite(aud, file.path(S, "np_row_audit.csv"))
say("wrote np_row_audit.csv: ", nrow(aud), " payment rows")

# --- the checks the structure makes cheap ------------------------------------
say("\n=== does every row's shipped payments sum to the row value? ===")
chk <- aud[, .(paid = sum(raw), row_value = row_value[1]), by = .(match_id, display_order)]
say("  max |paid - row value|: ", signif(max(abs(chk$paid - chk$row_value)), 3))

say("\n=== does each SIDE of a row sum to the full row value after the rescale? ===")
c2 <- aud[!is.na(conv_full), .(got = sum(conv_full), want = sign(sum(conv_full)) * abs(row_value[1])),
          by = .(match_id, display_order, side)]
say("  max |got - want|: ", signif(max(abs(c2$got - c2$want)), 3),
    " over ", nrow(c2), " row-sides")

say("\n=== and after the named split, does each side keep exactly ", NS, "? ===")
c3 <- aud[!is.na(conv_named), .(kept = sum(conv_named), target = sign(sum(conv_named)) * abs(row_value[1])),
          by = .(match_id, display_order, side)]
c3[, ratio := kept / target]
say("  median ratio ", round(median(c3$ratio), 4), " | min ", round(min(c3$ratio), 4),
    " | max ", round(max(c3$ratio), 4), " (all should be ", NS, ")")

a <- aud[match_id == MATCH]
if (nzchar(WHO)) a <- a[player_id == WHO]
say("\n=== ", MATCH, if (nzchar(WHO)) paste0(", player ", WHO) else "", " ===")
say("per-player totals, shipped against the convention's named part:")
tot <- a[!is.na(player_id), .(rows = .N, shipped = round(sum(raw), 2),
                              conv_named = round(sum(conv_named, na.rm = TRUE), 2)),
         by = .(who, team)][order(-shipped)]
print(head(tot, 12))
