# C1 (plan §7.3) — the `disp` channel diagnostic. The last unexamined EPV
# channel, and 27% of a key defender's rating SD (§6.9a) against the spoil
# channel's 11% -- 2.5x the lever round 1 spent most of its effort on.
#
# DIAGNOSTIC ONLY. No repricing proposed here. The discipline that would have
# saved round 1 a week is: establish that a sub-type is defender-CONCENTRATED
# (the way intercept marks are, 7x KD vs KF) before touching any constant.
# A channel that is merely large is not a lever -- §6.6 showed that scaling a
# channel everyone shares wrecks the buckets you were not aiming at.
#
# What the code does today (player_credit.R:120-136):
#   epv_disp = sum( fifelse(pos_team == -1, delta_epv + DISP_NEG_OFFSET,
#                                           delta_epv + DISP_POS_OFFSET) * scale )
#   scale = EPV_DISP_SCALE (0.5), or contest_share (1/3) on contested kicks
# with EPV_DISP_NEG_OFFSET = EPV_DISP_POS_OFFSET = 0.0000, so the fifelse is
# currently a NO-OP: both branches are identical. Two untuned placeholders sit
# on a branch that does nothing. Confirmed below rather than assumed.
suppressMessages({library(arrow); library(data.table)})

D  <- "C:/dev/torpverse/torpdata/data/"
S  <- 2023:2025
BK <- c("KEY_DEFENDER","MEDIUM_DEFENDER","MIDFIELDER","RUCK","MEDIUM_FORWARD","KEY_FORWARD")
DISP_SCALE <- 0.5; CONTEST_SHARE <- 1/3

PB_COLS <- c("match_id","player_id","description","disposal","delta_epv","pos_team",
             "x","goal_x","contest_target_id","venue_length","season")

cat("loading pbp (column-selected)...\n")
pb <- rbindlist(lapply(S, function(s) {
  ds <- arrow::open_dataset(file.path(D, sprintf("pbp_data_%d_all.parquet", s)))
  as.data.table(ds |> dplyr::select(dplyr::any_of(PB_COLS)) |> dplyr::collect())
}), use.names = TRUE, fill = TRUE)

pg <- rbindlist(lapply(S, function(s)
  as.data.table(read_parquet(file.path(D, sprintf("player_game_%d.parquet", s))))[
    , .(player_id, match_id, position_group, lineup_position)]), use.names=TRUE, fill=TRUE)
pg <- unique(pg[position_group %in% BK & !lineup_position %in% c("EMERG","SUB")],
             by = c("player_id","match_id"))

# --- disposal rows only, credited exactly as production credits them --------
d <- pb[!is.na(player_id) & !is.na(delta_epv)]
d <- d[disposal == 1 | grepl("^(Kick|Handball)", description)]
d[, scale := fifelse(!is.na(contest_target_id), CONTEST_SHARE, DISP_SCALE)]
d[, credit := delta_epv * scale]

cat(sprintf("disposal rows: %s over %s matches\n",
            format(nrow(d), big.mark=","), format(uniqueN(d$match_id), big.mark=",")))

# --- (0) are the offsets doing anything? ------------------------------------
cat("\n=== (0) the pos_team branch in the disp formula ===\n")
tb <- d[, .(rows = .N, mean_delta_epv = round(mean(delta_epv, na.rm=TRUE), 4)), by = pos_team]
print(tb[order(pos_team)])
cat("EPV_DISP_NEG_OFFSET = 0.0000, EPV_DISP_POS_OFFSET = 0.0000 ->",
    "both fifelse branches are identical.\n")
cat("The branch exists but is inert: two untuned placeholders on a dead code path.\n")

# --- (1) zone, in attacking direction ---------------------------------------
# goal_x carries the attacking direction; normalise so +ve x = toward own
# attacking goal, then cut by distance from each end.
d[, att_x := x * sign(goal_x)]
d[, halfL := fifelse(is.na(venue_length), 165, venue_length) / 2]
d[, zone := fcase(
  att_x < -(halfL - 50), "1_DEF_50",
  att_x < 0,             "2_DEF_HALF",
  att_x < (halfL - 50),  "3_FWD_HALF",
  default =              "4_FWD_50")]

m <- merge(d, pg, by = c("player_id","match_id"))
gms <- unique(m[, .(player_id, match_id, position_group)])[, .N, by = position_group]
setnames(gms, "N", "player_games")

per_game <- function(dt, by_cols) {
  a <- dt[, .(credit = sum(credit, na.rm=TRUE), n = .N), by = by_cols]
  merge(a, gms, by = "position_group")[, `:=`(
    credit_pg = credit / player_games, n_pg = n / player_games)][]
}

cat("\n=== (1) disp credit per player-game, by position ===\n")
tot <- per_game(m, "position_group")[order(position_group)]
print(tot[, .(position_group, player_games, credit_pg = round(credit_pg,3),
              disposals_pg = round(n_pg,1))])

cat("\n=== (2) where does each bucket's disp credit come from? (share by zone) ===\n")
z <- per_game(m, c("position_group","zone"))
zs <- dcast(z, position_group ~ zone, value.var = "credit_pg", fill = 0)
num <- setdiff(names(zs), "position_group")
zs_sh <- copy(zs); tot_v <- rowSums(zs[, ..num])
for (cc in num) zs_sh[[cc]] <- round(zs[[cc]] / tot_v, 3)
cat("-- credit per game by zone --\n"); print(zs[, c("position_group", num), with=FALSE][
  , (num) := lapply(.SD, round, 3), .SDcols = num][])
cat("-- share of the bucket's own disp credit --\n"); print(zs_sh)

cat("\n=== (3) DEFENDER CONCENTRATION: is any zone a KD-specific lever? ===\n")
conc <- dcast(z, zone ~ position_group, value.var = "credit_pg", fill = 0)
conc[, `:=`(KD_over_KF = round(KEY_DEFENDER / KEY_FORWARD, 2),
            KD_credit_pg = round(KEY_DEFENDER, 3))]
print(conc[order(zone), .(zone, KD_credit_pg, KF = round(KEY_FORWARD,3), KD_over_KF)])
cat("\nFor scale: intercept marks (the §6.11 lever) run 7x KD vs KF.\n")

cat("\n=== (4) same question by disposal description ===\n")
m[, desc2 := fcase(
  grepl("Kickin", description, ignore.case=TRUE), "KICK_IN",
  grepl("^Handball", description),                "HANDBALL",
  grepl("^Kick", description),                    "KICK",
  default = "OTHER")]
dz <- per_game(m, c("position_group","desc2"))
cd <- dcast(dz, desc2 ~ position_group, value.var = "credit_pg", fill = 0)
cd[, `:=`(KD = round(KEY_DEFENDER,3), KF = round(KEY_FORWARD,3),
          KD_over_KF = round(KEY_DEFENDER / KEY_FORWARD, 2))]
print(cd[order(-KD), .(desc2, KD, KF, KD_over_KF)])

cat("\n=== (5) kick-ins are NOT separately identifiable in this column ===\n")
# Checked rather than assumed: the only kick-bearing descriptions in PBP are
# "Kick", "Ground Kick" and "Out On Full After Kick". There is no kick-in
# marker here, so the kick-in question C1 raised cannot be answered from
# `description` alone -- it needs the chains `initial_state`/set-play fields.
# Recorded as an open limitation, not silently dropped.
print(pb[grepl("kick", description, ignore.case = TRUE), .N, by = description][order(-N)])

cat("\n=== (6) is the inert pos_team branch a DEFENDER-CONCENTRATED lever? ===\n")
# EPV_DISP_NEG_OFFSET would apply only to pos_team == -1 rows (mean delta_epv
# -1.25). §6.6's lesson: a channel everyone shares is not a lever -- scaling it
# wrecks the buckets you were not aiming at. So measure concentration first.
a <- m[, .(credit = sum(credit), rows = .N), by = .(position_group, pos_team)]
a <- merge(a, gms, by = "position_group")[, `:=`(
  credit_pg = credit / player_games, rows_pg = rows / player_games)]
w <- dcast(a, position_group ~ pos_team, value.var = c("credit_pg","rows_pg"), fill = 0)
setnames(w, c("credit_pg_-1","credit_pg_1","rows_pg_-1","rows_pg_1"),
         c("NEG_credit_pg","POS_credit_pg","NEG_rows_pg","POS_rows_pg"))
w[, NEG_share_of_rows := round(NEG_rows_pg / (NEG_rows_pg + POS_rows_pg), 3)]
print(w[order(position_group), .(position_group,
        NEG_credit_pg = round(NEG_credit_pg,3), POS_credit_pg = round(POS_credit_pg,3),
        NEG_rows_pg = round(NEG_rows_pg,2), NEG_share_of_rows)])
kd <- w[position_group=="KEY_DEFENDER"]; kf <- w[position_group=="KEY_FORWARD"]
cat(sprintf("\nGLOBAL NEG-branch concentration KD/KF: credit %.2fx, rows %.2fx",
            kd$NEG_credit_pg/kf$NEG_credit_pg, kd$NEG_rows_pg/kf$NEG_rows_pg))
cat("  -> NOT a lever: every bucket takes ~17%% of its disposals on this branch.\n")

cat("\n=== (7) the same branch restricted to D50 -- this IS the lever ===\n")
b <- m[zone == "1_DEF_50", .(credit = sum(credit)), by = .(position_group, pos_team)]
b <- merge(b, gms, by = "position_group")[, credit_pg := credit / player_games]
wb <- dcast(b, position_group ~ pos_team, value.var = "credit_pg", fill = 0)
setnames(wb, c("-1","1"), c("D50_NEG","D50_POS"))
print(wb[order(position_group), .(position_group, D50_NEG = round(D50_NEG,3),
                                  D50_POS = round(D50_POS,3),
                                  D50_net = round(D50_NEG + D50_POS, 3))])
kd2 <- wb[position_group=="KEY_DEFENDER"]; kf2 <- wb[position_group=="KEY_FORWARD"]
cat(sprintf("\nD50 NEG-branch concentration KD/KF: %.2fx", kd2$D50_NEG/kf2$D50_NEG))
cat("  (intercept marks, the §6.11 lever, run 7x)\n")
