# Side-by-side leaderboard: v2 against v3, with positions and sub-category splits.
#
# Answers "who does each engine think the best players are, and what are they
# being paid for" -- which no summary statistic can.
#
# READ THE CHANNEL NAMES CAREFULLY. They are the SAME COLUMNS in both engines and
# they mean DIFFERENT THINGS, which is the single easiest way to misread this
# table:
#
#   column       v2 means                          v3 means
#   epr_recv     receiving + contested poss +      chain reception only
#                marks + frees (box-weighted)
#   epr_disp     disposal + goals/behinds/shots/   chain disposal only
#                metres/clangers (box-weighted)
#   epr_spoil    spoils + tackles + pressure       AERIAL CONTEST value
#   epr_hitout   ruck box terms                    stoppage value, or 0 under
#                                                  EPV3_CHANNELS = 3L
#
# So v2's "spoil" and v3's "contest" are not the same quantity at all, and v2's
# disp restates the scoreboard where v3's does not. Columns are relabelled in the
# output for exactly that reason.
#
# Writes a CSV alongside the printed tables so it can be sorted in a spreadsheet.
#
# ~1 min, reads cached ratings only.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_leaderboards.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 60) for (l in capture.output(print(utils::head(x, n)))) say(l)

pick <- function(...) { for (f in c(...)) if (file.exists(file.path(OUT_DIR, f))) return(f); NA_character_ }
F_V2 <- pick("epv3_cal_rt_v2prod.parquet")
F_V3 <- pick("epv3_rt_SHIPPING_ship.parquet", "epv3_fin_rt_ship_final.parquet")
say("=== v2 vs v3 leaderboards ===")
say("v2: ", F_V2, "   v3: ", F_V3)
stopifnot(!is.na(F_V2), !is.na(F_V3))

latest <- function(f) {
  d <- as.data.table(read_parquet(file.path(OUT_DIR, f)))
  d <- d[season == max(season)]
  d[round == max(round)]
}
v2 <- latest(F_V2); v3 <- latest(F_V3)
say("season ", max(v2$season), " round ", max(v2$round),
    " | v2 players ", nrow(v2), " | v3 players ", nrow(v3))

CH <- c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout")
rankcol <- if ("torp" %in% names(v2)) "torp" else "epr"
say("ranking on: ", rankcol)

prep <- function(d, eng) {
  keep <- unique(c("player_id", "player_name", "position_group", rankcol, "epr", "psr", CH))
  x <- d[, intersect(keep, names(d)), with = FALSE]
  for (c in CH) if (!c %in% names(x)) x[, (c) := NA_real_]
  # Listed-but-unplayed players carry NA ratings (PSR falls back to the -2
  # prior). setorderv() puts NA FIRST, so without this the "top 30" was thirty
  # debutants who have never played a game.
  x <- x[is.finite(get(rankcol))]
  setorderv(x, rankcol, -1, na.last = TRUE)
  x[, rank := .I]
  # unique(): rankcol IS "epr" whenever the TORP blend is absent, and setnames
  # rejects a duplicated `old`.
  ren <- unique(c(rankcol, "epr", "psr", CH, "rank"))
  setnames(x, ren, paste0(ren, "_", eng))
  x
}
a <- prep(v2, "v2"); b <- prep(v3, "v3")
m <- merge(a, b[, setdiff(names(b), c("player_name", "position_group")), with = FALSE],
           by = "player_id", all = TRUE)
m[, rank_move := rank_v2 - rank_v3]   # positive = v3 rates him higher

r2 <- paste0(rankcol, "_v2"); r3 <- paste0(rankcol, "_v3")
rnd <- function(x, k = 2) round(x, k)

# ---------------------------------------------------------------------------
say("")
say("=== TOP 30 ON v2 (production today) ===")
say("channels: recv | disp | spoil(=spoils/tackles/pressure) | hitout(=ruck box)")
t2 <- m[order(rank_v2)][1:30, .(rk = rank_v2, player_name, pos = position_group,
        torp = rnd(get(r2)), epr = rnd(epr_v2), psr = rnd(psr_v2),
        recv = rnd(epr_recv_v2), disp = rnd(epr_disp_v2),
        spoil = rnd(epr_spoil_v2), ruck = rnd(epr_hitout_v2),
        v3_rk = rank_v3, move = rank_move)]
say_dt(t2, 30)

say("")
say("=== TOP 30 ON v3 ===")
say("channels: recv | disp | CONTEST(aerial+stoppage) ; hitout slot is empty")
t3 <- m[order(rank_v3)][1:30, .(rk = rank_v3, player_name, pos = position_group,
        torp = rnd(get(r3)), epr = rnd(epr_v3), psr = rnd(psr_v3),
        recv = rnd(epr_recv_v3), disp = rnd(epr_disp_v3),
        contest = rnd(epr_spoil_v3),
        v2_rk = rank_v2, move = rank_move)]
say_dt(t3, 30)

# ---------------------------------------------------------------------------
say("")
say("=== POSITION MIX IN THE TOP 40 ===")
pool <- m[!is.na(position_group), .(pool = .N), by = position_group]
i2 <- m[order(rank_v2)][1:40][!is.na(position_group), .(v2 = .N), by = position_group]
i3 <- m[order(rank_v3)][1:40][!is.na(position_group), .(v3 = .N), by = position_group]
cmp <- Reduce(function(x, y) merge(x, y, by = "position_group", all = TRUE),
              list(pool, i2, i3))
for (c in c("v2", "v3")) cmp[is.na(get(c)), (c) := 0L]
cmp[, expected := round(40 * pool / sum(pool), 1)]
setorder(cmp, -v3)
say_dt(cmp, 10)

say("")
say("=== WHERE EACH POSITION EARNS ITS RATING (mean per channel, top 100) ===")
for (eng in c("v2", "v3")) {
  say("")
  say("--- ", eng, " ---")
  cols <- paste0(CH, "_", eng)
  top <- m[order(get(paste0("rank_", eng)))][1:100]
  z <- top[!is.na(position_group), c(list(n = .N),
           lapply(.SD, function(v) rnd(mean(v, na.rm = TRUE)))),
           .SDcols = cols, by = position_group]
  setnames(z, cols, if (eng == "v2") c("recv", "disp", "spoil", "ruck")
                    else c("recv", "disp", "contest", "(empty)"))
  setorder(z, -n); say_dt(z, 10)
}

# ---------------------------------------------------------------------------
say("")
say("=== BIGGEST DISAGREEMENTS (top 150 on either engine) ===")
cand <- m[rank_v2 <= 150 | rank_v3 <= 150]
say("")
say("--- v3 rates them MUCH higher ---")
say_dt(cand[order(-rank_move)][1:15, .(player_name, pos = position_group,
      v2_rk = rank_v2, v3_rk = rank_v3, move = rank_move,
      contest_v3 = rnd(epr_spoil_v3), spoil_v2 = rnd(epr_spoil_v2),
      recv_v3 = rnd(epr_recv_v3), recv_v2 = rnd(epr_recv_v2))], 15)
say("")
say("--- v2 rates them MUCH higher ---")
say_dt(cand[order(rank_move)][1:15, .(player_name, pos = position_group,
      v2_rk = rank_v2, v3_rk = rank_v3, move = rank_move,
      contest_v3 = rnd(epr_spoil_v3), spoil_v2 = rnd(epr_spoil_v2),
      disp_v3 = rnd(epr_disp_v3), disp_v2 = rnd(epr_disp_v2))], 15)

say("")
say("=== HOW MUCH DO THE TWO AGREE? ===")
ok <- m[is.finite(get(r2)) & is.finite(get(r3))]
say("players in both: ", nrow(ok))
say("Spearman rank correlation: ", rnd(cor(ok$rank_v2, ok$rank_v3, method = "spearman"), 3))
say("Pearson on ", rankcol, ":        ", rnd(cor(ok[[r2]], ok[[r3]]), 3))
ov <- length(intersect(m[order(rank_v2)][1:40, player_id], m[order(rank_v3)][1:40, player_id]))
say("overlap in the top 40:      ", ov, " of 40")

say("")
say("=== CHANNEL SPREAD (sd across all rated players) ===")
sd_tab <- rbindlist(lapply(c("v2", "v3"), function(eng) {
  cols <- paste0(CH, "_", eng)
  s <- vapply(cols, function(c) sd(m[[c]], na.rm = TRUE), numeric(1))
  data.table(engine = eng, channel = c("recv", "disp", "spoil/contest", "ruck"),
             sd = rnd(s, 3), share_pct = rnd(100 * s^2 / sum(s^2, na.rm = TRUE), 1))
}))
say_dt(sd_tab, 10)

csv <- file.path(OUT_DIR, "epv3_leaderboards.csv")
fwrite(m[order(rank_v3)], csv)
say("")
say("full table written to ", csv)
close(con)
cat("\nDone\n")
