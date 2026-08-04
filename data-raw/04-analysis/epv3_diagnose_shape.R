# Diagnose the SHAPE of v3 against Pete's three quality criteria, before
# changing anything:
#
#   (a) variance balance across the three channels
#   (b) one unit = one point                     (already fitted, re-checked here)
#   (c) every position represented in the top 40
#
# Everything reads cached parquet -- no rebuild. The point is to find WHERE the
# imbalance comes from, not to re-establish that it exists.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_diagnose_shape.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 50) for (l in capture.output(print(utils::head(x, n)))) say(l)

d <- as.data.table(read_parquet(file.path(OUT_DIR, "epv3_pgd_3ch_raw.parquet")))
say("=== v3, 3-channel player-game frame: ", format(nrow(d), big.mark = ","), " rows ===")
say("columns present: ", paste(intersect(c("epv_cont_aerial", "epv_cont_stop",
    "epv_disp_aerial", "contests_won", "contests_lost"), names(d)), collapse = ", "))

# ---------------------------------------------------------------------------
# 1. Where does epv_disp come from? The aerial path pays the kicker the WHOLE
#    of (V_pre - exp_pts); the non-aerial path pays him EPV_DISP_SCALE (0.5) of
#    the swing. If the aerial part is a large share of disp, that asymmetry is
#    the mechanical reason disp dominates.
# ---------------------------------------------------------------------------
say("")
say("=== 1. composition of epv_disp (raw, pre-adjustment) ===")
if ("epv_disp_aerial" %in% names(d)) {
  d[, .disp_nonaerial := epv_disp - fifelse(is.na(epv_disp_aerial), 0, epv_disp_aerial)]
  say_dt(data.table(
    part = c("aerial (V_pre - exp_pts, UNSHARED)", "non-aerial (0.5 x delta_epv)", "total"),
    sd = round(c(sd(d$epv_disp_aerial, na.rm = TRUE), sd(d$.disp_nonaerial, na.rm = TRUE),
                 sd(d$epv_disp, na.rm = TRUE)), 4),
    mean_abs = round(c(mean(abs(d$epv_disp_aerial), na.rm = TRUE),
                       mean(abs(d$.disp_nonaerial), na.rm = TRUE),
                       mean(abs(d$epv_disp), na.rm = TRUE)), 4)), 5)
  say("share of disp VARIANCE carried by the aerial part: ",
      round(100 * var(d$epv_disp_aerial, na.rm = TRUE) / var(d$epv_disp, na.rm = TRUE), 1), "%")
} else {
  say("epv_disp_aerial not retained in this frame -- rebuild needed to split it")
}

# ---------------------------------------------------------------------------
# 2. The contest channel, split into its parts.
# ---------------------------------------------------------------------------
say("")
say("=== 2. composition of the merged contest channel ===")
cc <- intersect(c("epv_cont_aerial", "epv_cont_stop"), names(d))
if (length(cc) == 2) {
  say_dt(data.table(
    part = c("cont_aerial", "cont_stop", "merged (epv_spoil)"),
    sd = round(c(sd(d$epv_cont_aerial, na.rm = TRUE), sd(d$epv_cont_stop, na.rm = TRUE),
                 sd(d$epv_spoil, na.rm = TRUE)), 4),
    mean = round(c(mean(d$epv_cont_aerial, na.rm = TRUE), mean(d$epv_cont_stop, na.rm = TRUE),
                   mean(d$epv_spoil, na.rm = TRUE)), 4)), 5)
  say("cor(cont_aerial, cont_stop) = ",
      round(cor(d$epv_cont_aerial, d$epv_cont_stop, use = "complete.obs"), 4))
}

# ---------------------------------------------------------------------------
# 3. Position profile of every channel, ADJUSTED columns (what EPR consumes).
#    This is where "no key defender in the top 40" would originate.
# ---------------------------------------------------------------------------
say("")
say("=== 3. per-position level and spread, adjusted channels ===")
ADJ <- c("epv_recv_adj", "epv_disp_adj", "epv_spoil_adj")
pp <- d[!is.na(position_group), c(.(n = .N),
        lapply(.SD, function(v) round(mean(v, na.rm = TRUE), 4))),
        .SDcols = ADJ, by = position_group]
setorder(pp, -n)
say("MEANS (level):")
say_dt(pp, 10)
ps <- d[!is.na(position_group), c(.(n = .N),
        lapply(.SD, function(v) round(sd(v, na.rm = TRUE), 4))),
        .SDcols = ADJ, by = position_group]
setorder(ps, -n)
say("")
say("SDs (spread):")
say_dt(ps, 10)

# Total per game, by position.
d[, .epv_tot := epv_recv_adj + epv_disp_adj + epv_spoil_adj]
pt <- d[!is.na(position_group), .(n = .N,
        mean_per_game = round(mean(.epv_tot, na.rm = TRUE), 4),
        sd_per_game = round(sd(.epv_tot, na.rm = TRUE), 4)), by = position_group]
setorder(pt, -sd_per_game)
say("")
say("TOTAL epv_adj by position (this is what the leaderboard sorts on):")
say_dt(pt, 10)

# ---------------------------------------------------------------------------
# 4. The top 40 of 2026 by season EPV, and which positions are missing.
#    Uses the fitted points constants so the ranking is the delivered metric.
# ---------------------------------------------------------------------------
say("")
say("=== 4. 2026 top 40 on the delivered (points-calibrated) metric ===")
k <- tryCatch(readRDS(file.path(OUT_DIR, "epv3_spec_scale.rds"))$k,
              error = function(e) c(recv = 1, disp = 1, cont = 1))
say("using k = ", paste(names(k), round(k, 4), sep = "=", collapse = "  "))
# The leaderboard the human sees is built AFTER the positional level centring,
# so ranking the uncentred frame would blame v3 for a level the pipeline
# already removes. Centre first, then rank.
setattr(d, "epv_engine", "v3")
dc <- as.data.table(centre_epv_by_position(d))
say("centred: per-position residual level after centring, total per game:")
dc[, .tot_c := epv_recv_adj + epv_disp_adj + epv_spoil_adj]
say_dt(dc[!is.na(position_group), .(n = .N, mean_total = round(mean(.tot_c, na.rm = TRUE), 4)),
          by = position_group][order(-n)], 10)
dc[, `:=`(epv_recv_adj = epv_recv_adj * k[[1]],
          epv_disp_adj = epv_disp_adj * k[[2]],
          epv_spoil_adj = epv_spoil_adj * k[[3]])]
dc[, epv_pts := epv_recv_adj + epv_disp_adj + epv_spoil_adj]
agg <- dc[season == max(season), .(gms = .N, epv = round(sum(epv_pts), 1),
                                   recv = round(sum(epv_recv_adj), 1),
                                   disp = round(sum(epv_disp_adj), 1),
                                   cont = round(sum(epv_spoil_adj), 1)),
          by = .(player_name, position_group)][gms >= 8]
setorder(agg, -epv)
say_dt(agg[1:40], 40)

say("")
say("--- position counts in the top 40 ---")
cnt <- agg[1:40, .N, by = position_group]
allp <- d[!is.na(position_group), .(pool = uniqueN(player_id)), by = position_group]
cnt <- merge(allp, cnt, by = "position_group", all.x = TRUE)
cnt[is.na(N), N := 0L]
setorder(cnt, -N)
say_dt(cnt, 10)
say("")
say("positions with ZERO players in the top 40: ",
    paste(cnt[N == 0, position_group], collapse = ", "))

# Best player of each position group, and where he ranks.
agg[, rank := .I]
best <- agg[, .SD[which.min(rank)], by = position_group][, .(position_group, player_name, rank, epv)]
setorder(best, rank)
say("")
say("--- best-ranked player of each position group ---")
say_dt(best, 10)

close(con)
cat("\nWrote ", OUT, "\n")
