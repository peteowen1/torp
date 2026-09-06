# EPV v3 step 3: draft run of both engines, side by side.
#
# Produces the per-player-game frame under v2 and v3 from IDENTICAL inputs, then
# runs the sense checks in EPV-V3-CHAIN-NATIVE.md section 6 gates 3 and 4.
#
# Identical-arms guard: the two frames MUST differ numerically. Every gate in
# this repo checks "is it better" and none check "is it the same thing" -- that
# has cost half a headline gain before.

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_draft_run.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 60) for (l in capture.output(print(utils::head(x, n)))) say(l)

say("=== EPV v3 draft run: v2 vs v3 ===")
say("run at ", format(Sys.time()))

# ---- Inputs, loaded once and shared by both arms ---------------------------
pbp    <- load_pbp(TRUE)
stats_ <- load_player_stats(TRUE)
teams  <- load_teams(TRUE)
chains <- load_chains(TRUE)
say("pbp ", format(nrow(pbp), big.mark = ","),
    " | player_stats ", format(nrow(stats_), big.mark = ","),
    " | teams ", format(nrow(teams), big.mark = ","),
    " | chains ", format(nrow(chains), big.mark = ","))
say("pbp seasons: ", paste(range(lubridate::year(pbp$utc_start_time)), collapse = "-"))

t0 <- Sys.time()
v2 <- create_player_game_data(pbp, stats_, teams, chains, epv_engine = "v2")
say("v2 built in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min, ",
    format(nrow(v2), big.mark = ","), " player-games")
t0 <- Sys.time()
v3 <- create_player_game_data(pbp, stats_, teams, chains, epv_engine = "v3")
say("v3 built in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min, ",
    format(nrow(v3), big.mark = ","), " player-games")

v2 <- as.data.table(v2); v3 <- as.data.table(v3)

# ---- Identical-arms guard --------------------------------------------------
say("")
say("--- ARMS GUARD ---")
key <- c("player_id", "match_id")
cmp <- merge(v2[, c(key, "epv", "epv_recv", "epv_disp", "epv_spoil", "epv_hitout"), with = FALSE],
             v3[, c(key, "epv", "epv_recv", "epv_disp", "epv_spoil", "epv_hitout"), with = FALSE],
             by = key, suffixes = c("_v2", "_v3"))
say("rows in both arms: ", format(nrow(cmp), big.mark = ","))
for (ch in c("epv", "epv_recv", "epv_disp", "epv_spoil", "epv_hitout")) {
  d <- cmp[[paste0(ch, "_v3")]] - cmp[[paste0(ch, "_v2")]]
  say("  ", ch, ": mean|diff| ", round(mean(abs(d)), 4),
      " | identical rows ", round(100 * mean(abs(d) < 1e-9), 1), "%")
}
if (mean(abs(cmp$epv_v3 - cmp$epv_v2)) < 1e-6) {
  stop("v2 and v3 produced the same epv -- the engine switch is not live.")
}

# ---- Gate 3: channel variance shares ---------------------------------------
say("")
say("--- GATE 3: channel variance shares (per player-game, adjusted channels) ---")
say("v2 baseline on published ratings was epr_disp 67.3 / recv 30.8 / spoil 1.4 / hitout 0.5")
vshare <- function(d, label) {
  cols <- c("epv_recv_adj", "epv_disp_adj", "epv_spoil_adj", "epv_hitout_adj")
  s <- vapply(cols, function(c) stats::var(d[[c]], na.rm = TRUE), numeric(1))
  sd_ <- sqrt(s)
  data.table(arm = label, channel = c("recv", "disp", "cont_aerial", "cont_stop"),
             sd = round(sd_, 4), var_share_pct = round(100 * s / sum(s), 1))
}
say_dt(rbind(vshare(v2, "v2"), vshare(v3, "v3")), 12)
say("total epv_adj sd: v2 ", round(sd(v2$epv_adj, na.rm = TRUE), 4),
    " | v3 ", round(sd(v3$epv_adj, na.rm = TRUE), 4))

# ---- Contest channel magnitude ---------------------------------------------
say("")
say("--- contest channel, raw (unadjusted) per player-game ---")
say_dt(rbind(
  v2[, .(arm = "v2", mean = round(mean(epv_spoil), 4), sd = round(sd(epv_spoil), 4),
         p05 = round(quantile(epv_spoil, .05), 3), p95 = round(quantile(epv_spoil, .95), 3))],
  v3[, .(arm = "v3", mean = round(mean(epv_spoil), 4), sd = round(sd(epv_spoil), 4),
         p05 = round(quantile(epv_spoil, .05), 3), p95 = round(quantile(epv_spoil, .95), 3))]
), 5)
if ("contests_won" %in% names(v3)) {
  say("v3 contests won per player-game: mean ", round(mean(v3$contests_won), 2),
      " | max ", max(v3$contests_won))
  say("v3 player-games with NEGATIVE cont_aerial: ",
      round(100 * mean(v3$epv_cont_aerial < 0), 1),
      "%   [v2's flat spoil weight can only ever be >= 0]")

  # The loser-allocation defect guard. The debit is spread by AERIAL exposure,
  # so it must track marks/spoils and NOT total possessions. The first draft
  # weighted by all chain rows and taxed ball-winning as contest-losing.
  say("")
  say("--- loser-allocation sanity: what does the contest debit track? ---")
  say("cor(cont_aerial, disposals)      ", round(cor(v3$epv_cont_aerial, v3$disposals), 3),
      "   [want ~0: possessions must not drive it]")
  say("cor(cont_aerial, marks)          ", round(cor(v3$epv_cont_aerial, v3$marks), 3),
      "   [want ~0: an uncontested mark is not a contest]")
  say("cor(cont_aerial, uncont_poss)    ", round(cor(v3$epv_cont_aerial, v3$uncontested_possessions), 3))
  say("cor(cont_aerial, contested_marks)", round(cor(v3$epv_cont_aerial, v3$contested_marks), 3))
  say("cor(cont_aerial, spoils)         ", round(cor(v3$epv_cont_aerial, v3$spoils), 3))
  say("high-possession / low-aerial players (disposals >= 25, marks <= 3):")
  hp <- v3[disposals >= 25 & marks <= 3]
  say("  n = ", nrow(hp), " | mean cont_aerial ", round(mean(hp$epv_cont_aerial), 3),
      " | vs all-player mean ", round(mean(v3$epv_cont_aerial), 3))
}

# ---- Gate 4: by listed position -------------------------------------------
say("")
say("--- GATE 4a: epv_adj by listed position group (TOG-weighted mean and sd) ---")
bypos <- function(d, label) {
  d <- d[!is.na(position_group)]
  d[, .(arm = label, n = .N,
        mean_adj = round(weighted.mean(epv_adj, pmax(time_on_ground_percentage, 10), na.rm = TRUE), 3),
        sd_adj = round(sd(epv_adj, na.rm = TRUE), 3)),
    by = position_group][order(position_group)]
}
say_dt(merge(bypos(v2, "v2"), bypos(v3, "v3"), by = "position_group",
             suffixes = c("_v2", "_v3"))[, .(position_group, n_v2,
               mean_v2 = mean_adj_v2, mean_v3 = mean_adj_v3,
               sd_v2 = sd_adj_v2, sd_v3 = sd_adj_v3,
               sd_ratio = round(sd_adj_v3 / sd_adj_v2, 3))], 12)

say("")
say("--- GATE 4b: the populations that MUST move ---")
say("(b1) high-tackle players should move DOWN: v3 drops tackles entirely")
tk <- merge(v2[, .(player_id, match_id, tackles, epv_adj_v2 = epv_adj)],
            v3[, .(player_id, match_id, epv_adj_v3 = epv_adj)], by = c("player_id", "match_id"))
tk[, tk_q := cut(tackles, breaks = quantile(tackles, 0:5 / 5, na.rm = TRUE),
                 include.lowest = TRUE, labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))]
say_dt(tk[!is.na(tk_q), .(n = .N, mean_tackles = round(mean(tackles), 2),
                          v2 = round(mean(epv_adj_v2), 3), v3 = round(mean(epv_adj_v3), 3),
                          delta = round(mean(epv_adj_v3 - epv_adj_v2), 3)), by = tk_q][order(tk_q)], 6)

say("")
say("(b2) key defenders were under-dispersed ~1.65x in v2")
kd <- rbind(
  v2[grepl("KEY_DEF", position_group), .(arm = "v2", n = .N, sd = round(sd(epv_adj), 3))],
  v3[grepl("KEY_DEF", position_group), .(arm = "v3", n = .N, sd = round(sd(epv_adj), 3))]
)
say_dt(kd, 4)
say("pooled sd all positions: v2 ", round(sd(v2$epv_adj, na.rm = TRUE), 3),
    " | v3 ", round(sd(v3$epv_adj, na.rm = TRUE), 3))

# ---- Player-level sense check ----------------------------------------------
say("")
say("--- GATE 4c: 2026 season totals, top 25 by v3, with v2 alongside ---")
agg <- function(d, label) d[season == max(season)][
  , .(gms = .N, epv = round(sum(epv_adj), 1),
      recv = round(sum(epv_recv_adj), 1), disp = round(sum(epv_disp_adj), 1),
      cont = round(sum(epv_spoil_adj), 1), stop = round(sum(epv_hitout_adj), 1)),
  by = .(player_name, position_group)]
a2 <- agg(v2, "v2"); a3 <- agg(v3, "v3")
m <- merge(a2, a3, by = c("player_name", "position_group"), suffixes = c("_v2", "_v3"))
setorder(m, -epv_v3)
say_dt(m[1:25, .(player_name, position_group, gms = gms_v3,
                 v2 = epv_v2, v3 = epv_v3,
                 recv = recv_v3, disp = disp_v3, cont = cont_v3, stop = stop_v3)], 25)

say("")
say("--- biggest RISERS and FALLERS, 2026, min 8 games ---")
m[, delta := epv_v3 - epv_v2]
setorder(m, -delta)
say("RISERS:"); say_dt(m[gms_v3 >= 8][1:15, .(player_name, position_group, v2 = epv_v2, v3 = epv_v3, delta)], 15)
setorder(m, delta)
say("FALLERS:"); say_dt(m[gms_v3 >= 8][1:15, .(player_name, position_group, v2 = epv_v2, v3 = epv_v3, delta)], 15)

arrow::write_parquet(v3, file.path(OUT_DIR, "epv3_player_game_v3.parquet"))
arrow::write_parquet(v2, file.path(OUT_DIR, "epv3_player_game_v2.parquet"))
say("")
say("wrote both arms to ", OUT_DIR)
close(con)
cat("\nWrote ", OUT, "\n")
