# The difficulty split, wired into the real player-game build.
#
# WHY THE STANDALONE TEST COULD NOT ANSWER THIS. The previous run scored the
# disposal credit on its own and reported conversion -0.400 (t -3.9) on the
# disposal channel, which I read as the design failing. It was the harness AND a
# real defect, and only one of them was mine:
#
#   the defect   the surprise was measured against the FITTED value of the
#                branch that occurred, leaving `V_after - V_branch` unpaid.
#                That gap is 64.2% of gross |delta_epv|, sd 0.790 -- the largest
#                of the three terms, credited to nobody. The comment claiming it
#                flows to "the next row" was wrong: `V_after` IS the next row's
#                starting state, so the next row's own decomposition begins from
#                it and never collects the gap. Fixed -- the surprise is now
#                measured against `V_after`, and the row closes exactly.
#
#   the harness  it compared difficulty credit against WHOLE-channel v3 EPV, so
#                the covered disposals were double-counted on one side and the
#                uncovered ones missing from the other.
#
# So this rebuilds player-game data properly with the flag on: the covered
# disposals leave the flat 50/50 split (both sides) and are paid from the
# difficulty channel instead, exactly as the aerial contests already are.
#
# What it has to show, and none of it is assumed:
#   conserves    the team total still tracks the margin, and the per-channel
#                conversions are not wildly unbalanced
#   separates    the split varies with difficulty, which is the entire point
#   repeatable   year-over-year persistence per channel vs the ship build. A
#                rule that charges the right players should not be noisier.
#   who moves    and whether it is who we predicted
#
# ~35 min: 1 player-game build with three GAMs fitted leak-safe per season,
# against a cached baseline. Run detached.

suppressMessages({
  library(dplyr); library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_difficulty_wired.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 45) for (l in capture.output(print(utils::head(x, n)))) say(l)
set_const <- function(l) for (nm in names(l)) assignInNamespace(nm, l[[nm]], ns = "torp")

say("=== Difficulty split, wired into create_player_game_data() ===")
say("run at ", format(Sys.time()))

BASE_F <- file.path(OUT_DIR, "epv3_fin_pgd_ship.parquet")
if (!file.exists(BASE_F)) stop("missing baseline: ", BASE_F)

pbp <- load_pbp(TRUE); stats_ <- load_player_stats(TRUE)
teams <- load_teams(TRUE); chains <- load_chains(TRUE)
res <- as.data.table(load_results(TRUE))
tgt <- res[, .(match_id = as.character(match_id), home = home_team_name,
               away = away_team_name, margin = home_score - away_score)][is.finite(margin)]

FIN <- readRDS(file.path(OUT_DIR, "epv3_finalise_ship.rds"))
PG  <- FIN$prior_games
STRUCT <- list(EPV3_CHANNELS = 3L, EPV3_SUB_SCALE = c(cont_aerial = 1, cont_stop = 1),
               EPV_STANDARDISE_CHANNELS = c("recv", "disp"),
               EPV3_STOP_ZERO_SUM = TRUE,
               EPV3_POINTS_SCALE = c(recv = 1, disp = 1, cont_aerial = 1, cont_stop = 1),
               EPR_PRIOR_GAMES_RECV = PG[channel == "recv", prior_games],
               EPR_PRIOR_GAMES_DISP = PG[channel == "disp", prior_games],
               EPR_PRIOR_GAMES_SPOIL = PG[channel == "spoil", prior_games])
set_const(STRUCT)

base <- as.data.table(read_parquet(BASE_F))
setattr(base, "epv_engine", "v3")

NEW_F <- file.path(OUT_DIR, "epv3_difficulty_wired_pgd.parquet")
if (file.exists(NEW_F)) {
  say("reusing cached difficulty-split build")
  new <- as.data.table(read_parquet(NEW_F))
} else {
  set_const(list(EPV_DIFFICULTY_SPLIT = TRUE))
  new <- as.data.table(create_player_game_data(pbp, stats_, teams, chains,
                                               epv_engine = "v3"))
  write_parquet(new, NEW_F)
  set_const(list(EPV_DIFFICULTY_SPLIT = FALSE))
}
setattr(new, "epv_engine", "v3")
say("baseline player-games ", format(nrow(base), big.mark = ","),
    " | difficulty build ", format(nrow(new), big.mark = ","))

# ---------------------------------------------------------------- conservation
# Raw `epv` only. `epv_adj` is opponent-adjusted and position-centred, which is
# measured to drop conservation from 0.99 to 0.60 on the SHIP build too -- so
# using it here would report a property of the adjustment, not of the split.
conserve <- function(d, label) {
  ts <- d[, .(v = sum(epv, na.rm = TRUE), r = sum(epv_recv, na.rm = TRUE),
              p = sum(epv_disp, na.rm = TRUE), s = sum(epv_spoil, na.rm = TRUE)),
          by = .(match_id, team)]
  h <- merge(tgt, ts, by.x = c("match_id", "home"), by.y = c("match_id", "team"))
  a <- merge(tgt, ts, by.x = c("match_id", "away"), by.y = c("match_id", "team"))
  m <- merge(h[, .(match_id, margin, vh = v, rh = r, ph = p, sh = s)],
             a[, .(match_id, va = v, ra = r, pa = p, sa = s)], by = "match_id")
  m[, `:=`(dv = vh - va, dr = rh - ra, dp = ph - pa, ds = sh - sa)]
  ft <- summary(stats::lm(margin ~ 0 + dv, data = m))
  say(sprintf("  %-10s TOTAL -> margin %.4f (t %.1f, R2 %.3f)  sd %.1f vs margin %.1f",
              label, ft$coefficients[1, 1], ft$coefficients[1, 3], ft$r.squared,
              stats::sd(m$dv), stats::sd(m$margin)))
  f2 <- summary(stats::lm(margin ~ 0 + dr + dp + ds, data = m))
  cf <- f2$coefficients
  contrib <- c(stats::sd(m$dr), stats::sd(m$dp), stats::sd(m$ds)) * cf[, 1]
  say_dt(data.table(channel = c("recv", "disp", "contest"),
                    conversion = round(cf[, 1], 3), t = round(cf[, 3], 1),
                    sd = round(c(stats::sd(m$dr), stats::sd(m$dp), stats::sd(m$ds)), 2),
                    share_pct = round(100 * contrib^2 / sum(contrib^2), 1)), 3)
  invisible(m)
}
say(""); say("=== 1. DOES IT CONSERVE? ===")
say("A conversion of 1.000 means one credited point equals one point of margin.")
conserve(base, "ship")
conserve(new,  "difficulty")

# ----------------------------------------------------------------- separation
say(""); say("=== 2. DOES THE SPLIT STILL VARY WITH DIFFICULTY? ===")
sc_f <- file.path(OUT_DIR, "epv3_difficulty_scored.parquet")
if (file.exists(sc_f)) {
  sc <- as.data.table(read_parquet(sc_f))
  say("NOTE: read from the cached pre-fix scoring, so `surprise` there is the")
  say("branch-only version. p_hat and the gradient shape are unaffected; the")
  say("credit columns are not comparable to the build above and are omitted.")
  sc[, len_band := cut(kick_len, c(-Inf, 15, 25, 35, 45, 55, Inf),
                       labels = c("<15m", "15-25m", "25-35m", "35-45m", "45-55m", ">55m"))]
  say_dt(sc[description == "Kick" & !is.na(len_band),
            .(n = .N, p_turnover = round(mean(p_hat), 3)), by = len_band][order(len_band)], 8)
} else say("  (no cached scoring table)")

say(""); say("effect on the two channels, per player-game:")
say_dt(data.table(
  build = c("ship", "difficulty"),
  recv_sd = round(c(stats::sd(base$epv_recv, na.rm = TRUE), stats::sd(new$epv_recv, na.rm = TRUE)), 3),
  disp_sd = round(c(stats::sd(base$epv_disp, na.rm = TRUE), stats::sd(new$epv_disp, na.rm = TRUE)), 3),
  recv_mean = round(c(mean(base$epv_recv, na.rm = TRUE), mean(new$epv_recv, na.rm = TRUE)), 3),
  disp_mean = round(c(mean(base$epv_disp, na.rm = TRUE), mean(new$epv_disp, na.rm = TRUE)), 3)), 2)

# ---------------------------------------------------------------- repeatability
# Year over year, not game to game. Game-to-game reliability rises with anything
# that tracks event counts; year-over-year is what separates ability from noise.
say(""); say("=== 3. IS IT MORE OR LESS REPEATABLE? ===")
yoy <- function(d, col) {
  x <- d[is.finite(get(col))]
  s <- x[, .(v = sum(get(col)), g = .N), by = .(player_id, season)][g >= 8]
  s[, rate := v / g]
  b <- copy(s)[, season := season - 1]; setnames(b, "rate", "rate_next")
  m <- merge(s[, .(player_id, season, rate)], b[, .(player_id, season, rate_next)],
             by = c("player_id", "season"))
  m <- m[is.finite(rate) & is.finite(rate_next)]
  c(n = nrow(m), r = round(stats::cor(m$rate, m$rate_next), 4))
}
rows <- rbindlist(lapply(c("epv_recv", "epv_disp", "epv"), function(cc) {
  a <- yoy(base, cc); b <- yoy(new, cc)
  data.table(channel = cc, ship = a[["r"]], difficulty = b[["r"]],
             delta = round(b[["r"]] - a[["r"]], 4), n = b[["n"]])
}))
say_dt(rows, 5)
say("")
say("Higher is better ONLY if it is not bought by tracking event counts --")
say("a channel that becomes a disposal count is more repeatable and less useful:")
cnt <- rbindlist(lapply(list(list("ship", base), list("difficulty", new)), function(z) {
  d <- z[[2]]
  data.table(build = z[[1]],
             cor_disposals = round(stats::cor(d$epv_disp, d$disposals, use = "complete.obs"), 3),
             cor_marks = round(stats::cor(d$epv_recv, d$marks, use = "complete.obs"), 3),
             cor_cposs = round(stats::cor(d$epv_recv, d$contested_possessions, use = "complete.obs"), 3))
}))
say_dt(cnt, 3)

# ------------------------------------------------------------------ who moves
say(""); say("=== 4. WHO MOVES? ===")
cmp <- merge(base[, .(match_id, player_id, player_name, position_group,
                      b_recv = epv_recv, b_disp = epv_disp, b_epv = epv)],
             new[, .(match_id, player_id, n_recv = epv_recv, n_disp = epv_disp, n_epv = epv)],
             by = c("match_id", "player_id"))
say("mean change per player-game, by position:")
say_dt(cmp[!is.na(position_group), .(n = .N,
           d_recv = round(mean(n_recv - b_recv), 3),
           d_disp = round(mean(n_disp - b_disp), 3),
           d_epv  = round(mean(n_epv - b_epv), 3)),
           by = position_group][order(-d_epv)], 8)

cur <- new[season == max(season, na.rm = TRUE)]
agg <- cur[, .(g = .N, epv = round(sum(epv, na.rm = TRUE), 1),
               recv = round(sum(epv_recv, na.rm = TRUE), 1),
               disp = round(sum(epv_disp, na.rm = TRUE), 1)),
           by = .(player_name, position_group)][g >= 8]
setorder(agg, -epv)
say(""); say("top 15 by raw EPV under the difficulty split (current season):")
say_dt(agg[1:15], 15)

say(""); say("done ", format(Sys.time()))
close(con); cat("\nDone\n")
