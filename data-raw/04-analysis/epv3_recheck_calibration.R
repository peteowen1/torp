# Was the retraction of the EPV-layer calibration itself wrong?
#
# I retracted "v2's contest coefficient on margin is -0.4105" on the grounds that
# it came from a team aggregation ignoring lineups and TOG weighting. That
# reasoning was applied to the wrong script.
#
# There are TWO aggregations in play and only one of them had the defect:
#
#   epv3_optimiser_headroom.R  summed EPR RATINGS per team-round. Ratings exist
#     for every listed player, not just the 22 selected, so it needed
#     .build_team_ratings_df(). DEFECTIVE -- correctly retracted.
#
#   epv3_calibrate_points.R    sums epv_*_adj per (match_id, team) from
#     player_game_data. That frame has one row per player who ACTUALLY PLAYED
#     that match, so lineup filtering is automatic. And epv_*_adj is already
#     TOG-scaled by .position_adjust(), so the POSITION_AVG_TOG weighting would
#     be double-counting, not a fix. NOT DEFECTIVE.
#
# They also measure DIFFERENT QUANTITIES, which is why they can disagree without
# either being wrong:
#   EPV layer: did this team accumulate contest value IN THIS MATCH -> margin
#              (contemporaneous)
#   EPR layer: is this team's HISTORICAL contest rating higher -> margin
#              (predictive)
#
# This checks the lineup claim empirically rather than by reading code, then
# reports both quantities side by side.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_recheck_calibration.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

CH <- c("recv", "disp", "spoil", "hitout")
LBL <- c(recv = "recv", disp = "disp", spoil = "cont_aerial", hitout = "cont_stop")

v2 <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_player_game_v2.parquet")))
v3 <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_player_game_v3.parquet")))
res <- as.data.table(load_results(TRUE))

say("=== Was the EPV-layer calibration actually defective? ===")

# --- The empirical test of the lineup claim --------------------------------
n_per <- v2[, .N, by = .(match_id, team)]
say("")
say("--- players per (match, team) in player_game_data ---")
say("mean ", round(mean(n_per$N), 2), " | median ", median(n_per$N),
    " | p05 ", quantile(n_per$N, .05), " | p95 ", quantile(n_per$N, .95))
say("")
if (mean(n_per$N) > 18 && mean(n_per$N) < 30) {
  say("VERDICT: ~", round(mean(n_per$N), 1), " players per team-match. This frame")
  say("already contains only players who PLAYED, so the lineup defect that")
  say("invalidated the EPR-layer screen does NOT apply here.")
  say("The retraction of the EPV-layer number was WRONG.")
} else {
  say("VERDICT: ", round(mean(n_per$N), 1), " per team-match -- the retraction stands.")
}
say("")
say("TOG: epv_*_adj is already TOG-scaled by .position_adjust() (it multiplies")
say("by tog_safe), so applying POSITION_AVG_TOG on top would DOUBLE-COUNT.")
say("Its absence here is correct, not an omission.")

# --- Refit both layers, side by side ----------------------------------------
team_sums <- function(d) {
  cols <- paste0("epv_", CH, "_adj")
  s <- d[, lapply(.SD, sum, na.rm = TRUE), .SDcols = cols, by = .(match_id, team)]
  setnames(s, cols, paste0("t_", LBL[CH])); s
}
mframe <- function(d) {
  s <- team_sums(d)
  r <- res[, .(match_id = as.character(match_id), home = home_team_name,
               away = away_team_name, margin = home_score - away_score)][is.finite(margin)]
  h <- merge(r, s, by.x = c("match_id", "home"), by.y = c("match_id", "team"))
  a <- merge(r, s, by.x = c("match_id", "away"), by.y = c("match_id", "team"))
  m <- merge(h, a[, c("match_id", paste0("t_", LBL[CH])), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  for (ch in LBL[CH]) m[, (paste0("d_", ch)) := get(paste0(ch, "_h")) - get(paste0(ch, "_a"))]
  m
}
# column names come out as t_<lbl>_h / t_<lbl>_a
mframe2 <- function(d) {
  s <- team_sums(d)
  r <- res[, .(match_id = as.character(match_id), home = home_team_name,
               away = away_team_name, margin = home_score - away_score)][is.finite(margin)]
  h <- merge(r, s, by.x = c("match_id", "home"), by.y = c("match_id", "team"))
  a <- merge(r, s, by.x = c("match_id", "away"), by.y = c("match_id", "team"))
  m <- merge(h, a[, c("match_id", paste0("t_", LBL[CH])), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  for (ch in LBL[CH]) {
    m[, (paste0("d_", ch)) := get(paste0("t_", ch, "_h")) - get(paste0("t_", ch, "_a"))]
  }
  m
}

frm <- as.formula(paste("margin ~ 0 +", paste0("d_", LBL[CH], collapse = " + ")))
report <- function(d, label) {
  m <- mframe2(d)
  f <- lm(frm, data = m)
  co <- summary(f)$coefficients
  data.table(arm = label, channel = sub("^d_", "", rownames(co)),
             coef = round(co[, 1], 4), se = round(co[, 2], 4), t = round(co[, 3], 2),
             n = nrow(m))
}
say("")
say("=== EPV LAYER (contemporaneous): this match's accumulated value -> margin ===")
tb <- rbind(report(v2, "v2"), report(v3, "v3"))
say_dt(tb, 10)

say("")
say("The v2 cont_aerial coefficient here is the -0.41 figure. If it reproduces,")
say("it was never a lineup artefact and the retraction must itself be retracted.")

say("")
say("=== Refitted EPV3_POINTS_SCALE (v3, EPV layer) ===")
k <- tb[arm == "v3"]
say(paste0("EPV3_POINTS_SCALE <- c(",
           paste(sprintf('%s = %.4f', k$channel, k$coef), collapse = ", "), ")"))
say("")
say("Refit check -- scaling each channel by its own coefficient must return 1.0:")
m3 <- mframe2(v3)
for (i in seq_len(nrow(k))) m3[, (paste0("d_", k$channel[i])) := get(paste0("d_", k$channel[i])) * k$coef[i]]
co2 <- summary(lm(frm, data = m3))$coefficients
say_dt(data.table(channel = sub("^d_", "", rownames(co2)), coef = round(co2[, 1], 6)), 6)

close(con)
cat("\nWrote ", OUT, "\n")
