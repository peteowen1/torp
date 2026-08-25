## AFLW halflife sweep -- same §17 method, ONE pass, identical() common match set.
## stats-discipline #5: ONLY halflife_days varies. comp=AFLW, decay grain,
## round-level walk-forward, TEST_SEASONS all fixed.
## PREDICTED: AFLM's sweep was flat (R^2 span 0.0043 across 180-100000). AFLW has
## a 510-day calendar-shift gap between the 2022 and 2023 seasons that AFLM has no
## analogue for, so a SHORTER halflife could plausibly be favoured here (it would
## down-weight the pre-shift era harder). If the curve is flat like AFLM's, 730
## transfers fine by default rather than by validation.
suppressMessages({
  library(data.table)
  devtools::load_all(quiet = TRUE)
})
OUT <- Sys.getenv("AFLW_DECAY_OUT", unset = tempdir())
GRID <- c(365, 730, 1095, 1825, 1e5)
TEST_SEASONS <- c(2023L, 2024L, 2025L, 2026L); MIN_TRAIN <- 20L

res <- as.data.table(load_results(TRUE, comp = "AFLW"))
res <- res[!is.na(home_score) & !is.na(away_score)]
tmd <- rbindlist(list(
  res[, .(match_id, season, round_number, team_id = home_team_id, score_diff = home_score - away_score)],
  res[, .(match_id, season, round_number, team_id = away_team_id, score_diff = away_score - home_score)]))
tmd[, season := as.integer(season)]
fx <- as.data.table(load_fixtures(all = TRUE, comp = "AFLW"))
fx <- fx[!is.na(utc_start_time), .(match_id, match_date = as.Date(utc_start_time))]
mo <- merge(unique(tmd[, .(match_id, season, round_number)]), fx, by = "match_id")
round_order <- mo[, .(d = min(match_date)), by = .(season, round_number)]
setorder(round_order, d); round_order[, seq_idx := .I]

ps <- as.data.table(load_player_stats(TRUE, comp = "AFLW"))
ps <- ps[!is.na(time_on_ground_percentage) & !is.na(player_id) & !is.na(team_id)]
ps[, `:=`(tog = pmin(pmax(time_on_ground_percentage/100, 0), 1),
          player_id = as.character(player_id), season = as.integer(season))]
ps_r <- merge(ps[, .(match_id, team_id, player_id, season, tog)],
              unique(tmd[, .(match_id, round_number)]), by = "match_id")

.opp_diff <- function(tt) {
  o <- merge(tt, tt, by = "match_id", allow.cartesian = TRUE)[team_id.x != team_id.y]
  o[, rating_diff := team_val.x - team_val.y][, .(match_id, team_id = team_id.x, rating_diff)]
}
mk_frame <- function(d) {
  f <- merge(tmd, d, by = c("match_id","team_id"), all.x = TRUE)
  f <- merge(f, round_order[, .(season, round_number, seq_idx)], by = c("season","round_number"))
  f[!is.na(rating_diff) & !is.na(score_diff)]
}
wf <- function(f, use = TRUE) {
  tb <- round_order[season %in% TEST_SEASONS]; pr <- vector("list", nrow(tb))
  for (i in seq_len(nrow(tb))) {
    b <- tb[i]; tr <- f[seq_idx < b$seq_idx]; te <- f[season == b$season & round_number == b$round_number]
    if (nrow(te) == 0 || nrow(tr) < MIN_TRAIN) next
    p <- if (use) predict(lm(score_diff ~ rating_diff, data = tr), newdata = te) else rep(mean(tr$score_diff), nrow(te))
    pr[[i]] <- data.table(match_id = te$match_id, team_id = te$team_id, actual = te$score_diff, pred = p)
  }
  rbindlist(pr, fill = TRUE)
}
met <- function(p) { r <- p$actual - p$pred
  data.frame(n = nrow(p), RMSE = sqrt(mean(r^2)), R2 = 1 - sum(r^2)/sum((p$actual-mean(p$actual))^2)) }

arms <- list(); cov <- c()
for (hl in GRID) {
  fn <- file.path(OUT, sprintf("aflw_snap_hl%s.rds", hl))
  s <- readRDS(fn); s[, player_id := as.character(player_id)]
  cov[as.character(hl)] <- nrow(unique(s[, .(season, round_number)]))
  for (v in c("team_rapm_shrunk", "rapm_raw_net")) {
    pr <- merge(ps_r, s[, c("player_id","season","round_number",v), with = FALSE],
                by = c("player_id","season","round_number"))
    setnames(pr, v, "val")
    lbl <- sprintf("%s hl=%s", if (v == "team_rapm_shrunk") "xRAPM (shrunk)" else "RAPM (raw)  ", hl)
    arms[[lbl]] <- wf(mk_frame(.opp_diff(pr[, .(team_val = sum(tog*val)), by = .(match_id, team_id)])))
  }
}
cat("=== Checkpoint coverage per halflife (stats-discipline #7) ===\n"); print(cov)
common <- Reduce(intersect, lapply(arms, function(p) unique(p$match_id)))
cat("\nCommon match set:", length(common), "matches\n")
sub <- lapply(arms, function(p) p[match_id %in% common])
ref <- sort(unique(sub[[1]]$match_id))
for (nm in names(sub)) stopifnot(identical(sort(unique(sub[[nm]]$match_id)), ref))
cat("identical() check passed across all", length(sub), "arms.\n")
rep <- rbindlist(lapply(names(sub), function(nm) cbind(arm = nm, met(sub[[nm]]))))
setorder(rep, -R2)
cat("\n=== AFLW HALFLIFE SWEEP (decay grain, pooled 2023-2026) ===\n")
print(rep, row.names = FALSE)
cat(sprintf("\nR2 span across sweep: %.4f\n", max(rep$R2) - min(rep$R2)))
write.csv(rep, file.path(OUT, "aflw_halflife_sweep.csv"), row.names = FALSE)
cat("\n=== DONE ===\n")
