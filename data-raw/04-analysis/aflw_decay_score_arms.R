## AFLW: score all decay/season arms on margin, §17 round-level expanding
## walk-forward, ONE controlled pass, identical() common match set.
##
## stats-discipline #5 AXIS TABLE -- what varies, what is held fixed:
##   VARIES: rating-type (raw RAPM / pure SPM / shrunk xRAPM) x
##           grain (season-block expanding / decay hl=730)
##   FIXED : comp=AFLW, round-level expanding walk-forward, TEST_SEASONS,
##           TOG-weighted team aggregation, identical common match set, MIN_TRAIN.
##
## PREDICTED BEFORE RUNNING (limiting case / directional):
##   AFLW has ~485 matches vs AFLM's ~1269, and far fewer games per player, so
##   shrinkage_weight = n_games/(n_games+prior_games) should sit LOWER than AFLM's
##   -> the SPM prior does MORE work here. If the empirical-Bayes rationale holds,
##   shrunk xRAPM should beat raw RAPM for AFLW, REVERSING the AFLM finding (§20-21,
##   raw 0.2871 > shrunk 0.2839). That would be an explainable difference, not a
##   contradiction. If raw still wins even at AFLW's thinness, that is evidence the
##   SPM prior itself is weak here rather than that shrinkage is mis-tuned.
suppressMessages({
  library(data.table)
  devtools::load_all(quiet = TRUE)
})
OUT <- Sys.getenv("AFLW_DECAY_OUT", unset = tempdir())
HL <- 730
TEST_SEASONS <- c(2023L, 2024L, 2025L, 2026L)
MIN_TRAIN <- 20L

# --- AFLW team-match frame: score_diff per team-side (no ws0r3 equivalent exists) ---
res <- as.data.table(load_results(TRUE, comp = "AFLW"))
res <- res[!is.na(home_score) & !is.na(away_score)]
tmd <- rbindlist(list(
  res[, .(match_id, season, round_number, team_id = home_team_id, score_diff = home_score - away_score)],
  res[, .(match_id, season, round_number, team_id = away_team_id, score_diff = away_score - home_score)]
))
tmd[, season := as.integer(season)]

fx <- as.data.table(load_fixtures(all = TRUE, comp = "AFLW"))
fx <- fx[!is.na(utc_start_time), .(match_id, match_date = as.Date(utc_start_time))]
mo <- merge(unique(tmd[, .(match_id, season, round_number)]), fx, by = "match_id")
round_order <- mo[, .(round_first_date = min(match_date)), by = .(season, round_number)]
setorder(round_order, round_first_date); round_order[, seq_idx := .I]

# --- player -> team -> match -> TOG (AFLW uses player_stats; no chain data exists) ---
ps <- as.data.table(load_player_stats(TRUE, comp = "AFLW"))
ps <- ps[!is.na(time_on_ground_percentage) & !is.na(player_id) & !is.na(team_id)]
ps[, `:=`(tog = pmin(pmax(time_on_ground_percentage / 100, 0), 1),
          player_id = as.character(player_id), season = as.integer(season))]
ps_r <- merge(ps[, .(match_id, team_id, player_id, season, tog)],
              unique(tmd[, .(match_id, round_number)]), by = "match_id")

.opp_diff <- function(team_totals) {
  opp <- merge(team_totals, team_totals, by = "match_id", allow.cartesian = TRUE)
  opp <- opp[team_id.x != team_id.y]
  opp[, rating_diff := team_val.x - team_val.y]
  opp[, .(match_id, team_id = team_id.x, rating_diff)]
}
mk_frame <- function(diff_dt) {
  f <- merge(tmd, diff_dt, by = c("match_id", "team_id"), all.x = TRUE)
  f <- merge(f, round_order[, .(season, round_number, seq_idx)], by = c("season", "round_number"))
  f[!is.na(rating_diff) & !is.na(score_diff)]
}

# --- DECAY grain: per-round snapshots (join on season+round, point-in-time by construction) ---
snap <- readRDS(file.path(OUT, sprintf("aflw_snap_hl%s.rds", HL)))
snap[, player_id := as.character(player_id)]
cat(sprintf("Decay snapshot: %d rows, %d round-checkpoints, mean shrinkage_weight=%.3f\n",
            nrow(snap), nrow(unique(snap[, .(season, round_number)])),
            mean(snap$shrinkage_weight, na.rm = TRUE)))

mk_decay <- function(valcol) {
  pr <- merge(ps_r, snap[, c("player_id", "season", "round_number", valcol), with = FALSE],
              by = c("player_id", "season", "round_number"))
  setnames(pr, valcol, "val")
  .opp_diff(pr[, .(team_val = sum(tog * val)), by = .(match_id, team_id)])
}

# --- SEASON grain: expanding-window season-block engine (one rating per player-season) ---
cat("Building season-block arms (AFLW)...\n")
rapm_exp <- build_team_rapm_expanding(design = "split", comp = "AFLW")
spm_exp  <- build_team_spm_expanding(rapm_exp, comp = "AFLW")
spm_exp[, player_id := as.character(player_id)]
spm_exp[, `:=`(rapm_raw_net_s = rapm_offense - rapm_defense,
               spm_pure_net_s = spm_offense - spm_defense)]
cat(sprintf("Season-block: %d player-season rows, seasons %s, mean shrinkage_weight=%.3f\n",
            nrow(spm_exp), paste(sort(unique(spm_exp$season)), collapse=","),
            mean(spm_exp$shrinkage_weight, na.rm = TRUE)))

mk_season <- function(valcol) {
  pr <- merge(ps_r, spm_exp[, c("player_id", "season", valcol), with = FALSE],
              by = c("player_id", "season"))
  setnames(pr, valcol, "val")
  .opp_diff(pr[, .(team_val = sum(tog * val)), by = .(match_id, team_id)])
}

frames <- list(
  `RAPM decay hl=730 (raw)`   = mk_frame(mk_decay("rapm_raw_net")),
  `xRAPM decay hl=730 (shrunk)` = mk_frame(mk_decay("team_rapm_shrunk")),
  `SPM decay hl=730 (pure)`   = mk_frame(mk_decay("spm_pure_net")),
  `RAPM season-block (raw)`   = mk_frame(mk_season("rapm_raw_net_s")),
  `xRAPM season-block (shrunk)` = mk_frame(mk_season("team_rapm_shrunk")),
  `SPM season-block (pure)`   = mk_frame(mk_season("spm_pure_net_s"))
)

round_walk_forward <- function(f, use_feature = TRUE) {
  test_blocks <- round_order[season %in% TEST_SEASONS]
  preds <- vector("list", nrow(test_blocks))
  for (i in seq_len(nrow(test_blocks))) {
    blk <- test_blocks[i]
    train <- f[seq_idx < blk$seq_idx]
    test  <- f[season == blk$season & round_number == blk$round_number]
    if (nrow(test) == 0 || nrow(train) < MIN_TRAIN) next
    if (use_feature) {
      m <- lm(score_diff ~ rating_diff, data = train); pred <- predict(m, newdata = test)
    } else pred <- rep(mean(train$score_diff), nrow(test))
    preds[[i]] <- data.table(match_id = test$match_id, team_id = test$team_id,
                             season = blk$season, actual = test$score_diff, pred = pred)
  }
  rbindlist(preds, fill = TRUE)
}
metrics <- function(p) {
  r <- p$actual - p$pred
  data.frame(n = nrow(p), RMSE = sqrt(mean(r^2)),
             R2 = 1 - sum(r^2) / sum((p$actual - mean(p$actual))^2))
}

raw_preds <- lapply(frames, round_walk_forward)
naive_raw <- round_walk_forward(frames[[1]], use_feature = FALSE)

# stats-discipline #7: coverage BEFORE the headline number
cat("\n=== Coverage per arm (pre-intersection) ===\n")
for (nm in names(raw_preds)) {
  p <- raw_preds[[nm]]
  cat(sprintf("  %-30s %4d matches, seasons %s\n", nm, uniqueN(p$match_id),
              paste(sort(unique(p$season)), collapse = ",")))
}
common <- Reduce(intersect, lapply(raw_preds, function(p) unique(p$match_id)))
cat(sprintf("\nCommon match set: %d matches\n", length(common)))
sub <- lapply(raw_preds, function(p) p[match_id %in% common])
naive <- naive_raw[match_id %in% common]
ref <- sort(unique(sub[[1]]$match_id))
for (nm in names(sub)) stopifnot(identical(sort(unique(sub[[nm]]$match_id)), ref))
stopifnot(identical(sort(unique(naive$match_id)), ref))
cat("identical() match-set check passed across all arms + naive.\n")
sea <- sub[[1]][, .(matches = uniqueN(match_id)), by = season][order(season)]
cat("Common-set seasons:\n"); print(sea)

report <- rbind(cbind(arm = "Naive (train-mean)", metrics(naive)),
                rbindlist(lapply(names(sub), function(nm) cbind(arm = nm, metrics(sub[[nm]])))))
setorder(as.data.table(report), R2)
cat("\n=== AFLW FINAL TABLE (pooled", paste(range(TEST_SEASONS), collapse="-"), ") ===\n")
print(report[order(report$R2), ], row.names = FALSE)
write.csv(report, file.path(OUT, "aflw_arms_report.csv"), row.names = FALSE)
cat("\n=== DONE ===\n")
