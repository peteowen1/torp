# The team-sum convention: how should a side's charge be divided?
# =============================================================================
# Two questions, crossed.
#
#   named share  NA  = double each side and stop, keeping whatever shares the
#                      ledger already computed. Pete's reading, and the one that
#                      preserves ruck symmetry for free: on a stoppage the two
#                      rucks stay mirror images at +1.00 and -1.00.
#                0.5 = the named players collectively keep half the row and the
#                      pool takes the rest. Sensible on a turnover, where the
#                      ledger has no considered view; on a stoppage it throws
#                      away the 50/30/20 split and breaks the symmetry, because
#                      the winning side has two names to share the half and the
#                      losing side has one. That is the whole ruck deficit:
#                      -10.51 in concessions against +8.61 in gains, while every
#                      other position nets between +2.27 and +2.55.
#
#   pool by      tog   = whoever was on the ground.
#                dacts = tackles, intercepts, one-percenters. Gives key
#                      defenders 20.3% of a positive pool against 13.1% by
#                      minutes, which is most of the defender advantage. Note it
#                      pays defenders twice: an intercept mark already pays the
#                      winner 80% of the defensive share on its own row.
#
# Scored on within-position repeatability 2025 -> 2026, the criterion that chose
# every v4 parameter. An earlier version of this sweep ran against a broken
# split and its numbers (0.564 against 0.598) are void.
suppressMessages({library(data.table); library(arrow); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
stopifnot(is.null(get_local_data_dir()))
S <- "data-raw/outputs"
SEASONS <- c(2025L, 2026L)
MIN_G <- 10L
say <- function(...) cat(..., "\n", sep = "")
t0 <- Sys.time()

terms_all <- fread(file.path(S, "np_difficulty_terms_2025_2026.csv"))
terms_all[, match_id := as.character(match_id)]

raw <- list()
for (s in SEASONS) {
  raw[[as.character(s)]] <- list(
    pbp = as.data.table(load_pbp(s)), ch = as.data.table(load_chains(s)),
    ps  = as.data.table(load_player_stats(s, refresh = TRUE)),
    tms = as.data.table(load_teams(s)),
    tm  = terms_all[substr(match_id, 5, 8) == as.character(s)])
  say("loaded ", s, " (", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min)")
}

lu <- as.data.table(read_parquet(file.path(S, "v3v4_pgd_v4.parquet"),
        col_select = c("match_id", "player_id", "season", "position_group", "player_name")))
lu <- lu[season %in% SEASONS]
lu[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
info <- lu[, .(pos = position_group[1], name = player_name[1]), by = player_id]

arm_frame <- function(s, named_share, pool_by) {
  assignInNamespace("NP_TEAM_MARGIN_CONVENTION", TRUE, ns = "torp")
  assignInNamespace("NP_TEAM_MARGIN_NAMED_SHARE", named_share, ns = "torp")
  assignInNamespace("NP_TEAM_MARGIN_POOL_BY", pool_by, ns = "torp")
  r <- raw[[as.character(s)]]
  d <- as.data.table(create_player_game_data(r$pbp, r$ps, r$tms, chains = r$ch,
                                             epv_engine = "v4", difficulty_terms = r$tm))
  d[, .(match_id = as.character(match_id), player_id = as.character(player_id),
        team, epv, tog = time_on_ground_percentage / 100)]
}

fz <- function(r, n) { z <- atanh(r); w <- n - 3; tanh(sum(z * w) / sum(w)) }

score <- function(label, named_share, pool_by) {
  a <- rbindlist(lapply(SEASONS, function(s) arm_frame(s, named_share, pool_by)[, season := s]))
  p <- a[, .(g = .N, v = mean(epv), tog = mean(tog), team = team[1]), by = .(player_id, season)]
  p <- merge(p, info, by = "player_id")[g >= MIN_G]
  w <- dcast(p, player_id + pos ~ season, value.var = "v")
  setnames(w, c("2025", "2026"), c("y1", "y2"), skip_absent = TRUE)
  w <- w[!is.na(y1) & !is.na(y2)]
  by_pos <- w[, .(n = .N, r = if (.N > 8) cor(y1, y2) else NA_real_), by = pos][!is.na(r)]
  cur <- p[season == 2026L]
  pos_mean <- cur[, .(m = mean(v)), by = pos]
  data.table(label,
             named = if (is.na(named_share)) "ledger shares" else as.character(named_share),
             pool = pool_by, n = nrow(w),
             r_within = round(fz(by_pos$r, by_pos$n), 4),
             r_all = round(cor(w$y1, w$y2), 4),
             team_pct = round(100 * summary(lm(cur$v ~ factor(cur$team)))$r.squared),
             tog_cor = round(cor(cur$tog, cur$v), 3),
             ruck = round(pos_mean[pos == "RUCK"]$m, 2),
             keydef = round(pos_mean[pos == "KEY_DEFENDER"]$m, 2),
             keyfwd = round(pos_mean[pos == "KEY_FORWARD"]$m, 2),
             pos_spread = round(diff(range(pos_mean$m)), 2))
}

grid <- list(
  list("double only, minutes",      NA_real_, "tog"),
  list("double only, def acts",     NA_real_, "dacts"),
  list("double + half, minutes",    0.5,      "tog"),
  list("double + half, def acts",   0.5,      "dacts"))

out <- rbindlist(lapply(grid, function(g) {
  r <- score(g[[1]], g[[2]], g[[3]])
  say(sprintf("  %-24s r_within %.4f | team %2d%% | ruck %+.2f | keydef %+.2f | keyfwd %+.2f | spread %.2f",
              r$label, r$r_within, r$team_pct, r$ruck, r$keydef, r$keyfwd, r$pos_spread))
  r
}))
say("")
print(out[order(-r_within)])
fwrite(out, file.path(S, "np_convention_sweep.csv"))
say("\nFor reference, the SHIPPED ledger on the same pair scores r_within 0.591,")
say("team 11%, and its position spread runs key forward 8.02 to medium defender 5.24.")
say("\ndone in ", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min")
