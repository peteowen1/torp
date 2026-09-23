# How much of a turnover should the defence be paid, and how should the rest be
# spread? Scored on Pete's criterion rather than repeatability: does a defensive
# rating built from LAST season predict the points a team concedes THIS season?
#
# Why that criterion. Repeatability asks whether a number persists; it cannot
# tell whether the number is defence. Points conceded is the thing defensive
# credit is supposed to be about, and the test is out of sample by construction:
# ratings come from 2025, the target is 2026.
#
# Why the defensive share matters at player level but not team level: raising it
# moves value from blame-on-the-loser to credit-to-the-winner, who is on the
# OTHER team. A team's own total is barely touched; who inside each team holds
# the value changes a lot. So the test has to run through lineups.
suppressMessages({library(data.table); library(arrow); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
stopifnot(is.null(get_local_data_dir()))
S <- "data-raw/outputs"
say <- function(...) cat(..., "\n", sep = "")
t0 <- Sys.time()
terms_all <- fread(file.path(S, "np_difficulty_terms_2025_2026.csv"))
terms_all[, match_id := as.character(match_id)]

raw <- list()
for (s in c(2025L, 2026L)) {
  raw[[as.character(s)]] <- list(
    pbp = as.data.table(load_pbp(s)), ch = as.data.table(load_chains(s)),
    ps  = as.data.table(load_player_stats(s, refresh = TRUE)),
    res = as.data.table(load_results(s)),
    tm  = terms_all[substr(match_id, 5, 8) == as.character(s)])
  say("loaded ", s, " (", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min)")
}

# Defensive value comes from the FINISHED frame, not from the payment rows.
# The defensive pool is paid to a row with no player attached and is spread
# afterwards, so reading payments made every `spread` arm come back identical --
# the second bug this gate's duplicate-arm guard caught.
#   np_defensive_won : he won the ball off the opposition
#   np_contest_won   : he won the contest (marks, spoils)
#   np_defensive     : his share of the spread defensive pool
DEF_COLS <- c("np_defensive_won", "np_contest_won", "np_defensive")

# NOTE: under credit = "difficulty" the turnover split is governed by
# blame_share (the disposer's share of a lost surprise), NOT defensive_share,
# which only reaches the 5% of rows the difficulty model cannot score. A first
# version of this gate varied defensive_share and returned identical results for
# 0.20 and 0.30, which is what caught it. Lower blame = more to the defence.
build <- function(s, blame, spread, ground_share = NULL, default_share = NULL) {
  r <- raw[[as.character(s)]]
  args <- list(pbp_data = r$pbp, player_stats = r$ps, results = r$res, chains = r$ch,
               credit = "difficulty", stoppages = "allocate", difficulty_terms = r$tm,
               blame_share = blame, spread = spread)
  if (!is.null(ground_share)) {
    tb <- NP_BALL_WINNER_SHARE_BY_ACT
    ground <- c("Loose Ball Get", "Hard Ball Get", "Loose Ball Get Crumb", "Gather",
                "Gather From Hitout", "Gather from Opposition", "Handball Received",
                "Ground Kick")
    tb[names(tb) %in% ground] <- ground_share
    assignInNamespace("NP_BALL_WINNER_SHARE_BY_ACT", tb, ns = "torp")
  }
  if (!is.null(default_share)) {
    assignInNamespace("NP_BALL_WINNER_SHARE_BY_ACT_DEFAULT", default_share, ns = "torp")
  }
  np <- as.data.table(do.call(build_net_points, args))
  miss <- setdiff(DEF_COLS, names(np))
  if (length(miss)) stop("missing defensive columns: ", paste(miss, collapse = ", "))
  np[, def := rowSums(as.matrix(.SD)), .SDcols = DEF_COLS]
  list(def = np[, .(match_id = as.character(match_id),
                    player_id = as.character(player_id), def)])
}

lineups <- function(s) {
  r <- raw[[as.character(s)]]
  ps <- r$ps
  tid <- unique(r$pbp[, .(team_id = as.character(team_id), team)])
  l <- ps[, .(match_id = as.character(match_id), player_id = as.character(player_id),
              team_id = as.character(team_id))]
  merge(l, tid, by = "team_id")[, team_id := NULL]
}

conceded <- function(s) {
  r <- raw[[as.character(s)]]
  res <- r$res[!is.na(home_score)]
  rbind(res[, .(match_id = as.character(match_id), team = torp_replace_teams(home_team_name),
                against = away_score)],
        res[, .(match_id = as.character(match_id), team = torp_replace_teams(away_team_name),
                against = home_score)])
}

arm <- function(label, blame, spread = "matchup", gs = NULL, ds = NULL) {
  on.exit({
    assignInNamespace("NP_BALL_WINNER_SHARE_BY_ACT", .ORIG_TB, ns = "torp")
    assignInNamespace("NP_BALL_WINNER_SHARE_BY_ACT_DEFAULT", .ORIG_DEF, ns = "torp")
  }, add = TRUE)
  a25 <- build(2025L, blame, spread, gs, ds)
  # rating: a player's mean defensive value per game in 2025, min 5 games
  rate <- a25$def[, .(n = .N, def_rate = mean(def)), by = player_id][n >= 5]
  lu26 <- merge(lineups(2026L), rate, by = "player_id")
  tm <- lu26[, .(team_def = sum(def_rate), covered = .N), by = .(match_id, team)]
  tm <- tm[covered >= 15]                       # most of the side has a 2025 rating
  d <- merge(tm, conceded(2026L), by = c("match_id", "team"))
  fit <- lm(against ~ team_def, data = d)
  data.table(label, blame, spread, ground = gs %||% NA_real_, deflt = ds %||% NA_real_,
             n = nrow(d),
             r = round(cor(d$team_def, d$against), 4),
             beta = round(coef(fit)[2], 3),
             t = round(summary(fit)$coefficients[2, 3], 2),
             rmse = round(sqrt(mean(residuals(fit)^2)), 2))
}

`%||%` <- function(a, b) if (is.null(a)) b else a
.ORIG_TB <- NP_BALL_WINNER_SHARE_BY_ACT
.ORIG_DEF <- NP_BALL_WINNER_SHARE_BY_ACT_DEFAULT
# How much of the defence's share goes to the NAMED winner rather than the pool.
# Marks and frees already pay 0.8; ground balls and anything unlisted pay 0.3, and
# across 2026 that leaves 47% of all defensive value in an unnamed pool.
grid <- list(
  list("ground .30 (shipped)", 0.30, "matchup", 0.30, 0.30),
  list("ground .50",           0.30, "matchup", 0.50, 0.50),
  list("ground .70",           0.30, "matchup", 0.70, 0.70),
  list("ground .85",           0.30, "matchup", 0.85, 0.85),
  list("ground 1.00",          0.30, "matchup", 1.00, 1.00),
  list("ground .70, blame .15",0.15, "matchup", 0.70, 0.70),
  list("ground .70, tog",      0.30, "tog",     0.70, 0.70))

out <- rbindlist(lapply(grid, function(g) {
  r <- arm(g[[1]], g[[2]], g[[3]], g[[4]], g[[5]])
  say(sprintf("  %-26s r %+.4f | beta %+.3f | t %+.2f | rmse %.2f (n=%d)",
              r$label, r$r, r$beta, r$t, r$rmse, r$n))
  r
}))
if (uniqueN(out$r) < nrow(out)) {
  say("!! two arms returned an identical correlation -- a parameter is not reaching the code")
}
say("")
say("A GOOD defensive rating should have a NEGATIVE correlation with points conceded:")
say("more defensive value in the side, fewer points against.")
print(out[order(r)])
fwrite(out, file.path(S, "np_ball_winner_share_sweep.csv"))
say("\ndone in ", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min")
