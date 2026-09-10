# Does the difficulty leak actually move player ratings, and does it hurt
# defenders? (issues #210, #209, and the defender question this session opened
# with)
# =============================================================================
# Established: kick_len and fwd_gain are computed from the resolving row, so
# they leak the turnover target. Removing them costs 0.050 log loss but removes
# the p_hat = 1 rows entirely (max clean p_hat = 0.941 over 150,071 disposals).
#
# The hypothesis this tests, which was flagged as a hypothesis and not a claim:
# the defence is paid (1 - p_hat) * surprise, and the leak inflates p_hat's
# UPPER TAIL (p95 0.627 against 0.477). An inflated upper tail should therefore
# suppress defensive credit specifically where the model is most confident.
#
# Method: build the difficulty terms BOTH ways through the real machinery --
# only fit_disposal_models()'s formula is swapped, everything downstream
# (V_pre, decision, surprise, the ledger, the team-margin convention) is the
# shipped code. Same leak-safe regime as production: fit on 2025, score 2026.
#
# Then compare published net_points per position. That is the only number that
# decides whether the leak matters to a rating, as opposed to a metric.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_leak_effect_on_ratings.R"'
suppressMessages({library(data.table); library(mgcv); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))
ps  <- as.data.table(load_player_stats(SEASON, refresh = TRUE))
res <- as.data.table(load_results(SEASON))

# --- the shipped fitter, kept so it can be restored ---------------------------
orig_fit <- torp:::fit_disposal_models

clean_fit <- function(de, train_idx = rep(TRUE, nrow(de))) {
  tr <- de[train_idx]
  # kick_len and fwd_gain REMOVED -- both are functions of the resolving row's
  # coordinates, which is the row whose team membership is the target.
  rhs <- ~ s(x, abs_y) + s(goal_dist) + s(exp_pts) + is_handball + i50f
  fit <- function(f, dd, ...) {
    for (fv in c("is_handball", "i50f")) {
      if (fv %in% all.vars(f) && length(unique(dd[[fv]][!is.na(dd[[fv]])])) < 2) {
        f <- stats::update(f, stats::as.formula(paste(". ~ . -", fv)))
      }
    }
    mgcv::bam(f, data = droplevels(dd), discrete = TRUE, ...)
  }
  list(
    p   = fit(stats::update(rhs, turnover ~ .), tr, family = stats::binomial()),
    ret = fit(stats::update(rhs, V_after ~ .), tr[turnover == FALSE]),
    trn = fit(stats::update(rhs, V_after ~ .), tr[turnover == TRUE])
  )
}

build_terms <- function(label) {
  say("\n--- difficulty terms: ", label, " (fit on ", SEASON - 1, ") ---")
  tm <- np_difficulty_terms_for_season(SEASON, pbp_data = pbp, chains = ch)
  tm <- as.data.table(tm); tm[, match_id := as.character(match_id)]
  say("  rows ", format(nrow(tm), big.mark = ","),
      "   p_hat: median ", round(median(tm$p_hat), 3),
      "  p95 ", round(quantile(tm$p_hat, .95), 3),
      "  max ", round(max(tm$p_hat), 5),
      "   rows at p>=0.99: ", tm[p_hat >= 0.99, .N])
  tm
}

rate <- function(tm, label) {
  say("\n--- ledger: ", label, " ---")
  np <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                         stoppages = "allocate", difficulty_terms = tm,
                         return_payments = TRUE)
  fin <- as.data.table(torp:::.np_team_margin(np, pbp, ps, res))
  fin[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
  fin[, .(match_id, player_id, net_points)]
}

tm_leaky <- build_terms("LEAKY (shipped formula)")
r_leaky  <- rate(tm_leaky, "leaky")

assignInNamespace("fit_disposal_models", clean_fit, ns = "torp")
tm_clean <- build_terms("CLEAN (kick_len + fwd_gain removed)")
r_clean  <- rate(tm_clean, "clean")
assignInNamespace("fit_disposal_models", orig_fit, ns = "torp")

# --- compare, by position -----------------------------------------------------
pg <- as.data.table(load_player_game_ratings(SEASON))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
nm <- unique(pg[, .(match_id, player_id, position_group)])

cmp <- merge(r_leaky, r_clean, by = c("match_id", "player_id"),
             suffixes = c("_leaky", "_clean"))
cmp <- merge(cmp, nm, by = c("match_id", "player_id"), all.x = TRUE)
cmp[, delta := net_points_clean - net_points_leaky]

say("\n\n=== published net points per game, by position ===")
say("positive delta = the CLEAN model pays this position MORE")
ORD <- c("KEY_FORWARD", "MEDIUM_FORWARD", "MIDFIELDER", "RUCK",
         "MEDIUM_DEFENDER", "KEY_DEFENDER")
out <- cmp[!is.na(position_group), .(
  n = .N,
  leaky = round(mean(net_points_leaky), 3),
  clean = round(mean(net_points_clean), 3),
  delta = round(mean(delta), 3)
), by = position_group]
print(out[match(ORD, position_group)])

say("\n=== is the defender gap narrower under the clean model? ===")
g <- function(col) round(out[position_group == "KEY_FORWARD"][[col]] -
                         out[position_group == "KEY_DEFENDER"][[col]], 3)
say("  KEY_FORWARD - KEY_DEFENDER, leaky: ", g("leaky"))
say("  KEY_FORWARD - KEY_DEFENDER, clean: ", g("clean"))

say("\n=== how much does any individual move? ===")
say("  max |delta| on a player-game: ", round(max(abs(cmp$delta)), 3))
say("  mean |delta|:                 ", round(mean(abs(cmp$delta)), 3))
say("  player-games moving > 1 pt:   ",
    format(cmp[abs(delta) > 1, .N], big.mark = ","), " of ",
    format(nrow(cmp), big.mark = ","))
