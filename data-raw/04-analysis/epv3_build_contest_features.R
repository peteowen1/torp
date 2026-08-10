# Team-level contest features: the one genuinely NEW information from v3.
#
# WHY THESE AND NOT MORE RATING TUNING. The match model already consumes
# epr_recv_diff, epr_disp_diff, epr_spoil_diff and epr_hitout_diff alongside
# epr_diff, in all five GAMs. It already does free per-channel reweighting,
# which is why every calibration change tonight was absorbed: shrinkage, points
# scaling and 3-vs-4 channels are all re-weightings of information the model
# already holds. Only NEW information can move match MAE.
#
# The contest model is new information. p, V_att and V_def are estimated per
# aerial contest from kick context, and nothing in the existing feature set
# knows how HARD a team's contests were or whether it won more than the
# situation warranted. Player ratings summed over a lineup cannot express that.
#
# THE HEADLINE FEATURE is wins above expectation: actual wins minus the sum of
# win probabilities. A team can win many contests by kicking to easy ones (high
# volume, low difficulty) or by beating hard ones. Only the second is skill, and
# only WAE separates them.
#
# LEAK SAFETY is the design constraint. A match's own contests cannot predict
# that match, so every feature is a decay-weighted average over the team's
# STRICTLY EARLIER matches. The contest model itself is already fitted per season
# on earlier seasons only.
#
# WHICH TEAM IS DEFENDING: kick_tid gives the attacker, but out_tid only
# identifies the defender when the defence WON -- when the attack retains,
# out_tid == kick_tid. So the defender is derived as "the other team in this
# match", from the lineup data.
#
# PERFORMANCE: 290k contests -> ~2,500 (match, team) rows -> rolling over ~130
# matches per team for 18 teams. The rolling step is O(m^2) within a team at
# m~130, ~300k operations total. Not worth optimising; this workstream's cost is
# entirely the match gate.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_contest_features.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

HALF_LIFE <- 365

say("=== Team contest features from the v3 contest model ===")

cst <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_contest_table.parquet")))
say("contests: ", format(nrow(cst), big.mark = ","), " | matches ", uniqueN(cst$match_id))

pbp <- as.data.table(load_pbp(TRUE))
mdates <- unique(pbp[, .(match_id, date = as.Date(utc_start_time))], by = "match_id")
cst <- merge(cst, mdates, by = "match_id", all.x = TRUE)[!is.na(date)]

# The two teams in each match, so the defender can be derived.
teams <- as.data.table(load_teams(TRUE))
mt <- unique(teams[, .(match_id, team_id)])
mt <- mt[, if (.N == 2) .SD, by = match_id]
say("matches with exactly two teams resolved: ", uniqueN(mt$match_id))

pair <- merge(cst[, .(match_id, kick_tid)], mt, by = "match_id", allow.cartesian = TRUE)
pair <- unique(pair[team_id != kick_tid][, .(match_id, kick_tid, def_tid = team_id)])
cst <- merge(cst, pair, by = c("match_id", "kick_tid"), all.x = TRUE)
say("contests with a resolved defending team: ",
    round(100 * mean(!is.na(cst$def_tid)), 1), "%")
cst <- cst[!is.na(def_tid)]

# ---- Per (match, team) contest summary -------------------------------------
att <- cst[, .(
  n_att      = .N,
  wins_att   = sum(!def_win),
  exp_att    = sum(1 - p_hat),
  delta_att  = mean(Delta)
), by = .(match_id, date, team_id = kick_tid)]

def <- cst[, .(
  n_def      = .N,
  wins_def   = sum(def_win),
  exp_def    = sum(p_hat),
  delta_def  = mean(Delta)
), by = .(match_id, date, team_id = def_tid)]

tm <- merge(att, def, by = c("match_id", "date", "team_id"), all = TRUE)
for (c in c("n_att", "wins_att", "exp_att", "n_def", "wins_def", "exp_def")) {
  set(tm, which(is.na(tm[[c]])), c, 0)
}
tm[, `:=`(
  wae_att = wins_att - exp_att,          # attacking contests won above expectation
  wae_def = wins_def - exp_def,          # defensive contests won above expectation
  vol     = n_att + n_def,
  stakes  = fifelse(is.finite(delta_att), delta_att, 0)
)]
tm[, wae := wae_att + wae_def]

say("")
say("--- per (match, team) contest summary ---")
say("rows ", nrow(tm), " | teams ", uniqueN(tm$team_id))
say_dt(tm[, .(n_att = round(mean(n_att), 1), n_def = round(mean(n_def), 1),
              wae = round(mean(wae), 3), sd_wae = round(sd(wae), 3),
              stakes = round(mean(stakes), 3))], 4)
say("")
say("sanity: wae should average ~0 across all teams if the contest model is")
say("calibrated -- someone's win is someone else's loss.")
say("  mean wae over all (match, team): ", round(mean(tm$wae), 4))

# ---- Rolling, strictly-earlier-matches only --------------------------------
setorder(tm, team_id, date)
roll_one <- function(d) {
  n <- nrow(d)
  out_wae <- rep(NA_real_, n); out_stk <- rep(NA_real_, n); out_vol <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    prev <- seq_len(i - 1L)
    if (length(prev) == 0) next
    w <- exp(-as.numeric(d$date[i] - d$date[prev]) / HALF_LIFE * log(2))
    sw <- sum(w)
    if (sw <= 0) next
    out_wae[i] <- sum(d$wae[prev] * w) / sw
    out_stk[i] <- sum(d$stakes[prev] * w) / sw
    out_vol[i] <- sum(d$vol[prev] * w) / sw
  }
  list(roll_wae = out_wae, roll_stakes = out_stk, roll_vol = out_vol)
}
tm[, c("roll_wae", "roll_stakes", "roll_vol") := roll_one(.SD), by = team_id,
   .SDcols = c("date", "wae", "stakes", "vol")]

say("")
say("--- rolling features (decay half-life ", HALF_LIFE, " days) ---")
say("non-NA rows: ", sum(!is.na(tm$roll_wae)), " of ", nrow(tm),
    "   (the NAs are each team's first match, correctly)")
say_dt(tm[!is.na(roll_wae), .(
  roll_wae_mean = round(mean(roll_wae), 4), roll_wae_sd = round(sd(roll_wae), 4),
  roll_stakes_sd = round(sd(roll_stakes), 4), roll_vol_sd = round(sd(roll_vol), 3))], 4)

# ---- Does it carry signal? in-sample screen before spending a gate ---------
say("")
say("=== SCREEN: does any of this predict margin, before spending 40 min? ===")
res <- as.data.table(load_results(TRUE))
r <- res[, .(match_id = as.character(match_id), home_team_name, away_team_name,
             margin = home_score - away_score)][is.finite(margin)]
tj <- merge(tm, unique(teams[, .(match_id, team_id, team_name)]),
            by = c("match_id", "team_id"), all.x = TRUE)
h <- merge(r, tj, by.x = c("match_id", "home_team_name"), by.y = c("match_id", "team_name"))
a <- merge(r, tj, by.x = c("match_id", "away_team_name"), by.y = c("match_id", "team_name"))
F3 <- c("roll_wae", "roll_stakes", "roll_vol")
m <- merge(h[, c("match_id", "margin", F3), with = FALSE],
           a[, c("match_id", F3), with = FALSE], by = "match_id", suffixes = c("_h", "_a"))
for (f in F3) m[, (paste0("d_", f)) := get(paste0(f, "_h")) - get(paste0(f, "_a"))]
m <- m[complete.cases(m[, paste0("d_", F3), with = FALSE])]
say("matches usable: ", nrow(m))
if (nrow(m) > 200) {
  f <- lm(as.formula(paste("margin ~", paste0("d_", F3, collapse = " + "))), data = m)
  say_dt(as.data.table(round(summary(f)$coefficients, 4), keep.rownames = "feature"), 6)
  say("R2 ", round(summary(f)$r.squared, 5))
  say("")
  say("This is in-sample and uncontrolled -- the match model already has elo and")
  say("player ratings, so any real contribution is SMALLER than this. A null")
  say("here would be conclusive and would save the gate.")
}

arrow::write_parquet(
  tm[, .(match_id, team_id, date, roll_wae, roll_stakes, roll_vol, wae, vol, stakes)],
  file.path(OUT_DIR, "epv3_contest_team_features.parquet"))
say("")
say("wrote epv3_contest_team_features.parquet")
close(con)
cat("\nDone\n")
