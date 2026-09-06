# Refit epv_spoil's internal weights so the channel is points-anchored
# ====================================================================
# Why this and not a rescale: decompose_epv_channels.R showed epv_spoil is 100%
# box-score with NO chain-EPV term, which is why ws13 measured it converting at
# 4.02 points instead of 1. Multiplying the channel by 4 was measured and rejected
# (preview_per_channel_scale.R: narrows defender spread, drags every position's
# thin-evidence quartile down 0.25 via amplified negative priors). Fixing the
# channel's CONTENT is the remaining route.
#
# Today's weights and what they carry:
#   def_half_pressure_acts  -0.1882   41.8% of the channel   <- dominant, NEGATIVE
#   tackles                  0.2980   29.1%
#   rebound50s              -0.1763   11.4%                  <- also negative
#   one_percenters           0.1260    9.5%
#   spoils                   0.0737    3.9%                  <- 4% of "epv_spoil"
#   intercepts               0.0166    1.8%
#   frees_against            0.0428    1.3%
#   pressure_acts           -0.0024    1.1%
#
# SECTION 1 IS A CONFOUND CHECK AND IT GATES THE REST. A player accumulates
# defensive-half pressure acts when his team is under siege. If that weight is
# negative because it proxies "my team is losing territory" rather than anything
# the player did, then refitting against score margin will simply re-learn the
# same confound with a cleaner-looking number, and the honest fix is to drop or
# residualise the term rather than reprice it.
#
# Fitting level: team-match. The stats aggregate additively over a team's players,
# and score margin is the only target with points units. Weights are identified
# JOINTLY with the other three channels so they are conditional -- a univariate
# spoil-stat slope picks up everything it correlates with, which is how ws13's
# univariate 11.05 became 4.02 once the other channels were held fixed.

suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)

p <- torp:::default_epv_params()
SPOIL_STATS <- c(spoils = "spoil_wt", tackles = "tackle_wt",
                 pressure_acts = "pressure_wt", def_half_pressure_acts = "def_pressure_wt",
                 intercepts = "intercepts_wt", one_percenters = "one_percenters_wt",
                 rebound50s = "rebound50s_wt", frees_against = "frees_against_wt")
# Held fixed so the spoil weights come out conditional rather than univariate.
OTHER_CH <- c("epv_recv", "epv_disp", "epv_hitout")
# Territory / team-state proxies, for the confound check only -- never fitted as
# part of the channel.
TERRITORY <- c("inside50s", "rebound50s", "clangers", "turnovers", "metres_gained")

cli::cli_h1("0. build team-match aggregates")
pg <- as.data.table(load_player_game_data(TRUE))
res <- as.data.table(load_results(TRUE))
cli::cli_alert_info("{nrow(pg)} player-games, {nrow(res)} results")

agg_cols <- unique(c(names(SPOIL_STATS), OTHER_CH, TERRITORY))
missing <- setdiff(agg_cols, names(pg))
if (length(missing)) cli::cli_abort("Absent from player_game: {.val {missing}}")

tm <- pg[!is.na(team), lapply(.SD, function(x) sum(x, na.rm = TRUE)),
         by = .(match_id, season, round, team), .SDcols = agg_cols]

# Opponent join: each match has exactly two teams, so self-join on match_id and
# keep the rows where the teams differ. Guard the 2-team assumption rather than
# trusting it -- a 3-row match would silently produce duplicated pairs.
per_match <- tm[, .N, by = match_id]
if (any(per_match$N != 2L)) {
  cli::cli_alert_danger("{sum(per_match$N != 2)} match{?es} do not have exactly 2 team rows -- dropping them.")
  tm <- tm[match_id %in% per_match[N == 2L]$match_id]
}
pair <- merge(tm, tm, by = c("match_id", "season", "round"), allow.cartesian = TRUE)
pair <- pair[team.x != team.y]

# Score margin per (match, team). Built as a LONG frame rather than by resolving
# home/away orientation: .normalise_results_schema() drops the team names
# entirely (it keeps only match_id + scores), and player_game's `team` is
# canonicalised while raw results carry API names, so orientation logic here
# would be two chances to get a silent mismatch. A long join on (match_id, team)
# has neither, and the join rate below is asserted rather than assumed.
canon <- function(x) {
  y <- suppressWarnings(torp_team_full(x))
  data.table::fifelse(is.na(y), as.character(x), as.character(y))
}
sc <- rbind(
  res[, .(match_id, team = canon(home_team_name), score = home_score, opp_score = away_score)],
  res[, .(match_id, team = canon(away_team_name), score = away_score, opp_score = home_score)]
)
sc <- sc[is.finite(score) & is.finite(opp_score)]
pair[, team_c := canon(team.x)]
before <- nrow(pair)
pair[sc, `:=`(score = i.score, opp_score = i.opp_score), on = c("match_id", team_c = "team")]
matched <- pair[!is.na(score)]
rate <- nrow(matched) / before
cli::cli_alert_info("score join matched {nrow(matched)} of {before} team-match rows ({round(100*rate, 1)}%)")
if (rate < 0.95) {
  cli::cli_abort(c(
    "Only {round(100*rate, 1)}% of team-match rows matched a score.",
    "x" = "A silently unmatched team would drop whole clubs from the fit -- refusing to proceed."
  ))
}
pair <- matched
pair[, score_diff := score - opp_score]
# One row per (match, team) -- both orientations are kept deliberately: the diffs
# are antisymmetric so the fit is unchanged, but it keeps the frame balanced.
for (v in agg_cols) pair[, (paste0(v, "_d")) := get(paste0(v, ".x")) - get(paste0(v, ".y"))]
cli::cli_alert_info("{nrow(pair)} team-match rows, {uniqueN(pair$match_id)} matches, seasons {min(pair$season)}-{max(pair$season)}")

D <- function(v) paste0(v, "_d")

cli::cli_h1("1. CONFOUND CHECK -- is def_half_pressure_acts pricing team state?")
# If it is a territory proxy, its diff should correlate strongly with territory
# diffs, and its apparent effect should collapse once territory is controlled.
cm <- pair[, c(D("def_half_pressure_acts"), sapply(TERRITORY, D)), with = FALSE]
cm <- cm[complete.cases(cm)]
cr <- cor(cm)[1, -1]
print(round(cr, 3))
cli::cli_alert_info("|correlation| with territory proxies: max {round(max(abs(cr)), 3)}")

f_raw <- lm(reformulate(D("def_half_pressure_acts"), "score_diff"), data = pair)
f_ctl <- lm(reformulate(c(D("def_half_pressure_acts"), sapply(TERRITORY, D)), "score_diff"),
            data = pair)
b_raw <- coef(f_raw)[2]; b_ctl <- coef(f_ctl)[2]
cli::cli_alert_info("slope on def_half_pressure_acts_d: raw {round(b_raw, 4)} -> controlling for territory {round(b_ctl, 4)}")
if (abs(b_ctl) < 0.5 * abs(b_raw)) {
  cli::cli_alert_danger("CONFOUNDED: over half the apparent effect is territory, not the player.")
  cli::cli_alert_info("A refit against margin would re-learn this. Prefer dropping or residualising the term.")
} else {
  cli::cli_alert_success("Survives the territory control -- refitting it against margin is defensible.")
}

cli::cli_h1("2. SAME-MATCH fit -- DIAGNOSTIC ONLY, NOT a candidate weight set")
# Read this section as evidence about the confound, not as replacement weights.
#
# It regresses a match's margin on THAT MATCH's team stat diffs. Section 1 has
# just shown why that cannot produce rating weights: team state drives the stats
# as much as the stats drive the result. Every coefficient below carries the same
# contamination that turned def_half_pressure_acts from -0.511 into -0.008. The
# tell to look for is `intercepts` -- if it comes out strongly NEGATIVE, that is
# not "intercepting is bad", it is "the team intercepting more is the team
# defending more, which is the team losing".
#
# Kept because it demonstrates the instability rather than asserting it, and
# because the sign flips are informative about which terms are confound-driven.
# Section 4 is the fit that could actually produce weights.
rhs <- c(sapply(names(SPOIL_STATS), D), sapply(OTHER_CH, D))
fit <- lm(reformulate(rhs, "score_diff"), data = pair)
ci <- confint(fit)
out <- data.table(term = rownames(ci), fitted = round(coef(fit), 4),
                  lo = round(ci[, 1], 4), hi = round(ci[, 2], 4))
out <- out[term != "(Intercept)"]
out[, stat := sub("_d$", "", term)]
out[, current := sapply(stat, function(s) if (s %in% names(SPOIL_STATS)) p[[SPOIL_STATS[[s]]]] else NA_real_)]
out[, sign_flip := !is.na(current) & sign(current) != sign(fitted) & lo * hi > 0]
print(out[stat %in% names(SPOIL_STATS),
          .(stat, current, fitted, lo, hi, sign_flip)], row.names = FALSE)
cli::cli_alert_info("Channel controls (should be ~1 if the other channels are points-calibrated):")
print(out[stat %in% OTHER_CH, .(stat, fitted, lo, hi)], row.names = FALSE)
if (any(out$sign_flip, na.rm = TRUE)) {
  cli::cli_alert_danger("{sum(out$sign_flip, na.rm=TRUE)} weight{?s} have the OPPOSITE sign to production, significantly: {.val {out[sign_flip == TRUE]$stat}}")
}

cli::cli_h1("3. walk-forward on the SAME-MATCH target -- still diagnostic only")
# WARNING ABOUT HOW TO READ THIS. Out-of-sample here still means "predict this
# match's margin from this match's own stats", so a large improvement is expected
# and means nothing about rating quality: the refit has 8 free parameters against
# the production weighting's 1, aimed at a target a rating is not meant to hit.
# Do NOT quote the delta below as a gain. It is here to show that even a big
# same-match improvement does not license changing a weight.
seasons <- sort(unique(pair$season))
wf <- rbindlist(lapply(seasons[-1], function(s) {
  tr <- pair[season < s]; te <- pair[season == s]
  if (nrow(tr) < 200 || nrow(te) < 30) return(NULL)
  f <- lm(reformulate(rhs, "score_diff"), data = tr)
  pred_new <- predict(f, newdata = te)
  # Today's weighting, scored the same way: build the production spoil channel
  # diff, then give it one free global slope fitted on the SAME training data, so
  # the comparison is weighting-vs-weighting rather than weighting-vs-unscaled.
  mk_prod <- function(x) Reduce(`+`, lapply(names(SPOIL_STATS),
                                           function(s) x[[D(s)]] * p[[SPOIL_STATS[[s]]]]))
  tr2 <- copy(tr); te2 <- copy(te)
  tr2[, spoil_prod := mk_prod(tr2)]; te2[, spoil_prod := mk_prod(te2)]
  f0 <- lm(reformulate(c("spoil_prod", sapply(OTHER_CH, D)), "score_diff"), data = tr2)
  pred_old <- predict(f0, newdata = te2)
  data.table(season = s, n = nrow(te),
             mae_current = round(mean(abs(te$score_diff - pred_old)), 3),
             mae_refit   = round(mean(abs(te$score_diff - pred_new)), 3),
             delta       = round(mean(abs(te$score_diff - pred_new)) -
                                   mean(abs(te$score_diff - pred_old)), 3))
}))
print(wf, row.names = FALSE)
if (nrow(wf)) {
  cli::cli_alert_info("mean delta across seasons: {round(mean(wf$delta), 4)} (negative = refit better)")
  cli::cli_alert_info("seasons improved: {sum(wf$delta < 0)} of {nrow(wf)}")
}

cli::cli_h1("4. THE VALID FIT: pre-match defensive rates -> points conceded")
# This is §6.5 Test A's shape, which is the gate this channel has already been
# judged by once (it is how contextual spoil credit was shown to add nothing over
# the flat count). Every predictor is an EWMA over the team's PRIOR matches, so
# nothing from the match being predicted can leak in, which is what removes the
# reverse-causality that makes sections 2-3 unusable.
#
# Target is points CONCEDED, so a negative coefficient means the stat is
# associated with better defence -- and a rating weight is therefore -beta.
ALPHA <- 0.2   # ~5-game memory, one decay for every stat so none is advantaged
setorder(pair, team_c, season, round)

# Strictly-prior EWMA: seeded from the first observation and shifted, so row t
# only ever sees rows < t for that team.
prior_ewma <- function(x) {
  n <- length(x)
  out <- rep(NA_real_, n)
  if (n == 0) return(out)
  e <- NA_real_
  for (i in seq_len(n)) {
    out[i] <- e
    e <- if (is.na(e)) x[i] else ALPHA * x[i] + (1 - ALPHA) * e
  }
  out
}
EW <- function(v) paste0(v, "_ew")
for (v in names(SPOIL_STATS)) pair[, (EW(v)) := prior_ewma(get(paste0(v, ".x"))), by = team_c]
# Opponent attacking strength, same construction on the opponent's own scoring.
pair[, opp_scored_ew := prior_ewma(score), by = team_c]
opp_att <- pair[, .(match_id, team_c, opp_att = opp_scored_ew)]
pair[, team_y_c := canon(team.y)]
pair[opp_att, opp_att := i.opp_att, on = c("match_id", team_y_c = "team_c")]

fit_cols <- c(sapply(names(SPOIL_STATS), EW), "opp_att")
F4 <- pair[complete.cases(pair[, c(fit_cols, "opp_score"), with = FALSE])]
cli::cli_alert_info("{nrow(F4)} team-match rows with a full prior history")

f4 <- lm(reformulate(fit_cols, "opp_score"), data = F4)
c4 <- confint(f4)
w4 <- data.table(term = rownames(c4), beta = round(coef(f4), 4),
                 lo = round(c4[, 1], 4), hi = round(c4[, 2], 4))
w4 <- w4[term != "(Intercept)"]
w4[, stat := sub("_ew$", "", term)]
w4[, implied_weight := -beta]     # credit for REDUCING points conceded
w4[, current := sapply(stat, function(s) if (s %in% names(SPOIL_STATS)) p[[SPOIL_STATS[[s]]]] else NA_real_)]
w4[, significant := lo * hi > 0]
print(w4[stat %in% names(SPOIL_STATS),
         .(stat, current, implied_weight = round(implied_weight, 4),
           lo = round(-hi, 4), hi = round(-lo, 4), significant)], row.names = FALSE)
cli::cli_alert_info("opponent attack control: {round(w4[stat == 'opp_att']$beta, 3)} (expect POSITIVE -- a better attack scores more on you)")

cli::cli_h1("4b. does a refit beat production weights on THIS target, walk-forward?")
mk_prod_ew <- function(x) Reduce(`+`, lapply(names(SPOIL_STATS),
                                            function(s) x[[EW(s)]] * p[[SPOIL_STATS[[s]]]]))
F4[, spoil_prod_ew := mk_prod_ew(F4)]
wf4 <- rbindlist(lapply(sort(unique(F4$season))[-1], function(s) {
  tr <- F4[season < s]; te <- F4[season == s]
  if (nrow(tr) < 200 || nrow(te) < 30) return(NULL)
  f_new <- lm(reformulate(fit_cols, "opp_score"), data = tr)
  f_old <- lm(opp_score ~ spoil_prod_ew + opp_att, data = tr)
  data.table(season = s, n = nrow(te),
             mae_current = round(mean(abs(te$opp_score - predict(f_old, te))), 3),
             mae_refit   = round(mean(abs(te$opp_score - predict(f_new, te))), 3),
             delta = round(mean(abs(te$opp_score - predict(f_new, te))) -
                             mean(abs(te$opp_score - predict(f_old, te))), 3))
}))
print(wf4, row.names = FALSE)
if (nrow(wf4)) {
  cli::cli_alert_info("mean delta: {round(mean(wf4$delta), 4)} points conceded (negative = refit better)")
  cli::cli_alert_info("seasons improved: {sum(wf4$delta < 0)} of {nrow(wf4)}")
  if (mean(wf4$delta) < -0.5 && sum(wf4$delta < 0) >= nrow(wf4) - 1) {
    cli::cli_alert_success("Refit beats production weights on a leak-free forward target -- a real candidate.")
  } else {
    cli::cli_alert_danger("No reliable gain on the valid target. Production weights stand; do not reprice.")
  }
}

saveRDS(list(weights_same_match = out, walk_forward_same_match = wf,
             weights_forward = w4, walk_forward_forward = wf4,
             confound = list(raw = b_raw, controlled = b_ctl, cors = cr)),
        "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse/92e2b422-0dee-4727-90de-364d23375767/scratchpad/refit_spoil.rds")
cli::cli_alert_success("done")
