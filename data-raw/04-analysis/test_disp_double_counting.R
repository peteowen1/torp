# Is epv_disp over-weighted because it counts the same disposals twice?
# =====================================================================
# ws13 measured epv_disp converting at 0.569 points -- OVER-weighted, and in the
# opposite direction to the unanchored channels (spoil 4.02, hitout 7.22). A
# different direction needs a different explanation, so "no EPV anchor" cannot be it:
# disp is 30.6% chain-EPV.
#
# The hypothesis: the chain part already prices every disposal at its actual
# expected-points value (`0.5 x sum(delta_epv)` over the player's disposals), and
# then eleven box-score terms add credit for THE SAME disposals -- `kicks`,
# `handballs`, `metres_gained`, `score_involvements` are all just the disposals
# again, at a flat rate. If so, disp is not mis-scaled, it is double-paid, and the
# fix is removing redundancy rather than dividing by 1.76.
#
# This matters more than the other three channels put together: epv_disp carries
# 62% of epr_diff's spread, so it is the channel that actually moves ratings.
#
# Three tests, in increasing strictness:
#   A. how much of the box block is VOLUME (the disposals again) vs OUTCOME (goals,
#      turnovers -- things the chain delta may genuinely not capture)
#   B. redundancy: how much of the box block is predictable from the chain part
#   C. the valid one: on a leak-free forward target, does the box block add anything
#      once the chain part is present?

suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)

p <- torp:::default_epv_params()
DISP_BOX <- c(inside50s = "inside50s_wt", clangers = "clangers_wt",
              score_involvements = "score_involvements_wt", kicks = "kicks_wt",
              handballs = "handballs_wt", metres_gained = "metres_gained_wt",
              turnovers = "turnovers_wt", goal_assists = "goal_assists_wt",
              goals = "goals_wt", behinds = "behinds_wt",
              shots_at_goal = "shots_at_goal_wt")
# VOLUME = a restatement of the disposals the chain part already priced.
# OUTCOME = terminal events the chain delta may not fully carry.
VOLUME <- c("kicks", "handballs", "metres_gained", "score_involvements", "inside50s")
OUTCOME <- setdiff(names(DISP_BOX), VOLUME)

d <- as.data.table(load_player_game_data(TRUE))
stopifnot(all(names(DISP_BOX) %in% names(d)))
d[, box_part := Reduce(`+`, lapply(names(DISP_BOX), function(s) get(s) * p[[DISP_BOX[[s]]]]))]
d[, chain_part := epv_disp - box_part]
d[, vol_part := Reduce(`+`, lapply(VOLUME, function(s) get(s) * p[[DISP_BOX[[s]]]]))]
d[, out_part := Reduce(`+`, lapply(OUTCOME, function(s) get(s) * p[[DISP_BOX[[s]]]]))]

cli::cli_h1("A. volume vs outcome inside the box block")
tot <- function(x) mean(abs(x), na.rm = TRUE)
cli::cli_alert_info("mean |chain_part| {round(tot(d$chain_part),3)} | |box_part| {round(tot(d$box_part),3)} | |epv_disp| {round(tot(d$epv_disp),3)}")
cli::cli_alert_info("of the box block: VOLUME {round(100*tot(d$vol_part)/(tot(d$vol_part)+tot(d$out_part)),1)}%, OUTCOME {round(100*tot(d$out_part)/(tot(d$vol_part)+tot(d$out_part)),1)}%")
cli::cli_alert_info("VOLUME terms: {paste(VOLUME, collapse=', ')}")
# The plainest form of the double count: kicks + handballs IS disposals, so those
# two terms together are a flat bonus per disposal on top of its measured EPV.
d[, disposals_box := kicks + handballs]
flat <- (p$kicks_wt * mean(d$kicks, na.rm = TRUE) + p$handballs_wt * mean(d$handballs, na.rm = TRUE)) /
  mean(d$disposals_box, na.rm = TRUE)
cli::cli_alert_info("kicks + handballs = disposals, so those two terms alone are a FLAT {round(flat,4)} points per disposal,")
cli::cli_alert_info("paid on top of that disposal's own measured chain EPV.")

cli::cli_h1("B. redundancy: how much of the box block does the chain part explain?")
ok <- is.finite(d$chain_part) & is.finite(d$box_part)
r2 <- summary(lm(box_part ~ chain_part, data = d[ok]))$r.squared
r2v <- summary(lm(vol_part ~ chain_part, data = d[ok]))$r.squared
r2o <- summary(lm(out_part ~ chain_part, data = d[ok]))$r.squared
cli::cli_alert_info("R^2 of box_part on chain_part: {round(r2,3)}   (VOLUME {round(r2v,3)}, OUTCOME {round(r2o,3)})")
cli::cli_alert_info("cor(chain_part, box_part) = {round(cor(d$chain_part[ok], d$box_part[ok]),3)}")
cli::cli_alert_info("High R^2 on VOLUME and lower on OUTCOME is the double-count signature.")

cli::cli_h1("C. the valid test: does the box block add anything FORWARD?")
# Same construction as refit_spoil_weights.R §4, which is the only shape that
# avoids reverse causality: predictors are EWMAs over the team's PRIOR matches.
# Offensive analogue -- target is points SCORED, control is opponent defensive
# strength.
res <- as.data.table(load_results(TRUE))
tm <- d[!is.na(team), .(chain = sum(chain_part, na.rm = TRUE),
                        box = sum(box_part, na.rm = TRUE),
                        vol = sum(vol_part, na.rm = TRUE),
                        out = sum(out_part, na.rm = TRUE)),
        by = .(match_id, season, round, team)]
per <- tm[, .N, by = match_id]; tm <- tm[match_id %in% per[N == 2L]$match_id]
canon <- function(x) { y <- suppressWarnings(torp_team_full(x)); fifelse(is.na(y), as.character(x), as.character(y)) }
sc <- rbind(
  res[, .(match_id, team = canon(home_team_name), score = home_score, opp_score = away_score)],
  res[, .(match_id, team = canon(away_team_name), score = away_score, opp_score = home_score)]
)[is.finite(score) & is.finite(opp_score)]
tm[, team_c := canon(team)]
n0 <- nrow(tm)
tm[sc, `:=`(score = i.score, opp_score = i.opp_score), on = c("match_id", team_c = "team")]
tm <- tm[!is.na(score)]
if (nrow(tm) / n0 < 0.95) cli::cli_abort("Score join matched {round(100*nrow(tm)/n0,1)}% -- refusing to test on a partial frame.")

ALPHA <- 0.2
prior_ewma <- function(x) {
  out <- rep(NA_real_, length(x)); e <- NA_real_
  for (i in seq_along(x)) { out[i] <- e; e <- if (is.na(e)) x[i] else ALPHA * x[i] + (1 - ALPHA) * e }
  out
}
setorder(tm, team_c, season, round)
for (v in c("chain", "box", "vol", "out")) tm[, (paste0(v, "_ew")) := prior_ewma(get(v)), by = team_c]
tm[, conceded_ew := prior_ewma(opp_score), by = team_c]
# Opponent defensive strength = how much the opponent has been conceding.
opp <- tm[, .(match_id, team_c, opp_def = conceded_ew)]
tm2 <- merge(tm, tm[, .(match_id, other = team_c)], by = "match_id", allow.cartesian = TRUE)[team_c != other]
tm2[opp, opp_def := i.opp_def, on = c("match_id", other = "team_c")]
F <- tm2[complete.cases(tm2[, .(chain_ew, box_ew, vol_ew, out_ew, opp_def, score)])]
cli::cli_alert_info("{nrow(F)} team-match rows with full prior history")

m_chain <- lm(score ~ chain_ew + opp_def, data = F)
m_both  <- lm(score ~ chain_ew + box_ew + opp_def, data = F)
m_split <- lm(score ~ chain_ew + vol_ew + out_ew + opp_def, data = F)
cli::cli_alert_info("adj R^2  chain only {round(summary(m_chain)$adj.r.squared,4)} | + box {round(summary(m_both)$adj.r.squared,4)} | + vol/out split {round(summary(m_split)$adj.r.squared,4)}")
an <- anova(m_chain, m_both)
cli::cli_alert_info("does box_ew add over chain_ew? F = {round(an[['F']][2],2)}, p = {signif(an[['Pr(>F)']][2],3)}")
print(round(summary(m_split)$coefficients, 4))

cli::cli_h1("C2. walk-forward")
wf <- rbindlist(lapply(sort(unique(F$season))[-1], function(s) {
  tr <- F[season < s]; te <- F[season == s]
  if (nrow(tr) < 200 || nrow(te) < 30) return(NULL)
  f1 <- lm(score ~ chain_ew + opp_def, data = tr)
  f2 <- lm(score ~ chain_ew + box_ew + opp_def, data = tr)
  data.table(season = s, n = nrow(te),
             mae_chain_only = round(mean(abs(te$score - predict(f1, te))), 3),
             mae_with_box   = round(mean(abs(te$score - predict(f2, te))), 3),
             delta = round(mean(abs(te$score - predict(f2, te))) -
                             mean(abs(te$score - predict(f1, te))), 3))
}))
print(wf, row.names = FALSE)
if (nrow(wf)) {
  cli::cli_alert_info("mean delta {round(mean(wf$delta),4)} (negative = box block adds value); improved {sum(wf$delta<0)} of {nrow(wf)}")
  if (mean(wf$delta) > -0.05) {
    cli::cli_alert_danger("The box block adds nothing forward beyond the chain part -- consistent with double counting.")
  } else {
    cli::cli_alert_success("The box block DOES add forward value -- it is not merely a restatement of the chain part.")
  }
}

saveRDS(list(walk_forward = wf, r2 = c(box = r2, vol = r2v, out = r2o), flat_per_disposal = flat),
        "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse/92e2b422-0dee-4727-90de-364d23375767/scratchpad/disp_double_count.rds")
cli::cli_alert_success("done")
