# Audit ALL box-score credit terms for team-state confounding
# ===========================================================
# refit_spoil_weights.R found that epv_spoil's dominant term,
# def_half_pressure_acts (41.8% of the channel, weight -0.1882), is a TERRITORY
# PROXY: its diff correlates -0.791 with inside50s diff, and its same-match slope
# on margin collapses -0.511 -> -0.008 once territory is controlled. A player is
# being debited for his team defending, not for anything he did.
#
# CLAUDE.md rule: when one entry of hand-maintained reference data is found wrong,
# audit the ENTIRE set -- one error in static data predicts siblings. There are 29
# box-score weights across the four EPV channels and only that one has been
# checked. This checks all of them, the same way.
#
# METHOD, and why it is this and not a refit. For each term:
#   1. how much of its channel it carries (mass -- an ugly confound on a 1% term
#      does not matter)
#   2. correlation of its team-match diff with territory
#   3. what fraction of its same-match margin effect SURVIVES controlling for
#      territory
# (3) is the confound test. It is deliberately NOT used to produce replacement
# weights -- a same-match fit cannot do that (see refit_spoil_weights.R §2-3, where
# it shows a fake -2.09 MAE gain). It is being used only to ask "is this term's
# apparent signal actually about the player?", which is what it can answer.
#
# The chain-EPV parts of recv/disp are excluded: they are expected-points deltas by
# construction, so they are anchored and not at issue.

suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)

p <- torp:::default_epv_params()

# Every box-score term, by channel, exactly as player_credit.R Step 4/5 applies them.
TERMS <- rbindlist(list(
  data.table(channel = "epv_spoil", stat = c("spoils", "tackles", "pressure_acts",
    "def_half_pressure_acts", "intercepts", "one_percenters", "rebound50s", "frees_against"),
    par = c("spoil_wt", "tackle_wt", "pressure_wt", "def_pressure_wt",
            "intercepts_wt", "one_percenters_wt", "rebound50s_wt", "frees_against_wt")),
  data.table(channel = "epv_hitout", stat = c("hitouts", "hitouts_to_advantage", "ruck_contests"),
    par = c("hitout_wt", "hitout_adv_wt", "ruck_contest_wt")),
  data.table(channel = "epv_disp", stat = c("inside50s", "clangers", "score_involvements",
    "kicks", "handballs", "metres_gained", "turnovers", "goal_assists", "goals",
    "behinds", "shots_at_goal"),
    par = c("inside50s_wt", "clangers_wt", "score_involvements_wt", "kicks_wt",
            "handballs_wt", "metres_gained_wt", "turnovers_wt", "goal_assists_wt",
            "goals_wt", "behinds_wt", "shots_at_goal_wt")),
  data.table(channel = "epv_recv", stat = c("contested_possessions", "contested_marks",
    "ground_ball_gets", "marks_inside50", "marks", "uncontested_possessions", "frees_for"),
    par = c("contested_poss_wt", "contested_marks_wt", "ground_ball_gets_wt",
            "marks_inside50_wt", "marks_wt", "uncontested_poss_wt", "frees_for_wt"))
))
TERMS[, weight := sapply(par, function(x) p[[x]])]

# Territory / team-state controls. inside50s and metres_gained are the direct
# territory measures; rebound50s and turnovers are included because a team pinned
# in defence generates both. NOTE inside50s and rebound50s are themselves credited
# terms -- when auditing those two, they are dropped from their own control set so
# a term is never "controlled for itself".
TERRITORY <- c("inside50s", "metres_gained", "rebound50s", "turnovers")

cli::cli_h1("build team-match diffs")
pg <- as.data.table(load_player_game_data(TRUE))
res <- as.data.table(load_results(TRUE))
agg <- unique(c(TERMS$stat, TERRITORY))
stopifnot(all(agg %in% names(pg)))

tm <- pg[!is.na(team), lapply(.SD, function(x) sum(x, na.rm = TRUE)),
         by = .(match_id, season, round, team), .SDcols = agg]
per <- tm[, .N, by = match_id]
tm <- tm[match_id %in% per[N == 2L]$match_id]
pair <- merge(tm, tm, by = c("match_id", "season", "round"), allow.cartesian = TRUE)
pair <- pair[team.x != team.y]

canon <- function(x) {
  y <- suppressWarnings(torp_team_full(x))
  data.table::fifelse(is.na(y), as.character(x), as.character(y))
}
sc <- rbind(
  res[, .(match_id, team = canon(home_team_name), score = home_score, opp_score = away_score)],
  res[, .(match_id, team = canon(away_team_name), score = away_score, opp_score = home_score)]
)[is.finite(score) & is.finite(opp_score)]
pair[, team_c := canon(team.x)]
n0 <- nrow(pair)
pair[sc, `:=`(score = i.score, opp_score = i.opp_score), on = c("match_id", team_c = "team")]
pair <- pair[!is.na(score)]
if (nrow(pair) / n0 < 0.95) cli::cli_abort("Score join matched only {round(100*nrow(pair)/n0,1)}% -- refusing to audit on a partial frame.")
pair[, score_diff := score - opp_score]
for (v in agg) pair[, (paste0(v, "_d")) := get(paste0(v, ".x")) - get(paste0(v, ".y"))]
cli::cli_alert_info("{nrow(pair)} team-match rows, {uniqueN(pair$match_id)} matches")

D <- function(v) paste0(v, "_d")

# Channel mass, so a confound can be weighted by whether it matters.
mass <- rbindlist(lapply(split(TERMS, TERMS$channel), function(g) {
  contrib <- sapply(seq_len(nrow(g)), function(i) mean(abs(pg[[g$stat[i]]] * g$weight[i]), na.rm = TRUE))
  data.table(channel = g$channel, stat = g$stat, weight = g$weight,
             mean_abs = contrib, share = contrib / sum(contrib))
}))

cli::cli_h1("A. TEAM-CONTEXT SHARE -- the actual test")
# WHY NOT THE MARGIN-CONTROL TEST. The first version of this script asked "what
# fraction of each term's same-match margin effect survives controlling for
# territory?" and flagged 26 of 29 terms. That is the audit failing, not the data:
# inside50s and metres_gained are near-proxies for WHO WON, so conditioning on them
# absorbs the legitimate path as well as any confound and every coefficient
# collapses. An audit that flags everything discriminates nothing. Kept below as
# section B, explicitly demoted, because the mistake is instructive.
#
# The right question never involved margin. It is: does this stat vary because of
# the PLAYER, or because of the situation his whole team was in? A pure team-context
# quantity moves together for all 22 players in a match -- when the team is pinned
# in defence, everyone's defensive-half pressure acts rise at once. A genuine
# individual quantity varies between team-mates within the same match.
#
# So: share of player-game variance sitting BETWEEN team-matches rather than within
# them. Residualised on time-on-ground first, because more minutes means more of
# everything and that is not team context.
pg[, tog_safe := pmax(dplyr::coalesce(time_on_ground_percentage / 100, 0.1), 0.1)]
pgv <- pg[!is.na(team) & !is.na(match_id)]

team_share <- rbindlist(lapply(TERMS$stat, function(st) {
  y <- pgv[[st]]
  ok <- is.finite(y) & is.finite(pgv$tog_safe)
  d <- data.table(y = y[ok], tog = pgv$tog_safe[ok],
                  grp = paste(pgv$match_id[ok], pgv$team[ok]))
  if (nrow(d) < 100 || stats::sd(d$y) < 1e-9) {
    return(data.table(stat = st, team_share = NA_real_, n = nrow(d)))
  }
  d[, r := stats::residuals(stats::lm(y ~ tog))]
  gm <- d[, .(m = mean(r), n = .N), by = grp]
  grand <- mean(d$r)
  ss_between <- sum(gm$n * (gm$m - grand)^2)
  ss_total <- sum((d$r - grand)^2)
  data.table(stat = st, team_share = round(ss_between / ss_total, 3), n = nrow(d))
}))
A <- merge(TERMS[, .(channel, stat, weight)], team_share, by = "stat")
A[mass, share := i.share, on = c("channel", "stat")]
A[, context_mass := round(team_share * share, 4)]
setorder(A, -context_mass)
print(A[, .(channel, stat, weight, chan_share = round(share, 3), team_share, context_mass)],
      row.names = FALSE)
cli::cli_alert_info("team_share ~1 means the stat is a property of the team's situation that match;")
cli::cli_alert_info("~0 means it distinguishes team-mates. context_mass weights it by channel mass.")
ch_ctx <- A[, .(context_mass = round(sum(context_mass, na.rm = TRUE), 3),
                wtd_team_share = round(sum(team_share * share, na.rm = TRUE), 3)), by = channel][order(-context_mass)]
print(ch_ctx, row.names = FALSE)

cli::cli_h1("B. DEMOTED: margin-control test (flags 26 of 29 -- over-controlled)")
audit <- rbindlist(lapply(seq_len(nrow(TERMS)), function(i) {
  st <- TERMS$stat[i]
  ctl <- setdiff(TERRITORY, st)          # never control a term for itself
  f_raw <- lm(reformulate(D(st), "score_diff"), data = pair)
  f_ctl <- lm(reformulate(c(D(st), sapply(ctl, D)), "score_diff"), data = pair)
  b_raw <- coef(f_raw)[2]; b_ctl <- coef(f_ctl)[2]
  cors <- sapply(ctl, function(cv) cor(pair[[D(st)]], pair[[D(cv)]], use = "complete.obs"))
  data.table(channel = TERMS$channel[i], stat = st, weight = TERMS$weight[i],
             max_terr_cor = round(max(abs(cors)), 3),
             slope_raw = round(b_raw, 4), slope_ctl = round(b_ctl, 4),
             survives = round(ifelse(abs(b_raw) < 1e-9, NA_real_, abs(b_ctl) / abs(b_raw)), 3),
             sign_held = sign(b_raw) == sign(b_ctl))
}))
audit[mass, share := i.share, on = c("channel", "stat")]
audit[, weighted_risk := round((1 - pmin(survives, 1)) * share, 4)]

# Ranked by how much the channel actually leans on a confounded term, which is the
# only ordering that says where to spend effort.
setorder(audit, -weighted_risk)
print(audit[, .(channel, stat, weight, share = round(share, 3), max_terr_cor,
                slope_raw, slope_ctl, survives, sign_held, weighted_risk)],
      row.names = FALSE)

bad <- audit[!is.na(survives) & survives < 0.5]
cli::cli_alert_warning("{nrow(bad)} of {nrow(audit)} terms 'fail' this test -- which is why it is not used.")
cli::cli_alert_info("Only the max_terr_cor column above is a fact about the data rather than an artefact")
cli::cli_alert_info("of over-controlling; read section A for the verdict.")

cli::cli_h1("verdict (from section A)")
flag <- A[!is.na(team_share) & team_share > 0.5]
cli::cli_alert_info("{nrow(flag)} of {nrow(A)} terms are majority team-context: {.val {flag$stat}}")
worst <- A[1]
cli::cli_alert_info("Highest context_mass: {worst$stat} ({worst$channel}) -- {round(100*worst$share,1)}% of its channel, team_share {worst$team_share}")
cli::cli_alert_info("A term with a large channel share AND a high team_share is a CONTENT defect:")
cli::cli_alert_info("rescaling or repricing cannot help, because its variation is not about the player.")

saveRDS(list(team_context = A, margin_control_demoted = audit),
        "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse/92e2b422-0dee-4727-90de-364d23375767/scratchpad/box_score_confound_audit.rds")
cli::cli_alert_success("done")
