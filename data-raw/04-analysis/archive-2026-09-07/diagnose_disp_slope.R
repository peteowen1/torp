# Why does epv_disp convert at 0.569?
# ===================================
# Established so far: it is NOT redundancy between the chain part and the box block
# (R^2 0.072, and the box block adds forward value 5 of 5 seasons). And the channel
# matters more than the other three combined -- 62% of epr_diff's spread.
#
# Three questions here, cheapest first:
#
#  1. SLOPE DECOMPOSITION. epv_disp = chain_part (0.5 x sum delta_epv over the
#     player's disposals -- in points BY CONSTRUCTION) + box_part (11 unpriced
#     add-ons, 2.26x the chain part's mass). If the chain part alone converts near 1
#     and the full channel at 0.569, the box add-ons are the inflation and the
#     diagnosis is settled. Run the same decomposition on epv_recv as a control: it
#     is 65% chain and converts at 1.03, so its box block should inflate it less.
#
#  2. STABILITY. The per-season UNIVARIATE disp slope ran 0.715-1.805, a 2.5x wander.
#     If the multivariate 0.569 is equally unstable, it is not a quantity worth
#     explaining. 2021 is thin (95 matches) and was already excluded from the totals
#     calibration for that reason, so check with and without it.
#
#  3. behinds_wt 1.0899 vs goals_wt 0.4262 -- flagged twice and never checked.
#     Box-wise a behind earns MORE than a goal (behinds + shots = 1.532 against
#     goals + shots = 0.868). The chain part is supposed to more than compensate,
#     because a goal's delta_epv is far larger. Measure it instead of assuming it.

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
              goals = "goals_wt", behinds = "behinds_wt", shots_at_goal = "shots_at_goal_wt")
RECV_BOX <- c(contested_possessions = "contested_poss_wt", contested_marks = "contested_marks_wt",
              ground_ball_gets = "ground_ball_gets_wt", marks_inside50 = "marks_inside50_wt",
              marks = "marks_wt", uncontested_possessions = "uncontested_poss_wt",
              frees_for = "frees_for_wt")

d <- as.data.table(load_player_game_data(TRUE))
res <- as.data.table(load_results(TRUE))
d[, disp_box   := Reduce(`+`, lapply(names(DISP_BOX), function(s) get(s) * p[[DISP_BOX[[s]]]]))]
d[, disp_chain := epv_disp - disp_box]
# recv also carries contest_epv, which is chain-derived -- keep it on the chain side.
d[, recv_box   := Reduce(`+`, lapply(names(RECV_BOX), function(s) get(s) * p[[RECV_BOX[[s]]]]))]
d[, recv_chain := epv_recv - recv_box]

cli::cli_h1("0. team-match frame")
PARTS <- c("disp_chain", "disp_box", "recv_chain", "recv_box", "epv_spoil", "epv_hitout")
tm <- d[!is.na(team), lapply(.SD, function(x) sum(x, na.rm = TRUE)),
        by = .(match_id, season, round, team), .SDcols = PARTS]
per <- tm[, .N, by = match_id]; tm <- tm[match_id %in% per[N == 2L]$match_id]
canon <- function(x) { y <- suppressWarnings(torp_team_full(x)); fifelse(is.na(y), as.character(x), as.character(y)) }
pair <- merge(tm, tm, by = c("match_id", "season", "round"), allow.cartesian = TRUE)[team.x != team.y]
sc <- rbind(
  res[, .(match_id, team = canon(home_team_name), score = home_score, opp = away_score)],
  res[, .(match_id, team = canon(away_team_name), score = away_score, opp = home_score)]
)[is.finite(score) & is.finite(opp)]
pair[, team_c := canon(team.x)]
n0 <- nrow(pair)
pair[sc, `:=`(score = i.score, opp = i.opp), on = c("match_id", team_c = "team")]
pair <- pair[!is.na(score)]
if (nrow(pair) / n0 < 0.95) cli::cli_abort("Score join matched {round(100*nrow(pair)/n0,1)}% -- refusing to proceed.")
pair[, score_diff := score - opp]
for (v in PARTS) pair[, (paste0(v, "_d")) := get(paste0(v, ".x")) - get(paste0(v, ".y"))]
# One row per match, matching ws13's convention (mirrored team rows double n
# without adding information and would shrink every CI spuriously).
pair <- unique(pair, by = "match_id")
cli::cli_alert_info("{nrow(pair)} matches, seasons {min(pair$season)}-{max(pair$season)}")

D <- function(v) paste0(v, "_d")
rhs_split <- sapply(PARTS, D)

cli::cli_h1("1. SLOPE DECOMPOSITION -- chain part vs box part, held jointly")
# READ THIS BEFORE THE NUMBERS. These slopes are on SAME-MATCH team EPV sums.
# ws13's 0.569 is on `epr_disp_diff` -- an ACCUMULATED RATING difference. They are
# different quantities and the numbers below do NOT decompose ws13's slope, which is
# what this section was originally written to do.
#
# Worse, a same-match regression is the shape already established as invalid here
# (refit_spoil_weights.R §2-3): team state drives the stats as much as the stats
# drive the result. The tell is epv_spoil coming out NEGATIVE below -- a team's own
# spoil sum "predicting" a worse margin is the territory confound, not a price.
#
# Kept, labelled, because the parts' RELATIVE behaviour and the temporal trend in
# section 2 are still worth seeing. To genuinely decompose ws13's slope the chain
# and box components would have to be pushed through the EPR aggregation
# separately -- build_ratings_history()-scale work, not a regression.
f <- lm(reformulate(rhs_split, "score_diff"), data = pair)
ci <- confint(f)
tab <- data.table(part = rownames(ci), slope = round(coef(f), 3),
                  lo = round(ci[, 1], 3), hi = round(ci[, 2], 3))[part != "(Intercept)"]
tab[, hits_one := lo <= 1 & hi >= 1]
print(tab, row.names = FALSE)
cli::cli_alert_info("PREDICTION being tested: disp_chain ~1 (it is points by construction),")
cli::cli_alert_info("disp_box well below 1 (unpriced add-ons inflating the channel).")

# Mass, so the slopes can be read against how much each part contributes.
cli::cli_alert_info("mean |value| per player-game: disp_chain {round(mean(abs(d$disp_chain),na.rm=TRUE),3)}, disp_box {round(mean(abs(d$disp_box),na.rm=TRUE),3)}, recv_chain {round(mean(abs(d$recv_chain),na.rm=TRUE),3)}, recv_box {round(mean(abs(d$recv_box),na.rm=TRUE),3)}")

cli::cli_h1("2. STABILITY of the multivariate disp slope")
# Full four-channel model, as ws13 fitted it, refitted per season.
CH <- c("epv_recv", "epv_disp", "epv_spoil", "epv_hitout")
d[, epv_recv_t := epv_recv]; d[, epv_disp_t := epv_disp]
tm2 <- d[!is.na(team), lapply(.SD, function(x) sum(x, na.rm = TRUE)),
         by = .(match_id, season, round, team), .SDcols = CH]
tm2 <- tm2[match_id %in% per[N == 2L]$match_id]
p2 <- merge(tm2, tm2, by = c("match_id", "season", "round"), allow.cartesian = TRUE)[team.x != team.y]
p2[, team_c := canon(team.x)]
p2[sc, `:=`(score = i.score, opp = i.opp), on = c("match_id", team_c = "team")]
p2 <- unique(p2[!is.na(score)], by = "match_id")
p2[, score_diff := score - opp]
for (v in CH) p2[, (paste0(v, "_d")) := get(paste0(v, ".x")) - get(paste0(v, ".y"))]
rhs_ch <- sapply(CH, D)
seas <- rbindlist(lapply(c(list(all = sort(unique(p2$season))),
                          list(ex2021 = setdiff(sort(unique(p2$season)), 2021)),
                          lapply(sort(unique(p2$season)), function(s) s)), function(ss) {
  nm <- if (length(ss) > 1) paste0(min(ss), "-", max(ss)) else as.character(ss)
  sub <- p2[season %in% ss]
  if (nrow(sub) < 60) return(NULL)
  ff <- lm(reformulate(rhs_ch, "score_diff"), data = sub)
  cc <- confint(ff)
  data.table(window = nm, n = nrow(sub),
             disp = round(coef(ff)[D("epv_disp")], 3),
             lo = round(cc[D("epv_disp"), 1], 3), hi = round(cc[D("epv_disp"), 2], 3),
             recv = round(coef(ff)[D("epv_recv")], 3))
}))
seas[1, window := "ALL (ws13's fit)"]
seas[2, window := "excl 2021"]
print(seas, row.names = FALSE)
rng <- seas[!window %in% c("ALL (ws13's fit)", "excl 2021")]
if (nrow(rng)) {
  cli::cli_alert_info("per-season disp slope range {round(min(rng$disp),3)} to {round(max(rng$disp),3)} ({round(max(rng$disp)/min(rng$disp),2)}x)")
  cli::cli_alert_info("seasons whose CI contains 1: {sum(rng$lo <= 1 & rng$hi >= 1)} of {nrow(rng)}")
}

cli::cli_h1("3. goal vs behind -- what does each ACTUALLY earn in epv_disp?")
# Box-side is arithmetic; the chain side needs the real delta_epv of scoring shots.
box_goal   <- p$goals_wt + p$shots_at_goal_wt
box_behind <- p$behinds_wt + p$shots_at_goal_wt
cli::cli_alert_info("box-side only: goal {round(box_goal,3)} vs behind {round(box_behind,3)} -- a behind earns {round(box_behind-box_goal,3)} MORE")
pbp <- tryCatch(as.data.table(load_pbp(2025:2026, rounds = TRUE)), error = function(e) NULL)
# There are no "Goal"/"Behind" description rows -- a score is a Kick row carrying
# shot flags (checked against the description census: Kick, Handball, Marks, etc).
# Classify on points_shot, which is 6 for a goal and 1 for a behind.
if (is.null(pbp) || !all(c("delta_epv", "points_shot") %in% names(pbp))) {
  cli::cli_alert_warning("Could not load pbp with delta_epv/points_shot -- chain side of the goal/behind check not measured.")
} else {
  shots <- pbp[is.finite(delta_epv) & points_shot %in% c(1, 6)]
  shots[, description := ifelse(points_shot == 6, "Goal", "Behind")]
  gb <- shots[, .(n = .N, mean_delta_epv = round(mean(delta_epv), 3)), by = description]
  gb[, chain_credit := round(mean_delta_epv * p$disp_scale, 3)]
  gb[, box_credit := ifelse(description == "Goal", box_goal, box_behind)]
  gb[, total := round(chain_credit + box_credit, 3)]
  print(gb, row.names = FALSE)
  g <- gb[description == "Goal"]$total; b <- gb[description == "Behind"]$total
  if (length(g) && length(b)) {
    if (g > b) {
      cli::cli_alert_success("Total credit: goal {g} > behind {b}. The chain part compensates -- behinds_wt is not a defect.")
    } else {
      cli::cli_alert_danger("Total credit: goal {g} <= behind {b}. A behind is worth AS MUCH AS OR MORE THAN a goal -- real defect.")
    }
  }
}

saveRDS(list(decomposition = tab, stability = seas),
        "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse/92e2b422-0dee-4727-90de-364d23375767/scratchpad/disp_slope.rds")
cli::cli_alert_success("done")
