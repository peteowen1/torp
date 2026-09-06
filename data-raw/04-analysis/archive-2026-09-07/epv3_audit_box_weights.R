# Audit the 30 box-score weights that are LIVE in production (v2).
#
# The trigger: EPV_BEHINDS_WT = 1.0899 against EPV_GOALS_WT = 0.4262. A behind
# worth 2.5x a goal is not a football fact -- a goal is 6 points and a behind is
# 1. It is the signature of `goals`, `behinds` and `shots_at_goal` being fitted
# together without regularisation, where the three are near-collinear by
# construction (shots ~= goals + behinds + misses) and the fit can trade huge
# offsetting coefficients between them at no cost to its own error.
#
# If that is what happened, several of the 30 weights are unreliable, and they
# are multiplying real player counts in the metric served today.
#
# PRE-REGISTERED ANCHORS, written before looking (stats-discipline rule 1):
#   A1  goal coefficient > behind coefficient        (6 points against 1)
#   A2  goal coefficient > 0
#   A3  turnovers and clangers coefficients < 0
# A failure means the METHOD is wrong, not that football is surprising.
#
# TARGET: team-summed stat differences against match margin. That asks the
# directly interpretable question -- how many points of margin is one unit of
# this stat worth -- and it matches the "1 point = 1 point" aim.
#
# PERFORMANCE: 30 predictors on 1,241 matches. Seconds. Nothing to optimise.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_box_weight_audit.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 60) for (l in capture.output(print(utils::head(x, n)))) say(l)

# The live weights, by the channel each feeds.
LIVE <- rbindlist(list(
  data.table(stat = "goals",                  wt = EPV_GOALS_WT,              channel = "disp"),
  data.table(stat = "behinds",                wt = EPV_BEHINDS_WT,            channel = "disp"),
  data.table(stat = "shots_at_goal",          wt = EPV_SHOTS_AT_GOAL_WT,      channel = "disp"),
  data.table(stat = "inside50s",              wt = EPV_INSIDE50S_WT,          channel = "disp"),
  data.table(stat = "score_involvements",     wt = EPV_SCORE_INVOLVEMENTS_WT, channel = "disp"),
  data.table(stat = "kicks",                  wt = EPV_KICKS_WT,              channel = "disp"),
  data.table(stat = "handballs",              wt = EPV_HANDBALLS_WT,          channel = "disp"),
  data.table(stat = "metres_gained",          wt = EPV_METRES_GAINED_WT,      channel = "disp"),
  data.table(stat = "goal_assists",           wt = EPV_GOAL_ASSISTS_WT,       channel = "disp"),
  data.table(stat = "turnovers",              wt = EPV_TURNOVERS_WT,          channel = "disp"),
  data.table(stat = "clangers",               wt = EPV_CLANGERS_WT,           channel = "disp"),
  data.table(stat = "contested_possessions",  wt = EPV_CONTESTED_POSS_WT,     channel = "recv"),
  data.table(stat = "contested_marks",        wt = EPV_CONTESTED_MARKS_WT,    channel = "recv"),
  data.table(stat = "ground_ball_gets",       wt = EPV_GROUND_BALL_GETS_WT,   channel = "recv"),
  data.table(stat = "marks_inside50",         wt = EPV_MARKS_INSIDE50_WT,     channel = "recv"),
  data.table(stat = "marks",                  wt = EPV_MARKS_WT,              channel = "recv"),
  data.table(stat = "uncontested_possessions", wt = EPV_UNCONTESTED_POSS_WT,  channel = "recv"),
  data.table(stat = "frees_for",              wt = EPV_FREES_FOR_WT,          channel = "recv"),
  data.table(stat = "spoils",                 wt = EPV_SPOIL_WT,              channel = "spoil"),
  data.table(stat = "tackles",                wt = EPV_TACKLE_WT,             channel = "spoil"),
  data.table(stat = "pressure_acts",          wt = EPV_PRESSURE_WT,           channel = "spoil"),
  data.table(stat = "def_half_pressure_acts", wt = EPV_DEF_PRESSURE_WT,       channel = "spoil"),
  data.table(stat = "intercepts",             wt = EPV_INTERCEPTS_WT,         channel = "spoil"),
  data.table(stat = "one_percenters",         wt = EPV_ONE_PERCENTERS_WT,     channel = "spoil"),
  data.table(stat = "rebound50s",             wt = EPV_REBOUND50S_WT,         channel = "spoil"),
  data.table(stat = "frees_against",          wt = EPV_FREES_AGAINST_WT,      channel = "spoil"),
  data.table(stat = "hitouts",                wt = EPV_HITOUT_WT,             channel = "hitout"),
  data.table(stat = "hitouts_to_advantage",   wt = EPV_HITOUT_ADV_WT,         channel = "hitout"),
  data.table(stat = "ruck_contests",          wt = EPV_RUCK_CONTEST_WT,       channel = "hitout")
))

say("=== Audit of the 30 live box-score weights ===")
say("live weights: ", nrow(LIVE))

ps <- as.data.table(load_player_stats(TRUE))
res <- as.data.table(load_results(TRUE))
STATS <- LIVE$stat[LIVE$stat %in% names(ps)]
say("stats present in player_stats: ", length(STATS), " of ", nrow(LIVE))
missing <- setdiff(LIVE$stat, names(ps))
if (length(missing)) say("MISSING: ", paste(missing, collapse = ", "))

# Team-match sums, then home-minus-away differences. player_stats carries
# `team_status` (home/away) rather than a team name, which is better here --
# it sidesteps team-name canonicalisation entirely (Footscray vs Western
# Bulldogs, GWS vs GWS Giants) and cannot mis-join.
say("team_status values: ", paste(sort(unique(ps$team_status)), collapse = ", "))
tm <- ps[!is.na(team_status),
         lapply(.SD, function(v) sum(as.numeric(v), na.rm = TRUE)),
         .SDcols = STATS, by = .(match_id = as.character(match_id), team_status)]
r <- res[, .(match_id = as.character(match_id),
             margin = home_score - away_score)][is.finite(margin)]
h <- tm[team_status == "home"]
a <- tm[team_status == "away"]
m <- merge(h[, c("match_id", STATS), with = FALSE],
           a[, c("match_id", STATS), with = FALSE], by = "match_id",
           suffixes = c("_h", "_a"))
m <- merge(m, r, by = "match_id")
for (s in STATS) m[, (paste0("d_", s)) := get(paste0(s, "_h")) - get(paste0(s, "_a"))]
say("matched team-matches: ", nrow(m))

D <- paste0("d_", STATS)
frm <- as.formula(paste("margin ~", paste(D, collapse = " + ")))

# ---- 1. Collinearity, the suspected cause ----------------------------------
say("")
say("=== 1. Is the design collinear? ===")
X <- as.matrix(m[, ..D])
cn <- kappa(cor(X), exact = TRUE)
say("condition number of the correlation matrix: ", round(cn, 1))
say("  (>30 is usually called problematic; >100 severe)")
say("")
say("scoring block correlations -- the specific suspect:")
sc <- intersect(paste0("d_", c("goals", "behinds", "shots_at_goal",
                               "score_involvements", "marks_inside50", "inside50s")), D)
say_dt(as.data.table(round(cor(m[, ..sc]), 3), keep.rownames = "stat"), 8)

# ---- 2. Unregularised fit --------------------------------------------------
say("")
say("=== 2. Unregularised fit (what the live weights most likely came from) ===")
f0 <- lm(frm, data = m)
co <- summary(f0)$coefficients
ols <- data.table(stat = sub("^d_", "", rownames(co)), ols = round(co[, 1], 4),
                  se = round(co[, 2], 4), t = round(co[, 3], 2))[stat != "(Intercept)"]

# ---- 3. Ridge --------------------------------------------------------------
say("")
say("=== 3. Ridge fit (collinearity-stable) ===")
have_glmnet <- requireNamespace("glmnet", quietly = TRUE)
if (have_glmnet) {
  set.seed(1)
  cvf <- glmnet::cv.glmnet(X, m$margin, alpha = 0, nfolds = 10)
  rc <- as.matrix(coef(cvf, s = "lambda.min"))
  ridge <- data.table(stat = sub("^d_", "", rownames(rc)), ridge = round(rc[, 1], 4))[stat != "(Intercept)"]
  say("lambda.min ", signif(cvf$lambda.min, 3))
} else {
  say("glmnet unavailable -- ridge skipped.")
  ridge <- data.table(stat = ols$stat, ridge = NA_real_)
}

cmp <- Reduce(function(x, y) merge(x, y, by = "stat", all = TRUE),
              list(LIVE[, .(stat, live = round(wt, 4), channel)], ols, ridge))
setorder(cmp, channel, -live)
say("")
say("=== live weight vs unregularised vs ridge ===")
say_dt(cmp[, .(channel, stat, live, ols, se, t, ridge)], 40)

# ---- 4. Anchors ------------------------------------------------------------
say("")
say("=== ANCHOR CHECKS (pre-registered) ===")
gv <- function(tb, col, s) { v <- tb[stat == s][[col]]; if (length(v)) v[1] else NA_real_ }
for (src in c("live", "ols", "ridge")) {
  g <- gv(cmp, src, "goals"); b <- gv(cmp, src, "behinds")
  tu <- gv(cmp, src, "turnovers"); cl <- gv(cmp, src, "clangers")
  say("")
  say("-- ", src, " --")
  say(sprintf("  A1 goal > behind : %-8s (goal %s, behind %s)",
              if (isTRUE(g > b)) "PASS" else "FAIL", signif(g, 4), signif(b, 4)))
  say(sprintf("  A2 goal > 0      : %-8s", if (isTRUE(g > 0)) "PASS" else "FAIL"))
  say(sprintf("  A3 turnovers < 0 : %-8s (%s);  clangers < 0 : %-8s (%s)",
              if (isTRUE(tu < 0)) "PASS" else "FAIL", signif(tu, 4),
              if (isTRUE(cl < 0)) "PASS" else "FAIL", signif(cl, 4)))
}

say("")
say("=== how far are the live weights from a stable fit? ===")
cmp[, ratio_live_to_ridge := round(live / ridge, 2)]
say("stats where live and ridge disagree by more than 3x or in SIGN:")
bad <- cmp[is.finite(ridge) & (sign(live) != sign(ridge) |
                               abs(live / ridge) > 3 | abs(ridge / live) > 3)]
say_dt(bad[, .(channel, stat, live, ridge, ratio_live_to_ridge)], 30)
say("")
say("A sign disagreement is the serious one: it means the live metric is paying")
say("a player for something the data says costs his team points, or vice versa.")

arrow::write_parquet(cmp, file.path(OUT_DIR, "epv3_box_weight_audit.parquet"))
close(con)
cat("\nWrote ", OUT, "\n")
