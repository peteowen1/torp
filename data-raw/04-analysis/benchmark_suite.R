# One fixed protocol for judging a rating change. Source it, call it, compare.
#
# WHY THIS EXISTS, and it is not mainly about statistical power. On 2026-08-05 I
# measured the same rebuild eight different ways and had to withdraw three of
# those measurements as confounded:
#
#   * per-role repeatability of a quantity signed in one team's frame -- it
#     measured whether a player is usually the interceptor, i.e. his position
#   * allocation rules compared at team level, where every conserving rule is
#     identical by construction
#   * a channel scale applied as the reciprocal of the right number
#
# None of those were caught by a check. They were caught by rereading output.
# A fixed protocol would have caught all three, because each metric here carries
# the population, the centring and the failure mode it is vulnerable to.
#
# THE SECOND REASON. Match MAE has been the de-facto objective and it cannot do
# the job. Measured across three gates today, every dMAE 95% CI had a half-width
# of 0.24-0.29 on 396 matches, while every effect under test was 0.03-0.10.
# Detecting 0.08 at that precision needs ~3,560 matches; the whole dataset holds
# 1,241. So match MAE is a GUARDRAIL -- "did we break prediction" -- and never
# the objective. It is deliberately not computed here: it takes 20 minutes an
# arm and answers a different question.
#
# THE FRAME. torp builds two products in one number and they want opposite
# things from a noisy channel -- the descriptive one keeps it (it happened), the
# predictive one shrinks it (it does not repeat). So the scorecard is SPLIT and
# never averaged into a single verdict.
#
# THERE IS NO GROUND TRUTH FOR PLAYER QUALITY. Every block below is a proxy that
# fails in its own direction, which is the point: a change worth making should
# not lose on any of them and should win on more than one.
#
# Usage:
#   source("data-raw/04-analysis/benchmark_suite.R")
#   a <- benchmark_rating(pgd_ship, label = "ship")
#   b <- benchmark_rating(pgd_new,  label = "difficulty")
#   compare_benchmarks(a, b)

suppressMessages({ library(data.table) })

#' Per-player-season value, and the position it belongs to
#'
#' Everything downstream centres within (position, season). Pooled versions of
#' these statistics are dominated by BETWEEN-position variance -- key forwards
#' score differently from key defenders every year -- so a pooled correlation
#' largely re-measures the position map rather than the metric.
.bm_player_seasons <- function(pgd, value_col = "epv", min_games = 8) {
  d <- data.table(player_id = pgd$player_id, player_name = pgd$player_name,
                  season = pgd$season, pos = pgd$position_group,
                  v = pgd[[value_col]])
  d <- d[is.finite(v) & !is.na(pos)]
  s <- d[, .(g = .N, tot = sum(v), rate = sum(v) / .N,
             player_name = player_name[1], pos = pos[1]),
         by = .(player_id, season)][g >= min_games]
  s[, rate_c := rate - mean(rate), by = .(pos, season)]
  s[]
}

#' A. DESCRIPTIVE — does it add up to the game?
.bm_conservation <- function(pgd, results) {
  ch <- c("epv_recv", "epv_disp", "epv_spoil")
  if (!all(ch %in% names(pgd))) return(NULL)
  r <- as.data.table(results)
  tg <- r[, .(match_id = as.character(match_id), home = home_team_name,
              away = away_team_name, margin = home_score - away_score)][is.finite(margin)]
  ts <- as.data.table(pgd)[, lapply(.SD, function(v) sum(v, na.rm = TRUE)), .SDcols = ch,
                           by = .(match_id = as.character(match_id), team)]
  h <- merge(tg, ts, by.x = c("match_id", "home"), by.y = c("match_id", "team"))
  a <- merge(tg, ts, by.x = c("match_id", "away"), by.y = c("match_id", "team"))
  m <- merge(h[, c("match_id", "margin", ch), with = FALSE],
             a[, c("match_id", ch), with = FALSE], by = "match_id", suffixes = c("_h", "_a"))
  for (v in ch) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  m[, tot := d_epv_recv + d_epv_disp + d_epv_spoil]
  co <- stats::coef(stats::lm(margin ~ 0 + d_epv_recv + d_epv_disp + d_epv_spoil, data = m))
  sds <- vapply(ch, function(v) stats::sd(m[[paste0("d_", v)]]), 0)
  list(n_matches = nrow(m),
       total = round(stats::coef(stats::lm(margin ~ 0 + tot, data = m))[[1]], 4),
       recv = round(co[[1]], 4), disp = round(co[[2]], 4), contest = round(co[[3]], 4),
       share_recv = round(100 * (sds[1] * co[[1]])^2 / sum((sds * co)^2), 1),
       share_disp = round(100 * (sds[2] * co[[2]])^2 / sum((sds * co)^2), 1),
       share_contest = round(100 * (sds[3] * co[[3]])^2 / sum((sds * co)^2), 1))
}

#' B1. PREDICTIVE — does last season's value predict this season's, beyond position?
#'
#' Skill score against the honest baseline: predicting every player at his
#' position-season mean. A rating that only encodes position scores 0. This is
#' the closest thing here to "does it say how good a player is", and it has
#' ~3,000 player-season pairs against the match gate's 396 matches.
.bm_skill <- function(s) {
  b <- copy(s)[, season := season + 1]
  setnames(b, "rate_c", "prev_c")
  m <- merge(s[, .(player_id, season, pos, rate_c)],
             b[, .(player_id, season, prev_c)], by = c("player_id", "season"))
  m <- m[is.finite(rate_c) & is.finite(prev_c)]
  if (nrow(m) < 100) return(list(n = nrow(m), skill = NA_real_, r = NA_real_))
  # Baseline is 0 because rate_c is already centred within position-season.
  list(n = nrow(m),
       skill = round(1 - sum((m$rate_c - m$prev_c)^2) / sum(m$rate_c^2), 4),
       r = round(stats::cor(m$rate_c, m$prev_c), 4))
}

#' B2. Within-position repeatability, and B3. count-dependence
.bm_stability <- function(pgd, s) {
  cnt <- function(a, b) if (all(c(a, b) %in% names(pgd)))
    round(stats::cor(pgd[[a]], pgd[[b]], use = "complete.obs"), 3) else NA_real_
  list(within_r = .bm_skill(s)$r,
       cor_disposals = cnt("epv_disp", "disposals"),
       cor_marks = cnt("epv_recv", "marks"),
       cor_cposs = cnt("epv_recv", "contested_possessions"))
}

# C. EXTERNAL — REMOVED 2026-08-05, at Pete's instruction, and he is right.
#
# Brownlow votes are three umpires' opinion of who played well. That is not a
# measurement of a player's contribution to his team's score, and this metric
# exists to measure exactly that. Worse, the votes are heavily midfielder- and
# disposal-biased, so any weight on them pulls directly against the defender
# work this program exists to do. The block said so in its own comment and kept
# the metric anyway, which was the wrong call.
#
# Replaced by bm_epr_gate() in benchmark_epr_gate.R: does a team's lineup
# rating predict that team's points, with team fixed effects, so the question is
# "when THIS club fields better-rated players, does it score more" rather than
# "do good clubs win".

#' D. FACE VALIDITY — who is on top, and is every position represented
.bm_faces <- function(s) {
  cur <- s[season == max(season, na.rm = TRUE)]
  setorder(cur, -tot)
  list(top10 = cur$player_name[1:10],
       top40_positions = cur[1:40, .N, by = pos][order(-N)])
}

#' @param calibrate Score the CALIBRATED frame -- each channel scaled by its own
#'   margin coefficient. This is the version that would ship as the descriptive
#'   product, and it changes the picture a lot: uncalibrated, the contest channel
#'   carries ~43% of the variance for ~6% of the signal and dominates the
#'   leaderboard. Judging an uncalibrated frame answers a question nobody is
#'   asking. Each arm fits its OWN scale, which is the point -- a scale borrowed
#'   from another build is the staleness trap.
benchmark_rating <- function(pgd, label, results = NULL, value_col = "epv",
                             min_games = 8, calibrate = FALSE) {
  pgd <- as.data.table(pgd)
  if (is.null(results)) results <- load_results(TRUE)
  if (isTRUE(calibrate)) {
    pgd <- as.data.table(calibrate_epv_channels(pgd, results = results))
    # Swap the channel columns for their calibrated twins so every block below
    # -- conservation, skill, count-dependence, faces -- reads the same frame.
    for (cc in c("epv_recv", "epv_disp", "epv_spoil", "epv")) {
      data.table::set(pgd, j = cc, value = pgd[[paste0(cc, "_cal")]])
    }
    label <- paste0(label, " (calibrated)")
  }
  s <- .bm_player_seasons(pgd, value_col, min_games)
  out <- list(label = label, n_player_games = nrow(pgd), n_player_seasons = nrow(s),
              descriptive = .bm_conservation(pgd, results),
              skill = .bm_skill(s), stability = .bm_stability(pgd, s),
              faces = .bm_faces(s))
  class(out) <- c("torp_benchmark", "list")
  out
}

print.torp_benchmark <- function(x, ...) {
  cat("\n=== benchmark:", x$label, "===\n")
  cat(sprintf("  %s player-games | %s player-seasons\n",
              format(x$n_player_games, big.mark = ","),
              format(x$n_player_seasons, big.mark = ",")))
  d <- x$descriptive
  if (!is.null(d)) {
    cat("\n  DESCRIPTIVE (does it add up to the game?)\n")
    cat(sprintf("    total -> margin  %.4f   (target 1.000)\n", d$total))
    cat(sprintf("    per channel      recv %.3f  disp %.3f  contest %.3f\n",
                d$recv, d$disp, d$contest))
    cat(sprintf("    signal share     recv %.1f%%  disp %.1f%%  contest %.1f%%\n",
                d$share_recv, d$share_disp, d$share_contest))
  }
  cat("\n  PREDICTIVE (does it say how good a player is?)\n")
  cat(sprintf("    skill score      %.4f  over %d player-season pairs\n",
              x$skill$skill, x$skill$n))
  cat("                     (0 = no better than knowing his position)\n")
  cat(sprintf("    within-pos r     %.4f\n", x$stability$within_r))
  cat(sprintf("    count-dependence disposals %.3f  marks %.3f  cont.poss %.3f\n",
              x$stability$cor_disposals, x$stability$cor_marks, x$stability$cor_cposs))
  cat("                     (nearer zero is better -- an event count is not a skill)\n")
  cat("\n  FACE VALIDITY (current season)\n")
  cat("    top 10:", paste(x$faces$top10, collapse = ", "), "\n")
  cat("    top 40 by position:\n")
  print(x$faces$top40_positions)
  cat("\n  NOT MEASURED HERE: match MAE. It is a guardrail, not an objective --\n")
  cat("  396 matches gives a dMAE CI half-width of ~0.25 against effects of\n")
  cat("  0.03-0.10, so it cannot resolve them. Run it to check nothing broke.\n\n")
  invisible(x)
}

compare_benchmarks <- function(a, b) {
  # Plain nested-list lookup rather than eval(parse(...)). The paths here are
  # fixed literals so parsing them was never a real exposure, but it is also
  # never necessary -- `[[` walks a list perfectly well and cannot run code.
  f <- function(x, path) {
    for (k in path) {
      if (!is.list(x) || is.null(x[[k]])) return(NA_real_)
      x <- x[[k]]
    }
    if (length(x) == 1 && is.numeric(x)) as.numeric(x) else NA_real_
  }
  PATHS <- list(c("descriptive", "total"), c("descriptive", "share_contest"),
                c("skill", "skill"), c("stability", "within_r"),
                c("stability", "cor_disposals"), c("stability", "cor_cposs"))
  rows <- data.table(
    metric = c("conservation total", "contest signal share %", "skill score",
               "within-position r", "cor(disp, disposals)", "cor(recv, cont.poss)"),
    a = vapply(PATHS, function(p) f(a, p), 0),
    b = vapply(PATHS, function(p) f(b, p), 0))
  setnames(rows, c("a", "b"), c(a$label, b$label))
  rows[, delta := round(get(b$label) - get(a$label), 4)]
  cat("\n=== ", a$label, " vs ", b$label, " ===\n", sep = "")
  print(rows)
  cat("\nNo single verdict. Descriptive and predictive rows want different things\n")
  cat("from a noisy channel, and the Brownlow row is biased by construction.\n")
  cat("A change worth making does not lose on any row and wins on more than one.\n\n")
  invisible(rows)
}
