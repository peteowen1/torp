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

#' C2. THE ADJUSTMENT LAYER — what the positional adjustment did to each channel
#'
#' \strong{Added 2026-08-06 after THREE consecutive changes were invisible here.}
#' Everything above scores the RAW channels. The bench remap, the channel-specific
#' centring key and the rating-layer points scale all touch only the \code{_adj}
#' columns, so the panel read a delta of exactly zero on every row for all three
#' — while one of them was moving Mason Cox 427 places up the leaderboard.
#'
#' Each channel is passed through \code{.position_adjust()} inside a cell, and
#' the two ways that goes wrong are:
#' \itemize{
#'   \item \strong{the ordering breaks} — a player with less raw production rates
#'     above one with more, because they sit in different cells. Caught by
#'     \code{cor(adj, raw)} falling.
#'   \item \strong{it becomes a minutes artefact} — per-80 rewards part-timers.
#'     Caught by \code{cor(adj, tog)} moving away from zero.
#' }
#'
#' Both are measured WITHIN the cells that matter, and the top-5 overlap is a
#' blunt human-readable version: if the five biggest raw producers are not the
#' five highest rated, something in between reordered them.
.bm_adj <- function(pgd) {
  CH <- c("recv", "disp", "spoil", "hitout")
  have <- CH[vapply(CH, function(c)
    all(c(paste0("epv_", c), paste0("epv_", c, "_adj")) %in% names(pgd)), logical(1))]
  if (!length(have)) return(NULL)
  d <- data.table::as.data.table(pgd)
  tog <- pmax(data.table::fcoalesce(d$time_on_ground_percentage / 100, 0.1), 0.1)
  cur <- d$season == max(d$season, na.rm = TRUE)
  rows <- data.table::rbindlist(lapply(have, function(c) {
    raw <- d[[paste0("epv_", c)]]; adj <- d[[paste0("epv_", c, "_adj")]]
    ok <- is.finite(raw) & is.finite(adj)
    # Per-player season means -- a per-game correlation is dominated by
    # game-to-game noise and would look fine no matter what the cell did.
    pl <- data.table::data.table(p = d$player_name, raw = raw, adj = adj,
                                 tog = tog, pos = d$position_group)[ok & cur]
    pl <- pl[, .(raw = mean(raw), adj = mean(adj), tog = mean(tog), n = .N,
                 pos = pos[1]), by = p][n >= 6]
    if (nrow(pl) < 20) return(NULL)
    # Restrict to players who actually do this thing. A channel most players
    # score zero in has its correlation set by the zeros, not by the contest.
    # Top quartile by SIGNED raw, not by abs(raw). On a channel where 59% of
    # players are negative -- spoil, thanks to the negative
    # def_half_pressure_acts and rebound50s weights -- abs() selects both tails
    # and correlates a bimodal mixture.
    act <- pl[raw > stats::quantile(pl$raw, 0.75, na.rm = TRUE)]
    t5r <- utils::head(act[order(-raw)]$p, 5); t5a <- utils::head(act[order(-adj)]$p, 5)
    bypos <- pl[!is.na(pos), .(m = mean(adj)), by = pos]
    data.table::data.table(
      channel = c, n_active = nrow(act),
      cor_adj_raw = round(stats::cor(act$raw, act$adj), 3),
      # cor(adj, tog) is NOT to be judged against zero. Raw channels carry their
      # own minutes structure -- hitout -0.301 because specialist rucks play
      # fewer minutes, spoil +0.246 because tacklers accumulate with time on
      # ground -- so a channel can be perfectly sound and still read far from
      # zero. What matters is whether the adjustment REDUCES what it inherited.
      #
      # Judging against zero cost a retracted false alarm and a wasted
      # investigation on 2026-08-06, both on spoil, both from this one line.
      cor_raw_tog = round(stats::cor(act$raw, act$tog), 3),
      cor_adj_tog = round(stats::cor(act$adj, act$tog), 3),
      tog_removed = round(abs(stats::cor(act$raw, act$tog)) -
                            abs(stats::cor(act$adj, act$tog)), 3),
      top5_overlap = length(intersect(t5r, t5a)),
      pos_dominant = bypos[which.max(abs(m))]$pos,
      pos_gap_sd = round((max(bypos$m) - stats::median(bypos$m)) /
                           max(stats::sd(pl$adj), 1e-9), 2))
  }))
  rows
}

#' D. FACE VALIDITY — who is on top, and is every position represented
.bm_faces <- function(s, value_col = "epv") {
  cur <- s[season == max(season, na.rm = TRUE)]
  setorder(cur, -tot)
  ranked <- cur[, .(player_id, player_name, position_group = pos, epr = tot)]
  # Which column this leaderboard is OF. Load-bearing: the panel defaults to raw
  # `epv`, and a change that acts on `epv_*_adj` (all the centring work) leaves
  # raw `epv` untouched -- so face_validity() correctly reports a no-op and a
  # reader could take that "pass" for more than it is. Printed, not hidden.
  data.table::setattr(ranked, "value_col", value_col)
  list(top10 = cur$player_name[1:10],
       top40_positions = cur[1:40, .N, by = pos][order(-N)],
       # Kept so compare_benchmarks() can run face_validity() across two arms.
       # The position table above cannot support it -- rank movement needs the
       # players, not their counts.
       ranked = ranked)
}

#' D2. FACE VALIDITY AS A CHECK, not a printout
#'
#' Three times on 2026-08-06 a change passed every metric in this panel and
#' failed on inspection of the top 40. Twice it was the same per-channel scale,
#' failing in OPPOSITE directions -- rucks rising absurdly at one set of
#' constants (Mason Cox 506 -> 79), key defenders collapsing at another (5 of the
#' top 40 down to 1). Each was caught by eye, after a 50-minute run.
#'
#' \strong{Calibration of the thresholds.} They are set so the change that
#' SHIPPED passes and both that were rejected fail. That is the only honest way
#' to set them -- a check tuned only against failures will reject good work, and
#' this panel's whole problem has been measuring things that do not decide
#' anything. Re-run \code{verify_face_validity.R} after ANY threshold edit; it
#' fails loudly if the separation breaks. Measured 2026-08-06:
#'
#' \tabular{lllll}{
#'   \strong{case} \tab \strong{mix} \tab \strong{Spearman} \tab \strong{nowhere} \tab \strong{climb} \cr
#'   centring (shipped) \tab 0 \tab 0.9636 \tab 0 \tab +104 \cr
#'   combined arm (rejected) \tab \strong{4} \tab \strong{0.8673} \tab 1 \tab +95 \cr
#'   ws26 per-channel (rejected) \tab 3 \tab \strong{0.8981} \tab \strong{2} \tab \strong{+427} \cr
#' }
#'
#' Two things that table says and the verdict alone does not. \strong{No single
#' row catches both failures} -- the combined arm fails on mix and rank
#' stability, ws26 passes mix and fails on the other two. They were bad in
#' different ways, so the set is load-bearing and dropping a row is not free.
#' And \strong{Spearman is the tightest threshold}: ws26 missed it by 0.002. It
#' fails two other rows as well so nothing hangs on that margin, but do not
#' treat 0.90 as a robust separator on its own evidence.
#'
#' \strong{What this cannot do.} It cannot tell a correction from a defect. A
#' targeted fix SHOULD move one position group -- the centring work deliberately
#' moved rucks, in both directions and modestly. So the mover-concentration row
#' reports and never fails; it is there to be read, not to gate.
#'
#' @param before,after Frames with \code{player_id}, \code{player_name},
#'   \code{position_group} and a score column.
#' @param score Name of the score column, default \code{"epr"}.
#' @param n_top Size of the leaderboard being judged.
#' @return A \code{data.table} of checks, one row each, with a verdict.
face_validity <- function(before, after, score = "epr", n_top = 40) {
  b <- as.data.table(before); a <- as.data.table(after)
  for (d in list(b, a)) {
    if (!score %in% names(d)) cli::cli_abort("No {.field {score}} column.")
    data.table::setnames(d, score, ".sc")
  }
  m <- merge(b[, .(player_id, player_name, position_group, sc_b = .sc)],
             a[, .(player_id, sc_a = .sc)], by = "player_id")
  m <- m[is.finite(sc_b) & is.finite(sc_a)]
  if (!nrow(m)) cli::cli_abort("No players in common.")
  m[, `:=`(rk_b = data.table::frank(-sc_b), rk_a = data.table::frank(-sc_a))]
  m[, gain := rk_b - rk_a]

  # 1. Position mix. The ws28 signature: KEY_DEFENDER 5 -> 1.
  mix <- merge(m[rk_b <= n_top, .(before = .N), by = position_group],
               m[rk_a <= n_top, .(after = .N), by = position_group],
               by = "position_group", all = TRUE)
  mix[is.na(before), before := 0L][is.na(after), after := 0L]
  mix_worst <- max(abs(mix$after - mix$before))
  mix_who <- mix[which.max(abs(after - before)), position_group]

  # 2. Rank stability across the whole rated population.
  sp <- stats::cor(m$rk_b, m$rk_a, method = "spearman")

  # 3. Appear-from-nowhere: into the top N from outside 3N. The ws26 signature
  #    was Sean Darcy 125 -> 5. A player climbing from just outside is normal
  #    form; one arriving from three leaderboards away is a repricing artifact.
  nowhere <- m[rk_a <= n_top & rk_b > 3 * n_top]

  # 4. Biggest single climb among players who FINISH near the top. Restricted to
  #    the top 100 because a rise from 700th to 500th moves nobody's opinion.
  climb <- m[rk_a <= 100][order(-gain)]
  climb_max <- if (nrow(climb)) climb$gain[1] else 0
  climb_who <- if (nrow(climb)) climb$player_name[1] else NA_character_

  # Reported, never failed -- see the note above.
  risers <- m[order(-gain)][1:min(10, nrow(m))]
  conc <- risers[, .N, by = position_group][order(-N)]

  out <- data.table::data.table(
    check = c("position mix in top N", "rank stability (Spearman)",
              "appears from nowhere", "biggest climb into top 100",
              "riser concentration"),
    value = c(sprintf("%d (%s)", mix_worst, mix_who),
              sprintf("%.4f", sp),
              sprintf("%d player%s", nrow(nowhere), if (nrow(nowhere) == 1) "" else "s"),
              sprintf("%+d (%s)", as.integer(climb_max), climb_who),
              sprintf("%d of 10 are %s", conc$N[1], conc$position_group[1])),
    limit = c("<= 3", ">= 0.90", "<= 1", "<= +200", "reported only"),
    verdict = c(
      if (mix_worst <= 3) "pass" else "FAIL",
      if (is.finite(sp) && sp >= 0.90) "pass" else "FAIL",
      if (nrow(nowhere) <= 1) "pass" else "FAIL",
      if (climb_max <= 200) "pass" else "FAIL",
      "-"))
  data.table::setattr(out, "detail",
                      list(mix = mix, nowhere = nowhere, risers = risers, conc = conc))
  data.table::setattr(out, "overall", if (any(out$verdict == "FAIL")) "FAIL" else "pass")
  class(out) <- c("torp_face_validity", class(out))
  out
}

print.torp_face_validity <- function(x, ...) {
  cat("\n=== FACE VALIDITY ===\n")
  print(data.table::as.data.table(x))
  cat("\n  OVERALL: ", attr(x, "overall"), "\n", sep = "")
  if (identical(attr(x, "overall"), "FAIL")) {
    d <- attr(x, "detail")
    cat("\n  position mix:\n"); print(d$mix[order(-after)])
    if (nrow(d$nowhere)) {
      cat("\n  from nowhere:\n")
      print(d$nowhere[, .(player_name, position_group,
                          was = as.integer(rk_b), now = as.integer(rk_a))])
    }
  }
  cat("\n  This gates the DISPLAY, not the prediction. A change can pass every\n")
  cat("  predictive row in the panel and still fail here -- that is the point.\n\n")
  invisible(x)
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
  # The adjustment-layer view must see the UNCALIBRATED channels.
  #
  # It compares `epv_<ch>` against `epv_<ch>_adj`, and the calibration below
  # rewrites the first while never touching the second -- so with a NEGATIVE
  # channel scale the comparison flips sign and reports an inversion that is
  # not there. That is exactly what happened on 2026-08-06: v2's spoil scale is
  # -0.4812, the view printed cor(adj, raw) = -0.902, and I reported a
  # production bug. The true correlation is +0.902. Nothing was wrong with the
  # adjustment; the two sides of the comparison were on different scales.
  pgd_unscaled <- data.table::copy(pgd)
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
              adj = .bm_adj(pgd_unscaled), faces = .bm_faces(s, value_col))
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
  a <- x$adj
  if (!is.null(a) && nrow(a)) {
    cat("\n  ADJUSTMENT LAYER (what positional centring did to each channel)\n")
    for (i in seq_len(nrow(a))) cat(sprintf(
      "    %-7s cor(adj,raw) %+.3f | TOG raw %+.3f -> adj %+.3f (removed %+.3f) | top5 %d/5 | %s +%.2f sd\n",
      a$channel[i], a$cor_adj_raw[i], a$cor_raw_tog[i], a$cor_adj_tog[i],
      a$tog_removed[i], a$top5_overlap[i], a$pos_dominant[i], a$pos_gap_sd[i]))
    cat("\n    TOG: read the REMOVED column, never the adj column alone. Raw\n")
    cat("    channels carry their own minutes structure (hitout -0.301, spoil\n")
    cat("    +0.246), so 'adj far from zero' is not a fault. Positive removed =\n")
    cat("    the adjustment took some out; NEGATIVE = it added some, and that is\n")
    cat("    the real fault condition.\n")
    cat("\n    cor(adj,raw): high is good ONLY where every player in the channel\n")
    cat("    does the same job, as in hitout. For recv, disp and spoil every\n")
    cat("    position scores at a different rate and the adjustment is SUPPOSED\n")
    cat("    to reorder -- a low value there is the design working, not failing.\n")
    cat("\n    A low top-5 overlap WITH a large positional gap is the signature\n")
    cat("    of a channel celled on the wrong group.\n")
  }
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
  # The two adjustment-layer numbers that would have caught all three of the
  # changes this panel could not previously see. WORST channel, not the mean --
  # one broken channel is the failure mode and averaging hides it.
  worst <- function(x) {
    z <- x$adj
    if (is.null(z) || !nrow(z)) return(c(NA_real_, NA_real_))
    # Worst REDUCTION across channels, not worst absolute. Negative means some
    # channel's adjustment ADDED minutes dependence rather than removing it,
    # which is the genuine fault -- a large |cor(adj, tog)| on its own is not.
    c(min(z$cor_adj_raw, na.rm = TRUE), min(z$tog_removed, na.rm = TRUE))
  }
  wa <- worst(a); wb <- worst(b)
  rows <- data.table(
    metric = c("conservation total", "contest signal share %", "skill score",
               "within-position r", "cor(disp, disposals)", "cor(recv, cont.poss)",
               "WORST cor(adj, raw)", "WORST TOG removed"),
    a = c(vapply(PATHS, function(p) f(a, p), 0), wa),
    b = c(vapply(PATHS, function(p) f(b, p), 0), wb))
  setnames(rows, c("a", "b"), c(a$label, b$label))
  rows[, delta := round(get(b$label) - get(a$label), 4)]
  cat("\n=== ", a$label, " vs ", b$label, " ===\n", sep = "")
  print(rows)
  cat("\nNo single verdict on the rows above. Descriptive and predictive rows want\n")
  cat("different things from a noisy channel, and the Brownlow row is biased by\n")
  cat("construction. A change worth making does not lose on any row and wins on\n")
  cat("more than one.\n")

  # The face-validity block DOES have a verdict, and it is the one row on this
  # page that would have stopped 2026-08-06's two rejected changes before a
  # 50-minute run rather than after.
  fv <- NULL
  if (!is.null(a$faces$ranked) && !is.null(b$faces$ranked)) {
    fv <- tryCatch(face_validity(a$faces$ranked, b$faces$ranked),
                   error = function(e) { cat("\n  face validity unavailable: ",
                                             conditionMessage(e), "\n", sep = ""); NULL })
    if (!is.null(fv)) {
      vc <- attr(a$faces$ranked, "value_col")
      if (is.null(vc)) vc <- "epv"
      data.table::setattr(fv, "value_col", vc)
      print(fv)
      if (identical(vc, "epv")) {
        cat("  ^ computed on RAW `epv`. Every centring/adjustment change acts on\n")
        cat("    `epv_*_adj`, which leaves raw `epv` untouched -- so a `pass` here\n")
        cat("    means \"this column did not move\", NOT \"the leaderboard is fine\".\n")
        cat("    Gate those by calling face_validity() on the two RATING frames,\n")
        cat("    the way verify_face_validity.R does.\n")
      }
    }
  } else {
    cat("\n  NOTE: no face-validity check -- one arm predates `faces$ranked`.\n")
    cat("  Rebuild both arms before trusting this comparison; three changes on\n")
    cat("  2026-08-06 passed every numeric row above and failed on inspection.\n")
  }
  cat("\n")
  # Returns `rows` unchanged -- callers that saved this before still get the
  # same object. The face-validity result rides along as an attribute rather
  # than changing the shape.
  data.table::setattr(rows, "face_validity", fv)
  invisible(rows)
}
