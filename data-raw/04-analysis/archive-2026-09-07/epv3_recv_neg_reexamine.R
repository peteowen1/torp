# Re-examine EPV_RECV_NEG_MULT = 0 now that we know what it removes.
#
# WHY THIS IS BEING RE-OPENED. The change was adopted on a reliability
# measurement: zeroing the term raised reception reliability 0.2507 -> 0.3086
# (+23%) while count-dependence FELL. That is a good result and it is not in
# dispute. What was never measured is what the term CONTAINS.
#
# The value decomposition (epv3_value_buildup.R) says: the pos_team == -1 branch
# is the INTERCEPT branch, and intercepts are the highest-value receiving act in
# the game by a factor of eight -- 168 per match at mean +0.625 against ordinary
# receptions' 1,453 per match at +0.079. They are 10% of receiving events, 31%
# of gross reception value, and +130,571 of the +272,194 net.
#
# So "drop the noisy term" may actually be "stop crediting interceptions". Those
# are not the same change and they do not have the same verdict.
#
# THE CONSERVATION QUESTION, which is the one that decides it. On a turnover row
# the disposer is CHARGED half the swing and, with the multiplier at 0, nobody
# is CREDITED the other half. The metric would then charge losses without paying
# the corresponding gains -- and a metric that does that systematically
# under-rates whoever does the gaining, which in this case is defenders.
#
# Note what does NOT apply under v3: an intercept MARK is an aerial contest and
# v3 already removes every aerial-kick row from the reception split, paying the
# marker through the contest channel instead. So this only touches GROUND-level
# interceptions -- loose ball gets and gathers off a turnover. The question is
# whether those should be free.
#
# Two arms, same structure otherwise: 3 channels, raw merge, contest not
# standardised.
#
# PERFORMANCE: 1 player-game build (~4 min) + 2 rating builds. ~10 min.

suppressMessages({
  library(dplyr); library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_recv_neg_reexamine.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 45) for (l in capture.output(print(utils::head(x, n)))) say(l)
set_const <- function(l) for (nm in names(l)) assignInNamespace(nm, l[[nm]], ns = "torp")

say("=== Re-examining EPV_RECV_NEG_MULT = 0: what does it actually remove? ===")
say("run at ", format(Sys.time()))

pbp    <- load_pbp(TRUE); stats_ <- load_player_stats(TRUE)
teams  <- load_teams(TRUE); chains <- load_chains(TRUE)
shared_stat_ratings <- get_player_stat_ratings(current = FALSE)
shared_fixtures     <- load_fixtures(TRUE)
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)),
                   error = function(e) NULL)
res <- as.data.table(load_results(TRUE)); xg <- as.data.table(load_xg(TRUE))
tgt <- merge(res[, .(match_id = as.character(match_id), margin = home_score - away_score)],
             xg[, .(match_id = as.character(match_id), xmargin = xscore_diff)],
             by = "match_id")[is.finite(margin) & is.finite(xmargin)]

set_const(list(EPV3_CHANNELS = 3L, EPV3_SUB_SCALE = c(cont_aerial = 1, cont_stop = 1),
               EPV3_POINTS_SCALE = c(recv = 1, disp = 1, cont_aerial = 1, cont_stop = 1),
               EPV_STANDARDISE_CHANNELS = c("recv", "disp"),
               EPV3_STOP_ZERO_SUM = FALSE,
               EPR_PRIOR_RATE_RECV = -0.7, EPR_PRIOR_RATE_DISP = -0.7,
               EPR_PRIOR_RATE_SPOIL = -0.3, EPR_PRIOR_RATE_HITOUT = 0))

build_pgd <- function(tag, neg_mult) {
  f <- file.path(OUT_DIR, paste0("epv3_cal_pgd_", tag, ".parquet"))
  if (file.exists(f)) { cli::cli_alert_info("Reusing pgd {tag}")
    d <- as.data.table(read_parquet(f))
  } else {
    p <- default_epv_params(); p$recv_neg_mult <- neg_mult
    d <- as.data.table(create_player_game_data(pbp, stats_, teams, chains,
                                               epv_params = p, epv_engine = "v3"))
    write_parquet(d, f)
  }
  setattr(d, "epv_engine", "v3"); d
}
build_ratings <- function(pgd, tag) {
  f <- file.path(OUT_DIR, paste0("epv3_cal_rt_", tag, ".parquet"))
  if (file.exists(f)) { cli::cli_alert_info("Reusing ratings {tag}")
    return(as.data.table(read_parquet(f))) }
  d <- adjust_epv_for_opponents(as.data.table(copy(pgd)))
  setattr(d, "epv_engine", "v3")
  if (isTRUE(EPV_LEVEL_CENTRE)) d <- centre_epv_by_position(d)
  out <- rbindlist(lapply(sort(unique(d$season)), function(s) {
    sr <- if (s >= 2024) 0 else 1
    mr <- if (s == get_afl_season()) get_afl_week(type = "next") else 28
    torp:::.build_epr_season(s, sr:mr, d, shared_stat_ratings, shared_fixtures)
  }), use.names = TRUE, fill = TRUE)
  if (isTRUE(EPR_POSITION_CENTRE)) out <- centre_epr_by_position(out)
  if (!is.null(psr_df) && nrow(psr_df) > 0 && "psr" %in% names(psr_df)) {
    out <- calculate_torp(out, psr_df)
  }
  out <- as.data.table(out); write_parquet(out, f); out
}
CH3 <- c("epr_recv", "epr_disp", "epr_spoil")
fit3 <- function(rt) {
  tr <- as.data.table(.build_team_ratings_df(teams, as.data.frame(rt), psr_df))
  h <- tr[team_type == "home"]; a <- tr[team_type == "away"]
  m <- merge(h[, c("match_id", CH3), with = FALSE], a[, c("match_id", CH3), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  for (v in CH3) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  m <- merge(m, tgt, by = "match_id")
  co <- summary(lm(as.formula(paste("xmargin ~ 0 +", paste0("d_", CH3, collapse = " + "))),
                   data = m))$coefficients
  list(coef = setNames(co[, 1], CH3), t = setNames(co[, 3], CH3),
       sd = vapply(CH3, function(v) sd(m[[paste0("d_", v)]]), numeric(1)))
}
yoy <- function(rt, col) {
  eos <- rt[, .SD[which.max(round)], by = .(player_id, season)]
  a <- eos[, c("player_id", "season", col), with = FALSE]; setnames(a, col, "v")
  b <- copy(a)[, season := season - 1]; setnames(b, "v", "v2")
  m <- merge(a, b, by = c("player_id", "season"))[is.finite(v) & is.finite(v2)]
  round(cor(m$v, m$v2), 4)
}
# Game-to-game reliability: split each player's games odd/even and correlate.
reliab <- function(pgd, col) {
  d <- pgd[is.finite(get(col))]
  setorder(d, player_id, utc_start_time)
  d[, .i := seq_len(.N), by = player_id]
  s <- d[, .(a = mean(get(col)[.i %% 2 == 1]), b = mean(get(col)[.i %% 2 == 0]),
             n = .N), by = player_id][n >= 10 & is.finite(a) & is.finite(b)]
  round(cor(s$a, s$b), 4)
}

arms <- list(`neg_mult 1 (intercepts credited)` = list(tag = "3ch_neg1_nostd", nm = 1.0),
             `neg_mult 0 (intercepts free)`     = list(tag = "3ch_raw_nostd",  nm = 0.0))
R <- list()
for (nm in names(arms)) {
  a <- arms[[nm]]
  cli::cli_h1(nm)
  pgd <- build_pgd(a$tag, a$nm)
  rt  <- build_ratings(pgd, a$tag)
  R[[nm]] <- list(pgd = pgd, rt = rt, fit = fit3(rt))
}

say("")
say("=== 1. CONSERVATION: how much of the swing is allocated at all? ===")
say("Total EPV credited per team-match. If zeroing the intercept branch drops")
say("this, the metric is charging losses it no longer pays gains for.")
for (nm in names(R)) {
  d <- R[[nm]]$pgd
  tm <- d[, .(tot = sum(epv, na.rm = TRUE),
              recv = sum(epv_recv, na.rm = TRUE),
              disp = sum(epv_disp, na.rm = TRUE),
              cont = sum(epv_spoil, na.rm = TRUE)), by = .(match_id, team)]
  say(sprintf("  %-34s epv %+8.2f  recv %+7.2f  disp %+7.2f  contest %+7.2f  (per team-match)",
              nm, mean(tm$tot), mean(tm$recv), mean(tm$disp), mean(tm$cont)))
}

say("")
say("=== 2. THE RECEPTION CHANNEL ITSELF ===")
for (nm in names(R)) {
  d <- R[[nm]]$pgd
  say(sprintf("  %-34s sd %.4f  mean %+.4f  game-to-game reliability %.4f",
              nm, sd(d$epv_recv, na.rm = TRUE), mean(d$epv_recv, na.rm = TRUE),
              reliab(d, "epv_recv")))
}

say("")
say("=== 3. MARGIN FIT AT THE EPR LAYER ===")
for (nm in names(R)) {
  f <- R[[nm]]$fit
  say(""); say("--- ", nm, " ---")
  say_dt(data.table(channel = CH3, coef = round(f$coef, 4), t = round(f$t, 2),
                    sd_points = round(f$sd * f$coef, 3),
                    share_pct = round(100 * (f$sd * f$coef)^2 / sum((f$sd * f$coef)^2), 1)), 5)
  say("recv y-o-y repeatability: ", yoy(R[[nm]]$rt, "epr_recv"))
}

say("")
say("=== 4. WHO LOSES: intercepting defenders ===")
say("If the term is noise, dropping it should not systematically move one kind")
say("of player. If it is interception value, defenders fall.")
for (nm in names(R)) {
  rt <- R[[nm]]$rt
  cu <- rt[season == max(season)][round == max(round)]
  say(""); say("--- ", nm, " ---")
  pp <- cu[!is.na(position_group), .(n = .N,
            mean_recv = round(mean(epr_recv, na.rm = TRUE), 3),
            mean_epr = round(mean(epr, na.rm = TRUE), 3)), by = position_group]
  setorder(pp, -mean_recv); say_dt(pp, 8)
  rk <- if ("torp_value" %in% names(cu)) "torp_value" else "epr"
  inn <- cu[order(-get(rk))][1:40][!is.na(position_group), .N, by = position_group]
  setorder(inn, -N)
  say("top 40 by position: ", paste(inn$position_group, inn$N, sep = "=", collapse = "  "))
}

say("")
say("=== 5. NAMED INTERCEPT DEFENDERS, side by side ===")
who <- c("Harris Andrews", "Nick Haynes", "Harry Himmelberg", "Sam Taylor",
         "Jacob Weitering", "Tom Stewart", "Nathan Broad", "Jeremy Howe",
         "Steven May", "Darcy Moore")
cmp <- rbindlist(lapply(names(R), function(nm) {
  cu <- R[[nm]]$rt[season == max(season)][round == max(round)]
  cu[player_name %chin% who, .(arm = nm, player_name,
                               epr_recv = round(epr_recv, 3), epr = round(epr, 3))]
}))
if (nrow(cmp) > 0) say_dt(dcast(cmp, player_name ~ arm, value.var = c("epr_recv", "epr")), 12)

say("")
say("=== VERDICT INPUTS ===")
say("Ship neg_mult = 0 only if: reliability is up AND conservation is not")
say("materially broken AND defenders do not systematically fall. If defenders")
say("fall, the +23% was bought by deleting the thing they are good at, which")
say("is the same shape as the degenerate optimum -- a cleaner number that")
say("measures less.")

close(con)
cat("\nDone\n")
