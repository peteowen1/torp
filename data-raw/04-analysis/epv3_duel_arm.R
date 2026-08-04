# Points 1 and 2: rebuild the contest channel on GENUINE DUELS only, and
# re-test the ledger allocation on that population.
#
# WHY BOTH AT ONCE. The ledger rule (weight each unnamed debit by the beaten
# player's OWN recorded one-on-one losses from the AFL API) was measured and
# rejected -- year-over-year 0.485 against "team"'s 0.819. But it was scored
# against a population of 235 "contests" per match using a key that counts 26
# defensive + 26 offensive one-on-ones. A 4.5:1 mismatch would sink any
# weighting scheme, so that verdict is not safe and has to be re-run on the
# restricted population (74.2 duels per match) rather than inherited.
#
# WHAT RESTRICTING DOES, and it is not a pure subtraction. Kicks that stop being
# contests fall back to the ordinary disposer/receiver split, because
# `aerial_kick_keys` is built from the same table. So value MOVES from the
# contest channel into recv and disp rather than disappearing. All three
# channels change and all three need refitting.
#
# Four arms:
#   all_team     the shipping build                      (population all,  alloc team)
#   duel_team    duels only                              (population duel, alloc team)
#   duel_ledger  duels only, debit keyed on the API ledger
#   duel_none    duels only, debit left unallocated -- the upper bound on what
#                the channel could look like if attribution were solved
#
# PERFORMANCE: 3 player-game builds (~5 min each) + 4 rating builds (~2 min).
# ~25 min. Run detached.

suppressMessages({
  library(dplyr); library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_duel_arm.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 45) for (l in capture.output(print(utils::head(x, n)))) say(l)
set_const <- function(l) for (nm in names(l)) assignInNamespace(nm, l[[nm]], ns = "torp")

FIN <- readRDS(file.path(OUT_DIR, "epv3_finalise_ship.rds"))
PG  <- FIN$prior_games
say("=== Genuine duels only, and the ledger re-test ===")
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

STRUCT <- list(EPV3_CHANNELS = 3L, EPV3_SUB_SCALE = c(cont_aerial = 1, cont_stop = 1),
               EPV_STANDARDISE_CHANNELS = c("recv", "disp"),
               EPV3_STOP_ZERO_SUM = TRUE,
               EPV3_POINTS_SCALE = c(recv = 1, disp = 1, cont_aerial = 1, cont_stop = 1),
               EPR_PRIOR_RATE_RECV = -0.7, EPR_PRIOR_RATE_DISP = -0.7,
               EPR_PRIOR_RATE_SPOIL = -0.3, EPR_PRIOR_RATE_HITOUT = 0,
               EPR_PRIOR_GAMES_RECV = PG[channel == "recv", prior_games],
               EPR_PRIOR_GAMES_DISP = PG[channel == "disp", prior_games],
               EPR_PRIOR_GAMES_SPOIL = PG[channel == "spoil", prior_games],
               EPR_PRIOR_GAMES_HITOUT = 3)

build_pgd <- function(tag) {
  f <- file.path(OUT_DIR, paste0("epv3_duel_pgd_", tag, ".parquet"))
  if (file.exists(f)) { cli::cli_alert_info("Reusing pgd {tag}")
    d <- as.data.table(read_parquet(f))
  } else {
    d <- as.data.table(create_player_game_data(pbp, stats_, teams, chains,
                                               epv_engine = "v3"))
    write_parquet(d, f)
  }
  setattr(d, "epv_engine", "v3"); d
}
build_ratings <- function(pgd, tag) {
  f <- file.path(OUT_DIR, paste0("epv3_duel_rt_", tag, ".parquet"))
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
  m <- merge(m, tgt, by = "match_id"); setorder(m, match_id)
  fml <- as.formula(paste("xmargin ~ 0 +", paste0("d_", CH3, collapse = " + ")))
  co <- summary(lm(fml, data = m))$coefficients
  cut <- floor(nrow(m) / 2)
  sdv <- vapply(CH3, function(v) sd(m[[paste0("d_", v)]]), numeric(1))
  pts <- sdv * co[, 1]
  list(coef = setNames(co[, 1], CH3), t = setNames(co[, 3], CH3),
       share = 100 * pts^2 / sum(pts^2),
       half1 = coef(lm(fml, data = m[1:cut]))[["d_epr_spoil"]],
       half2 = coef(lm(fml, data = m[(cut + 1):nrow(m)]))[["d_epr_spoil"]])
}
yoy <- function(rt, col) {
  eos <- rt[, .SD[which.max(round)], by = .(player_id, season)]
  a <- eos[, c("player_id", "season", col), with = FALSE]; setnames(a, col, "v")
  b <- copy(a)[, season := season - 1]; setnames(b, "v", "v2")
  m <- merge(a, b, by = c("player_id", "season"))[is.finite(v) & is.finite(v2)]
  round(cor(m$v, m$v2), 4)
}

ARMS <- list(
  list(tag = "all_team",    pop = "all",  alloc = "team"),
  list(tag = "duel_team",   pop = "duel", alloc = "team"),
  list(tag = "duel_ledger", pop = "duel", alloc = "ledger"),
  list(tag = "duel_none",   pop = "duel", alloc = "none")
)

rows <- list()
for (a in ARMS) {
  cli::cli_h1(a$tag)
  set_const(c(STRUCT, list(EPV3_CONTEST_POPULATION = a$pop,
                           EPV_CONT_LOSS_ALLOC = a$alloc)))
  pgd <- build_pgd(a$tag)
  rt  <- build_ratings(pgd, a$tag)
  f   <- fit3(rt)

  say("")
  say("=== ", a$tag, "  (population ", a$pop, ", debit alloc ", a$alloc, ") ===")
  say_dt(data.table(channel = c("recv", "disp", "contest"),
                    coef = round(f$coef, 4), t = round(f$t, 2),
                    share_pct = round(f$share, 1)), 5)
  say("contest constant split-half: ", round(f$half1, 3), " / ", round(f$half2, 3))
  say("contest y-o-y repeatability: ", yoy(rt, "epr_spoil"))
  say("contest channel at the VALUE layer: sd ",
      round(sd(pgd$epv_spoil, na.rm = TRUE), 4), "  mean ",
      round(mean(pgd$epv_spoil, na.rm = TRUE), 4))
  say("cor(contest, contested_marks) ",
      round(cor(pgd$epv_spoil, pgd$contested_marks, use = "complete.obs"), 3),
      "   cor(contest, spoils) ",
      round(cor(pgd$epv_spoil, pgd$spoils, use = "complete.obs"), 3))

  cu <- rt[season == max(season)][round == max(round)]
  say("")
  say("contest leaders:")
  say_dt(cu[order(-epr_spoil)][1:10, .(player_name, position_group,
                                       contest = round(epr_spoil, 3))], 10)

  rows[[a$tag]] <- data.table(arm = a$tag, pop = a$pop, alloc = a$alloc,
    contest_coef = round(f$coef[["epr_spoil"]], 3),
    contest_t = round(f$t[["epr_spoil"]], 2),
    contest_share = round(f$share[["epr_spoil"]], 1),
    recv_share = round(f$share[["epr_recv"]], 1),
    disp_share = round(f$share[["epr_disp"]], 1),
    contest_yoy = yoy(rt, "epr_spoil"),
    half1 = round(f$half1, 2), half2 = round(f$half2, 2),
    cor_cm = round(cor(pgd$epv_spoil, pgd$contested_marks, use = "complete.obs"), 3))
}

say("")
say("=== SIDE BY SIDE ===")
say_dt(rbindlist(rows), 6)
say("")
say("WHAT WOULD COUNT AS SUCCESS. The contest channel currently reads t 1.57,")
say("share 0.6%, and a constant that sign-flips split-half. Restricting to real")
say("duels should raise t and stabilise the constant if the non-duels were")
say("noise. If t and the split-half stability do NOT improve, the channel is")
say("genuinely weak rather than diluted, and the population was not the problem.")
say("")
say("Watch cor(contest, contested_marks) too: rising sharply would mean the")
say("channel has become a contested-mark COUNT, which is the degenerate")
say("outcome, not a win.")

saveRDS(rbindlist(rows), file.path(OUT_DIR, "epv3_duel_arm.rds"))
say("")
say("done ", format(Sys.time()))
close(con)
cat("\nDone\n")
