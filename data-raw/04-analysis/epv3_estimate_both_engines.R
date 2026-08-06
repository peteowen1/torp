# Estimate prior_games and decay for BOTH engines' channels.
#
# The earlier estimator ran on v3 only. That is not transferable: prior_games is
# a ratio of within-player to between-player variance, and v2's channels are
# measurably less noisy than v3's (disposal reliability 0.267 against 0.077). A
# less noisy channel needs LESS shrinkage, so v2 and v3 require different
# numbers and using one set for both would be wrong in a way that looks fine.
#
# WHY v2 MATTERS MOST HERE: v2 is production. prior_games = 3 is live on every
# channel today, against a measured requirement several times that. If the
# corrected values improve v2, that is a shippable win NOW, independent of
# whether v3 ever ships.
#
# HONESTY ABOUT DECAY. For the noisy chain channels the lag correlation is both
# low and nearly flat (recv 0.100 -> 0.059 across 572 days), so the exponential
# fit is weakly identified -- a flat curve is consistent with a very long decay
# OR with no decay at all. The fit quality is reported per channel so a
# poorly-determined estimate is visible rather than quoted as if it were solid.
# Only cont_stop shows a curve with a real shape.
#
# PERFORMANCE: shift-based lag correlation, O(n) per lag. 2 engines x 4 channels
# x 30 lags over ~56k rows = ~13M row-ops. The pairwise alternative would be
# ~34M PAIRS per engine and quadratic in history. ~3 minutes.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_params_both_engines.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

CH <- c("recv", "disp", "spoil", "hitout")
MAX_LAG <- 30L
TOG_MIN <- 50          # the arm the robustness check said to trust

prep <- function(f) {
  d <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, f)))
  d <- adjust_epv_for_opponents(d)
  if (isTRUE(EPV_LEVEL_CENTRE)) d <- centre_epv_by_position(d)
  d[, tog_safe := pmax(fcoalesce(time_on_ground_percentage / 100, 0.1), 0.1)]
  d[, .date := as.Date(utc_start_time)]
  setorder(d, player_id, .date)
  d
}

sfx_of <- function(d) if (all(paste0("epv_", CH, "_oadj") %in% names(d))) "_oadj" else "_adj"

estimate <- function(d, engine) {
  sfx <- sfx_of(d)
  hi <- d[time_on_ground_percentage > TOG_MIN]
  out <- vector("list", length(CH))
  for (i in seq_along(CH)) {
    c <- CH[i]
    col <- paste0("epv_", c, sfx)

    # --- prior_games: within / between variance, on the TOG-restricted arm ---
    x <- hi[[col]] / hi$tog_safe
    ok <- is.finite(x)
    dd <- data.table(pid = hi$player_id[ok], x = x[ok])
    agg <- dd[, .(n = .N, m = mean(x), ss = sum((x - mean(x))^2)), by = pid][n >= 3]
    s2w <- sum(agg$ss) / sum(agg$n - 1)
    tau2 <- var(agg$m) - s2w * mean(1 / agg$n)
    pg <- if (is.finite(tau2) && tau2 > 0) s2w / tau2 else NA_real_

    # --- decay: shift-based lag correlation on the full frame ---
    ld <- data.table(pid = d$player_id, dt = d$.date, x = d[[col]] / d$tog_safe)
    ld <- ld[is.finite(x)]
    setorder(ld, pid, dt)
    r <- numeric(MAX_LAG); nn <- integer(MAX_LAG); dy <- numeric(MAX_LAG)
    for (k in seq_len(MAX_LAG)) {
      ld[, `:=`(x2 = shift(x, k, type = "lead"), d2 = shift(dt, k, type = "lead"),
                p2 = shift(pid, k, type = "lead"))]
      s <- ld[!is.na(x2) & p2 == pid]
      nn[k] <- nrow(s)
      r[k]  <- if (nrow(s) > 50) cor(s$x, s$x2) else NA_real_
      dy[k] <- if (nrow(s) > 50) mean(as.numeric(s$d2 - s$dt)) else NA_real_
    }
    ld[, c("x2", "d2", "p2") := NULL]
    fd <- data.table(r = r, n = nn, days = dy)[is.finite(r) & r > 0.01 & is.finite(days)]
    dec <- NA_real_; r2 <- NA_real_; slope_p <- NA_real_
    if (nrow(fd) >= 5) {
      m <- lm(log(r) ~ days, data = fd, weights = n)
      sm <- summary(m)
      sl <- coef(m)[["days"]]
      dec <- if (sl < 0) -1 / sl else NA_real_
      r2 <- sm$r.squared
      slope_p <- sm$coefficients["days", 4]
    }
    out[[i]] <- data.table(
      engine = engine, channel = c,
      reliability_lag1 = round(r[1], 4),
      prior_games = round(pg, 2),
      decay_days = round(dec, 0),
      decay_fit_r2 = round(r2, 3),
      decay_p = signif(slope_p, 3),
      n_players = nrow(agg)
    )
  }
  rbindlist(out)
}

say("=== prior_games and decay, estimated per engine ===")
say("prior_games from the TOG > ", TOG_MIN, " arm (the robustness check's verdict).")
say("decay from all games, ", MAX_LAG, " lags.")

v2 <- prep("epv3_player_game_v2.parquet")
v3 <- prep("epv3_player_game_v3.parquet")
est <- rbind(estimate(v2, "v2"), estimate(v3, "v3"))

say("")
say_dt(est, 12)

say("")
say("production for every channel: prior_games 3.0")
say("production decay: recv ", EPR_DECAY_RECV, " disp ", EPR_DECAY_DISP,
    " spoil ", EPR_DECAY_SPOIL, " hitout ", EPR_DECAY_HITOUT)

say("")
say("--- READ THE DECAY FIT QUALITY BEFORE USING A DECAY NUMBER ---")
say("A low r2 or a large p means the lag curve is nearly FLAT, and a flat curve")
say("is consistent with a very long decay OR with no decay at all. Those")
say("estimates are not usable; the prior_games numbers do not have this problem")
say("because they come from a variance ratio, not a curve shape.")
weak <- est[!is.finite(decay_fit_r2) | decay_fit_r2 < 0.5 | decay_p > 0.05]
if (nrow(weak) > 0) {
  say("")
  say("WEAKLY IDENTIFIED decay estimates (do not ship these):")
  say_dt(weak[, .(engine, channel, decay_days, decay_fit_r2, decay_p)], 10)
}

say("")
say("=== ANCHOR: v2 should need LESS shrinkage than v3 ===")
say("v2's channels carry box-score counts, which are mechanically more stable")
say("than sums of chain deltas (disposal reliability 0.267 against 0.077). If")
say("v2's prior_games is NOT lower, the estimator disagrees with the reliability")
say("measurement and one of them is wrong.")
cmp <- dcast(est, channel ~ engine, value.var = "prior_games")
say_dt(cmp, 6)
lower <- cmp[, sum(v2 < v3, na.rm = TRUE)]
say("channels where v2 < v3: ", lower, " of ", nrow(cmp),
    if (lower >= 3) "   PASS" else "   <- CHECK, this contradicts the reliability table")

say("")
say("=== PROPOSED CONSTANTS ===")
for (e in c("v2", "v3")) {
  say("")
  say("-- ", e, " --")
  ee <- est[engine == e]
  for (i in seq_len(nrow(ee))) {
    say(sprintf("  EPR_PRIOR_GAMES_%-7s  3.0  ->  %s",
                toupper(ee$channel[i]), ee$prior_games[i]))
  }
}
say("")
say("Decay is deliberately NOT proposed where the fit is weak. Gate prior_games")
say("first: it is the well-identified change and it is a 3-7x correction to a")
say("constant that is live in production today.")

saveRDS(est, file.path(OUT_DIR, "epv3_params_both_engines.rds"))
close(con)
cat("\nWrote ", OUT, "\n")
