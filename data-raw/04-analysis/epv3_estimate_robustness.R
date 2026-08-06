# Robustness check on the EPR parameter estimates.
#
# The estimator divides by tog_safe = max(tog/100, 0.1) to get a per-80 rate, so
# a 10%-TOG cameo is multiplied by TEN. That inflates within-player variance and
# therefore prior_games, which is the headline number. Raised as a caveat on my
# own result; this settles it.
#
# Also revisits the failed anchor rather than explaining it away: cont_stop had
# the highest reliability (rho 0.247) where disposal was predicted to. If that is
# a ruck/non-ruck ROLE marker rather than measurement precision, then WITHIN
# rucks only its reliability should collapse. That is a testable prediction and
# it is tested here.
#
# PERFORMANCE: same shift-based O(n) per lag structure as the estimator. The TOG
# arms are subsets, so each is cheaper than the full run. No new bottleneck.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_estimate_robustness.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

CH <- c("recv", "disp", "spoil", "hitout")
LBL <- c(recv = "recv", disp = "disp", spoil = "cont_aerial", hitout = "cont_stop")
MAX_LAG <- 20L

pgd <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_player_game_v3.parquet")))
pgd <- adjust_epv_for_opponents(pgd)
if (isTRUE(EPV_LEVEL_CENTRE)) pgd <- centre_epv_by_position(pgd)
sfx <- if (all(paste0("epv_", CH, "_oadj") %in% names(pgd))) "_oadj" else "_adj"
pgd[, tog_safe := pmax(fcoalesce(time_on_ground_percentage / 100, 0.1), 0.1)]
pgd[, .date := as.Date(utc_start_time)]
setorder(pgd, player_id, .date)

say("=== Robustness of the EPR parameter estimates ===")
say("player-games ", format(nrow(pgd), big.mark = ","))
say("")
say("--- how much of the data is low-TOG (the amplification risk)? ---")
say_dt(pgd[, .(n = .N, pct = round(100 * .N / nrow(pgd), 1)),
           by = .(tog_band = cut(time_on_ground_percentage,
                                 c(-1, 25, 50, 70, 85, 101),
                                 labels = c("<25", "25-50", "50-70", "70-85", "85+")))][
           order(tog_band)], 8)

# ---- prior_games under three TOG regimes and two value definitions ---------
pg_est <- function(d, use_rate, label) {
  out <- vector("list", length(CH))
  for (i in seq_along(CH)) {
    c <- CH[i]
    v <- d[[paste0("epv_", c, sfx)]]
    x <- if (use_rate) v / d$tog_safe else v
    pid <- d$player_id
    ok <- is.finite(x)
    dd <- data.table(pid = pid[ok], x = x[ok])
    agg <- dd[, .(n = .N, m = mean(x), ss = sum((x - mean(x))^2)), by = pid][n >= 2]
    s2w <- sum(agg$ss) / sum(agg$n - 1)
    tau2 <- var(agg$m) - s2w * mean(1 / agg$n)
    out[[i]] <- data.table(arm = label, channel = LBL[[c]],
                           n_pg = nrow(dd), n_players = nrow(agg),
                           prior_games = if (is.finite(tau2) && tau2 > 0)
                             round(s2w / tau2, 2) else NA_real_)
  }
  rbindlist(out)
}

say("")
say("=== 1. prior_games sensitivity to the TOG floor ===")
say("If the 0.1 floor is driving the estimate, the TOG>50 and TOG>70 arms will")
say("differ sharply from the full one.")
res <- rbindlist(list(
  pg_est(pgd, TRUE, "all, per-80 rate"),
  pg_est(pgd[time_on_ground_percentage > 50], TRUE, "TOG>50, per-80 rate"),
  pg_est(pgd[time_on_ground_percentage > 70], TRUE, "TOG>70, per-80 rate"),
  pg_est(pgd, FALSE, "all, TOG-scaled value")
))
say_dt(dcast(res, channel ~ arm, value.var = "prior_games"), 8)
say("")
say("production is 3.0 for every channel.")

# ---- reliability under the same regimes ------------------------------------
rho_est <- function(d, label) {
  out <- vector("list", length(CH))
  for (i in seq_along(CH)) {
    c <- CH[i]
    dd <- data.table(pid = d$player_id, dt = d$.date,
                     x = d[[paste0("epv_", c, sfx)]] / d$tog_safe)
    dd <- dd[is.finite(x)]
    setorder(dd, pid, dt)
    n_lag <- integer(MAX_LAG); r_lag <- numeric(MAX_LAG); day_lag <- numeric(MAX_LAG)
    for (k in seq_len(MAX_LAG)) {
      dd[, `:=`(x2 = shift(x, k, type = "lead"), d2 = shift(dt, k, type = "lead"),
                p2 = shift(pid, k, type = "lead"))]
      sub <- dd[!is.na(x2) & p2 == pid]
      n_lag[k] <- nrow(sub)
      r_lag[k] <- if (nrow(sub) > 50) cor(sub$x, sub$x2) else NA_real_
      day_lag[k] <- if (nrow(sub) > 50) mean(as.numeric(sub$d2 - sub$dt)) else NA_real_
    }
    dd[, c("x2", "d2", "p2") := NULL]
    ld <- data.table(lag_games = seq_len(MAX_LAG), n = n_lag, r = r_lag, days = day_lag)
    f <- ld[is.finite(r) & r > 0.01 & is.finite(days)]
    dec <- NA_real_; rho <- NA_real_
    if (nrow(f) >= 5) {
      m <- lm(log(r) ~ days, data = f, weights = n)
      s <- coef(m)[["days"]]
      dec <- if (s < 0) -1 / s else NA_real_
      rho <- exp(coef(m)[["(Intercept)"]])
    }
    out[[i]] <- data.table(arm = label, channel = LBL[[c]],
                           r_lag1 = round(ld$r[1], 4),
                           rho = round(rho, 3), decay_days = round(dec, 0))
  }
  rbindlist(out)
}

say("")
say("=== 2. THE FAILED ANCHOR, tested rather than explained ===")
say("Prediction: cont_stop's high reliability (rho 0.247) is a ruck/non-ruck")
say("ROLE marker, not precision. If so, WITHIN rucks only it should COLLAPSE.")
say("If it does not collapse, my explanation was wrong and the anchor failure")
say("stands unexplained.")
say("")
ruck <- pgd[grepl("RUCK", position_group)]
say("ruck player-games: ", format(nrow(ruck), big.mark = ","),
    " | ruck players: ", uniqueN(ruck$player_id))
cmp <- rbindlist(list(rho_est(pgd, "all players"), rho_est(ruck, "rucks only")))
say_dt(dcast(cmp, channel ~ arm, value.var = "rho"), 8)
say("")
say("also r at lag 1:")
say_dt(dcast(cmp, channel ~ arm, value.var = "r_lag1"), 8)

cs_all <- cmp[arm == "all players" & channel == "cont_stop"]$rho
cs_rk  <- cmp[arm == "rucks only" & channel == "cont_stop"]$rho
say("")
say("cont_stop rho: all players ", cs_all, " -> rucks only ", cs_rk)
if (is.finite(cs_rk) && is.finite(cs_all) && cs_rk < cs_all * 0.6) {
  say("COLLAPSED as predicted. The reliability was role, not precision, and the")
  say("anchor was wrong rather than the method.")
} else {
  say("DID NOT COLLAPSE. My explanation was wrong -- the anchor failure stands")
  say("unexplained and the method needs scrutiny before these numbers are used.")
}

say("")
say("=== 3. decay sensitivity ===")
say_dt(dcast(cmp, channel ~ arm, value.var = "decay_days"), 8)

say("")
say("=== VERDICT ===")
say("The prior_games estimates are usable if the TOG arms agree to within about")
say("a factor of 1.5. Where they do not, the number is an artefact of the floor")
say("and the TOG>50 arm is the one to trust.")

saveRDS(list(prior_games = res, reliability = cmp),
        file.path(OUT_DIR, "epv3_estimate_robustness.rds"))
close(con)
cat("\nWrote ", OUT, "\n")
