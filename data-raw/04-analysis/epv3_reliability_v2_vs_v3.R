# Are box-score channels mechanically more RELIABLE than chain-derived ones?
#
# HYPOTHESIS, formed after an earlier explanation failed its own test. cont_stop
# is the only v3 channel that is a box-score COUNT times a constant, and it is
# the only one with game-to-game reliability above 0.1 (rho 0.247 against
# 0.053-0.091 for the three chain channels). My first explanation -- that this
# was a ruck/non-ruck role marker -- was tested and REFUTED: within rucks only it
# read 0.243, essentially unchanged.
#
# The remaining explanation is mechanical rather than football: counting events
# is stable, summing their values is not. A player's hitout COUNT barely moves
# week to week; the total delta_epv of his disposals swings hard because a single
# kick can be worth +/- 3 points.
#
# If true this explains the whole session's central puzzle -- why v2 (chain +
# box) predicts better than v3 (chain only). Not because box weights are
# meaningful, but because box counts are LESS NOISY, and a noisy per-game
# measure carries less information into a rating no matter how well specified it
# is.
#
# THE TEST: v2's channels are box-heavy, v3's are chain-only, and they are built
# from the same matches. If the hypothesis holds, v2's channels have
# systematically higher reliability. If they do not, the hypothesis dies like the
# last one.
#
# PERFORMANCE: shift-based lag correlation, O(n) per lag, 8 lags, two frames.
# Nothing quadratic. ~30 seconds.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_reliability_v2_vs_v3.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

CH <- c("recv", "disp", "spoil", "hitout")
LBL <- c(recv = "recv", disp = "disp", spoil = "cont_aerial/spoil", hitout = "cont_stop/hitout")
MAX_LAG <- 8L

prep <- function(f) {
  d <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, f)))
  d[, tog_safe := pmax(fifelse(is.na(time_on_ground_percentage), 100,
                               time_on_ground_percentage) / 100, 0.1)]
  d[, .date := as.Date(utc_start_time)]
  setorder(d, player_id, .date)
  d
}
v2 <- prep("epv3_player_game_v2.parquet")
v3 <- prep("epv3_player_game_v3.parquet")

say("=== Reliability: box-score channels vs chain-derived channels ===")
say("v2 player-games ", format(nrow(v2), big.mark = ","),
    " | v3 ", format(nrow(v3), big.mark = ","))

# Reliability of a per-game measure = correlation between a player's consecutive
# games. Uses the RAW per-80 channel, before position adjustment, so the
# comparison is about the measure itself rather than the centring.
rel <- function(d, col, label, chan) {
  dd <- data.table(pid = d$player_id, dt = d$.date, x = d[[col]] / d$tog_safe)
  dd <- dd[is.finite(x)]
  setorder(dd, pid, dt)
  r <- numeric(MAX_LAG); n <- integer(MAX_LAG)
  for (k in seq_len(MAX_LAG)) {
    dd[, `:=`(x2 = shift(x, k, type = "lead"), p2 = shift(pid, k, type = "lead"))]
    s <- dd[!is.na(x2) & p2 == pid]
    n[k] <- nrow(s); r[k] <- if (nrow(s) > 50) cor(s$x, s$x2) else NA_real_
  }
  dd[, c("x2", "p2") := NULL]
  data.table(arm = label, channel = chan, r_lag1 = round(r[1], 4),
             r_lag4 = round(r[4], 4), r_lag8 = round(r[8], 4), n_lag1 = n[1])
}

say("")
say("=== raw channel reliability (correlation with the player's next game) ===")
tab <- rbindlist(c(
  lapply(CH, function(c) rel(v2, paste0("epv_", c), "v2 (chain + box)", LBL[[c]])),
  lapply(CH, function(c) rel(v3, paste0("epv_", c), "v3 (chain only)",  LBL[[c]]))
))
say_dt(dcast(tab, channel ~ arm, value.var = "r_lag1"), 8)

say("")
say("--- the composition of each channel, for reading the table above ---")
say("  recv        v2 = chain + 7 box terms   | v3 = chain only")
say("  disp        v2 = chain + 11 box terms  | v3 = chain only")
say("  spoil       v2 = 8 box terms, NO chain | v3 = chain only (aerial contest)")
say("  hitout      v2 = 3 box terms           | v3 = IDENTICAL to v2")
say("")
say("hitout is the control: it is the SAME formula in both engines, so its two")
say("numbers must match. If they do not, something other than composition is")
say("driving this table.")

h2 <- tab[arm == "v2 (chain + box)" & channel == LBL[["hitout"]]]$r_lag1
h3 <- tab[arm == "v3 (chain only)"  & channel == LBL[["hitout"]]]$r_lag1
say("hitout control: v2 ", h2, " vs v3 ", h3,
    if (isTRUE(abs(h2 - h3) < 1e-6)) "   MATCH" else "   <- MISMATCH, investigate")

say("")
say("=== THE TEST ===")
sp2 <- tab[arm == "v2 (chain + box)" & channel == LBL[["spoil"]]]$r_lag1
sp3 <- tab[arm == "v3 (chain only)"  & channel == LBL[["spoil"]]]$r_lag1
say("The cleanest contrast is the spoil slot: v2's is PURE BOX (8 count terms,")
say("no chain at all), v3's is PURE CHAIN (aerial contest surprise).")
say("  v2 pure-box   ", sp2)
say("  v3 pure-chain ", sp3)
if (is.finite(sp2) && is.finite(sp3)) {
  say("  ratio ", round(sp2 / sp3, 2), "x")
  if (sp2 > sp3 * 1.5) {
    say("")
    say("HYPOTHESIS SUPPORTED. A pure box-score channel is materially more")
    say("reliable game to game than a pure chain-derived one built from the same")
    say("matches. Counting events is stable; summing their values is not.")
    say("")
    say("Consequence for prediction: v3's chain channels need FAR more shrinkage")
    say("than v2's box ones, and a rating built only from chain deltas starts")
    say("from a worse signal-to-noise position no matter how well specified it")
    say("is. That is a better explanation of v3's +0.184 MAE than anything about")
    say("the contest channel's meaning.")
  } else {
    say("")
    say("HYPOTHESIS NOT SUPPORTED. Composition does not explain the reliability")
    say("gap, and cont_stop's high reliability remains unexplained. Do not use")
    say("this reasoning downstream.")
  }
}

say("")
say("=== decay of reliability with lag (does it fall, and how fast) ===")
say_dt(tab[, .(arm, channel, r_lag1, r_lag4, r_lag8)], 12)

saveRDS(tab, file.path(OUT_DIR, "epv3_reliability_compare.rds"))
close(con)
cat("\nWrote ", OUT, "\n")
