# Why does a unit of contest EPV convert to less than a point of margin?
#
# THE QUESTION, and it is the one that decides whether the contest channel is
# small because the GAME says so or because our MEASUREMENT is bad.
#
# Every channel is already denominated in expected points -- delta_epv is an
# expected-points difference. So a unit of any channel should convert to ~1.0
# points of margin by construction. Measured, they do not:
#
#   EPV layer, all-outcome population:  recv 0.599  disp 0.612  contest 0.260
#
# Disposal converts at 0.61 and contest at 0.26. Neither is 1.0, and contest is
# less than half of disposal. Two candidate explanations and they imply opposite
# responses:
#
#   ATTENUATION. The channel is measured with error, so its regression
#   coefficient is biased toward zero by the reliability ratio. The fix is to
#   measure it better -- and the duel-population change already moved the EPR
#   contest coefficient 0.524 -> 2.900, which is what attenuation looks like
#   when you remove noise from a regressor.
#
#   NOT REALLY POINTS. The contest credit is not an unbiased estimate of the
#   expected-points swing it claims to be, so it should not convert at 1.0 and
#   no amount of cleaning will make it.
#
# The discriminating test is whether conversion moves TOWARD 1.0 as the
# population gets cleaner. Attenuation predicts yes; a construction problem
# predicts no.
#
# Also measured here, because it is the same question asked of the whole metric:
# does the SUM of the channels convert at 1.0? If a team's total EPV in a match
# does not equal its margin, the metric is not conserving to the game, and
# "1 unit = 1 point" is a fitted constant rather than a property.
#
# Reads cached player-game frames. ~3 min, no rebuild.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_conversion_diagnosis.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 45) for (l in capture.output(print(utils::head(x, n)))) say(l)

res <- as.data.table(load_results(TRUE))
xg  <- as.data.table(load_xg(TRUE))
tgt <- merge(res[, .(match_id = as.character(match_id), home = home_team_name,
                     away = away_team_name, margin = home_score - away_score)],
             xg[, .(match_id = as.character(match_id), xmargin = xscore_diff)],
             by = "match_id")[is.finite(margin)]

CH <- c("epv_recv_adj", "epv_disp_adj", "epv_spoil_adj")

# Team-match sums of the ADJUSTED channels. player_game_data is lineup-correct
# by construction and epv_*_adj is already TOG-scaled, so this needs no lineup
# filter and no TOG weighting -- adding either double-counts.
team_diffs <- function(d) {
  ts <- d[, lapply(.SD, sum, na.rm = TRUE), .SDcols = CH, by = .(match_id, team)]
  h <- merge(tgt, ts, by.x = c("match_id", "home"), by.y = c("match_id", "team"))
  a <- merge(tgt, ts, by.x = c("match_id", "away"), by.y = c("match_id", "team"))
  m <- merge(h[, c("match_id", "margin", "xmargin", CH), with = FALSE],
             a[, c("match_id", CH), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  for (v in CH) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  m[, d_total := Reduce(`+`, lapply(CH, function(v) get(paste0("d_", v))))]
  m
}

fit_report <- function(m, label) {
  say(""); say("=== ", label, "  (", nrow(m), " matches) ===")
  f <- lm(as.formula(paste("margin ~ 0 +", paste0("d_", CH, collapse = " + "))), data = m)
  co <- summary(f)$coefficients
  sdv <- vapply(CH, function(v) sd(m[[paste0("d_", v)]]), numeric(1))
  pts <- sdv * co[, 1]
  say("per-channel, against ACTUAL margin:")
  say_dt(data.table(channel = c("recv", "disp", "contest"),
                    conversion = round(co[, 1], 4), t = round(co[, 3], 2),
                    sd_raw = round(sdv, 3),
                    share_calibrated_pct = round(100 * pts^2 / sum(pts^2), 1),
                    share_raw_pct = round(100 * sdv^2 / sum(sdv^2), 1)), 5)

  # The single most important line: does the TOTAL convert at 1.0?
  ft <- lm(margin ~ 0 + d_total, data = m)
  ct <- summary(ft)$coefficients
  say(sprintf("TOTAL EPV -> margin: %.4f  (t %.2f, R2 %.3f)   [1.000 = conserves]",
              ct[1, 1], ct[1, 3], summary(ft)$r.squared))
  say(sprintf("  team-match total EPV: mean %+.2f  sd %.2f   |  margin sd %.2f",
              mean(m$d_total) / 2, sd(m$d_total), sd(m$margin)))
  invisible(co[, 1])
}

# --------------------------------------------------------------------------
say("=== Does a unit of each channel convert to a point of margin? ===")
say("run at ", format(Sys.time()))
say("")
say("All channels are denominated in EXPECTED POINTS already, so a conversion")
say("of 1.0 is the null hypothesis, not an aspiration. Departures from 1.0 are")
say("the thing to explain.")

arms <- list(
  list(f = "epv3_fin_pgd_ship.parquet",      lab = "all outcomes (the shipping build)"),
  list(f = "epv3_duel_pgd_duel_team.parquet", lab = "GENUINE DUELS only, team alloc"),
  list(f = "epv3_duel_pgd_duel_none.parquet", lab = "genuine duels, debit unallocated")
)
co <- list()
for (a in arms) {
  p <- file.path(OUT_DIR, a$f)
  if (!file.exists(p)) { say(""); say("MISSING: ", a$f); next }
  d <- as.data.table(read_parquet(p))
  co[[a$lab]] <- fit_report(team_diffs(d), a$lab)
}

say("")
say("=== THE DISCRIMINATING COMPARISON ===")
say("If contest conversion moves TOWARD 1.0 as the population is cleaned, the")
say("small share was ATTENUATION -- a measurement problem with a measurement")
say("fix. If it does not move, the credit is not really expected points and no")
say("amount of cleaning will make it convert.")
if (length(co) >= 2) {
  cmp <- rbindlist(lapply(names(co), function(n)
    data.table(arm = n, recv = round(co[[n]][1], 3),
               disp = round(co[[n]][2], 3), contest = round(co[[n]][3], 3))))
  say_dt(cmp, 5)
}

say("")
say("=== AND THE SAME QUESTION FOR THE WHOLE METRIC ===")
say("A descriptive metric should conserve: a team's total EPV in a match should")
say("BE its margin, not a fitted multiple of it. Net Points does this by")
say("construction -- summing every 76ers player's Net Points in a game gives +2,")
say("and they won 118-116. Read the TOTAL EPV -> margin lines above: anything")
say("far from 1.000 says torp's EPV is on its own scale, and every 'points'")
say("statement about it depends on a fitted constant rather than on the game.")

close(con)
cat("\nDone\n")
