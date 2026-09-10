# Can kick_len be measured WITHOUT reading the outcome? (issue #210)
# =============================================================================
# kick_len is currently sqrt((out_x - x)^2 + (out_y - y)^2) where out_* is the
# first NON-in-flight row after the disposal -- i.e. where the next possession
# happened. After a turnover that is often the far end of the ground (median
# 115m on the rows the model is most certain about), so the feature reads the
# outcome.
#
# But the code SKIPS the in-flight rows on the way there, and those are exactly
# the ones that describe the ball in the air:
#   Contest Target, Kick Inside 50 Result, Kick Into F50, Shot At Goal,
#   Inside 50, Rebound 50, Kickin long/short/play on, ...
#
# If an in-flight row carries the ball's LANDING coordinates, that is a property
# of the KICK, not of who won it -- legitimate for splitting credit
# retrospectively. This measures whether that route exists and whether it is
# actually cleaner.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_kick_len_repair_options.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
ch  <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
setorder(ch, match_id, display_order)

say("=== do in-flight rows carry coordinates at all? ===")
inf <- ch[description %chin% CHAINS_INFLIGHT_DESCS]
print(inf[, .(n = .N, pct_x_present = round(100 * mean(!is.na(x)), 1),
              pct_y_present = round(100 * mean(!is.na(y)), 1)),
          by = description][order(-n)])

# --- for each Kick, what is the FIRST following row, and is it in-flight? -----
c2 <- ch[, .(match_id, display_order, description, team_id, x, y)]
for (k in 1:6) for (s in c("description", "team_id", "x", "y"))
  c2[, (paste0("f", k, "_", s)) := shift(get(s), k, type = "lead"), by = match_id]

kk <- c2[description == "Kick" & !is.na(x)]
say("\nkicks in chains: ", format(nrow(kk), big.mark = ","))
say("first following row is an in-flight annotation: ",
    round(100 * mean(kk$f1_description %chin% CHAINS_INFLIGHT_DESCS, na.rm = TRUE), 1), "%")
say("\nwhat the first following row is:")
print(head(kk[, .N, by = f1_description][order(-N)], 12))

# --- candidate A: distance to the first IN-FLIGHT row that has coordinates ----
inflight_x <- function(dt) {
  out <- rep(NA_real_, nrow(dt)); outy <- rep(NA_real_, nrow(dt))
  for (k in 1:3) {
    d  <- dt[[paste0("f", k, "_description")]]
    xx <- dt[[paste0("f", k, "_x")]]; yy <- dt[[paste0("f", k, "_y")]]
    hit <- is.na(out) & !is.na(d) & d %chin% CHAINS_INFLIGHT_DESCS &
      is.finite(xx) & is.finite(yy)
    out[hit] <- xx[hit]; outy[hit] <- yy[hit]
  }
  list(x = out, y = outy)
}
ifl <- inflight_x(kk)
kk[, `:=`(ifx = ifl$x, ify = ifl$y)]
say("\nkicks with an in-flight row carrying coordinates within 3 rows: ",
    round(100 * mean(is.finite(kk$ifx)), 1), "%")

# --- candidate B: the current definition, for comparison ---------------------
de <- as.data.table(build_disposal_events(ch, pbp))
de <- de[description == "Kick" & !is.na(turnover)]
cmp <- merge(de[, .(match_id, display_order, kick_len_now = kick_len, turnover)],
             kk[, .(match_id, display_order, ifx, ify, x, y)],
             by = c("match_id", "display_order"))
cmp[, kick_len_inflight := sqrt((ifx - x)^2 + (ify - y)^2)]
cmp <- cmp[is.finite(kick_len_inflight)]
say("\ncomparable rows: ", format(nrow(cmp), big.mark = ","))

say("\n=== how implausible is each definition? ===")
say("(a real kick is at most ~60m; anything beyond that is not a kick)")
for (v in c("kick_len_now", "kick_len_inflight")) {
  s <- cmp[[v]]
  say("  ", formatC(v, width = -20), " median ", formatC(median(s), format = "f", digits = 1),
      "  p95 ", formatC(quantile(s, .95), format = "f", digits = 1),
      "  max ", formatC(max(s), format = "f", digits = 1),
      "  share >60m ", formatC(100 * mean(s > 60), format = "f", digits = 1), "%")
}

say("\n=== the leakage test: does each definition predict the target? ===")
ll <- function(y, p) { p <- pmin(pmax(p, 1e-15), 1 - 1e-15)
  -mean(y * log(p) + (1 - y) * log(1 - p)) }
base <- mean(cmp$turnover)
say("  intercept only            : ", round(ll(cmp$turnover, rep(base, nrow(cmp))), 5))
for (v in c("kick_len_now", "kick_len_inflight")) {
  f <- stats::glm(stats::as.formula(paste("turnover ~", v)), data = cmp,
                  family = stats::binomial())
  say("  ", formatC(v, width = -25), " : ",
      round(ll(cmp$turnover, stats::predict(f, type = "response")), 5))
}
say("\n(a feature knowable before the ball lands should move this only a little;")
say(" the current one moved it 0.54031 -> 0.45151 on the full disposal set)")

say("\n=== turnover rate by the IN-FLIGHT length, for shape ===")
cmp[, b := cut(kick_len_inflight, c(-Inf, 5, 15, 25, 35, 45, 55, Inf))]
print(cmp[, .(n = .N, turnover_rate = round(100 * mean(turnover), 1)), by = b][order(b)])
