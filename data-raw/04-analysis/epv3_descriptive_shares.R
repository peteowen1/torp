# The descriptive decomposition: what share of a game each sub-category is.
#
# Raw `epv` conserves to the margin at 0.9879 (t 44.9) where `epv_adj` converts
# at 0.5951 -- the rating-layer transformations destroy conservation, with the
# opponent adjustment and level centring alone costing 0.886 -> 0.595. So the
# descriptive product already exists in the pipeline; it is the RAW channels,
# and nothing downstream looks at them.
#
# This measures the honest answer to "good variance across sub-categories", at
# the layer where the question is well posed, across the population fixes:
#
#   share_raw     each channel's share of the variance in team-match totals
#   conversion    points of margin per unit of that channel -- 1.0 is the null,
#                 since the channels are already denominated in expected points
#   share_points  share after calibrating, i.e. how much of the MARGIN each
#                 channel explains
#
# If a channel converts at ~1.0 then share_raw and share_points agree and the
# distinction stops mattering -- which is the state to aim for, and the state
# that says the metric is measuring what it claims.
#
# ~3 min, cached frames only.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_descriptive_shares.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 45) for (l in capture.output(print(utils::head(x, n)))) say(l)

res <- as.data.table(load_results(TRUE))
tgt <- res[, .(match_id = as.character(match_id), home = home_team_name,
               away = away_team_name, margin = home_score - away_score)][is.finite(margin)]

CH <- c("epv_recv", "epv_disp", "epv_spoil")
LBL <- c("recv", "disp", "contest")

analyse <- function(f, label) {
  p <- file.path(OUT_DIR, f)
  if (!file.exists(p)) { say(""); say("MISSING: ", f); return(NULL) }
  d <- as.data.table(read_parquet(p))
  ts <- d[, lapply(.SD, sum, na.rm = TRUE), .SDcols = CH, by = .(match_id, team)]
  h <- merge(tgt, ts, by.x = c("match_id", "home"), by.y = c("match_id", "team"))
  a <- merge(tgt, ts, by.x = c("match_id", "away"), by.y = c("match_id", "team"))
  m <- merge(h[, c("match_id", "margin", CH), with = FALSE],
             a[, c("match_id", CH), with = FALSE], by = "match_id", suffixes = c("_h", "_a"))
  for (v in CH) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  m[, d_tot := Reduce(`+`, lapply(CH, function(v) get(paste0("d_", v))))]

  co <- summary(lm(as.formula(paste("margin ~ 0 +", paste0("d_", CH, collapse = " + "))),
                   data = m))$coefficients
  sdv <- vapply(CH, function(v) sd(m[[paste0("d_", v)]]), numeric(1))
  pts <- sdv * co[, 1]
  tot <- summary(lm(margin ~ 0 + d_tot, data = m))$coefficients

  say(""); say("=== ", label, " ===")
  say_dt(data.table(channel = LBL,
                    conversion = round(co[, 1], 3), t = round(co[, 3], 1),
                    share_raw_pct = round(100 * sdv^2 / sum(sdv^2), 1),
                    share_points_pct = round(100 * pts^2 / sum(pts^2), 1)), 4)
  say(sprintf("  TOTAL -> margin %.4f  (t %.1f, R2 %.3f)   sd %.1f vs margin 39.85",
              tot[1, 1], tot[1, 3], summary(lm(margin ~ 0 + d_tot, data = m))$r.squared,
              sd(m$d_tot)))
  data.table(arm = label, conv_recv = round(co[1, 1], 3), conv_disp = round(co[2, 1], 3),
             conv_cont = round(co[3, 1], 3),
             raw_recv = round(100 * sdv[1]^2 / sum(sdv^2), 1),
             raw_disp = round(100 * sdv[2]^2 / sum(sdv^2), 1),
             raw_cont = round(100 * sdv[3]^2 / sum(sdv^2), 1),
             total_conv = round(tot[1, 1], 3))
}

say("=== Descriptive sub-category shares, RAW layer ===")
say("run at ", format(Sys.time()))
say("Channels are already in expected points, so conversion 1.0 is the null.")

rows <- rbindlist(list(
  analyse("epv3_fin_pgd_ship.parquet",       "all outcomes, team alloc (shipping)"),
  analyse("epv3_duel_pgd_duel_team.parquet", "genuine duels, team alloc"),
  analyse("epv3_duel_pgd_duel_ledger.parquet", "genuine duels, ledger alloc"),
  analyse("epv3_duel_pgd_duel_none.parquet", "genuine duels, no debit spreading")
), fill = TRUE)

say("")
say("=== SIDE BY SIDE ===")
say_dt(rows, 6)
say("")
say("HOW TO READ IT. Where conversion is near 1.0, share_raw and share_points")
say("agree and the channel is measuring what it claims. Where conversion is far")
say("below 1.0, the channel is being discounted by the regression because it is")
say("measured with error -- and share_points understates what actually happened")
say("in the game.")
say("")
say("The descriptive question Pete asked -- 'good variance across sub-categories'")
say("-- is answered by share_raw, but ONLY the arms whose conversions sit near")
say("1.0 have earned the right to that reading.")

saveRDS(rows, file.path(OUT_DIR, "epv3_descriptive_shares.rds"))
close(con)
cat("\nDone\n")
