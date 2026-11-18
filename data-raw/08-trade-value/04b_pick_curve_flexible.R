# Pick value curve: does a flexible shape, or a longer draft history, say picks
# 5-20 are worth more than our log-line curve (04_pick_curve.R) and the AFL index?
# Prompted by Nick Welch's empirical curve (first 8 seasons, 2011-2017 drafts),
# which values pick 10 at about 54% of pick 1 against our 41% (Pete, 2026-11-17).
#
# Two checks:
#   A. Our own eight-season value (04_pick_seasons.csv: 2020 and 2021 drafts),
#      refitted with a natural spline in log(pick) instead of the straight line.
#      Answers: is our match with the AFL index the data, or the fixed shape?
#   B. Long history (Footywire, 1991-2015 National Drafts, careers essentially
#      complete): career games, and games beyond 50 and 100 (a short career
#      treated as replacement level), each fitted as a log line and as a spline.
#      Games are not value, so this bounds the shape rather than settling it.
# Every curve is put on the DVI scale (pick 1 = 3,000).
#
# Run from torp/: Rscript data-raw/08-trade-value/04b_pick_curve_flexible.R
# Output: torpdata/data/raw/trade_value/04b_pick_curve_flexible.csv (gitignored)

suppressMessages({ library(data.table); library(splines) })
raw <- "C:/dev/torpverse/torpdata/data/raw"
out_dir <- file.path(raw, "trade_value")
dvi <- fread(file.path(out_dir, "06_dvi.csv"))
show <- c(1, 2, 3, 5, 10, 15, 20, 25, 30, 40, 50)
grid <- data.table(pick = 1:60)
scale_to <- function(v) round(3000 * pmax(v, 0) / pmax(v[1], 1e-9))
fit_both <- function(dt, y, label) {
  f_log <- lm(reformulate("log(pick)", y), data = dt)
  f_spl <- lm(reformulate("ns(log(pick), df = 4)", y), data = dt)
  data.table(curve = label, n = nrow(dt), pick = grid$pick,
             log_line = scale_to(predict(f_log, newdata = grid)),
             spline = scale_to(predict(f_spl, newdata = grid)))
}

# ---- A. Our eight-season value, two drafts ----
ps <- fread(file.path(out_dir, "04_pick_seasons.csv"))
v8 <- ps[, .(v8 = sum(v)), by = .(draft_year, pick)]
cat("A. our value data:", nrow(v8), "picks from drafts", paste(sort(unique(v8$draft_year)), collapse = ", "), "\n")
stopifnot(nrow(v8) > 50, all(v8$draft_year %in% 2020:2021))
a <- fit_both(v8, "v8", "A: our 8-season value (2020-21 drafts)")

# ---- B. Long history, career games ----
fw <- fread(file.path(raw, "footywire_drafts.csv"))[is_pass == FALSE]
h <- fw[draft_year %in% 1991:2015 & !is.na(games_since_drafted) & (is.na(flag) | flag != "R") & pick <= 60]
h[, `:=`(g = games_since_drafted, g50 = pmax(games_since_drafted - 50, 0), g100 = pmax(games_since_drafted - 100, 0))]
cat("B. long history:", nrow(h), "picks, drafts 1991-2015, picks 1-60\n")
stopifnot(nrow(h) > 1000)
b <- rbind(fit_both(h, "g", "B1: career games"),
           fit_both(h, "g50", "B2: games beyond 50"),
           fit_both(h, "g100", "B3: games beyond 100"))
# Welch's window, as far as complete careers allow (2011-2015)
hw <- h[draft_year %in% 2011:2015]
b <- rbind(b, fit_both(hw, "g50", "B4: games beyond 50, 2011-15 drafts only"))

res <- rbind(a, b)
res <- merge(res, dvi[, .(pick, afl_dvi = points)], by = "pick", all.x = TRUE)
fwrite(res, file.path(out_dir, "04b_pick_curve_flexible.csv"))

cat("\nValue of each pick as a share of pick 1, on the DVI scale (pick 1 = 3,000; higher = pick worth more):\n")
wide <- dcast(res[pick %in% show], pick + afl_dvi ~ curve, value.var = "spline")
print(wide)
cat("\nSame, straight log line (our current form):\n")
print(dcast(res[pick %in% show], pick ~ curve, value.var = "log_line"))
cat("\nPick 10 as % of pick 1 (spline): ")
print(res[pick == 10, .(curve, n, pct = round(100 * spline / 3000))])
cat("AFL index pick 10:", round(100 * dvi[pick == 10, points] / 3000), "%; Welch (read off chart) about 54%\n")
