# Draft pick value curve (plan step 4, pick side).
#
# Two views of the same question, "what is pick p worth":
#   1. Direct, in the same units as 03_project_value.R: for the 2020 and 2021
#      National Drafts, whose first five seasons (2021-2025, 2022-2026) are all
#      inside the TORP window, sum games x (TORP - replacement) over those five
#      seasons, discounted 10% a year. A draftee who never played or was
#      delisted scores 0 for those seasons, so busts are counted, not dropped.
#   2. Long history, games only: career games by pick for the 1991-2015 drafts
#      (careers essentially complete), from the Footywire harvest.
# Both are smoothed as a function of log(pick) and put on the AFL Draft Value
# Index scale (pick 1 = 3,000) beside the DVI points we have checked.
#
# Output: torpdata/data/raw/trade_value/04_pick_curve.csv (gitignored)

suppressMessages({ library(data.table); library(splines) })
invisible(capture.output(source("data-raw/08-trade-value/02_age_survival.R")))
repl <- -1.38   # 03_project_value.R: mean first-season TORP of mature pickups
disc <- 0.9

fw <- fread("C:/dev/torpverse/torpdata/data/raw/footywire_drafts.csv")[is_pass == FALSE]

# ---- 1. Direct five-season value, 2020 and 2021 drafts ----
d <- fw[draft_year %in% 2020:2021, .(draft_year, pick, player, flag, fw_games = games_since_drafted)]
d <- match_draft_names(d, end)   # draft_names.R, sourced by 02_age_survival.R
cat("2020-21 draftees:", nrow(d), " matched to a TORP player:", d[!is.na(player_id), .N], "\n")
# A draftee with Footywire games but no TORP match is a name mismatch, not a
# bust. List them so they can be fixed rather than scored as zero.
miss <- d[is.na(player_id) & fw_games > 0]
cat("unmatched draftees who HAVE played (name mismatches):", nrow(miss), "\n")
if (nrow(miss)) print(miss[, .(draft_year, pick, player, fw_games)])

seas <- end[, .(player_id, season, games, torp)]
d5 <- d[!is.na(player_id) | fw_games == 0]       # drop only the mismatches
d5[, k := 0L]
grid <- d5[, .(k = 1:5), by = .(draft_year, pick, player, player_id, flag)]
grid[, season := draft_year + k]
grid <- merge(grid, seas, by = c("player_id", "season"), all.x = TRUE)
grid[is.na(games) | is.na(torp), `:=`(games = 0L, torp = repl)]
grid[, v := games * (torp - repl) * disc^(k - 1)]
dv <- grid[, .(v5 = sum(v), games5 = sum(games)), by = .(draft_year, pick, player, flag)]
cat("picks scored:", nrow(dv), " with zero five-season games:", dv[games5 == 0, .N], "\n")

f_dir <- lm(v5 ~ log(pick), data = dv)
cat("direct: v5 = ", round(coef(f_dir)[1], 1), " + ", round(coef(f_dir)[2], 1), " x log(pick)",
    "  (R2 ", round(summary(f_dir)$r.squared, 3), ", n ", nrow(dv), ")\n", sep = "")
dv[, band := cut(pick, c(0, 5, 10, 20, 30, 45, 100))]
cat("\nobserved five-season value by pick band (2020-21 drafts):\n")
print(dv[, .(n = .N, mean_v5 = round(mean(v5), 1), sd = round(sd(v5), 1),
             share_negative = round(mean(v5 < 0), 2), share_no_games = round(mean(games5 == 0), 2)),
         by = band][order(band)])

# ---- 1b. Eight seasons: observed through 2026, projected after ----
# Seasons up to 2026 are what happened; later seasons are the expected value
# from 03_project_value.R (0 for a draftee no longer rated at the end of 2026).
proj <- fread(file.path(out_dir, "03_projected_value_2026.csv"))
ev_long <- melt(proj[, c("player_id", grep("^ev_", names(proj), value = TRUE)), with = FALSE],
                id.vars = "player_id", variable.name = "season", value.name = "ev")
ev_long[, season := as.integer(sub("ev_", "", season))]
g8 <- d5[, .(k = 1:8), by = .(draft_year, pick, player, player_id, flag)]
g8[, season := draft_year + k]
g8 <- merge(g8, seas, by = c("player_id", "season"), all.x = TRUE)
g8 <- merge(g8, ev_long, by = c("player_id", "season"), all.x = TRUE)
g8[, v := fifelse(season <= 2026,
                  fifelse(is.na(games) | is.na(torp), 0, games * (torp - repl)),
                  fifelse(is.na(ev), 0, ev)) * disc^(k - 1)]
dv8 <- g8[, .(v8 = sum(v), v8_observed = sum(v[season <= 2026])), by = .(draft_year, pick, player)]
# Per-season (discounted) value by pick, so 05 can weight each season by the
# chance the draftee is still at the drafting club.
fwrite(g8[, .(draft_year, pick, k, v)], file.path(out_dir, "04_pick_seasons.csv"))
dv <- merge(dv, dv8, by = c("draft_year", "pick", "player"))
f_dir8 <- lm(v8 ~ log(pick), data = dv)
cat("\neight seasons: v8 = ", round(coef(f_dir8)[1], 1), " + ", round(coef(f_dir8)[2], 1),
    " x log(pick)  (R2 ", round(summary(f_dir8)$r.squared, 3), ", n ", nrow(dv), ")\n", sep = "")
cat("share of eight-season value already observed (seasons to 2026):",
    round(sum(dv$v8_observed) / sum(dv$v8), 2), "\n")
print(dv[, .(n = .N, mean_v5 = round(mean(v5), 1), mean_v8 = round(mean(v8), 1)), by = band][order(band)])

# ---- 2. Long history: career games by pick, 1991-2015 ----
h <- fw[draft_year %in% 1991:2015 & !is.na(games_since_drafted)]
f_g <- lm(games_since_drafted ~ log(pick), data = h)
cat("\nlong history: career games = ", round(coef(f_g)[1], 1), " + ", round(coef(f_g)[2], 1),
    " x log(pick)  (n ", nrow(h), ")\n", sep = "")
h[, band := cut(pick, c(0, 5, 10, 20, 30, 45, 100))]
print(h[, .(n = .N, mean_games = round(mean(games_since_drafted), 1),
            p_50plus = round(mean(games_since_drafted >= 50), 2),
            p_100plus = round(mean(games_since_drafted >= 100), 2),
            p_zero = round(mean(games_since_drafted == 0), 2)), by = band][order(band)])

# ---- 3. Curves on the DVI scale ----
show <- c(1, 2, 3, 5, 6, 10, 15, 20, 25, 30, 40, 50, 60)
dvi_checked <- c(`1` = 3000, `2` = 2481, `5` = 1795, `6` = 1659, `25` = 590)
# Floored at 0: a club can always pass or delist, so a pick cannot cost it value.
pv <- data.table(pick = 1:110)
pv[, v5_direct := pmax(predict(f_dir, newdata = .SD), 0)]
pv[, v8_direct := pmax(predict(f_dir8, newdata = .SD), 0)]
pv[, games_hist := pmax(predict(f_g, newdata = .SD), 0)]
pv[, `:=`(v5_dvi_scale = round(3000 * v5_direct / v5_direct[1]),
          v8_dvi_scale = round(3000 * v8_direct / v8_direct[1]),
          games_dvi_scale = round(3000 * games_hist / games_hist[1]),
          dvi = dvi_checked[as.character(pick)])]
num <- c("v5_direct", "v8_direct", "games_hist"); pv[, (num) := lapply(.SD, round, 1), .SDcols = num]
cat("\npick value curves, scaled so pick 1 = 3,000 (DVI = AFL Draft Value Index, checked points only):\n")
print(pv[pick %in% show])
fwrite(pv, file.path(out_dir, "04_pick_curve.csv"))

# Anchor (chosen before looking): value falls with pick in every view.
stopifnot(coef(f_dir)[2] < 0, coef(f_dir8)[2] < 0, coef(f_g)[2] < 0)
