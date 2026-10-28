# Grade the 2021-2025 trade periods AT THE TIME: each asset valued as it
# looked when the trade was made, by the same rules as 05_grade_trades.R
# uses for 2026 (Pete, 2026-10-28: two analyses, at the time and as it
# turned out; this is the first).
#
#   - Player: eight-season value from 03_project_value.R run as at the trade
#     year (`Rscript 03_project_value.R <year>` first), so the forecast starts
#     from his rating at the end of that season.
#   - Pick in that year's draft: the pick curve at the number quoted in the
#     trade. (2026 uses the AFL's final post-trade order; no such map exists
#     for earlier years, so a pick later pushed back by compensation picks or
#     bids is valued a few places too early.)
#   - Future pick: as in 05, from the original club's ladder finish that
#     season, half-way to the middle for next year and mid-round after that,
#     plus the Tasmania shift for 2027 and 2028 picks, discounted a season per
#     year of wait. A priority pick with a stated number uses that number.
#   - Value to club: the same stay curves as 05.
#
# Only trade-period trades are graded; draft-night swaps are left out
# (trade_tables.R). Source: Wikipedia "<year> AFL draft" pages, saved raw in
# torpdata/data/raw/trades_past/.
#
# Output per year: 10_assets_<year>.csv, 10_grades_<year>.csv (gitignored).

suppressMessages({ library(data.table) })
source("data-raw/08-trade-value/trade_tables.R")
source("data-raw/08-trade-value/draft_names.R")
suppressMessages(devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE))
raw_dir <- "C:/dev/torpverse/torpdata/data/raw/trades_past"
out_dir <- "C:/dev/torpverse/torpdata/data/raw/trade_value"
disc <- 0.9
TAS_SHIFT <- c(`2027` = 8, `2028` = 5)
years <- 2021:2025

curve <- fread(file.path(out_dir, "04_pick_curve.csv"))
pick_value <- function(n) {
  n <- pmin(pmax(round(n), 1), max(curve$pick))
  curve$v8_direct[match(n, curve$pick)]
}

# Stay curves, smoothed exactly as in 05 section 2b.
stay <- fread(file.path(out_dir, "stay_curve.csv"))[k %in% 1:8]
setorder(stay, group_type, group, k)
stay[n_in_league < 15, share_at_club := NA_real_]
stay[, share_at_club := nafill(share_at_club, type = "locf"), by = .(group_type, group)]
stopifnot(!anyNA(stay$share_at_club))
age_band <- function(x) fifelse(x < 24, "under24", fifelse(x < 28, "24-27", fifelse(x < 31, "28-30", "31plus")))
pick_band <- function(p) as.character(cut(p, c(0, 5, 10, 20, 30, 45, Inf),
                                          labels = c("1-5", "6-10", "11-20", "21-30", "31-45", "46plus")))
ps <- fread(file.path(out_dir, "04_pick_seasons.csv"))
ps[, band := pick_band(pick)]
ps <- merge(ps[, .(v = mean(v)), by = .(band, k)], stay[group_type == "draftee", .(band = group, k, share_at_club)],
            by = c("band", "k"))
fac <- ps[, .(club_factor = if (sum(v) > 0) sum(v * share_at_club) / sum(v) else mean(share_at_club)), by = band]

grade_year <- function(y) {
  a <- parse_wiki_trades(file.path(raw_dir, sprintf("wiki_%d_draft.html", y)), y)
  n_trades <- attr(a, "n_trades")
  stopifnot(!anyNA(a$from_club), !anyNA(a$to_club),
            !anyNA(a[type == "pick_draft", pick]), !anyNA(a[type == "future_pick", .(year, round, orig_club)]))

  # Future picks.
  lad <- as.data.table(get_afl_ladder(y))[, .(abbr = torp_team_abbr(team), ladder = as.integer(rank))]
  stopifnot(nrow(lad) == 18, !anyNA(lad$abbr))
  a[type == "future_pick", abbr := torp_team_abbr(orig_club)]
  a <- merge(a, lad, by = "abbr", all.x = TRUE, sort = FALSE)
  stopifnot(a[type == "future_pick", !anyNA(ladder)])
  a[type == "future_pick", slot := 19 - ladder]
  a[type == "future_pick", slot := fifelse(year == y + 1L, 0.5 * slot + 0.5 * 9.5, 9.5)]
  a[type == "future_pick", exp_pick := (round - 1) * 18 + slot +
      fifelse(as.character(year) %in% names(TAS_SHIFT), TAS_SHIFT[as.character(year)], 0)]
  a[type == "future_pick" & !is.na(stated_pick), exp_pick := stated_pick]
  a[type == "pick_draft", value := pick_value(pick)]
  a[type == "future_pick", value := pick_value(exp_pick) * disc^(year - y)]

  # Players, from the projection as at this season.
  proj <- fread(file.path(out_dir, sprintf("03_projected_value_%d.csv", y)))
  # "Malcolm Rosas Jr." is "Malcolm Rosas" in the ratings.
  pl <- match_draft_names(a[type == "player", .(row = .I, player = sub(" Jr\\.?$", "", item))], proj[, .(player_id, player_name)])
  a[type == "player", player_id := pl$player_id[order(pl$row)]]
  setnames(proj, paste0("torp_", y), "torp_then")
  a <- merge(a, proj[, .(player_id, age, torp_then, v8_mean)], by = "player_id", all.x = TRUE, sort = FALSE)
  a[type == "player", value := v8_mean]
  unm <- a[type == "player" & is.na(value)]
  cat(y, ": trades", n_trades, "| assets", nrow(a), "| players matched", a[type == "player" & !is.na(value), .N],
      "of", a[type == "player", .N], "\n")
  if (nrow(unm)) print(unm[, .(trade, item, to_club)])

  # Value to club.
  evl <- melt(proj[, c("player_id", grep("^ev_", names(proj), value = TRUE)), with = FALSE],
              id.vars = "player_id", value.name = "ev")[, k := as.integer(sub("ev_", "", variable)) - y]
  pv <- merge(evl, proj[, .(player_id, band = age_band(age + 1))], by = "player_id")
  pv <- merge(pv, stay[group_type == "moved", .(band = group, k, share_at_club)], by = c("band", "k"))
  pv <- pv[, .(value_club = sum(ev * disc^(k - 1) * share_at_club)), by = player_id]
  a[pv, on = "player_id", value_club := fifelse(type == "player", i.value_club, NA_real_)]
  a[type != "player", band := pick_band(fifelse(type == "pick_draft", pick, exp_pick))]
  a[fac, on = "band", value_club := fifelse(type != "player", value * i.club_factor, value_club)]

  g_in  <- a[, .(v_in = sum(value, na.rm = TRUE), v_in_club = sum(value_club, na.rm = TRUE)), by = .(trade, club = to_club)]
  g_out <- a[, .(v_out = sum(value, na.rm = TRUE), v_out_club = sum(value_club, na.rm = TRUE)), by = .(trade, club = from_club)]
  gr <- merge(g_in, g_out, by = c("trade", "club"), all = TRUE)
  for (cc in c("v_in", "v_in_club", "v_out", "v_out_club")) set(gr, which(is.na(gr[[cc]])), cc, 0)
  gr[, `:=`(net = v_in - v_out, net_club = v_in_club - v_out_club)]
  stopifnot(abs(gr[, sum(net)]) < 1e-6, uniqueN(gr$trade) == n_trades)

  fwrite(a, file.path(out_dir, sprintf("10_assets_%d.csv", y)))
  fwrite(gr, file.path(out_dir, sprintf("10_grades_%d.csv", y)))
  gr[, year := y]
  gr[]
}

all_gr <- rbindlist(lapply(years, grade_year))
cat("\nlargest wins at the time, all years (net = value received minus given, eight-season TORP units; higher is better for the club):\n")
print(all_gr[order(-net)][1:12, .(year, trade, club, v_in = round(v_in, 1), v_out = round(v_out, 1), net = round(net, 1))])
