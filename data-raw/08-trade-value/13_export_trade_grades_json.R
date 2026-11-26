# Export the public trade-grades page data as one JSON file for inthegame-blog:
# 2021-2023 trades graded as they turned out, with what each club received.
# Plan and the reason for the years: torpverse/docs/plans/TRADE-GRADES-PAGE.md
# (a year goes public once half its value is settled; 2024 and 2025 wait).
#
# Inputs: 12_assets_<year>.csv and 12_grades_<year>.csv (run 12 first).
# Output: torpdata/data/raw/trade_value/trade_grades.json (gitignored); the blog
# session copies it in as afl/trade-grades.json.
#
# At-the-time values (value, torp_then, net_then) are left out on purpose: those
# grades stay private until the salary bias has a caveat or a fix.

suppressMessages({ library(data.table); library(jsonlite) })
out_dir <- "C:/dev/torpverse/torpdata/data/raw/trade_value"
YEARS <- 2021:2023

blank_na <- function(x) { x[x == ""] <- NA_character_; x }  # fread reads empty strings as ""

assets <- rbindlist(lapply(YEARS, function(y)
  fread(file.path(out_dir, sprintf("12_assets_%d.csv", y)))[, year := y]), fill = TRUE)
grades <- rbindlist(lapply(YEARS, function(y)
  fread(file.path(out_dir, sprintf("12_grades_%d.csv", y)))[, year := y]))
cat("rows: assets", nrow(assets), "| grades", nrow(grades),
    "| trades", uniqueN(grades[, .(year, trade)]), "\n")
print(grades[, .(trades = uniqueN(trade)), by = year])
stopifnot(nrow(assets) > 0, nrow(grades) > 0, !anyNA(assets$v_turned), !anyNA(grades$net_turned))

assets[, `:=`(draftee = blank_na(as.character(draftee)), how = blank_na(how))]
stopifnot(!anyNA(assets$how))
# Rows valued partly by a forecast: still at the club (forecast to the window's
# end) or traded on (the forecast for the rest of the window at that trade).
# Labels may end "(hand-traced)" (12, 2026-11-26); a pick traded on, or traded
# on and then spent on a bid, is valued with no forecast.
how_base <- sub(" (hand-traced)", "", assets$how, fixed = TRUE)
still <- grepl("still there", how_base) |
  (grepl("traded on", how_base) & !how_base %in% c("pick traded on", "traded on, then spent on a bid"))
assets[, `:=`(played = round(fifelse(is.na(realised), 0, realised), 1),
              value = round(v_turned, 1))]
# Forecast as the rounded difference, so played + forecast = value exactly in
# the file (rounding each on its own missed by 0.1 on 24 rows).
assets[, forecast := fifelse(still, round(value - played, 1), 0)]

# Checks: a forecast row is exactly played + forecast; each trade's
# club nets cancel; Dawson's trade is the known anchor; only the public years.
gap <- assets[still, abs(fifelse(is.na(realised), 0, realised) + after - v_turned)]
stopifnot(length(gap) > 0, max(gap) < 0.01)
stopifnot(grades[, abs(sum(net_turned)), by = .(year, trade)][, max(V1)] < 1e-6)
# Dawson's seasons played are fixed (408.1); his forecast moves with the model,
# so the net only has to stay a clear Adelaide win.
dawson <- grades[year == 2021 & trade == 11 & club == "Adelaide", net_turned]
dawson_played <- assets[year == 2021 & item == "Jordan Dawson", played]
# (net_turned counts Dawson at PLAYER_VALUE_FACTOR: about +460 unscaled, about +205 at 0.5.)
stopifnot(length(dawson) == 1, dawson > 150, length(dawson_played) == 1, abs(dawson_played - 408.1) < 0.05)
stopifnot(setequal(unique(grades$year), YEARS), nrow(unique(grades[, .(year, trade)])) == 79)
# Show traded players at the share they count for in the grade, so each club's
# rows add up to its v_in. Draftees (pick rows) are shown in full.
source("C:/dev/torpverse/torp/data-raw/08-trade-value/trade_constants.R")
assets[type == "player", `:=`(played = round(played * PLAYER_VALUE_FACTOR, 1),
                              value = round(value * PLAYER_VALUE_FACTOR, 1))]
assets[, still_ := still]
assets[type == "player", forecast := fifelse(still_, round(value - played, 1), 0)]
assets[, still_ := NULL]
stopifnot(assets[, .(s = sum(value)), by = .(year, trade, to_club)][
  grades, on = .(year, trade, to_club = club), nomatch = NULL][, max(abs(s - v_in))] < 0.1 * 20)

trades <- lapply(split(grades, by = c("year", "trade"), sorted = TRUE), function(g) {
  y <- g$year[1]; t <- g$trade[1]
  list(year = y, trade = t, clubs = lapply(seq_len(nrow(g)), function(i) {
    a <- assets[year == y & trade == t & to_club == g$club[i],
                .(item, type, from_club, how, played, forecast, value, draftee, final_pick)]
    list(club = g$club[i], net = round(g$net_turned[i], 1), v_in = round(g$v_in[i], 1),
         v_out = round(g$v_out[i], 1), assets = a[order(-abs(value))])
  }))
})
names(trades) <- NULL
stopifnot(assets[still, all(abs(played + forecast - value) < 1e-9)])
stopifnot(sum(vapply(trades, function(t) sum(vapply(t$clubs, function(c) nrow(c$assets), 0L)), 0L)) == nrow(assets))

out <- list(
  generated = format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
  years = YEARS,
  as_of = 2026L,
  notes = list(
    value = "Value is games played x time on ground x how far the player's TORP rating was above a replacement-level player, summed over 8 seasons from the trade, with later seasons counting 10% less each year. Picks count as the player the club drafted with them.",
    forecast = "Players are valued on the seasons they played for the club plus a forecast of the rest of the 8-year window: from now if they are still there, or from the day the club traded them on. For these years the forecast is 20% (2021) to 40% (2022) of the value. A forecast can be below zero when a player is expected to rate below a replacement-level player. It is a middle estimate: players taken with the first few draft picks have, on average, done better than it expects.",
    untraced = "We follow each pick to the player drafted with it. A pick that a club held in a draft where it matched an academy or father-son bid, and that was never used on a selection, is taken as spent on that bid: it is valued by how the bid player turned out against what his pick was expected to bring. This is inferred, because the draft records do not say which picks paid for most bids. Any other pick we could not follow keeps its value on the day of the trade.",
    salary = "A player a club receives in a trade counts at half his value, picks in full. That half stands in for what clubs pay players, which the value leaves out: across 2021-2026, clubs taking on a player gave up about a third of his value in picks. Players drafted with a pick count in full, being on rookie contracts.",
    not_2026 = "This year's trades are not graded here yet."),
  trades = trades)
dest <- file.path(out_dir, "trade_grades.json")
write_json(out, dest, auto_unbox = TRUE, pretty = TRUE, na = "null", dataframe = "rows", digits = NA)

back <- fromJSON(dest, simplifyVector = FALSE)
stopifnot(length(back$trades) == 79, all(vapply(back$trades, `[[`, 0L, "year") %in% YEARS),
          !grepl("net_then|torp_then", paste(readLines(dest, warn = FALSE), collapse = "")))
cat("wrote", dest, sprintf("(%.1f KB, %d trades)", file.size(dest) / 1024, length(back$trades)), "\n")
