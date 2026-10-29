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
still <- grepl("still there", assets$how)
assets[, `:=`(played = round(fifelse(is.na(realised), 0, realised), 1),
              value = round(v_turned, 1))]
# Forecast as the rounded difference, so played + forecast = value exactly in
# the file (rounding each on its own missed by 0.1 on 24 rows).
assets[, forecast := fifelse(still, round(value - played, 1), 0)]

# Checks: a player still at the club is exactly played + forecast; each trade's
# club nets cancel; Dawson's trade is the known anchor; only the public years.
gap <- assets[still, abs(fifelse(is.na(realised), 0, realised) + after - v_turned)]
stopifnot(length(gap) > 0, max(gap) < 0.01)
stopifnot(grades[, abs(sum(net_turned)), by = .(year, trade)][, max(V1)] < 1e-6)
# Dawson's seasons played are fixed (408.1); his forecast moves with the model,
# so the net only has to stay a clear Adelaide win.
dawson <- grades[year == 2021 & trade == 11 & club == "Adelaide", net_turned]
dawson_played <- assets[year == 2021 & item == "Jordan Dawson", played]
stopifnot(length(dawson) == 1, dawson > 400, length(dawson_played) == 1, abs(dawson_played - 408.1) < 0.05)
stopifnot(setequal(unique(grades$year), YEARS), nrow(unique(grades[, .(year, trade)])) == 79)

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
    forecast = "Players still at the club are valued on seasons played so far plus a forecast of the seasons left in the 8-year window. For these years the forecast is 18% (2021) to 39% (2023) of the value. A forecast can be below zero when a player is expected to rate below a replacement-level player. It is a middle estimate: players taken with the first few draft picks have, on average, done better than it expects.",
    untraced = "We follow each pick to the player drafted with it. Where we could not (mostly late picks used to match bids), the pick keeps its value on the day of the trade.",
    salary = "Grades do not count what clubs pay players. A club that takes on a big contract gets the player's full value here, so these grades lean towards clubs that took on players.",
    not_2026 = "This year's trades are not graded here yet."),
  trades = trades)
dest <- file.path(out_dir, "trade_grades.json")
write_json(out, dest, auto_unbox = TRUE, pretty = TRUE, na = "null", dataframe = "rows", digits = NA)

back <- fromJSON(dest, simplifyVector = FALSE)
stopifnot(length(back$trades) == 79, all(vapply(back$trades, `[[`, 0L, "year") %in% YEARS),
          !grepl("net_then|torp_then", paste(readLines(dest, warn = FALSE), collapse = "")))
cat("wrote", dest, sprintf("(%.1f KB, %d trades)", file.size(dest) / 1024, length(back$trades)), "\n")
