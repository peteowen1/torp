# Export the 2026 draft-night page data as one JSON file for inthegame-blog
# (inthegame-blog#1059): the final draft order with each pick's points and
# TORP value, the six priced bids, and the Hodge/McCartney what-if grid.
#
# Inputs: 04_pick_curve.csv and 06_bid_pricing.csv / 06_bid_grid.csv / 06_dvi.csv (run 04
# and 06 first), and trades_2026/official/final_order_1_60.csv.
# Output: torpdata/data/raw/trade_value/draft_2026.json (gitignored); the blog
# session copies it in as a static file.
#
# Re-run after night one of the draft once the order and bids are refreshed.

suppressMessages({ library(data.table); library(jsonlite) })
raw_dir <- "C:/dev/torpverse/torpdata/data/raw"
out_dir <- file.path(raw_dir, "trade_value")

curve <- fread(file.path(out_dir, "04_pick_curve.csv"))
order <- fread(file.path(raw_dir, "trades_2026", "official", "final_order_1_60.csv"))
bids  <- fread(file.path(out_dir, "06_bid_pricing.csv"))
grid  <- fread(file.path(out_dir, "06_bid_grid.csv"))
cat("rows: curve", nrow(curve), "| order", nrow(order), "| bids", nrow(bids), "| grid", nrow(grid), "\n")
stopifnot(nrow(order) == 60, identical(order$pick, 1:60), !anyNA(order$holder),
          nrow(bids) == 6, nrow(grid) == 160, all(1:60 %in% curve$pick))

# TORP value: expected eight-season value of a player taken at that pick
# (games x time on ground x rating above replacement, 10% discount a year).
picks <- merge(order[, .(pick, holder, original_club)],
               curve[, .(pick, torp_value = round(v8_direct, 1))], by = "pick")
# Points from the checked list in 06 (the curve's own dvi column is incomplete).
dvi <- fread(file.path(out_dir, "06_dvi.csv"))
picks <- merge(picks, dvi, by = "pick", all.x = TRUE)
picks[is.na(points), points := 0]  # the Draft Value Index stops at pick 54
stopifnot(nrow(picks) == 60, !anyNA(picks$torp_value), all(diff(picks$points) <= 0))

out <- list(
  generated = format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
  draft_dates = c("2026-11-19", "2026-11-20"),
  notes = list(
    order = "Final order after the trade period (AFL page, 2026-10-14). Compensation picks are added after night one.",
    torp_value = "Expected eight-season TORP value of a player taken at the pick, discounted 10% a year.",
    points = "Draft Value Index points (0 from pick 55).",
    bids = "Bid positions from one mock draft (Reading the Play, 2026-09-21); they will move.",
    can_match = "The club can match if its shortfall is 412 points or less."),
  picks = picks,
  bids = bids,
  bid_grid = grid)
dest <- file.path(out_dir, "draft_2026.json")
write_json(out, dest, auto_unbox = TRUE, pretty = TRUE, na = "null", dataframe = "rows")

back <- fromJSON(dest)
stopifnot(nrow(back$picks) == 60, nrow(back$bids) == 6, nrow(back$bid_grid) == 160)
cat("wrote", dest, sprintf("(%.1f KB)", file.size(dest) / 1024), "\n")
print(head(back$picks, 5))
