# Export the 2026 draft-night page data as one JSON file for inthegame-blog
# (inthegame-blog#1059): the final draft order with each pick's points and
# TORP value, the seven priced bids, and the Hodge/McCartney what-if grid with the
# press's bid ranges.
#
# Inputs: 04_pick_curve.csv and 06_bid_pricing.csv / 06_bid_grid.csv / 06_bid_ranges.csv / 06_dvi.csv (run 04
# and 06 first), and trades_2026/official/final_order_1_80.csv (the AFL's final
# order, 2026-11-11; 06 reads the same file).
# Output: torpdata/data/raw/trade_value/draft_2026.json (gitignored); the blog
# session copies it in as a static file.
#
# Re-run after night one of the draft once the order and bids are refreshed.

suppressMessages({ library(data.table); library(jsonlite) })
raw_dir <- "C:/dev/torpverse/torpdata/data/raw"
out_dir <- file.path(raw_dir, "trade_value")

curve <- fread(file.path(out_dir, "04_pick_curve.csv"))
order <- fread(file.path(raw_dir, "trades_2026", "official", "final_order_1_80.csv"))
bids  <- fread(file.path(out_dir, "06_bid_pricing.csv"))
grid  <- fread(file.path(out_dir, "06_bid_grid.csv"))
ranges <- fread(file.path(out_dir, "06_bid_ranges.csv"))
cat("rows: curve", nrow(curve), "| order", nrow(order), "| bids", nrow(bids), "| grid", nrow(grid), "\n")
stopifnot(nrow(order) == 80, identical(order$pick, 1:80), !anyNA(order$holder), !anyNA(order$original_club),
          nrow(bids) == 7, nrow(grid) == 108, all(1:80 %in% curve$pick))

# TORP value: expected eight-season value of a player taken at that pick
# (games x time on ground x rating above replacement, 10% discount a year).
picks <- merge(order[, .(pick, holder, original_club)],
               curve[, .(pick, torp_value = round(v8_direct, 1))], by = "pick")
# Points from the checked list in 06 (the curve's own dvi column is incomplete).
dvi <- fread(file.path(out_dir, "06_dvi.csv"))
picks <- merge(picks, dvi, by = "pick", all.x = TRUE)
picks[is.na(points), points := 0]  # the Draft Value Index stops at pick 54
stopifnot(nrow(picks) == 80, !anyNA(picks$torp_value), all(diff(picks$points) <= 0),
          all(diff(picks$torp_value) <= 0))

out <- list(
  generated = format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
  draft_dates = c("2026-11-19", "2026-11-20"),
  notes = list(
    order = "AFL final order, 11 November 2026 (80 picks). Compensation picks are added after night one.",
    torp_value = "Expected eight-season TORP value of a player taken at the pick, discounted 10% a year.",
    points = "Draft Value Index points (0 from pick 55).",
    bids = "Bid positions from Cal Twomey's November form guide (AFL.com.au, 11 November 2026): a ranking, not a mock, so they will move. Boi at 30 is the first pick after Hawthorn's 29.",
    can_match = "The club can match if its shortfall is 412 points or less.",
    bid_grid = "A bid priced at every pick 1-54 for Hodge (Hawthorn) and McCartney (North Melbourne); bid_ranges says where the press places them."),
  picks = picks,
  bids = bids,
  bid_grid = grid,
  bid_ranges = ranges)
dest <- file.path(out_dir, "draft_2026.json")
write_json(out, dest, auto_unbox = TRUE, pretty = TRUE, na = "null", dataframe = "rows")

back <- fromJSON(dest)
stopifnot(nrow(back$picks) == 80, nrow(back$bids) == 7, nrow(back$bid_grid) == 108, nrow(back$bid_ranges) == 2)
cat("wrote", dest, sprintf("(%.1f KB)", file.size(dest) / 1024), "\n")
print(head(back$picks, 5))
