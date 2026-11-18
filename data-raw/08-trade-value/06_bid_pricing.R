# Price the 2026 father-son and academy bids in draft points and in TORP value
# (plan decision (d): price a bid by the draft points the club gives up).
#
# Inputs (torpdata/data/raw/draft_2026/FINDINGS.md, researched 2026-10-28):
#   - Draft Value Index points for picks 1-54 (0 from pick 55), checked
#     against two independent pages and AFL article text;
#   - 2026 bid rules (AFL, 2026-04-29): 1st-2nd +20% and 3rd-4th +10% on bids
#     at picks 1-18; 11th-18th -10% on bids at picks 1-36; at most two picks
#     to match a bid up to pick 36, a bid after 36 is matched with the next
#     pick; deficit up to 412 points, taken off next year's first-round pick;
#   - the AFL's final draft order, picks 1-80, locked after list lodgement
#     (AFL.com.au news 1640440, 2026-11-11; trades_2026/official/final_order_1_80.csv,
#     raw page saved beside it). The October order (final_order_1_60.csv) differed
#     at 9 of picks 51-60;
#   - bid positions from Cal Twomey's Late Mail Phantom Draft (AFL.com.au news
#     1641905, 2026-11-18; Pete's pick of set, 2026-11-19): Cochrane 1 (Essendon),
#     Walker 2 (Richmond), Matthews 4 (Melbourne; was 6 from his 11 November form
#     guide), Murray 14 (Melbourne), Justice 37. El Souki 23 and Boi 30 are not
#     bid in his top 30 and stay at Pete's 16 November positions.
# The TORP value of a pick is the eight-season curve from 04_pick_curve.R.
#
# Matching, simplified: the club hands over its next picks after the bid, in
# order, until their points cover the cost. Points left over on the last pick
# turn it into a later pick worth those points. Earlier bids pushing picks
# back by one place are ignored.
#
# Output: torpdata/data/raw/trade_value/06_bid_pricing.csv (gitignored)

suppressMessages(library(data.table))
out_dir <- "C:/dev/torpverse/torpdata/data/raw/trade_value"
# Drop the last run's stamp first: if this run stops part-way, 09 must not export.
unlink(file.path(out_dir, "06_run_stamp.csv"))

dvi <- c(3000, 2481, 2178, 1962, 1795, 1659, 1543, 1443, 1355, 1276, 1205, 1140, 1080, 1024,
         973, 924, 879, 836, 796, 757, 721, 686, 653, 621, 590, 561, 533, 505, 479, 454, 429,
         405, 382, 360, 338, 317, 297, 277, 257, 238, 220, 202, 184, 167, 150, 134, 118, 102,
         86, 71, 57, 42, 28, 14)
stopifnot(length(dvi) == 54, dvi[1] == 3000, dvi[25] == 590, all(diff(dvi) < 0))
fwrite(data.table(pick = 1:54, points = dvi), file.path(out_dir, "06_dvi.csv"))  # for 09_export_draft_json.R
dvi_at <- function(p) ifelse(p <= 54, dvi[pmin(p, 54)], 0)
# Where a part-used pick slides to: the first pick worth no more than the points
# left on it, so a club can never come out with a pick worth more than it kept.
slide_to <- function(pts) if (pts <= 0) NA_integer_ else if (pts > dvi[1]) 1L else min(c(which(dvi <= pts), 55L))

curve <- fread(file.path(out_dir, "04_pick_curve.csv"))
torp_at <- function(p) ifelse(is.na(p), 0, curve$v8_direct[match(pmin(p, max(curve$pick)), curve$pick)])

# Draft order, picks 1-80: the AFL's final order (the file 09 also reads, so the
# two scripts cannot disagree). Each club's picks are checked against the
# club-by-club list in the same AFL article.
order_file <- "C:/dev/torpverse/torpdata/data/raw/trades_2026/official/final_order_1_80.csv"
order_tbl <- fread(order_file)[, .(pick, club)]
club_picks <- list(
  ADEL = c(15, 32, 35, 43, 54), BL = c(21, 27, 28, 45), CARL = c(13, 18, 31, 50, 59, 67, 73),
  COLL = c(11, 30, 63, 64, 65), ESS = c(1, 17, 25, 69, 75, 78, 80), FRE = c(19, 40, 58, 61, 68),
  GEEL = c(36, 55, 60), GC = c(8, 70, 72, 77), GWS = c(9, 20, 26, 57), HAW = c(29, 37, 66, 74),
  MELB = c(4, 14, 48, 51, 79), NMFC = c(33, 49, 62), PORT = c(7, 12, 22, 24, 34, 46),
  RICH = c(2, 5, 44, 76), STK = c(10, 16, 41), SYD = c(6, 38, 39, 47), WCE = c(3, 23, 56),
  WB = c(42, 52, 53, 71))
# Set to FALSE once the order file is edited after night one (compensation picks,
# matched bids pushing picks back): the 11 November list no longer applies then.
ORDER_IS_AFL_11NOV <- TRUE
n_order <- nrow(order_tbl)
stopifnot(n_order >= 54, identical(order_tbl$pick, seq_len(n_order)), !anyNA(order_tbl$club),
          all(order_tbl$club %in% names(club_picks)))
if (ORDER_IS_AFL_11NOV) {
  differs <- names(club_picks)[!vapply(names(club_picks), function(k)
    identical(as.numeric(order_tbl$pick[order_tbl$club == k]), club_picks[[k]]), logical(1))]
  if (n_order != 80 || length(differs))
    stop("order file does not match the AFL's 11 November list (", n_order, " picks; clubs that differ: ",
         paste(differs, collapse = ", "), "). If it was edited after night one, set ORDER_IS_AFL_11NOV <- FALSE.")
} else {
  message("NOTE: order file NOT checked against the AFL's 11 November club list (ORDER_IS_AFL_11NOV is FALSE).")
}
cat("draft order:", n_order, "picks,", uniqueN(order_tbl$club), "clubs (", order_file, ")\n")

# Candidates and the matching club's 2026 FINISHING POSITION, which counts finals:
# grand finalists are 1st-2nd (+20%), losing preliminary finalists 3rd-4th (+10%).
# AFL.com.au news 1508308 (2026-04-29): "the flag-winning Swans (with the 20 per
# cent loading for finishing first)" and "Made a preliminary final (third or
# fourth)?". Brisbane was 3rd on the home-and-away ladder and won the flag, so it
# is 1 here; until 2026-11-16 it was 3 and its bids were priced 10% too cheap.
# The draft order agrees: Brisbane's own first-round pick is last (20), Fremantle
# 19, Sydney 18, Hawthorn 17. For every other club here the ladder and the
# finishing position fall in the same band.
cands <- data.table(
  player = c("Dougie Cochrane", "Cody Walker", "Ethan Matthews", "Caylen Murray", "Khaled El Souki", "Keenan Boi", "Blake Justice"),
  club   = c("PORT", "CARL", "GWS", "BL", "WB", "HAW", "ESS"),
  link   = c("NGA", "father-son", "academy", "academy", "NGA", "NGA", "NGA"),
  finish = c(15, 10, 12, 1, 8, 4, 18),
  bid    = c(1, 2, 4, 14, 23, 30, 37))
stopifnot(all(cands$club %in% order_tbl$club),
          cands[club == "BL", all(finish == 1)])  # premiers: 20% loading, see above

factor_for <- function(finish, bid) {
  if (finish <= 2 && bid <= 18) return(1.2)
  if (finish <= 4 && bid <= 18) return(1.1)
  if (finish >= 11 && bid <= 36) return(0.9)
  1
}

price_bid <- function(player, club, link, finish, bid) {
  f <- factor_for(finish, bid)
  cost <- round(dvi_at(bid) * f)
  own <- order_tbl$pick[order_tbl$club == club & order_tbl$pick > bid]  # plain vectors: `club` is also a column
  used <- integer(0); leftover_pick <- NA_integer_; remaining <- cost; deficit <- 0
  if (order_tbl$club[bid] == club) {
    # The club holds the bid pick itself: it just takes him there, no bid to match.
    used <- bid; f <- 1; cost <- dvi_at(bid); remaining <- 0
  } else if (bid > 36) {
    used <- if (length(own)) own[1] else nrow(order_tbl) + 1L  # matched with the next pick, no points (past the listed order if the club has none left)
  } else {
    for (p in head(own, 2)) {
      used <- c(used, p)
      if (dvi_at(p) >= remaining) {
        leftover_pick <- slide_to(dvi_at(p) - remaining); remaining <- 0; break
      }
      remaining <- remaining - dvi_at(p)
    }
    deficit <- remaining
  }
  # A deficit comes off next year's first-round pick: price it as the drop
  # from an average 2027 first-rounder (pick ~18 after the Tasmania shift)
  # to the pick worth that many fewer points, discounted one season.
  p27 <- 18L
  deficit_torp <- if (deficit > 0) 0.9 * (torp_at(p27) - torp_at(slide_to(dvi_at(p27) - deficit))) else 0
  given_torp <- sum(torp_at(used)) - torp_at(leftover_pick) + deficit_torp
  data.table(player, club, link, finish, bid, factor = f, cost_points = cost,
             picks_used = paste(used, collapse = "+"),
             leftover_becomes = ifelse(is.na(leftover_pick), "-", as.character(leftover_pick)),
             deficit_points = deficit, can_match = deficit <= 412,
             player_torp = round(torp_at(bid), 1), given_torp = round(given_torp, 1),
             gain_torp = round(torp_at(bid) - given_torp, 1),
             points_discount = dvi_at(bid) - cost)
}

res <- rbindlist(lapply(seq_len(nrow(cands)), function(i) do.call(price_bid, as.list(cands[i]))))
fwrite(res, file.path(out_dir, "06_bid_pricing.csv"))
cat("bid pricing (points = Draft Value Index; TORP = eight-season value units; gain = player minus what was given, higher is better for the club):\n")
print(res)

# What-if grid for the two prospects with no firm bid position: price a bid at
# every pick 1-54 (the last pick with Draft Value Index points) for the club
# that can match. Both were dual eligible and
# have nominated one club, so only that club can match on the night:
# Cooper Hodge chose Hawthorn (father-son) over the Brisbane academy (SEN,
# 2026-09-07); Aidan McCartney chose North Melbourne (father-son) over the
# Sydney academy (Zero Hanger, 2026-08-28). Finishes from the 2026
# home-and-away ladder. Read off the row at the bid once phantoms place him.
# Where the press places them (neither is in Twomey's October top 30, AFL.com.au):
# Hodge "looms as a late selection" (AFL.com.au Inside Trading, Josh Gabelich,
# early October; no pick number given); McCartney "could attract a bid in the
# 40s" (Zero Hanger, Jonty Ralphsmith, 2026-10-13).
ranges <- data.table(
  player = c("Cooper Hodge", "Aidan McCartney"),
  club   = c("HAW", "NMFC"),
  bid_from = c(NA_integer_, 40L), bid_to = c(NA_integer_, 49L),
  said   = c("late selection; may not attract a bid", "could attract a bid in the 40s"),
  source = c("AFL.com.au Inside Trading (Josh Gabelich), early October 2026",
             "Zero Hanger (Jonty Ralphsmith), 13 October 2026"))
fwrite(ranges, file.path(out_dir, "06_bid_ranges.csv"))
dual <- data.table(
  player = c("Cooper Hodge", "Aidan McCartney"),
  club   = c("HAW", "NMFC"),
  link   = c("father-son", "father-son"),
  finish = c(4, 14))
grid <- rbindlist(lapply(seq_len(nrow(dual)), function(i)
  rbindlist(lapply(1:54, function(b) do.call(price_bid, c(as.list(dual[i]), bid = b))))))
fwrite(grid, file.path(out_dir, "06_bid_grid.csv"))
cat("\nwhat-if grid written:", nrow(grid), "rows (2 nominated clubs x bids 1-54)\n")
stopifnot(nrow(grid) == 108)
# Stamp for 09_export_draft_json.R, written last: 09 refuses to export unless this
# run finished on the order file as it is now, so it cannot pair a new order with
# bids priced on an old one.
# The script's own md5 goes in too: `cands` and the bid ranges live in this file,
# so an edit here that was never run also has to block the export.
script_file <- "C:/dev/torpverse/torp/data-raw/08-trade-value/06_bid_pricing.R"
stopifnot(file.exists(script_file))
fwrite(data.table(order_md5 = unname(tools::md5sum(order_file)),
                  script_md5 = unname(tools::md5sum(script_file)), n_order = n_order,
                  n_bids = nrow(res), n_grid = nrow(grid)),
       file.path(out_dir, "06_run_stamp.csv"))
print(dcast(grid, bid ~ paste(player, club), value.var = "can_match")[bid >= 36])
