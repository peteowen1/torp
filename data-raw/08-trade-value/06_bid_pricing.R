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
#   - the draft order at the end of the trade period, picks 1-60 (SEN 2026-10-14,
#     identical pick by pick to the AFL page in trades_2026/official/final_order_1_60.csv);
#   - bid positions from one mock (Reading the Play, 2026-09-21).
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

# Draft order, picks 1-60, final after the trade period (FINDINGS.md section 4).
order_tbl <- data.table(pick = 1:60, club = c(
  "ESS", "RICH", "WCE", "MELB", "RICH", "SYD", "PORT", "GC", "GWS", "STK", "COLL", "PORT", "CARL",
  "MELB", "ADEL", "STK", "ESS", "CARL", "FRE", "GWS", "BL", "PORT", "WCE", "PORT", "ESS", "GWS",
  "BL", "BL", "HAW", "COLL", "CARL", "ADEL", "NMFC", "PORT", "ADEL", "GEEL", "HAW", "SYD", "SYD",
  "FRE", "STK", "WB", "ADEL", "RICH", "BL", "PORT", "SYD", "MELB", "NMFC", "CARL", "STK", "MELB",
  "WB", "SYD", "WB", "ADEL", "GEEL", "WCE", "GWS", "FRE"))

# Candidates and the matching club's 2026 home-and-away finish (FINDINGS.md section 3).
cands <- data.table(
  player = c("Dougie Cochrane", "Cody Walker", "Ethan Matthews", "Caylen Murray", "Khaled El Souki", "Blake Justice"),
  club   = c("PORT", "CARL", "GWS", "BL", "WB", "ESS"),
  link   = c("NGA", "father-son", "academy", "academy", "NGA", "NGA"),
  finish = c(15, 10, 12, 3, 8, 18),
  bid    = c(1, 2, 7, 14, 30, 37))

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
    used <- if (length(own)) own[1] else 61L  # matched with the next pick, no points (61+ = past the listed order)
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

# What-if grid for the two prospects with no bid range yet: price a bid at
# every pick 1-40 for the club that can match. Both were dual eligible and
# have nominated one club, so only that club can match on the night:
# Cooper Hodge chose Hawthorn (father-son) over the Brisbane academy (SEN,
# 2026-09-07); Aidan McCartney chose North Melbourne (father-son) over the
# Sydney academy (Zero Hanger, 2026-08-28). Finishes from the 2026
# home-and-away ladder. Read off the row at the bid once phantoms place him.
dual <- data.table(
  player = c("Cooper Hodge", "Aidan McCartney"),
  club   = c("HAW", "NMFC"),
  link   = c("father-son", "father-son"),
  finish = c(4, 14))
grid <- rbindlist(lapply(seq_len(nrow(dual)), function(i)
  rbindlist(lapply(1:40, function(b) do.call(price_bid, c(as.list(dual[i]), bid = b))))))
fwrite(grid, file.path(out_dir, "06_bid_grid.csv"))
cat("\nwhat-if grid written:", nrow(grid), "rows (2 nominated clubs x bids 1-40)\n")
print(dcast(grid, bid ~ paste(player, club), value.var = "can_match")[bid <= 25])
