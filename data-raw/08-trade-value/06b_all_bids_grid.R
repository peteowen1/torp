# Price each of the seven tied prospects at every pick 1-54, using 06's own
# price_bid() (sourced, so the rules are the production ones, not a copy).
suppressMessages(library(data.table))
e <- new.env()
invisible(capture.output(sys.source("C:/dev/torpverse/torp/data-raw/08-trade-value/06_bid_pricing.R", envir = e)))
cands <- e$cands
cat("candidates:", nrow(cands), "| order picks:", nrow(e$order_tbl), "\n")
stopifnot(nrow(cands) == 7)

full <- rbindlist(lapply(seq_len(nrow(cands)), function(i)
  rbindlist(lapply(1:54, function(b) {
    a <- as.list(cands[i]); a$bid <- b
    do.call(e$price_bid, a)
  }))))
stopifnot(nrow(full) == 7 * 54, !anyNA(full$can_match))

# Anchor: the grid row at each prospect's current page position must equal 06's own table.
chk <- merge(full, e$res, by = c("player", "bid"), suffixes = c("", ".res"))
stopifnot(nrow(chk) == 7, all(chk$cost_points == chk$cost_points.res),
          all(chk$deficit_points == chk$deficit_points.res), all(chk$gain_torp == chk$gain_torp.res))
cat("anchor: grid equals 06's table at all 7 current bid positions\n")

out <- "C:/dev/torpverse/torpdata/data/raw/trade_value/06_all_bids_grid.csv"
fwrite(full, out); cat("wrote", out, nrow(full), "rows\n")

# Summary: earliest pick at which the club can match, and whether can_match is
# monotone after that (if not, the threshold is not a clean line).
summ <- full[, {
  first_ok <- if (any(can_match)) min(bid[can_match]) else NA_integer_
  .(page_bid = cands$bid[match(player[1], cands$player)],
    first_matchable = first_ok,
    clean_line = all(can_match[bid >= first_ok]),
    own_pick_before = { o <- e$order_tbl$pick[e$order_tbl$club == club[1]]; if (length(o)) min(o) else NA_integer_ })
}, by = .(player, club)]
print(summ)

# Rows around each page position (2 either side), for the facts pack.
near <- full[, .SD[bid >= pmax(1, cands$bid[match(player[1], cands$player)] - 3) &
                   bid <= cands$bid[match(player[1], cands$player)] + 3], by = player
             ][, .(player, bid, cost_points, picks_used, leftover_becomes, deficit_points, can_match, gain_torp)]
print(near, nrows = 100)
