# Price the 2026 bids IN SEQUENCE, on an order that moves as bids are matched.
#
# 06_bid_pricing.R prices every bid on the starting order. In the real draft a
# matched bid puts the player into the order at the bid pick and takes the picks
# used to pay out of it, so every later pick number changes: after Port matches
# a pick-1 bid on Cochrane with picks 7 and 12, Richmond's pick 2 is pick 3 and
# Carlton's 13 and 18 are 12 and 17. Rule confirmed with Pete on that example,
# 2026-11-16 (DECISIONS), including compensation picks.
#
# Rules, and where each comes from (AFL.com.au news 1508240 and 1508308,
# 2026-04-29, raw copies in torpdata/data/raw/draft_2026/):
#   - a bid is priced at the LIVE number of the pick it is made with;
#   - the matching club pays with its next picks after the bid, valued at their
#     live numbers, at most two up to pick 36; after 36, its next pick, no points;
#   - points left on the last pick used turn it into a later pick;
#   - a club that cannot match (short by more than 412) loses the player to the
#     bidder, and nothing else moves;
#   - compensation: a bottom-five club holding its own first-round pick, pushed
#     back by a matched bid, gets one extra pick "at the start of the second round".
#
# ASSUMPTIONS, not confirmed anywhere:
#   - picks are valued at their numbers at the moment of the bid, BEFORE the
#     matched player goes in. The AFL's worked examples fit this and Zero
#     Hanger's mock uses it ("CARL use Picks 12 and 17"), but neither states it.
#     Valued one place later instead, only one answer flips: El Souki in
#     scenario A goes from 391 short (can match) to 423 (cannot).
#   - compensation picks go together at the start of round two, worst club
#     first. Zero Hanger's mock (15 Nov) interleaves them with other picks.
#   - a leftover pick lands at the live number its remaining points are worth.
#   - NOT modelled: a club needs next year's first-round pick to run a deficit.
#
# A bid is entered as the STARTING pick it is made with (and so which club
# bids). If that pick has gone by then, the next pick still in the order bids.
#
# Output: torpdata/data/raw/trade_value/06c_bids_in_sequence.csv (gitignored)

suppressMessages(library(data.table))
e <- new.env()
invisible(capture.output(sys.source("C:/dev/torpverse/torp/data-raw/08-trade-value/06_bid_pricing.R", envir = e)))
dvi_at <- e$dvi_at; slide_to <- e$slide_to; torp_at <- e$torp_at; factor_for <- e$factor_for
out_dir <- e$out_dir
start <- fread(e$order_file)[, .(start = pick, club, original_club, holder)]
cat("starting order:", nrow(start), "picks | candidates:", nrow(e$cands), "\n")

# Bottom five by finishing position: the original owners of starting picks 1-6
# are Essendon, Richmond, West Coast, Port Adelaide (4 and 5), North Melbourne.
# Only a club still HOLDING its own first-round pick can be compensated.
bottom5_own <- start[start <= 6 & holder == original_club, .(club, start)]
cat("bottom-five clubs holding their own first-round pick:", paste(bottom5_own$club, collapse = ", "), "\n")
ROUND1_LAST_START <- 20L   # starting picks 1-20 are round one (two extra picks this year)

# The pick a bid is made with: the named starting pick if it is still there and
# unused, else the next unused pick, not held by the matching club, in the LIVE
# order after where that pick stood (behind the last surviving pick that started
# ahead of it; leftover and compensation picks have no starting number).
find_bid_pick <- function(ord, bd) {
  b <- which(ord$kind == "pick" & ord$start == bd$bid_start & !ord$taken)
  if (!length(b)) {
    ahead <- which(!is.na(ord$start) & ord$start < bd$bid_start)
    after <- if (length(ahead)) max(ahead) else 0L
    b <- which(!ord$taken & ord$club != bd$club & seq_len(nrow(ord)) > after)[1]
  }
  stopifnot(length(b) == 1, !is.na(b))
  b
}

run_sequence <- function(bids, label) {
  ord <- copy(start)[, .(club, start, kind = "pick", taken = FALSE)]
  comp_done <- FALSE; n_matched <- 0L; n_used <- 0L; n_left <- 0L; n_comp <- 0L
  res <- vector("list", nrow(bids))
  for (i in seq_len(nrow(bids))) {
    bd <- bids[i]
    b <- find_bid_pick(ord, bd)
    # Night two begins: hand out compensation picks once, before the first bid
    # made with a pick that sits at or behind the start of round two. Decided by
    # where the pick SITS: a bid entered at a round-two starting pick that has
    # gone can fall to a pick still inside round one, and that bid is a night-one
    # bid (found 2026-11-16: it was priced with the compensation picks already in).
    r2 <- which(ord$kind == "pick" & !is.na(ord$start) & ord$start > ROUND1_LAST_START)[1]
    if (!comp_done && b >= r2) {
      pushed <- bottom5_own[vapply(seq_len(.N), function(k) {
        now <- which(ord$start == start[k] & ord$kind == "pick")
        length(now) == 1 && now > start[k]
      }, logical(1))]
      if (nrow(pushed)) {
        at <- which(ord$kind == "pick" & !is.na(ord$start) & ord$start > ROUND1_LAST_START)[1]
        ord <- rbind(ord[seq_len(at - 1)],
                     data.table(club = pushed$club, start = NA_integer_, kind = "comp", taken = FALSE),
                     ord[at:nrow(ord)])
        n_comp <- nrow(pushed)
      }
      comp_done <- TRUE
      b <- find_bid_pick(ord, bd)
    }
    stopifnot(ord$club[b] != bd$club)   # a club does not bid on its own tied player
    # A bid can never be made with a pick ahead of one already used for an earlier bid.
    stopifnot(i == 1 || b > res[[i - 1]]$bid_live || !res[[i - 1]]$can_match)
    bidder <- ord$club[b]
    f <- factor_for(bd$finish, b); cost <- round(dvi_at(b) * f)
    own <- which(ord$club == bd$club & !ord$taken & seq_len(nrow(ord)) > b)
    used <- integer(0); left_pts <- 0; remaining <- cost
    if (b > 36) {
      used <- if (length(own)) own[1] else integer(0); remaining <- 0
    } else {
      for (p in head(own, 2)) {
        used <- c(used, p)
        if (dvi_at(p) >= remaining) { left_pts <- dvi_at(p) - remaining; remaining <- 0; break }
        remaining <- remaining - dvi_at(p)
      }
    }
    deficit <- remaining; can <- deficit <= 412
    left_pick <- if (left_pts > 0) slide_to(left_pts) else NA_integer_
    p27 <- 18L
    deficit_torp <- if (deficit > 0) 0.9 * (torp_at(p27) - torp_at(slide_to(dvi_at(p27) - deficit))) else 0
    given <- sum(torp_at(used)) - torp_at(left_pick) + deficit_torp
    res[[i]] <- data.table(scenario = label, player = bd$player, club = bd$club, bidder,
      bid_start = bd$bid_start, bid_live = b, factor = f, cost_points = cost,
      used_live = paste(used, collapse = "+"),
      used_start = paste(ifelse(is.na(ord$start[used]), ord$kind[used], ord$start[used]), collapse = "+"),
      leftover_live = left_pick, deficit_points = deficit, can_match = can,
      player_torp = round(torp_at(b), 1),
      # A club that cannot match gives nothing and gains nothing: the bidder takes him.
      given_torp = if (can) round(given, 1) else NA_real_,
      gain_torp = if (can) round(torp_at(b) - given, 1) else NA_real_)
    if (can) {
      # Player goes in at the bid pick; the picks used leave; the bidder and
      # everyone behind move back one.
      keep <- setdiff(seq_len(nrow(ord)), used)
      new <- rbind(ord[keep[keep < b]],
                   data.table(club = bd$club, start = NA_integer_, kind = "matched", taken = TRUE),
                   ord[keep[keep >= b]])
      if (!is.na(left_pick)) {
        at <- min(max(left_pick, b + 2L), nrow(new) + 1L)
        new <- rbind(new[seq_len(at - 1)],
                     data.table(club = bd$club, start = NA_integer_, kind = "leftover", taken = FALSE),
                     if (at <= nrow(new)) new[at:nrow(new)])
        n_left <- n_left + 1L
      }
      ord <- new; n_matched <- n_matched + 1L; n_used <- n_used + length(used)
    } else {
      ord[b, taken := TRUE]   # the bidder takes him; nothing else moves
    }
  }
  # Conservation: every pick is accounted for.
  stopifnot(nrow(ord) == nrow(start) + n_matched - n_used + n_left + n_comp)
  out <- rbindlist(res)
  attr(out, "comp") <- n_comp
  out
}

cands <- copy(e$cands)
cands[, bid_start := bid]   # the page position, read as the starting pick the bid is made with
setorder(cands, bid_start)

# Scenario A: Richmond bids on Walker with its first pick (starting pick 2).
A <- run_sequence(cands, "A: Richmond bids on Walker")
# Scenario B: Essendon, having lost Cochrane to a matched bid, bids on Walker
# with the same pick (starting pick 1, by then live pick 2).
cB <- copy(cands); cB[player == "Cody Walker", bid_start := 1L]
cB <- cB[order(bid_start, player != "Dougie Cochrane")]
B <- run_sequence(cB, "B: Essendon bids on Walker")

# Scenario P: Cal Twomey's Phantom Draft (AFL.com.au news 1640523, 2026-11-17,
# raw copy torpdata/data/raw/trades_2026/official/afl_1640523_*). Bids entered
# as the bidding club's starting pick: Essendon 1 on Cochrane, Richmond 2 on
# Walker (live 3), Melbourne 4 on Matthews (live 6), Melbourne 14 on Murray
# (live 13). Not bid inside his top 30, but named as possible: West Coast on
# Boi with a mid-20s pick (starting 23) and Port on El Souki (starting 24).
# Justice stays at 37 (Essendon "keen to match a bid", no position given).
cP <- copy(cands)
cP[player == "Ethan Matthews", bid_start := 4L]
cP[player == "Caylen Murray", bid_start := 14L]
cP[player == "Keenan Boi", bid_start := 23L]
cP[player == "Khaled El Souki", bid_start := 24L]
setorder(cP, bid_start)
P <- run_sequence(cP, "P: Phantom Draft bids")
# Scenario Q: as P, after the trade Twomey has going through at the start of
# the draft: GWS sends starting picks 20 and 26 (and a future second) to Port
# for Port's pick 12. Brisbane's planned trade of pick 21 has no named partner,
# so it is not modelled.
start_saved <- start
start <- copy(start)[start == 12L, club := "GWS"][start %in% c(20L, 26L), club := "PORT"]
Q <- run_sequence(cP, "Q: Phantom + GWS gets pick 12 for 20 and 26")
start <- start_saved

# ---- Anchors, written before the first run ----
# 1. The first bid has nothing ahead of it, so it must equal 06's row exactly.
a1 <- A[player == "Dougie Cochrane"]; r1 <- e$res[player == "Dougie Cochrane"]
stopifnot(nrow(a1) == 1, nrow(r1) == 1, nrow(A[player == "Cody Walker"]) == 1, nrow(B[player == "Cody Walker"]) == 1)
stopifnot(a1$bid_live == 1, a1$cost_points == r1$cost_points, a1$deficit_points == r1$deficit_points,
          a1$gain_torp == r1$gain_torp)
# 2. The example Pete confirmed: Richmond's bid is live pick 3, Carlton pays
#    with live 12 and 17 (starting 13 and 18), 159 short, can match.
aw <- A[player == "Cody Walker"]
stopifnot(aw$bid_live == 3, aw$cost_points == 2178, aw$used_live == "12+17", aw$used_start == "13+18",
          aw$deficit_points == 159, aw$can_match)
# 3. Essendon's bid is live pick 2: 2,481 against 2,019, 462 short, cannot match.
bw <- B[player == "Cody Walker"]
stopifnot(bw$bid_live == 2, bw$cost_points == 2481, bw$deficit_points == 462, !bw$can_match)
cat("anchors hold: Cochrane equals 06; Walker 3 / 12+17 / 159 (A); Walker 2 / 462 / no (B)\n")
cat("compensation picks handed out: A", attr(A, "comp"), "| B", attr(B, "comp"), "\n\n")

# 4. The Phantom's Cochrane and Walker bids are scenario A's.
stopifnot(identical(P[player %in% c("Dougie Cochrane", "Cody Walker"), .(bid_live, cost_points, deficit_points)],
                    A[player %in% c("Dougie Cochrane", "Cody Walker"), .(bid_live, cost_points, deficit_points)]))
cat("Phantom live picks (P): ", paste(P$player, P$bid_live, collapse = "; "), "\n")
both <- rbind(A, B, P, Q)
fwrite(both, file.path(out_dir, "06c_bids_in_sequence.csv"))
old <- e$res[, .(player, old_cost = cost_points, old_deficit = deficit_points, old_can = can_match, old_gain = gain_torp)]
show <- merge(both, old, by = "player")[order(scenario, bid_live)]
cat("In sequence, against the starting-order numbers (cost and deficit in AFL points; gain in TORP value, higher is better for the club):\n")
print(show[, .(scenario = substr(scenario, 1, 1), player, bidder, start = bid_start, live = bid_live, cost = cost_points,
               old_cost, used_live, used_start, left = leftover_live, deficit = deficit_points, old_deficit,
               can = can_match, old_can, gain = gain_torp, old_gain)])
