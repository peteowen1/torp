# Does the difficulty split still look good at the layer that actually ships?
#
# A GAP IN MY OWN EVIDENCE. Everything measured for the difficulty split so far
# lives at one of two ends: the raw player-game frame (conservation,
# repeatability, count-dependence, who moves) or match MAE. Neither is what a
# user sees. What ships is **EPR** -- and between raw EPV and EPR sit opponent
# adjustment, position centring, per-80 scaling, per-channel points scaling, and
# Bayesian shrinkage toward a prior.
#
# Those transformations are not gentle. Measured earlier this program: raw `epv`
# converts to margin at 0.9879 and `epv_adj` at 0.5951. A change that improves
# the raw frame can be entirely absorbed, or amplified, by the time it reaches a
# rating -- and "the raw numbers got better" is not evidence about the shipped
# metric.
#
# So this re-asks Pete's criteria of the EPR frames the ws24 arms actually
# produced:
#   1. within-position year-over-year repeatability of `epr` and `torp`
#   2. position balance in the top 40 -- criterion 3, at the shipping layer
#   3. how far the two leaderboards actually diverge
#   4. whether the channels are still distinguishable or have collapsed together
#
# Reads the ws24 rating frames directly. Each arm's scale was refitted and
# verified, so they are comparable. ~2 min.

suppressMessages({ library(data.table); library(arrow) })

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_difficulty_at_epr_layer.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

say("=== The difficulty split at the EPR (shipping) layer ===")
say("run at ", format(Sys.time()))

ARMS <- c(ship = "epv3_dgc_rt_dgc_ship_scaled.parquet",
          difficulty = "epv3_dgc_rt_dgc_flat_scaled.parquet",
          measured = "epv3_dgc_rt_dgc_table_scaled.parquet")
d <- lapply(ARMS, function(f) as.data.table(read_parquet(file.path(OUT_DIR, f))))
say("rows per arm: ", paste(names(d), vapply(d, nrow, 0L), sep = "=", collapse = ", "))

have <- Reduce(intersect, lapply(d, names))
pos_col <- intersect(c("position_group", "pos_group"), have)[1]
say("position column in use: ", pos_col,
    "  (torp_ratings' position_group is the SEASON listing, 7 levels -- count them)")
say("levels: ", paste(sort(unique(na.omit(d[[1]][[pos_col]]))), collapse = ", "))

# End-of-season rating per player, then centre within (position, season) so only
# the within-position part is measured. Pooled repeatability is dominated by
# between-position variance and would mostly re-measure the position map.
eos <- function(x, col) {
  v <- data.table(player_id = x$player_id, season = x$season, round = x$round,
                  pos = x[[pos_col]], v = x[[col]])
  v <- v[is.finite(v) & !is.na(pos)]
  s <- v[, .SD[which.max(round)], by = .(player_id, season)]
  s[, v_c := v - mean(v), by = .(pos, season)]
  s
}
wyoy <- function(x, col) {
  s <- eos(x, col)
  b <- copy(s)[, season := season - 1]
  setnames(b, c("v", "v_c"), c("v_next", "v_c_next"))
  m <- merge(s[, .(player_id, season, pos, v, v_c)],
             b[, .(player_id, season, v_next, v_c_next)], by = c("player_id", "season"))
  m <- m[is.finite(v_c) & is.finite(v_c_next)]
  list(n = nrow(m), pooled = round(cor(m$v, m$v_next), 4),
       within = round(cor(m$v_c, m$v_c_next), 4),
       per = m[, .(n = .N, r = round(cor(v_c, v_c_next), 4)), by = pos])
}

say(""); say("=== 1. REPEATABILITY AT THE EPR LAYER ===")
for (cc in intersect(c("epr", "torp"), have)) {
  rows <- rbindlist(lapply(names(d), function(nm) {
    w <- wyoy(d[[nm]], cc)
    data.table(metric = cc, arm = nm, n = w$n, pooled = w$pooled, within = w$within)
  }))
  say_dt(rows, 5); say("")
}

say("=== 2. WITHIN-POSITION, BY GROUP (epr) ===")
per <- Reduce(function(a, b) merge(a, b, by = "pos", all = TRUE),
              lapply(names(d), function(nm) {
                p <- wyoy(d[[nm]], "epr")$per
                setnames(p, c("n", "r"), c(paste0("n_", nm), nm)); p
              }))
say_dt(per[order(-get(paste0("n_", names(d)[1])))], 8)

say(""); say("=== 3. POSITION BALANCE IN THE TOP 40 (criterion 3, shipping layer) ===")
for (nm in names(d)) {
  s <- eos(d[[nm]], "epr")
  cur <- s[season == max(season, na.rm = TRUE)]
  setorder(cur, -v)
  nmz <- if ("player_name" %in% names(d[[nm]])) {
    unique(d[[nm]][, .(player_id, player_name)])[cur[1:8], on = "player_id"]$player_name
  } else cur$player_id[1:8]
  say(""); say("  --- ", nm, " ---")
  say("  top 8: ", paste(nmz, collapse = ", "))
  say_dt(cur[1:40, .N, by = pos][order(-N)], 8)
}

say(""); say("=== 4. HOW FAR APART ARE THE LEADERBOARDS? ===")
a <- eos(d$ship, "epr"); b <- eos(d$difficulty, "epr")
m <- merge(a[, .(player_id, season, va = v)], b[, .(player_id, season, vb = v)],
           by = c("player_id", "season"))
cur <- m[season == max(season)]
cur[, `:=`(ra = frank(-va), rb = frank(-vb))]
say(sprintf("  ship vs difficulty, current season: Spearman %.4f | mean |rank change| %.1f of %d",
            cor(cur$ra, cur$rb, method = "spearman"), mean(abs(cur$ra - cur$rb)), nrow(cur)))

say(""); say("=== 5. ARE THE CHANNELS STILL DISTINGUISHABLE? ===")
say("If the channels have collapsed into each other the split is not adding")
say("structure, only moving numbers between columns.")
ch <- intersect(c("epr_recv", "epr_disp", "epr_spoil"), have)
for (nm in names(d)) {
  x <- d[[nm]][, ..ch]
  x <- x[stats::complete.cases(x)]
  cm <- cor(x)
  say(sprintf("  %-11s cor(recv,disp) %+.3f  cor(recv,contest) %+.3f  cor(disp,contest) %+.3f",
              nm, cm[1, 2], cm[1, 3], cm[2, 3]))
}

say(""); say("=== HOW TO READ IT ===")
say("The raw-frame gains (conservation 0.9936, repeatability +0.065,")
say("count-dependence down) are only worth something if they survive opponent")
say("adjustment, centring, scaling and shrinkage. If the arms are")
say("indistinguishable here, the split improved a frame nobody sees.")

close(con); cat("\nDone\n")
