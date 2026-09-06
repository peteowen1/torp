# Channel SDs, and the evidence on whether splitting contest into aerial +
# stoppage (4 channels) carries information over one merged contest channel (3).
#
# The 3-vs-4 choice was made by Pete up front and never tested. This does not
# test it either -- a real test needs both arms through EPR and the match gate,
# because each channel carries its OWN decay, prior_games, prior_rate and
# Bayesian shrinkage, so merging is not simply adding two columns. What this
# does is measure whether there is anything to test: if the two contest channels
# are strongly correlated and hit the same players, merging costs nothing.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_channel_sds.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 60) for (l in capture.output(print(utils::head(x, n)))) say(l)

v2 <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_player_game_v2.parquet")))
v3 <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_player_game_v3.parquet")))

# v3 aliases cont_aerial into the epv_spoil slot and cont_stop into epv_hitout.
CH  <- c("recv", "disp", "spoil", "hitout")
LBL <- c(recv = "recv", disp = "disp", spoil = "cont_aerial", hitout = "cont_stop")
K   <- c(recv = 0.5969, disp = 0.6095, cont_aerial = 0.2656, cont_stop = 1.7680)

say("=== EPV channel standard deviations ===")
say("per player-game, ", format(nrow(v3), big.mark = ","), " rows, 2021-2026")
say("")
say("Three different scales, and they say different things:")
say("  RAW        as computed, before any position adjustment")
say("  ADJUSTED   after .position_adjust() -- what EPR actually consumes")
say("  CALIBRATED adjusted x EPV3_POINTS_SCALE -- margin points (v3 only)")
say("")

tbl <- rbindlist(lapply(CH, function(ch) {
  raw2 <- v2[[paste0("epv_", ch)]];      adj2 <- v2[[paste0("epv_", ch, "_adj")]]
  raw3 <- v3[[paste0("epv_", ch)]];      adj3 <- v3[[paste0("epv_", ch, "_adj")]]
  data.table(
    channel      = LBL[[ch]],
    v2_raw       = round(sd(raw2, na.rm = TRUE), 4),
    v3_raw       = round(sd(raw3, na.rm = TRUE), 4),
    v2_adj       = round(sd(adj2, na.rm = TRUE), 4),
    v3_adj       = round(sd(adj3, na.rm = TRUE), 4),
    v3_calib     = round(sd(adj3, na.rm = TRUE) * K[[LBL[[ch]]]], 4)
  )
}))
say_dt(tbl, 8)

vshare <- function(x) round(100 * x^2 / sum(x^2), 1)
say("")
say("variance share (%):")
say_dt(data.table(channel = tbl$channel,
                  v2_adj = vshare(tbl$v2_adj),
                  v3_adj = vshare(tbl$v3_adj),
                  v3_calibrated = vshare(tbl$v3_calib)), 8)

say("")
say("total epv_adj sd: v2 ", round(sd(v2$epv_adj, na.rm = TRUE), 4),
    " | v3 ", round(sd(v3$epv_adj, na.rm = TRUE), 4))

# ---- Is there anything in the 3-vs-4 split? --------------------------------
say("")
say("=== 3 channels vs 4: is the split carrying information? ===")
a <- v3$epv_spoil_adj      # cont_aerial
s <- v3$epv_hitout_adj     # cont_stop
say("cor(cont_aerial, cont_stop) = ", round(cor(a, s, use = "complete.obs"), 4))
say("  [near zero => they measure different things and merging LOSES information]")
say("  [near one  => merging costs nothing and 3 channels is simpler]")
say("")
merged <- a + s
say("sd if merged into one contest channel: ", round(sd(merged, na.rm = TRUE), 4))
say("  vs cont_aerial alone ", round(sd(a, na.rm = TRUE), 4),
    " and cont_stop alone ", round(sd(s, na.rm = TRUE), 4))
say("  (sd adds in quadrature when uncorrelated: sqrt(a^2+s^2) = ",
    round(sqrt(sd(a, na.rm = TRUE)^2 + sd(s, na.rm = TRUE)^2), 4), ")")

say("")
say("--- who does each contest channel actually touch? ---")
say("If cont_stop is a ruck-only channel, merging it into cont_aerial dilutes a")
say("signal that applies to ~4% of player-games with one that applies to most.")
pg <- v3[!is.na(position_group)]
say_dt(pg[, .(n = .N,
              pct_nonzero_aerial = round(100 * mean(abs(epv_spoil_adj) > 1e-9), 1),
              pct_nonzero_stop   = round(100 * mean(abs(epv_hitout_adj) > 1e-9), 1),
              sd_aerial = round(sd(epv_spoil_adj), 3),
              sd_stop   = round(sd(epv_hitout_adj), 3)),
          by = position_group][order(position_group)], 10)

say("")
say("--- full v3 channel correlation matrix (adjusted) ---")
m <- as.matrix(v3[, .(recv = epv_recv_adj, disp = epv_disp_adj,
                      cont_aerial = epv_spoil_adj, cont_stop = epv_hitout_adj)])
say_dt(as.data.table(round(cor(m, use = "complete.obs"), 3), keep.rownames = "channel"), 6)

say("")
say("--- v2 for comparison ---")
m2 <- as.matrix(v2[, .(recv = epv_recv_adj, disp = epv_disp_adj,
                       spoil = epv_spoil_adj, hitout = epv_hitout_adj)])
say_dt(as.data.table(round(cor(m2, use = "complete.obs"), 3), keep.rownames = "channel"), 6)

say("")
say("NOTE: this does NOT decide 3 vs 4. Each channel carries its own")
say("EPR_DECAY_*, EPR_PRIOR_GAMES_* and EPR_PRIOR_RATE_*, so merging changes the")
say("Bayesian shrinkage a ruck receives, not just the arithmetic. A real test")
say("needs both arms through .build_epr_season() and the match gate.")

close(con)
cat("\nWrote ", OUT, "\n")
