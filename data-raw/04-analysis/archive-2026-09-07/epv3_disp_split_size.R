# How big is the aerial part of epv_disp, and what would splitting it do?
#
# The asymmetry: a NON-aerial kick pays its disposer EPV_DISP_SCALE (0.5) of the
# swing and the receiver the other half. An AERIAL kick pays its disposer the
# WHOLE of (V_pre - exp_pts) and pays the marker only the contest SURPRISE,
# which is zero-sum and averages ~0. So every aerial kick moves value out of
# `recv` and into `disp`, and under-credits the player who took the mark.
#
# Before designing a fix, size it: what share of disp is the aerial term, and
# what would recv/disp/cont look like if the aerial disposal term were split the
# same 50/50 the rest of the model uses.
#
# Reads the cached contest table only -- no rebuild.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_disp_split_size.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

cst <- as.data.table(read_parquet(file.path(OUT_DIR, "epv3_contest_table.parquet")))
say("contest table: ", format(nrow(cst), big.mark = ","), " rows")
say("columns: ", paste(names(cst), collapse = ", "))

need <- c("disp_credit", "winner_credit", "kick_pid", "out_pid", "def_win", "match_id")
miss <- setdiff(need, names(cst))
if (length(miss) > 0) {
  say("MISSING: ", paste(miss, collapse = ", "), " -- rescoring from p_hat/V_pre/exp_pts")
}

if (!"disp_credit" %in% names(cst) && all(c("V_pre", "exp_pts") %in% names(cst))) {
  cst[, disp_credit := V_pre - exp_pts]
}

say("")
say("=== the aerial disposal term itself ===")
say("contests: ", format(nrow(cst), big.mark = ","),
    "   defence wins: ", round(100 * mean(cst$def_win), 1), "%")
say("mean disp_credit (V_pre - exp_pts): ", round(mean(cst$disp_credit, na.rm = TRUE), 4))
say("sd   disp_credit:                   ", round(sd(cst$disp_credit, na.rm = TRUE), 4))
say("total points of disp credit paid:   ",
    format(round(sum(cst$disp_credit, na.rm = TRUE)), big.mark = ","))

# Per player-game: how much does the aerial term contribute to a kicker's disp,
# and how much would the marker gain if half of it went to him?
d_kick <- cst[!is.na(kick_pid), .(aer_disp = sum(disp_credit, na.rm = TRUE)),
              by = .(player_id = kick_pid, match_id)]
d_mark <- cst[def_win == FALSE & !is.na(out_pid),
              .(aer_recv_if_split = sum(disp_credit, na.rm = TRUE) / 2),
              by = .(player_id = out_pid, match_id)]

say("")
say("=== per player-game ===")
say_dt(data.table(
  quantity = c("aerial disp credit banked by the kicker",
               "half of it, if the ATTACK-RETAINED share went to the marker"),
  n_player_games = c(nrow(d_kick), nrow(d_mark)),
  mean = round(c(mean(d_kick$aer_disp), mean(d_mark$aer_recv_if_split)), 4),
  sd   = round(c(sd(d_kick$aer_disp), sd(d_mark$aer_recv_if_split)), 4)), 5)

# Compare against the built channels.
pg <- as.data.table(read_parquet(file.path(OUT_DIR, "epv3_pgd_3ch_raw.parquet")))
pg <- merge(pg, d_kick, by = c("player_id", "match_id"), all.x = TRUE)
pg[is.na(aer_disp), aer_disp := 0]
say("")
say("=== share of epv_disp that is the aerial term ===")
say("sd(epv_disp)            ", round(sd(pg$epv_disp, na.rm = TRUE), 4))
say("sd(aerial part)         ", round(sd(pg$aer_disp), 4))
say("sd(non-aerial part)     ", round(sd(pg$epv_disp - pg$aer_disp, na.rm = TRUE), 4))
say("var share of the aerial part: ",
    round(100 * var(pg$aer_disp) / var(pg$epv_disp, na.rm = TRUE), 1), "%")
say("cor(aerial, non-aerial parts): ",
    round(cor(pg$aer_disp, pg$epv_disp - pg$aer_disp, use = "complete.obs"), 3))

# What the channels would look like with the split, before any re-calibration.
pg <- merge(pg, d_mark, by = c("player_id", "match_id"), all.x = TRUE)
pg[is.na(aer_recv_if_split), aer_recv_if_split := 0]
pg[, `:=`(disp_split = epv_disp - aer_recv_if_split,
          recv_split = epv_recv + aer_recv_if_split)]
say("")
say("=== raw channel sds, before and after a 50/50 aerial split ===")
say_dt(data.table(
  channel = c("recv", "disp", "cont"),
  now  = round(c(sd(pg$epv_recv, na.rm = TRUE), sd(pg$epv_disp, na.rm = TRUE),
                 sd(pg$epv_spoil, na.rm = TRUE)), 4),
  split = round(c(sd(pg$recv_split, na.rm = TRUE), sd(pg$disp_split, na.rm = TRUE),
                  sd(pg$epv_spoil, na.rm = TRUE)), 4)), 5)
say("")
say("NOTE these are RAW sds. The points shares depend on how each channel")
say("converts to margin, which only a rebuild + refit can answer -- a channel")
say("can grow in sd and still convert worse. This sizes the lever, it does not")
say("settle it.")

# Which positions would gain from the split?
say("")
say("=== who gains from the split, by position (season totals, 2026) ===")
g <- pg[season == max(season) & !is.na(position_group),
        .(n = .N, gain = round(sum(aer_recv_if_split), 1)), by = position_group]
setorder(g, -gain)
say_dt(g, 10)
g2 <- pg[season == max(season) & !is.na(position_group),
         .(gain_per_game = round(mean(aer_recv_if_split), 3)), by = position_group]
setorder(g2, -gain_per_game)
say_dt(g2, 10)

close(con)
cat("\nWrote ", OUT, "\n")
