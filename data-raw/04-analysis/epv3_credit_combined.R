# Do the two GENUINE credit-parameter changes hold together, and are they real?
#
# The one-at-a-time sweep found four movers, two of which were degenerate --
# they raised reliability by dragging the channel toward pure event count
# (recv_pos_offset pushed count-correlation 0.53 -> 0.75; disp_pos_offset pushed
# it -0.12 -> -0.50, same trap with the sign flipped). Pure count scores
# 0.57/0.59, so those are not better measures of value, they are a different
# measure.
#
# Two were genuine, raising reliability while LOWERING count-dependence:
#   recv_neg_mult  1.0 -> 0.4   (gain 0.040, count-dep 0.53 -> 0.42)
#   recv_pos_mult  1.0 -> 1.6   (gain 0.026, count-dep 0.53 -> 0.47)
#
# Both say the same thing: the 1:1 symmetry between "you won the ball" and "the
# opposition lost it to you" is wrong. When possession turns over, the lead
# player is whoever picked it up, and crediting him the full delta_epv
# attributes an opponent's error to him.
#
# THIS SCRIPT ASKS THREE THINGS the sweep cannot:
#   1. Do the two changes ADD, or are they the same effect twice?
#   2. Does the gain survive at longer lags -- i.e. is it a better measure of a
#      STABLE player property, not just of adjacent-game similarity?
#   3. Is there a better joint optimum, and does it stay non-degenerate?
#
# PERFORMANCE: reuses the linear sufficient-statistic trick, so every evaluation
# is microseconds. The grid below is ~500 combinations and runs in seconds.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_credit_combined.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

pbp <- as.data.table(load_pbp(TRUE))
chains <- load_chains(TRUE)
aer <- build_aerial_contests(chains, pbp)
keys <- unique(aer[, .(match_id, display_order = kick_do)])
pbp[, .is_aerial_kick := FALSE]
pbp[keys, .is_aerial_kick := TRUE, on = .(match_id, display_order)]
pbp[, .date := as.Date(utc_start_time)]

R <- pbp[!is.na(lead_player_id) & .is_aerial_kick == FALSE & is.finite(delta_epv),
         .(T_neg = sum((delta_epv * pos_team)[pos_team == -1L]),
           M_neg = sum(pos_team == -1L),
           T_pos = sum((delta_epv * pos_team)[pos_team != -1L]),
           M_pos = sum(pos_team != -1L)),
         by = .(player_id = lead_player_id, match_id)]
md <- unique(pbp[, .(match_id, date = .date)], by = "match_id")
R <- merge(R, md, by = "match_id")
ps <- as.data.table(load_player_stats(TRUE))
R <- merge(R, unique(ps[, .(player_id, match_id, tog = time_on_ground_percentage)]),
           by = c("player_id", "match_id"), all.x = TRUE)
R[, tog_safe := pmax(fifelse(is.na(tog), 100, tog) / 100, 0.1)]
setorder(R, player_id, date)
say("=== Combined credit-parameter test (reception channel) ===")
say("player-match rows: ", format(nrow(R), big.mark = ","))

mk <- function(nm, pm, no = 0, po = 0) {
  (nm * R$T_neg + no * R$M_neg + pm * R$T_pos + po * R$M_pos) / R$tog_safe
}
cnt <- (R$M_neg + R$M_pos) / R$tog_safe

rel_at <- function(x, lag) {
  d <- data.table(pid = R$player_id, x = x)[is.finite(x)]
  d[, `:=`(x2 = shift(x, lag, type = "lead"), p2 = shift(pid, lag, type = "lead"))]
  s <- d[!is.na(x2) & p2 == pid]
  if (nrow(s) < 100) return(NA_real_)
  cor(s$x, s$x2)
}

say("")
say("=== 1. Do the two changes ADD? ===")
arms <- list(
  "baseline (1.0 / 1.0)"        = c(1.0, 1.0),
  "neg_mult 0.4 only"           = c(0.4, 1.0),
  "pos_mult 1.6 only"           = c(1.0, 1.6),
  "BOTH (0.4 / 1.6)"            = c(0.4, 1.6)
)
tab <- rbindlist(lapply(names(arms), function(nm) {
  v <- arms[[nm]]; x <- mk(v[1], v[2])
  data.table(arm = nm, rel_lag1 = round(rel_at(x, 1), 4),
             rel_lag4 = round(rel_at(x, 4), 4), rel_lag8 = round(rel_at(x, 8), 4),
             cor_count = round(cor(x, cnt, use = "complete.obs"), 3))
}))
say_dt(tab, 6)
b <- tab[arm == "baseline (1.0 / 1.0)"]$rel_lag1
say("")
say("gains at lag 1: neg_mult ", round(tab[2]$rel_lag1 - b, 4),
    " | pos_mult ", round(tab[3]$rel_lag1 - b, 4),
    " | both ", round(tab[4]$rel_lag1 - b, 4),
    "  (sum of singles ", round((tab[2]$rel_lag1 - b) + (tab[3]$rel_lag1 - b), 4), ")")
say("If 'both' is close to the sum, the two changes are independent levers.")
say("If it is much less, they are the same effect measured twice.")

say("")
say("=== 2. Does it survive at longer lags? ===")
say("Lag 1 can be inflated by short-term form or by opponent/venue carryover.")
say("A change that improves a STABLE player property should hold at lag 8.")
say("baseline lag8 ", tab[1]$rel_lag8, " -> both lag8 ", tab[4]$rel_lag8,
    "  (gain ", round(tab[4]$rel_lag8 - tab[1]$rel_lag8, 4), ")")

say("")
say("=== 3. Joint grid, with the degeneracy guard applied throughout ===")
g <- CJ(nm = seq(0.0, 1.4, by = 0.1), pm = seq(0.6, 2.0, by = 0.1))
g[, `:=`(rel = NA_real_, cc = NA_real_)]
for (i in seq_len(nrow(g))) {
  x <- mk(g$nm[i], g$pm[i])
  g$rel[i] <- rel_at(x, 1)
  g$cc[i] <- cor(x, cnt, use = "complete.obs")
}
base_cc <- tab[1]$cor_count
say("baseline count-dependence ", base_cc,
    " -- any candidate whose |cor_count| EXCEEDS this is walking toward the")
say("degenerate count solution and is rejected regardless of reliability.")
ok <- g[abs(cc) <= abs(base_cc)]
say("")
say("candidates passing the guard: ", nrow(ok), " of ", nrow(g))
setorder(ok, -rel)
say("top 10 non-degenerate:")
say_dt(ok[1:10, .(recv_neg_mult = nm, recv_pos_mult = pm,
                  reliability = round(rel, 4), cor_count = round(cc, 3))], 10)
say("")
setorder(g, -rel)
say("top 5 IGNORING the guard, for contrast:")
say_dt(g[1:5, .(recv_neg_mult = nm, recv_pos_mult = pm,
                reliability = round(rel, 4), cor_count = round(cc, 3))], 5)
say("")
say("If the unguarded winners all have high |cor_count|, that is the trap")
say("re-appearing in the joint search and the guard is doing real work.")

best <- ok[1]
say("")
say("=== RECOMMENDED (non-degenerate optimum) ===")
say("  EPV_RECV_NEG_MULT  1.0  ->  ", best$nm)
say("  EPV_RECV_POS_MULT  1.0  ->  ", best$pm)
say("  reception reliability ", round(tab[1]$rel_lag1, 4), " -> ", round(best$rel, 4),
    "  (+", round(best$rel - tab[1]$rel_lag1, 4), ")")
say("  count-dependence ", base_cc, " -> ", round(best$cc, 3), " (must not rise)")
say("")
say("This still has to clear a match gate before shipping. But unlike every")
say("calibration change tonight, it changes the MEASURE rather than rescaling")
say("it -- so the match model cannot simply reweight it away.")

arrow::write_parquet(g, file.path(OUT_DIR, "epv3_credit_joint_grid.parquet"))
close(con)
cat("\nDone\n")
