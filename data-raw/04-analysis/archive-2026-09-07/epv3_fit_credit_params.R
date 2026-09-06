# Fit the CREDIT-ASSIGNMENT parameters, not the aggregation ones.
#
# Everything tonight tuned the aggregation layer (decay, prior_games) while
# treating the chain EPV values as given. But those values come from credit
# parameters -- offsets and the neg/pos multiplier asymmetry -- which are v2
# inheritances sitting at neutral defaults (offsets 0, multipliers 1) and have
# never been fitted for v3's structure. So "chain EPV is noisy" may be a
# statement about OUR CREDIT ASSIGNMENT rather than about chain data.
#
# WHAT IS AND IS NOT TUNABLE HERE. Reliability is a correlation, so pure SCALE
# parameters cannot change it: EPV_DISP_SCALE 0.5 -> 0.6 multiplies every
# player's value by 1.2 and the correlation between his games is identical.
# EPV_DISP_SCALE and EPV_RECV_SCALE are therefore INERT for this objective and
# are excluded. What can change the measure's shape:
#   disp_neg_offset, disp_pos_offset   per-event constant: volume vs value
#   recv_neg_mult,  recv_pos_mult      asymmetry between losing and keeping it
#   recv_neg_offset, recv_pos_offset   same, reception side
#
# *** THE DEGENERACY TRAP, named before running ***
# Maximising reliability has a degenerate optimum. Push the offsets large enough
# and epv_disp becomes proportional to DISPOSAL COUNT, which is highly reliable
# (~0.5) and measures nothing about value. An unbounded fit finds that and looks
# like a triumph. Two guards:
#   1. offsets bounded to +/-0.15, small against a typical |delta_epv| (~0.5)
#   2. the correlation between each optimised channel and raw event COUNT is
#      reported. If it approaches 1 we found the degenerate solution and the
#      result is void, not a finding.
#
# PERFORMANCE. The channels are LINEAR in these parameters:
#   epv_disp = scale * (S_neg + neg_off*N_neg + S_pos + pos_off*N_pos)
# so per (player, match) sufficient statistics are precomputed ONCE and any
# parameter set then evaluates in microseconds instead of re-aggregating 2M PBP
# rows. That is what makes a real sweep affordable -- otherwise each evaluation
# is ~20s and a 6-parameter sweep is hours.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_credit_params.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

say("=== Fitting the credit-assignment parameters ===")

pbp <- as.data.table(load_pbp(TRUE))
chains <- load_chains(TRUE)
say("pbp rows ", format(nrow(pbp), big.mark = ","))

# Aerial kicks are priced by the contest model, not by the 50/50 split, so they
# are excluded from these sufficient statistics exactly as create_player_game_data
# excludes them under v3.
aer <- build_aerial_contests(chains, pbp)
keys <- unique(aer[, .(match_id, display_order = kick_do)])
pbp[, .is_aerial_kick := FALSE]
pbp[keys, .is_aerial_kick := TRUE, on = .(match_id, display_order)]
say("aerial-kick rows excluded: ", format(sum(pbp$.is_aerial_kick), big.mark = ","))

pbp[, .date := as.Date(utc_start_time)]
pbp[, is_intercept_mark := pos_team == -1L & grepl("ted Mark|Mark On", lead_desc_tot)]

# ---- Sufficient statistics, computed ONCE -----------------------------------
# DISPOSAL, by the acting player.
D <- pbp[!is.na(player_id) & .is_aerial_kick == FALSE & is.finite(delta_epv),
         .(S_neg = sum(delta_epv[pos_team == -1L]),
           N_neg = sum(pos_team == -1L),
           S_pos = sum(delta_epv[pos_team != -1L]),
           N_pos = sum(pos_team != -1L)),
         by = .(player_id, match_id)]

# RECEPTION, by the next player. delta_epv * pos_team is the oriented value.
R <- pbp[!is.na(lead_player_id) & .is_aerial_kick == FALSE & is.finite(delta_epv),
         .(T_neg = sum((delta_epv * pos_team)[pos_team == -1L]),
           M_neg = sum(pos_team == -1L),
           T_pos = sum((delta_epv * pos_team)[pos_team != -1L]),
           M_pos = sum(pos_team != -1L)),
         by = .(player_id = lead_player_id, match_id)]

suff <- merge(D, R, by = c("player_id", "match_id"), all = TRUE)
for (c in setdiff(names(suff), c("player_id", "match_id"))) {
  set(suff, which(is.na(suff[[c]])), c, 0)
}
md <- unique(pbp[, .(match_id, date = .date)], by = "match_id")
suff <- merge(suff, md, by = "match_id")
ps <- as.data.table(load_player_stats(TRUE))
suff <- merge(suff, unique(ps[, .(player_id, match_id,
                                  tog = time_on_ground_percentage,
                                  disposals)]),
              by = c("player_id", "match_id"), all.x = TRUE)
suff[, tog_safe := pmax(fifelse(is.na(tog), 100, tog) / 100, 0.1)]
suff <- suff[is.finite(tog_safe)]
setorder(suff, player_id, date)
say("sufficient-statistic rows (player-match): ", format(nrow(suff), big.mark = ","))

# ---- Channels reconstructed from the statistics -----------------------------
mk <- function(p) {
  disp <- (suff$S_neg + p$disp_neg_offset * suff$N_neg +
           suff$S_pos + p$disp_pos_offset * suff$N_pos)
  recv <- (p$recv_neg_mult * suff$T_neg + p$recv_neg_offset * suff$M_neg +
           p$recv_pos_mult * suff$T_pos + p$recv_pos_offset * suff$M_pos)
  list(disp = disp / suff$tog_safe, recv = recv / suff$tog_safe)
}

# Reliability = correlation between a player's consecutive games. Shift-based,
# O(n), same structure proven earlier tonight.
reliab <- function(x) {
  d <- data.table(pid = suff$player_id, x = x)[is.finite(x)]
  d[, `:=`(x2 = shift(x, 1L, type = "lead"), p2 = shift(pid, 1L, type = "lead"))]
  s <- d[!is.na(x2) & p2 == pid]
  if (nrow(s) < 100) return(NA_real_)
  cor(s$x, s$x2)
}

DEF <- list(disp_neg_offset = 0, disp_pos_offset = 0,
            recv_neg_mult = 1, recv_pos_mult = 1,
            recv_neg_offset = 0, recv_pos_offset = 0)

base <- mk(DEF)
say("")
say("=== BASELINE (production defaults: offsets 0, multipliers 1) ===")
say("disposal reliability ", round(reliab(base$disp), 4))
say("reception reliability ", round(reliab(base$recv), 4))

# ---- Degeneracy reference: what does pure event-counting score? -------------
say("")
say("=== THE DEGENERATE SOLUTION, measured so we can recognise it ===")
cnt_disp <- (suff$N_neg + suff$N_pos) / suff$tog_safe
cnt_recv <- (suff$M_neg + suff$M_pos) / suff$tog_safe
say("reliability of pure EVENT COUNT: disposal ", round(reliab(cnt_disp), 4),
    " | reception ", round(reliab(cnt_recv), 4))
say("Any 'improvement' that approaches these numbers is the offsets turning the")
say("channel into a count. That is not a better measure of value, it is a")
say("different measure entirely.")

# ---- One-at-a-time sensitivity: the LEARNING output ------------------------
say("")
say("=== SENSITIVITY: how much does each parameter move reliability? ===")
say("(one at a time, others at default. This is the interpretable output --")
say(" a flat parameter tells us that lever does not exist.)")

grid <- list(
  disp_neg_offset = seq(-0.15, 0.15, by = 0.05),
  disp_pos_offset = seq(-0.15, 0.15, by = 0.05),
  recv_neg_mult   = seq(0.4, 1.6, by = 0.2),
  recv_pos_mult   = seq(0.4, 1.6, by = 0.2),
  recv_neg_offset = seq(-0.15, 0.15, by = 0.05),
  recv_pos_offset = seq(-0.15, 0.15, by = 0.05)
)
rows <- list()
for (nm in names(grid)) {
  for (v in grid[[nm]]) {
    p <- DEF; p[[nm]] <- v
    ch <- mk(p)
    tgt <- if (grepl("^disp", nm)) ch$disp else ch$recv
    rows[[length(rows) + 1L]] <- data.table(
      param = nm, value = v, reliability = round(reliab(tgt), 4),
      cor_with_count = round(cor(tgt,
        if (grepl("^disp", nm)) cnt_disp else cnt_recv, use = "complete.obs"), 3))
  }
}
sens <- rbindlist(rows)
for (nm in names(grid)) {
  s <- sens[param == nm]
  say("")
  say("--- ", nm, " (default ", DEF[[nm]], ", baseline reliability ",
      round(if (grepl("^disp", nm)) reliab(base$disp) else reliab(base$recv), 4), ") ---")
  say_dt(s[, .(value, reliability, cor_with_count)], 12)
  best <- s[which.max(reliability)]
  say("  best ", best$value, " -> ", best$reliability,
      "   (gain ", round(best$reliability -
        (if (grepl("^disp", nm)) reliab(base$disp) else reliab(base$recv)), 4), ")")
}

say("")
say("=== WHAT TO READ FROM THIS ===")
say("A parameter whose reliability curve is FLAT is not a lever -- that is a")
say("finding, and it means credit assignment is not what makes chain EPV noisy.")
say("A parameter that helps ONLY as cor_with_count rises has found the")
say("degenerate solution and must be rejected.")
say("A parameter that helps while cor_with_count stays flat is a real")
say("improvement to how we attribute value, and is worth taking further.")

arrow::write_parquet(sens, file.path(OUT_DIR, "epv3_credit_sensitivity.parquet"))
close(con)
cat("\nDone\n")
