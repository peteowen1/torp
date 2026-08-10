# Why is epv_disp_adj 2.6x its raw sd while epv_recv_adj is only 1.3x?
#
# .position_adjust() is  (p80 - m_pos)/s_pos * S_pooled * tog, so the inflation
# a channel receives is S_pooled / s_pos -- the ratio of the POOLED spread of
# the per-80 value to the spread WITHIN the player's role. If that ratio differs
# a lot between channels, the standardisation step is silently re-weighting the
# channels against each other, and the "disposal carries 72% of the value"
# finding would be an artifact of this step rather than a property of the game.
#
# Reads the cached v3 player-game frame; recomputes the adjustment inputs
# exactly as create_player_game_data() does.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_standardise_audit.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

d <- as.data.table(read_parquet(file.path(OUT_DIR, "epv3_pgd_3ch_raw.parquet")))
d[, tog := pmax(dplyr::coalesce(time_on_ground_percentage / 100, 0.1), 0.1)]

CH <- c("recv", "disp", "spoil")
for (ch in CH) d[, (paste0("p80_", ch)) := get(paste0("epv_", ch)) / tog]

wsd <- function(x, w) { ok <- !is.na(x) & !is.na(w); m <- weighted.mean(x[ok], w[ok])
                        sqrt(sum(w[ok] * (x[ok] - m)^2) / sum(w[ok])) }

say("=== the inflation factor S_pooled / s_role, per channel ===")
rows <- rbindlist(lapply(CH, function(ch) {
  p80 <- d[[paste0("p80_", ch)]]
  S <- wsd(p80, d$tog)
  # s_role is computed inside group_by(lineup_position); take the TOG-weighted
  # average of the per-role sds, which is what the adjustment effectively uses.
  sr <- d[!is.na(lineup_position), .(s = wsd(get(paste0("p80_", ch)), tog), w = sum(tog)),
          by = lineup_position]
  s_bar <- sum(sr$s * sr$w) / sum(sr$w)
  data.table(channel = ch,
             sd_raw = round(sd(d[[paste0("epv_", ch)]], na.rm = TRUE), 4),
             S_pooled_p80 = round(S, 4),
             s_role_mean = round(s_bar, 4),
             inflation = round(S / s_bar, 3),
             sd_adj = round(sd(d[[paste0("epv_", ch, "_adj")]], na.rm = TRUE), 4))
}))
say_dt(rows, 5)

say("")
say("=== is the pooled p80 spread being driven by low-TOG tails? ===")
say("p80 = raw / tog, and tog floors at 0.10, so a 12-minute cameo is")
say("multiplied by up to 10. If the pooled sd is dominated by those rows the")
say("standardisation is rescaling every channel by a tail artifact.")
qs <- c(0.5, 0.9, 0.99, 0.999, 1)
say_dt(rbindlist(lapply(CH, function(ch) {
  v <- abs(d[[paste0("p80_", ch)]])
  as.data.table(c(list(channel = ch), setNames(as.list(round(quantile(v, qs, na.rm = TRUE), 2)),
                                               paste0("q", qs))))
})), 5)
say("")
say("tog distribution: min ", round(min(d$tog), 3), "  q01 ", round(quantile(d$tog, .01), 3),
    "  median ", round(median(d$tog), 3), "  max ", round(max(d$tog), 3))
say("player-games at the tog FLOOR (0.10): ", sum(d$tog <= 0.1001),
    " (", round(100 * mean(d$tog <= 0.1001), 2), "%)")

say("")
say("=== the same inflation, computed on TOG > 0.5 only ===")
d2 <- d[tog > 0.5]
say_dt(rbindlist(lapply(CH, function(ch) {
  p80 <- d2[[paste0("p80_", ch)]]
  S <- wsd(p80, d2$tog)
  sr <- d2[!is.na(lineup_position), .(s = wsd(get(paste0("p80_", ch)), tog), w = sum(tog)),
           by = lineup_position]
  s_bar <- sum(sr$s * sr$w) / sum(sr$w)
  data.table(channel = ch, S_pooled_p80 = round(S, 4), s_role_mean = round(s_bar, 4),
             inflation = round(S / s_bar, 3))
})), 5)

say("")
say("=== per-role s_pos for the disposal channel (the divisor) ===")
sr <- d[!is.na(lineup_position),
        .(n = .N, s_disp = round(wsd(p80_disp, tog), 3),
          s_recv = round(wsd(p80_recv, tog), 3),
          s_spoil = round(wsd(p80_spoil, tog), 3)), by = lineup_position]
setorder(sr, s_disp)
say_dt(sr, 25)

close(con)
cat("\nWrote ", OUT, "\n")
