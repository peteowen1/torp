# Where does EPV actually come from? Decompose the chain value by event type.
#
# The rating says disposal is ~55% of value and reception ~43%, but not what is
# INSIDE those. This takes every play-by-play row that carries a `delta_epv` and
# attributes it the way the engine does, then buckets it:
#
#   disposal credit  EPV_DISP_SCALE * delta_epv        -> the player who disposed
#   reception credit EPV_RECV_SCALE * delta_epv * pos_team -> the lead player
#
# `pos_team` is -1 when the lead player is on the side that did NOT have the
# ball, i.e. a turnover reception, which is why the reception term flips sign --
# a defender gaining the ball is credited for the attacking team's loss.
#
# Questions this answers:
#   - is disposal value mostly SHOOTING, or is it built up in general play?
#   - short kicks or long ones? kicks or handballs?
#   - is reception value mostly receiving for a shot, receiving handballs, or
#     intercepting?
#   - what does a single event of each type typically move the scoreboard by,
#     and how much total value does each type carry across a season?
#
# The aerial-contest path is v3's one departure from this split and is measured
# separately in epv3_contest_anatomy.R; the buckets below are the ordinary
# disposer/receiver split, which both engines share.
#
# ~4 min over ~1.8M rows.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_value_buildup.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

p <- as.data.table(load_pbp(TRUE))
p <- p[is.finite(delta_epv)]
n_matches <- uniqueN(p$match_id)
say("=== Where EPV comes from ===")
say("rows with a delta_epv: ", format(nrow(p), big.mark = ","),
    " over ", n_matches, " matches (", round(nrow(p) / n_matches), " per match)")

DS <- EPV_DISP_SCALE; RS <- EPV_RECV_SCALE
p[, disp_cr := delta_epv * DS]
p[, recv_cr := delta_epv * pos_team * RS]
say("using EPV_DISP_SCALE ", DS, " and EPV_RECV_SCALE ", RS,
    " (the ordinary split; v3 replaces this on aerial kicks only)")

# Shot flag. `shot_at_goal` is the real one -- logical, TRUE only on Kick and
# Ground Kick rows that were scoring attempts.
#
# NOT `xscore` / `on_target_prob` / `goal` / `behind`: those are the shot MODEL's
# predictions and are populated on every row of the frame, so any of them would
# flag 100% of rows as shots. And NOT `is_shot`, which already exists in pbp as
# an integer meaning something else entirely (1 on 94% of rows) -- assigning to
# that name silently overwrote a real column and then broke on its type.
p[, .shot := shot_at_goal %in% TRUE]
say("shot flag: shot_at_goal -> ", format(sum(p$.shot), big.mark = ","),
    " shot rows (", round(100 * mean(p$.shot), 2), "% of rows, ",
    round(sum(p$.shot) / n_matches, 1), " per match)")

# ---------------------------------------------------------------------------
# 1. total value by event type
# ---------------------------------------------------------------------------
say("")
say("=== 1. VALUE BY EVENT TYPE (all seasons) ===")
say("gross_abs is sum|delta_epv| -- the total value that MOVED. net is the")
say("signed sum, which is near zero for most types because one side's gain is")
say("the other's loss.")
by_desc <- p[, .(n = .N,
                 per_match = round(.N / n_matches, 1),
                 mean_abs_delta = round(mean(abs(delta_epv)), 4),
                 gross_abs = round(sum(abs(delta_epv))),
                 share_gross_pct = 0,
                 net = round(sum(delta_epv))), by = description]
by_desc[, share_gross_pct := round(100 * gross_abs / sum(gross_abs), 1)]
setorder(by_desc, -gross_abs)
say_dt(by_desc, 22)

# ---------------------------------------------------------------------------
# 2. the disposal channel, taken apart
# ---------------------------------------------------------------------------
say("")
say("=== 2. THE DISPOSAL CHANNEL ===")
disp <- p[!is.na(player_id) & description %chin% c("Kick", "Handball", "Ground Kick")]
say("disposal rows ", format(nrow(disp), big.mark = ","),
    " | total disposal credit ", round(sum(disp$disp_cr)))
d1 <- disp[, .(n = .N, per_match = round(.N / n_matches, 1),
               mean_credit = round(mean(disp_cr), 4),
               total_credit = round(sum(disp_cr)),
               gross_abs = round(sum(abs(disp_cr)))), by = description]
d1[, share_gross_pct := round(100 * gross_abs / sum(gross_abs), 1)]
setorder(d1, -gross_abs)
say_dt(d1, 6)

say("")
say("--- is it SHOOTING? kicks split by whether they were a scoring shot ---")
d2 <- disp[description %chin% c("Kick", "Ground Kick"),
           .(n = .N, per_match = round(.N / n_matches, 1),
             mean_credit = round(mean(disp_cr), 4),
             gross_abs = round(sum(abs(disp_cr)))),
           by = .(shot = fifelse(.shot, "shot at goal", "general play"))]
d2[, share_gross_pct := round(100 * gross_abs / sum(gross_abs), 1)]
say_dt(d2, 4)
say("")
say("A shot is worth many times an ordinary kick per event. Whether it")
say("DOMINATES the channel depends on the count, and there are far fewer of")
say("them -- read mean_credit and share_gross_pct together.")

if (all(c("x", "y") %in% names(p))) {
  say("")
  say("--- by field position of the disposal (x, attacking direction) ---")
  half <- as.numeric(quantile(abs(p$x), 0.995, na.rm = TRUE))
  disp[, zone := fcase(x > half - 50, "forward 50",
                       x > 0, "attacking midfield",
                       x > -(half - 50), "defensive midfield",
                       default = "defensive 50")]
  d3 <- disp[!is.na(zone), .(n = .N, per_match = round(.N / n_matches, 1),
                             mean_credit = round(mean(disp_cr), 4),
                             gross_abs = round(sum(abs(disp_cr)))), by = zone]
  d3[, share_gross_pct := round(100 * gross_abs / sum(gross_abs), 1)]
  setorder(d3, -gross_abs)
  say_dt(d3, 5)
}

say("")
say("--- kick LENGTH, from the disposal to where the ball next was ---")
setorder(p, match_id, display_order)
p[, `:=`(.nx = shift(x, 1, type = "lead"), .ny = shift(y, 1, type = "lead"),
         .nm = shift(match_id, 1, type = "lead"))]
p[, kick_len := fifelse(.nm == match_id, sqrt((.nx - x)^2 + (.ny - y)^2), NA_real_)]
kk <- p[description %chin% c("Kick", "Ground Kick") & is.finite(kick_len)]
kk[, len_band := cut(kick_len, c(-Inf, 20, 35, 50, Inf),
                     labels = c("short <20m", "20-35m", "35-50m", "long >50m"))]
d4 <- kk[, .(n = .N, per_match = round(.N / n_matches, 1),
             mean_credit = round(mean(disp_cr), 4),
             gross_abs = round(sum(abs(disp_cr)))), by = len_band]
d4[, share_gross_pct := round(100 * gross_abs / sum(gross_abs), 1)]
setorder(d4, len_band)
say_dt(d4, 6)

say("")
say("--- kicks that KEPT the ball against kicks that lost it ---")
d5 <- kk[, .(n = .N, per_match = round(.N / n_matches, 1),
             mean_credit = round(mean(disp_cr), 4),
             gross_abs = round(sum(abs(disp_cr)))),
         by = .(retained = fifelse(delta_epv >= 0, "gained EPV", "lost EPV"))]
d5[, share_gross_pct := round(100 * gross_abs / sum(gross_abs), 1)]
say_dt(d5, 4)

# ---------------------------------------------------------------------------
# 3. the reception channel, taken apart
# ---------------------------------------------------------------------------
say("")
say("=== 3. THE RECEPTION CHANNEL ===")
say("Credit goes to `lead_player_id` and flips sign when pos_team is -1, i.e.")
say("when the receiver is on the side that did NOT have the ball -- an")
say("intercept. So the channel is receiving AND intercepting in one number.")
recv <- p[!is.na(lead_player_id)]
say("reception rows ", format(nrow(recv), big.mark = ","),
    " | total reception credit ", round(sum(recv$recv_cr)))
say("")
say("--- own-team reception against intercept ---")
r1 <- recv[, .(n = .N, per_match = round(.N / n_matches, 1),
               mean_credit = round(mean(recv_cr), 4),
               total_credit = round(sum(recv_cr)),
               gross_abs = round(sum(abs(recv_cr)))),
           by = .(side = fifelse(pos_team == -1L, "INTERCEPT (opposition lost it)",
                                 "own-team reception"))]
r1[, share_gross_pct := round(100 * gross_abs / sum(gross_abs), 1)]
say_dt(r1, 4)

say("")
say("--- by what the receiving event actually was ---")
lead_col <- if ("lead_desc_tot" %in% names(recv)) "lead_desc_tot" else "description"
say("bucketing on ", lead_col)
r2 <- recv[, .(n = .N, per_match = round(.N / n_matches, 1),
               mean_credit = round(mean(recv_cr), 4),
               gross_abs = round(sum(abs(recv_cr)))), by = c(lead_col)]
r2[, share_gross_pct := round(100 * gross_abs / sum(gross_abs), 1)]
setorder(r2, -gross_abs)
say_dt(r2, 22)

say("")
say("--- is reception value RECEIVING FOR A SHOT? ---")
r3 <- recv[, .(n = .N, per_match = round(.N / n_matches, 1),
               mean_credit = round(mean(recv_cr), 4),
               gross_abs = round(sum(abs(recv_cr)))),
           by = .(shot = fifelse(.shot, "the reception WAS a shot", "general play"))]
r3[, share_gross_pct := round(100 * gross_abs / sum(gross_abs), 1)]
say_dt(r3, 4)

# ---------------------------------------------------------------------------
# 4. the headline: how much of all EPV is scoring?
# ---------------------------------------------------------------------------
say("")
say("=== 4. HOW MUCH OF ALL CHAIN VALUE IS THE SHOT ITSELF? ===")
tot <- sum(abs(p$delta_epv))
say("total gross |delta_epv| across all rows: ", format(round(tot), big.mark = ","))
sh <- p[.shot == TRUE, sum(abs(delta_epv))]
say("carried by shot rows:                    ", format(round(sh), big.mark = ","),
    "  (", round(100 * sh / tot, 1), "%)")
sc <- p[description %chin% c("Goal", "Behind"), sum(abs(delta_epv))]
say("carried by Goal/Behind rows:             ", format(round(sc), big.mark = ","),
    "  (", round(100 * sc / tot, 1), "%)")
say("")
say("The rest is possession value -- moving the ball into better positions and")
say("keeping it. That is what makes this a chain metric rather than a")
say("box-score one, and it is why a player who never shoots can rate highly.")

close(con)
cat("\nDone\n")
