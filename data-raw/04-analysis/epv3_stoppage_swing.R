# Two questions about stoppages, and the second one may replace the ruck
# box-score carve-out entirely.
#
# 1. IS THE CENTRE BOUNCE DELTA REAL? `Centre Bounce` rows carry a mean
#    delta_epv of +0.793 and, unusually, their NET equals their GROSS -- every
#    bounce reads positive, ~20 points per match. Nothing a player does looks
#    like that. A centre bounce follows a goal, so the suspicion is that the
#    delta is the scoreboard RESETTING from a just-scored state to a neutral
#    one, which is not value anybody created. `Ball Up Call` is the control: it
#    is the same kind of stoppage but does NOT follow a goal, so if the bounce
#    figure is an artifact the two will look completely different.
#
#    This matters because that 0.793 feeds the argument that the ruck box
#    weights under-pay by 3.14x. If it is a reset, that argument is built on a
#    number measuring the restart rather than the contest, and the amplification
#    should not ship.
#
# 2. WHAT IS A STOPPAGE ACTUALLY WORTH? The honest analogue of the aerial
#    contest's Delta = V_att - V_def is: what is the EPV of the state when OUR
#    team wins the tap, against when THEIRS does?
#
# PERFORMANCE NOTE, learned the hard way in this session. The first version of
# this script shifted 6 columns x 3 lags x 2 directions with `get(s)` inside
# `dt[, ... , by = match_id]` on the FULL 190-column, 2M-row frame. That breaks
# data.table's fast column-reference path -- a documented trap in
# C:/dev/.claude/rules/r-datatable-gotchas.md -- and it was still grinding after
# five minutes when it was killed. Narrowing to the six columns actually read,
# once, up front, is the documented fix and makes it seconds.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_stoppage_swing.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

full <- as.data.table(load_pbp(TRUE))
n_matches <- uniqueN(full$match_id)
# Narrow FIRST, with [[ extraction rather than a bracket select, then work on a
# six-column table.
p <- data.table(match_id = full$match_id, display_order = full$display_order,
                description = full$description, exp_pts = full$exp_pts,
                delta_epv = full$delta_epv, team_id = full$team_id,
                player_id = full$player_id)
rm(full); invisible(gc())
setorder(p, match_id, display_order)
say("=== Stoppages: is the bounce delta real, and what is a tap worth? ===")
say("rows ", format(nrow(p), big.mark = ","), " over ", n_matches, " matches")

STOP <- c("Centre Bounce", "Ball Up Call")
p[, `:=`(prev_desc = shift(description, 1),
         prev_pts  = shift(exp_pts, 1),
         f1_desc   = shift(description, 1, type = "lead"),
         f1_pts    = shift(exp_pts, 1, type = "lead"),
         f1_tid    = shift(team_id, 1, type = "lead"),
         f2_desc   = shift(description, 2, type = "lead"),
         f2_pts    = shift(exp_pts, 2, type = "lead"),
         f2_tid    = shift(team_id, 2, type = "lead"),
         f3_desc   = shift(description, 3, type = "lead"),
         f3_pts    = shift(exp_pts, 3, type = "lead"),
         f3_tid    = shift(team_id, 3, type = "lead")), by = match_id]

# ---------------------------------------------------------------------------
# 1. what precedes a stoppage, and what its delta is made of
# ---------------------------------------------------------------------------
say("")
say("=== 1. WHAT PRECEDES EACH STOPPAGE TYPE ===")
for (s in STOP) {
  d <- p[description == s]
  say("")
  say("--- ", s, "  (n = ", format(nrow(d), big.mark = ","), ", ",
      round(nrow(d) / n_matches, 1), " per match) ---")
  say("preceded by:")
  say_dt(d[, .N, by = .(prev = prev_desc)][order(-N)][1:6], 6)
  say(sprintf("delta_epv   mean %+.4f   sd %.4f   %% positive %.1f%%",
              mean(d$delta_epv, na.rm = TRUE), sd(d$delta_epv, na.rm = TRUE),
              100 * mean(d$delta_epv > 0, na.rm = TRUE)))
  say(sprintf("exp_pts     previous row %+.4f -> this row %+.4f -> next row %+.4f",
              mean(d$prev_pts, na.rm = TRUE), mean(d$exp_pts, na.rm = TRUE),
              mean(d$f1_pts, na.rm = TRUE)))
  say(sprintf("share of these stoppages that follow a SCORE: %.1f%%",
              100 * mean(d$prev_desc %chin% c("Goal", "Behind"), na.rm = TRUE)))
}

say("")
say("HOW TO READ IT. If Centre Bounce is preceded overwhelmingly by Goal/Behind")
say("and reads ~100%% positive, while Ball Up Call is preceded by general play")
say("and reads ~50%%, then the bounce delta is the post-score RESET and not")
say("contest value. Nobody created it, so no ruck can be paid for it.")

# ---------------------------------------------------------------------------
# 2. the real swing at a stoppage
# ---------------------------------------------------------------------------
say("")
say("=== 2. WHAT A STOPPAGE IS ACTUALLY WORTH ===")
st <- p[description %chin% STOP]
st[, `:=`(nxt_desc = f1_desc, nxt_tid = f1_tid, nxt_pts = f1_pts)]
st[is.na(nxt_tid), `:=`(nxt_desc = f2_desc, nxt_tid = f2_tid, nxt_pts = f2_pts)]
st[is.na(nxt_tid), `:=`(nxt_desc = f3_desc, nxt_tid = f3_tid, nxt_pts = f3_pts)]

say("what happens immediately after a stoppage:")
say_dt(st[, .N, by = .(nxt_desc)][order(-N)][1:12], 12)

resolved <- st[!is.na(nxt_tid) & is.finite(nxt_pts)]
say("")
say("stoppages whose next possessing team is identifiable: ",
    format(nrow(resolved), big.mark = ","), " of ", format(nrow(st), big.mark = ","),
    " (", round(100 * nrow(resolved) / nrow(st), 1), "%)")
say("")
say("exp_pts is in the possessing team's frame, so the value of the resulting")
say("state TO THE TEAM THAT WON THE BALL is exp_pts on that row:")
say_dt(resolved[, .(n = .N, mean_exp_pts = round(mean(nxt_pts), 4),
                    sd = round(sd(nxt_pts), 4)), by = .(stoppage = description)], 4)
say("")
say("The swing between winning and losing is twice that, because the ball going")
say("the other way is the mirror state:")
for (s in STOP) {
  v <- resolved[description == s, mean(nxt_pts)]
  say(sprintf("  %-14s value to the winner %+.4f  =>  swing %.4f per stoppage",
              s, v, 2 * abs(v)))
}

say("")
say("--- what the box weights actually pay a ruck who wins a tap ---")
say("EPV_HITOUT_WT ", EPV_HITOUT_WT, " + EPV_RUCK_CONTEST_WT ", EPV_RUCK_CONTEST_WT,
    " = ", round(EPV_HITOUT_WT + EPV_RUCK_CONTEST_WT, 4))
say("The figure that justifies the 3.14x correction is a 0.3925 swing per contest.")

# ---------------------------------------------------------------------------
# 3. does the tap decide possession?
# ---------------------------------------------------------------------------
say("")
say("=== 3. DOES THE TAP DECIDE WHO GETS THE BALL? ===")
say("If the ruck's tap only weakly determines possession, even a correctly")
say("priced swing belongs mostly to the midfielders at the stoppage.")
say_dt(resolved[, .(n = .N, pct = round(100 * .N / nrow(resolved), 1),
                    mean_exp_pts = round(mean(nxt_pts), 4)),
                by = .(nxt_desc)][order(-n)][1:12], 12)

say("")
say("=== 4. VERDICT INPUT ===")
say("Read parts 1 and 2 together. If the bounce delta is a reset, the 0.3925")
say("that justifies amplifying the ruck weights 3.14x measures the restart, not")
say("the contest, and the amplification does not ship. Part 2's swing is the")
say("number that would replace it, derived the way every other v3 channel is.")

close(con)
cat("\nDone\n")
