# EVERY play type, what happens to its value, and what is left unallocated.
#
# Two jobs.
#
# ONE: correct the centre-bounce claim. This session asserted that the +0.7912
# on Centre Bounce rows is a "reset artifact" and that the 0.3925 swing figure
# behind EPV_RUCK_SWING_SCALE is ~93% artifact. Pete's objection is that a
# centre bounce is a genuinely NEUTRAL state -- neither team has the ball, so
# EP = 0 by symmetry -- and winning the clearance genuinely moves you to +0.79.
# If he is right, the value is real, the gatherer correctly receives half, and
# the other half is unallocated because a stoppage HAS no disposer. That would
# make 0.3925 the legitimate unallocated share rather than an artifact.
#
# Decisive checks:
#   - is exp_pts EXACTLY 0 on every Centre Bounce row, or does it vary? A
#     constant is a placeholder; a distribution is a model output.
#   - what does the model say about other neutral-ish states (Ball Up Call)?
#   - how much of each stoppage's value is paid out, and how much is left?
#
# TWO: the complete accounting table Pete asked for -- every description in
# chains, its volume, and what the engine does with its value. Nothing omitted,
# including the ones we do nothing with.
#
# ~4 min.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_full_play_accounting.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 80) for (l in capture.output(print(utils::head(x, n)))) say(l)

full <- as.data.table(load_pbp(TRUE))
p <- data.table(match_id = full$match_id, display_order = full$display_order,
                description = full$description, delta_epv = full$delta_epv,
                exp_pts = full$exp_pts, pos_team = full$pos_team,
                player_id = full$player_id, lead_player_id = full$lead_player_id)
rm(full); invisible(gc())
nm <- uniqueN(p$match_id)
pf <- p[is.finite(delta_epv)]

# ---------------------------------------------------------------------------
say("=== 1. IS THE CENTRE-BOUNCE exp_pts A PLACEHOLDER OR A MODEL OUTPUT? ===")
say("A constant is a placeholder. A distribution is the model saying something.")
chk <- p[description %chin% c("Centre Bounce", "Ball Up Call", "Throw In",
                              "Out of Bounds", "Kick", "Handball"),
         .(n = .N,
           mean_exp_pts = round(mean(exp_pts, na.rm = TRUE), 4),
           sd_exp_pts = round(sd(exp_pts, na.rm = TRUE), 6),
           min = round(min(exp_pts, na.rm = TRUE), 4),
           max = round(max(exp_pts, na.rm = TRUE), 4),
           pct_exactly_zero = round(100 * mean(exp_pts == 0, na.rm = TRUE), 1)),
         by = description]
setorder(chk, -n); say_dt(chk, 8)
say("")
say("VERDICT LOGIC: if Centre Bounce reads sd 0 and 100% exactly zero, exp_pts")
say("is HARD-CODED there. That still does not make it wrong -- 0 is the correct")
say("value for a neutral contest by symmetry -- but it means the number is an")
say("assumption of the pipeline rather than something the EP model estimated.")

# ---------------------------------------------------------------------------
say("")
say("=== 2. HOW MUCH OF EACH STOPPAGE'S VALUE IS ACTUALLY PAID OUT? ===")
say("A stoppage has no DISPOSER -- the ball comes from the umpire -- so the")
say("disposer half of the usual 50/50 has nobody to go to.")
DS <- EPV_DISP_SCALE; RS <- EPV_RECV_SCALE
st <- pf[description %chin% c("Centre Bounce", "Ball Up Call")]
st[, paid_recv := fifelse(!is.na(lead_player_id), delta_epv * pos_team * RS, 0)]
st[, paid_disp := fifelse(!is.na(player_id), delta_epv * DS, 0)]
st[, unallocated := delta_epv - paid_recv - paid_disp]
z <- st[, .(n = .N, per_match = round(.N / nm, 1),
            mean_delta = round(mean(delta_epv), 4),
            mean_paid_recv = round(mean(paid_recv), 4),
            mean_paid_disp = round(mean(paid_disp), 4),
            mean_unallocated = round(mean(unallocated), 4),
            per_match_unalloc = round(sum(unallocated) / nm, 2)), by = description]
say_dt(z, 4)
say("")
say("The documented figure behind EPV_RUCK_SWING_SCALE's 3.14x is a swing of")
say("0.3925 per ruck contest, described as 'credited to NOBODY'. Compare it")
say("with mean_unallocated above -- if they match, the 3.14x premise is SOUND")
say("and this session's 'artifact' verdict was wrong.")
say("")
say("What the three ruck box weights pay for one contest, both rucks combined:")
say("  EPV_HITOUT_WT + EPV_HITOUT_ADV_WT*share + EPV_RUCK_CONTEST_WT ~ 0.1249")
say("  a tap WINNER gets EPV_HITOUT_WT + EPV_RUCK_CONTEST_WT = ",
    round(EPV_HITOUT_WT + EPV_RUCK_CONTEST_WT, 4))

# ---------------------------------------------------------------------------
say("")
say("=== 3. EVERY PLAY TYPE: VOLUME, VALUE, AND WHERE IT GOES ===")
say("disp   = credited to player_id       (EPV_DISP_SCALE of delta_epv)")
say("recv   = credited to lead_player_id  (EPV_RECV_SCALE, sign-flipped on a turnover)")
say("unallo = the remainder, credited to nobody through the chain path")
pf[, paid_recv := fifelse(!is.na(lead_player_id), delta_epv * pos_team * RS, 0)]
pf[, paid_disp := fifelse(!is.na(player_id) &
     description %chin% c("Kick", "Handball", "Ground Kick"), delta_epv * DS, 0)]
pf[, unallocated := delta_epv - paid_recv - paid_disp]

acct <- pf[, .(n = .N, per_match = round(.N / nm, 1),
               mean_delta = round(mean(delta_epv), 4),
               gross_abs = round(sum(abs(delta_epv))),
               disp_pm = round(sum(paid_disp) / nm, 2),
               recv_pm = round(sum(paid_recv) / nm, 2),
               unalloc_pm = round(sum(unallocated) / nm, 2)), by = description]
setorder(acct, -gross_abs)
say_dt(acct, 60)

say("")
say("=== 4. TOTALS PER MATCH ===")
say(sprintf("  paid to disposers   %8.2f", sum(pf$paid_disp) / nm))
say(sprintf("  paid to receivers   %8.2f", sum(pf$paid_recv) / nm))
say(sprintf("  UNALLOCATED         %8.2f", sum(pf$unallocated) / nm))
say(sprintf("  total delta_epv     %8.2f", sum(pf$delta_epv) / nm))
say("")
say("--- where the unallocated value sits, biggest first ---")
un <- pf[, .(unalloc_pm = round(sum(unallocated) / nm, 2),
             pct_rows_no_disposer = round(100 * mean(paid_disp == 0), 1)),
         by = description][order(-abs(unalloc_pm))]
say_dt(un[1:20], 20)
say("")
say("Rows with no disposer are the ones where the 50/50 cannot complete: a")
say("stoppage (umpire), a turnover the opposition gathers, a ball out of")
say("bounds. That is the honest home of the ruck carve-out -- NOT the whole")
say("swing, just the half with nobody to pay.")

close(con)
cat("\nDone\n")
