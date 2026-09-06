# Why is contest value small? Take the contest apart and count everything.
#
# Four questions, answered with numbers rather than reasoning:
#   1. How many aerial contests does a player actually face per game, by position?
#   2. What is the EPV swing at stake in one -- attacking possession against
#      opposition possession?
#   3. What SHARE of that swing does the contestant receive? The identity splits
#      a kick three ways (kicker / contest / whoever plays on next), and only the
#      middle one is the contest channel.
#   4. Is there anything equivalent to v2's intercept-mark multiplier, where a
#      defensive act is paid at 1.0 instead of the ordinary 0.5?
#
# The identity, kicking-team frame:
#   delta_epv = (V_pre - exp_pts)   -> kicker,        "disp_credit"
#             + (V_branch - V_pre)  -> the contest,   zero-sum winner/loser
#             + (V_after - V_branch) -> the NEXT row,  "play_resid"
# with V_pre = (1-p)V_att + p*V_def and Delta = V_att - V_def, so the contest
# term is +p*Delta when the attack retains and -(1-p)*Delta when the defence
# wins. The winner banks the magnitude and the loser sheds it.
#
# Reads epv3_contest_table.parquet and the v3 player-game frame. ~2 min.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_contest_anatomy.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 45) for (l in capture.output(print(utils::head(x, n)))) say(l)

cst <- as.data.table(read_parquet(file.path(OUT_DIR, "epv3_contest_table.parquet")))
say("=== Anatomy of an aerial contest ===")
say("contests ", format(nrow(cst), big.mark = ","),
    " over ", uniqueN(cst$match_id), " matches")

# ---------------------------------------------------------------------------
# 1. how many contests, and who is in them
# ---------------------------------------------------------------------------
say("")
say("=== 1. VOLUME ===")
per_match <- nrow(cst) / uniqueN(cst$match_id)
say("aerial contests per match: ", round(per_match, 1),
    "   per team: ", round(per_match / 2, 1))
say("defence wins: ", round(100 * mean(cst$def_win), 1), "% of contests")
say("")
say("A contest names its WINNER always and its LOSER only when a Contest Target")
say("row happened to be logged, which is why the debit has to be allocated:")
say("  winner named: ", round(100 * mean(!is.na(cst$out_pid)), 1), "%")
say("  loser named:  ", round(100 * mean(!is.na(cst$loser_pid)), 1), "%")

pgd <- as.data.table(read_parquet(file.path(OUT_DIR, "epv3_cal_pgd_3ch_raw_nostd.parquet")))
if (!"contests_won" %in% names(pgd)) {
  pgd <- as.data.table(read_parquet(file.path(OUT_DIR, "epv3_player_game_v3.parquet")))
}
say("")
say("contests WON per player-game, by listed position (the only side the chain")
say("names, so this is a win count, not an involvement count):")
w <- pgd[!is.na(position_group), .(
  player_games = .N,
  won_per_game = round(mean(contests_won, na.rm = TRUE), 2),
  won_p90 = round(quantile(contests_won, .9, na.rm = TRUE), 1),
  won_max = max(contests_won, na.rm = TRUE)), by = position_group]
setorder(w, -won_per_game)
say_dt(w, 10)
say("")
say("total wins per team-match must equal contests per team-match: ",
    round(sum(pgd$contests_won, na.rm = TRUE) / (2 * uniqueN(pgd$match_id)), 1),
    " against ", round(per_match / 2, 1))

# ---------------------------------------------------------------------------
# 2. the swing at stake
# ---------------------------------------------------------------------------
say("")
say("=== 2. THE SWING AT STAKE ===")
say("Delta = V_att - V_def: the difference between the attack keeping the ball")
say("and the defence taking it, in expected points, at the moment of contest.")
qq <- function(v) round(quantile(v, c(.1, .25, .5, .75, .9), na.rm = TRUE), 3)
say_dt(data.table(
  quantity = c("Delta (V_att - V_def)", "p (P(defence wins))",
               "V_att", "V_def", "V_pre", "exp_pts (pre-kick state)"),
  mean = round(c(mean(cst$Delta), mean(cst$p_hat), mean(cst$V_att_hat),
                 mean(cst$V_def_hat), mean(cst$V_pre), mean(cst$exp_pts)), 3),
  sd = round(c(sd(cst$Delta), sd(cst$p_hat), sd(cst$V_att_hat),
               sd(cst$V_def_hat), sd(cst$V_pre), sd(cst$exp_pts)), 3)), 6)
say("")
say("Delta quantiles: ", paste(names(qq(cst$Delta)), qq(cst$Delta), sep = " ", collapse = "  "))
say("p quantiles:     ", paste(names(qq(cst$p_hat)), qq(cst$p_hat), sep = " ", collapse = "  "))

# ---------------------------------------------------------------------------
# 3. how the swing is split -- THIS is the answer to "why is contest small"
# ---------------------------------------------------------------------------
say("")
say("=== 3. HOW THE SWING IS SPLIT ===")
cst[, tot := disp_credit + cont_att + play_resid]
say("identity check, max |delta_epv - (disp + cont + play)|: ",
    signif(max(abs(cst$delta_epv - cst$tot), na.rm = TRUE), 3))
parts <- data.table(
  part = c("disp_credit  (V_pre - exp_pts)  -> the KICKER",
           "contest      (V_branch - V_pre) -> WINNER banks, loser sheds",
           "play_resid   (V_after - V_branch) -> the NEXT chain row",
           "TOTAL        delta_epv"),
  mean = round(c(mean(cst$disp_credit), mean(cst$cont_att), mean(cst$play_resid),
                 mean(cst$delta_epv)), 4),
  mean_abs = round(c(mean(abs(cst$disp_credit)), mean(abs(cst$cont_att)),
                     mean(abs(cst$play_resid)), mean(abs(cst$delta_epv))), 4),
  sd = round(c(sd(cst$disp_credit), sd(cst$cont_att), sd(cst$play_resid),
               sd(cst$delta_epv)), 4))
parts[, share_of_abs_pct := round(100 * mean_abs / mean_abs[4], 1)]
say_dt(parts, 5)
say("")
say("The share_of_abs column is the answer to 'how much credit do they get'.")
say("A contest is settled inside a kick whose OUTCOME value mostly lands on the")
say("player who does the next thing, not on the player who won the ball.")

say("")
say("--- the contest term, separately for the two outcomes ---")
say_dt(cst[, .(n = .N,
               mean_Delta = round(mean(Delta), 3),
               mean_p = round(mean(p_hat), 3),
               credit_to_winner = round(mean(abs(cont_att)), 3),
               as_pct_of_Delta = round(100 * mean(abs(cont_att)) / mean(Delta), 1)),
           by = .(outcome = fifelse(def_win, "defence wins", "attack retains"))], 5)
say("")
say("This is the payout rule and it has no free parameter: beating a contest")
say("you were expected to lose pays more than winning a gimme, because the")
say("payout IS the surprise. A defensive win pays (1-p)*Delta and an attacking")
say("one pays p*Delta, and p averages ", round(mean(cst$p_hat), 3), " -- so the")
say("defence, winning the less likely outcome, is paid the larger share.")

# ---------------------------------------------------------------------------
# 4. is there anything like v2's intercept-mark multiplier?
# ---------------------------------------------------------------------------
say("")
say("=== 4. IS THERE A MULTIPLIER, AS v2 HAS FOR INTERCEPT MARKS? ===")
say("v2 constants:")
say("  EPV_RECV_SCALE                 ", EPV_RECV_SCALE,
    "   ordinary reception share")
say("  EPV_RECV_INTERCEPT_MARK_SCALE  ", EPV_RECV_INTERCEPT_MARK_SCALE,
    "   an intercept mark is paid DOUBLE")
say("  EPV_DISP_SCALE                 ", EPV_DISP_SCALE)
say("")
say("v3 has NO equivalent. The contest term is p*Delta exactly, with no share")
say("parameter, because the split falls out of the identity rather than being")
say("chosen. That is the design's main claim -- and it is also why a contest")
say("winner is paid LESS than v2 pays an intercept marker:")
say("")
iv <- cst[def_win == TRUE]
say_dt(data.table(
  measure = c("v3: credit to the defensive winner  (1-p)*Delta",
              "v2 equivalent: 1.0 * |delta_epv| on the same kicks",
              "v2 ordinary reception: 0.5 * |delta_epv|"),
  mean = round(c(mean(abs(iv$cont_att)), mean(abs(iv$delta_epv)),
                 0.5 * mean(abs(iv$delta_epv))), 4)), 5)
say("")
say("So on the very kicks v2 pays an intercept marker in full, v3 pays ",
    round(100 * mean(abs(iv$cont_att)) / mean(abs(iv$delta_epv)), 1), "%.")
say("Whether that is right depends on what the rest of the swing IS: under v3")
say("it has not vanished, it has gone to the kicker (who is charged for a kick")
say("whose expected value was already low) and to the next chain row (whoever")
say("actually used the ball). v2 pays the intercept marker for all three.")

# ---------------------------------------------------------------------------
# 5. and the team-level consequence
# ---------------------------------------------------------------------------
say("")
say("=== 5. WHAT REACHES THE TEAM TOTAL ===")
say("The channel is zero-sum, so a team's total contest value is its SURPLUS:")
say("what it won above what it lost. That is what the margin regression sees.")
tm <- cst[, .(won = sum(abs(cont_att))), by = .(match_id, team_id = winner_tid)]
lo <- cst[, .(lost = sum(abs(cont_att))), by = .(match_id, team_id = loser_tid)]
tt <- merge(tm, lo, by = c("match_id", "team_id"), all = TRUE)
tt[is.na(won), won := 0]; tt[is.na(lost), lost := 0]
tt[, surplus := won - lost]
say_dt(data.table(
  quantity = c("contest value WON per team-match", "contest value LOST per team-match",
               "SURPLUS (won - lost)"),
  mean = round(c(mean(tt$won), mean(tt$lost), mean(tt$surplus)), 2),
  sd = round(c(sd(tt$won), sd(tt$lost), sd(tt$surplus)), 2)), 5)
say("")
say("Two numbers to compare: a team wins and loses ~", round(mean(tt$won), 0),
    " points of contest value")
say("in a match, but the SURPLUS -- the only part that is not cancelled -- has")
say("sd ", round(sd(tt$surplus), 1), ". Almost all of the gross value nets out")
say("against the opponent doing the same thing, and only the residual can")
say("possibly show up in a margin. That is the mechanical reason contest value")
say("is small in the rating while being large in the game.")

close(con)
cat("\nDone\n")
