# Is the "contest" population actually contests?
#
# PETE'S CHALLENGE, and it goes to the definition rather than the tuning: a duel
# has a winner AND a loser. If chains names the loser only 11.6% of the time,
# either chains is failing to log him or most of the population is not a duel at
# all -- it is an ordinary reception with nobody to beat.
#
# The code already disagrees with itself about this. EPV3_AERIAL_OUT, which
# DEFINES the population, includes `Uncontested Mark` and `Mark On Lead`.
# EPV3_AERIAL_EXPOSURE_DESCS, which defines who was exposed to a contest,
# excludes them and says why: "An uncontested mark means by definition that no
# contest happened". Both cannot be right.
#
# The original design argument for including them is not silly: the branch model
# prices every aerial kick, an uncontested mark gets p = P(defence wins) near
# zero, so it earns p * Delta which is near zero too. Nothing is over-credited.
#
# But the DEBIT does not behave that way. Every contest with an unnamed loser
# has its debit spread flat across the defending team -- so conceding an
# uncontested mark charges 22 players for a duel nobody entered. That is a
# common-mode term added to every player's contest rating, and it is exactly the
# kind of thing that dilutes a channel until it stops predicting anything.
#
# What this measures:
#   1. the population, by outcome type -- how much is genuinely contested
#   2. the CREDIT MASS by outcome type, which is what actually matters
#   3. the named-loser rate within genuine duels only
#   4. how many genuine duels per match, against the AFL API's one-on-one count
#      (~26/match). If those line up, the "ledger" allocation was rejected on a
#      population ten times too large and deserves re-testing.
#
# Reads the cached contest table. ~1 min, no rebuild.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_duel_population.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

cst <- as.data.table(read_parquet(file.path(OUT_DIR, "epv3_contest_table.parquet")))
nm <- uniqueN(cst$match_id)
say("=== Is the contest population actually contests? ===")
say("rows ", format(nrow(cst), big.mark = ","), " over ", nm, " matches (",
    round(nrow(cst) / nm, 1), " per match)")
say("")
say("EPV3_AERIAL_OUT (what DEFINES the population):")
say("  ", paste(EPV3_AERIAL_OUT, collapse = ", "))
say("")
say("EPV3_AERIAL_EXPOSURE_DESCS excludes Uncontested Mark / Mark On Lead: ",
    !("Uncontested Mark" %in% EPV3_AERIAL_EXPOSURE_DESCS))

# A duel is an outcome where two players could plausibly have contested the ball.
CONTESTED <- c("Contested Mark", "Pack Mark (P)", "Pack Mark (O)",
               "Spoil", "Spoil gaining possession", "Spoil ineffective")
cst[, genuine := out_desc %chin% CONTESTED]

say("")
say("=== 1. POPULATION BY OUTCOME ===")
t1 <- cst[, .(n = .N, per_match = round(.N / nm, 1),
              pct = round(100 * .N / nrow(cst), 1),
              def_win_pct = round(100 * mean(def_win), 1),
              loser_named_pct = round(100 * mean(!is.na(loser_pid)), 1),
              mean_p = round(mean(p_hat), 3),
              mean_Delta = round(mean(Delta), 3)), by = .(out_desc, genuine)]
setorder(t1, -n)
say_dt(t1, 12)

say("")
say("=== 2. CREDIT MASS -- the number that decides it ===")
say("Population share is not the question; a near-zero-p outcome earns")
say("near-zero credit. What matters is where the |credit| actually is.")
cst[, mass := abs(cont_att)]
t2 <- cst[, .(n = .N, per_match = round(.N / nm, 1),
              total_mass = round(sum(mass)),
              mean_credit = round(mean(mass), 4)), by = genuine]
t2[, mass_pct := round(100 * total_mass / sum(total_mass), 1)]
t2[, pop_pct := round(100 * n / sum(n), 1)]
setorder(t2, -total_mass)
say_dt(t2[, .(genuine, n, per_match, pop_pct, total_mass, mass_pct, mean_credit)], 4)

say("")
say("--- and the UNNAMED DEBIT, which is what gets smeared across a team ---")
un <- cst[is.na(loser_pid), .(n = .N, per_match = round(.N / nm, 1),
                              debit = round(sum(abs(loser_credit)))), by = genuine]
un[, debit_pct := round(100 * debit / sum(debit), 1)]
setorder(un, -debit)
say_dt(un, 4)
say("")
say("Every point in the non-genuine row is a debit charged to 22 players for a")
say("duel that did not happen. If that share is large, the contest channel of")
say("every player in the competition carries a common-mode term built from")
say("UNCONTESTED marks.")

say("")
say("=== 3. WITHIN GENUINE DUELS ONLY ===")
g <- cst[genuine == TRUE]
say("genuine duels: ", format(nrow(g), big.mark = ","), " (",
    round(nrow(g) / nm, 1), " per match, ", round(nrow(g) / nm / 2, 1), " per team)")
say("defence wins:        ", round(100 * mean(g$def_win), 1), "%")
say("loser NAMED:         ", round(100 * mean(!is.na(g$loser_pid)), 1),
    "%   (against ", round(100 * mean(!is.na(cst$loser_pid)), 1), "% over the whole population)")
say("mean p (P def wins): ", round(mean(g$p_hat), 3),
    "   against ", round(mean(cst$p_hat), 3), " overall")
say("mean Delta:          ", round(mean(g$Delta), 3),
    "   against ", round(mean(cst$Delta), 3), " overall")
say("")
say("--- named-loser rate by outcome, genuine duels ---")
say_dt(g[, .(n = .N, loser_named_pct = round(100 * mean(!is.na(loser_pid)), 1),
             def_win_pct = round(100 * mean(def_win), 1)), by = out_desc][order(-n)], 8)
say("")
say("A loser can only be named when a `Contest Target` row was logged in the")
say("in-flight span. Recon found 0 of 7,736 ATTACKING mark wins carry an")
say("opposing row, so the ceiling on naming is set by defensive wins.")
say("  defence-win duels with a named loser: ",
    round(100 * mean(!is.na(g[def_win == TRUE]$loser_pid)), 1), "%")
say("  attack-win duels with a named loser:  ",
    round(100 * mean(!is.na(g[def_win == FALSE]$loser_pid)), 1), "%")

say("")
say("=== 4. DOES THE DUEL COUNT MATCH THE AFL API's ONE-ON-ONE LEDGER? ===")
say("The `ledger` allocation rule -- weight the debit by the player's OWN")
say("recorded one-on-one losses -- was measured and rejected (year-over-year")
say("0.485 against `team`'s 0.819). It was scored against the FULL population.")
ps <- tryCatch(as.data.table(load_player_stats(TRUE)), error = function(e) NULL)
if (!is.null(ps) && "contest_def_one_on_ones" %in% names(ps)) {
  z <- function(v) fifelse(is.na(v), 0, as.numeric(v))
  oo <- ps[, .(def_oo = sum(z(contest_def_one_on_ones)),
               off_oo = sum(z(contest_off_one_on_ones))), by = match_id]
  say("AFL API one-on-ones per match: defensive ", round(mean(oo$def_oo), 1),
      ", offensive ", round(mean(oo$off_oo), 1))
  say("chain genuine duels per match: ", round(nrow(g) / nm, 1))
  say("chain FULL population per match: ", round(nrow(cst) / nm, 1))
  say("")
  say("If the genuine-duel count is close to the API's one-on-one count, then")
  say("the ledger rule was being asked to spread a debit built from ~",
      round(nrow(cst) / nm), " events")
  say("using a key that counts ~", round(mean(oo$def_oo) + mean(oo$off_oo)),
      " -- a ten-to-one mismatch, which would sink any key.")
  say("That is a reason to RE-TEST it on the restricted population, not a")
  say("reason to believe the old verdict.")
} else {
  say("player_stats one-on-one columns unavailable here; skipping the comparison.")
}

say("")
say("=== 5. WHAT RESTRICTING WOULD DO TO THE CHANNEL ===")
say("Credit is zero-sum, so dropping non-duels removes both credit and debit.")
say("Per team-match, gross and surplus, on each population:")
for (lab in c("all", "genuine")) {
  d <- if (lab == "all") cst else g
  w <- d[, .(won = sum(abs(cont_att))), by = .(match_id, team_id = winner_tid)]
  l <- d[, .(lost = sum(abs(cont_att))), by = .(match_id, team_id = loser_tid)]
  tt <- merge(w, l, by = c("match_id", "team_id"), all = TRUE)
  tt[is.na(won), won := 0]; tt[is.na(lost), lost := 0]
  tt[, surplus := won - lost]
  say(sprintf("  %-8s gross won %6.2f   surplus sd %5.2f", lab, mean(tt$won), sd(tt$surplus)))
}
say("")
say("The surplus sd is the ceiling on how much of this can ever reach a margin.")
say("If restricting RAISES it, the non-duels were adding noise, not signal --")
say("and the contest channel has been diluted rather than correctly small.")

close(con)
cat("\nDone\n")
