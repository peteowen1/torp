# Does the AFL API's one-on-one contest ledger actually arrive populated?
#
# AFL-API-REFERENCE.md lists contestDefOneOnOnes / contestDefLosses /
# contestOffOneOnOnes / contestOffWins in playerStats extendedStats. If those are
# real, they solve v3's biggest structural weakness: chains names the beaten
# aerial opponent in only ~12% of contests, and these fields are a per-player
# count of exactly the contests he lost.
#
# That is NOT a box-score weight sneaking back into EPV. The credit stays
# entirely delta_epv-derived; the stat only decides WHOSE ledger an already
# chain-derived debit lands on. It is the allocation key, not the price.
#
# Also checks the ruck fields, which could let cont_stop split a stoppage swing
# by who actually won the tap instead of paying a flat per-hitout constant.

suppressPackageStartupMessages({ library(data.table) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT <- "C:/dev/torpverse/torp/data-raw/outputs/epv3_contest_stats_check.txt"
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 60) for (l in capture.output(print(utils::head(x, n)))) say(l)

ps <- as.data.table(load_player_stats(TRUE))
say("=== AFL API one-on-one contest ledger: is it there? ===")
say("player_stats rows ", format(nrow(ps), big.mark = ","),
    " | cols ", ncol(ps),
    " | seasons ", paste(range(ps$season, na.rm = TRUE), collapse = "-"))

want <- c("contest_def_one_on_ones", "contest_def_losses", "contest_def_loss_percentage",
          "contest_off_one_on_ones", "contest_off_wins", "contest_off_wins_percentage",
          "hitouts", "hitouts_to_advantage", "hitout_win_percentage",
          "hitout_to_advantage_rate", "ruck_contests", "centre_bounce_attendances",
          "intercept_marks", "marks_on_lead", "spoils", "one_percenters")
say("")
say("--- presence and fill ---")
rows <- rbindlist(lapply(want, function(w) {
  if (!w %in% names(ps)) return(data.table(field = w, present = FALSE, pct_nonzero = NA_real_,
                                           mean = NA_real_, max = NA_real_))
  v <- suppressWarnings(as.numeric(ps[[w]]))
  data.table(field = w, present = TRUE,
             pct_nonzero = round(100 * mean(!is.na(v) & v != 0), 1),
             mean = round(mean(v, na.rm = TRUE), 3),
             max = round(max(v, na.rm = TRUE), 1))
}))
say_dt(rows, 20)

# Which seasons carry them? A field that only exists from 2023 cannot be the
# allocation key for a 2021-2026 rating without a documented fallback.
say("")
say("--- coverage BY SEASON (pct of player-games with a non-zero value) ---")
have <- intersect(want, names(ps))
cov <- ps[, lapply(.SD, function(v) round(100 * mean(!is.na(v) & v != 0), 1)),
          .SDcols = have, by = season][order(season)]
say_dt(cov, 12)

if (all(c("contest_def_one_on_ones", "contest_def_losses") %in% names(ps))) {
  say("")
  say("--- does the ledger behave like a real contest count? ---")
  d <- ps[!is.na(contest_def_one_on_ones)]
  say("player-games with >=1 defensive one-on-one: ",
      format(d[contest_def_one_on_ones > 0, .N], big.mark = ","),
      " (", round(100 * mean(d$contest_def_one_on_ones > 0), 1), "%)")
  say("mean per player-game ", round(mean(d$contest_def_one_on_ones), 3),
      " | max ", max(d$contest_def_one_on_ones))
  say("losses <= one-on-ones on every row: ",
      all(d$contest_def_losses <= d$contest_def_one_on_ones, na.rm = TRUE))
  say("league-wide defensive one-on-ones per match: ",
      round(d[, sum(contest_def_one_on_ones), by = match_id][, mean(V1)], 1))
  say("  (chains gives ~254 aerial contests per match; these are the ONE-ON-ONE")
  say("   subset, so a much smaller number here is expected and correct.)")

  say("")
  say("--- who leads defensive one-on-ones? (career, min 40 games) ---")
  agg <- ps[, .(gms = .N, oo = sum(contest_def_one_on_ones, na.rm = TRUE),
                loss = sum(contest_def_losses, na.rm = TRUE)),
            by = .(player_id, player_name)][gms >= 40]
  agg[, `:=`(oo_pg = round(oo / gms, 2), loss_pct = round(100 * loss / pmax(oo, 1), 1))]
  setorder(agg, -oo_pg)
  say_dt(agg[1:15, .(player_name, gms, oo_pg, loss_pct)], 15)
  say("")
  say("(These should be recognisable key defenders. If they are not, the field")
  say(" does not mean what the API reference says it means.)")
}

close(con)
cat("\nWrote ", OUT, "\n")
