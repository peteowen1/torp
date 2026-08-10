# Comparing debit-allocation rules where they can actually differ: PER PLAYER.
#
# MY TEST DESIGN WAS WRONG AND THIS FIXES IT. The previous arm compared
# allocation rules by the contest channel's conversion to margin at the
# TEAM-MATCH level. Every conserving rule -- flat, positional matchup, the AFL
# one-on-one ledger -- distributes the same debit among the same team's players,
# so the team total is IDENTICAL by construction. The three rules duly came back
# 0.384 / 0.380 / 0.387, which is arithmetic, not evidence.
#
# It also means the earlier reading of "flat spreading costs 0.924 -> 0.384" was
# wrong. That gap is DROPPING the unnamed debits versus INCLUDING them, not the
# spreading method: `none` sums to gross wins where the others sum to net
# surplus. Gross wins may simply be a better team-dominance proxy.
#
# Allocation rules differ in WHO is charged, so they have to be judged on
# player-level properties:
#
#   repeatability   does a player's contest rating persist year over year? A
#                   rule that charges the wrong players adds noise that will not
#                   repeat.
#   count-dependence  cor with contested marks and spoils. A rule that collapses
#                   into a count of events has stopped measuring skill.
#   face validity   who tops the channel, and do the biggest DEBITS land on
#                   players who plausibly lost duels.
#   spread          a rule that concentrates the debit on a few players creates
#                   more separation; whether that separation is real is what
#                   repeatability answers.
#
# No arm is declared a winner on any single one of these.
#
# ~4 min, cached frames only.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_alloc_player_level.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 45) for (l in capture.output(print(utils::head(x, n)))) say(l)

ARMS <- c(team = "epv3_duel_pgd_duel_team.parquet",
          none = "epv3_duel_pgd_duel_none.parquet",
          mirror = "epv3_duel_pgd_duel_mirror.parquet",
          ledger = "epv3_duel_pgd_duel_ledger.parquet")

say("=== Debit allocation rules, judged per player ===")
say("run at ", format(Sys.time()))
say("The team-level test cannot separate these -- every conserving rule gives")
say("the same team total. These are the properties that can differ.")

# Season-to-season repeatability of a player's per-game contest rate.
yoy <- function(d) {
  s <- d[, .(v = sum(epv_spoil, na.rm = TRUE), g = .N), by = .(player_id, season)][g >= 8]
  s[, rate := v / g]
  a <- s[, .(player_id, season, rate)]
  b <- copy(a)[, season := season - 1]; setnames(b, "rate", "rate_next")
  m <- merge(a, b, by = c("player_id", "season"))[is.finite(rate) & is.finite(rate_next)]
  c(n = nrow(m), r = round(cor(m$rate, m$rate_next), 4))
}
# Split-half within a season: odd games against even games, same player.
splithalf <- function(d) {
  x <- copy(d)[is.finite(epv_spoil)]
  setorder(x, player_id, season, utc_start_time)
  x[, i := seq_len(.N), by = .(player_id, season)]
  s <- x[, .(a = mean(epv_spoil[i %% 2 == 1]), b = mean(epv_spoil[i %% 2 == 0]),
             n = .N), by = .(player_id, season)][n >= 12 & is.finite(a) & is.finite(b)]
  c(n = nrow(s), r = round(cor(s$a, s$b), 4))
}

rows <- list()
for (nm in names(ARMS)) {
  f <- file.path(OUT_DIR, ARMS[[nm]])
  if (!file.exists(f)) { say(""); say("MISSING: ", ARMS[[nm]]); next }
  d <- as.data.table(read_parquet(f))
  y <- yoy(d); s <- splithalf(d)
  cur <- d[season == max(season)]
  agg <- cur[, .(g = .N, contest = sum(epv_spoil, na.rm = TRUE)),
             by = .(player_name, position_group)][g >= 8]

  say(""); say("=== ", nm, " ===")
  say(sprintf("  year-over-year r %.4f (n %d) | within-season split-half r %.4f (n %d)",
              y[["r"]], y[["n"]], s[["r"]], s[["n"]]))
  say(sprintf("  per player-game: sd %.4f  mean %+.4f  |  cor(contested_marks) %.3f  cor(spoils) %.3f",
              sd(d$epv_spoil, na.rm = TRUE), mean(d$epv_spoil, na.rm = TRUE),
              cor(d$epv_spoil, d$contested_marks, use = "complete.obs"),
              cor(d$epv_spoil, d$spoils, use = "complete.obs")))
  say(sprintf("  share of players with a NEGATIVE season contest total: %.1f%%",
              100 * mean(agg$contest < 0)))
  setorder(agg, -contest)
  say("  top 6:"); say_dt(agg[1:6, .(player_name, position_group, contest = round(contest, 1))], 6)
  setorder(agg, contest)
  say("  bottom 6 (the biggest debits -- do these look like players who lost duels?):")
  say_dt(agg[1:6, .(player_name, position_group, contest = round(contest, 1))], 6)

  rows[[nm]] <- data.table(arm = nm, yoy = y[["r"]], splithalf = s[["r"]],
                           sd = round(sd(d$epv_spoil, na.rm = TRUE), 3),
                           cor_cm = round(cor(d$epv_spoil, d$contested_marks, use = "complete.obs"), 3),
                           cor_spoils = round(cor(d$epv_spoil, d$spoils, use = "complete.obs"), 3),
                           pct_negative = round(100 * mean(agg$contest < 0), 1))
}

say("")
say("=== SIDE BY SIDE ===")
say_dt(rbindlist(rows), 6)
say("")
say("HOW TO READ IT. Higher repeatability with count-dependence FLAT or falling")
say("is the good direction -- that is a rule charging the right players. Higher")
say("repeatability bought with rising cor(contested_marks) is the degenerate")
say("case: the channel has become an event count.")
say("")
say("`none` is not a candidate -- it drops ~69% of the debits, so a player can")
say("barely lose contest value. It is here as the ceiling, to show what the")
say("channel looks like when no unnamed debit is charged to anybody.")

saveRDS(rbindlist(rows), file.path(OUT_DIR, "epv3_alloc_player_level.rds"))
close(con)
cat("\nDone\n")
