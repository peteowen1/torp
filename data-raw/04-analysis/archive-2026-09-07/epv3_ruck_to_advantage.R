# Does the ruck's tap TO ADVANTAGE move the ball, over and above winning taps?
#
# THIS IS OWED. I wrote that the ruck's value was "refuted by cor = 0.082" and
# that was not a safe claim. It came from a team-match aggregate regression of
# clearance differential on raw hitout differential -- which is a correlational
# team-level quantity, and which never touched `hitouts_to_advantage` at all.
# Pete's prior is that the ruck should carry most of the value of winning a
# centre bounce or a throw-in, and that prior is not in conflict with anything
# measured so far, because the thing that would test it was not measured.
#
# The distinction that matters, and that raw hitouts cannot see:
#
#   hitouts                 taps won. Winning the tap while palming it into a
#                           scrum is worth close to nothing.
#   hitouts_to_advantage    taps won AND directed to a teammate. If the ruck has
#                           a skill beyond jumping, this is where it lives.
#
# So the test is the PARTIAL effect of hitouts-to-advantage holding raw hitouts
# fixed. If a tap-to-advantage differential buys clearances and stoppage EPV
# with raw taps controlled, the ruck is doing something a coin flip does not,
# and the current box weights are too small. If it does not, the 24.7%
# clean-tap ceiling stands.
#
# CONTROLS. A good midfield wins taps and clearances for reasons unrelated to
# the ruck, so every fit is repeated with the total disposal differential (team
# box-score strength) in it. The uncontrolled version is reported too, and the gap
# between them is the point: how much of the apparent ruck effect is the team.
#
# OUT OF SAMPLE. Fitted on the earlier half of matches, scored on the later
# half. An in-sample coefficient that does not reproduce is a fit, not a
# constant -- that is the specific trap that overturned five conclusions in one
# session, so it is checked here rather than assumed away.
#
# ~5 min, no model fitting beyond lm.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_ruck_to_advantage.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 45) for (l in capture.output(print(utils::head(x, n)))) say(l)
tid <- function(f) { s <- summary(f); data.table(term = rownames(s$coefficients),
  coef = round(s$coefficients[, 1], 4), t = round(s$coefficients[, 3], 2)) }

say("=== Does the tap TO ADVANTAGE do anything raw taps do not? ===")
say("run at ", format(Sys.time()))

ps <- as.data.table(load_player_stats(TRUE))
need <- c("hitouts", "hitouts_to_advantage", "ruck_contests", "centre_clearances",
          "stoppage_clearances", "clearances", "disposals")
have <- intersect(need, names(ps))
say("box columns present: ", paste(have, collapse = ", "))
miss <- setdiff(need, have)
if (length(miss)) say("MISSING (dropped from the fits): ", paste(miss, collapse = ", "))

agg_cols <- have
# player_stats has no team_name column -- only team_id plus the two fixture
# names and a Home/Away flag. Build the name from the flag rather than assuming
# a column that is not there.
ps[, team := data.table::fifelse(team_status == "home", home_team_name, away_team_name)]
if (anyNA(ps$team)) {
  ps[is.na(team), team := data.table::fifelse(
    tolower(team_status) == "home", home_team_name, away_team_name)]
}
tm <- ps[!is.na(team), lapply(.SD, function(v) sum(v, na.rm = TRUE)), .SDcols = agg_cols,
         by = .(match_id = as.character(match_id), team)]

# Stoppage EPV per team-match, straight from the chain: the value of the state
# the winning team ends up in after each stoppage.
ch <- as.data.table(load_chains(TRUE))
pbp <- as.data.table(load_pbp(TRUE))
setorder(ch, match_id, display_order)
narrow <- data.table(match_id = ch$match_id, display_order = ch$display_order,
                     description = ch$description, team_id = ch$team_id)
rm(ch); invisible(gc())
narrow[, `:=`(nxt_d = shift(description, 1, type = "lead"),
              nxt_t = shift(team_id, 1, type = "lead")), by = match_id]
stp <- narrow[description %chin% c("Centre Bounce", "Ball Up Call") & !is.na(nxt_t)]
stp <- merge(stp, pbp[, .(match_id, display_order = display_order + 1L,
                          nxt_exp = exp_pts)],
             by = c("match_id", "display_order"), all.x = TRUE)
stp <- stp[is.finite(nxt_exp)]
say("stoppages with a valued next state: ", format(nrow(stp), big.mark = ","))
say("resolved by a clean tap (Gather From Hitout): ",
    round(100 * mean(stp$nxt_d == "Gather From Hitout", na.rm = TRUE), 1), "%")

tid_map <- unique(ps[!is.na(team), .(team_id = as.character(team_id), team)])
sv <- stp[, .(stop_epv = sum(nxt_exp)), by = .(match_id, team_id = as.character(nxt_t))]
sv <- merge(sv, tid_map, by = "team_id")[, .(match_id, team, stop_epv)]
tm <- merge(tm, sv, by = c("match_id", "team"), all.x = TRUE)
tm[is.na(stop_epv), stop_epv := 0]

res <- as.data.table(load_results(TRUE))
tg <- res[, .(match_id = as.character(match_id), home = home_team_name,
              away = away_team_name, margin = home_score - away_score)][is.finite(margin)]
h <- merge(tg, tm, by.x = c("match_id", "home"), by.y = c("match_id", "team"))
a <- merge(tg, tm, by.x = c("match_id", "away"), by.y = c("match_id", "team"))
kk <- intersect(names(h), names(a))
d <- merge(h, a, by = "match_id", suffixes = c("_h", "_a"))
for (cc in c(agg_cols, "stop_epv")) {
  d[, (paste0("d_", cc)) := get(paste0(cc, "_h")) - get(paste0(cc, "_a"))]
}
# `margin` is in both sides of the merge, so it comes back as margin_h/margin_a
# (identical values). Restore the plain name the fits below use.
if (!"margin" %in% names(d) && "margin_h" %in% names(d)) d[, margin := margin_h]
d <- d[is.finite(d_hitouts)]
say("matches ", nrow(d))

# Midfield / team-strength control. Total disposal differential rather than a
# rating: it is in the same box-score frame as the taps, needs no rating build,
# and is the same control the earlier increment measurement used, so the two are
# comparable. It is a coarse instrument and the caveat at the bottom says so.
d[, d_psr := d_disposals]
has_psr <- sum(is.finite(d$d_psr)) > 500
say("team-strength control (disposal differential) available: ", has_psr)

say(""); say("=== 1. DESCRIPTIVES ===")
say_dt(data.table(
  quantity = c("hitouts", "hitouts_to_advantage", "ruck_contests", "stoppage EPV"),
  per_team_match = round(c(mean(tm$hitouts), mean(tm$hitouts_to_advantage),
                           mean(tm$ruck_contests), mean(tm$stop_epv)), 2),
  sd_of_diff = round(c(sd(d$d_hitouts), sd(d$d_hitouts_to_advantage),
                       sd(d$d_ruck_contests), sd(d$d_stop_epv)), 2)), 4)
say("")
say("how much of a tap-win is a tap to advantage: ",
    round(100 * sum(tm$hitouts_to_advantage) / sum(tm$hitouts), 1), "%")
say("cor(d_hitouts, d_hitouts_to_advantage) = ",
    round(cor(d$d_hitouts, d$d_hitouts_to_advantage), 3),
    "  -- if this is near 1 the partial effect is not identified and")
say("  everything below is a variance-inflation artifact rather than a finding.")

say(""); say("=== 2. CLEARANCES ON TAPS, RAW vs TO ADVANTAGE ===")
if ("d_clearances" %in% names(d)) {
  say("-- raw taps only --");            say_dt(tid(lm(d_clearances ~ d_hitouts, data = d)), 4)
  say("-- taps + taps to advantage --"); say_dt(tid(lm(d_clearances ~ d_hitouts + d_hitouts_to_advantage, data = d)), 4)
  if (has_psr) { say("-- + team strength (disposal diff) --")
    say_dt(tid(lm(d_clearances ~ d_hitouts + d_hitouts_to_advantage + d_psr, data = d)), 5) }
}

say(""); say("=== 3. STOPPAGE EPV ON TAPS ===")
say("This is the quantity the cont_stop channel is trying to pay for.")
say("-- raw taps only --");            say_dt(tid(lm(d_stop_epv ~ d_hitouts, data = d)), 4)
say("-- taps + taps to advantage --"); say_dt(tid(lm(d_stop_epv ~ d_hitouts + d_hitouts_to_advantage, data = d)), 4)
if (has_psr) { say("-- + team strength (disposal diff) --")
  say_dt(tid(lm(d_stop_epv ~ d_hitouts + d_hitouts_to_advantage + d_psr, data = d)), 5) }

say(""); say("=== 4. MARGIN ON TAPS ===")
say_dt(tid(lm(margin ~ d_hitouts + d_hitouts_to_advantage, data = d)), 4)
if (has_psr) { say("-- + team strength --")
  say_dt(tid(lm(margin ~ d_hitouts + d_hitouts_to_advantage + d_psr, data = d)), 5) }

say(""); say("=== 5. DOES IT HOLD OUT OF SAMPLE? ===")
setorder(d, match_id)
cut <- floor(nrow(d) / 2)
f1 <- lm(d_stop_epv ~ d_hitouts + d_hitouts_to_advantage, data = d[1:cut])
f2 <- lm(d_stop_epv ~ d_hitouts + d_hitouts_to_advantage, data = d[(cut + 1):nrow(d)])
say_dt(data.table(term = names(coef(f1)),
                  first_half = round(coef(f1), 4), second_half = round(coef(f2), 4)), 4)

say(""); say("=== 6. WHAT A TAP TO ADVANTAGE IS WORTH, IN POINTS ===")
fbest <- if (has_psr) lm(d_stop_epv ~ d_hitouts + d_hitouts_to_advantage + d_psr, data = d) else
                      lm(d_stop_epv ~ d_hitouts + d_hitouts_to_advantage, data = d)
cf <- coef(fbest)
say(sprintf("  per extra tap won (not to advantage): %+.4f points of stoppage EPV", cf[["d_hitouts"]]))
say(sprintf("  per extra tap TO ADVANTAGE (on top): %+.4f points", cf[["d_hitouts_to_advantage"]]))
say("")
say("These are differentials, so one extra tap to us is one fewer contested by")
say("them; the per-ruck credit is half the differential coefficient.")
say(sprintf("  => per-ruck: %+.4f (tap) and %+.4f (tap to advantage)",
            cf[["d_hitouts"]] / 2, cf[["d_hitouts_to_advantage"]] / 2))
say(sprintf("  currently paid by the box weights: EPV_HITOUT_WT %s, EPV_HITOUT_ADV_WT %s",
            EPV_HITOUT_WT, EPV_HITOUT_ADV_WT))
say("")
say("STILL A CORRELATION. The disposal differential controls team strength with a")
say("coarse box-score aggregate -- it cannot separate 'this ruck is good' from")
say("'this ruck plays behind a good midfield that makes his taps look good'.")
say("A coefficient surviving that control is evidence the effect is not purely")
say("the team; it is not proof the ruck caused it.")

saveRDS(list(coefs = coef(fbest), oos = list(coef(f1), coef(f2))),
        file.path(OUT_DIR, "epv3_ruck_to_advantage.rds"))
say(""); say("done ", format(Sys.time())); close(con); cat("\nDone\n")
