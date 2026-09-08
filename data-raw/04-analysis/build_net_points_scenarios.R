#' Generate pkgdown/assets/net-points-scenarios.html from the real Net Points
#' ledger -- every number on the page comes from build_net_points() and its
#' np_payments / np_team_margin_parts attributes, never a hand-typed literal.
#'
#' Twenty passages in ten numbered categories (group number + a letter within
#' it, e.g. "1a"/"1b" -- Pete's ask, 2026-09-10, so a category keeps its
#' number regardless of how many examples live in it), each anchored
#' (id="example-N"). Examples 1-11 show a play-by-play trace (before / EV of
#' the choice / after / delta, all from the row's OWN delta_epv, never the
#' next row's exp_pts) and a "who was charged" table built with the same
#' team-sum scaling as local-tools/np_explorer/app.R's finish(). Example 12
#' uses torp:::.np_team_margin()'s per-player parts to show Carlton's full
#' roster summing to their own match margin.
#'
#' Run via PowerShell (arrow segfaults under Git Bash R):
#'   powershell.exe -Command 'Rscript "data-raw/04-analysis/build_net_points_scenarios.R"'
#'
#' Spec: docs/plans/NET-POINTS-SCENARIOS-FIX-PLAN.md SS1, SS3 item 1.

suppressMessages({
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
  library(data.table)
  library(arrow)
})

D        <- "C:/dev/torpverse/torp/local-tools/np_explorer/data"
OUT_HTML <- "C:/dev/torpverse/torp/pkgdown/assets/net-points-scenarios.html"

# ---------------------------------------------------------------------------
# 1. Load the same snapshot the explorer app and the ground-truth checks use,
#    join the released frame for names/scores/period, build the ledger and
#    the shipped-default payments table.
# ---------------------------------------------------------------------------
PBP <- as.data.table(read_parquet(file.path(D, "pbp_2026_snapshot.parquet")))
PS  <- as.data.table(read_parquet(file.path(D, "player_stats_2026_snapshot.parquet")))
RES <- as.data.table(read_parquet(file.path(D, "results_2026_snapshot.parquet")))
TM  <- fread(file.path(D, "np_difficulty_terms_2025_2026.csv"))
TM[, match_id := as.character(match_id)]
TM  <- TM[substr(match_id, 5, 8) == "2026"]
PBP[, match_id := as.character(match_id)]
RES[, match_id := as.character(match_id)]

FULL <- as.data.table(load_pbp(seasons = 2026))
FULL[, match_id := as.character(match_id)]
fcols <- setdiff(intersect(c("player_name", "points_shot", "period",
                              "home_team_name", "away_team_name"), names(FULL)),
                  names(PBP))
PBP <- merge(PBP, FULL[, c("match_id", "display_order", fcols), with = FALSE],
             by = c("match_id", "display_order"), all.x = TRUE)
setorder(PBP, match_id, display_order)
PBP[, last_in_period := display_order == max(display_order), by = .(match_id, period)]
rm(FULL)

led <- suppressMessages(torp:::.np_build_ledger(PBP, chains = NULL, stoppages = "allocate"))
l <- suppressMessages(torp:::.np_credit_terms(led, "difficulty",
       alpha = torp:::NP_RECEIVER_SHARE, phi = torp:::NP_DEFENSIVE_SHARE,
       beta = torp:::NP_BLAME_SHARE, omega = torp:::NP_OFFENCE_POOL_SHARE, terms = TM))
l[, match_id := as.character(match_id)]

np <- suppressMessages(build_net_points(PBP, PS, RES, chains = NULL, credit = "difficulty",
       stoppages = "allocate", difficulty_terms = TM, leak_safe = FALSE,
       return_payments = TRUE))

nm <- unique(PBP[!is.na(player_id), .(player_id = as.character(player_id), player_name)])

pay <- as.data.table(attr(np, "np_payments"))
pay[, match_id := as.character(match_id)]
pay <- merge(pay, nm, by = "player_id", all.x = TRUE)

ha <- unique(PBP[, .(match_id, team, home_away)])
pay <- merge(pay, ha, by = c("match_id", "team"), all.x = TRUE)

# --- team-sum scaling, copied from local-tools/np_explorer/app.R's finish()
# (own / v / side / side_sum / target / scaled) -- the production convention.
pay[, own := hm * fifelse(home_away == "Home", 1, -1)]
# the row's value is the ledger's own payments; `doubled` rows are the blame
# side's second allocation (NP_BLAME_POOL) and take part in the side sums only
if (!"doubled" %in% names(pay)) pay[, doubled := FALSE]
pay[, v := sum(hm[doubled == FALSE]), by = .(match_id, display_order)]
PAY <- pay[abs(v) > 1e-12]
PAY[, gain_home := v > 0]
PAY[, side := fifelse((home_away == "Home") == gain_home, "gain", "concede")]
PAY[, side_sum := sum(own), by = .(match_id, display_order, side)]
PAY[, target := fifelse(side == "gain", abs(v), -abs(v))]
PAY[, scaled := fifelse(abs(side_sum) > 0.05 * abs(v), own * target / side_sum, NA_real_)]

cat("Loaded", nrow(PBP), "pbp rows,", nrow(PAY), "scaled payment rows.\n")

# ---------------------------------------------------------------------------
# 2. Formatting helpers
# ---------------------------------------------------------------------------
esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

# plain numeric cell, no leading "+": used for EP before / EP after
fmt_plain <- function(x, dp = 3) {
  xr <- round(x, dp)
  xr[!is.na(xr) & xr == 0] <- 0  # kill IEEE negative zero (round(-1e-16,3) == -0)
  ifelse(is.na(x), "&mdash;",
         ifelse(xr < 0, paste0("&minus;", sprintf(paste0("%.", dp, "f"), -xr)),
                sprintf(paste0("%.", dp, "f"), xr)))
}

# signed numeric cell with a leading "+"/"-": used for deltas and charges
fmt_signed <- function(x, dp = 3) {
  xr <- round(x, dp)
  xr[!is.na(xr) & xr == 0] <- 0
  ifelse(is.na(x), "&mdash;",
         ifelse(xr < 0, paste0("&minus;", sprintf(paste0("%.", dp, "f"), -xr)),
                paste0("+", sprintf(paste0("%.", dp, "f"), xr))))
}
cls_sign <- function(x) ifelse(is.na(x), "", ifelse(x < 0, "neg", "pos"))

period_word <- function(p) {
  w <- c("first", "second", "third", "fourth", "extra-time")
  ifelse(p >= 1 & p <= 5, w[pmin(p, 5)], paste0("period ", p))
}

# ---------------------------------------------------------------------------
# 3. The passages, in nine numbered categories
# ---------------------------------------------------------------------------
# Numbered by CATEGORY (Pete's ask, 2026-09-10): the group number is the tag, a
# letter distinguishes passages within it, so "ruck stoppages = 1" and "shots =
# 2" stay fixed reference points regardless of how many examples live in each.
passages <- list(
  list(n = "1a", tag = "Ruck stoppages", title = "Centre bounce, clean advantage, then lost",
       match_id = "CD_M20260140001", lo = 1018, hi = 1026, hl = 1023,
       mech = "A stoppage is repriced against a neutral baseline for its type and location first -- a centre bounce reads EP = 0 on the raw play-by-play, which would otherwise hide the whole swing. The ruck contest, the first player to gather, and a team-level pool each take a share."),
  list(n = "1b", tag = "Ruck stoppages", title = "Centre bounce, a clean tap to a teammate",
       match_id = "CD_M20260140001", lo = 133, hi = 141, hl = 134,
       mech = "A different split applies when the ball comes out of the ruck contest as a genuine tap (\"Gather From Hitout\") rather than a scramble: the ruck who wins it keeps the larger share (50%) against the gatherer's 30%, because a clean hitout is mostly his doing. This is the one split Pete asked to leave untouched -- his own earlier, deliberate call to favour the tap."),
  list(n = "1c", tag = "Ruck stoppages", title = "Centre bounce, a contested scramble",
       match_id = "CD_M20260140001", lo = 929, hi = 937, hl = 934,
       mech = "Where the ball does NOT come out as a clean hitout -- this one resolves as a Hard Ball Get, a scramble -- the ruck and the first player to the ball now split roughly evenly (35/35), corrected 2026-09-10 from an earlier 20/50 split that read, on a real row, as far too small a share for the ruck relative to the player who gathered it. Compare Example 1b, the clean tap, which keeps the old 50/30 split unchanged."),
  list(n = "1d", tag = "Ruck stoppages", title = "Centre bounce, the ruck wins his own hard ball get",
       match_id = "CD_M20260140003", lo = 1854, hi = 1862, hl = 1856,
       mech = "A third way the ball can come out: the ruck himself is the one who wins the hard ball get, not a teammate. There is no separate \"receiver\" here -- he already is the disposer and the winner both -- so this pays him 80% directly, with no ruck-share/player-share split to make."),
  list(n = "2a", tag = "Shots", title = "A goal",
       match_id = "CD_M20260141001", lo = 147, hi = 157, hl = 154,
       mech = "A shot at goal is priced with the exact same decision/surprise split as any other kick -- there is no separate \"shot\" rule. The scoring kick's own delta is the gap between the position just before it and the fixed value of a goal (6.000); most of a goal's value is usually already banked by the time the shot is taken."),
  list(n = "2b", tag = "Shots", title = "A behind, then the kick-in",
       match_id = "CD_M20260141505", lo = 1971, hi = 1981, hl = 1976,
       mech = "A behind is scored as a turnover: the shot's after-state is 1 minus the opposition's expected kick-in return, not the scoreboard's +1. A shot can score and still be a net loss for the shooting side once the resulting kick-in is priced in."),
  list(n = "2c", tag = "Shots", title = "A shot that goes out on the full",
       match_id = "CD_M20260140201", lo = 362, hi = 372, hl = 366,
       mech = "A third, different shot outcome: the kick sails out of bounds on the full without registering a score at all. There is no goal or behind to price against, so the row is valued like any other kick that gives the ball away outright -- the after-state is the negative of wherever the free kick handed the opposition the ball, same as a normal turnover, not a scoring rule of any kind."),
  list(n = "3a", tag = "Contested marks", title = "The attacking side wins the contest",
       match_id = "CD_M20260140001", lo = 7, hi = 15, hl = 13,
       mech = "A kick resolves at a contest three ways (D8). Here a teammate wins it: the marking player is paid directly (win_hm) as a same-team contest winner, and the kicker keeps his decision plus a share of the contest surprise -- no value crosses to the opposition at all."),
  list(n = "3b", tag = "Contested marks", title = "The opposition marks it (an intercept mark)",
       match_id = "CD_M20260140001", lo = 40, hi = 48, hl = 44,
       mech = "The same contest, won the other way: the defender's mark is paid through cede_c_hm, routed to him as the named contest winner by NP_CONTEST_WINNER_SHARE (0.80 for a mark) -- almost all of the cession, because catching it cleanly is nearly all his doing."),
  list(n = "3c", tag = "Contested marks", title = "Spoiled -- nobody marks it",
       match_id = "CD_M20260140001", lo = 15, hi = 22, hl = 17,
       mech = "The third contest outcome: the ball is spoiled clear rather than marked by anyone. A spoil is a CHAINS event, not a play-by-play row in its own right, so it never appears as its own line in this trace -- only the kick that was contested and whatever the ball becomes afterward. NP_CONTEST_WINNER_SHARE for a spoil is 0.50, half what a mark pays, because a spoil is a shared outcome (the ball still has to be won again) rather than a clean take."),
  list(n = "4a", tag = "Uncontested receive", title = "An uncontested mark, caught cleanly",
       match_id = "CD_M20260140102", lo = 1217, hi = 1226, hl = 1222,
       mech = "The receiver's share of the surprise term when the disposal lands cleanly and uncontested -- value the receiving player earns just by being in space and taking the simple mark."),
  list(n = "4b", tag = "Uncontested receive", title = "A mark attempt, fumbled and then dropped",
       match_id = "CD_M20260140003", lo = 877, hi = 889, hl = 881,
       mech = "Correction, 2026-09-10: a dropped mark DOES exist -- it was checked against play-by-play only the first time, and chains records it as two chained events, \"Mark Fumbled\" then \"Mark Dropped\" (1,846 and 1,477 times in 2026). Like a spoil, neither is its own play-by-play row, so it never appears as its own line here -- only the kick whose reception failed, and how play resumes (a ball-up, in this case). The description doesn't record whether the attempt was contested or uncontested, so this stands for the general case rather than a clean uncontested-only instance."),
  list(n = "5a", tag = "Loose ball pickups", title = "A loose ball, recovered by the same team",
       match_id = "CD_M20260140001", lo = 74, hi = 80, hl = 76,
       mech = "A loose-ball or hard-ball get is not itself a disposal (only Kick and Handball get the difficulty-model split) -- it is priced as a plain act, paid in full to whoever wins it, unless the next act belongs to the other side."),
  list(n = "5b", tag = "Loose ball pickups", title = "A loose ball, lost to the opposition",
       match_id = "CD_M20260140002", lo = 1094, hi = 1099, hl = 1095,
       mech = "The same act, the other outcome: the very next touch belongs to the opposing team, so this becomes a turnover row under NP_TURNOVER_ON_ALL_ACTS -- the same blame-pool and ball-winner mechanics as a lost kick or handball, even though nobody disposed of anything."),
  list(n = "5c", tag = "Loose ball pickups", title = "A kick lands with no mark attempted at all",
       match_id = "CD_M20260140002", lo = 364, hi = 371, hl = 367,
       mech = "No stoppage, no mark -- the kick simply comes down in a contest and bounces to whoever gets to it first, here the opposition. This is the same turnover rule as Example 5b; the only thing distinct about it is what did NOT happen: nobody marked it, and it wasn't preceded by a whistle."),
  list(n = "6", tag = "Open-play free kicks", title = "A kick goes out on the full, in general play",
       match_id = "CD_M20260140306", lo = 423, hi = 431, hl = 427,
       mech = "The same rule as the shot in Example 2c, away from goal: kicking the ball out on the full anywhere on the ground is a free kick to the opposition, and the row is priced as an ordinary turnover to wherever that free kick handed them the ball -- nothing goal-related about the rule itself."),
  list(n = "7", tag = "Intercepts", title = "Reading a long kick (intercept)",
       match_id = "CD_M20260141904", lo = 1589, hi = 1599, hl = 1595,
       mech = "The biggest single-row swings in the ledger happen here -- but like a contested mark, an intercept can fire more than one role on the same player (ball_winner and contest_winner both), and only the net across them is his real credit for the row. The conceding side has no pool to spread onto, so the disposer alone wears the whole loss."),
  list(n = "8", tag = "Other turnovers", title = "A tackle, a free, a goal",
       match_id = "CD_M20260140303", lo = 1138, hi = 1146, hl = 1141,
       mech = "A non-disposal turnover (a tackle forcing a free kick) still fires the winner's credit against the loser's blame, the same shape as a lost kick or handball -- and the same asymmetry: the losing side has no pool, so the tackled player wears the full charge alone."),
  list(n = "9", tag = "Compound cases", title = "A kick into a contest, then the siren",
       match_id = "CD_M20260141903", lo = 604, hi = 612, hl = 609,
       mech = "Two things at once. Winning a contested mark (as in Example 3a) fires two separate terms on the same row: a contest-win credit for beating an opponent to it, and a receiver term still priced against how much of a coin-flip the contest was -- only their net is the real credit. And separately: the last row of a quarter has no next row, so its whole remaining position is charged to whoever last touched it -- the siren, not a mistake."),
  list(n = "11", tag = "Ground kick", title = "A kick along the ground bypasses the disposal split entirely",
       match_id = "CD_M20260140001", lo = 1162, hi = 1169, hl = 1165,
       mech = "A full audit of every description the ledger ever sees (27 of them) found exactly one genuinely different treatment, not just a different label on an already-shown rule: \"Ground Kick\" is not in NP_DISPOSAL_DESCS (only \"Kick\" and \"Handball\" are), so it never gets the decision/surprise split D6-D8 give a real kick -- it is priced as a plain act, paid in full to the kicker, or (as here) a turnover in full to whoever the ball bounces to. The other 26 descriptions all resolve to a rule already shown elsewhere on this page under a different name.")
)

# ---------------------------------------------------------------------------
# 4. Trace-table row builder
# ---------------------------------------------------------------------------
lost_to_team <- function(m, d) {
  w <- PAY[match_id == m & display_order == d & role == "ball_winner", team]
  if (length(w) == 0 || all(is.na(w))) {
    w <- PAY[match_id == m & display_order == d & role == "defence_pool", team]
  }
  w <- unique(w[!is.na(w)])
  if (length(w)) w[1] else NA_character_
}

act_label <- function(desc, kind, points_shot, last_in_period, m, d) {
  if (isTRUE(kind == "stoppage")) return(paste0(esc(desc), " (stoppage)"))
  parts <- character(0)
  if (!is.na(points_shot) && points_shot == 6) parts <- c(parts, "a goal")
  else if (!is.na(points_shot) && points_shot == 1) parts <- c(parts, "a behind")
  if (isTRUE(kind == "turnover")) {
    wt <- lost_to_team(m, d)
    if (!is.na(wt)) parts <- c(parts, paste0("lost to ", esc(wt)))
  }
  if (isTRUE(last_in_period)) parts <- c(parts, "quarter ends")
  if (length(parts)) paste0(esc(desc), " &mdash; ", paste(parts, collapse = "; "))
  else esc(desc)
}

build_trace <- function(pg) {
  rows <- PBP[match_id == pg$match_id & display_order %between% c(pg$lo, pg$hi)]
  setorder(rows, display_order)
  rows <- merge(rows, l[, .(match_id, display_order, kind, scored, dec_hm, contested)],
                by = c("match_id", "display_order"), all.x = TRUE)
  setorder(rows, display_order)
  stopifnot(nrow(rows) > 0)
  rows[, ev_choice := ifelse(isTRUE(scored) & !is.na(dec_hm),
                              exp_pts + dec_hm * fifelse(home_away == "Home", 1, -1),
                              NA_real_)]
  rows[, ep_after := exp_pts + delta_epv]
  rows
}

render_trace_html <- function(pg, rows) {
  trs <- vapply(seq_len(nrow(rows)), function(i) {
    r <- rows[i]
    is_hl <- r$display_order == pg$hl
    # the source data encodes "no player" (a contest row) as the literal
    # string "NA NA", not an actual NA -- both are treated as missing.
    no_player <- is.na(r$player_name) || identical(r$player_name, "NA NA")
    player <- if (no_player) "&mdash; (contest)" else esc(r$player_name)
    team   <- if (is.na(r$team)) "&mdash;" else esc(r$team)
    act <- act_label(r$description, r$kind, r$points_shot, r$last_in_period, r$match_id, r$display_order)
    delta_cls <- cls_sign(r$delta_epv)
    sprintf(
      '<tr%s><td>%d</td><td class="desc">%s</td><td>%s</td><td>%s</td><td class="num-col">%s</td><td class="num-col">%s</td><td class="num-col">%s</td><td class="num-col delta %s">%s</td></tr>',
      if (is_hl) ' class="turn"' else "",
      r$display_order, act, player, team,
      fmt_plain(r$exp_pts), fmt_plain(r$ev_choice), fmt_plain(r$ep_after),
      delta_cls, fmt_signed(r$delta_epv)
    )
  }, character(1))
  paste0(
    '<p class="ctx framenote">Expected points are shown from the acting player&rsquo;s own side on each row: ',
    'positive is good for the team listed on that row. At a turnover the &ldquo;after&rdquo; value is negative ',
    'because the other side now has the ball.</p>',
    '<table class="trace"><thead><tr><th>Row</th><th>Act</th><th>Player</th><th>Team</th>',
    '<th class="num-col">EP before</th><th class="num-col">EV of the choice</th>',
    '<th class="num-col">EP after</th><th class="num-col">&Delta;EP</th></tr></thead><tbody>',
    paste(trs, collapse = "\n"),
    '</tbody></table>'
  )
}

# ---------------------------------------------------------------------------
# 5. "Who was charged" table
# ---------------------------------------------------------------------------
role_label <- function(role, kind, next_desc) {
  fcase(
    role == "actor" & kind == "turnover", "actor_turnover",
    role == "actor", "actor_plain",
    role == "receiver", "receiver",
    role == "ball_winner", "ball_winner",
    role == "contest_winner", "contest_winner",
    role == "stoppage_player", "stoppage_player",
    role == "stoppage_ruck", "stoppage_ruck",
    default = "other"
  )
}

build_charged <- function(pg, rows) {
  sub <- PAY[match_id == pg$match_id & display_order %between% c(pg$lo, pg$hi)]
  stopifnot(nrow(sub) > 0)

  # assert every cited row is in the trace above it
  stopifnot(all(sub$display_order %in% rows$display_order))

  # conservation assertion: gaining side sums to +|v|, conceding to -|v|
  chk <- sub[!is.na(scaled), .(got = sum(scaled), want = target[1]),
             by = .(display_order, side)]
  gap <- max(abs(chk$got - chk$want), 0, na.rm = TRUE)
  if (!is.finite(gap) || gap > 1e-6) {
    bad <- chk[abs(got - want) > 1e-6]
    stop("Example ", pg$n, ": side-sum conservation failed on row(s) ",
         paste(bad$display_order, collapse = ", "), " (gap ", signif(gap, 4), ")")
  }

  sub <- merge(sub, l[, .(match_id, display_order, kind, next_desc)],
               by = c("match_id", "display_order"), all.x = TRUE)
  sub <- merge(sub, PBP[, .(match_id, display_order, description)],
               by = c("match_id", "display_order"), all.x = TRUE)
  pool_roles <- c("attack_pool", "defence_pool", "stoppage_pool", "blame_pool")

  # named player lines: net per (display_order, player), plus a footnote when
  # a player carries more than one role on the same row.
  named <- sub[!is.na(player_id)]
  named[, lbl := fcase(
    role == "actor" & kind == "turnover", paste0("Lost it (", esc(description), ")"),
    role == "actor", paste0(esc(description), " decision"),
    role == "receiver", "Received it",
    role == "ball_winner", paste0("Won the ball (", esc(fifelse(is.na(next_desc), "next act", next_desc)), ")"),
    role == "contest_winner", "Won the contest",
    role == "stoppage_player", "First possession",
    role == "stoppage_ruck", "Ruck",
    role == "pressure_back", "Passed it to him under pressure",
    default = esc(role)
  )]
  # short tag for the footnote line when a player carries >1 role on a row
  # (e.g. "contest winner +0.93, ball winner -0.73"), distinct from the full
  # `lbl` used for a single-role row.
  named[, short_role := fcase(
    role == "actor" & kind == "turnover", "lost it",
    role == "actor", "own act",
    role == "receiver", "received it",
    role == "ball_winner", "ball winner",
    role == "contest_winner", "contest winner",
    role == "stoppage_player", "first possession",
    role == "stoppage_ruck", "ruck",
    role == "pressure_back", "the pass that led to it",
    default = gsub("_", " ", role)
  )]

  player_rows <- named[, .(net = sum(scaled, na.rm = TRUE),
                            n_roles = .N,
                            footnote = paste(sprintf("%s %s", short_role, fmt_signed(scaled, 2)),
                                              collapse = ", ")),
                        by = .(display_order, team, player_name)]
  setorder(player_rows, display_order)

  # pool lines: one per (row, team), summing attack/defence/stoppage pool
  pool <- sub[role %in% pool_roles]
  pool_rows <- pool[, .(net = sum(scaled, na.rm = TRUE)), by = .(display_order, team)]
  setorder(pool_rows, display_order)

  list(player_rows = player_rows, pool_rows = pool_rows,
       max_gap = gap)
}

render_charged_html <- function(pg, charged) {
  pr <- charged$player_rows
  prows <- vapply(seq_len(nrow(pr)), function(i) {
    r <- pr[i]
    main <- sprintf(
      '<tr><td>Row %d</td><td>%s</td><td>%s</td><td class="num-col %s">%s</td></tr>',
      r$display_order, esc(r$player_name), esc(r$team), cls_sign(r$net), fmt_signed(r$net, 2)
    )
    if (r$n_roles > 1) {
      # r$footnote is built from lbl (already esc()'d internally) and
      # fmt_signed() (HTML entities) -- do NOT esc() it again here, or the
      # entities double-escape ("&amp;minus;").
      main <- paste0(main, sprintf(
        '<tr><td></td><td colspan="3"><span class="poolnote" style="margin:0">(%s, netted)</span></td></tr>',
        r$footnote))
    }
    main
  }, character(1))
  po <- charged$pool_rows
  porows <- vapply(seq_len(nrow(po)), function(i) {
    r <- po[i]
    sprintf(
      '<tr><td>Row %d</td><td>Team pool (%s) &mdash; spread across the roster at full time</td><td>%s</td><td class="num-col %s">%s</td></tr>',
      r$display_order, esc(r$team), esc(r$team), cls_sign(r$net), fmt_signed(r$net, 2)
    )
  }, character(1))
  paste0(
    '<h4>Who was charged</h4>',
    '<table><thead><tr><th>Row</th><th>Role / player</th><th>Team</th><th class="num-col">Points</th></tr></thead><tbody>',
    paste(c(prows, porows), collapse = "\n"),
    '</tbody></table>'
  )
}

# ---------------------------------------------------------------------------
# 6. Render examples 1-8
# ---------------------------------------------------------------------------
example_html <- character(length(passages))
summary_lines <- character(length(passages) + 1L)

for (i in seq_along(passages)) {
  pg <- passages[[i]]
  rows <- build_trace(pg)
  first <- rows[1]
  ctx <- sprintf("%s %d, %s %d, %s quarter.",
                 esc(first$home_team_name), first$home_points,
                 esc(first$away_team_name), first$away_points,
                 period_word(first$period))
  trace_html <- render_trace_html(pg, rows)
  charged <- build_charged(pg, rows)
  charged_html <- render_charged_html(pg, charged)

  example_html[i] <- paste0(
    '<div class="scenario">',
    '<span class="tag">', esc(pg$tag), '</span>',
    '<h2>Example ', pg$n, ' &mdash; ', esc(pg$title), '</h2>',
    '<p class="mech">', esc(pg$mech), '</p>',
    '<div class="example" id="example-', pg$n, '">',
    '<h3>Example ', pg$n, '</h3>',
    '<p class="ctx">', ctx, '</p>',
    '<h4>Play by play</h4>',
    trace_html,
    charged_html,
    '</div></div>'
  )

  n_charged <- nrow(charged$player_rows) + nrow(charged$pool_rows)
  summary_lines[i] <- sprintf(
    "Example %s (%s): %d trace rows, %d charged lines, max side-sum gap %.2e",
    pg$n, pg$title, nrow(rows), n_charged, charged$max_gap)
  cat(summary_lines[i], "\n")
}

# ---------------------------------------------------------------------------
# 7. Example 12 -- the team pool table (Carlton, CD_M20260140001)
# ---------------------------------------------------------------------------
np9 <- copy(np)
cv <- suppressMessages(torp:::.np_team_margin(np9, PBP, PS, RES))
parts <- as.data.table(attr(cv, "np_team_margin_parts"))
parts[, match_id := as.character(match_id)]

carl_match <- "CD_M20260140001"
carl_team  <- unique(PBP[match_id == carl_match & grepl("Carlton", team), team])
stopifnot(length(carl_team) == 1)

mg <- RES[match_id == carl_match, .(margin = home_score - away_score,
                                     home_score, away_score,
                                     home_team_name, away_team_name)]
carl_ha <- unique(PBP[match_id == carl_match & team == carl_team, home_away])
carl_margin <- mg$margin * ifelse(carl_ha == "Home", 1, -1)

pool9 <- parts[match_id == carl_match & team == carl_team]
pool9 <- merge(pool9, nm, by = "player_id", all.x = TRUE)
stopifnot(nrow(pool9) > 0)

# assert Named + Pool share + Reconciliation == Total, per row
gap9a <- max(abs((pool9$named + pool9$share + pool9$recon) - pool9$val))
if (!is.finite(gap9a) || gap9a > 1e-6) {
  stop("Example 12: named + share + recon != val (gap ", signif(gap9a, 4), ")")
}
# assert the roster sums to Carlton's own margin
gap9b <- abs(sum(pool9$val) - carl_margin)
if (!is.finite(gap9b) || gap9b > 1e-6) {
  stop("Example 12: roster total ", round(sum(pool9$val), 3),
       " != Carlton's margin ", carl_margin, " (gap ", signif(gap9b, 4), ")")
}
setorder(pool9, val)

pool_trs <- vapply(seq_len(nrow(pool9)), function(i) {
  r <- pool9[i]
  sprintf(
    '<tr><td>%s</td><td class="num-col %s">%s</td><td class="num-col %s">%s</td><td class="num-col %s">%s</td><td class="num-col %s">%s</td></tr>',
    esc(r$player_name),
    cls_sign(r$named), fmt_signed(r$named, 2),
    cls_sign(r$share), fmt_signed(r$share, 2),
    cls_sign(r$recon), fmt_signed(r$recon, 2),
    cls_sign(r$val), fmt_signed(r$val, 2)
  )
}, character(1))

ex9_mech <- sprintf(
  "Every named-role table above only ever books value that can be pinned to one player. What can't -- most of a row's conceding side, most of a stoppage pool -- accumulates as a team-level bucket across the whole match, then gets spread across the roster once, at full time, weighted by defensive involvement. %s %d d. %s %d: %s's %d listed players below sum to exactly %s, their own team's margin.",
  esc(mg$home_team_name), mg$home_score, esc(mg$away_team_name), mg$away_score,
  esc(carl_team), nrow(pool9), fmt_plain(carl_margin, 0)
)

example9_html <- paste0(
  '<div class="scenario">',
  '<span class="tag">Team pool</span>',
  '<h2>Example 12 &mdash; Team pool spread</h2>',
  '<p class="mech">', ex9_mech, '</p>',
  '<div class="example" id="example-9">',
  '<h3>Example 12</h3>',
  '<table><thead><tr><th>Player</th><th class="num-col">Named</th>',
  '<th class="num-col">Pool share</th><th class="num-col">Reconciliation</th>',
  '<th class="num-col">Total</th></tr></thead><tbody>',
  paste(pool_trs, collapse = "\n"),
  '</tbody></table>',
  '</div></div>'
)

summary_lines[length(passages) + 1L] <- sprintf(
  "Example 12 (Team pool, %s): %d players, sum %.2f vs margin %.2f, max reconciliation gap %.2e",
  carl_team, nrow(pool9), sum(pool9$val), carl_margin, gap9b)
cat(summary_lines[length(passages) + 1L], "\n")

# ---------------------------------------------------------------------------
# 8. Assemble the page -- keep the existing skeleton/style, replace content
# ---------------------------------------------------------------------------
head_html <- '<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Net Points &mdash; every credit and blame scenario</title>
<style>
  :root{
    --bg:#fbfaf8; --panel:#fff; --ink:#1c1c1e; --muted:#6b6b70;
    --line:#e3e1dd; --pos:#1a7f5a; --neg:#b3341f; --accent:#2b5c8a;
    --tag-bg:#f1efe9; --tag-ink:#5a5750;
  }
  @media (prefers-color-scheme: dark){
    :root:not([data-theme="light"]){
      --bg:#16171a; --panel:#1e2024; --ink:#e9e8e6; --muted:#9a9aa2;
      --line:#33363c; --pos:#4cc38a; --neg:#f0705a; --accent:#7fb0dd;
      --tag-bg:#26282c; --tag-ink:#b7b4ac;
    }
  }
  :root[data-theme="dark"]{
    --bg:#16171a; --panel:#1e2024; --ink:#e9e8e6; --muted:#9a9aa2;
    --line:#33363c; --pos:#4cc38a; --neg:#f0705a; --accent:#7fb0dd;
    --tag-bg:#26282c; --tag-ink:#b7b4ac;
  }
  *{box-sizing:border-box}
  body{margin:0;background:var(--bg);color:var(--ink);
       font:15px/1.6 -apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif}
  .wrap{max-width:920px;margin:0 auto;padding:32px 20px 90px}
  h1{font-size:1.85rem;line-height:1.2;margin:0 0 6px;letter-spacing:-.02em}
  h2{font-size:1.2rem;margin:0 0 4px;letter-spacing:-.01em}
  .sub{color:var(--muted);margin:0 0 26px;font-size:1.02rem;max-width:62ch}
  .rule{background:var(--panel);border:1px solid var(--line);border-radius:10px;padding:18px 20px;margin-bottom:34px}
  .rule p{margin:0 0 10px} .rule p:last-child{margin-bottom:0}
  .scenario{border-top:1px solid var(--line);padding:30px 0}
  .scenario:first-of-type{border-top:none}
  .num{display:inline-block;color:var(--muted);font-variant-numeric:tabular-nums;margin-right:8px}
  .mech{color:var(--muted);max-width:66ch;margin:6px 0 16px}
  .tag{display:inline-block;background:var(--tag-bg);color:var(--tag-ink);border-radius:5px;
       font-size:.72rem;font-weight:600;letter-spacing:.02em;text-transform:uppercase;
       padding:2px 7px;margin:0 6px 6px 0}
  table{border-collapse:collapse;width:100%;font-size:.9rem;margin-top:4px}
  th,td{text-align:left;padding:6px 10px;border-bottom:1px solid var(--line)}
  th{color:var(--muted);font-weight:600;font-size:.76rem;text-transform:uppercase;letter-spacing:.02em}
  td.num-col,th.num-col{text-align:right;font-variant-numeric:tabular-nums;font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace}
  .pos{color:var(--pos)} .neg{color:var(--neg)}
  .take{margin-top:12px;padding:10px 14px;border-left:3px solid var(--accent);
        background:var(--panel);border-radius:0 6px 6px 0;font-size:.92rem}
  .take b{color:var(--accent)}
  .terms{display:grid;grid-template-columns:auto 1fr;gap:4px 14px;margin:14px 0 0;font-size:.88rem}
  .terms dt{font-weight:700;white-space:nowrap}
  .terms dd{margin:0;color:var(--muted)}
  .example{margin:22px 0 26px;padding:16px 18px;background:var(--panel);border:1px solid var(--line);
           border-radius:10px}
  .example h3{margin:0 0 4px;font-size:1rem}
  .example .ctx{color:var(--muted);font-size:.85rem;margin:0 0 12px}
  .example h4{margin:16px 0 4px;font-size:.76rem;text-transform:uppercase;letter-spacing:.03em;
              color:var(--muted)}
  .framenote{font-size:.85rem;color:var(--muted);margin:6px 0 10px}
  .trace td.desc{white-space:nowrap} .trace td.delta{font-weight:600}
  .trace tr.turn td{background:color-mix(in srgb, var(--neg) 8%, transparent)}
  .poolnote{font-size:.85rem;color:var(--muted);margin:10px 0 0}
  footer{margin-top:40px;padding-top:20px;border-top:1px solid var(--line);color:var(--muted);font-size:.88rem}
  footer a{color:var(--accent)}
  a{color:var(--accent)}
</style>
</head>
<body>
<div class="wrap">
  <h1>Net Points &mdash; every credit and blame scenario</h1>
  <p class="sub">Every point of a match margin is allocated to the players on the field. This
    page walks through nine real passages from the 2026 season -- every row-by-row value below
    is pulled straight from build_net_points() and its payment ledger, not typed by hand.
    Companion to the player-level walkthroughs for
    <a href="net-points.html">a forward (Papley)</a> and <a href="net-points-defender.html">a
    defender (Andrews)</a>.</p>

  <div class="rule">
    <p><b>The one rule everything below serves:</b> a play\'s value is the gap between the
      expected points before it and after it &mdash; delta_epv = (EV &minus; before) + (after &minus; EV).
      The first half is the <b>decision</b>: did this option beat what was expected, and it
      always goes to the player who chose it. The second half is the <b>surprise</b>: how far
      the actual outcome (kept, dropped, intercepted) landed from what the decision alone
      predicted, and it splits between the players involved.</p>
    <p>Every row is charged twice: once as credit to the side that gained value, once as blame
      to the side that conceded it. That\'s why each team\'s own players sum to that team\'s own
      margin, not just the gap between the two sides. Values below are in <b>points</b>, the
      same units as the final score.</p>
    <dl class="terms">
      <dt>Expected Points (EP)</dt>
      <dd>The per-play state model &mdash; "if the match stopped right now, how many points is
        this position worth?" This is the exp_pts column, the "before" and "after" in
        every row-by-row trace below.</dd>
      <dt>Net Points</dt>
      <dd>What this whole page is about &mdash; the play-by-play EP swings, allocated to named
        players and re-summed so each team totals its own margin. This is the ledger.</dd>
      <dt>EV of the choice</dt>
      <dd>The invisible intermediate value between "before" and "after": what the model expected
        this specific decision to be worth, before the outcome (kept, dropped, intercepted) was
        known. Blank on rows the difficulty model does not score (stoppages, non-disposal acts).</dd>
    </dl>
  </div>

'

body_html <- paste(example_html, collapse = "\n\n")

footer_html <- '
  <footer>
    Every number above is a real 2026 payment row, generated straight from
    build_net_points() by data-raw/04-analysis/build_net_points_scenarios.R -- nothing on this
    page is hand-typed. Full technical reference with every constant and its live/dead status:
    docs/reference/EPV-CREDIT-BLAME-CHOICES.md in the torpverse repo.
  </footer>
</div>
</body>
</html>
'

full_html <- paste0(head_html, body_html, "\n\n", example9_html, "\n", footer_html)
writeLines(full_html, OUT_HTML, useBytes = TRUE)

cat("\nWrote", OUT_HTML, "\n")
cat("\n--- summary ---\n")
cat(paste(summary_lines, collapse = "\n"), "\n")
