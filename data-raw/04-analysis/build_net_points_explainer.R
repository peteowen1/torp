#!/usr/bin/env Rscript
# Regenerate pkgdown/assets/net-points.html -- the interactive Net Points explainer
# ==============================================================================
# Follows ONE player through ONE game, act by act, on an AFL ground you can click.
# The page is deliberately SELF-CONTAINED: the data is injected into the HTML at
# build time rather than fetched, so it works from a file:// path and cannot
# break because a sibling asset moved.
#
# WHY THIS SCRIPT EXISTS. The page carries real numbers, so it goes stale the
# moment a share or a rule changes. Regenerating must be one command, or the
# published explainer quietly starts describing a metric that no longer exists.
# Run it after any change to build_net_points() or its constants:
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/build_net_points_explainer.R"'
#
# Data source: the RELEASE, not the local torpdata/data shadow. get_local_data_dir()
# has no freshness check, and on 2026-09-06 the local pbp parquet was ~9 hours
# behind the release and simply did not contain the match this page is about.

suppressMessages({
  library(data.table)
  library(jsonlite)
})
options(torp.local_data_dir = NA)
devtools::load_all(quiet = TRUE)
stopifnot(is.null(get_local_data_dir()))

MID    <- Sys.getenv("NP_MATCH", unset = "CD_M20260142602")
PLAYER <- Sys.getenv("NP_PLAYER", unset = "Papley")
HERE   <- "data-raw/04-analysis"
OUT    <- "pkgdown/assets/net-points.html"
say <- function(...) cat(..., "\n", sep = "")

say("=== NET POINTS EXPLAINER ===")
say("match ", MID, " | player ~", PLAYER)

pbp <- as.data.table(load_pbp(2026, rounds = TRUE))
ps  <- as.data.table(load_player_stats(2026))
res <- load_results(TRUE)
pgr <- as.data.table(load_player_game_ratings(seasons = 2026))

# ANCHOR: refuse to publish a page about a match the data does not contain.
if (nrow(pbp[match_id == MID]) == 0) {
  cli::cli_abort(c(
    "Match {.val {MID}} is not in the play-by-play release.",
    "i" = "Publishing the page anyway would ship an empty or stale explainer."
  ))
}
PID <- unique(pgr[player_name %like% PLAYER, player_id])[1]
stopifnot(!is.na(PID))
pname <- unique(pgr[player_id == PID, player_name])[1]

m   <- pbp[match_id == MID]
led <- .np_build_ledger(pbp)[match_id == MID]
setorder(led, display_order)
led[, kind := fcase(
  !(description %in% NP_DISPOSAL_DESCS), "possession",
  is.na(next_team),                      "terminal",
  next_team == team,                     "retained",
  default =                              "turnover")]

np  <- build_net_points(pbp, ps, res)
row <- np[match_id == MID & player_id == PID]
stopifnot(nrow(row) == 1)

ev <- merge(led, m[, .(display_order, x, y, goal_x, period, period_seconds)],
            by = "display_order")

# A single PBP row can carry TWO roles for the same player -- he disposes and is
# himself the next actor (a self-pass off the ground). A single fcase picks one
# and silently drops the other's credit, which broke the reconciliation below by
# exactly that event's value. Build the three role sets independently.
own_ev  <- ev[player_id == PID][, role := "own"]
recv_ev <- ev[kind == "retained" & next_player == PID][, role := "received"]
won_ev  <- ev[kind == "turnover" & next_player == PID][, role := "won"]
mine <- rbindlist(list(own_ev, recv_ev, won_ev), use.names = TRUE)
setorder(mine, display_order, role)

# pbp `x` is ALREADY in the acting team's attacking frame: goal_x == 77.5 - x on
# every row. goal_x is a DISTANCE to goal (never negative), NOT a side indicator,
# and treating it as one flips half the pitch.
stopifnot(all(abs((77.5 - mine$x) - mine$goal_x) < 1e-6))

mine[, contrib := fcase(
  role == "own" & kind == "retained", hm * (1 - NP_RECEIVER_SHARE),
  role == "own" & kind == "turnover", hm * (1 - NP_DEFENSIVE_SHARE),
  role == "own",                      hm,
  role == "received",                 hm * NP_RECEIVER_SHARE,
  role == "won",                      hm * NP_DEFENSIVE_SHARE * NP_BALL_WINNER_SHARE,
  default = 0)]

# The per-event numbers on the page must add up to the ledger components the
# function actually produced. Without this the page can drift from the metric
# while still looking authoritative.
expect <- row$np_direct + row$np_ceded + row$np_defensive_won
if (abs(sum(mine$contrib) - expect) > 1e-9) {
  cli::cli_abort(c(
    "Per-event contributions do not reconcile to the ledger.",
    "x" = "events sum to {round(sum(mine$contrib), 4)}, components give {round(expect, 4)}."
  ))
}
say("reconciled: ", nrow(mine), " event-roles sum to ", round(sum(mine$contrib), 4))

mres <- as.data.table(res)[match_id == MID]
payload <- list(
  match = list(id = MID, home = mres$home_team_name, away = mres$away_team_name,
               home_score = mres$home_score, away_score = mres$away_score,
               margin = mres$home_score - mres$away_score,
               round = mres$round_number, date = as.character(as.Date(mres$utc_start_time))),
  player = list(id = PID, name = pname, team = row$team,
                position = ps[match_id == MID & player_id == PID]$position,
                tog = ps[match_id == MID & player_id == PID]$time_on_ground_percentage,
                published_epv = pgr[match_id == MID & player_id == PID]$epv),
  params = list(receiver_share = NP_RECEIVER_SHARE,
                defensive_share = NP_DEFENSIVE_SHARE,
                ball_winner_share = NP_BALL_WINNER_SHARE,
                mirror_share = NP_MIRROR_SHARE),
  totals = list(np_direct = row$np_direct, np_defensive_won = row$np_defensive_won,
                np_defensive = row$np_defensive, np_ceded = row$np_ceded,
                np_residual = row$np_residual, net_points = row$net_points),
  events = mine[, .(order = display_order, period, secs = period_seconds,
                    desc = description, kind, role, x = round(x, 1), y = round(y, 1),
                    hm = round(hm, 3), contrib = round(contrib, 3))]
)

tmpl <- paste(readLines(file.path(HERE, "net-points-explainer-template.html"),
                        warn = FALSE), collapse = "\n")
if (!grepl("__DATA__", tmpl, fixed = TRUE)) {
  cli::cli_abort("Template has no {.code __DATA__} placeholder -- nothing would be injected.")
}
html <- sub("__DATA__", toJSON(payload, auto_unbox = TRUE, digits = 4), tmpl, fixed = TRUE)
stopifnot(!grepl("__DATA__", html, fixed = TRUE))
writeLines(html, OUT, useBytes = TRUE)

say("wrote ", OUT, " (", round(file.size(OUT) / 1024, 1), " KB)")
say(pname, ": net points ", round(row$net_points, 2),
    "  (published epv ", round(pgr[match_id == MID & player_id == PID]$epv, 1), ")")
say("=== DONE ===")
