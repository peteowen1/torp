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
# DATA SOURCE, precisely. `torp.local_data_dir = NA` bypasses the local
# torpdata/data shadow, which has no freshness check -- on 2026-09-06 the local
# pbp parquet was ~9 hours behind the release and did not contain this match at
# all. That covers load_pbp() and load_player_game_ratings().
#
# It does NOT cover load_player_stats() and load_results(), which never touch the
# shadow: they go through .load_with_cache()'s IN-MEMORY cache with a one-hour
# TTL. Running this as documented (a fresh Rscript process) starts with an empty
# cache, so it is moot. Sourcing it inside a warm R session is not, so
# load_player_stats() is asked to refresh explicitly. load_results() exposes no
# refresh argument; in a long-lived session its data can be up to an hour old.

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
ps  <- as.data.table(load_player_stats(2026, refresh = TRUE))
res <- load_results(TRUE)
pgr <- as.data.table(load_player_game_ratings(seasons = 2026))

# ANCHOR: refuse to publish a page about a match the data does not contain.
if (nrow(pbp[match_id == MID]) == 0) {
  cli::cli_abort(c(
    "Match {.val {MID}} is not in the play-by-play release.",
    "i" = "Publishing the page anyway would ship an empty or stale explainer."
  ))
}

# NP_PLAYER is a SUBSTRING match over every player in the season, so it must be
# checked for ambiguity rather than truncated with [1]. Measured on the 2026
# release: "Ryan" matches 10 players (it hits first names too -- Liam Ryan, Ryan
# Byrnes, Samson Ryan...), "Smith" 3, "Cripps" 2. Taking the first would publish
# a page about a real but arbitrary athlete, and every downstream check here
# tests internal consistency rather than identity, so all of them would pass.
cand <- unique(pgr[player_name %like% PLAYER, .(player_id, player_name)])
if (nrow(cand) == 0) {
  cli::cli_abort("No player matches {.val {PLAYER}} in the 2026 ratings.")
}
if (nrow(cand) > 1) {
  cli::cli_abort(c(
    "{.val {PLAYER}} matches {nrow(cand)} players -- refusing to guess.",
    "i" = "Candidates: {paste(cand$player_name, collapse = ', ')}",
    "i" = "Set {.envvar NP_PLAYER} to something unambiguous."
  ))
}
PID <- cand$player_id
pname <- cand$player_name

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

ev <- merge(led, m[, .(display_order, x, y, goal_x, venue_length, venue_width,
                       period, period_seconds)],
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

stopifnot(nrow(mine) > 0)
if (nrow(mine) < 5) {
  cli::cli_warn(c(
    "Only {nrow(mine)} event-role{?s} for {pname} in this match.",
    "i" = "A full game usually gives dozens. Check NP_PLAYER and NP_MATCH."
  ))
}

# pbp `x` is ALREADY in the acting team's attacking frame: production computes
# goal_x as venue_length/2 - x (clean_pbp.R). goal_x is a DISTANCE to goal (never
# negative), NOT a side indicator, and treating it as one flips half the pitch.
#
# HALF-LENGTH IS PER VENUE, not a constant. An earlier version hardcoded 77.5,
# which is right only for this 155m ground; venue_length runs 155 to 175 across
# 2026, so pointing NP_MATCH at almost any other game aborted here. It also has
# to reach the page, or the ground would be drawn the wrong size and every dot
# misplaced.
stopifnot(length(unique(mine$venue_length)) == 1)
HALF_LEN <- mine$venue_length[1] / 2
HALF_WID <- mine$venue_width[1] / 2
stopifnot(all(abs((HALF_LEN - mine$x) - mine$goal_x) < 1e-6))
say("venue ", mine$venue_length[1], "m x ", mine$venue_width[1],
    "m -- goal at x = ", HALF_LEN)

# FRAME. `hm` is the home-margin frame; build_net_points() reports its components
# in each player's OWN frame, flipping the sign for away players. Both must be in
# the same frame or the page shows every event backwards -- which is exactly what
# happened, and the reconciliation below caught it: for an away player the two
# sides came out as -1.0875 against +1.0875, an exact sign flip. It went unnoticed
# on the default match only because Papley is on the home team, where the flip is
# the identity. This is the single most valuable line in the script.
SGN <- if (identical(row$home_away, "Home")) 1 else -1
mine[, hm := hm * SGN]

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

# Every one of these lookups is used as if it returns exactly one row. Assert it
# rather than rely on being shielded by an earlier check -- a duplicate row in
# results or player_stats would otherwise reach the page as a silently wrong or
# vector-valued field.
mres  <- as.data.table(res)[match_id == MID]
pstat <- ps[match_id == MID & player_id == PID]
prat  <- pgr[match_id == MID & player_id == PID]
stopifnot(nrow(mres) == 1, nrow(pstat) == 1, nrow(prat) == 1)

payload <- list(
  match = list(id = MID, home = mres$home_team_name, away = mres$away_team_name,
               home_score = mres$home_score, away_score = mres$away_score,
               margin = mres$home_score - mres$away_score,
               round = mres$round_number, date = as.character(as.Date(mres$utc_start_time)),
               half_length = HALF_LEN, half_width = HALF_WID),
  player = list(id = PID, name = pname, team = row$team,
                position = pstat$position, tog = pstat$time_on_ground_percentage,
                published_epv = prat$epv),
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

# Write via a temp file and rename, so a failure part-way through leaves the
# previously published page intact rather than truncated. writeLines() opens the
# destination and truncates it immediately, so a disk-full or killed process
# would otherwise destroy a good page and leave a broken one in its place.
tmp <- tempfile(fileext = ".html")
writeLines(html, tmp, useBytes = TRUE)
stopifnot(file.exists(tmp), file.size(tmp) > 5000)
if (!dir.exists(dirname(OUT))) dir.create(dirname(OUT), recursive = TRUE)
if (!file.rename(tmp, OUT)) {
  file.copy(tmp, OUT, overwrite = TRUE) || cli::cli_abort("Could not write {.path {OUT}}.")
  unlink(tmp)
}

say("wrote ", OUT, " (", round(file.size(OUT) / 1024, 1), " KB)")
say(pname, " (", pstat$position, ", ", pstat$time_on_ground_percentage, "% TOG): ",
    nrow(mine), " event-roles, net points ", round(row$net_points, 2),
    "  (published epv ", round(prat$epv, 1), ")")
say("=== DONE ===")
