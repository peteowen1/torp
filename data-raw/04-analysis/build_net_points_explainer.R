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
CREDIT <- Sys.getenv("NP_CREDIT", unset = "difficulty")
HERE   <- "data-raw/04-analysis"
OUT    <- Sys.getenv("NP_OUT", unset = "pkgdown/assets/net-points.html")
stopifnot(CREDIT %in% c("flat", "difficulty"))
say <- function(...) cat(..., "\n", sep = "")

say("=== NET POINTS EXPLAINER ===")
say("match ", MID, " | player ~", PLAYER)

# Two seasons, so the difficulty models that score 2026 are fitted on 2025
# (leak-safe) rather than on the season being explained.
SEASONS <- if (identical(CREDIT, "difficulty")) 2025:2026 else 2026
pbp <- as.data.table(load_pbp(SEASONS, rounds = TRUE))
ch  <- if (identical(CREDIT, "difficulty")) as.data.table(load_chains(SEASONS)) else NULL
ps  <- as.data.table(load_player_stats(SEASONS, refresh = TRUE))
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
led <- .np_build_ledger(pbp, ch)[match_id == MID]
setorder(led, display_order)

# The page shows what the LEDGER paid, read back from its own payment table,
# not re-derived here from the shares. An earlier version re-implemented the
# split in this script and could drift from the metric while still looking
# authoritative; now the only arithmetic here is a reconciliation.
np  <- build_net_points(pbp, ps, res, chains = ch, credit = CREDIT, return_payments = TRUE)
pay <- attr(np, "np_payments")
row <- np[match_id == MID & player_id == PID]
stopifnot(nrow(row) == 1)
flat_row <- if (identical(CREDIT, "difficulty")) {
  suppressMessages(build_net_points(pbp[match_id == MID], ps, res))[match_id == MID & player_id == PID]
} else NULL

# difficulty terms (kicker's frame) for the page's explanations
terms <- if (identical(CREDIT, "difficulty")) attr(np, "np_terms") else NULL
if (identical(CREDIT, "difficulty") && is.null(terms)) {
  terms <- .np_difficulty_terms(pbp, ch, leak_safe = TRUE)
}

ev <- merge(led, m[, .(display_order, x, y, goal_x, venue_length, venue_width,
                       period, period_seconds, exp_pts)],
            by = "display_order")
ev[, kind := fcase(
  !(description %in% NP_DISPOSAL_DESCS), "act",
  is.na(next_team),                      "terminal",
  next_team == team & !is.na(next_player), "retained",
  next_team == team,                     "terminal",
  default =                              "turnover")]
if (!is.null(terms)) {
  tt <- as.data.table(terms)[match_id == MID]
  ev[tt, on = "display_order", `:=`(p = i.p_hat, dec = i.decision, sur = i.surprise,
                                    contested = i.contested, cont_desc = i.cont_desc,
                                    csur = i.cont_surprise, gsur = i.ground_surprise,
                                    def_win = i.def_win, winner_pid = i.winner_pid)]
  ev[is.na(contested), contested := FALSE]
} else {
  ev[, `:=`(p = NA_real_, dec = NA_real_, sur = NA_real_, contested = FALSE,
            cont_desc = NA_character_, csur = NA_real_, gsur = NA_real_,
            def_win = NA, winner_pid = NA_character_)]
}
ev[, scored := is.finite(p) & description %in% NP_DISPOSAL_DESCS]

# His payments, one row per (act, role), from the ledger's own table
mypay <- pay[match_id == MID & player_id == PID]
mypay[, role := fcase(role == "actor", "own", role == "receiver", "received",
                      role == "ball_winner", "won", role == "contest_winner", "contest",
                      default = role)]
stopifnot(all(mypay$role %in% c("own", "received", "won", "contest")))
mine <- merge(mypay[, .(display_order, role, pay_hm = hm)], ev, by = "display_order")
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
# HALF-LENGTH IS PER VENUE, not a constant; venue_length runs 155 to 175 across
# 2026, and it has to reach the page or the ground is drawn the wrong size.
stopifnot(length(unique(mine$venue_length)) == 1)
HALF_LEN <- mine$venue_length[1] / 2
HALF_WID <- mine$venue_width[1] / 2
stopifnot(all(abs((HALF_LEN - mine$x) - mine$goal_x) < 1e-6))
say("venue ", mine$venue_length[1], "m x ", mine$venue_width[1],
    "m -- goal at x = ", HALF_LEN)

# FRAME. Payments and the ledger are in the home-margin frame; the page shows
# the player's OWN frame, so an away player's numbers flip sign. The terms
# (before / expectation / after / surprise) are shown in the KICKING side's
# frame, which is what the explanations describe, so they do not flip.
SGN <- if (identical(row$home_away, "Home")) 1 else -1
mine[, `:=`(hm = hm * SGN, contrib = pay_hm * SGN)]
mine[, `:=`(before = exp_pts, ev_pts = exp_pts + dec, after = exp_pts + dec + sur)]
mine[, same_team := !is.na(winner_pid) & winner_pid == PID & (resolve_team == team)]
mine[, share := fcase(role == "won", .np_ball_winner_share(next_desc),
                      role == "contest" & !same_team, .np_contest_winner_share(cont_desc),
                      default = NA_real_)]

# The per-event numbers on the page must add up to the ledger components the
# function actually produced. np_direct carries his own rows at face value and
# what he received; np_ceded is the transfer out; the two won columns are what
# came in. Their sum is exactly what the payment table says he was paid.
expect <- row$np_direct + row$np_ceded + row$np_defensive_won + row$np_contest_won
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
  params = list(credit = CREDIT,
                receiver_share = NP_RECEIVER_SHARE,
                defensive_share = NP_DEFENSIVE_SHARE,
                ball_winner_share = NP_BALL_WINNER_SHARE,
                mirror_share = NP_MIRROR_SHARE,
                blame_share = NP_BLAME_SHARE,
                offence_pool_share = NP_OFFENCE_POOL_SHARE),
  totals = c(list(np_direct = row$np_direct, np_defensive_won = row$np_defensive_won,
                  np_contest_won = row$np_contest_won, np_defensive = row$np_defensive,
                  np_team = row$np_team, np_ceded = row$np_ceded,
                  np_residual = row$np_residual, net_points = row$net_points),
              if (!is.null(flat_row)) list(flat_net_points = flat_row$net_points)),
  events = mine[, .(order = display_order, period, secs = period_seconds,
                    desc = description, kind, role, x = round(x, 1), y = round(y, 1),
                    hm = round(hm, 3), contrib = round(contrib, 3),
                    scored, p = round(p, 3), before = round(before, 3),
                    ev = round(ev_pts, 3), after = round(after, 3),
                    dec = round(dec, 3), sur = round(sur, 3),
                    contested, cont_desc, csur = round(csur, 3), gsur = round(gsur, 3),
                    def_win, next_desc, same_team, share)]
)

tmpl <- paste(readLines(file.path(HERE, "net-points-explainer-template.html"),
                        warn = FALSE), collapse = "\n")
if (!grepl("__DATA__", tmpl, fixed = TRUE)) {
  cli::cli_abort("Template has no {.code __DATA__} placeholder -- nothing would be injected.")
}
html <- sub("__DATA__", toJSON(payload, auto_unbox = TRUE, digits = 4, na = "null"), tmpl, fixed = TRUE)
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
