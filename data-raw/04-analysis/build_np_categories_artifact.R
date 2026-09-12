# "Where Net Points Come From": rebuild the category breakdown from live code
# =============================================================================
# The published artifact was built ad-hoc and had no committed generator, so
# nobody could regenerate it -- it still says rating vintage v8 while production
# is on v11. An artifact nobody can rebuild is a liability, and this session
# already showed what that costs: the ledger walkthrough's generator was reading
# a difficulty-terms cache written before torp#210, so every number on the page
# came from a superseded model and only Pete's arithmetic caught it.
#
# So this computes EVERYTHING live. No cached CSV, no hand-entered figures.
#
# WHAT THE CATEGORIES ARE. build_net_points() returns eight named columns per
# player-match that sum to net_points exactly. Three of them (the team pools and
# the residual) are used verbatim. The other five are split further by play type
# by reading the row-level payment ledger (return_payments = TRUE) and grouping
# on its real `role` plus the play-by-play `description`.
#
# HONEST ABOUT THE GAP, as the page already was: the payment ledger is read one
# step earlier than .np_team_margin()'s correction, which books its adjustment
# into np_team only. So the categories sum to Net within a small tolerance
# rather than exactly. That is measured and reported here rather than forced to
# zero.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/build_np_categories_artifact.R"'
suppressMessages({library(data.table); library(jsonlite); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON  <- 2026
MIN_GMS <- 8

pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))
ps  <- as.data.table(load_player_stats(SEASON, refresh = TRUE))
res <- as.data.table(load_results(SEASON))
tm  <- as.data.table(np_difficulty_terms_for_season(SEASON, pbp_data = pbp, chains = ch))

say("live constants: EPV_ENGINE ", EPV_ENGINE, " | RATING_VINTAGE ", RATING_VINTAGE,
    " | population ", EPV3_CONTEST_POPULATION,
    " | pool by ", NP_TEAM_MARGIN_POOL_BY,
    " | error blame ", NP_ERROR_BLAME_SHARE)

np  <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                        stoppages = "allocate", difficulty_terms = tm,
                        return_payments = TRUE)
pay <- as.data.table(attr(np, "np_payments"))
pay[, match_id := as.character(match_id)]
fin <- as.data.table(torp:::.np_team_margin(np, pbp, ps, res))
fin[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]

pg <- as.data.table(load_player_game_ratings(SEASON))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
meta <- unique(pg[, .(match_id, player_id, position_group, tog)])

# --- categories: role x play type --------------------------------------------
desc <- unique(pbp[, .(match_id, display_order, description)])
p <- merge(pay[!is.na(player_id) & doubled == FALSE], desc,
           by = c("match_id", "display_order"), all.x = TRUE)
p[, player_id := as.character(player_id)]

lab <- function(role, d) {
  data.table::fcase(
    role == "actor" & d == "Kick",                    "Kick",
    role == "actor" & d == "Handball",                "Handball",
    role == "actor" & d == "Ground Kick",             "Ground kick",
    role == "actor" & d == "Bounce",                  "Bounce",
    role == "actor" & d %like% "^Free For",           "Free kick (own)",
    role == "actor" & d == "Handball Received",       "Taking a handball",
    role == "actor" & d %like% "Gather",              "Gathering the ball",
    role == "actor" & d == "Uncontested Mark",        "Marking (uncontested)",
    role == "actor" & d %like% "Contested Mark|Pack Mark", "Marking (contested)",
    role == "actor",                                  "Other own act",
    role == "receiver" & d == "Kick",                 "Receiving a kick",
    role == "receiver",                               "Receiving a handball",
    role == "contest_winner",                         "Winning a contest (mark/spoil)",
    role == "ball_winner" & d %like% "Loose Ball|Hard Ball|Crumb", "Winning a loose/ground ball",
    role == "ball_winner",                            "Winning the ball off a disposal",
    role == "error_blame",                            "Charged for an error",
    # No "pressure_back" branch on purpose. Those rows are booked with
    # doubled = TRUE (epv_net_points.R:2230) because they sit on the blame side
    # of a turnover that is already counted once from the other side, and the
    # filter above keeps only doubled == FALSE. Pressure credit is real but it
    # is not separable at this read point -- it reaches the player inside
    # np_team, after .np_team_margin()'s correction.
    role == "stoppage_ruck" & d == "Centre Bounce",   "Ruck: centre bounce",
    role == "stoppage_ruck" & d == "Ball Up Call",    "Ruck: ball-up",
    role == "stoppage_ruck",                          "Ruck: boundary throw-in",
    role == "stoppage_player" & d == "Centre Bounce", "First possession: centre bounce",
    role == "stoppage_player" & d == "Ball Up Call",  "First possession: ball-up",
    role == "stoppage_player",                        "First possession: boundary throw-in",
    default = "Other own act")
}
p[, cat := lab(role, description)]
# to each player's OWN frame, so positive is good for everyone
ha <- unique(pbp[!is.na(team) & !is.na(home_away), .(match_id, team, home_away)])
p <- merge(p, ha, by = c("match_id", "team"), all.x = TRUE)
p[, own := hm * fifelse(home_away == "Home", 1, -1)]

cats <- p[, .(v = sum(own)), by = .(match_id, player_id, cat)]

POOLS <- c(np_defensive = "Team pressure pool (np_defensive)",
           np_team      = "Team offence pool (np_team)",
           np_residual  = "Unexplained margin (np_residual)")
pl <- melt(fin[, c("match_id", "player_id", names(POOLS)), with = FALSE],
           id.vars = c("match_id", "player_id"), variable.name = "k", value.name = "v")
pl[, cat := POOLS[as.character(k)]][, k := NULL]

all_cats <- rbind(cats, pl)
all_cats <- merge(all_cats, meta, by = c("match_id", "player_id"), all.x = TRUE)

g <- all_cats[, .(gms = uniqueN(match_id)), by = player_id][gms >= MIN_GMS]
all_cats <- all_cats[player_id %in% g$player_id]

per <- all_cats[, .(v = sum(v)), by = .(player_id, cat)]
gms <- all_cats[, .(gms = uniqueN(match_id), tog = round(mean(tog, na.rm = TRUE), 1)),
                by = player_id]
per <- merge(per, gms, by = "player_id")
per[, v := round(v / gms, 3)]

w <- dcast(per, player_id + gms + tog ~ cat, value.var = "v", fill = 0)
nm <- unique(pbp[!is.na(player_id), .(player_id = as.character(player_id), name = player_name)])

# One row per player, and it has to be FORCED rather than assumed.
# `position_group` in player_game_ratings is the PER-MATCH lineup listing, not
# the season-stable club listing (docs/reference/POSITIONS.md), so
# `unique(player_id, team, position_group)` is not one row per player: 53 of 669
# players in 2026 line up in more than one position across a season, and merging
# that straight onto `w` duplicated all 53 -- identical Net Points, two position
# labels, both published. Take the MODAL (team, position) by games played, and
# break ties on the labels themselves so the choice is deterministic rather than
# whatever order the table arrived in.
tmf <- pg[, .N, by = .(player_id, team, pos = position_group)]
data.table::setorder(tmf, player_id, -N, team, pos)
tmf <- tmf[, .SD[1], by = player_id][, .(player_id, team, pos)]
stopifnot(!anyDuplicated(tmf$player_id))

n_before <- nrow(w)
w <- merge(w, nm, by = "player_id"); w <- merge(w, tmf, by = "player_id", all.x = TRUE)
if (nrow(w) != n_before) {
  cli::cli_abort(c(
    "The name/position merges changed the row count: {n_before} -> {nrow(w)}.",
    "x" = "A lookup table has more than one row for some player, so those players are duplicated in the artifact.",
    "i" = "Both lookups must be one row per {.field player_id} before this merge."))
}
netv <- fin[, .(net = round(sum(net_points) / uniqueN(match_id), 3)), by = player_id]
w <- merge(w, netv, by = "player_id")
setorder(w, -net)

CATS <- setdiff(names(w), c("player_id", "gms", "tog", "name", "team", "pos", "net"))
w[, .chk := rowSums(.SD), .SDcols = CATS]
w[, .gap := abs(.chk - net)]
say("\n=== how close do the categories sum to Net? ===")
say("  mean |gap| ", round(mean(w$.gap), 4), "   worst ", round(max(w$.gap), 4),
    "   (Net itself runs about +/-8 a game)")
say("  The gap exists because the payment ledger is read one step before")
say("  .np_team_margin()'s correction, which books its adjustment into np_team")
say("  only. Reported, not forced to zero.")

# ...but it is GATED, not merely reported. This is the one check in the script
# that can catch "the numbers are wrong", and until 2026-09-12 it only printed --
# which is the same shape as the defect this whole file exists to prevent: a
# published artifact nobody could tell had gone bad. The bound is the measured
# baseline with about 3x headroom (2026, v11: mean 0.0329, worst 0.147), loose
# enough that ordinary vintage-to-vintage movement passes and tight enough that
# a duplicated player or a mis-labelled role cannot. Raise it only with a new
# measured baseline in this comment.
GAP_MEAN_MAX <- 0.10
GAP_WORST_MAX <- 0.50
if (mean(w$.gap) > GAP_MEAN_MAX || max(w$.gap) > GAP_WORST_MAX) {
  cli::cli_abort(c(
    "Categories do not reconcile with Net Points: mean |gap| {round(mean(w$.gap), 4)} (limit {GAP_MEAN_MAX}), worst {round(max(w$.gap), 4)} (limit {GAP_WORST_MAX}).",
    "x" = "Something is being double-counted, dropped, or labelled into the wrong bucket. The artifact is NOT written.",
    "i" = "Worst offenders: {paste(utils::head(w[order(-.gap)]$player_id, 5), collapse = ', ')}."))
}
w[, c(".chk", ".gap") := NULL]

out <- list(
  season = SEASON, n = nrow(w), min_games = MIN_GMS,
  vintage = RATING_VINTAGE, engine = EPV_ENGINE,
  population = EPV3_CONTEST_POPULATION, pool_by = NP_TEAM_MARGIN_POOL_BY,
  error_blame = NP_ERROR_BLAME_SHARE,
  generated_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  methods = CATS,
  # families group the categories for the page's legend. Built from CATS so a
  # new category (v10 added "Charged for an error") cannot silently go missing
  # from the legend while still appearing in the table.
  families = local({
    fam <- list(
      "Disposal (own act)" = c("Kick","Handball","Ground kick","Bounce","Free kick (own)"),
      "Taking possession (own act)" = c("Taking a handball","Gathering the ball",
        "Marking (uncontested)","Marking (contested)","Other own act"),
      "Receiving (own act)" = c("Receiving a kick","Receiving a handball"),
      "Contests, turnovers & errors" = c("Winning a contest (mark/spoil)",
        "Winning a loose/ground ball","Winning the ball off a disposal",
        "Charged for an error"),
      "Stoppages" = c("Ruck: centre bounce","Ruck: ball-up","Ruck: boundary throw-in",
        "First possession: centre bounce","First possession: ball-up",
        "First possession: boundary throw-in"),
      "Team pools & residual" = unname(POOLS))
    fam <- lapply(fam, function(m) intersect(m, CATS))
    missing <- setdiff(CATS, unlist(fam))
    if (length(missing)) {
      cli::cli_warn("{length(missing)} categor{?y/ies} not in any family: {missing}")
      fam[["Other"]] <- missing
    }
    lapply(names(fam), function(n) list(name = n, members = fam[[n]]))
  }),
  players = w
)
dir.create("data-raw/outputs", showWarnings = FALSE, recursive = TRUE)
write_json(out, "data-raw/outputs/np_categories_artifact.json",
           auto_unbox = TRUE, na = "null", digits = 6)
say("\nwrote data-raw/outputs/np_categories_artifact.json  (",
    nrow(w), " players, ", length(CATS), " categories)")
