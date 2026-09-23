# "Where Net Points Come From": rebuild the category breakdown from live code
# =============================================================================
# The published artifact was built ad-hoc and had no committed generator, so
# nobody could regenerate it -- it still says rating vintage v8 while production
# is on v11. An artifact nobody can rebuild is a liability, and this session
# already showed what that costs: the ledger walkthrough's generator was reading
# a difficulty-terms cache written before torp#210, so every number on the page
# came from a superseded model and only Pete's arithmetic caught it.
#
# So this computes everything from live code. The one cache, np_season_build.R,
# is keyed on the code and constants that decide the ledger. No hand-entered figures.
#
# WHAT THE CATEGORIES ARE. .np_team_margin() -- the step production runs --
# gives every player-match three parts that sum to net_points exactly: `named`
# (payments to him by name), `share` (his slice of the team pool) and `recon`
# (his slice of the gap to the team's real margin). `named` is split by play
# type from attr(, "np_team_margin_payments"), the same payment rows at their
# FINAL value, grouped on `role` plus the play-by-play `description`.
#
# So the categories add up to Net EXACTLY (gated at 1e-9 below). Until
# 2026-09-23 this read np_payments, the values from BEFORE the rescale, and
# missed Net by up to 0.23 a game; that gap was also what inflated the old
# "Team offence pool (np_team)" column to about -5 a game on every top player.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/build_np_categories_artifact.R"'
suppressMessages({library(data.table); library(jsonlite); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON  <- 2026
MIN_GMS <- 8

# shared with np_first_goal_walkthrough.R and cached; see np_season_build.R
source("data-raw/04-analysis/np_season_build.R")
b <- np_season_build(SEASON)
pbp <- b$pbp; ch <- b$ch; ps <- b$ps; res <- b$res; tm <- b$tm

say("live constants: EPV_ENGINE ", EPV_ENGINE, " | RATING_VINTAGE ", RATING_VINTAGE,
    " | population ", EPV3_CONTEST_POPULATION,
    " | pool by ", NP_TEAM_MARGIN_POOL_BY,
    " | error blame ", NP_ERROR_BLAME_SHARE)

np  <- b$np
fin <- torp:::.np_team_margin(np, pbp, ps, res)
pay   <- as.data.table(attr(fin, "np_team_margin_payments"))
parts <- as.data.table(attr(fin, "np_team_margin_parts"))
fin <- as.data.table(fin)
for (d in list(pay, parts, fin)) d[, `:=`(match_id = as.character(match_id),
                                           player_id = as.character(player_id))]

pg <- as.data.table(load_player_game_ratings(SEASON))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
meta <- unique(pg[, .(match_id, player_id, position_group, tog)])

# --- categories: role x play type --------------------------------------------
desc <- unique(pbp[, .(match_id, display_order, description)])
p <- merge(pay, desc,
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
    # The blame side of a turnover sent back to the teammate who set it up
    # (doubled = TRUE rows). Separable now that payments are read at their
    # final value.
    role == "pressure_back",                          "Blame passed back (turnover)",
    role == "stoppage_ruck" & d == "Centre Bounce",   "Ruck: centre bounce",
    role == "stoppage_ruck" & d == "Ball Up Call",    "Ruck: ball-up",
    role == "stoppage_ruck",                          "Ruck: boundary throw-in",
    role == "stoppage_player" & d == "Centre Bounce", "First possession: centre bounce",
    role == "stoppage_player" & d == "Ball Up Call",  "First possession: ball-up",
    role == "stoppage_player",                        "First possession: boundary throw-in",
    default = "Other own act")
}
p[, cat := lab(role, description)]
# `paid` is already in the player's own frame (positive = good for him).
unk <- p[is.na(description), .N]
if (unk > 0) say("  ", unk, " payment rows have no play-by-play description (labelled 'Other own act')")

cats <- p[, .(v = sum(paid)), by = .(match_id, player_id, cat)]

POOLS <- c(share = "Team pool share",
           recon = "Anchor to the real margin")
pl <- melt(parts[, c("match_id", "player_id", names(POOLS)), with = FALSE],
           id.vars = c("match_id", "player_id"), variable.name = "k", value.name = "v")
pl[, cat := POOLS[as.character(k)]][, k := NULL]

all_cats <- rbind(cats, pl)
all_cats <- merge(all_cats, meta, by = c("match_id", "player_id"), all.x = TRUE)

g <- all_cats[, .(gms = uniqueN(match_id)), by = player_id][gms >= MIN_GMS]
all_cats <- all_cats[player_id %in% g$player_id]

per <- all_cats[, .(v = sum(v)), by = .(player_id, cat)]
gms <- all_cats[, .(gms = uniqueN(match_id), tog = round(mean(tog, na.rm = TRUE), 3)),
                by = player_id]
per <- merge(per, gms, by = "player_id")
per[, v := v / gms]

# THE GATE, on unrounded values: categories must rebuild Net exactly.
netv <- fin[player_id %in% g$player_id,
            .(net = sum(net_points) / uniqueN(match_id)), by = player_id]
.chk <- merge(per[, .(tot = sum(v)), by = player_id], netv, by = "player_id")
.gap <- max(abs(.chk$tot - .chk$net))
say("\n=== do the categories sum to Net? ===  worst |gap| ", signif(.gap, 3),
    " over ", nrow(.chk), " players (unrounded)")
if (!is.finite(.gap) || .gap > 1e-9) {
  cli::cli_abort(c(
    "Categories do not add up to Net Points (worst gap {signif(.gap, 3)}).",
    "x" = "Something is double-counted, dropped, or labelled into the wrong bucket. The artifact is NOT written.",
    "i" = "Worst offenders: {paste(utils::head(.chk[order(-abs(tot - net))]$player_id, 5), collapse = ', ')}."))
}
per[, v := round(v, 3)]

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
w <- merge(w, netv[, .(player_id, net = round(net, 3))], by = "player_id")
setorder(w, -net)
CATS <- setdiff(names(w), c("player_id", "gms", "tog", "name", "team", "pos", "net"))


out <- list(
  sport = "afl",
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
        "Charged for an error", "Blame passed back (turnover)"),
      "Stoppages" = c("Ruck: centre bounce","Ruck: ball-up","Ruck: boundary throw-in",
        "First possession: centre bounce","First possession: ball-up",
        "First possession: boundary throw-in"),
      "Team pool & anchor" = unname(POOLS))
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
