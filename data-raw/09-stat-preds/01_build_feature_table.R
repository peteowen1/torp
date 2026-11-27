# 01_build_feature_table.R
# =========================
# T2 of the next-game stat model (torpverse docs/plans/STAT-PREDS-PLAN.md;
# evaluation charter: docs/plans/STAT-PREDS-CHARTER.md, locked v1).
#
# One row per player-match (players who played), 2021-2026, with the 12 core
# stats as targets and only inputs known before the match:
#   - stat ratings at the match's round (v19), asserted ref_date <= match date;
#   - recent form: last 1/3/5 games' rate per full game, last-3 time on ground,
#     days since his last game (all from earlier matches only);
#   - role: the position he is named in for this match (team sheet), sub/bench;
#   - opponent: what the opponent has conceded of each stat over its previous
#     10 matches, relative to the league average before the match; the team's
#     own rate the same way;
#   - team strength: his side's and the opponent's EPR (named lineup, as of
#     the round) and as-of xRAPM;
#   - context: home/away, venue.
# Coverage is asserted on every column: a column under its floor stops the
# build (CLAUDE.md: assert coverage, not presence).
#
# Usage (torp package root, PowerShell):
#   Rscript data-raw/09-stat-preds/01_build_feature_table.R
# Output: data-raw/cache-stat-preds/features.parquet (+ coverage.csv). Nothing
# is uploaded.

suppressMessages({
  library(data.table)
  devtools::load_all(".", quiet = TRUE)
})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")
out_dir <- file.path("data-raw", "cache-stat-preds")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

CORE <- c("disposals", "kicks", "handballs", "marks", "tackles", "goals", "behinds",
          "hitouts", "frees_for", "frees_against", "clangers", "turnovers")
SEASONS <- 2021:2026
OPP_WINDOW <- 10L

# Load ----
ps <- rbindlist(lapply(SEASONS, function(s) as.data.table(load_player_stats(s))), fill = TRUE)
ps <- ps[time_on_ground_percentage > 0]
ps[, `:=`(match_date = as.Date(substr(utc_start_time, 1, 10)),
          tog = time_on_ground_percentage / 100,
          team = data.table::fifelse(team_status == "home", home_team_name, away_team_name),
          opp = data.table::fifelse(team_status == "home", away_team_name, home_team_name),
          home = as.integer(team_status == "home"),
          round = as.integer(round_number))]
stopifnot(!anyDuplicated(ps, by = c("player_id", "match_id")))
say("player-games played: ", nrow(ps), " | seasons ", paste(range(ps$season), collapse = "-"),
    " | dates ", format(min(ps$match_date)), " to ", format(max(ps$match_date)))

sr <- rbindlist(lapply(SEASONS, function(s) as.data.table(load_player_stat_ratings(s))), fill = TRUE)
say("stat ratings: ", nrow(sr), " rows")
rating_cols <- grep("_rating$", names(sr), value = TRUE)
keep_sr <- c("player_id", "season", "round", "ref_date", "pos_group", "n_games", "wt_games",
             "n_80s", "wt_80s", rating_cols, paste0(CORE, "_n80s"), paste0(CORE, "_wt80s"))
sr <- sr[, intersect(keep_sr, names(sr)), with = FALSE]
setnames(sr, setdiff(names(sr), c("player_id", "season", "round")),
         paste0("sr_", setdiff(names(sr), c("player_id", "season", "round"))))

tm_full <- rbindlist(lapply(SEASONS, function(s) as.data.table(load_teams(s))), fill = TRUE)
tm <- unique(tm_full[, .(match_id, player_id, lineup_position)], by = c("match_id", "player_id"))

# Ratings as of the round ----
ft <- merge(ps[, c("player_id", "match_id", "season", "round", "match_date", "team", "opp", "home",
                   "venue_name", "tog", CORE), with = FALSE],
            sr, by = c("player_id", "season", "round"), all.x = TRUE)
n_no_rating <- ft[is.na(sr_ref_date), .N]
say("rows without a stat rating at their round: ", n_no_rating)
stopifnot(ft[!is.na(sr_ref_date), all(sr_ref_date <= match_date)])
say("ref_date to match: ", paste(range(as.numeric(ft$match_date - ft$sr_ref_date), na.rm = TRUE), collapse = " to "), " days")

# Role from the team sheet ----
ft <- merge(ft, tm, by = c("match_id", "player_id"), all.x = TRUE)
# Bench and sub have no on-field group; they get their own value so the column
# is complete (about 5 of 23 a team) rather than NA.
ft[, `:=`(named_pos_group = data.table::fcase(lineup_position %in% "SUB", "SUB",
                                              lineup_position %in% "INT", "BENCH",
                                              default = .map_position_group(lineup_position)),
          is_sub = as.integer(lineup_position %in% "SUB"),
          is_bench = as.integer(lineup_position %in% "INT"))]

# Recent form, from earlier matches only ----
setorder(ft, player_id, match_date, match_id)
roll_prev <- function(x, k) shift(frollsum(x, k, align = "right", na.rm = TRUE, partial = TRUE), 1L)
ft[, games_before := seq_len(.N) - 1L, by = player_id]
for (k in c(1L, 3L, 5L)) {
  ft[, (paste0("tog_last", k)) := roll_prev(tog, k), by = player_id]
  for (s in CORE) {
    ft[, (paste0(s, "_last", k)) := roll_prev(get(s), k) / get(paste0("tog_last", k)), by = player_id]
  }
}
ft[, tog_last3_mean := tog_last3 / pmin(games_before, 3L)]
ft[, days_since_last := as.numeric(match_date - shift(match_date)), by = player_id]

# Opponent conceded and team rate, previous OPP_WINDOW matches ----
team_match <- ps[, lapply(.SD, sum), by = .(match_id, match_date, team, opp), .SDcols = CORE]
setorder(team_match, match_date, match_id)
lg <- team_match[, lapply(.SD, mean), by = match_date, .SDcols = CORE]
setorder(lg, match_date)
# League average per team-match over everything before each date
lg_n <- team_match[, .N, by = match_date][order(match_date)]
lg_cum <- team_match[, lapply(.SD, sum), by = match_date, .SDcols = CORE][order(match_date)]
for (s in CORE) lg_cum[, (s) := shift(cumsum(get(s))) / shift(cumsum(lg_n$N))]
setnames(lg_cum, CORE, paste0("lg_", CORE))
conc <- team_match[, c("match_id", "match_date", "opp", CORE), with = FALSE]  # what `opp` conceded
setorder(conc, opp, match_date, match_id)
for (s in CORE) conc[, (paste0("conc_", s)) := shift(frollmean(get(s), OPP_WINDOW, align = "right", partial = TRUE)), by = opp]
own <- team_match[, c("match_id", "match_date", "team", CORE), with = FALSE]
setorder(own, team, match_date, match_id)
for (s in CORE) own[, (paste0("own_", s)) := shift(frollmean(get(s), OPP_WINDOW, align = "right", partial = TRUE)), by = team]
ft <- merge(ft, conc[, c("match_id", "opp", paste0("conc_", CORE)), with = FALSE], by = c("match_id", "opp"), all.x = TRUE)
ft <- merge(ft, own[, c("match_id", "team", paste0("own_", CORE)), with = FALSE], by = c("match_id", "team"), all.x = TRUE)
ft <- merge(ft, lg_cum, by = "match_date", all.x = TRUE)
for (s in CORE) {
  ft[, (paste0("opp_conc_ratio_", s)) := get(paste0("conc_", s)) / get(paste0("lg_", s))]
  ft[, (paste0("team_rate_ratio_", s)) := get(paste0("own_", s)) / get(paste0("lg_", s))]
}
ft[, c(paste0("conc_", CORE), paste0("own_", CORE), paste0("lg_", CORE)) := NULL]

# Team strength as of the round ----
# The match model's own lineup aggregation (.build_team_ratings_df(), the
# production helper, not a copy): EPR of the named side at its (season, round),
# weighted by each slot's average time on ground, plus the as-of xRAPM snapshot.
# Anchored 27 Nov 2026 against the honest backfill's home_epr: correlation
# 0.9998 in each of 2022-2025 (n = 828 matches).
# Team PSR is left out on purpose: the shipped PSR coefficients were fitted on
# 2021-2024 margins and calculate_psr() pools its scale over every season, 2026
# included, so it is not as of T (the honest backfill's per-season PSR differs
# by 12 points a team in 2022). Player stat ratings are already in as sr_*.
# The published predictions_<season> rows are not used either: 2021-2025 were
# made on 12 Apr 2026 by models trained on those seasons (hindsight), and the
# honest backfill covers only 2022 R4-2025, so a pre-match margin/total from
# one generator for every season is still to build (STAT-PREDS-PLAN.md, T2).
# Read the release copy explicitly: run from the torp root, the loader prefers a
# cwd-relative data-raw/03-ratings/ file, which can be an old local build.
xr_file <- file.path(tempdir(), "career_team_rapm_asof_AFLM.parquet")
piggyback::pb_download(basename(xr_file), repo = "peteowen1/torpdata", tag = TEAM_RAPM_ASOF_RELEASE_TAG,
                       dest = tempdir(), overwrite = TRUE)
xr <- load_team_rapm_asof(comp = "AFLM", path = xr_file)
stopifnot(!is.null(xr), nrow(xr) > 0)
trt <- as.data.table(.build_team_ratings_df(as.data.frame(tm_full), load_torp_ratings(), NULL, xr))
trt[, home := as.integer(team_type == "home")]
str_cols <- c("epr", "epr_recv", "epr_disp", "epr_spoil", "xrapm")
own_str <- trt[, c("match_id", "home", str_cols), with = FALSE]
setnames(own_str, str_cols, paste0("team_", str_cols))
opp_str <- copy(own_str)[, home := 1L - home]
setnames(opp_str, paste0("team_", str_cols), paste0("opp_", str_cols))
stopifnot(!anyDuplicated(own_str, by = c("match_id", "home")))
ft <- merge(ft, own_str, by = c("match_id", "home"), all.x = TRUE)
ft <- merge(ft, opp_str, by = c("match_id", "home"), all.x = TRUE)
ft[, epr_diff := team_epr - opp_epr]

# Coverage: assert, don't assume ----
# Floors: what can legitimately be missing. A debut has no form or days-since
# (games_before == 0); the first match of a team in 2021 has no conceded window;
# a player can lack a rating only if the ratings never listed him.
setorder(ft, match_date, match_id, player_id)
cov <- data.table(column = names(ft), coverage = vapply(ft, function(x) mean(!is.na(x)), numeric(1)))
cov[, floor := 0.999]
cov[grepl("_last[135]$|^tog_last|days_since_last", column), floor := 0.97]
cov[grepl("^opp_conc_ratio_|^team_rate_ratio_", column), floor := 0.99]
cov[grepl("^sr_", column), floor := 0.99]
cov[column %in% c("lineup_position", "named_pos_group", "venue_name"), floor := 0.99]
cov[column == "sr_pos_group", floor := 0.95]
# xRAPM snapshots start part-way through 2021 (57% of 2021 team-matches, 100%
# after), so it is 0 rather than NA there; checked by season below.
for (s in c("team_xrapm", "opp_xrapm")) {
  nz <- ft[, .(nonzero = mean(get(s) != 0, na.rm = TRUE)), by = season]
  say(s, " non-zero by season: ", paste(sprintf("%d %.2f", nz$season, nz$nonzero), collapse = ", "))
  stopifnot(nz[season >= 2022, all(nonzero > 0.99)])
}
fwrite(cov, file.path(out_dir, "coverage.csv"))
bad <- cov[coverage < floor]
say("columns: ", ncol(ft), " | rows: ", nrow(ft), " | under their coverage floor: ", nrow(bad))
if (nrow(bad)) { print(bad); stop("coverage below floor; nothing written") }
for (s in CORE) stopifnot(!anyNA(ft[[s]]))

arrow::write_parquet(ft, file.path(out_dir, "features.parquet"))
say("wrote ", file.path(out_dir, "features.parquet"), " (", nrow(ft), " rows x ", ncol(ft), " cols)")
print(ft[, .N, by = season][order(season)])
