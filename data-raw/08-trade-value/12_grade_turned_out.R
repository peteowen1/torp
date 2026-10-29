# Grade the 2021-2025 trade periods AS IT TURNED OUT: each asset is worth
# what the receiving club actually got from it in the eight seasons after the
# trade (Pete, 2026-10-29: "everything they got in that 8 year window").
# 10_grade_past_trades.R is the same trades graded at the time.
#
#   - Player: the seasons he played for the club, games x TOG x (end-of-season
#     TORP - replacement), up to 2026. Then one of:
#       traded on in trade period t: his value at the time of that trade
#         (03_projected_value_<t>.csv) for the window seasons still to come,
#         standing in for what the club got for him;
#       still at the club at the end of 2026: his 2026 projection for the
#         window seasons still to come;
#       otherwise (free agency, delisted, retired): nothing more.
#   - Pick: traced through the draft's "pick came via" notes to the selection
#     it became. Used by the club: the draftee, valued as a player from his
#     first season. Traded on: the pick curve at the number it was finally
#     used at. A 2024 or 2025 draftee who has not played yet: the pick curve at
#     that number (two seasons is too early to call him a miss). Picks into the 2026 draft or later, and picks the notes do not
#     trace (most of them spent matching an academy or father-son bid), keep
#     their at-the-time value.
#   - Every season is discounted 10% a year from the trade, as at the time.
#
# One value per asset, so what one club gains the other loses and each
# trade's nets sum to zero. The stay curves are not used: the realised seasons
# are already the ones at the club.
#
# Output per year: 12_assets_<year>.csv, 12_grades_<year>.csv (gitignored).

suppressMessages({ library(data.table) })
source("data-raw/08-trade-value/trade_tables.R")
source("data-raw/08-trade-value/draft_names.R")
suppressMessages(devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE))
raw_dir <- "C:/dev/torpverse/torpdata/data/raw/trades_past"
out_dir <- "C:/dev/torpverse/torpdata/data/raw/trade_value"
disc <- 0.9
years <- 2021:2025
last_season <- 2026L
repl <- as.numeric(readLines(file.path(out_dir, "03_replacement_level.txt")))
curve <- fread(file.path(out_dir, "04_pick_curve.csv"))
pick_value <- function(n) curve$v8_direct[match(pmin(pmax(round(n), 1), max(curve$pick)), curve$pick)]
abbr <- function(x) torp_team_abbr(x)

# ---- 1. Seasons played, by club ----
# The published releases, not the local mirror (it is not refreshed by a republish).
td <- tempfile(); dir.create(td)
system2("gh", c("release", "download", "ratings-data", "--repo", "peteowen1/torpdata",
                "--pattern", "torp_ratings.parquet", "--dir", td))
r <- as.data.table(arrow::read_parquet(file.path(td, "torp_ratings.parquet"),
                                       col_select = c("player_id", "player_name", "team", "season", "round", "torp")))
setorder(r, player_id, season, round)
end <- r[, .SD[.N], by = .(player_id, season)]
neale <- end[player_name == "Lachie Neale" & season == 2026, torp]
stopifnot(length(neale) == 1, abs(neale - -0.23) < 0.005)
pg <- rbindlist(lapply(2021:last_season, function(s) {
  system2("gh", c("release", "download", "player_game_ratings-data", "--repo", "peteowen1/torpdata",
                  "--pattern", sprintf("player_game_ratings_%d.parquet", s), "--dir", td))
  d <- as.data.table(arrow::read_parquet(file.path(td, sprintf("player_game_ratings_%d.parquet", s)),
                                         col_select = c("player_id", "season", "team", "tog")))
  d[, .(games = .N, tog = mean(tog, na.rm = TRUE)), by = .(player_id, season, team)]
}))
pg[, club := abbr(team)]
seas <- merge(pg, end[, .(player_id, season, torp)], by = c("player_id", "season"), all.x = TRUE)
stopifnot(!anyNA(seas$club), seas[, mean(!is.na(torp))] > 0.99, seas[, mean(!is.na(tog))] > 0.99)
seas[, ev := fifelse(is.na(torp) | is.na(tog), 0, games * tog * (torp - repl))]
setkey(seas, player_id, club, season)
club_2026 <- end[season == last_season, .(player_id, club = abbr(team))]
cat("player-club-seasons:", nrow(seas), "| players rated at the end of 2026:", nrow(club_2026), "\n")

# Projections as at each trade period, long: player, season, expected value.
proj <- rbindlist(lapply(2021:last_season, function(t) {
  p <- fread(file.path(out_dir, sprintf("03_projected_value_%d.csv", t)))
  p <- melt(p[, c("player_id", grep("^ev_", names(p), value = TRUE)), with = FALSE],
            id.vars = "player_id", value.name = "ev")
  p[, .(as_of = t, player_id, season = as.integer(sub("ev_", "", variable)), ev)]
}))
setkey(proj, as_of, player_id, season)

# ---- 2. Every player moved by trade, 2021-2026 ----
# fread reads an empty player_id as "", which is.na() misses: make it NA so the
# name match below fills it (Darcy Fort, Jacob Konstanty, Patrick Retschko had
# no projection in their trade year and were credited 0 seasons).
blank_na <- function(a) a[player_id == "", player_id := NA_character_][]
assets <- lapply(setNames(years, years), function(y) blank_na(fread(file.path(out_dir, sprintf("10_assets_%d.csv", y)))))
a26 <- blank_na(fread(file.path(out_dir, "05_trade_assets.csv")))[, draft_year := last_season]
all_names <- unique(end[, .(player_id, player_name)], by = "player_id")
moves <- rbindlist(c(lapply(assets, function(a) a[type == "player", .(year = draft_year, player_id, item, from_club)]),
                     list(a26[type == "player", .(year = draft_year, player_id, item, from_club)])))
# Fill the players 10 could not match against that season's projection.
miss <- moves[is.na(player_id)]
if (nrow(miss)) {
  m <- match_draft_names(miss[, .(row = .I, player = sub(" Jr\\.?$", "", item))], all_names)
  moves[is.na(player_id), player_id := m$player_id[order(m$row)]]
}
moves[, from := abbr(from_club)]
cat("players moved by trade 2021-2026:", nrow(moves), "| unmatched to a player:", moves[is.na(player_id), .N], "\n")

# ---- 3. What a club got from a player ----
# pid joined club cl in time for season s_from, via a trade in period y.
credit_player <- function(pid, cl, s_from, y) {
  w_end <- y + 8L
  t_out <- moves[player_id == pid & from == cl & year >= s_from, suppressWarnings(min(year))]
  last <- min(t_out, last_season, w_end)
  rs <- seas[.(pid, cl)][!is.na(season) & season >= s_from & season <= last]
  realised <- rs[, sum(ev * disc^(season - y - 1L))]
  if (is.finite(t_out) && t_out < w_end) {
    rest <- proj[.(as.integer(t_out), pid)][season > t_out & season <= w_end]
    list(realised = realised, after = rest[, sum(ev * disc^(season - y - 1L), na.rm = TRUE)],
         how = sprintf("traded on %d", t_out), seasons = nrow(rs))
  } else if (!is.finite(t_out) && pid %in% club_2026[club == cl, player_id] && w_end > last_season) {
    rest <- proj[.(last_season, pid)][season > last_season & season <= w_end]
    list(realised = realised, after = rest[, sum(ev * disc^(season - y - 1L), na.rm = TRUE)],
         how = "still there", seasons = nrow(rs))
  } else {
    list(realised = realised, after = 0, how = if (is.finite(t_out)) sprintf("traded on %d", t_out) else "left",
         seasons = nrow(rs))
  }
}

# ---- 4. Grade each year ----
drafts <- lapply(setNames(years, years), function(y) parse_wiki_draft(file.path(raw_dir, sprintf("wiki_%d_draft.html", y)), y))
grade_year <- function(y) {
  a <- copy(assets[[as.character(y)]])
  a[, `:=`(to = abbr(to_club), from = abbr(from_club))]
  a[, `:=`(v_turned = NA_real_, realised = NA_real_, after = NA_real_, how = NA_character_,
           draftee = NA_character_, final_pick = NA_integer_)]
  # Players.
  a[type == "player" & is.na(player_id), player_id := moves[year == y & item %in% .BY$item, player_id[1]], by = item]
  for (i in a[type == "player" & !is.na(player_id), which = TRUE]) {
    cp <- credit_player(a$player_id[i], a$to[i], y + 1L, y)
    set(a, i, c("realised", "after", "how"), list(cp$realised, cp$after, cp$how))
  }
  a[type == "player" & is.na(player_id), how := "no player match"]

  # Picks: find the selection each one became.
  a[, draft := fifelse(type == "pick_draft", y, fifelse(type == "future_pick", year, NA_integer_))]
  a[type != "player" & draft > max(years), how := "draft not yet held"]
  for (Yd in intersect(unique(a[type != "player" & is.na(how), draft]), years)) {
    dr <- drafts[[as.character(Yd)]]
    hp <- merge(dr$hops[when == as.character(y), .(pick, from = abbr(giver), to = abbr(receiver))],
                dr$picks[, .(pick, round, user = abbr(club), player, orig = abbr(orig_club))], by = "pick")
    rows <- a[type != "player" & draft == Yd & is.na(how), which = TRUE]
    # A club can get several picks from the same club in one period: pair them
    # in order, by stated number this year or by round and original club for a
    # future pick.
    key <- a[rows, .(row = rows, from, to, ord = fifelse(type == "pick_draft", as.numeric(pick), round * 100),
                     orig = fifelse(type == "future_pick", abbr(orig_club), NA_character_), rnd = round)]
    for (g in split(key, by = c("from", "to"))) {
      cand <- hp[from == g$from[1] & to == g$to[1]]
      if (!all(is.na(g$orig))) {
        cand <- cand[orig %in% g$orig & round %in% g$rnd]
      }
      g <- g[order(ord)]; cand <- cand[order(pick)]
      # A future pick takes the first unused hop with its own original club and
      # round, so two same-round picks in one trade cannot swap draftees.
      used <- logical(nrow(cand))
      for (j in seq_len(nrow(g))) {
        k <- which(!used & (is.na(g$orig[j]) | (cand$orig == g$orig[j] & cand$round == g$rnd[j])))[1]
        if (is.na(k)) next
        used[k] <- TRUE
        i <- g$row[j]; cj <- cand[k]
        set(a, i, "final_pick", cj$pick)
        if (cj$user == a$to[i]) {
          set(a, i, "draftee", cj$player)
        } else {
          set(a, i, c("realised", "after", "how"), list(0, pick_value(cj$pick) * disc^(Yd - y), "pick traded on"))
        }
      }
    }
  }
  # Draftees, valued as players from their first season.
  dd <- a[!is.na(draftee), which = TRUE]
  if (length(dd)) {
    m <- match_draft_names(data.table(row = dd, player = a$draftee[dd]), all_names)
    for (k in seq_along(dd)) {
      i <- dd[k]; pid <- m$player_id[m$row == i]
      if (length(pid) == 1 && !is.na(pid)) {
        cp <- credit_player(pid, a$to[i], a$draft[i] + 1L, y)
        set(a, i, c("realised", "after", "how"), list(cp$realised, cp$after, paste("drafted,", cp$how)))
        set(a, i, "player_id", pid)
      } else if (a$draft[i] >= 2024L) {
        set(a, i, c("realised", "after", "how"),
            list(0, pick_value(a$final_pick[i]) * disc^(a$draft[i] - y), "drafted, not played yet"))
      } else {
        message(sprintf("  %d trade %s: draftee '%s' (%d draft) not matched to a player; valued 0",
                        y, a$trade[i], a$draftee[i], a$draft[i]))
        set(a, i, c("realised", "after", "how"), list(0, 0, "drafted, never played"))
      }
    }
  }
  a[type != "player" & is.na(how), how := "not traced"]
  a[how %in% c("not traced", "draft not yet held"), `:=`(realised = 0, after = value)]
  a[how == "no player match", `:=`(realised = 0, after = 0)]
  a[, v_turned := realised + after]
  stopifnot(!anyNA(a$v_turned))

  g_in  <- a[, .(v_in = sum(v_turned), at_in = sum(value, na.rm = TRUE)), by = .(trade, club = to_club)]
  g_out <- a[, .(v_out = sum(v_turned), at_out = sum(value, na.rm = TRUE)), by = .(trade, club = from_club)]
  gr <- merge(g_in, g_out, by = c("trade", "club"), all = TRUE)
  for (cc in c("v_in", "v_out", "at_in", "at_out")) set(gr, which(is.na(gr[[cc]])), cc, 0)
  gr[, `:=`(net_turned = v_in - v_out, net_then = at_in - at_out)]
  stopifnot(abs(gr[, sum(net_turned)]) < 1e-6)

  cat("\n", y, ": assets", nrow(a), "| how:\n"); print(a[, .N, by = .(type, how = sub(" [0-9]{4}$", "", how))][order(type, -N)])
  fwrite(a, file.path(out_dir, sprintf("12_assets_%d.csv", y)))
  fwrite(gr, file.path(out_dir, sprintf("12_grades_%d.csv", y)))
  a[, yr := y]; gr[, year := y]
  list(a = a, g = gr)
}
res <- lapply(years, grade_year)
all_a <- rbindlist(lapply(res, `[[`, "a"), fill = TRUE)
all_g <- rbindlist(lapply(res, `[[`, "g"))

# ---- 5. Anchors (chosen before looking) ----
pick_rows <- all_a[type != "player" & draft <= 2025]
anchors <- c(
  nets_sum_to_zero = all_g[, abs(sum(net_turned)) < 1e-6, by = year][, all(V1)],
  # By at-the-time value: most untraced picks are late ones spent matching a
  # bid or never used, so a count understates what is traced.
  picks_traced_80pct_of_value = pick_rows[, sum(value[how != "not traced"]) / sum(value)] >= 0.8,
  dawson_still_at_adelaide = all_a[yr == 2021 & item == "Jordan Dawson", how] == "still there",
  grundy_traded_on_2023 = all_a[yr == 2022 & item == "Brodie Grundy" & to == abbr("Melbourne"), how] == "traded on 2023",
  # Added after Junior Rioli (Willie) and three others were found credited 0.
  every_traded_player_matched = all_a[type == "player", !anyNA(player_id) & all(player_id != "")]
)
print(anchors)
cat("picks traced:", pick_rows[how != "not traced", .N], "of", nrow(pick_rows),
    "| share of at-the-time pick value traced:", round(pick_rows[, sum(value[how != "not traced"]) / sum(value)], 3), "\n")
print(pick_rows[, .(n = .N, mean_pick_then = round(mean(fifelse(type == "pick_draft", pick, exp_pick)), 1),
                    value_then = round(sum(value), 1)), by = .(traced = how != "not traced")])

cat("\nbiggest wins as it turned out (eight-season TORP units; net = value received minus given):\n")
print(all_g[order(-net_turned)][1:12, .(year, trade, club, then = round(net_then, 1), turned = round(net_turned, 1))])
cat("\nassets that most beat their at-the-time value:\n")
print(all_a[order(-(v_turned - value))][1:12, .(yr, trade, item, to_club, draftee, how,
                                              then = round(value, 1), turned = round(v_turned, 1))])
stopifnot(all(anchors))
