# Grade the 23 trades of the 2026 trade period in TORP value (plan step 5).
#
# Every asset is valued in the same units: expected games x (TORP - replacement)
# over the next eight seasons, discounted 10% a year from 2027.
#   - Player: v8_mean from 03_project_value.R.
#   - 2026 pick N: the eight-season pick curve from 04_pick_curve.R at N.
#   - Future pick (year Y, round R, original club C): expected position
#     (R - 1) x 18 + slot, where slot comes from C's 2026 ladder finish pulled
#     halfway to the middle for 2027 and all the way for 2028 (ladders persist
#     only partly); plus a talent shift for Tasmania's pre-signed 17-year-olds
#     (7 signed from the 2027 class so far, up to 12 across 2026-27: +8 picks
#     in 2027, +5 in 2028; my reading of the rules, not an AFL figure); then
#     discounted one season per year of wait. ABC's "about a round" (+18) is
#     printed as a sensitivity.
# Source: Wikipedia "2026 AFL draft", Trades table, saved raw in
# torpdata/data/raw/trades_2026/. Free-agency moves are not trades and are
# listed separately.

suppressMessages({ library(data.table); library(rvest) })
source("data-raw/08-trade-value/draft_names.R")
suppressMessages(devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE))
raw_dir <- "C:/dev/torpverse/torpdata/data/raw/trades_2026"
out_dir <- "C:/dev/torpverse/torpdata/data/raw/trade_value"
disc <- 0.9
TAS_SHIFT <- c(`2027` = 8, `2028` = 5)

proj <- fread(file.path(out_dir, "03_projected_value_2026.csv"))
curve <- fread(file.path(out_dir, "04_pick_curve.csv"))
pick_value <- function(n) {
  n <- pmin(pmax(round(n), 1), max(curve$pick))
  curve$v8_direct[match(n, curve$pick)]
}

# ---- 1. Parse the trades table ----
page <- read_html(file.path(raw_dir, "wiki_2026_draft.html"))
tb <- html_table(page, fill = TRUE)
trades <- as.data.table(Filter(function(d) identical(names(d)[1], "Clubs involved"), tb)[[1]])
stopifnot(nrow(trades) == 23)   # ABC: "23 trades"

clean <- function(x) trimws(gsub("\\s*\\[[a-z0-9]+\\]|\\.mw-parser-output.*?\\}", "", x, perl = TRUE))
rows <- list()
for (i in seq_len(nrow(trades))) {
  txt <- paste(unlist(trades[i, 2:3]), collapse = "\n")
  txt <- gsub("\\)to ", ")\nto ", txt)                       # multi-club rows run segments together
  txt <- gsub("\\s*\\.mw-parser-output[^\n]*", "", txt)
  segs <- strsplit(txt, "\n(?=to )", perl = TRUE)[[1]]
  for (sg in segs) {
    parts <- trimws(strsplit(sg, "\n")[[1]]); parts <- parts[nzchar(parts)]
    hdr <- parts[1]
    to_club <- trimws(sub("^to ([^(;]+).*$", "\\1", hdr))
    from_hdr <- if (grepl("\\(from ", hdr)) sub("^.*\\(from ([^)]+)\\).*$", "\\1", hdr) else NA_character_
    for (it in parts[-1]) {
      it <- clean(it)
      from_it <- if (grepl("\\(from [^)]+\\)\\s*$", it)) sub("^.*\\(from ([^)]+)\\)\\s*$", "\\1", it) else from_hdr
      it0 <- trimws(sub("\\s*\\(from [^)]+\\)\\s*$", "", it))
      rows[[length(rows) + 1]] <- data.table(trade = i, to_club = to_club, from_club = from_it, item = it0)
    }
  }
}
a <- rbindlist(rows)
a[, type := fifelse(grepl("^pick", item), "pick_2026",
             fifelse(grepl("^20[0-9]{2} .*round pick", item), "future_pick", "player"))]
a[type == "pick_2026", pick := as.integer(sub("^pick (No\\. )?([0-9]+).*$", "\\2", item))]
a[type == "future_pick", `:=`(
  year = as.integer(substr(item, 1, 4)),
  round = match(sub("^20[0-9]{2} ([a-z]+) round.*$", "\\1", item), c("first", "second", "third", "fourth")),
  orig_club = sub("^.*round pick \\(([^)]+)\\).*$", "\\1", item))]
cat("assets parsed:", nrow(a), " by type:", paste(names(table(a$type)), table(a$type), collapse = ", "), "\n")
stopifnot(!anyNA(a[type == "pick_2026", pick]),
          !anyNA(a[type == "future_pick", .(year, round, orig_club)]))
# The five-club trade (7) gives only a future pick's original club, not who
# handed it over. Every such pick there is a party's own, so the original
# club is taken as the giver; anything else would stop here.
fill <- a[is.na(from_club)]
cat("future picks with no giving club, filled from the original club:", nrow(fill), "\n")
stopifnot(fill[, all(type == "future_pick")],
          all(fill$orig_club %in% c(a[trade %in% fill$trade, unique(to_club)])))
a[is.na(from_club), from_club := orig_club]

# ---- 2. Value each asset ----
# get_afl_ladder() `rank`: final ladder position (1 = top).
lad <- as.data.table(get_afl_ladder(2026))[, .(abbr = torp_team_abbr(team), ladder = as.integer(rank))]
stopifnot(nrow(lad) == 18, !anyNA(lad$abbr))
a[type == "future_pick", abbr := torp_team_abbr(orig_club)]
a <- merge(a, lad, by = "abbr", all.x = TRUE, sort = FALSE)
stopifnot(a[type == "future_pick", !anyNA(ladder)])
a[type == "future_pick", slot := 19 - ladder]                    # 18th on the ladder picks first
a[type == "future_pick", slot := fifelse(year == 2027, 0.5 * slot + 0.5 * 9.5, 9.5)]
a[type == "future_pick", exp_pick := (round - 1) * 18 + slot + TAS_SHIFT[as.character(year)]]
a[type == "future_pick", exp_pick_abc := (round - 1) * 18 + slot + 18]
a[type == "pick_2026", value := pick_value(pick)]
a[type == "future_pick", `:=`(value = pick_value(exp_pick) * disc^(year - 2026),
                              value_abc = pick_value(exp_pick_abc) * disc^(year - 2026))]

pl <- match_draft_names(a[type == "player", .(row = .I, player = item)], proj[, .(player_id, player_name)])
a[type == "player", player_id := pl$player_id[order(pl$row)]]
a <- merge(a, proj[, .(player_id, age, torp_2026, v8_mean)], by = "player_id", all.x = TRUE, sort = FALSE)
a[type == "player", value := v8_mean]
cat("players matched to a projection:", a[type == "player" & !is.na(value), .N], "of", a[type == "player", .N], "\n")
if (a[type == "player" & is.na(value), .N]) print(a[type == "player" & is.na(value), .(trade, item, to_club)])
a[is.na(value_abc), value_abc := value]

# ---- 2b. Value at the club, not just in the league ----
# The projection already handles leaving the league; this multiplies each
# season by the share of players still at the club given they are still
# playing (stay_curve.csv, from the tenure research). Players use the curve
# for moved players by age at arrival; picks use the draftee curve by pick
# band. The same weight applies on both sides of a trade, so trades stay
# zero-sum; it prices what the asset is worth to whoever holds it.
stay_path <- "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse-torp/fd72fa3a-4884-46e7-9859-8c01702f8ead/scratchpad/tenure/stay_curve.csv"
a[, value_club := NA_real_]
if (file.exists(stay_path)) {
  stay <- fread(stay_path)[k %in% 1:8]
  # players: sum_k ev_k x disc^(k-1) x share(age band, k)
  age_band <- function(x) fifelse(x < 24, "under24", fifelse(x < 28, "24-27", fifelse(x < 31, "28-30", "31plus")))
  evl <- melt(proj[, c("player_id", grep("^ev_", names(proj), value = TRUE)), with = FALSE],
              id.vars = "player_id", value.name = "ev")[, k := as.integer(sub("ev_", "", variable)) - 2026L]
  pv_club <- merge(evl, proj[, .(player_id, band = age_band(age + 1))], by = "player_id")
  pv_club <- merge(pv_club, stay[group_type == "moved", .(band = group, k, share_at_club)], by = c("band", "k"))
  pv_club <- pv_club[, .(value_club = sum(ev * disc^(k - 1) * share_at_club)), by = player_id]
  a[pv_club, on = "player_id", value_club := fifelse(type == "player", i.value_club, value_club)]
  # picks: scale the eight-season value by the band's at-club share of value
  ps <- fread(file.path(out_dir, "04_pick_seasons.csv"))
  pick_band <- function(p) as.character(cut(p, c(0, 5, 10, 20, 30, 45, Inf),
                                            labels = c("1-5", "6-10", "11-20", "21-30", "31-45", "46plus")))
  ps[, band := pick_band(pick)]
  ps <- merge(ps[, .(v = mean(v)), by = .(band, k)], stay[group_type == "draftee", .(band = group, k, share_at_club)],
              by = c("band", "k"))
  fac <- ps[, .(club_factor = if (sum(v) > 0) sum(v * share_at_club) / sum(v) else mean(share_at_club)), by = band]
  cat("\ndraftee at-club factor by pick band (share of eight-season value kept by the drafting club):\n"); print(fac)
  a[type != "player", band := pick_band(fifelse(type == "pick_2026", pick, exp_pick))]
  a[fac, on = "band", value_club := fifelse(type != "player", value * i.club_factor, value_club)]
  stopifnot(a[, !anyNA(value_club)])
} else {
  cat("\nno stay curve yet; value_club not computed\n")
}

# ---- 3. Grade ----
# value in for each club minus value out, per trade
g_in  <- a[, .(v_in = sum(value, na.rm = TRUE), v_in_abc = sum(value_abc, na.rm = TRUE),
               v_in_club = sum(value_club), got = paste(item, collapse = "; ")), by = .(trade, club = to_club)]
g_out <- a[, .(v_out = sum(value, na.rm = TRUE), v_out_abc = sum(value_abc, na.rm = TRUE),
               v_out_club = sum(value_club)), by = .(trade, club = from_club)]
gr <- merge(g_in, g_out, by = c("trade", "club"), all = TRUE)
for (v in c("v_in", "v_out", "v_in_abc", "v_out_abc", "v_in_club", "v_out_club")) gr[is.na(get(v)), (v) := 0]
gr[, `:=`(net = round(v_in - v_out, 1), net_abc = round(v_in_abc - v_out_abc, 1),
          net_club = round(v_in_club - v_out_club, 1))]
stopifnot(gr[, abs(sum(net)), by = trade][, all(V1 < 1e-6 + 0.1 * .N)])  # each trade is zero-sum
fwrite(a, file.path(out_dir, "05_trade_assets.csv"))
fwrite(gr, file.path(out_dir, "05_trade_grades.csv"))

cat("\nasset values (8-season TORP value units):\n")
print(a[, .(trade, to_club, item, type, exp_pick = round(exp_pick, 1), age, torp_2026, value = round(value, 1))][order(trade)], nrows = 200)
cat("\nnet value by trade and club (value in minus value out; positive = club won):\n")
print(gr[order(trade, -net), .(trade, club, v_in = round(v_in, 1), v_out = round(v_out, 1), net, net_abc, net_club)], nrows = 100)
cat("\nnet over the whole trade period, by club:\n")
print(gr[, .(trades = .N, net = round(sum(net), 1), net_abc = round(sum(net_abc), 1),
             net_club = round(sum(net_club), 1)), by = club][order(-net)])
