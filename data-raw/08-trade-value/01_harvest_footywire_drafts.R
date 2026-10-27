# Harvest every AFL National Draft from Footywire, raw first.
#
# Why: player_details' draft fields record a player's latest list move, not his
# original draft (Jake Lloyd shows as a 2025 rookie elevation), and only cover
# players listed 2021+, so busts are missing. The pick-value curve needs every
# draftee, including the ones who never played. See docs/plans/TRADE-VALUE.md.
#
# Step 1 saves each page exactly as received (gzipped, one file per year) and
# never re-fetches a year already on disk. Step 2 parses those files only.
#
# Output (gitignored, under torpdata/data/raw/):
#   footywire_drafts/<year>.html.gz   raw pages
#   footywire_drafts.csv              parsed: one row per pick, every column kept

library(rvest)
library(data.table)

years   <- 1991:2025  # Footywire has no draft table for 1986-1990
raw_dir <- "C:/dev/torpverse/torpdata/data/raw/footywire_drafts"
out_csv <- "C:/dev/torpverse/torpdata/data/raw/footywire_drafts.csv"
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Fetch (polite: 3s between requests, skip what we already have) ----
for (yr in years) {
  f <- file.path(raw_dir, paste0(yr, ".html.gz"))
  if (file.exists(f)) next
  url <- paste0("https://www.footywire.com/afl/footy/ft_drafts?year=", yr)
  resp <- httr::GET(url, httr::user_agent("torp research (fptpost@gmail.com)"))
  if (httr::status_code(resp) != 200) {
    message(yr, ": HTTP ", httr::status_code(resp), " - skipped")
  } else {
    con <- gzfile(f, "wb"); writeBin(httr::content(resp, "raw"), con); close(con)
    message(yr, ": saved ", length(httr::content(resp, "raw")), " bytes")
  }
  Sys.sleep(3)
}

# ---- 2. Parse from disk ----
parse_year <- function(yr) {
  f <- file.path(raw_dir, paste0(yr, ".html.gz"))
  if (!file.exists(f)) return(NULL)
  page <- read_html(gzfile(f))
  tbs <- html_table(page, fill = TRUE)
  # 1997+ tables lead with a Round column (6 cols); 1986-1996 start at Pick (5).
  hit <- Filter(function(d) (ncol(d) == 6 && identical(d[[1]][1], "Round")) ||
                            (ncol(d) == 5 && identical(d[[1]][1], "Pick")), tbs)
  if (length(hit) != 1) {
    message(yr, ": expected 1 draft table, found ", length(hit))
    return(NULL)
  }
  d <- as.data.table(hit[[1]])
  d <- d[-1]
  cols <- c("pick", "drafted_by", "player_raw", "current_team", "games_since_drafted")
  if (ncol(d) == 5) {
    setnames(d, cols)
    d[, round := NA_character_]
  } else {
    setnames(d, c("round", cols))
  }
  # Clubs passing or forfeiting a pick leave a placeholder row: keep it, tag it.
  d[, is_pass := player_raw %in% c("PASS", "FORFEIT")]
  # Footywire appends a status letter on a new line (e.g. "Nick Blakey\nA").
  # Keep the raw cell and split it; the letters are kept, not interpreted here.
  d[, player := trimws(sub("\n.*$", "", player_raw))]
  d[, flag := fifelse(grepl("\n", player_raw),
                      trimws(sub("^.*\n", "", player_raw)), NA_character_)]
  d[, `:=`(draft_year = yr,
           round = as.integer(round),
           pick = as.integer(pick),
           games_since_drafted = as.integer(games_since_drafted))]
  d
}

drafts <- rbindlist(lapply(years, parse_year), fill = TRUE)

# ---- 3. Sanity and coverage ----
cat("rows:", nrow(drafts), " years:", uniqueN(drafts$draft_year),
    " (", min(drafts$draft_year), "-", max(drafts$draft_year), ")\n")
print(drafts[, .(picks = .N, passes = sum(is_pass),
                 max_pick = max(pick, na.rm = TRUE),
                 zero_games = sum(games_since_drafted == 0, na.rm = TRUE)),
             by = draft_year], nrows = 100)
# Anchors chosen before looking: famous picks must land where history says.
anchor <- function(y, p, who) {
  got <- drafts[draft_year == y & pick == p, player]
  ok <- length(got) == 1 && grepl(who, got)
  cat(sprintf("anchor %d pick %d = %s: %s\n", y, p, who, if (ok) "ok" else paste("FAIL, got", got)))
  ok
}
stopifnot(anchor(2018, 1, "Sam Walsh"), anchor(2018, 10, "Nick Blakey"),
          anchor(2001, 1, "Luke Hodge"), anchor(1995, 2, "Matthew Primus"),
          anchor(2025, 1, "Willem Duursma"))
stopifnot(drafts[is.na(games_since_drafted), all(is_pass)])
cat("\nflag letters:\n"); print(drafts[, .N, by = flag][order(-N)])
cov <- drafts[, lapply(.SD, function(x) round(mean(!is.na(x) & x != ""), 3))]
cat("\ncoverage per column (share non-empty):\n"); print(cov)
stopifnot(all(unlist(cov[, .(pick, player, drafted_by, draft_year)]) > 0.99))
stopifnot(setequal(unique(drafts$draft_year), years))  # a failed year drops silently otherwise
dup <- drafts[, .N, by = .(draft_year, pick)][N > 1]
if (nrow(dup)) { message("duplicate (year, pick) rows:"); print(dup) }

fwrite(drafts, out_csv)
cat("wrote", out_csv, "\n")
