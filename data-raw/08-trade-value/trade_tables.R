# Parse a Wikipedia "<year> AFL draft" page's Trades table into one row per
# asset moved: trade number, receiving club, giving club, item text, and its
# type (player, pick in that year's draft, or a future-year pick).
#
# Same parsing as 05_grade_trades.R section 1 (which stays as it is, so the
# published 2026 grades cannot move), generalised to any year. Used by
# 10_grade_past_trades.R.

suppressMessages({ library(data.table); library(rvest) })

CLUB_START <- paste(c("Adelaide", "Brisbane", "Carlton", "Collingwood", "Essendon", "Fremantle", "Geelong",
                      "Gold Coast", "Greater Western Sydney", "Hawthorn", "Melbourne", "North Melbourne",
                      "Port Adelaide", "Richmond", "St Kilda", "Sydney", "West Coast", "Western Bulldogs"),
                    collapse = "|")

parse_wiki_trades <- function(html_path, draft_year) {
  tb <- html_table(read_html(html_path), fill = TRUE)
  hit <- Filter(function(d) identical(names(d)[1], "Clubs involved"), tb)
  stopifnot(length(hit) == 1)
  trades <- as.data.table(hit[[1]])
  clean <- function(x) trimws(gsub("\\s*\\[[a-z0-9]+\\]|\\.mw-parser-output.*?\\}", "", x, perl = TRUE))
  rows <- list()
  for (i in seq_len(nrow(trades))) {
    txt <- paste(unlist(trades[i, 2:3]), collapse = "\n")
    txt <- gsub("\\)to ", ")\nto ", txt)                       # multi-club rows run segments together
    # ...including after a player's name ("Sam Petrevski-Setonto Carlton", 2021).
    txt <- gsub(paste0("([a-z])to (", CLUB_START, ")"), "\\1\nto \\2", txt)
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
  # Draft-night swaps sit below a "Picks swapped at the <year> National Draft"
  # row, in a different (and in 2025 free-text) format. Only trade-period
  # trades are graded.
  cut_at <- a[grepl("^Picks swapped at the", item), min(trade)]
  if (is.finite(cut_at)) a <- a[trade < cut_at]
  # Priority picks read "2023 second round priority pick (North Melbourne)" or
  # "2024 end of first round priority pick (pick No. 19) (North Melbourne)".
  a[, type := fifelse(grepl("^pick", item), "pick_draft",
               fifelse(grepl("^20[0-9]{2} .*round .*pick", item), "future_pick", "player"))]
  a[type == "pick_draft", pick := as.integer(sub("^pick (No\\. )?([0-9]+).*$", "\\2", item))]
  a[type == "future_pick", `:=`(
    year = as.integer(substr(item, 1, 4)),
    round = match(sub("^20[0-9]{2} (end of )?([a-z]+) round.*$", "\\2", item), c("first", "second", "third", "fourth")),
    # "(Collingwood, from Greater Western Sydney)": the original club is the first name.
    orig_club = fifelse(grepl("\\)\\s*$", item), sub(",.*$", "", sub("^.*\\(([^)]+)\\)\\s*$", "\\1", item)), NA_character_),
    stated_pick = suppressWarnings(as.integer(sub("^.*\\(pick No\\. ([0-9]+)\\).*$", "\\1", item))))]
  # "2024 third round pick" with no club named is the giving club's own pick.
  a[type == "future_pick" & is.na(orig_club), orig_club := from_club]
  # A header can read "to Western Bulldogs from West Coast" (2025 trade 29).
  a[grepl(" from ", to_club), `:=`(from_club = fifelse(is.na(from_club), sub("^.* from ", "", to_club), from_club),
                                  to_club = sub(" from .*$", "", to_club))]
  # Non-breaking spaces ("St Kilda") would split one club in two.
  for (col in c("to_club", "from_club", "orig_club", "item")) set(a, j = col, value = trimws(gsub(" ", " ", a[[col]])))
  # Multi-club rows sometimes leave the giver blank. Take the pick's original
  # club, or the club it came via ("(Collingwood, from GWS)"), when it is a
  # party to the trade, else the only other party; anything still unresolved
  # stops the caller.
  parties <- a[, .(clubs = list(unique(na.omit(c(to_club, from_club))))), by = trade]
  a[parties, on = "trade", clubs := i.clubs]
  a[is.na(from_club), from_club := mapply(function(oc, to, cl, it) {
    others <- setdiff(cl, to)
    via <- if (grepl(", from [^)]+\\)\\s*$", it)) sub("^.*, from ([^)]+)\\)\\s*$", "\\1", it) else NA_character_
    if (!is.na(via) && via %in% others) via
    else if (!is.na(oc) && oc %in% others) oc
    else if (length(others) == 1) others else NA_character_
  }, orig_club, to_club, clubs, item)]
  a[, clubs := NULL]
  a[, draft_year := draft_year]
  setattr(a, "n_trades", if (is.finite(cut_at)) cut_at - 1L else nrow(trades))
  a[]
}
