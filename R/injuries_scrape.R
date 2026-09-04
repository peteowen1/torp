# AFL Injury Data: Scraping & Loading
# ===================================
# Sources for injury data: scrape the live afl.com.au weekly injury list and
# load the curated preseason CSV from inst/extdata. Combined and matched to
# rating data downstream (see R/injuries_match.R).

#' AFL Injury Data Functions
#'
#' Centralized functions for scraping, loading, matching, and validating AFL
#' injury data. Used by both the predictions pipeline and season simulation.
#'
#' @name injuries
NULL


#' Scrape AFL Weekly Injury List
#'
#' Scrapes the current injury list from afl.com.au. The page contains 18 tables
#' (one per team) in alphabetical team order. Each table's last row contains an
#' "Updated: ..." date stamp. Returns player names, injury descriptions,
#' estimated return timelines, team names, and the per-team updated date.
#'
#' @param timeout Numeric timeout in seconds for the HTTP request.
#' @return A data.frame with columns: `player`, `team`, `injury`,
#'   `estimated_return`, `updated`, `player_norm`.
#' @export
scrape_injuries <- function(timeout = 30) {
  empty_df <- data.frame(
    player = character(), team = character(), injury = character(),
    estimated_return = character(), updated = as.Date(character()),
    player_norm = character(), stringsAsFactors = FALSE
  )

  tryCatch({
    if (!requireNamespace("rvest", quietly = TRUE)) {
      cli::cli_abort("Package {.pkg rvest} is required to scrape injuries.")
    }
    url <- "https://www.afl.com.au/matches/injury-list"
    session <- rvest::session(url, httr::timeout(timeout))
    raw_tables <- rvest::html_table(session)
    n_tables <- length(raw_tables)

    # READ each table's club off the page; never infer it from position.
    #
    # Position-based labelling is what broke in the 2026 finals: the page lists
    # only clubs still alive (8 tables, not 18), and pairing the first 8
    # alphabetical names with them mislabelled 30 of 49 rows -- Sydney's whole
    # list, Heeney/Warner/Blakey included, came out as Geelong, and every row
    # was then silently dropped downstream for not matching a player rating.
    #
    # Each table is preceded by that club's badge image, whose URL carries a
    # club code: "..._Straps-Badge-Refresh_ADEL_FA-1x.jpg". Verified against
    # the live page on 2026-09-04: the eight tables resolve to ADEL, BRIS,
    # CARL, FREM, GEEL, HAW, SYD, WB -- exactly the eight clubs still alive,
    # and confirming independently which tables the old code had mislabelled.
    #
    # That markup sits inside an HTML COMMENT, so the parsed DOM cannot see it
    # and we scan the raw response text instead. That is the fragile part, and
    # it is why every failure below is FAIL CLOSED rather than a fallback: if
    # the AFL restructures the page, this returns nothing and says so, instead
    # of quietly attributing injuries to the wrong clubs again.
    raw_html <- tryCatch(
      httr::content(session$response, as = "text", encoding = "UTF-8"),
      error = function(e) NA_character_
    )
    if (is.na(raw_html) || !nzchar(raw_html)) {
      cli::cli_warn(c("!" = "Could not read the injury page's raw HTML -- returning NO weekly injuries.",
                      "i" = "Club identity is read from a badge URL in the page source; without it every label would be a guess."))
      return(empty_df)
    }

    team_names <- injury_table_clubs(raw_html, n_tables)
    if (is.null(team_names)) return(empty_df)   # every failure already warned

    all_rows <- vector("list", n_tables)

    for (i in seq_len(min(n_tables, length(team_names)))) {
      tbl <- torp_clean_names(raw_tables[[i]])

      if (nrow(tbl) == 0) next

      # Last row contains "Updated: March 17, 2026" -- extract and remove it
      last_vals <- as.character(tbl[nrow(tbl), 1])
      updated_date <- NA
      if (grepl("^Updated:", last_vals, ignore.case = TRUE)) {
        date_str <- trimws(sub("^Updated:\\s*", "", last_vals, ignore.case = TRUE))
        updated_date <- tryCatch(
          as.Date(date_str, format = "%B %d, %Y"),
          error = function(e) NA
        )
        tbl <- tbl[-nrow(tbl), , drop = FALSE]
      }

      if (nrow(tbl) == 0) next

      tbl$team <- team_names[i]
      tbl$updated <- updated_date
      all_rows[[i]] <- tbl
    }

    inj <- do.call(rbind, all_rows[!vapply(all_rows, is.null, logical(1))])
    if (is.null(inj) || nrow(inj) == 0) return(empty_df)

    # Standardise known name mismatches between AFL injury list and player data
    # Add new entries here as they arise (injury_name = canonical_name)
    injury_name_fixes <- c(
      "Cam Zurhaar" = "Cameron Zurhaar"
    )
    fix_idx <- match(inj$player, names(injury_name_fixes))
    inj$player[!is.na(fix_idx)] <- injury_name_fixes[fix_idx[!is.na(fix_idx)]]

    inj$player_norm <- norm_name(inj$player)
    inj
  }, error = function(e) {
    cli::cli_warn("Failed to scrape injury list: {conditionMessage(e)}")
    empty_df
  })
}


#' Load Preseason Injury List
#'
#' Reads a curated CSV of preseason injuries from `inst/extdata/`. These
#' capture long-term injuries known before the season starts (e.g., ACL
#' reconstructions, stress fractures) that won't appear on the weekly AFL
#' injury list until teams are required to report.
#'
#' @param season Numeric season year (e.g. 2026).
#' @return A data.frame with columns: `player`, `team`, `injury`,
#'   `estimated_return`, `player_norm`. Returns an empty data.frame (with
#'   correct columns) if no file exists for the requested season.
#' @export
load_preseason_injuries <- function(season) {
  filename <- paste0("preseason_injuries_", season, ".csv")
  path <- system.file("extdata", filename, package = "torp")

  empty_df <- data.frame(
    player = character(),
    team = character(),
    injury = character(),
    estimated_return = character(),
    player_norm = character(),
    stringsAsFactors = FALSE
  )

  if (path == "") {
    cli::cli_alert_info("No preseason injury file for {season}")
    return(empty_df)
  }

  inj <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE),
    error = function(e) {
      cli::cli_warn("Failed to read preseason injuries: {conditionMessage(e)}")
      return(empty_df)
    }
  )

  if (nrow(inj) == 0) return(empty_df)

  # Ensure expected columns exist
  expected <- c("player", "team", "injury", "estimated_return")
  missing <- setdiff(expected, names(inj))
  if (length(missing) > 0) {
    cli::cli_warn("Preseason injury file missing columns: {paste(missing, collapse = ', ')}")
    return(empty_df)
  }

  inj$player_norm <- norm_name(inj$player)
  inj
}


#' Resolve each injury table's club from the page source
#'
#' `scrape_injuries()` must never infer a club from a table's POSITION -- see
#' its comments for what that cost in the 2026 finals. Each table on
#' afl.com.au's injury list is preceded by that club's badge image, whose URL
#' carries a club code (`..._Straps-Badge-Refresh_ADEL_FA-1x.jpg`). This reads
#' those codes in document order and resolves them through [AFL_TEAM_ALIASES].
#'
#' Pulled out as its own function so the correctness claim can be tested
#' directly, on strings, rather than through a mocked HTTP session.
#'
#' Every failure mode returns `NULL` after warning -- FAIL CLOSED. The badge
#' markup sits inside an HTML comment, so it is not visible to a DOM parser and
#' this scans raw text; if the AFL restructures the page, publishing nothing and
#' saying so beats attributing injuries to the wrong clubs again.
#'
#' @param raw_html Character scalar, the page's raw HTML.
#' @param n_tables Integer, how many tables the parser found.
#' @return A character vector of canonical club names, one per table, or `NULL`.
#' @keywords internal
injury_table_clubs <- function(raw_html, n_tables) {
  if (length(raw_html) != 1L || is.na(raw_html) || !nzchar(raw_html)) {
    cli::cli_warn(c(
      "!" = "Could not read the injury page's raw HTML -- returning NO weekly injuries.",
      "i" = "Club identity is read from a badge URL in the page source; without it every label would be a guess."
    ))
    return(NULL)
  }
  # One segment per table, each holding everything before it. A table's badge is
  # the LAST one appearing in its segment.
  segments <- strsplit(raw_html, "<table", fixed = TRUE)[[1]]
  if (length(segments) - 1L != n_tables) {
    cli::cli_warn(c(
      "!" = "Found {n_tables} parsed table{?s} but {length(segments) - 1L} <table> tag{?s} in the source -- returning NO weekly injuries.",
      "i" = "The two must correspond one-to-one for the badge lookup to name the right club."
    ))
    return(NULL)
  }
  # The scheme rests on an invariant the page is not obliged to keep: EXACTLY
  # ONE club's badge per segment, sitting immediately BEFORE its table. Both
  # halves are checked, because a violation of either resolves cleanly to a
  # wrong club and none of the other guards can see it — the failure would be
  # silent and confident, which is the thing this whole function exists to stop.
  #
  # Measured on the live page 2026-09-04: each segment carries 16 matches — the
  # responsive srcset variants of one badge — ALL WITH THE SAME CODE, and the
  # last sits 83-86 characters before its table.
  MAX_BADGE_GAP <- 4000L   # ~45x the observed 83-86, far below a segment (~36KB)
  mixed <- character(0)
  far <- integer(0)
  codes <- vapply(seq_len(n_tables), function(i) {
    # Lookaround rather than a capture group plus sub(): the backreference form
    # needs "\1" in R source, which is one escaping layer away from silently
    # becoming a control character and matching nothing (it did, first time).
    m <- gregexpr("(?<=Straps-Badge-Refresh_)[A-Z]+(?=_)", segments[i], perl = TRUE)[[1]]
    hits <- regmatches(segments[i], list(m))[[1]]
    if (!length(hits)) return(NA_character_)
    # ONE club per segment. A promo strip or related-content widget carrying
    # another club's crest between the real badge and the table would otherwise
    # win on "last match" and silently relabel the table.
    if (length(unique(hits)) > 1L) {
      mixed <<- c(mixed, paste0("table ", i, ": ", paste(unique(hits), collapse = "/")))
      return(NA_character_)
    }
    # ...and it must sit just before the table. If the AFL ever moves badges to
    # FOLLOW their tables, every label shifts by one and still resolves — the
    # one arrangement that defeats every other check here.
    gap <- nchar(segments[i]) - (m[length(m)] + attr(m, "match.length")[length(m)] - 1L)
    if (gap > MAX_BADGE_GAP) {
      far <<- c(far, i)
      return(NA_character_)
    }
    hits[1]
  }, character(1))
  if (length(mixed)) {
    cli::cli_warn(c(
      "!" = "More than one club's badge sits before the same injury table -- returning NO weekly injuries.",
      "i" = "{paste(mixed, collapse = '; ')}.",
      "i" = "Taking the nearest would be a guess; the page layout has changed and the lookup needs rechecking."
    ))
    return(NULL)
  }
  if (length(far)) {
    cli::cli_warn(c(
      "!" = "{length(far)} injury table{?s} had no badge within {MAX_BADGE_GAP} characters (table{?s}: {paste(far, collapse = ', ')}) -- returning NO weekly injuries.",
      "i" = "A distant badge usually means badges now FOLLOW their tables, which shifts every club label by one while still resolving cleanly."
    ))
    return(NULL)
  }

  team_names <- unname(AFL_TEAM_ALIASES[codes])
  unresolved <- is.na(team_names)
  if (any(unresolved)) {
    # A code we do not know is a club we would otherwise mislabel. Name it: the
    # fix is one line in AFL_TEAM_ALIASES, and only 8 of the 18 codes are
    # observable during finals, so the rest will surface this way in March.
    bad <- ifelse(is.na(codes[unresolved]), "(no badge found)", codes[unresolved])
    cli::cli_warn(c(
      "!" = "Could not resolve {sum(unresolved)} of {n_tables} injury table{?s} to a club -- returning NO weekly injuries.",
      "i" = "Unresolved badge code{?s}: {paste(unique(bad), collapse = ', ')}.",
      "i" = "If these are real club codes, add them to AFL_TEAM_ALIASES; if the page no longer carries badge URLs, the club must be read some other way."
    ))
    return(NULL)
  }
  if (anyDuplicated(team_names)) {
    cli::cli_warn(c(
      "!" = "Two injury tables resolved to the same club ({paste(unique(team_names[duplicated(team_names)]), collapse = ', ')}) -- returning NO weekly injuries.",
      "i" = "The badge lookup has drifted out of step with the tables."
    ))
    return(NULL)
  }
  team_names
}
