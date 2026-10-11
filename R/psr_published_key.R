# Position keys for published PSR (Thread G, decided 2026-10-11)
# ==============================================================
# Published PSR compares each player with his listed position for that season,
# the key EPR centres on and torp_ratings presents under; the match model keeps
# the career-modal pos_group. The weekly team-sheet key is kept reachable for
# scoring only. See calculate_psr()'s `psr_centre` and torpverse
# docs/DECISIONS.md 2026-10-11.

#' Load each player's listed position, per season
#'
#' The source \code{torp_ratings$position_group} and EPR use:
#' \code{load_player_details()}, one call per season. Unlike
#' \code{.load_listed_positions()}, which keeps only a player's latest listing
#' for all of history, this keeps one row per player-season, so a 2022 rating is
#' compared with how he was listed in 2022.
#'
#' Also loads the season before the earliest one requested, so a one-season
#' build (the daily job) rolls a player with no listing that season back to the
#' same earlier listing a full rebuild would.
#'
#' A requested season that fails to load, comes back empty, or has no position
#' column ABORTS. Skipping it would roll that season's rows back to older
#' listings and publish them under the listed key's name; the current season,
#' fetched fresh from the API every run, is the likeliest to fail.
#'
#' @param seasons Integer vector of seasons.
#' @param comp Competition, forwarded to \code{load_player_details()}.
#' @return A data.table of \code{player_id}, \code{season}, \code{position}.
#' @keywords internal
.load_season_listing <- function(seasons, comp = "AFLM") {
  seasons <- sort(unique(stats::na.omit(as.integer(seasons))))
  prior <- min(seasons) - 1L
  min_season <- if (identical(comp, "AFLM")) AFL_MIN_SEASON else -Inf
  if (prior >= min_season) seasons <- c(prior, seasons)
  got <- lapply(seasons, function(s) {
    d <- tryCatch(data.table::as.data.table(load_player_details(s, comp = comp)),
                  error = function(e) {
                    # The prior season is a convenience; tolerate its absence
                    # (e.g. a competition's first year), never a requested one.
                    if (s == prior) return(NULL)
                    cli::cli_abort("Listed positions for season {s} ({comp}) failed to load: {conditionMessage(e)}")
                  })
    pcol <- if (is.null(d)) NA_character_ else intersect(c("position_group", "position"), names(d))[1]
    if (is.null(d) || nrow(d) == 0 || is.na(pcol) || !"player_id" %in% names(d)) {
      if (s == prior) return(NULL)
      cli::cli_abort("Listed positions for season {s} ({comp}) came back without rows, player_id or a position column.")
    }
    d[!is.na(get(pcol)), list(player_id = as.character(player_id), season = s,
                              position = as.character(get(pcol)))]
  })
  out <- data.table::rbindlist(got, use.names = TRUE)
  unique(out, by = c("player_id", "season"), fromLast = TRUE)
}

#' Attach each player's listed position for that season
#'
#' Adds \code{listed_pos_group}: the \code{MATCH_LISTED_POS_MAP} bucket of his
#' listing for the row's season. A player-season with no listing takes his
#' latest EARLIER season's listing, then his career \code{pos_group} (mapped to
#' the same buckets) -- never \code{calculate_psr()}'s global-mean branch.
#'
#' Aborts when, in ANY season, the rows that matched a listing for their own
#' season carry under 90\% of that season's \code{wt_80s} (under 50\% of rows
#' when there are no weights). Unmatched rows are mostly players who never
#' play: measured 2026-10-11, 63-71\% of rows but 95-100\% of weight matched in
#' every season 2021-2026. Per season, because one missing season out of six
#' still leaves the overall share above any sensible overall floor.
#'
#' @param skills Stat-ratings frame with \code{player_id}, \code{season}, and
#'   (for the fallback) \code{pos_group}.
#' @param listing Frame with \code{player_id}, \code{season}, \code{position}.
#' @return \code{skills} as a data.table with \code{listed_pos_group} added.
#' @keywords internal
.attach_season_listing <- function(skills, listing) {
  dt <- data.table::copy(data.table::as.data.table(skills))
  if (!all(c("player_id", "season") %in% names(dt))) {
    cli::cli_abort("{.arg skills} needs player_id and season; got {.val {names(dt)}}")
  }
  ls <- data.table::as.data.table(listing)
  if (!all(c("player_id", "season", "position") %in% names(ls))) {
    cli::cli_abort("{.arg listing} needs player_id, season, position; got {.val {names(ls)}}")
  }
  ls <- ls[, list(.pid = as.character(player_id), .key = as.integer(season),
                  .lpg = .collapse_listed_position(position))]
  ls <- unique(ls[!is.na(.lpg) & !is.na(.key)], by = c(".pid", ".key"), fromLast = TRUE)
  if (nrow(ls) == 0) {
    cli::cli_abort("No usable listed positions in {.arg listing}.")
  }

  q <- dt[, list(.pid = as.character(player_id), .key = as.integer(season))]
  exact  <- ls[q, on = c(".pid", ".key"), mult = "last"]$.lpg
  rolled <- ls[q, on = c(".pid", ".key"), roll = TRUE, mult = "last"]$.lpg

  n <- nrow(dt)
  n_exact  <- sum(!is.na(exact))
  n_rolled <- sum(!is.na(rolled)) - n_exact
  dt[, listed_pos_group := rolled]
  if ("pos_group" %in% names(dt)) {
    dt[is.na(listed_pos_group), listed_pos_group := .collapse_listed_position(pos_group)]
  }
  n_none <- sum(is.na(dt$listed_pos_group))
  cli::cli_inform(
    "Listed PSR key: {n_exact} listed that season, {n_rolled} from an earlier season, {n - n_exact - n_rolled - n_none} career fallback, {n_none} none (of {n})."
  )
  has_w <- "wt_80s" %in% names(dt)
  cov <- data.table::data.table(
    season = dt$season, hit = !is.na(exact),
    w = if (has_w) pmax(data.table::fcoalesce(as.numeric(dt$wt_80s), 0), 0) else 1
  )[, list(share = if (sum(w) > 0) sum(w[hit]) / sum(w) else mean(hit)), by = season]
  floor_share <- if (has_w) 0.9 else 0.5
  bad <- cov[share < floor_share]
  if (nrow(bad) > 0) {
    unit <- if (has_w) "wt_80s" else "rows"
    cli::cli_abort(c(
      "Listed PSR key: these seasons matched their own season's listing on too little of their {unit} (floor {100 * floor_share}%): {paste0(bad$season, ' ', round(100 * bad$share), '%', collapse = ', ')}.",
      "x" = "Refusing to publish PSR centred on fallbacks under the listed key's name."
    ))
  }
  dt[]
}

#' Load weekly team-sheet slots for PSR centring
#'
#' One row per named player-match: \code{player_id}, \code{season},
#' \code{round}, \code{lineup_position}, from \code{player_game_data} -- the
#' same source EPV's role stage reads.
#'
#' Also loads the season before the earliest one requested, so a player's
#' early-round rows can roll back to where he was named last year. Without
#' that, a single-season rebuild (the daily job) and a full-history rebuild
#' would give different keys for the same row.
#'
#' @param seasons Integer vector of seasons.
#' @return A data.table.
#' @keywords internal
.load_weekly_lineup <- function(seasons) {
  seasons <- sort(unique(stats::na.omit(as.integer(seasons))))
  seasons <- unique(c(seasons, min(seasons) - 1L))
  seasons <- sort(seasons[seasons >= AFL_MIN_SEASON])
  pgd <- load_player_game_data(
    seasons, columns = c("player_id", "season", "round", "lineup_position"))
  data.table::as.data.table(pgd)
}

#' Attach each player's latest named 6-way slot for weekly PSR centring
#'
#' For every \code{(player_id, season, round)} row of \code{skills}, takes the
#' on-field slot he was named in that round or, when he was not named on the
#' field (bench, injured, not selected, or a round not played yet), the most
#' recent earlier one. A rolling join against the team sheets rather than
#' carry-forward over \code{skills}' own rows, so a single-round frame still
#' finds his history. Rows with no named slot at or before that round fall back
#' to \code{pos_group} -- never to \code{calculate_psr()}'s global-mean branch,
#' which would quietly make every never-named row a different comparison.
#'
#' Bench and emergency codes (\code{INT}, \code{SUB}, \code{EMERG}) map to NA in
#' \code{LINEUP_POSITION_GROUP_MAP} and are skipped, so they roll back to the
#' last on-field slot.
#'
#' @param skills Stat-ratings frame with \code{player_id}, \code{season},
#'   \code{round}, and (for the fallback) \code{pos_group}.
#' @param lineup Frame with \code{player_id}, \code{season}, \code{round},
#'   \code{lineup_position}.
#' @return \code{skills} as a data.table with \code{lineup_pos_group} added.
#' @keywords internal
.attach_weekly_pos <- function(skills, lineup) {
  dt <- data.table::copy(data.table::as.data.table(skills))
  need <- c("player_id", "season", "round")
  if (!all(need %in% names(dt))) {
    cli::cli_abort("{.arg skills} needs {.field {need}}; got {.val {names(dt)}}")
  }
  lp <- data.table::as.data.table(lineup)
  if (!all(c(need, "lineup_position") %in% names(lp))) {
    cli::cli_abort("{.arg lineup} needs player_id, season, round, lineup_position; got {.val {names(lp)}}")
  }
  lp <- lp[, list(
    .pid = as.character(player_id),
    .key = as.integer(season) * 1000L + as.integer(round),
    .lpg = unname(LINEUP_POSITION_GROUP_MAP[as.character(lineup_position)])
  )]
  lp <- unique(lp[!is.na(.lpg) & !is.na(.key)], by = c(".pid", ".key"))
  if (nrow(lp) == 0) {
    cli::cli_abort(c(
      "No named on-field slots in {.arg lineup}.",
      "x" = "Refusing to centre weekly on nothing: every row would fall back to pos_group and still look like a weekly run."
    ))
  }

  q <- dt[, list(.pid = as.character(player_id),
                 .key = as.integer(season) * 1000L + as.integer(round))]
  rolled <- lp[q, on = c(".pid", ".key"), roll = TRUE, mult = "last"]$.lpg
  exact  <- lp[q, on = c(".pid", ".key"), mult = "last"]$.lpg

  n <- nrow(dt)
  n_exact  <- sum(!is.na(exact))
  n_rolled <- sum(!is.na(rolled)) - n_exact
  dt[, lineup_pos_group := rolled]
  if ("pos_group" %in% names(dt)) {
    dt[is.na(lineup_pos_group), lineup_pos_group := pos_group]
  }
  n_none <- sum(is.na(dt$lineup_pos_group))
  cli::cli_inform(
    "Weekly PSR key: {n_exact} named that round, {n_rolled} from an earlier round, {n - n_exact - n_rolled - n_none} career fallback, {n_none} none (of {n})."
  )
  if (n > 0 && n_exact + n_rolled == 0) {
    cli::cli_abort("Weekly PSR key matched no team sheet at all -- the join is broken (check player_id/season/round types).")
  }
  dt[]
}

#' Attach the centring key a `psr_centre` value needs
#'
#' The one place a key column is attached, used by
#' \code{.compute_psr_from_stat_ratings()} and by the pipeline's \code{psr-data}
#' stage (which calls \code{calculate_psr()} directly), so the two cannot pick
#' different keys. \code{"career"} needs nothing; a frame that already carries
#' the key column is returned as-is.
#'
#' @param skills Stat-ratings frame.
#' @param psr_centre One of \code{"career"}, \code{"listed"}, \code{"weekly"}.
#' @param comp Competition, for the listing load.
#' @param season_listing,lineup Optional pre-loaded sources; loaded when NULL.
#' @return \code{skills}, with the key column added when one is needed.
#' @keywords internal
.attach_psr_key <- function(skills, psr_centre = c("career", "listed", "weekly"),
                            comp = "AFLM", season_listing = NULL, lineup = NULL) {
  psr_centre <- match.arg(psr_centre)
  seasons <- unique(data.table::as.data.table(skills)$season)
  if (identical(psr_centre, "listed") && !"listed_pos_group" %in% names(skills)) {
    if (is.null(season_listing)) season_listing <- .load_season_listing(seasons, comp = comp)
    return(.attach_season_listing(skills, season_listing))
  }
  if (identical(psr_centre, "weekly") && !"lineup_pos_group" %in% names(skills)) {
    if (is.null(lineup)) lineup <- .load_weekly_lineup(seasons)
    return(.attach_weekly_pos(skills, lineup))
  }
  skills
}
