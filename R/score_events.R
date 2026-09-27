#' Get Every Scoring Event for AFL Matches
#'
#' Fetches the AFL API's match item (`cfs/afl/matchItem/{match_id}`) for each
#' match and returns its `score.scoreWorm.scoringEvents`: one row per score,
#' with the team, quarter, second, type (`GOAL`, `BEHIND`, `RUSHED_BEHIND`),
#' the scorer, and the running score after it.
#'
#' The chains feed cannot give this on its own. It has no row for most rushed
#' behinds, it can mislabel the chain that ended in one, and it records nothing
#' for a kick that was in flight when the siren went. Reconstructing the score
#' from the chains matched the official final in 1,275 of 1,279 matches
#' (2021-2026); this list is the official score, event by event.
#'
#' The full match item JSON (quarter-by-quarter scores, left / right / poster /
#' rushed / touched behind counts, minutes in front, venue, weather) is kept as
#' the `match_items` attribute, one row per match, so nothing the endpoint
#' returns is thrown away.
#'
#' @param season Season year. Ignored when `match_ids` is given.
#' @param round Round number, or `NA` for every round of the season.
#' @param match_ids Optional character vector of match IDs
#'   (e.g. `"CD_M20260142901"`) to fetch instead of a season / round.
#' @return A data.table of scoring events, snake_case, with `match_id`,
#'   `season`, `round_number` and `event_number` (order within the match)
#'   added, and a `match_items` attribute: a data.table of `match_id`,
#'   `fetched_at` and `match_item_json`.
#' @export
get_match_score_events <- function(season = get_afl_season(), round = NA, match_ids = NULL) {
  if (is.null(match_ids)) {
    games <- if (is.na(round)) {
      # get_season_games() walks rounds 1..last, so the Opening Round
      # (round 0, since 2024) has to be fetched on its own.
      g0 <- tryCatch(get_round_games(season, 0), error = function(e) data.frame())
      g <- get_season_games(season)
      if (nrow(g0) > 0) g <- dplyr::bind_rows(g0[, intersect(names(g0), names(g)), drop = FALSE], g)
      g
    } else {
      get_round_games(season, round)
    }
    if (nrow(games) == 0) {
      cli::cli_inform("No concluded matches for {season} round {round}")
      return(data.table::data.table())
    }
    match_ids <- games$matchId
  }
  match_ids <- unique(match_ids)

  json <- .fetch_match_items(match_ids)

  events <- data.table::rbindlist(lapply(names(json), function(id) {
    parsed <- jsonlite::fromJSON(json[[id]], flatten = TRUE)
    se <- parsed$score$scoreWorm$scoringEvents
    home <- parsed$score$homeTeamScore$matchScore$totalScore
    away <- parsed$score$awayTeamScore$matchScore$totalScore
    status <- parsed$match$status %||% NA_character_
    if (is.null(se) || NROW(se) == 0) {
      # A played match with a score and no scoring events is a broken
      # response, not a 0-0 game.
      if (identical(status, "CONCLUDED") && isTRUE((home %||% 0) + (away %||% 0) > 0)) {
        cli::cli_abort("matchItem for {.val {id}} has a final score ({home}-{away}) but no scoring events")
      }
      return(NULL)
    }
    se <- data.table::as.data.table(se)
    # The last event's running score must be the final score.
    last <- se[nrow(se)]
    if (identical(status, "CONCLUDED") &&
        (!isTRUE(last$aggregateHomeScore == home) || !isTRUE(last$aggregateAwayScore == away))) {
      cli::cli_abort("matchItem for {.val {id}}: scoring events end at {last$aggregateHomeScore}-{last$aggregateAwayScore} but the final score is {home}-{away}")
    }
    se[, `:=`(match_id = id, event_number = seq_len(.N), match_status = status)]
    se
  }), use.names = TRUE, fill = TRUE)

  if (nrow(events) > 0) {
    .bulk_snake_case(events, verbose = FALSE)
    events[, `:=`(season = as.integer(substr(match_id, 5, 8)),
                  round_number = .extract_round_from_match_id(match_id))]
    data.table::setcolorder(events, c("match_id", "season", "round_number", "event_number"))
  }

  data.table::setattr(events, "match_items", data.table::data.table(
    match_id = names(json),
    fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    match_item_json = unname(unlist(json))
  ))
  cli::cli_inform("Fetched {nrow(events)} scoring events from {length(json)} match{?es}")
  events
}

#' Fetch AFL API match items in parallel
#'
#' @param match_ids Character vector of match IDs.
#' @return Named list of raw JSON strings, in `match_ids` order.
#' @keywords internal
.fetch_match_items <- function(match_ids) {
  token <- get_token()
  urls <- paste0(AFL_CFS_API_BASE_URL, "matchItem/", match_ids)
  out <- stats::setNames(vector("list", length(urls)), match_ids)

  pool <- curl::new_pool(total_con = 20L, host_con = 10L)
  for (i in seq_along(urls)) {
    local({
      idx <- i
      h <- curl::new_handle(httpheader = paste0("x-media-mis-token: ", token))
      curl::curl_fetch_multi(urls[idx], done = function(resp) {
        if (resp$status_code == 200L) out[[idx]] <<- rawToChar(resp$content)
      }, fail = function(msg) NULL, handle = h, pool = pool)
    })
  }
  curl::multi_run(pool = pool)

  # Anything the parallel pass missed gets one sequential retry through
  # access_api() (fresh token, its own retry/backoff). A match that still
  # fails stops the fetch: a silently missing match would read as a match
  # with no scores.
  missing <- which(vapply(out, is.null, logical(1)))
  for (idx in missing) {
    res <- tryCatch(access_api(urls[idx]), error = function(e) e)
    if (inherits(res, "error")) {
      cli::cli_abort("Could not fetch matchItem for {.val {match_ids[idx]}}: {conditionMessage(res)}")
    }
    out[[idx]] <- as.character(jsonlite::toJSON(res, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
  }
  out
}
