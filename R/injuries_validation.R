# AFL Injury Data: Listing-Accuracy Validation
# ============================================
# Diagnostic helpers for measuring how accurate our injury listings are over
# time: how often "test" / "TBC" players actually play, how accurate the
# estimated return rounds are, and survival of TBC listings. Added 2026-04-24
# (see NEWS.md); calibrate after R10+ when there's enough history.



# Internal context builder: loads injury history + round-start timestamps +
# played-player lookup keys used by the listing-accuracy helpers
# (test_played_rate, tbc_played_rate, injury_return_accuracy,
# tbc_return_survival). Returns NULL on any missing dependency (pre-tracking
# history, no fixtures, no teams) so callers can short-circuit cleanly.
.injury_accuracy_context <- function(season, round = NULL) {
  inj_hist <- tryCatch(load_injury_data(season), error = function(e) NULL)
  if (is.null(inj_hist) || nrow(inj_hist) == 0) {
    cli::cli_alert_info("No injury history for {season} yet")
    return(NULL)
  }
  if (!"scraped_at" %in% names(inj_hist)) {
    cli::cli_alert_info("Injury history for {season} predates scraped_at tracking -- waiting on fresh snapshots")
    return(NULL)
  }

  fixtures <- load_fixtures(all = TRUE) |>
    dplyr::filter(season == .env$season)
  teams <- load_teams(season)
  if (nrow(teams) == 0) {
    cli::cli_alert_info("No lineup data for {season}")
    return(NULL)
  }

  round_starts <- fixtures |>
    dplyr::mutate(
      utc_dt = as.POSIXct(utc_start_time, format = "%Y-%m-%dT%H:%M", tz = "UTC")
    ) |>
    dplyr::group_by(round_number) |>
    dplyr::summarise(round_start = min(utc_dt, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(round_start < Sys.time())

  if (!is.null(round)) {
    round_starts <- dplyr::filter(round_starts, round_number %in% round)
  }
  if (nrow(round_starts) == 0) {
    cli::cli_alert_info("No completed rounds to analyse")
    return(NULL)
  }

  played_df <- teams |>
    dplyr::filter(!lineup_position %in% c("EMERG", "SUB") | is.na(lineup_position)) |>
    dplyr::mutate(player_norm = norm_name(paste(given_name, surname))) |>
    dplyr::transmute(round = as.integer(.data$round_number),
                     player_norm = .data$player_norm) |>
    dplyr::distinct()

  played_keys <- paste(played_df$round, played_df$player_norm)

  list(
    inj_hist = inj_hist,
    round_starts = round_starts,
    played_df = played_df,
    played_keys = played_keys
  )
}


# Internal: for each round with a fixture start, collect the latest
# pre-kickoff weekly listing per (player_norm, team) whose estimated_return
# matches `return_filter` (a predicate on the lowercased string). Returns a
# tibble with `round_number` (evaluation round), `scrape_round` (round the
# listing was published for -- first upcoming round after scraped_at),
# `player`, `team`, `injury`, `estimated_return`, `player_norm`,
# `scraped_at`, `played`.
.latest_listings_per_round <- function(ctx, return_filter) {
  rs_sorted <- ctx$round_starts |> dplyr::arrange(.data$round_start)

  listings <- purrr::map_dfr(seq_len(nrow(ctx$round_starts)), function(i) {
    r  <- ctx$round_starts$round_number[i]
    rs <- ctx$round_starts$round_start[i]
    ctx$inj_hist |>
      dplyr::filter(.data$source == "weekly",
                    .data$scraped_at < rs,
                    return_filter(tolower(trimws(.data$estimated_return)))) |>
      dplyr::arrange(.data$player_norm, .data$scraped_at) |>
      dplyr::group_by(.data$player_norm) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup() |>
      dplyr::mutate(round_number = r)
  })

  if (nrow(listings) == 0) return(listings)

  # scrape_round = first upcoming round whose start is after scraped_at.
  # parse_return_round() needs this (not the evaluation round), otherwise
  # a "1-2 weeks" listing keeps sliding forward each round we evaluate.
  listings$scrape_round <- vapply(listings$scraped_at, function(ts) {
    idx <- which(rs_sorted$round_start > ts)
    if (length(idx) == 0) NA_integer_ else as.integer(rs_sorted$round_number[idx[1]])
  }, integer(1))

  listings |>
    dplyr::mutate(played = paste(.data$round_number, .data$player_norm) %in% ctx$played_keys)
}


#' TEST Listing Played-Rate
#'
#' For each completed round, finds players listed as "Test" on the weekly
#' injury list at the latest scrape before the round's first match, and
#' checks whether they were named in the selected 22 (non-EMERG/SUB). Useful
#' for validating whether the model's TEST-as-TBC assumption is too harsh.
#'
#' Requires accumulated injury snapshots from [save_injury_data()] -- returns
#' an empty tibble if the release predates `scraped_at` tracking or if no
#' TEST listings have been captured yet.
#'
#' @param season Season year. Defaults to current via [get_afl_season()].
#' @param round Optional round number (or vector) to filter to. If `NULL`,
#'   analyses all rounds with injury history and lineup data available.
#' @return A tibble with one row per TEST listing: `round`, `player`,
#'   `team`, `injury`, `scraped_at`, `played` (logical).
#' @export
test_played_rate <- function(season = NULL, round = NULL) {
  if (is.null(season)) season <- get_afl_season()

  ctx <- .injury_accuracy_context(season, round)
  if (is.null(ctx)) return(tibble::tibble())

  tests <- .latest_listings_per_round(ctx, function(x) x == "test")

  if (nrow(tests) == 0) {
    cli::cli_alert_info("No TEST listings captured yet -- give it a few rounds of scrapes")
    return(tibble::tibble())
  }

  tests |>
    dplyr::transmute(round = .data$round_number, .data$player, .data$team, .data$injury,
                     .data$scraped_at, .data$played) |>
    dplyr::arrange(.data$round, .data$team, .data$player)
}


#' TBC Listing Played-Rate
#'
#' Per-round analogue of [test_played_rate()] for players listed as "TBC"
#' (estimated return unknown). For each completed round, finds players whose
#' latest pre-kickoff weekly listing says "TBC" and checks whether they were
#' named in the selected 22. The overall played fraction here directly
#' calibrates the `current_round + 3` fallback used by [parse_return_round()]
#' for TBC entries.
#'
#' Also returns a `summary` attribute with the overall and per-round played
#' rate -- useful for dashboards and for feeding back into simulation
#' defaults.
#'
#' @param season Season year. Defaults to current via [get_afl_season()].
#' @param round Optional round number (or vector) to filter to.
#' @return A tibble with one row per TBC listing: `round`, `player`, `team`,
#'   `injury`, `scraped_at`, `played` (logical). Attached `summary` attribute
#'   is a list with elements:
#'   * `n_listings` — total listing-round rows
#'   * `overall_played_pct` — fraction of listings where the player played
#'   * `per_round` — tibble of `round`, `n`, `played`, `played_pct`
#' @export
tbc_played_rate <- function(season = NULL, round = NULL) {
  if (is.null(season)) season <- get_afl_season()

  ctx <- .injury_accuracy_context(season, round)
  if (is.null(ctx)) return(tibble::tibble())

  tbcs <- .latest_listings_per_round(ctx, function(x) x == "tbc")

  if (nrow(tbcs) == 0) {
    cli::cli_alert_info("No TBC listings captured yet -- give it a few rounds of scrapes")
    return(tibble::tibble())
  }

  out <- tbcs |>
    dplyr::transmute(round = .data$round_number, .data$player, .data$team, .data$injury,
                     .data$scraped_at, .data$played) |>
    dplyr::arrange(.data$round, .data$team, .data$player)

  per_round <- out |>
    dplyr::group_by(.data$round) |>
    dplyr::summarise(n = dplyr::n(),
                     played = sum(.data$played),
                     played_pct = mean(.data$played),
                     .groups = "drop")

  attr(out, "summary") <- list(
    n_listings = nrow(out),
    overall_played_pct = mean(out$played),
    per_round = per_round
  )
  out
}


#' Injury-Return Accuracy (Predicted vs Actual Rounds Out)
#'
#' For every round with injury-history coverage, take the latest pre-kickoff
#' weekly listing for each player, compute the predicted return round via
#' [parse_return_round()], and compare to whether the player actually played
#' that round. Produces a listing-level tibble plus a per-band summary
#' (`attr(x, "by_band")`) showing calibration of each `estimated_return` band
#' (e.g. "1-2 weeks", "TBC", "Test").
#'
#' Interpretation of `played_pct` varies by the sign of
#' `predicted_rounds_out`:
#' * Among rows with `predicted_rounds_out > 0` (model says "still out"),
#'   `played_pct` is a **false-positive rate** — fraction who played despite
#'   being flagged out.
#' * Among rows with `predicted_rounds_out <= 0` (model says "back this
#'   round"), `played_pct` is a **true-positive rate** — recall of
#'   predicted-back listings.
#'
#' @param season Season year. Defaults to current via [get_afl_season()].
#' @param round Optional round number (or vector) to filter to.
#' @return A tibble with one row per listing-round:
#'   `round`, `player`, `team`, `injury`, `estimated_return`,
#'   `predicted_return_round` (may be `Inf` = out for season),
#'   `predicted_rounds_out` (predicted - round, `Inf` for season-out),
#'   `played` (actual appearance in selected 22), `scraped_at`.
#'   Attached `by_band` attribute is a tibble of calibration stats per
#'   `estimated_return` value, with columns:
#'   * `band` — lowercased trimmed estimate string (e.g. `"1-2 weeks"`,
#'     `"tbc"`, `"test"`)
#'   * `n` — count of listing-rounds in this band
#'   * `mean_predicted_out` — mean `predicted_rounds_out` (finite only)
#'   * `played_pct` — fraction where the player actually played
#' @export
injury_return_accuracy <- function(season = NULL, round = NULL) {
  if (is.null(season)) season <- get_afl_season()

  ctx <- .injury_accuracy_context(season, round)
  if (is.null(ctx)) return(tibble::tibble())

  # All weekly listings (any estimated_return that isn't NA/empty/none)
  listings <- .latest_listings_per_round(ctx, function(x) {
    !is.na(x) & nzchar(x) & x != "none"
  })

  if (nrow(listings) == 0) {
    cli::cli_alert_info("No weekly listings captured yet -- give it a few rounds of scrapes")
    return(tibble::tibble())
  }

  listings$predicted_return_round <- purrr::map_dbl(
    seq_len(nrow(listings)), function(i) {
      parse_return_round(listings$estimated_return[i], season,
                         current_round = listings$scrape_round[i])
    }
  )

  out <- listings |>
    dplyr::mutate(
      predicted_rounds_out = .data$predicted_return_round - .data$round_number
    ) |>
    dplyr::transmute(
      round = .data$round_number, .data$player, .data$team, .data$injury,
      .data$estimated_return, .data$predicted_return_round,
      .data$predicted_rounds_out, .data$played, .data$scraped_at
    ) |>
    dplyr::arrange(.data$round, .data$team, .data$player)

  by_band <- out |>
    dplyr::mutate(band = tolower(trimws(.data$estimated_return))) |>
    dplyr::group_by(.data$band) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_predicted_out = mean(.data$predicted_rounds_out[is.finite(.data$predicted_rounds_out)]),
      played_pct = mean(.data$played),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$n))

  attr(out, "by_band") <- by_band
  out
}


#' Empirical Return Distribution for TBC Listings
#'
#' For every weekly TBC listing in the season's injury history, measures
#' rounds-until-first-senior-game after the listing's scrape. Returns a
#' tibble of listing-level observations plus an `attr(x, "cdf")` with the
#' empirical P(returned by +N rounds) and median rounds-to-return (NA if
#' more than half are still censored).
#'
#' Players who have not yet returned are right-censored: their observed
#' rounds-out is a lower bound. The CDF uses a simple observed / total
#' estimator -- it under-estimates the true return probability at higher
#' offsets (because some censored players will eventually return), so treat
#' it as a conservative floor.
#'
#' Used to audit the `parse_return_round("TBC")` default of
#' `current_round + 3` -- if the median actual return is 1-2 rounds, the
#' simulation is treating TBC as more severe than the data supports.
#'
#' @param season Season year. Defaults to current via [get_afl_season()].
#' @return A tibble with one row per TBC listing episode:
#'   `player`, `team`, `injury`, `first_tbc_round` (round at scrape),
#'   `return_round` (first lineup appearance, `NA` if censored),
#'   `rounds_out` (observed, with censoring flag), `censored` (logical).
#'   Attached attributes:
#'   * `cdf` — tibble of `offset`, `returned`, `total`, `p_returned`
#'     (cumulative empirical P(returned by +offset rounds))
#'   * `median_rounds_out` — median of observed (non-censored) returns,
#'     `NA_real_` when fewer than half of episodes have returned
#'   * `n_returned` — count of episodes where the player has returned
#'   * `n_censored` — count of episodes still out at `last_completed`
#' @export
tbc_return_survival <- function(season = NULL) {
  if (is.null(season)) season <- get_afl_season()

  ctx <- .injury_accuracy_context(season)
  if (is.null(ctx)) return(tibble::tibble())

  tbc_hist <- ctx$inj_hist |>
    dplyr::filter(.data$source == "weekly",
                  tolower(trimws(.data$estimated_return)) == "tbc") |>
    dplyr::arrange(.data$player_norm, .data$team, .data$scraped_at)

  if (nrow(tbc_hist) == 0) {
    cli::cli_alert_info("No TBC listings captured yet -- give it a few rounds of scrapes")
    return(tibble::tibble())
  }

  # For each (player_norm, team) keep the first TBC scrape -- that's the
  # start of the TBC episode. Later scrapes with the same state were already
  # collapsed by save_injury_data(), but we guard anyway.
  tbc_first <- tbc_hist |>
    dplyr::group_by(.data$player_norm, .data$team) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup()

  # Tag each episode with its scrape round (first round whose kickoff is
  # after scraped_at; if scrape precedes round 1, use round 1).
  rs <- ctx$round_starts |> dplyr::arrange(.data$round_start)
  tbc_first$first_tbc_round <- vapply(tbc_first$scraped_at, function(ts) {
    idx <- which(rs$round_start > ts)
    if (length(idx) == 0) NA_integer_ else as.integer(rs$round_number[idx[1]])
  }, integer(1))

  completed_rounds <- rs$round_number
  last_completed <- if (length(completed_rounds) > 0) max(completed_rounds) else NA_integer_

  played_df <- ctx$played_df

  # For each episode, find first played round >= first_tbc_round
  out <- tbc_first |>
    dplyr::mutate(
      return_round = purrr::map2_int(.data$player_norm, .data$first_tbc_round, function(pn, r0) {
        if (is.na(r0)) return(NA_integer_)
        hits <- played_df$round[played_df$player_norm == pn & played_df$round >= r0]
        if (length(hits) == 0) NA_integer_ else min(hits)
      }),
      censored = is.na(.data$return_round),
      rounds_out = dplyr::if_else(
        .data$censored,
        as.integer(last_completed) - .data$first_tbc_round,
        .data$return_round - .data$first_tbc_round
      )
    ) |>
    dplyr::transmute(
      .data$player, .data$team, .data$injury, .data$first_tbc_round,
      .data$return_round, .data$rounds_out, .data$censored
    ) |>
    dplyr::filter(!is.na(.data$first_tbc_round))

  # Empirical CDF: at offset k, fraction of all episodes whose observed
  # rounds_out <= k AND NOT censored. Conservative (under-counts at high k).
  max_off <- suppressWarnings(max(out$rounds_out, na.rm = TRUE))
  if (!is.finite(max_off)) max_off <- 0L
  offsets <- seq.int(0L, max(1L, as.integer(max_off)))
  total <- nrow(out)
  returned_by <- vapply(offsets, function(k) {
    sum(!out$censored & out$rounds_out <= k)
  }, integer(1))
  p_returned <- if (total == 0) {
    rep(NA_real_, length(offsets))
  } else {
    returned_by / total
  }
  cdf <- tibble::tibble(
    offset = offsets,
    returned = returned_by,
    total = total,
    p_returned = p_returned
  )

  # Median rounds-to-return (NA if fewer than half have returned)
  returned_out <- out$rounds_out[!out$censored]
  median_out <- if (length(returned_out) >= max(1L, total / 2)) {
    stats::median(returned_out)
  } else {
    NA_real_
  }

  attr(out, "cdf") <- cdf
  attr(out, "median_rounds_out") <- median_out
  attr(out, "n_returned") <- length(returned_out)
  attr(out, "n_censored") <- sum(out$censored)
  out
}
