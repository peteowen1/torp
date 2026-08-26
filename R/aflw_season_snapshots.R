# AFLW season-stat snapshots and round-level differencing ----
#
# `statspro/playersStats/seasons/{providerId}` returns a player's SEASON-TO-DATE
# cumulative totals and nothing finer -- confirmed against the site's own client
# source that no as-at-date/round parameter exists (AFL-API-REFERENCE.md,
# "Endpoint family: statspro"). The 28 extended fields it carries (spoils,
# pressure_acts, ...) are genuinely empty in CFS's per-match `extendedStats`
# block for AFLW, so there is no match-grain source for them at all.
#
# The way round-level figures become recoverable is by SNAPSHOTTING that
# cumulative table on a schedule and differencing consecutive snapshots: this
# week's cumulative minus last week's cumulative is what the games in between
# contributed. That only ever works FORWARD from the first snapshot -- rounds
# played before snapshotting began are permanently season-total-only, and no
# amount of later scraping recovers them.

#' Release tag holding the dated AFLW season-stat snapshots
#'
#' Deliberately separate from `aflw_season_stats-data` (the one-file-per-season
#' current totals): snapshots accumulate one file per scrape date and would
#' otherwise bury the canonical per-season file. Defined here rather than in
#' `constants_afl.R` to keep this feature's merge surface to a single new file.
#' @keywords internal
AFLW_SNAPSHOT_RELEASE_TAG <- "aflw_season_stats_snapshots-data"

#' Non-cumulative columns in an AFLW season-stat snapshot
#'
#' Differencing only means something for columns that ACCUMULATE. A rate, a
#' percentage, a per-game average and a rank are all season-to-date summaries:
#' subtracting last week's value from this week's produces a number with no
#' interpretation at all (e.g. `disposal_efficiency` 62.1 - 60.4 = 1.7 is not
#' "1.7% efficiency in the games between"). These are dropped from the delta
#' rather than silently differenced.
#'
#' Per-game averages ARE recoverable, but by recomputation rather than
#' subtraction -- `diff_aflw_season_snapshots(recompute_averages = TRUE)`
#' divides each differenced total by the differenced `games_played`.
#'
#' @format Character vector of exact column names, plus see
#'   `AFLW_SNAPSHOT_RATE_PATTERNS` for the suffix rules.
#' @keywords internal
AFLW_SNAPSHOT_RATE_COLS <- c(
  "time_on_ground_percentage",
  "disposal_efficiency",
  "goal_accuracy",
  "kick_efficiency",
  "kick_to_handball_ratio",
  "contested_possession_rate",
  "contest_def_loss_percentage",
  "contest_off_wins_percentage",
  "hitout_win_percentage",
  "hitout_to_advantage_rate",
  "ranking"
)

#' Suffix patterns marking a non-cumulative snapshot column
#'
#' Applied in addition to `AFLW_SNAPSHOT_RATE_COLS` so that fields added to the
#' endpoint later are excluded by shape rather than needing this file edited.
#' `_avg` covers the parallel per-game block the endpoint returns alongside
#' every total.
#' @keywords internal
AFLW_SNAPSHOT_RATE_PATTERNS <- "(_avg|_percentage|_rate|_efficiency|_accuracy)$"

#' Identity columns carried through a snapshot diff unchanged
#' @keywords internal
AFLW_SNAPSHOT_ID_COLS <- c("player_id", "season", "comp", "team_abbr")

#' Longest snapshot gap still consistent with a single round
#'
#' The weekly capture runs Tuesdays, so a normal pair is 7 days apart. AFLW
#' rounds run Thursday-Monday with 3-8 day gaps between them, so a pair a
#' little over a week apart can still be one round (a public-holiday fixture,
#' a rescheduled match). Past this, the pair has almost certainly swallowed a
#' skipped capture and covers two rounds -- which looks identical in the output
#' unless something says so.
#' @keywords internal
AFLW_SNAPSHOT_MAX_GAP_DAYS <- 10L

#' Most players that can plausibly debut between two weekly snapshots
#'
#' Round-one captures aside, AFLW debutants arrive a handful at a time. A
#' double-digit count between consecutive weekly snapshots is far more likely
#' to mean the earlier capture was partial -- and every "new" player is then
#' credited her whole season-to-date total as one window's delta.
#' @keywords internal
AFLW_SNAPSHOT_MAX_NEW_PLAYERS <- 25L

#' Classify a snapshot's columns into identity / cumulative / non-cumulative
#'
#' @param nms Character vector of column names.
#' @return List with `id`, `cumulative` and `rate` character vectors.
#' @keywords internal
.aflw_snapshot_classify_cols <- function(nms) {
  id <- intersect(AFLW_SNAPSHOT_ID_COLS, nms)
  rest <- setdiff(nms, id)
  rate <- rest[rest %in% AFLW_SNAPSHOT_RATE_COLS |
                 grepl(AFLW_SNAPSHOT_RATE_PATTERNS, rest)]
  cumulative <- setdiff(rest, rate)
  list(id = id, cumulative = cumulative, rate = rate)
}

#' Difference two AFLW season-stat snapshots into a round-window delta
#'
#' Given the same season's cumulative season-to-date totals captured at two
#' different dates, returns what each player accumulated BETWEEN them -- i.e.
#' the contribution of whatever games fell in that window.
#'
#' Pure by design: it takes two data frames and touches no network, so the
#' differencing logic is testable without a release or an API. Use
#' `load_aflw_season_stats_snapshot()` to fetch the frames.
#'
#' @param earlier,later Data frames of the SAME season's snapshot, `earlier`
#'   captured before `later`. Both need `player_id` and the cumulative stat
#'   columns; typically the output of `load_aflw_season_stats_snapshot()`.
#' @param recompute_averages Logical. When `TRUE` (default), adds an `_avg`
#'   column per cumulative stat, computed as the differenced total divided by
#'   the differenced `games_played` -- the per-game rate WITHIN the window.
#'   Players with zero games in the window get `NA` rather than a divide-by-zero.
#' @param drop_unplayed Logical. When `TRUE`, players with no games in the
#'   window are dropped. Default `FALSE`, which keeps them with explicit zero
#'   deltas -- "she played no games" and "she is missing from the data" are
#'   different facts and the caller should be able to tell them apart.
#' @return A data.table: identity columns, `games_played` (games in the window),
#'   one delta column per cumulative stat, optional recomputed `_avg` columns,
#'   and `snapshot_from`/`snapshot_to` marking the window. Non-cumulative source
#'   columns are dropped. Attributes carry what the numbers alone cannot show:
#'   `"rate_cols_dropped"` (excluded as non-cumulative),
#'   `"schema_cols_dropped"` (cumulative but absent from `earlier`, so not
#'   differenceable), `"gap_days"` (elapsed days between snapshots -- a pair
#'   spanning much more than a week has probably swallowed a skipped capture
#'   and covers two rounds), `"players_changed_team"` (traded mid-window, so
#'   their deltas are attributed wholly to the later club),
#'   `"n_players_new_since_earlier"` and `"n_players_unplayed_in_window"`.
#' @export
diff_aflw_season_snapshots <- function(earlier, later,
                                       recompute_averages = TRUE,
                                       drop_unplayed = FALSE) {
  if (is.null(earlier) || is.null(later)) {
    cli::cli_abort("diff_aflw_season_snapshots: both {.arg earlier} and {.arg later} are required.")
  }
  earlier <- data.table::as.data.table(earlier)
  later <- data.table::as.data.table(later)

  if (nrow(later) == 0) {
    cli::cli_abort("diff_aflw_season_snapshots: {.arg later} snapshot has 0 rows -- nothing to difference.")
  }
  for (nm in c("player_id", "games_played")) {
    if (!nm %in% names(later) || !nm %in% names(earlier)) {
      cli::cli_abort("diff_aflw_season_snapshots: both snapshots need a {.val {nm}} column.")
    }
  }

  # Differencing keys on player_id via a merge. A duplicated key on EITHER side
  # produces a cartesian expansion -- every stat for that player silently
  # multiplied, no error, a plausible-looking table. The scrape script guards
  # its own fresh capture, but `earlier` arrives from a previously-published
  # file (or, since this function is exported, from any caller at all), so the
  # check has to live here too.
  for (side in c("earlier", "later")) {
    frame <- if (side == "earlier") earlier else later
    dup <- anyDuplicated(frame$player_id)
    if (dup > 0) {
      cli::cli_abort(paste0(
        "diff_aflw_season_snapshots: {.arg {side}} has duplicate {.val player_id} values ",
        "(first at row {dup}, {.val {frame$player_id[dup]}}). Differencing merges on ",
        "player_id, so duplicates would silently produce a cartesian delta."))
    }
  }

  # Reversed arguments only half-announce themselves: the deltas come out
  # negative, which merely WARNS below, so a reversed pair can still be
  # published. Identical arguments do not announce themselves at all -- every
  # delta is a clean zero and the table looks entirely valid.
  from_date <- attr(earlier, "snapshot_date", exact = TRUE)
  to_date <- attr(later, "snapshot_date", exact = TRUE)
  if (!is.null(from_date) && !is.null(to_date)) {
    if (as.Date(from_date) >= as.Date(to_date)) {
      cli::cli_abort(paste0(
        "diff_aflw_season_snapshots: {.arg earlier} is dated {as.Date(from_date)} and ",
        "{.arg later} {as.Date(to_date)} -- {.arg earlier} must precede {.arg later}. ",
        "Equal dates give an all-zero delta that looks valid; reversed dates give ",
        "negatives that only warn."))
    }
  }

  # A snapshot pair from different seasons is a caller error, not something to
  # difference: cumulative totals restart each season, so the delta would be
  # this season's running total minus an unrelated one.
  if ("season" %in% names(earlier) && "season" %in% names(later)) {
    seasons <- unique(c(earlier$season, later$season))
    if (length(seasons) > 1) {
      cli::cli_abort(paste0(
        "diff_aflw_season_snapshots: snapshots span multiple seasons ({.val {seasons}}). ",
        "Cumulative totals reset each season, so differencing across one is meaningless."))
    }
  }

  cls <- .aflw_snapshot_classify_cols(names(later))
  stat_cols <- intersect(cls$cumulative, names(earlier))
  # A cumulative column present in `later` but not `earlier` -- the endpoint
  # gained a field mid-season -- cannot be differenced (there is no prior value
  # to subtract). It drops out here exactly like a rate column does, but for a
  # completely different reason, so it gets its own attribute: a consumer
  # seeing a column vanish should be able to tell "not differenceable this
  # window" from "deliberately excluded as a rate".
  schema_dropped <- setdiff(cls$cumulative, names(earlier))
  stat_cols <- stat_cols[vapply(stat_cols, function(c) is.numeric(later[[c]]), logical(1))]
  if (length(stat_cols) == 0) {
    cli::cli_abort("diff_aflw_season_snapshots: no cumulative numeric columns common to both snapshots.")
  }
  if (length(schema_dropped) > 0) {
    cli::cli_warn(paste0(
      "diff_aflw_season_snapshots: {length(schema_dropped)} cumulative column{?s} present in ",
      "{.arg later} but absent from {.arg earlier} ({.val {schema_dropped}}) -- no prior value ",
      "to difference against, so {?it is/they are} omitted from this delta. Recorded in the ",
      "{.val schema_cols_dropped} attribute."))
  }

  id_cols <- cls$id
  keep_later <- unique(c("player_id", id_cols, stat_cols))

  # Identity columns are taken from `later` only, so a player traded inside the
  # window has the WHOLE window -- including games for her former club --
  # attributed to her new one. Her own totals stay correct; any team-level
  # aggregation of these deltas would not. Detect it here, while `earlier`'s
  # team_abbr is still in scope, and record the affected players.
  traded <- character(0)
  if ("team_abbr" %in% names(earlier) && "team_abbr" %in% names(later)) {
    tm <- merge(
      later[, c("player_id", "team_abbr"), with = FALSE],
      earlier[, c("player_id", "team_abbr"), with = FALSE],
      by = "player_id", suffixes = c(".later", ".earlier")
    )
    moved <- !is.na(tm$team_abbr.earlier) & !is.na(tm$team_abbr.later) &
      tm$team_abbr.earlier != tm$team_abbr.later
    traded <- as.character(tm$player_id[moved])
    if (length(traded) > 0) {
      cli::cli_warn(paste0(
        "diff_aflw_season_snapshots: {length(traded)} player{?s} changed team_abbr inside this ",
        "window. Their deltas are attributed entirely to the LATER club, so team-level ",
        "aggregation of this delta will misattribute games played for the former one. ",
        "Player ids in the {.val players_changed_team} attribute."))
    }
  }

  prev <- earlier[, unique(c("player_id", stat_cols)), with = FALSE]
  data.table::setnames(prev, stat_cols, paste0(".prev_", stat_cols))

  # all.x: everyone in `later` survives. A player absent from `earlier` is new
  # to the season since that snapshot -- her whole running total accrued inside
  # the window, so the correct prior is 0, not NA (which would propagate and
  # silently drop her from the delta entirely).
  out <- merge(later[, keep_later, with = FALSE], prev, by = "player_id", all.x = TRUE)
  n_new <- sum(is.na(out[[paste0(".prev_", stat_cols[1])]]))
  for (c in stat_cols) {
    pc <- paste0(".prev_", c)
    data.table::set(out, which(is.na(out[[pc]])), pc, 0)
    data.table::set(out, j = c, value = out[[c]] - out[[pc]])
    data.table::set(out, j = pc, value = NULL)
  }

  # Cumulative counts cannot decrease. If one does, the upstream table was
  # revised between snapshots (a stat correction) -- surface it rather than
  # clamping it away, because a silently-clamped negative is a wrong round-level
  # figure that looks fine.
  neg <- vapply(stat_cols, function(c) sum(out[[c]] < 0, na.rm = TRUE), integer(1))
  if (any(neg > 0)) {
    worst <- names(sort(neg[neg > 0], decreasing = TRUE))[1]
    cli::cli_warn(paste0(
      "diff_aflw_season_snapshots: {sum(neg)} negative delta{?s} across ",
      "{sum(neg > 0)} column{?s} (worst: {.val {worst}}, {max(neg)} row{?s}). ",
      "Cumulative totals should never fall -- the source table was probably ",
      "revised between snapshots. Left unclamped deliberately; inspect before trusting."))
  }

  # A player who did not play still belongs in the output: her delta is a
  # genuine, informative zero. Only `drop_unplayed` removes her.
  unplayed <- out$games_played == 0
  if (isTRUE(drop_unplayed)) {
    out <- out[!unplayed]
  }

  if (isTRUE(recompute_averages)) {
    for (c in setdiff(stat_cols, "games_played")) {
      avg <- data.table::fifelse(out$games_played > 0, out[[c]] / out$games_played, NA_real_)
      data.table::set(out, j = paste0(c, "_avg"), value = avg)
    }
  }

  out[, `:=`(
    snapshot_from = if (is.null(from_date)) NA_character_ else as.character(from_date),
    snapshot_to = if (is.null(to_date)) NA_character_ else as.character(to_date)
  )]

  # The whole design assumes one snapshot pair spans one round. Nothing
  # enforces that: the cron can be skipped (a script error, or GitHub simply
  # not firing a `schedule` trigger, which is a known and unannounced Actions
  # behaviour), and the next pair then silently covers TWO rounds while looking
  # identical in shape to every other week's file. Surface the gap so a
  # consumer can check it, and say so loudly when it is out of range.
  gap_days <- NA_integer_
  if (!is.null(from_date) && !is.null(to_date)) {
    gap_days <- as.integer(as.Date(to_date) - as.Date(from_date))
    if (gap_days > AFLW_SNAPSHOT_MAX_GAP_DAYS) {
      cli::cli_warn(paste0(
        "diff_aflw_season_snapshots: {gap_days} days between snapshots ",
        "({as.Date(from_date)} to {as.Date(to_date)}), beyond the ",
        "{AFLW_SNAPSHOT_MAX_GAP_DAYS}-day single-round window. This delta probably covers ",
        "MORE THAN ONE ROUND -- a skipped weekly capture looks exactly like this. Treating ",
        "it as one round's contribution would overstate every figure in it."))
    }
  }

  # A player in `later` but not `earlier` is credited her entire season-to-date
  # total as this window's delta -- correct for a genuine debutant, badly wrong
  # if she was simply missing from the earlier capture. A partial upstream
  # fetch is realistic (get_afl_player_season_stats() isolates errors per
  # provider id precisely so one failure does not zero the rest), and the
  # per-player inflation is invisible in the output. Debutant counts are small
  # and lumpy; a large one mid-season means the earlier snapshot was short.
  if (n_new > AFLW_SNAPSHOT_MAX_NEW_PLAYERS) {
    cli::cli_warn(paste0(
      "diff_aflw_season_snapshots: {n_new} players appear in {.arg later} but not ",
      "{.arg earlier}, above the plausible-debutant threshold of ",
      "{AFLW_SNAPSHOT_MAX_NEW_PLAYERS}. Each is credited her FULL season-to-date total as ",
      "this window's delta. If the earlier snapshot was a partial capture rather than these ",
      "being real debutants, every one of those figures is inflated. Verify before trusting."))
  }

  data.table::setattr(out, "rate_cols_dropped", cls$rate)
  data.table::setattr(out, "schema_cols_dropped", schema_dropped)
  data.table::setattr(out, "players_changed_team", traded)
  data.table::setattr(out, "gap_days", gap_days)
  data.table::setattr(out, "n_players_new_since_earlier", n_new)
  data.table::setattr(out, "n_players_unplayed_in_window", sum(unplayed))
  out[]
}

#' List the AFLW season-stat snapshot dates available on the release
#'
#' @param season Integer season, or `NULL` for every season present.
#' @return A data.table of `season` and `snapshot_date`, ordered oldest first;
#'   zero rows if the release has no snapshots yet.
#' @export
list_aflw_season_stat_snapshots <- function(season = NULL) {
  assets <- get_release_assets(AFLW_SNAPSHOT_RELEASE_TAG)
  empty <- data.table::data.table(season = integer(0), snapshot_date = as.Date(character(0)))
  if (is.null(assets) || length(assets) == 0) {
    return(empty)
  }
  rx <- "^aflw_season_stats_(\\d{4})_asof_(\\d{4}-\\d{2}-\\d{2})\\.parquet$"
  hits <- assets[grepl(rx, assets)]
  if (length(hits) == 0) {
    return(empty)
  }
  out <- data.table::data.table(
    season = as.integer(sub(rx, "\\1", hits)),
    snapshot_date = as.Date(sub(rx, "\\2", hits))
  )
  if (!is.null(season)) {
    # Captured into a differently-named local first: inside `[`, data.table
    # resolves `season` to the COLUMN, not this argument, so filtering on the
    # bare name silently matches every row instead of the requested season.
    want_seasons <- as.integer(season)
    out <- out[out$season %in% want_seasons]
  }
  data.table::setorder(out, season, snapshot_date)
  out[]
}

#' Load one dated AFLW season-stat snapshot
#'
#' @param season Integer season.
#' @param as_of Snapshot date (`Date` or `"YYYY-MM-DD"`). `NULL` (default)
#'   takes the most recent snapshot available for that season.
#' @param use_disk_cache Passed to `load_from_url()`.
#' @return A data.table with a `"snapshot_date"` attribute, so a frame carries
#'   its own as-of date into `diff_aflw_season_snapshots()`.
#' @export
load_aflw_season_stats_snapshot <- function(season, as_of = NULL, use_disk_cache = FALSE) {
  season <- as.integer(season)
  if (length(season) != 1 || is.na(season)) {
    cli::cli_abort("load_aflw_season_stats_snapshot: {.arg season} must be a single year.")
  }
  if (is.null(as_of)) {
    avail <- list_aflw_season_stat_snapshots(season)
    if (nrow(avail) == 0) {
      cli::cli_abort(paste0(
        "load_aflw_season_stats_snapshot: no snapshots published for season {season}. ",
        "Snapshots only exist from when the weekly scrape started -- see ",
        "data-raw/01-data/scrape_aflw_season_stats_snapshot.R."))
    }
    as_of <- max(avail$snapshot_date)
  }
  as_of <- as.Date(as_of)
  url <- paste0(
    "https://github.com/", get_torp_data_repo(), "/releases/download/",
    AFLW_SNAPSHOT_RELEASE_TAG, "/",
    .aflw_snapshot_file_name(season, as_of), ".parquet"
  )
  out <- load_from_url(url, use_disk_cache = use_disk_cache)
  data.table::setattr(out, "snapshot_date", as_of)
  out
}

#' Snapshot file name (without extension) for a season and as-of date
#' @keywords internal
.aflw_snapshot_file_name <- function(season, as_of) {
  paste0("aflw_season_stats_", as.integer(season), "_asof_", format(as.Date(as_of), "%Y-%m-%d"))
}
