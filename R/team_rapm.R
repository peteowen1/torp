# Team RAPM (Regularized Adjusted Plus-Minus) -- shared AFLM/AFLW engine
# =========================================================================
# One comp-parameterized engine, not two near-duplicate files. Built
# 2026-08-24/25: AFLW's box-score RAPM was new territory (no chain data to
# build a splint-based RAPM from); AFLM's was a productionisation of an
# already-validated ad-hoc script (data-raw/04-analysis/rapm_general.R,
# 2026-07-27, docs/plans/FABLE-DEFENDER-VALUE-PLAN.md sec7.20-7.22: margin
# RAPM OOS R^2=0.2561, coefficient split-half reliability 0.784-0.803 --
# "the most stable rating the program has produced"). Consolidated into one
# file because the CORE fitting machinery -- ridge/glmnet CV, column-pruning/
# replacement-level pooling, sign handling, split-half reliability,
# co-appearance diagnostic -- is identical between comps; only the DATA
# SOURCE and POSITION TAXONOMY genuinely differ, and both are handled by a
# `comp` argument rather than two copies of everything else.
#
# WHERE COMPS DIFFER (kept, not papered over):
#   - Data source: AFLM's TOG/participation comes from load_player_game_data()
#     (chain-derived, lineup-correct by construction, no bench/emergency
#     rows). AFLW has no chain data at all (verified live, every season
#     2018-2026) -- TOG/participation instead comes from
#     load_player_stats(comp="AFLW"), which DOES carry bench/emergency rows,
#     hence AFLW's position taxonomy needs an INT (interchange) bucket that
#     AFLM's doesn't.
#   - Position taxonomy: AFLM's load_player_game_data() carries a pre-built
#     6-level position_group (KEY_DEFENDER/MEDIUM_DEFENDER/MIDFIELDER/
#     KEY_FORWARD/MEDIUM_FORWARD/RUCK) -> DEF/MID/FWD/RUCK. AFLW has no
#     pre-built position_group, so this buckets directly from player_stats'
#     raw 18-slot lineup code -> DEF/MID/FWD/INT.
#   - Pruning unit: AFLM uses a GAMES-count threshold (rapm_general.R's
#     already-validated MIN_GAMES=10 -- games are the natural, proven lever
#     given men's data volume). AFLW uses a TOG-MINUTES threshold (a fresh
#     empirical sweep found 900 optimal -- AFLW's shorter history means a
#     games-count threshold behaves very differently in relative terms, so
#     the unit was NOT carried over from men's without re-checking). Both
#     units are supported by one function (`unit=`), not two.
#
# THE FIX A STATIC FULL-HISTORY FIT MISSES (found and retracted 2026-08-25):
# fitting RAPM once on ALL history and using the result as a match-prediction
# feature is leak-shaped -- a naive gate test produced an implausible SHIP
# result (dMAE -2.89, ~10x any real candidate this repo has ever gated) that
# turned out to be exactly that. build_team_rapm_expanding()/
# build_team_spm_expanding() (this file / team_spm.R) are the leak-safe
# point-in-time path: an expanding-window season-block refit, mirroring how
# build_team_elo()'s elo_pre and the per-round stat-ratings pipeline are
# already leak-safe by construction. The static full-history functions below
# (build_team_rapm_net/split with seasons=TRUE) are still useful for "what's
# the best CURRENT rating" -- just never for a prediction feature.
#
# Do not brand any of this "TORP for AFLW"/"TORP for AFLM" -- box-score RAPM
# is a different, noisier instrument than torp's chain-native EPV/EPR/PSR/
# TORP, built specifically to test whether it adds anything ON TOP of them,
# not to be presented as comparable to or a replacement for them.

# .team_rapm_position_bucket ----

#' Bucket a player's position into 4 coarse groups for RAPM's
#' replacement-level pooling -- comp-dispatched, see file header for why the
#' bucket SETS genuinely differ (AFLM: DEF/MID/FWD/RUCK; AFLW: DEF/MID/FWD/INT).
#'
#' @param position Character vector. For \code{comp="AFLM"}: 6-level
#'   \code{position_group} from \code{load_player_game_data()}. For
#'   \code{comp="AFLW"}: raw 18-slot lineup code from
#'   \code{load_player_stats(comp="AFLW")}.
#' @param comp "AFLM" (default) or "AFLW".
#' @return Character vector of bucket labels.
#' @keywords internal
.team_rapm_position_bucket <- function(position, comp = "AFLM") {
  .validate_afl_comp(comp)
  if (comp == "AFLM") {
    dplyr::case_when(
      position %in% c("KEY_DEFENDER", "MEDIUM_DEFENDER") ~ "DEF",
      position == "MIDFIELDER" ~ "MID",
      position %in% c("KEY_FORWARD", "MEDIUM_FORWARD") ~ "FWD",
      position == "RUCK" ~ "RUCK",
      TRUE ~ NA_character_
    )
  } else {
    def <- c("FB", "BPL", "BPR", "HBFL", "HBFR", "CHB")
    mid <- c("C", "WL", "WR", "R", "RR", "RK")
    fwd <- c("FF", "FPR", "HFFL", "HFFR", "CHF")
    dplyr::case_when(
      position %in% def ~ "DEF",
      position %in% mid ~ "MID",
      position %in% fwd ~ "FWD",
      TRUE ~ "INT"
    )
  }
}

# .prepare_team_rapm_player_rows ----

#' Build the long player-match-appearance table RAPM's design matrices are
#' built from -- comp-dispatched data source, identical output shape either
#' way.
#'
#' @param seasons Numeric vector of seasons, or \code{TRUE} for all available
#'   seasons.
#' @param comp "AFLM" (default) or "AFLW".
#' @return data.table, one row per player-match appearance: match_id,
#'   player_id, player_name, team_id, position_bucket, tog (0-1 fraction),
#'   is_home, home_margin, team_score, opp_score, season.
#' @keywords internal
.prepare_team_rapm_player_rows <- function(seasons = TRUE, comp = "AFLM") {
  .validate_afl_comp(comp)

  if (comp == "AFLM") {
    pgd <- load_player_game_data(seasons)
    res <- load_results(seasons)
    if (nrow(pgd) == 0 || nrow(res) == 0) return(data.table::data.table())

    ps_dt <- data.table::as.data.table(pgd)
    ps_dt <- ps_dt[!is.na(team_id) & !is.na(player_id)]

    n_na_position <- sum(is.na(ps_dt$position_group))
    if (n_na_position > 0) {
      cli::cli_warn("{n_na_position} row{?s} have NA position_group -- excluded from position bucketing (no INT-style default here, player_game_data has no ambiguous/bench rows to default toward)")
    }

    res_dt <- data.table::as.data.table(res)[
      !is.na(home_score) & !is.na(away_score) & !is.na(home_team_id) & !is.na(away_team_id),
      .(match_id, home_team_id, away_team_id, home_score, away_score)
    ]

    n_ps_matches <- length(unique(ps_dt$match_id))
    rows <- merge(ps_dt, res_dt, by = "match_id")
    n_matched <- length(unique(rows$match_id))
    if (n_matched < n_ps_matches) {
      cli::cli_warn("{n_ps_matches - n_matched} of {n_ps_matches} match_id{?s} in player_game_data had no matching result and were dropped")
    }

    bad_team <- rows[team_id != home_team_id & team_id != away_team_id]
    if (nrow(bad_team) > 0) {
      cli::cli_abort("{nrow(bad_team)} row{?s} have a team_id matching neither home_team_id nor away_team_id -- cannot resolve is_home")
    }

    tog_col <- if ("time_on_ground_percentage" %in% names(rows)) {
      pmin(pmax(rows$time_on_ground_percentage / 100, 0), 1)
    } else {
      cli::cli_warn("No time_on_ground_percentage column -- tog exposure weighting unavailable, defaulting to 1 (binary-equivalent) for all rows")
      rep(1, nrow(rows))
    }

    rows[, `:=`(
      is_home         = team_id == home_team_id,
      position_bucket = .team_rapm_position_bucket(position_group, comp = "AFLM"),
      tog             = tog_col,
      season          = season
    )]
  } else {
    ps <- load_player_stats(seasons, comp = "AFLW")
    res <- load_results(seasons, comp = "AFLW")
    if (nrow(ps) == 0 || nrow(res) == 0) return(data.table::data.table())

    ps_dt <- data.table::as.data.table(ps)
    ps_dt <- ps_dt[!is.na(time_on_ground_percentage) & !is.na(team_status) &
                     !is.na(player_id) & !is.na(team_id)]

    bad_status <- ps_dt[!team_status %in% c("home", "away")]
    if (nrow(bad_status) > 0) {
      cli::cli_abort("{nrow(bad_status)} row{?s} have team_status outside {{'home','away'}}: {.val {unique(bad_status$team_status)}}")
    }

    n_na_position <- sum(is.na(ps_dt$position))
    if (n_na_position > 0) {
      cli::cli_warn("{n_na_position} row{?s} have NA position -- bucketed as INT (interchange) by default, not distinguished from genuine interchange players")
    }

    res_dt <- data.table::as.data.table(res)[
      !is.na(home_score) & !is.na(away_score),
      .(match_id, home_score, away_score)
    ]

    n_ps_matches <- length(unique(ps_dt$match_id))
    rows <- merge(ps_dt, res_dt, by = "match_id")
    n_matched <- length(unique(rows$match_id))
    if (n_matched < n_ps_matches) {
      cli::cli_warn("{n_ps_matches - n_matched} of {n_ps_matches} match_id{?s} in player_stats had no matching result and were dropped -- n is the whole lever for this fit, a silent partial drop matters more here than usual")
    }

    rows[, `:=`(
      is_home         = team_status == "home",
      position_bucket = .team_rapm_position_bucket(position, comp = "AFLW"),
      tog             = pmin(pmax(time_on_ground_percentage / 100, 0), 1)
    )]
  }

  rows[, `:=`(
    home_margin = home_score - away_score,
    team_score  = data.table::fifelse(is_home, home_score, away_score),
    opp_score   = data.table::fifelse(is_home, away_score, home_score)
  )]

  out_cols <- c("match_id", "player_id", "player_name", "team_id", "position_bucket",
                "tog", "is_home", "home_margin", "team_score", "opp_score")
  if ("season" %in% names(rows)) out_cols <- c(out_cols, "season")
  rows[, ..out_cols]
}

# .team_rapm_modal_bucket ----

#' Modal (most frequent) non-NA value -- used to pick one position_bucket for
#' a player whose bucket drifted match-to-match.
#' @keywords internal
.team_rapm_modal_bucket <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)  # table()/names() on all-NA input
  # returns NULL, not NA -- which corrupts data.table's j-column type
  # inference for the FIRST group specifically. Handle explicitly.
  names(sort(table(x), decreasing = TRUE))[1]
}

# .team_rapm_prune_columns ----

#' Decide RAPM's design-matrix columns: players clearing the threshold get
#' their own column, everyone else pools into a shared
#' "replacement_<position>" column -- the actual fix for the
#' overparameterization a static per-player column set produces (see file
#' header). Directly cuts column count `p` to fit the row count `n` that
#' exists.
#'
#' @param player_rows Output of \code{.prepare_team_rapm_player_rows()}.
#' @param threshold Minimum value (in \code{unit}'s terms) required for an
#'   individual column. Default depends on \code{comp}: \code{TEAM_RAPM_MIN_GAMES}
#'   for AFLM, \code{AFLW_RAPM_MIN_TOG_MINUTES} for AFLW.
#' @param unit "games" (count of appearances) or "tog_minutes" (total
#'   time-on-ground minutes). Default depends on \code{comp} -- AFLM uses
#'   "games" (rapm_general.R's already-validated, proven lever for men's data
#'   volume); AFLW uses "tog_minutes" (a fresh empirical sweep found this the
#'   right lever for AFLW's much shorter history -- games alone behaves very
#'   differently in relative terms there). Not silently unified to one unit
#'   without re-checking which one actually works for each comp's data.
#' @param game_minutes Nominal full-game minutes, used only when
#'   \code{unit="tog_minutes"} to convert \code{tog} (a 0-1 fraction) into
#'   minutes. Default depends on \code{comp}.
#' @param comp "AFLM" (default) or "AFLW".
#' @return data.table(player_id, player_name, position_bucket, n_games,
#'   total_tog_minutes, rapm_col) -- one row per unique player.
#' @keywords internal
.team_rapm_prune_columns <- function(player_rows, threshold = NULL, unit = NULL,
                                     game_minutes = NULL, comp = "AFLM") {
  .validate_afl_comp(comp)
  if (is.null(unit)) unit <- if (comp == "AFLM") "games" else "tog_minutes"
  if (is.null(game_minutes)) game_minutes <- if (comp == "AFLM") 120 else 80
  if (is.null(threshold)) {
    threshold <- if (comp == "AFLM") TEAM_RAPM_MIN_GAMES else AFLW_RAPM_MIN_TOG_MINUTES
  }

  totals <- player_rows[, .(
    n_games            = .N,
    total_tog_minutes  = sum(tog * game_minutes),
    player_name        = data.table::first(player_name),
    position_bucket    = .team_rapm_modal_bucket(position_bucket)
  ), by = player_id]

  metric <- if (unit == "games") totals$n_games else totals$total_tog_minutes
  totals[, rapm_col := data.table::fifelse(
    metric >= threshold & !is.na(position_bucket),
    player_id,
    paste0("replacement_", position_bucket)
  )]
  # NA position_bucket has no valid replacement_<NA> target -- pool those
  # rows into a single catch-all rather than producing a "replacement_NA"
  # column that would silently look like a real position bucket.
  totals[is.na(position_bucket) & rapm_col != player_id, rapm_col := "replacement_UNKNOWN"]

  survival <- totals[, .(n_kept = sum(rapm_col == player_id), n_pooled = sum(rapm_col != player_id)),
                      by = position_bucket]
  empty_buckets <- survival[n_kept == 0 & !is.na(position_bucket)]
  if (nrow(empty_buckets) > 0) {
    cli::cli_warn("threshold={threshold} ({unit}) pools EVERY player in bucket{?s} {.val {empty_buckets$position_bucket}} into replacement-level -- no individually-rated player will exist for {?that bucket/those buckets}")
  }

  totals
}

# build_team_rapm_net ----

#' Build the "net" RAPM design matrix: one row per match, signed
#' home(+)/away(-) columns, target = home_margin.
#'
#' @param seasons Passed to \code{.prepare_team_rapm_player_rows()}.
#' @param comp "AFLM" (default) or "AFLW".
#' @param exposure "binary" (played=1) or "tog" (time-on-ground weighted).
#'   Default "binary" for AFLM (rapm_general.R's own best OOS-R^2 flavour,
#'   see sec7.22b: 0.2561 vs 0.2303 -- an unresolved tradeoff against
#'   coefficient reliability, not a settled pick, report both). Default "tog"
#'   for AFLW (TOG-weighting is the only exposure signal that's ever been
#'   validated there -- binary has not been tested for AFLW, don't assume it
#'   transfers).
#' @inheritParams .team_rapm_prune_columns
#' @return list(X (sparse Matrix), y (home_margin), match_ids, columns, n, p,
#'   n_over_p).
#' @keywords internal
build_team_rapm_net <- function(seasons = TRUE, comp = "AFLM",
                                exposure = NULL, threshold = NULL, unit = NULL,
                                game_minutes = NULL) {
  .validate_afl_comp(comp)
  if (is.null(exposure)) exposure <- if (comp == "AFLM") "binary" else "tog"
  exposure <- match.arg(exposure, c("binary", "tog"))
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    cli::cli_abort("Matrix package required for team RAPM.")
  }
  rows <- .prepare_team_rapm_player_rows(seasons, comp = comp)
  if (nrow(rows) == 0) {
    cli::cli_abort("No player rows available to build a RAPM matrix for comp {.val {comp}}.")
  }

  col_map <- .team_rapm_prune_columns(rows, threshold, unit, game_minutes, comp = comp)
  rows <- merge(rows, col_map[, .(player_id, rapm_col)], by = "player_id")

  matches <- unique(rows[, .(match_id, home_margin)])
  matches[, row_idx := .I]
  columns <- sort(unique(col_map$rapm_col))
  col_idx <- stats::setNames(seq_along(columns), columns)

  rows <- merge(rows, matches[, .(match_id, row_idx)], by = "match_id")
  w <- if (exposure == "tog") rows$tog else 1
  rows[, value := data.table::fifelse(is_home, w, -w)]
  rows[, j := col_idx[rapm_col]]

  # Multiple pooled players in the same match/column (replacement bucket) sum
  # -- deliberate: one shared column absorbs the net replacement-level
  # presence differential between the two teams.
  agg <- rows[, .(value = sum(value)), by = .(row_idx, j)]

  X <- Matrix::sparseMatrix(
    i = agg$row_idx, j = agg$j, x = agg$value,
    dims = c(nrow(matches), length(columns))
  )

  list(
    X = X, y = matches$home_margin, match_ids = matches$match_id,
    columns = columns, n = nrow(X), p = ncol(X), n_over_p = nrow(X) / ncol(X)
  )
}

# build_team_rapm_split ----

#' Build the "split" RAPM design matrix: one row per team-side (2 per
#' match), columns doubled into an own-block and an opp-block, target =
#' points scored by that row's team.
#'
#' A single ridge fit on this design yields BOTH offense (own-block
#' coefficient, positive-is-good) and defense (opp-block coefficient,
#' NEGATIVE-is-good, matching panna's internal RAPM convention) simultaneously
#' -- see \code{extract_team_rapm_ratings()} for how the two blocks get read
#' back out.
#'
#' @inheritParams build_team_rapm_net
#' @return list(X (sparse Matrix, n team-sides x 2p columns), y (points
#'   scored), team_match_ids, match_ids, columns (length p), n, p, n_over_p).
#' @keywords internal
#' @param player_rows Optional pre-built row table (same shape
#'   \code{.prepare_team_rapm_player_rows()} returns) to build the design from
#'   instead of fetching fresh -- lets a caller row-filter first (e.g.
#'   \code{build_team_rapm_asof()}'s point-in-time \code{match_date <= ref_date}
#'   filter) while reusing this function's column-pruning and matrix-building
#'   logic unchanged. \code{NULL} (default) preserves existing behaviour
#'   exactly: fetch fresh via \code{seasons}/\code{comp}.
build_team_rapm_split <- function(seasons = TRUE, comp = "AFLM",
                                  exposure = NULL, threshold = NULL, unit = NULL,
                                  game_minutes = NULL, player_rows = NULL) {
  .validate_afl_comp(comp)
  if (is.null(exposure)) exposure <- if (comp == "AFLM") "binary" else "tog"
  exposure <- match.arg(exposure, c("binary", "tog"))
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    cli::cli_abort("Matrix package required for team RAPM.")
  }
  rows <- if (is.null(player_rows)) .prepare_team_rapm_player_rows(seasons, comp = comp) else player_rows
  if (nrow(rows) == 0) {
    cli::cli_abort("No player rows available to build a RAPM matrix for comp {.val {comp}}.")
  }

  col_map <- .team_rapm_prune_columns(rows, threshold, unit, game_minutes, comp = comp)
  rows <- merge(rows, col_map[, .(player_id, rapm_col)], by = "player_id")

  sides <- unique(rows[, .(match_id, is_home, team_score)])
  sides[, team_match_id := paste(match_id, data.table::fifelse(is_home, "home", "away"), sep = "_")]
  sides[, row_idx := .I]

  columns <- sort(unique(col_map$rapm_col))
  col_idx <- stats::setNames(seq_along(columns), columns)
  P <- length(columns)

  rows[, w := if (exposure == "tog") tog else 1]

  own <- merge(sides[, .(row_idx, match_id, is_home)],
               rows[, .(match_id, is_home, rapm_col, w)],
               by = c("match_id", "is_home"))
  own[, j := col_idx[rapm_col]]

  opp_side <- rows[, .(match_id, is_home = !is_home, rapm_col, w)]
  opp <- merge(sides[, .(row_idx, match_id, is_home)], opp_side,
               by = c("match_id", "is_home"))
  opp[, j := col_idx[rapm_col] + P]

  agg <- rbind(
    own[, .(value = sum(w)), by = .(row_idx, j)],
    opp[, .(value = sum(w)), by = .(row_idx, j)]
  )

  X <- Matrix::sparseMatrix(
    i = agg$row_idx, j = agg$j, x = agg$value,
    dims = c(nrow(sides), 2L * P)
  )

  list(
    X = X, y = sides$team_score, team_match_ids = sides$team_match_id,
    match_ids = sides$match_id, columns = columns,
    n = nrow(X), p = P, n_over_p = nrow(X) / P
  )
}

# .team_rapm_fit ----

#' Shared ridge-fit core for both net and split designs -- CV R^2 (out-of-
#' fold, the gate number) and in-sample R^2, glmnet's own documented
#' index-based lambda.min lookup (not a fragile floating-point == match).
#'
#' Wide lambda grid (\code{nlambda=400, lambda.min.ratio=1e-6}) applied
#' unconditionally: glmnet's plain default grid never searches low enough for
#' a design this wide (p in the hundreds) and silently picks its own
#' smallest-tried lambda as "lambda.min" -- under-regularised relative to
#' where the true minimum sits. Measured directly on AFLW data: default grid
#' -> CV R^2=0.258; this wider grid -> CV R^2=0.493, same data, same design.
#' Applying it to AFLM too is a strict improvement (a wider search can only
#' find an equal-or-better minimum), not a change carried over without
#' checking.
#' @param weights Optional numeric vector, glmnet observation (case) weights
#'   -- e.g. decay weights for an as-of/career-style fit
#'   (\code{build_team_rapm_asof()}). \code{NULL} (default) fits unweighted,
#'   identical to every existing caller's behaviour.
#' @keywords internal
.team_rapm_fit <- function(X, y, foldid = NULL, nfolds = 10, seed = 20260825, weights = NULL) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    cli::cli_abort("glmnet package required for team RAPM.")
  }
  set.seed(seed)
  args <- list(x = X, y = y, alpha = 0, standardize = FALSE, keep = TRUE,
               nlambda = 400, lambda.min.ratio = 1e-6)
  if (!is.null(weights)) {
    stopifnot(length(weights) == nrow(X))
    args$weights <- weights
  }
  if (is.null(foldid)) {
    args$nfolds <- min(nfolds, nrow(X))
  } else {
    args$foldid <- foldid
  }
  cv_model <- do.call(glmnet::cv.glmnet, args)

  lambda_idx <- cv_model$index["min", "Lambda"]
  stopifnot(length(lambda_idx) == 1L)
  oof_pred <- cv_model$fit.preval[, lambda_idx]
  in_sample_pred <- as.vector(stats::predict(cv_model, newx = X, s = "lambda.min"))
  # Weighted R^2 when observation weights are supplied (e.g. decay weights) --
  # an unweighted R^2 on a decay-weighted fit would score heavily-downweighted
  # old rows exactly as much as recent ones, which is not what the fit optimised.
  wv <- if (is.null(weights)) rep(1, length(y)) else weights
  wmean_y <- stats::weighted.mean(y, wv)
  cv_r2 <- 1 - sum(wv * (y - oof_pred)^2) / sum(wv * (y - wmean_y)^2)
  in_sample_r2 <- 1 - sum(wv * (y - in_sample_pred)^2) / sum(wv * (y - wmean_y)^2)

  list(model = cv_model, lambda_min = cv_model$lambda.min, cv_r2 = cv_r2, in_sample_r2 = in_sample_r2)
}

# fit_team_rapm_net ----

#' Ridge-fit the "net" RAPM design.
#' @param rapm_data Output of \code{build_team_rapm_net()}.
#' @param nfolds CV folds. Default 10.
#' @param seed RNG seed.
#' @return list(model, lambda_min, cv_r2, in_sample_r2, n, p, n_over_p).
#' @keywords internal
fit_team_rapm_net <- function(rapm_data, nfolds = 10, seed = 20260825) {
  fit <- .team_rapm_fit(rapm_data$X, rapm_data$y, nfolds = nfolds, seed = seed)
  c(fit, list(n = rapm_data$n, p = rapm_data$p, n_over_p = rapm_data$n_over_p))
}

# fit_team_rapm_split ----

#' Ridge-fit the "split" RAPM design, holding out whole MATCHES per CV fold
#' (both sides of a match share players via the opp-block columns, so
#' splitting them across folds would let a player's opp-block presence leak
#' between train and test).
#' @param rapm_data Output of \code{build_team_rapm_split()}.
#' @inheritParams fit_team_rapm_net
#' @return list(model, lambda_min, cv_r2, in_sample_r2, n, p, n_over_p).
#' @keywords internal
fit_team_rapm_split <- function(rapm_data, nfolds = 10, seed = 20260825) {
  set.seed(seed)
  match_ids <- unique(rapm_data$match_ids)
  fold_of_match <- stats::setNames(
    sample(rep_len(seq_len(min(nfolds, length(match_ids))), length(match_ids))),
    match_ids
  )
  foldid <- unname(fold_of_match[rapm_data$match_ids])
  fit <- .team_rapm_fit(rapm_data$X, rapm_data$y, foldid = foldid, seed = seed)
  c(fit, list(n = rapm_data$n, p = rapm_data$p, n_over_p = rapm_data$n_over_p))
}

# .team_rapm_split_half_reliability ----

#' Split-half reliability of a RAPM design's fitted coefficients -- "a rating
#' that does not replicate on independent halves of the data is not a
#' rating" (FABLE-DEFENDER-VALUE-PLAN.md sec7.21). Splits by whole MATCH (not
#' row), refits on each half at the SAME lambda the full-data CV chose,
#' correlates the two coefficient vectors (nonzero-in-either only).
#' @param X,y,match_ids Same-length design matrix, target, and match_id per row.
#' @param lambda The lambda to refit both halves at (typically the full-data
#'   cv.glmnet's lambda.min).
#' @param seed RNG seed for the half-split.
#' @return Numeric, Pearson r between the two halves' coefficients.
#' @keywords internal
.team_rapm_split_half_reliability <- function(X, y, match_ids, lambda, seed = 20260825) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    cli::cli_abort("glmnet package required for team RAPM.")
  }
  set.seed(seed)
  ug <- unique(match_ids)
  half_a_matches <- sample(ug, floor(length(ug) / 2))
  a <- match_ids %in% half_a_matches
  b <- !a

  ca <- as.numeric(stats::coef(glmnet::glmnet(X[a, ], y[a], alpha = 0, lambda = lambda, standardize = FALSE)))[-1]
  cb <- as.numeric(stats::coef(glmnet::glmnet(X[b, ], y[b], alpha = 0, lambda = lambda, standardize = FALSE)))[-1]
  ok <- is.finite(ca) & is.finite(cb) & (ca != 0 | cb != 0)
  suppressWarnings(stats::cor(ca[ok], cb[ok]))
}

# .team_rapm_coappearance ----

#' Same-team co-appearance diagnostic -- quantifies how separable a player's
#' coefficient is from his regular teammates' before trusting any individual
#' rating (panna's men's-side finding: co-appearance ~0.97 made RAPM
#' unidentifiable there; this measures the same risk here).
#'
#' @param player_rows Output of \code{.prepare_team_rapm_player_rows()}.
#' @return list(mean_jaccard, pct_over_090, n_pairs).
#' @keywords internal
.team_rapm_coappearance <- function(player_rows) {
  by_player <- split(player_rows$match_id, player_rows$player_id)
  team_players <- unique(player_rows[, .(player_id, team_id)])
  pairs <- team_players[, {
    ids <- sort(player_id)
    if (length(ids) < 2) return(NULL)
    cb <- utils::combn(ids, 2)
    data.table::data.table(p1 = cb[1, ], p2 = cb[2, ])
  }, by = team_id]
  pairs <- unique(pairs[, .(p1, p2)])
  if (nrow(pairs) == 0) return(list(mean_jaccard = NA_real_, pct_over_090 = NA_real_, n_pairs = 0L))

  jac <- vapply(seq_len(nrow(pairs)), function(i) {
    s1 <- by_player[[pairs$p1[i]]]; s2 <- by_player[[pairs$p2[i]]]
    if (is.null(s1) || is.null(s2)) return(NA_real_)
    inter <- length(intersect(s1, s2)); uni <- length(union(s1, s2))
    if (uni == 0) return(NA_real_)
    inter / uni
  }, numeric(1))
  jac <- jac[!is.na(jac)]

  list(mean_jaccard = mean(jac), pct_over_090 = 100 * mean(jac > 0.90), n_pairs = length(jac))
}

# extract_team_rapm_ratings ----

#' Combine a fitted "split" model's own-block/opp-block coefficients into the
#' published per-player RAPM table.
#'
#' @param rapm_data Output of \code{build_team_rapm_split()}.
#' @param rapm_fit Output of \code{fit_team_rapm_split()}.
#' @return data.table(player_id, rating_type, rapm_offense, rapm_defense,
#'   rapm) -- one row per matrix column, REAL players AND
#'   \code{replacement_<bucket>} rows both kept (dropping the replacement
#'   rows leaves no way for a caller to get ANY rating for a pooled
#'   low-minutes player -- not even the replacement-level fallback that was
#'   actually fit for them). \code{rapm_defense} is negative-is-good,
#'   unflipped -- \code{rapm = rapm_offense - rapm_defense} is therefore
#'   correctly signed as-is. Flip \code{rapm_defense}'s sign only at any
#'   future publication/display step, exactly as panna does.
#' @keywords internal
extract_team_rapm_ratings <- function(rapm_data, rapm_fit) {
  coefs <- as.vector(stats::coef(rapm_fit$model, s = rapm_fit$model$lambda.min))[-1]
  P <- length(rapm_data$columns)
  stopifnot(length(coefs) == 2L * P)

  out <- data.table::data.table(
    player_id    = rapm_data$columns,
    rating_type  = data.table::fifelse(startsWith(rapm_data$columns, "replacement_"),
                                       "replacement", "individual"),
    rapm_offense = coefs[seq_len(P)],
    rapm_defense = coefs[P + seq_len(P)]
  )
  out[, rapm := rapm_offense - rapm_defense]
  data.table::setorder(out, -rapm)
  out[]
}

# .team_rapm_season_cutoffs ----

#' Season cutoffs for an expanding-window refit: every season from
#' \code{min_train_seasons+1}'th onward has enough prior history to train on.
#' @param seasons Numeric vector of seasons, or \code{TRUE} for all available.
#' @param comp "AFLM" or "AFLW".
#' @param min_train_seasons Minimum number of prior seasons required before
#'   the first cutoff (default 2 -- don't fit off a single season).
#' @return Sorted integer vector of season cutoffs, each meaning "fit on
#'   seasons strictly before this one, rate this one."
#' @keywords internal
.team_rapm_season_cutoffs <- function(seasons = TRUE, comp = "AFLM", min_train_seasons = 2L) {
  res <- load_results(seasons, comp = comp)
  if (nrow(res) == 0) return(integer(0))
  all_seasons <- sort(unique(res$season))
  if (length(all_seasons) <= min_train_seasons) return(integer(0))
  all_seasons[(min_train_seasons + 1):length(all_seasons)]
}

# build_team_rapm_expanding ----

#' Point-in-time RAPM: an expanding-window season-block refit. For each
#' season cutoff S, fits RAPM using ONLY matches from seasons strictly before
#' S, and uses that fit's ratings for season S. This is the leak-safe path
#' for anything feeding a match-prediction feature -- the static full-history
#' functions above (\code{build_team_rapm_net(seasons=TRUE)} etc.) fit once on
#' everything, which is fine for "what's the best CURRENT rating" but leaks
#' future-season information into any earlier prediction (see file header).
#'
#' RAPM is a batch ridge regression, not sequential like Elo, so it cannot be
#' made point-in-time for free -- this refits the whole design once per
#' season cutoff, which is real compute (each cutoff is its own CV fit).
#'
#' @param seasons Numeric vector of seasons, or \code{TRUE} for all available.
#' @param comp "AFLM" (default) or "AFLW".
#' @param min_train_seasons Minimum prior seasons required before the first
#'   cutoff. Default 2.
#' @param design "split" (default, produces offense+defense ratings via
#'   \code{extract_team_rapm_ratings()}) or "net" (home-margin design,
#'   returns raw per-cutoff fit objects instead since there's no ratings
#'   table to extract from a net design).
#' @param nfolds,seed Passed to the per-cutoff fit.
#' @inheritParams build_team_rapm_net
#' @return For \code{design="split"}: data.table with all of
#'   \code{extract_team_rapm_ratings()}'s columns plus \code{season} (the
#'   cutoff each row's rating is valid FOR -- computed using strictly-prior
#'   seasons only) and \code{n_train_seasons}. For \code{design="net"}: a
#'   list of per-cutoff \code{list(season, rapm_data, rapm_fit)}.
#' @keywords internal
build_team_rapm_expanding <- function(seasons = TRUE, comp = "AFLM", min_train_seasons = 2L,
                                      design = c("split", "net"), exposure = NULL,
                                      threshold = NULL, unit = NULL, game_minutes = NULL,
                                      nfolds = 10, seed = 20260825) {
  .validate_afl_comp(comp)
  design <- match.arg(design)
  cutoffs <- .team_rapm_season_cutoffs(seasons, comp = comp, min_train_seasons = min_train_seasons)
  if (length(cutoffs) == 0) {
    cli::cli_abort("Not enough seasons of history to build an expanding-window RAPM (need > {min_train_seasons}).")
  }
  cli::cli_inform("build_team_rapm_expanding: {length(cutoffs)} season cutoff{?s} to fit for comp {.val {comp}}: {.val {cutoffs}}")

  results <- vector("list", length(cutoffs))
  for (i in seq_along(cutoffs)) {
    cutoff <- cutoffs[i]
    train_seasons <- sort(unique(load_results(seasons, comp = comp)$season))
    train_seasons <- train_seasons[train_seasons < cutoff]
    cli::cli_inform("build_team_rapm_expanding: cutoff {cutoff} -- training on {length(train_seasons)} prior season{?s}")

    builder <- if (design == "split") build_team_rapm_split else build_team_rapm_net
    fitter  <- if (design == "split") fit_team_rapm_split else fit_team_rapm_net

    rapm_data <- tryCatch(
      builder(train_seasons, comp = comp, exposure = exposure, threshold = threshold,
              unit = unit, game_minutes = game_minutes),
      error = function(e) {
        cli::cli_warn("build_team_rapm_expanding: cutoff {cutoff} failed to build a design matrix: {conditionMessage(e)}")
        NULL
      }
    )
    if (is.null(rapm_data)) next

    rapm_fit <- fitter(rapm_data, nfolds = nfolds, seed = seed)

    if (design == "split") {
      ratings <- extract_team_rapm_ratings(rapm_data, rapm_fit)
      ratings[, `:=`(season = cutoff, n_train_seasons = length(train_seasons))]
      results[[i]] <- ratings
    } else {
      results[[i]] <- list(season = cutoff, rapm_data = rapm_data, rapm_fit = rapm_fit)
    }
  }

  if (design == "split") {
    out <- data.table::rbindlist(results, fill = TRUE)
    if (nrow(out) == 0) {
      cli::cli_abort("build_team_rapm_expanding: every season cutoff failed to fit.")
    }
    out[]
  } else {
    Filter(Negate(is.null), results)
  }
}
