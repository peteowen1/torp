# AFLW RAPM (Regularized Adjusted Plus-Minus)
# =============================================
# New territory -- torp's men's-side chain-native EPV/EPR doesn't fit a
# RAPM-shaped regression at all, so there's no packaged men's file to mirror.
# The design instead follows torp's own existing ad-hoc RAPM engine at
# data-raw/04-analysis/rapm_general.R (built 2026-07-27 for men's defensive
# value work) -- same sparse-matrix construction, same replacement-level
# pooling idea, same "margin" and "split" flavours -- adapted to source AFLW
# data via load_player_stats(comp="AFLW")/load_results(comp="AFLW") instead
# of local men's parquet files.
#
# THE FIX THIS FILE IMPLEMENTS (docs/plans/AFLW-MIGRATION-PLAN.md §6.2):
# daisychain's original AFLW RAPM matrix was overparameterized -- 1,409 player
# columns against only 690 whole-match rows (2.04 columns per row, MORE
# parameters than observations). TOG%-weighting a presence column is real
# information a binary indicator throws away, but it does NOT by itself fix
# an n<p problem -- it changes what's inside each cell, not the matrix shape.
# So this file does two things together: (1) TOG%-weighted presence (kept,
# necessary), and (2) column pruning -- players below AFLW_RAPM_MIN_TOG_MINUTES
# total time-on-ground pool into a shared "replacement_<position>" column
# instead of each getting a free parameter, which is the actual lever
# available here (n cannot be increased; AFLW's full history is what it is).
#
# TWO DESIGNS, both built from the same pruned column set so results combine:
#   - build_aflw_rapm_net(): one row per MATCH, signed home(+)/away(-) TOG-
#     weighted columns, target = home_margin. Directly comparable to Phase 0's
#     gate number (0.4765 CV R^2, same target/row structure as daisychain's
#     own fit) -- this is the flavour the Phase 3 gate is measured against.
#   - build_aflw_rapm_split(): one row per TEAM-SIDE (2 per match), columns
#     DOUBLED into an own-block and an opp-block, target = points scored by
#     that row's team. One ridge fit gives both offense (own-block coef,
#     positive-is-good) and defense (opp-block coef, NEGATIVE-is-good, same
#     internal convention as panna's RAPM -- see pannaverse/docs/explainers/
#     rapm.md -- flip only at any future publication step). This is the
#     flavour that produces the published data.table(player_id, rapm_offense,
#     rapm_defense, rapm) shape from plan §6.1.
#
# Do not brand this "TORP for AFLW" -- box-score RAPM on ~500 matches of
# history is a different, much noisier instrument than men's chain-native
# EPV/EPR, and is not comparable to it.

# .aflw_rapm_position_bucket ----

#' Bucket AFLW's 18-slot lineup position taxonomy into 4 coarse groups for
#' RAPM's replacement-level pooling.
#'
#' AFLW's \code{position} column (from \code{load_player_stats(comp="AFLW")})
#' uses the same abbreviated on-field slot codes as men's guernsey-position
#' lists (BPL/BPR/C/CHB/CHF/FB/FF/FPR/HBFL/HBFR/HFFL/HFFR/R/RK/RR/WL/WR), plus
#' INT (interchange/bench) and EMERG (emergency, did not take the field).
#' There is no pre-built \code{position_group} for AFLW the way men's
#' \code{load_player_game_data()} carries one, so this buckets directly from
#' the raw slot code.
#'
#' @param position Character vector of raw AFLW position codes.
#' @return Character vector: "DEF", "MID", "FWD", or "INT" (interchange/
#'   emergency/unmapped -- the bucket every low-minute fringe player
#'   ultimately pools toward regardless of their nominal slot, since an
#'   interchange player's on-ground time is what actually determines pruning,
#'   not their programmed position).
#' @keywords internal
.aflw_rapm_position_bucket <- function(position) {
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

# .prepare_aflw_rapm_player_rows ----

#' Build the long player-match-appearance table RAPM's design matrices are
#' built from.
#'
#' @param seasons Numeric vector of seasons, or \code{TRUE} for all available
#'   AFLW seasons. Passed to \code{load_player_stats()}/\code{load_results()}.
#' @return data.table, one row per player-match appearance:
#'   match_id, player_id, player_name, team_id, position_bucket,
#'   tog (0-1 fraction of \code{time_on_ground_percentage}), is_home,
#'   home_margin, team_score, opp_score. Rows with missing TOG% or team
#'   status are dropped (mirrors daisychain's own
#'   \code{filter(!is.na(time_on_ground_percentage))}).
#' @keywords internal
.prepare_aflw_rapm_player_rows <- function(seasons = TRUE) {
  ps <- load_player_stats(seasons, comp = "AFLW")
  res <- load_results(seasons, comp = "AFLW")
  if (nrow(ps) == 0 || nrow(res) == 0) return(data.table::data.table())

  ps_dt <- data.table::as.data.table(ps)
  ps_dt <- ps_dt[!is.na(time_on_ground_percentage) & !is.na(team_status) &
                   !is.na(player_id) & !is.na(team_id)]

  # team_status must be exactly "home"/"away" -- an unexpected value (typo,
  # case difference, a new API value) would otherwise silently evaluate
  # `is_home <- team_status == "home"` to FALSE, flipping that row's sign
  # in build_aflw_rapm_net() and swapping which block it lands in for
  # build_aflw_rapm_split(), with no error.
  bad_status <- ps_dt[!team_status %in% c("home", "away")]
  if (nrow(bad_status) > 0) {
    cli::cli_abort("{nrow(bad_status)} row{?s} have team_status outside {{'home','away'}}: {.val {unique(bad_status$team_status)}}")
  }

  # NA position falls through dplyr::case_when()'s final TRUE branch in
  # .aflw_rapm_position_bucket() to "INT" -- indistinguishable from a genuine
  # interchange player. Log rather than silently absorb.
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
    cli::cli_warn("{n_ps_matches - n_matched} of {n_ps_matches} AFLW match_id{?s} in player_stats had no matching result and were dropped -- n is the whole lever for this fit, a silent partial drop matters more here than usual")
  }

  rows[, `:=`(
    is_home          = team_status == "home",
    position_bucket  = .aflw_rapm_position_bucket(position),
    tog              = pmin(pmax(time_on_ground_percentage / 100, 0), 1)
  )]
  rows[, `:=`(
    home_margin = home_score - away_score,
    team_score  = data.table::fifelse(is_home, home_score, away_score),
    opp_score   = data.table::fifelse(is_home, away_score, home_score)
  )]

  rows[, .(match_id, player_id, player_name, team_id, position_bucket,
           tog, is_home, home_margin, team_score, opp_score)]
}

# .aflw_rapm_prune_columns ----

#' Decide RAPM's design-matrix columns: real players above the TOG-minutes
#' threshold get their own column, everyone else pools into a shared
#' "replacement_<position>" column.
#'
#' This is the actual fix for daisychain's overparameterization (§6.2) -- it
#' directly cuts the column count `p` to fit the row count `n` that exists,
#' which TOG-weighting alone cannot do.
#'
#' @param player_rows Output of \code{.prepare_aflw_rapm_player_rows()}.
#' @param min_tog_minutes Minimum total time-on-ground minutes (summed across
#'   all a player's appearances) required to get an individual column.
#'   Default \code{AFLW_RAPM_MIN_TOG_MINUTES}.
#' @param game_minutes Nominal minutes in a full AFLW match, used to convert
#'   \code{tog} (a 0-1 fraction) into minutes. Default 80 (AFLW quarters are
#'   shorter than men's -- this is a coarse approximation, not a modelled
#'   quantity; tune alongside \code{min_tog_minutes} against the resulting
#'   n/p ratio, not assumed precise).
#' @return data.table(player_id, player_name, position_bucket,
#'   total_tog_minutes, n_games, rapm_col) -- one row per unique player.
#' @keywords internal
.aflw_rapm_prune_columns <- function(player_rows,
                                     min_tog_minutes = AFLW_RAPM_MIN_TOG_MINUTES,
                                     game_minutes = 80) {
  totals <- player_rows[, .(
    total_tog_minutes = sum(tog * game_minutes),
    n_games            = .N,
    player_name         = data.table::first(player_name),
    # a player's bucket can drift match-to-match (moved position); take the
    # modal bucket rather than the first, since pruning uses it to decide
    # WHICH replacement column a pooled player's presence lands in
    position_bucket = names(sort(table(position_bucket), decreasing = TRUE))[1]
  ), by = player_id]

  totals[, rapm_col := data.table::fifelse(
    total_tog_minutes >= min_tog_minutes,
    player_id,
    paste0("replacement_", position_bucket)
  )]

  # Visibility for the failure mode a men's-side prototype (rapm_general.R)
  # used to print and this port dropped: if every player in a bucket pools
  # into replacement (threshold set too high, or a bucket genuinely thin),
  # the resulting ratings table is syntactically valid with zero individually
  # -rated players for that bucket -- indistinguishable from "AFLW has no
  # players at this position" without this check.
  survival <- totals[, .(n_kept = sum(rapm_col == player_id), n_pooled = sum(rapm_col != player_id)),
                      by = position_bucket]
  empty_buckets <- survival[n_kept == 0]
  if (nrow(empty_buckets) > 0) {
    cli::cli_warn("min_tog_minutes={min_tog_minutes} pools EVERY player in bucket{?s} {.val {empty_buckets$position_bucket}} into replacement-level -- no individually-rated player will exist for {?that bucket/those buckets}")
  }

  totals
}

# build_aflw_rapm_net ----

#' Build the AFLW RAPM "net" design matrix: one row per match, signed
#' home(+)/away(-) TOG-weighted player columns, target = home_margin.
#'
#' Directly comparable to Phase 0's gate number (daisychain's own
#' \code{fit_rapm_model()}, CV R^2 0.4765, n=690) -- same row unit (one row
#' per match) and same target (home_margin), so a CV R^2 measured on this
#' design is an apples-to-apples improvement check, not a different number
#' wearing the same name.
#'
#' @inheritParams .aflw_rapm_prune_columns
#' @param seasons Passed to \code{.prepare_aflw_rapm_player_rows()}.
#' @return list(X (sparse Matrix, n matches x p columns), y (home_margin),
#'   match_ids, columns (character vector, column j's name), n, p, n_over_p).
#' @keywords internal
build_aflw_rapm_net <- function(seasons = TRUE,
                                min_tog_minutes = AFLW_RAPM_MIN_TOG_MINUTES,
                                game_minutes = 80) {
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    cli::cli_abort("Matrix package required for AFLW RAPM.")
  }
  rows <- .prepare_aflw_rapm_player_rows(seasons)
  if (nrow(rows) == 0) {
    cli::cli_abort("No AFLW player-stat rows available to build a RAPM matrix.")
  }

  col_map <- .aflw_rapm_prune_columns(rows, min_tog_minutes, game_minutes)
  rows <- merge(rows, col_map[, .(player_id, rapm_col)], by = "player_id")

  matches <- unique(rows[, .(match_id, home_margin)])
  matches[, row_idx := .I]
  columns <- sort(unique(col_map$rapm_col))
  col_idx <- stats::setNames(seq_along(columns), columns)

  rows <- merge(rows, matches[, .(match_id, row_idx)], by = "match_id")
  rows[, value := data.table::fifelse(is_home, tog, -tog)]
  rows[, j := col_idx[rapm_col]]

  # Multiple pooled players in the same match/column (replacement bucket) sum
  # -- deliberate: that's what makes one shared column absorb the net
  # replacement-level presence differential between the two teams.
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

# build_aflw_rapm_split ----

#' Build the AFLW RAPM "split" design matrix: one row per team-side (2 per
#' match), columns doubled into an own-block and an opp-block, target =
#' points scored by that row's team.
#'
#' A single ridge fit on this design yields BOTH offense (own-block
#' coefficient, positive-is-good) and defense (opp-block coefficient,
#' NEGATIVE-is-good -- this player's presence on the opposing side lowers
#' the row's team's score when he's a good defender, matching panna's
#' internal RAPM convention exactly) simultaneously, jointly regularised --
#' see \code{fit_aflw_rapm_split()}/\code{extract_aflw_rapm_ratings()} for
#' how the two blocks get read back out.
#'
#' @inheritParams build_aflw_rapm_net
#' @return list(X (sparse Matrix, n team-sides x 2p columns), y (points
#'   scored by that row's team), team_match_ids, columns (length p -- doubled
#'   internally, own-block is columns 1:p, opp-block is p+1:2p), n, p,
#'   n_over_p (using p, not 2p, since the two blocks are the same n<p
#'   diagnostic viewed from each side)).
#' @keywords internal
build_aflw_rapm_split <- function(seasons = TRUE,
                                  min_tog_minutes = AFLW_RAPM_MIN_TOG_MINUTES,
                                  game_minutes = 80) {
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    cli::cli_abort("Matrix package required for AFLW RAPM.")
  }
  rows <- .prepare_aflw_rapm_player_rows(seasons)
  if (nrow(rows) == 0) {
    cli::cli_abort("No AFLW player-stat rows available to build a RAPM matrix.")
  }

  col_map <- .aflw_rapm_prune_columns(rows, min_tog_minutes, game_minutes)
  rows <- merge(rows, col_map[, .(player_id, rapm_col)], by = "player_id")

  # One row per team-side: each match contributes a home-side row and an
  # away-side row. team_score is already "this side's own score" (resolved
  # in .prepare_aflw_rapm_player_rows()). team_match_id keys them; match_id
  # is kept for fold grouping (both sides of one match must land in the same
  # CV fold, since they share players via the opp-block columns -- see
  # fit_aflw_rapm_split()).
  sides <- unique(rows[, .(match_id, is_home, team_score)])
  sides[, team_match_id := paste(match_id, data.table::fifelse(is_home, "home", "away"), sep = "_")]
  sides[, row_idx := .I]

  columns <- sort(unique(col_map$rapm_col))
  col_idx <- stats::setNames(seq_along(columns), columns)
  P <- length(columns)

  # own block: this row's own team's players (j in 1:P)
  own <- merge(sides[, .(row_idx, match_id, is_home)],
               rows[, .(match_id, is_home, rapm_col, tog)],
               by = c("match_id", "is_home"))
  own[, j := col_idx[rapm_col]]

  # opp block: the OTHER team's players in the same match (j in P+1:2P)
  opp_side <- rows[, .(match_id, is_home = !is_home, rapm_col, tog)]
  opp <- merge(sides[, .(row_idx, match_id, is_home)], opp_side,
               by = c("match_id", "is_home"))
  opp[, j := col_idx[rapm_col] + P]

  agg <- rbind(
    own[, .(value = sum(tog)), by = .(row_idx, j)],
    opp[, .(value = sum(tog)), by = .(row_idx, j)]
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

# fit_aflw_rapm_net ----

#' Ridge-fit the AFLW RAPM "net" design, reporting both in-sample and
#' out-of-fold CV R^2 -- the CV number is what gets compared against Phase
#' 0's 0.4765 baseline (plan §6.3 gate).
#'
#' @param rapm_data Output of \code{build_aflw_rapm_net()}.
#' @param nfolds CV folds. Default 10 (daisychain's own default).
#' @param seed RNG seed for fold assignment, for reproducibility.
#' @return list(model (cv.glmnet object), lambda_min, cv_r2 (out-of-fold,
#'   the gate number), in_sample_r2, n, p, n_over_p).
#' @keywords internal
fit_aflw_rapm_net <- function(rapm_data, nfolds = 10, seed = 20260824) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    cli::cli_abort("glmnet package required for AFLW RAPM.")
  }
  X <- rapm_data$X
  y <- rapm_data$y

  set.seed(seed)
  # nlambda/lambda.min.ratio matched to daisychain's own fit_rapm_model()
  # tuning (rapm_analysis.R) -- NOT cosmetic. glmnet's plain default grid
  # (nlambda=100, narrower lambda.min.ratio) never searches low enough for
  # this wide a design (p in the hundreds) and silently picks its own
  # smallest-tried lambda as "lambda.min" -- which is under-regularised
  # relative to where the true minimum sits. Measured directly on this data:
  # default grid -> lambda.min=36.97, CV R^2=0.258; this wider grid ->
  # lambda.min=0.97, CV R^2=0.493. Same design, same data -- the only
  # difference is whether the search actually reached the right lambda.
  cv_model <- glmnet::cv.glmnet(
    x = X, y = y, alpha = 0, nfolds = min(nfolds, nrow(X)),
    nlambda = 400, lambda.min.ratio = 1e-6,
    standardize = FALSE, keep = TRUE
  )

  # glmnet's own documented lookup, not a floating-point == match against
  # lambda.min -- the latter is fragile if the grid ever produced a
  # degenerate/duplicate-lambda situation (e.g. Finding 3's empty-bucket
  # case), where `which(...)` could silently return length 0 or >1 and
  # turn `fit.preval[, lambda_idx]` into a matrix instead of a vector.
  lambda_idx <- cv_model$index["min", "Lambda"]
  stopifnot(length(lambda_idx) == 1L)
  oof_pred <- cv_model$fit.preval[, lambda_idx]
  cv_r2 <- 1 - sum((y - oof_pred)^2) / sum((y - mean(y))^2)

  in_sample_pred <- as.vector(stats::predict(cv_model, newx = X, s = "lambda.min"))
  in_sample_r2 <- 1 - sum((y - in_sample_pred)^2) / sum((y - mean(y))^2)

  list(
    model = cv_model, lambda_min = cv_model$lambda.min,
    cv_r2 = cv_r2, in_sample_r2 = in_sample_r2,
    n = rapm_data$n, p = rapm_data$p, n_over_p = rapm_data$n_over_p
  )
}

# fit_aflw_rapm_split ----

#' Ridge-fit the AFLW RAPM "split" design, holding out whole MATCHES per CV
#' fold (not individual team-side rows) -- the two sides of a match share
#' players via the opp-block columns, so splitting them across folds would
#' let a player's opp-block presence leak between train and test.
#'
#' @param rapm_data Output of \code{build_aflw_rapm_split()}.
#' @inheritParams fit_aflw_rapm_net
#' @return list(model, lambda_min, cv_r2, in_sample_r2, n, p, n_over_p) --
#'   same shape as \code{fit_aflw_rapm_net()}. `p` here is the per-block
#'   column count (matrix has 2p columns; n/p uses the per-block p, since a
#'   fit with 2p free parameters against n rows is really "does each of the
#'   two p-sized coefficient blocks have enough rows behind it", not 2p).
#' @keywords internal
fit_aflw_rapm_split <- function(rapm_data, nfolds = 10, seed = 20260824) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    cli::cli_abort("glmnet package required for AFLW RAPM.")
  }
  X <- rapm_data$X
  y <- rapm_data$y

  set.seed(seed)
  match_ids <- unique(rapm_data$match_ids)
  fold_of_match <- stats::setNames(
    sample(rep_len(seq_len(min(nfolds, length(match_ids))), length(match_ids))),
    match_ids
  )
  foldid <- unname(fold_of_match[rapm_data$match_ids])

  # See fit_aflw_rapm_net() -- same wide-grid fix, same reason.
  cv_model <- glmnet::cv.glmnet(
    x = X, y = y, alpha = 0, foldid = foldid,
    nlambda = 400, lambda.min.ratio = 1e-6,
    standardize = FALSE, keep = TRUE
  )

  # glmnet's own documented lookup, not a floating-point == match against
  # lambda.min -- the latter is fragile if the grid ever produced a
  # degenerate/duplicate-lambda situation (e.g. Finding 3's empty-bucket
  # case), where `which(...)` could silently return length 0 or >1 and
  # turn `fit.preval[, lambda_idx]` into a matrix instead of a vector.
  lambda_idx <- cv_model$index["min", "Lambda"]
  stopifnot(length(lambda_idx) == 1L)
  oof_pred <- cv_model$fit.preval[, lambda_idx]
  cv_r2 <- 1 - sum((y - oof_pred)^2) / sum((y - mean(y))^2)

  in_sample_pred <- as.vector(stats::predict(cv_model, newx = X, s = "lambda.min"))
  in_sample_r2 <- 1 - sum((y - in_sample_pred)^2) / sum((y - mean(y))^2)

  list(
    model = cv_model, lambda_min = cv_model$lambda.min,
    cv_r2 = cv_r2, in_sample_r2 = in_sample_r2,
    n = rapm_data$n, p = rapm_data$p, n_over_p = rapm_data$n_over_p
  )
}

# extract_aflw_rapm_ratings ----

#' Combine a fitted "split" model's own-block/opp-block coefficients into
#' the published per-player AFLW RAPM table.
#'
#' @param rapm_data Output of \code{build_aflw_rapm_split()}.
#' @param rapm_fit Output of \code{fit_aflw_rapm_split()}.
#' @return data.table(player_id, rating_type, rapm_offense, rapm_defense,
#'   rapm), one row per matrix column -- REAL players (\code{rating_type =
#'   "individual"}) AND the \code{replacement_<bucket>} pooling columns
#'   (\code{rating_type = "replacement"}) are both kept. Earlier versions of
#'   this function dropped the replacement rows entirely, which left a
#'   caller with no way to get ANY rating for a pooled (low-minutes) player
#'   -- not even the replacement-level fallback that was actually fit for
#'   them. To find a specific pooled player's fallback: look up their
#'   position bucket via \code{.aflw_rapm_prune_columns()}'s \code{rapm_col}
#'   (not reproduced here, since a pooled player has no individual matrix
#'   column to key this table on), then filter this table to
#'   \code{player_id == paste0("replacement_", bucket)}. \code{rapm} =
#'   \code{rapm_offense - rapm_defense}, matching panna's
#'   \code{RAPM = offense - defense} convention with \code{rapm_defense} left
#'   in its raw negative-is-good form (unflipped) -- a good defender has a
#'   very negative \code{rapm_defense}, so subtracting it correctly adds to
#'   \code{rapm}. Flip \code{rapm_defense}'s sign only at any future
#'   publication/display step, exactly as panna does.
#' @keywords internal
extract_aflw_rapm_ratings <- function(rapm_data, rapm_fit) {
  coefs <- as.vector(stats::coef(rapm_fit$model, s = rapm_fit$model$lambda.min))[-1]  # drop intercept
  P <- length(rapm_data$columns)
  stopifnot(length(coefs) == 2L * P)

  out <- data.table::data.table(
    player_id     = rapm_data$columns,
    rating_type   = data.table::fifelse(startsWith(rapm_data$columns, "replacement_"),
                                        "replacement", "individual"),
    rapm_offense  = coefs[seq_len(P)],
    rapm_defense  = coefs[P + seq_len(P)]
  )
  out[, rapm := rapm_offense - rapm_defense]
  data.table::setorder(out, -rapm)
  out[]
}
