# honest_backfill_predictions.R
# =============================
# Regenerate the 2021-2025 match predictions as genuine out-of-sample
# predictions, round by round. Plan and evidence:
# torpverse/docs/plans/HONEST-BACKFILL-2021-2025.md
#
# Why: the predictions_<season> files for 2021-2025 on the torpdata
# `predictions` release were written by run_predictions_pipeline() over
# finished seasons, which trains on every completed match -- including the
# season being "predicted". This script retrains before every round instead.
#
# What it does, for each season s and each completed round r of s:
#   1. train the match GAMs (and XGBoost, if it can change the output) on every
#      match completed before round r: season < s, or season == s and
#      round < r, and also kicked off before round r's first bounce;
#   2. predict round r only, and keep only those rows;
#   3. write that round to <out>/_partial/<s>/round_<rr>.parquet at once.
# When every round of s is done it binds them into
# <out>/backfill_predictions_<s>.parquet. Nothing is uploaded anywhere.
#
# Resume: a season whose backfill_predictions_<s>.parquet exists is skipped,
# and inside an unfinished season every round with a partial file is skipped.
# A run killed at 90% therefore leaves every finished round on disk, not just
# every finished season.
#
# The model code is NOT copied. Training is .train_match_gams() /
# .train_match_xgb() with their train_filter argument (R/match_train.R), the
# feature frame is build_team_mdl_df() (R/match_model.R), and formatting,
# reconciliation and the finals rule are the production helpers. Three things
# differ from run_predictions_pipeline(), each because production's version
# would leak:
#   * time-decay weights are re-anchored to round r's first bounce and
#     normalised over the training rows only, which is what production computes
#     when it runs live before round r;
#   * the margin calibration scale `b` is NOT the published sidecar (fitted on
#     data up to today). It is refitted before each round from this script's
#     own earlier honest predictions (identity until MATCH_RECAL_MIN_N matches,
#     and identity if the slope gate fails);
#   * the finals sigma is the spread of this script's own earlier honest
#     errors (FINALS_MARGIN_SIGMA_FALLBACK until MARGIN_SIGMA_MIN_N), not
#     load_predictions(TRUE), which reads the hindsight rows themselves.
# So a season's numbers depend on the seasons before it in the same output
# folder. Run seasons in order, into one folder.
#
# PSR coefficients. The player ratings are as-of (EPR uses only games before
# the round), but the coefficients that turn stat skills into PSR
# (inst/extdata/psr_coefficients.csv, from data-raw/06-stat-ratings/
# 06_train_psr_model.R, `train_idx <- which(match_df$season < 2025)`) were
# fitted to the 2021-2024 match margins. Used as shipped, PSR is in-sample for
# 2022-2024. The script refuses to run without one of:
#   --psr-coef-dir=DIR   per-season coefficients, DIR/through_<s-1>/
#                        {psr,osr,dsr}_coefficients.csv, each trained only on
#                        seasons before s (see the plan's next steps);
#   --accept-psr-coef-leak   use the shipped file and label every row so.
#
# Usage (from the torp package root; arrow needs PowerShell on Windows):
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/honest_backfill_predictions.R" --seasons=2022:2025 --psr-coef-dir=PATH'
# Options:
#   --seasons=2022:2025   seasons to hold out (default 2022:2025). 2021 cannot
#                         be held out: match data starts 2021 round 14
#                         (MATCH_MIN_DATA_SEASON / MATCH_MIN_DATA_ROUND).
#   --rounds=5            only these rounds (profiling). Writes partial files
#                         and never the season file.
#   --out=DIR             output folder (default data-raw/outputs/honest-backfill,
#                         gitignored).
#   --xgb=auto|yes|no     auto (default) trains XGBoost only when
#                         MATCH_BLEND_WEIGHT < 1. At 1.0 the served margin is
#                         pure GAM and pred_win comes from the GAM win model,
#                         so XGBoost cannot change any output column.
#   --accept-psr-coef-leak, --psr-coef-dir=DIR   see above.
#   --local-inputs=DIR    read fixtures and lineups from a torpdata data folder
#                         (e.g. C:/dev/torpverse/torpdata/data) instead of the
#                         AFL API; see hb_seed_local_inputs().

suppressMessages({
  devtools::load_all(quiet = TRUE)
  library(data.table)
})

# Arguments ----

hb_parse_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  get_opt <- function(name, default = NULL) {
    hit <- grep(paste0("^--", name, "(=|$)"), args, value = TRUE)
    if (length(hit) == 0) return(default)
    val <- sub(paste0("^--", name, "=?"), "", hit[1])
    if (!nzchar(val)) TRUE else val
  }
  parse_int_set <- function(x) {
    if (is.null(x)) return(NULL)
    # "2022:2025", "5", "3,5,7-9" -- parsed by hand, no eval().
    parts <- strsplit(x, ",", fixed = TRUE)[[1]]
    vals <- unlist(lapply(parts, function(p) {
      ends <- as.integer(strsplit(p, "[:-]")[[1]])
      if (anyNA(ends) || length(ends) > 2) cli::cli_abort("Cannot parse {.val {x}} as integers")
      if (length(ends) == 2) seq(ends[1], ends[2]) else ends
    }))
    sort(unique(vals))
  }
  opts <- list(
    seasons = parse_int_set(get_opt("seasons", "2022:2025")),
    rounds = parse_int_set(get_opt("rounds")),
    out = get_opt("out", file.path("data-raw", "outputs", "honest-backfill")),
    xgb = get_opt("xgb", "auto"),
    psr_coef_dir = get_opt("psr-coef-dir"),
    local_inputs = get_opt("local-inputs"),
    accept_psr_leak = isTRUE(get_opt("accept-psr-coef-leak", FALSE))
  )
  if (!opts$xgb %in% c("auto", "yes", "no")) {
    cli::cli_abort("--xgb must be auto, yes or no (got {.val {opts$xgb}}).")
  }
  if (is.null(opts$psr_coef_dir) && !opts$accept_psr_leak) {
    cli::cli_abort(c(
      "No as-of PSR coefficients given.",
      "x" = "inst/extdata/psr_coefficients.csv was fitted to 2021-2024 match margins, so PSR is in-sample for 2022-2024.",
      "i" = "Pass --psr-coef-dir=DIR (DIR/through_<s-1>/psr_coefficients.csv per season), or --accept-psr-coef-leak to run with the shipped file and label the rows."
    ))
  }
  if (any(opts$seasons <= MATCH_MIN_DATA_SEASON)) {
    message(sprintf(
      "WARNING: season %d is the first season of match data (from round %d); its rounds train on a handful of matches.",
      MATCH_MIN_DATA_SEASON, MATCH_MIN_DATA_ROUND))
  }
  opts
}

# Feature frame ----

# One frame per PSR coefficient source. build_team_mdl_df() takes
# psr_coef_path; osr/dsr files are read from the same folder.
hb_psr_coef_path <- function(opts, season) {
  if (is.null(opts$psr_coef_dir)) return(NULL)
  p <- file.path(opts$psr_coef_dir, paste0("through_", season - 1L), "psr_coefficients.csv")
  if (!file.exists(p)) cli::cli_abort("Missing as-of PSR coefficients for {season}: {.path {p}}")
  normalizePath(p, winslash = "/")
}

# Fixtures, results and lineups come from the AFL API, and a failed season or
# a partly failed lineup fetch does not stop build_team_mdl_df(): it warns and
# carries on with a hole (seen 2026-10-25: fixtures 2021 failed outright, and
# get_afl_lineups() returned 3 of 2021's matches and 2 of 2023's, which
# load_teams() then wrote to the permanent disk cache). A hole in the training
# seasons changes every prediction after it without failing anything, so check
# coverage here and refuse to train on a frame with one.
hb_assert_frame_coverage <- function(df, last_season) {
  seasons <- MATCH_MIN_DATA_SEASON:last_season
  done <- !is.na(df$win)
  per_season <- vapply(seasons, function(y) length(unique(df$match_id[done & df$season.x == y])), integer(1))
  # 2021 starts at round 14 (~95 matches); a full season is 198+ home-and-away.
  floor_n <- ifelse(seasons == MATCH_MIN_DATA_SEASON, 80L, 190L)
  short <- seasons[per_season < floor_n]
  thin <- done & df$season.x %in% seasons &
    (is.na(df$count.x) | df$count.x < MIN_PLAUSIBLE_LINEUP)
  message(sprintf("[frame] completed matches by season: %s",
                  paste(sprintf("%d=%d", seasons, per_season), collapse = ", ")))
  if (length(short) > 0 || any(thin)) {
    cli::cli_abort(c(
      "team_mdl_df has holes, so training would silently differ from the real history.",
      "x" = if (length(short)) "Too few completed matches for season{?s} {short}." else NULL,
      "x" = if (any(thin)) "{length(unique(df$match_id[thin]))} completed match{?es} with a missing or short lineup (count < {MIN_PLAUSIBLE_LINEUP})." else NULL,
      "i" = "Usually an AFL API fetch that failed part-way. Check ~/.torp/cache/cfs_teams_<season>.parquet for a tiny file and delete it, then rerun."
    ))
  }
  invisible(TRUE)
}

# --local-inputs=DIR: serve fixtures, results and lineups for MATCH_MIN_DATA_SEASON..
# last_season from a torpdata data folder (fixtures_<y>.parquet,
# teams_<y>.parquet) through the in-memory cache that load_fixtures(TRUE) and
# load_teams(TRUE) read, and drop later seasons from results. On 2026-10-25 the
# AFL API fixture call hung twice for 9+ minutes at zero CPU and the lineup
# fetch came back partial, so the profiling run used this. Finished seasons do
# not change, so a local copy is as good as the API for them; seasons after
# last_season are left out of every input (their lineups in a local copy may
# be stale, and a completed match with no lineup aborts the frame build).
hb_seed_local_inputs <- function(dir, last_season) {
  all_seasons <- .validate_seasons_comp(TRUE, "AFLM")
  keep <- all_seasons[all_seasons <= last_season]
  read_all <- function(stem) {
    rbindlist(lapply(keep, function(y) {
      f <- file.path(dir, sprintf("%s_%d.parquet", stem, y))
      if (!file.exists(f)) cli::cli_abort("--local-inputs: missing {.path {f}}")
      as.data.table(arrow::read_parquet(f))
    }), fill = TRUE)
  }
  key <- function(prefix) paste0(prefix, "_", paste(sort(all_seasons), collapse = "_"))
  fx <- read_all("fixtures"); tm <- read_all("teams")
  store_in_cache(key("fixtures"), tibble::as_tibble(fx))
  store_in_cache(key("teams"), tibble::as_tibble(tm))
  # Results too: on 2026-10-25 the API dropped 2021 from load_results() twice
  # ("Could not fetch fixtures for season 2021"), which the coverage check then
  # caught. results_<y>.parquet in the torpdata folder is the same concluded-
  # fixture frame get_afl_results() returns.
  res <- read_all("results")
  need <- table(factor(fx$season, levels = keep))
  got <- table(factor(res$season, levels = keep))
  if (any(got < need)) {
    cli::cli_abort("--local-inputs: results cover fewer matches than fixtures ({paste(names(got), got, '/', need, collapse = ', ')})")
  }
  store_in_cache(key("results"), tibble::as_tibble(res))
  message(sprintf("[inputs] local fixtures %d rows, lineups %d matches, results %d rows (seasons %d-%d)",
                  nrow(fx), length(unique(tm$match_id)), nrow(res), min(keep), last_season))
  invisible(TRUE)
}

hb_frame_cache <- new.env()
hb_get_frame <- function(opts, season) {
  coef_path <- hb_psr_coef_path(opts, season)
  psr_label <- coef_path %||% "shipped"
  # One frame per predicted season: PSR's standardisation SDs and the mean that
  # centres position-less rows are pooled across rounds, so they are taken from
  # seasons before `season` only (psr_ref_before_season). Pooled over the whole
  # table, as production does, 2026 would set the scale of 2022's PSR.
  key <- paste0(psr_label, "|ref<", season)
  if (!exists(key, envir = hb_frame_cache, inherits = FALSE)) {
    if (!is.null(opts$local_inputs)) hb_seed_local_inputs(opts$local_inputs, max(opts$seasons))
    t0 <- proc.time()[["elapsed"]]
    df <- build_team_mdl_df(psr_coef_path = coef_path, psr_ref_before_season = season)
    message(sprintf("[frame] built team_mdl_df (%s): %d rows, %d cols, seasons %s, %.0fs",
                    key, nrow(df), ncol(df), paste(range(df$season.x), collapse = "-"),
                    proc.time()[["elapsed"]] - t0))
    needed <- c("season.x", "round_number.x", "match_id", "utc_dt", "win",
                "score_diff", "team_type_fac.x", "harmean_shots", "weightz")
    miss <- setdiff(needed, names(df))
    if (length(miss) > 0) cli::cli_abort("team_mdl_df lacks {.val {miss}}")
    hb_assert_frame_coverage(df, max(opts$seasons))
    # Matches with no weather row get temp/wind/humidity medians taken over the
    # whole frame (.build_team_mdl_df()). Flag them -- after imputation they
    # sit exactly on all three medians -- so each round can re-impute them from
    # its training rows. (Its "Weather joined: 100%" log line is computed after
    # the imputation, so it cannot show how many there are.)
    df$.wx_imputed <- df$temp_avg == stats::median(df$temp_avg) &
      df$wind_avg == stats::median(df$wind_avg) &
      df$humidity_avg == stats::median(df$humidity_avg)
    message(sprintf("[frame] %d team-rows (%d matches) carry imputed weather",
                    sum(df$.wx_imputed), length(unique(df$match_id[df$.wx_imputed]))))
    assign(key, df, envir = hb_frame_cache)
  }
  list(df = get(key, envir = hb_frame_cache), psr_coef = psr_label)
}

# Earlier honest predictions (for calibration and finals sigma) ----

hb_season_file <- function(out, s) file.path(out, sprintf("backfill_predictions_%d.parquet", s))
# A --rounds= (profiling) run writes under _profile/, a full run under
# _partial/, so a profiling partial -- possibly made by older code or other
# settings -- is never picked up and bound into a season file.
hb_partial_dir <- function(out, s, root = "_partial") file.path(out, root, s)
hb_partial_file <- function(out, s, r, root = "_partial") file.path(hb_partial_dir(out, s, root), sprintf("round_%02d.parquet", r))

# Every honest row this output folder already holds from before (s, r), and
# kicked off before round_start (a postponed game from an earlier round that
# was played after round r began is not "earlier" in time).
hb_prior_honest <- function(out, s, r, round_start = NULL, root = "_partial") {
  files <- character(0)
  for (y in list.files(out, pattern = "^backfill_predictions_[0-9]{4}[.]parquet$")) {
    if (as.integer(gsub("\\D", "", y)) < s) files <- c(files, file.path(out, y))
  }
  pd <- hb_partial_dir(out, s, root)
  if (dir.exists(pd)) {
    for (f in list.files(pd, pattern = "^round_[0-9]+[.]parquet$")) {
      if (as.integer(gsub("\\D", "", f)) < r) files <- c(files, file.path(pd, f))
    }
  }
  if (length(files) == 0) return(NULL)
  prior <- rbindlist(lapply(files, function(f) {
    as.data.table(arrow::read_parquet(f, col_select = c("match_id", "utc_start_time", "margin", "pred_margin", "pred_margin_raw")))
  }), fill = TRUE)
  if (!is.null(round_start)) {
    started <- .parse_utc_start(prior$utc_start_time)
    if (anyNA(started)) cli::cli_abort("{sum(is.na(started))} earlier honest row{?s} with an unparseable utc_start_time")
    prior <- prior[started < round_start]
  }
  if (nrow(prior) == 0) NULL else prior
}

# Margin scale b from earlier honest raw margins: same fit and gate as
# fit_match_margin_calibration() (through-origin b, intercept slope for the
# gate), on predictions that were genuinely out of sample when made.
hb_margin_calibration <- function(prior) {
  out <- list(b = 1, slope_raw = NA_real_, n = 0L, why = "no earlier honest rows")
  if (is.null(prior)) return(out)
  d <- prior[is.finite(margin) & is.finite(pred_margin_raw)]
  out$n <- nrow(d)
  if (nrow(d) < MATCH_RECAL_MIN_N) {
    out$why <- sprintf("cold start (%d < %d matches)", nrow(d), MATCH_RECAL_MIN_N)
    return(out)
  }
  out$slope_raw <- unname(coef(lm(margin ~ pred_margin_raw, data = d))[2])
  if (!.margin_slope_ok(out$slope_raw)) {
    out$why <- sprintf("slope gate failed (raw slope %.3f)", out$slope_raw)
    return(out)
  }
  b <- unname(coef(lm(margin ~ pred_margin_raw + 0, data = d))[1])
  if (!is.finite(b)) { out$why <- "non-finite b"; return(out) }
  out$b <- b
  out$why <- sprintf("fitted on %d earlier honest matches", nrow(d))
  out
}

# Weights as production computes them live before round r ----

hb_reweight <- function(d, anchor_date, train_mask) {
  w <- exp(as.numeric(-(anchor_date - as.Date(d$utc_dt))) / MATCH_WEIGHT_DECAY_DAYS)
  w <- w / mean(w[train_mask & is.finite(d$score_diff)], na.rm = TRUE)
  d$weightz <- w
  d$shot_weightz <- (d$harmean_shots / mean(d$harmean_shots[train_mask], na.rm = TRUE)) * w
  d
}

# One round ----

hb_predict_round <- function(frame, s, r, opts, prior = NULL, part_root = "_partial") {
  t_start <- proc.time()[["elapsed"]]
  df <- frame$df
  keep <- df$season.x < s | (df$season.x == s & df$round_number.x <= r)
  d <- df[keep, ]
  round_rows <- d$season.x == s & d$round_number.x == r
  if (!any(round_rows)) cli::cli_abort("No rows for {s} round {r}")
  round_start <- min(d$utc_dt[round_rows], na.rm = TRUE)
  if (missing(prior)) prior <- hb_prior_honest(opts$out, s, r, round_start, part_root)

  train_filter <- (d$season.x < s | (d$season.x == s & d$round_number.x < r)) &
    d$utc_dt < round_start
  train_filter[is.na(train_filter)] <- FALSE
  # The two guarantees the whole exercise rests on.
  stopifnot(!any(train_filter & round_rows))
  stopifnot(all(d$utc_dt[train_filter] < round_start))
  trained_on <- train_filter & !is.na(d$win)
  n_train_matches <- length(unique(d$match_id[trained_on]))
  last_s <- max(d$season.x[trained_on])
  last_r <- max(d$round_number.x[trained_on & d$season.x == last_s])
  train_through <- sprintf("%d R%d", last_s, last_r)

  d <- hb_reweight(d, as.Date(round_start), train_filter)
  # Model 1's target is total_xpoints_adj = total_xpoints * scale, and
  # .build_team_mdl_df() sizes the scale over the whole frame -- later seasons
  # included. Re-size it from the training rows, as production would live.
  xp_scale <- mean(d$total_points[train_filter], na.rm = TRUE) /
    mean(d$total_xpoints[train_filter], na.rm = TRUE)
  if (!is.finite(xp_scale)) cli::cli_abort("Cannot size the xpoints scale for {s} R{r}")
  d$total_xpoints_adj <- d$total_xpoints * xp_scale
  # Re-impute missing weather from the training rows' medians.
  if (any(d$.wx_imputed)) {
    src <- train_filter & !d$.wx_imputed
    for (v in c("temp_avg", "wind_avg", "humidity_avg")) {
      d[[v]][d$.wx_imputed] <- stats::median(d[[v]][src], na.rm = TRUE)
    }
    d$log_wind <- log1p(d$wind_avg)
  }

  t0 <- proc.time()[["elapsed"]]
  gam_result <- .train_match_gams(d, train_filter = train_filter)
  d <- gam_result$data
  t_gam <- proc.time()[["elapsed"]] - t0

  use_xgb <- switch(opts$xgb, yes = TRUE, no = FALSE, auto = MATCH_BLEND_WEIGHT < 1)
  t_xgb <- 0
  xgb_ok <- FALSE
  if (use_xgb) {
    t0 <- proc.time()[["elapsed"]]
    xgb_result <- tryCatch(.train_match_xgb(d, train_filter = train_filter), error = function(e) {
      message(sprintf("[%d R%d] XGBoost failed (%s); GAM-only, as production does", s, r, conditionMessage(e)))
      NULL
    })
    t_xgb <- proc.time()[["elapsed"]] - t0
    if (!is.null(xgb_result)) { d <- xgb_result$data; xgb_ok <- TRUE }
  }
  # Same arithmetic as build_prediction_state()'s blend block.
  if (xgb_ok) {
    d$pred_tot_xscore  <- .blend_gam_xgb(d$gam_pred_tot_xscore,  d$xgb_pred_tot_xscore)
    d$pred_xscore_diff <- .blend_gam_xgb(d$gam_pred_xscore_diff, d$xgb_pred_xscore_diff)
    d$pred_score_diff  <- .blend_gam_xgb(d$gam_pred_score_diff,  d$xgb_pred_score_diff)
    d$pred_win <- predict(gam_result$models$win, newdata = d, type = "response")
  } else {
    d$pred_tot_xscore  <- d$gam_pred_tot_xscore
    d$pred_xscore_diff <- d$gam_pred_xscore_diff
    d$pred_score_diff  <- d$gam_pred_score_diff
    d$pred_win         <- d$gam_pred_win
  }

  calib <- hb_margin_calibration(prior)
  d$pred_score_diff <- apply_match_margin_calibration(d$pred_score_diff, list(b = calib$b))

  # .reconcile_win_with_margin() sizes its sigma from every row with a result.
  # Live, those are the training rows; here the frame also holds round r's
  # own results, so hide them for the sizing and keep only the pred_win it sets.
  tmp <- d
  tmp$score_diff[!train_filter] <- NA_real_
  tmp <- .reconcile_win_with_margin(tmp)
  d$pred_win <- tmp$pred_win
  rm(tmp)

  preds <- .format_match_preds(d[round_rows, ])
  sigma <- if (is.null(prior)) NA_real_ else .residual_sd(prior$margin, prior$pred_margin)
  sigma_used <- if (is.finite(sigma)) sigma else FINALS_MARGIN_SIGMA_FALLBACK
  preds <- .finals_win_from_margin(preds, sigma = sigma)

  # Production's validation checks, reported rather than fatal: a backfill
  # row that fails them is still worth keeping and inspecting.
  bad <- c(
    if (anyNA(preds$pred_win)) "NA pred_win",
    if (anyNA(preds$pred_margin)) "NA pred_margin",
    if (any(preds$pred_xtotal < 100 | preds$pred_xtotal > 250, na.rm = TRUE)) "pred_xtotal outside 100-250",
    if (any(abs(preds$pred_margin) > 1 & abs(preds$pred_win - 0.5) > 0.02 &
            sign(preds$pred_margin) != sign(preds$pred_win - 0.5), na.rm = TRUE)) "margin/win direction disagreement"
  )
  if (length(bad) > 0) message(sprintf("[%d R%d] VALIDATION: %s", s, r, paste(bad, collapse = "; ")))

  b <- calib$b
  out <- as.data.table(preds)
  out[, `:=`(
    pred_margin_raw = pred_margin / b,
    generated_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    train_through = train_through,
    n_train_matches = n_train_matches,
    margin_calib_b = b,
    margin_calib_note = calib$why,
    finals_sigma = sigma_used,
    model_spec = if (xgb_ok) "gam+xgb" else "gam",
    psr_coef = if (frame$psr_coef == "shipped") "shipped psr_coefficients.csv (fitted on 2021-2024: in-sample for 2022-2024)" else frame$psr_coef
  )]
  data.table::setnames(out, "round", "week")
  out[, season := NULL]
  data.table::setcolorder(out, "week")

  timing <- list(gam = t_gam, xgb = t_xgb, total = proc.time()[["elapsed"]] - t_start)
  message(sprintf("[%d R%d] %d matches predicted; trained on %d matches through %s; b=%.3f (%s); GAM %.0fs, XGB %.0fs, round %.0fs",
                  s, r, nrow(out), n_train_matches, train_through, b, calib$why,
                  t_gam, t_xgb, timing$total))
  attr(out, "timing") <- timing
  attr(out, "state") <- list(d = d, round_rows = round_rows, train_filter = train_filter)
  out
}

# Season loop ----

hb_run <- function(opts) {
  dir.create(opts$out, recursive = TRUE, showWarnings = FALSE)
  message(sprintf("Output folder: %s", normalizePath(opts$out, winslash = "/")))
  profile_only <- !is.null(opts$rounds)

  for (s in opts$seasons) {
    sf <- hb_season_file(opts$out, s)
    if (!profile_only && file.exists(sf)) {
      message(sprintf("[%d] %s exists -- skipping (delete it to redo)", s, basename(sf)))
      next
    }
    frame <- hb_get_frame(opts, s)
    df <- frame$df
    done_rounds <- sort(unique(df$round_number.x[df$season.x == s & !is.na(df$win)]))
    rounds <- if (profile_only) intersect(done_rounds, opts$rounds) else done_rounds
    if (length(rounds) == 0) { message(sprintf("[%d] no completed rounds to predict", s)); next }
    part_root <- if (profile_only) "_profile" else "_partial"
    dir.create(hb_partial_dir(opts$out, s, part_root), recursive = TRUE, showWarnings = FALSE)

    failed <- character(0)
    for (r in rounds) {
      pf <- hb_partial_file(opts$out, s, r, part_root)
      if (file.exists(pf)) {
        # Reuse only a partial made with the same PSR source and model spec.
        had <- unique(arrow::read_parquet(pf, col_select = c("psr_coef", "model_spec")))
        want_spec <- if (switch(opts$xgb, yes = TRUE, no = FALSE, auto = MATCH_BLEND_WEIGHT < 1)) "gam+xgb" else "gam"
        if (nrow(had) != 1 || had$psr_coef != frame$psr_coef || had$model_spec != want_spec) {
          cli::cli_abort("{.path {pf}} was made with other settings ({had$psr_coef[1]}, {had$model_spec[1]}); delete it to redo the round.")
        }
        message(sprintf("[%d R%d] partial exists -- skipping", s, r)); next
      }
      res <- tryCatch(hb_predict_round(frame, s, r, opts, part_root = part_root), error = function(e) {
        message(sprintf("[%d R%d] FAILED: %s", s, r, conditionMessage(e)))
        NULL
      })
      if (is.null(res)) { failed <- c(failed, as.character(r)); next }
      attr(res, "state") <- NULL
      arrow::write_parquet(res, pf)
      rm(res); invisible(gc(verbose = FALSE))
    }

    if (profile_only) next

    # Season file: bind the partials, then check coverage against the
    # completed matches the frame holds for this season.
    parts <- list.files(hb_partial_dir(opts$out, s, part_root), pattern = "^round_[0-9]+[.]parquet$", full.names = TRUE)
    season_dt <- rbindlist(lapply(parts, function(f) as.data.table(arrow::read_parquet(f))), fill = TRUE)
    setorder(season_dt, week, match_id)
    expected <- length(unique(df$match_id[df$season.x == s & !is.na(df$win)]))
    message(sprintf("[%d] %d of %d completed matches predicted (%d round(s) failed: %s)",
                    s, nrow(season_dt), expected, length(failed),
                    if (length(failed)) paste(failed, collapse = ", ") else "none"))
    if (anyDuplicated(season_dt$match_id)) cli::cli_abort("Duplicate match_id in {s} -- refusing to write {.path {sf}}")
    if (nrow(season_dt) == 0) { message(sprintf("[%d] nothing to write", s)); next }
    arrow::write_parquet(season_dt, sf)
    if (length(failed) > 0) {
      fwrite(data.table(season = s, failed_round = failed), file.path(opts$out, sprintf("failed_rounds_%d.csv", s)))
    }
    message(sprintf("[%d] wrote %s (%d rows)", s, sf, nrow(season_dt)))
  }
  invisible(TRUE)
}

# Run only when executed (Rscript), not when sourced by a profiling harness.
if (sys.nframe() == 0L) {
  options(torp.local_data_dir = NA)  # releases, never a possibly stale local torpdata copy
  hb_run(hb_parse_args())
}
