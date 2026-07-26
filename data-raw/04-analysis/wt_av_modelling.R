# Setup ----
# Canonical trainer for the 58 per-stat GAMs (stat-models release tag) --
# TRAINING-CONSOLIDATION-PLAN.md Non-goal 3's deferred provenance/manifest
# extension, closed 2026-07-26. Everything below "Combine Predictions" is
# unrelated exploratory/manual analysis, left as-is.
library(purrr)
library(tidyverse)
# library(fitzRoy)  # replaced by internal AFL API functions
devtools::load_all()

torpmodels_root <- if (file.exists("torpmodels/DESCRIPTION")) {
  "torpmodels"
} else if (file.exists("../torpmodels/DESCRIPTION")) {
  "../torpmodels"
} else {
  NULL
}
if (is.null(torpmodels_root)) {
  cli::cli_abort("torpmodels not found (checked torpmodels/DESCRIPTION, ../torpmodels/DESCRIPTION) -- required for stamp_model_meta()/publish_stat_models().")
}
devtools::load_all(torpmodels_root)

szns <- 2021:get_afl_season()
# temporal-holdout CV metric: train on seasons before this, score on it.
# Deliberately the CURRENT (possibly in-progress) season, not "the last
# COMPLETED season" the way WP/match-margin do it -- this script is re-run
# ad hoc through a season, and holding out whatever of the current season
# has been played so far is more useful than a stale prior-season number.
# The nrow()==0 guard in each loop degrades to cv_metric=NA if the current
# season has no rows yet (e.g. run in the off-season).
holdout_season <- get_afl_season()

# RMSE that tolerates the NA predictions a re/factor-smooth term can produce
# for an unseen level in the held-out season (e.g. a venue/day not present
# pre-holdout) -- ModelMetrics::rmse() has no na.rm.
.safe_rmse <- function(actual, predicted) {
  ok <- !is.na(actual) & !is.na(predicted)
  if (!any(ok)) return(NA_real_)
  sqrt(mean((actual[ok] - predicted[ok])^2))
}

minmax <- function(x, na.rm = TRUE) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# fil_val <- 24
decay <- 365

tictoc::tic()

pl_details <- get_afl_player_details(get_afl_season())

# ps24 <- fetch_player_stats_afl(2024) %>%
#   janitor::remove_constant() %>%
#   janitor::clean_names()
# ps23 <- fetch_player_stats_afl(2023) %>%
#   janitor::remove_constant() %>%
#   janitor::clean_names()
# ps22 <- fetch_player_stats_afl(2022) %>%
#   janitor::remove_constant() %>%
#   janitor::clean_names()
# ps21 <- fetch_player_stats_afl(2021) %>%
#   janitor::remove_constant() %>%
#   janitor::clean_names()
# ps20 <- fetch_player_stats_afl(2020) %>%
#   janitor::remove_constant() %>%
#   janitor::clean_names()
# pstot <- bind_rows(ps20, ps21, ps22, ps23, ps24)

pstot <- load_player_stats(TRUE)

cols <- colnames(pstot)[18:79]
cols <- cols[!cols %in% c(
  "dream_team_points", "rating_points", "metres_gained", "last_updated",
  "centre_bounce_attendances", "kickins", "kickins_playon"
)]

cols_binom <- c(
  "time_on_ground_percentage", "disposal_efficiency", "goal_accuracy",
  "kick_efficiency", "contested_possession_rate", "hitout_win_percentage",
  "hitout_to_advantage_rate", "contest_def_loss_percentage", "contest_off_wins_percentage"
)

# Guard against columns with no usable data at all (e.g. "ranking" is 100%
# NA/logical in the current AFL API response, confirmed live 2026-07-26) --
# mgcv::bam() hard-errors ("Not enough (non-NA) data to do anything
# meaningful") rather than degrading gracefully, which previously crashed
# the whole run partway through the loop with zero models published (the
# publish step only runs after both loops fully complete). Drop any column
# below a minimal non-NA threshold before the loop starts, rather than
# discovering it mid-run. Applied to cols_binom too (a separate hardcoded
# literal, not derived from cols) -- a filter that only reached cols_pois
# would silently do nothing if one of the 9 binomial stats went all-NA.
.insufficient_data <- function(col_names) vapply(col_names, function(col) sum(!is.na(pstot[[col]])) < 100, logical(1))
.bad_cols <- .insufficient_data(cols)
.bad_binom <- .insufficient_data(cols_binom)
if (any(.bad_cols) || any(.bad_binom)) {
  cli::cli_warn("Excluding stat column(s) with insufficient non-NA data: {paste(c(cols[.bad_cols], cols_binom[.bad_binom]), collapse = ', ')}")
  cols <- cols[!.bad_cols]
  cols_binom <- cols_binom[!.bad_binom]
}

cols_pois <- setdiff(cols, cols_binom)

# Weighted Average Function ----
wav_data <- function(df, fil_val, model_col, decay = 365) {
  df$season_round <- paste0(substr(df$match_id, 5, 8), substr(df$match_id, 12, 13))

  df$opponent_name <- ifelse(df$team_status == "home", df$away_team_name, df$home_team_name)
  # team_name no longer comes back directly from load_player_stats() (schema
  # drift since this script was last run) -- derive it the same way as
  # opponent_name, just the other side of team_status.
  df$team_name <- ifelse(df$team_status == "home", df$home_team_name, df$away_team_name)

  df_old <- df %>%
    filter(season_round < fil_val) %>%
    mutate(weight_gm = exp(as.numeric(-(max(as.Date(.data$utc_start_time)) - as.Date(.data$utc_start_time))) / decay))

  df_cur <- df %>% filter(season_round == fil_val)

  ###
  df_player <-
    df_old %>%
    group_by(
      player_id = player_id,
    ) %>%
    summarise(
      wt_avg = round(sum(.data[[model_col]] * .data$weight_gm, na.rm = T) / sum(.data$weight_gm, na.rm = T), 3),
      wt_avg = replace_na(wt_avg, 0),
      log_wt_avg = log(wt_avg + 1),
      wt_gms = sum(.data$weight_gm, na.rm = T)
    ) %>%
    ungroup()

  ###
  df_team <-
    df_old %>%
    group_by(team_name) %>%
    summarise(
      wt_avg_team = round(sum(.data[[model_col]] * .data$weight_gm, na.rm = T) / sum(.data$weight_gm, na.rm = T), 3),
      wt_avg_team = replace_na(wt_avg_team, 0),
      log_wt_avg_team = log(wt_avg_team + 1),
      wt_gms_team = sum(.data$weight_gm, na.rm = T)
    ) %>%
    ungroup()

  ###
  df_opp <-
    df_old %>%
    group_by(opponent_name) %>%
    summarise(
      wt_avg_opp = round(sum(.data[[model_col]] * .data$weight_gm, na.rm = T) / sum(.data$weight_gm, na.rm = T), 3),
      wt_avg_opp = replace_na(wt_avg_opp, 0),
      log_wt_avg_opp = log(wt_avg_opp + 1),
      wt_gms_opp = sum(.data$weight_gm, na.rm = T)
    ) %>%
    ungroup()

  ###########
  df_tot <-
    df_player %>%
    right_join(df_cur, by = c("player_id" = "player_id")) %>%
    left_join(df_team, by = c("team_name" = "team_name")) %>%
    left_join(df_opp, by = c("opponent_name" = "opponent_name")) %>%
    mutate(
      round = round_number,
      season = as.factor(substr(match_id, 5, 8)),
      venue = as.factor(venue_name),
      aest_start = with_tz(as_datetime(utc_start_time), "Australia/Brisbane"),
      date_numeric = as.numeric(aest_start),
      aest_hour = (hour(aest_start) * 60 + minute(aest_start)) / 60,
      aest_day = wday(aest_start, label = TRUE),
      # lineup_position no longer comes back from load_player_stats() either
      # (same schema drift as team_name above), but position is the same raw
      # granular lineup code lineup_position used to be (BPL/BPR/CHB/CHF/...,
      # plus EMERG/INT/SUB -- confirmed via a live table() check), so the
      # same substr(.,1,2) truncation applies to the new field name and
      # reproduces the exact old collapsing (EMERG->EM, INT->IN, etc),
      # including what the position != "EM" filter below depends on.
      position = as.factor(substr(position, 1, 2)),
      home_away = as.factor(team_status),
      player_name = paste(given_name, surname)
    ) %>%
    filter(
      position != "EM",
      !is.na(position)
    ) %>%
    relocate(any_of(model_col), position)

  return(df_tot)
}


# Fit Poisson Models ----
stat_list <- list()
stat_model_files <- character(0)

stat_formula_str <- paste0(
  "%s ~ ti(log_wt_avg,wt_gms, bs = 'ts') + s(log_wt_avg, bs='ts') + s(wt_gms, bs='ts')",
  "+ s(position,bs='re') + s(log_wt_avg_team, bs='ts') + s(log_wt_avg_opp, bs='ts') + s(home_away, bs='re')",
  "+ s(venue, bs='re') + s(round, bs='ts') + s(date_numeric, bs='ts', m=1) + s(aest_day, bs='re') + s(aest_hour, bs='ts')"
)
stat_feature_names <- c(
  "log_wt_avg", "wt_gms", "position", "log_wt_avg_team", "log_wt_avg_opp",
  "home_away", "venue", "round", "date_numeric", "aest_day", "aest_hour"
)

tictoc::tic()
for (i in cols_pois) {
  # One stat's failure (e.g. the "ranking" all-NA landmine that crashed a
  # prior real run 2026-07-26 with zero models published, since publish only
  # runs after the whole loop completes) must not take down the other ~57.
  tryCatch({
    df_mdl <- purrr::map(
      paste0(rep(szns, each = 29), rep(sprintf("%02d", 0:28), times = length(szns))),
      ~ wav_data(pstot,
        fil_val = .,
        model_col = i
      )
    ) %>%
      purrr::list_rbind()

    fml <- as.formula(sprintf(stat_formula_str, i))

    # Temporal-holdout CV metric (mirrors the pattern already established for
    # WP/match-margin): fit on all-but-holdout_season, score the held-out
    # season OOS. A second, separate fit from the production model below --
    # this is why the loop roughly doubles in runtime vs the pre-provenance
    # version.
    train_df <- df_mdl %>% dplyr::filter(as.integer(as.character(season)) < holdout_season)
    test_df <- df_mdl %>% dplyr::filter(as.integer(as.character(season)) == holdout_season)
    cv_metric <- tryCatch({
      if (nrow(train_df) == 0 || nrow(test_df) == 0) {
        NA_real_
      } else {
        mdl_cv <- mgcv::bam(fml, data = train_df, family = poisson(), select = T, discrete = T, nthreads = 4)
        preds_cv <- mgcv::predict.bam(mdl_cv, newdata = test_df, type = "response")
        .safe_rmse(test_df[[i]], preds_cv)
      }
    }, error = function(e) {
      cli::cli_warn("Temporal-holdout CV fit failed for {i}: {conditionMessage(e)}")
      NA_real_
    })

    mdl <- mgcv::bam(
      fml,
      data = df_mdl, family = poisson(),
      select = T, discrete = T, nthreads = 4,
    )
    mdl <- stamp_model_meta(mdl, build_model_meta(
      model_name = i, seasons = szns,
      params = list(family = "poisson", select = TRUE, discrete = TRUE, decay = decay),
      feature_names = stat_feature_names, cv_metric = cv_metric,
      n_rows = nrow(df_mdl), n_matches = length(unique(df_mdl$match_id)),
      extra = list(script = "data-raw/04-analysis/wt_av_modelling.R", cv_metric_type = "temporal_holdout_rmse")
    ))

    model_preds <- tibble(
      player_id = df_mdl$player_id,
      player_name = df_mdl$player_name,
      player_position = df_mdl$position,
      match_id = df_mdl$match_id,
      round = df_mdl$round_number,
      home_away = df_mdl$home_away,
      team_name = df_mdl$team_name,
      opp_name = df_mdl$opponent_name,
      "{i}" := df_mdl[[i]],
      "pred_{i}" := mgcv::predict.bam(mdl, newdata = df_mdl, type = "response"),
      "wt_avg_{i}" := df_mdl$wt_avg,
      "wt_gms_{i}" := df_mdl$wt_gms,
      "wt_avg_team_{i}" := df_mdl$wt_avg_team,
      "wt_avg_opp_{i}" := df_mdl$wt_avg_opp
    )

    stat_list[[i]] <- model_preds

    saveRDS(mdl, glue::glue("./data-raw/stat-models/{i}.rds"))
    stat_model_files <- c(stat_model_files, paste0(i, ".rds"))
    print(i)
  }, error = function(e) {
    cli::cli_warn("Skipping stat {.val {i}} -- fitting failed: {conditionMessage(e)}")
  })
}

tictoc::toc()

# Fit Binomial Models ----
# stat_list_binom <- list()

stat_formula_str_binom <- paste0(
  "%s ~ ti(log_wt_avg,wt_gms, bs = 'ts') + s(log_wt_avg, bs='ts') + s(wt_gms, bs='ts')",
  "+ s(position,bs='re') + s(log_wt_avg_team, bs='ts', k=4) + s(log_wt_avg_opp, bs='ts', k=4) + s(home_away, bs='re')",
  "+ s(venue, bs='re') + s(round, bs='ts') + s(date_numeric, bs='ts', m=1) + s(aest_day, bs='re') + s(aest_hour, bs='ts')"
)

tictoc::tic()
for (i in cols_binom) {
  # See the Poisson loop above -- one stat's failure must not take down the
  # other ~57.
  tryCatch({
    df_mdl <- purrr::map(
      paste0(rep(szns, each = 29), rep(sprintf("%02d", 0:28), times = length(szns))),
      ~ wav_data(
        pstot %>%
          mutate("{i}" := .data[[i]] / 100),
        fil_val = .,
        model_col = i
      )
    ) %>%
      purrr::list_rbind()

    fml <- as.formula(sprintf(stat_formula_str_binom, i))

    # Temporal-holdout CV metric -- see the Poisson loop above for rationale.
    train_df <- df_mdl %>% dplyr::filter(as.integer(as.character(season)) < holdout_season)
    test_df <- df_mdl %>% dplyr::filter(as.integer(as.character(season)) == holdout_season)
    cv_metric <- tryCatch({
      if (nrow(train_df) == 0 || nrow(test_df) == 0) {
        NA_real_
      } else {
        mdl_cv <- mgcv::bam(fml, data = train_df, family = binomial(), select = T, discrete = T, nthreads = 4)
        preds_cv <- mgcv::predict.bam(mdl_cv, newdata = test_df, type = "response")
        .safe_rmse(test_df[[i]], preds_cv)
      }
    }, error = function(e) {
      cli::cli_warn("Temporal-holdout CV fit failed for {i}: {conditionMessage(e)}")
      NA_real_
    })

    mdl <- mgcv::bam(
      fml,
      data = df_mdl, family = binomial(),
      select = T, discrete = T, nthreads = 4,
    )
    mdl <- stamp_model_meta(mdl, build_model_meta(
      model_name = i, seasons = szns,
      params = list(family = "binomial", select = TRUE, discrete = TRUE, decay = decay),
      feature_names = stat_feature_names, cv_metric = cv_metric,
      n_rows = nrow(df_mdl), n_matches = length(unique(df_mdl$match_id)),
      extra = list(script = "data-raw/04-analysis/wt_av_modelling.R", cv_metric_type = "temporal_holdout_rmse")
    ))

    model_preds <- tibble(
      player_id = df_mdl$player_id,
      player_name = df_mdl$player_name,
      player_position = df_mdl$position,
      match_id = df_mdl$match_id,
      round = df_mdl$round_number,
      home_away = df_mdl$home_away,
      team_name = df_mdl$team_name,
      opp_name = df_mdl$opponent_name,
      "{i}" := df_mdl[[i]],
      "pred_{i}" := mgcv::predict.bam(mdl, newdata = df_mdl, type = "response"),
      "wt_avg_{i}" := df_mdl$wt_avg,
      "wt_gms_{i}" := df_mdl$wt_gms,
      "wt_avg_team_{i}" := df_mdl$wt_avg_team,
      "wt_avg_opp_{i}" := df_mdl$wt_avg_opp
    )

    stat_list[[i]] <- model_preds

    saveRDS(mdl, glue::glue("./data-raw/stat-models/{i}.rds"))
    stat_model_files <- c(stat_model_files, paste0(i, ".rds"))
    print(i)
  }, error = function(e) {
    cli::cli_warn("Skipping stat {.val {i}} -- fitting failed: {conditionMessage(e)}")
  })
}

tictoc::toc()

# Publish stat models (provenance + manifest) ----
# NOTE: n_expected deliberately has no leading dot -- cli::cli_warn()'s
# glue-style interpolation reserves {.foo} for its own inline-markup classes
# (e.g. {.val}, {.file}), so a variable named with a leading dot inside a
# cli string is misparsed as an unknown style and hard-errors ("Invalid cli
# literal") instead of interpolating. This crashed a real ~2.4-hour run
# AFTER all 53 stats had already trained successfully, right before the
# publish step -- costing nothing to retrain (files were all on disk) but
# wasting the whole run's wall time until noticed.
n_expected <- length(cols_pois) + length(cols_binom)
if (length(stat_model_files) < n_expected) {
  cli::cli_warn("{length(stat_model_files)}/{n_expected} stats actually trained -- {n_expected - length(stat_model_files)} skipped due to a per-stat fitting failure (see warnings above).")
}

# publish_stat_models() warns-and-continues on individual upload failures
# rather than aborting the whole batch (58 independent files, not an atomic
# sidecar-pair group) -- but that means a partial failure is easy to miss
# among ~58 stats' worth of tic/toc/print(i) console output unless the
# caller actually checks the result, so check it here. This only compares
# against stat_model_files (what was actually trained), not n_expected --
# a training skip above is already warned about, this catches a SEPARATE
# upload failure on top of that.
uploaded_stat_models <- publish_stat_models(stat_model_files, dir = "./data-raw/stat-models")
if (length(uploaded_stat_models) < length(stat_model_files)) {
  cli::cli_abort("Only {length(uploaded_stat_models)}/{length(stat_model_files)} trained stat models actually published -- see warnings above for which failed.")
}

# Combine Predictions ----
pred_df <- stat_list %>% reduce(left_join, by = c(
  "player_id", "player_name", "player_position",
  "match_id", "round", "home_away", "team_name", "opp_name"
))
pred_df

arrow::write_parquet(pred_df, "./data-raw/outputs/stat_pred_df.parquet")
tictoc::toc()

pred_df <- arrow::read_parquet("./data-raw/outputs/stat_pred_df.parquet")

model_val <- "goals"
pred_model_val <- paste0("pred_", model_val)
wt_avg_model_val <- paste0("wt_avg_", model_val)

mdl <- readRDS(paste0("./data-raw/stat-models/", model_val, ".rds"))
summary(mdl)
mixedup::extract_random_effects(mdl) %>%
  arrange(-value) %>%
  View()
# plot(mgcViz::getViz(mdl))


# Model Validation ----
stat_perf <- function(var) {
  model_val <- var
  pred_model_val <- paste0("pred_", model_val)
  wt_avg_model_val <- paste0("wt_avg_", model_val)
  sim_model_val <- paste0("sim_", model_val)

  test_df <- pred_df %>% filter(!is.na(.data[[pred_model_val]]))
  test_df <- test_df %>%
    mutate("sim_{model_val}" := rpois(nrow(test_df), .data[[pred_model_val]]))

  df <- tibble(
    var = paste(var),
    sim_rmse = ModelMetrics::rmse(test_df %>% pull(sim_model_val), test_df %>% pull(pred_model_val)),
    model_rmse = ModelMetrics::rmse(test_df %>% pull(model_val), test_df %>% pull(pred_model_val)),
    wt_avg_rmse = ModelMetrics::rmse(test_df %>% pull(model_val), test_df %>% pull(wt_avg_model_val)),
    naive_rmse = ModelMetrics::rmse(test_df %>% pull(model_val), rep(mean(test_df %>% pull(model_val)), nrow(test_df)))
  )

  return(df)
}

all_perf <- map(c(cols_pois, cols_binom), ~ stat_perf(.)) %>% list_rbind()

# Team-Level Predictions ----
team_preds <- pred_df %>%
  group_by(match_id, team_name, opp_name) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) # %>% View()

team_mdl_df <- team_mdl_df %>%
  # mutate(team_name_adj = torp_replace_teams(team_name)) %>%
  left_join(team_preds %>% mutate(team_name_adj = torp_replace_teams(team_name)),
    by = c("match_id", "team_name_adj.x" = "team_name")
  ) # %>% View()

colnames(team_mdl_df)[str_detect(colnames(team_mdl_df), "pred")]

# Player-Level Analysis ----
pl_df_final <-
  pl_details %>%
  select(providerId, position) %>%
  left_join(pred_df %>% filter(substr(match_id, 1, 13) == "CD_M202301423"),
    by = c("providerId" = "player_id")
  ) # %>%
# filter(position == "RUCK") %>%
# arrange(-pred_val) %>%
# relocate(pred_val) #%>%
# select(1:10) %>%
# group_by(position) %>%
# summarise(sqrt(var(pred_val, na.rm=T))) %>%
# View()

pl_df_final %>%
  group_by(position) %>%
  summarise(across(starts_with("pred"), ~ mean(.x, na.rm = T))) %>%
  view()


tst_df <- pred_df %>% filter(substr(match_id,1,8)=='CD_M2025',round == 4) %>% select(1:50)
View(tst_df)
