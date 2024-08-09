# Load necessary libraries
library(fitzRoy)
library(tidyverse)
library(brms)
library(future)
library(parallelly)
library(janitor)
devtools::load_all()

tictoc::tic()

# Set up parallel processing
available_cores <- parallelly::availableCores() - 2
plan(multisession, workers = available_cores) # Dynamically set the number of workers

# Fetch player statistics
# player_stats <- fitzRoy::fetch_player_stats_afltables(2016:2024)
player_stats <- fitzRoy::fetch_player_stats_fryzigg(2012:2024) # 2012 for advanced stats, 2003 for basics
# maybe split 2012:2017 and 2018:2024 and do two regressions

# Prepare the data for modeling
player_game_stats <- player_stats %>%
  group_by(player_id) %>%
  mutate(
    tot_tog = sum(as.integer(time_on_ground_percentage) / 100, na.rm = TRUE),
    ID2 = case_when(
      tot_tog < 5 ~ "05lt",
      tot_tog < 10 ~ "10lt",
      tot_tog < 15 ~ "15lt",
      tot_tog < 20 ~ "20lt",
      TRUE ~ as.character(player_id)
    )
  ) %>%
  ungroup() %>%
  mutate(
    score_diff = match_home_team_score - match_away_team_score,
    time_on_ground_percentage = ifelse(match_home_team == player_team, as.integer(time_on_ground_percentage) / 100, -as.integer(time_on_ground_percentage) / 100)
  ) %>%
  # select(Season, Round, Home.team, Away.team, ID = ID2, Player, Time.on.Ground, score_diff)
  select(match_id, match_home_team, match_away_team, ID = ID2, player_first_name, player_last_name, time_on_ground_percentage, score_diff)

# Convert the data to a wide format where each row represents a game, and each column represents a player's performance
wide_data <- player_game_stats %>%
  select(match_id, match_home_team, match_away_team, ID, time_on_ground_percentage, score_diff) %>%
  pivot_wider(names_from = ID, values_from = time_on_ground_percentage, values_fn = sum) %>%
  janitor::clean_names()

# Handle missing values
wide_data[is.na(wide_data)] <- 0

# Prepare the design matrix and response variable
X <- as.matrix(wide_data %>% select(-any_of(c(
  "season", "round", "home_team", "away_team", "score_diff",
  "match_id", "match_home_team", "match_away_team"
))))
y <- wide_data$score_diff

# Prepare the data frame for brms
df_brms <- wide_data %>%
  select(-any_of(c(
    "season", "round", "home_team", "away_team", "pred_diff",
    "match_id", "match_home_team", "match_away_team"
  ))) %>%
  rename(score_diff = score_diff)

# Define the model formula
formula <- bf(score_diff ~ 0 + ., center = FALSE)

# Define the prior distributions (ridge regression equivalent)
priors <- c(
  prior(normal(0, 5), class = "b"),
  prior(cauchy(0, 2.5), class = "sigma")
)

# Fit the Bayesian ridge regression model with reduced iterations and chains
fit <- brm(formula, data = df_brms, prior = priors, family = gaussian(), chains = 2, iter = 1000, cores = available_cores, verbose = TRUE)

# Save model
saveRDS(fit,'./data-raw/bayes_rapm')

# Load model
fit <- readRDS("./data-raw/bayes_rapm")

# Extract posterior samples using as_draws
posterior_samples <- as_draws_df(fit)

# Get the means of the coefficients and retain their names
coef_means <- posterior_samples %>%
  select(starts_with("b_")) %>%
  summarise(across(everything(), mean))

# Convert to a named numeric vector
coef_means_vector <- as.numeric(coef_means)
names(coef_means_vector) <- colnames(coef_means)

# Create a data frame of coefficients
rapm_df <- data.frame(
  ID = names(coef_means_vector),
  RAPM = coef_means_vector
)

# Clean up ID names
rapm_df$ID <- gsub("^b_x", "", rapm_df$ID)
rapm_df$ID <- gsub("_", "", rapm_df$ID)

# Merge with player information
player_info <- player_game_stats %>%
  group_by(ID) %>%
  summarize(player = max(paste(player_first_name, player_last_name)), gmz = sum(abs(time_on_ground_percentage), na.rm = TRUE))

rapm_df <- left_join(rapm_df, player_info, by = "ID")

# View the RAPM values
View(rapm_df)

# Predict the point differentials
pred_diff <- predict(fit, newdata = df_brms, type = "response")

# Add predictions to wide_data
wide_data$pred_diff <- pred_diff[, "Estimate"]
MLmetrics::MAE(wide_data$pred_diff, wide_data$score_diff)

View(wide_data %>% select(pred_diff, 1:20))


# Calculate tot_tog and add it to player_bpm
player_bpm <- player_stats %>%
  mutate(ID = as.character(player_id)) %>%
  group_by(ID) %>%
  mutate(
    tot_tog = sum(as.integer(time_on_ground_percentage) / 100, na.rm = TRUE),
    ID = case_when(
      tot_tog < 5 ~ "05lt",
      tot_tog < 10 ~ "10lt",
      tot_tog < 15 ~ "15lt",
      tot_tog < 20 ~ "20lt",
      TRUE ~ ID
    )
  ) %>%
  ungroup() %>%
  group_by(ID) %>%
  summarise(
    tot_tog = sum(as.integer(time_on_ground_percentage) / 100, na.rm = TRUE),
    pos = get_mode(player_position),
    across(
      c(
        kicks:effective_disposals,
        goals:free_kicks_against,
        contested_possessions:goal_assists,
        centre_clearances:hitouts_to_advantage,
        intercept_marks:pressure_acts,
        ruck_contests:spoils
      ),
      ~ mean(.x, na.rm = TRUE)
    )
  ) %>%
  left_join(rapm_df) %>%
  # mutate(across(Kicks:Goal.Assists, scale)) %>%
  relocate(player, RAPM, gmz)


# Prepare the data for glmnet
library(glmnet)

# Assume the response variable is the point differential
X <- as.matrix(player_bpm %>% dplyr::select(-any_of(c("player", "gmz", "pos", "ID", "RAPM", "Brownlow.Votes", "tot_tog", "pred_bpm"))))
y <- player_bpm$RAPM
weights <- player_bpm$tot_tog

# Standardize the data
# X_scaled <- scale(X)

# Fit the ridge regression model (RAPM) with weights
lambda_seq <- 10^seq(5, 1, by = -0.1)
# ridge_model <- glmnet(X, y, alpha = 0, lambda = lambda_seq, weights = weights)

# Choose the best lambda using cross-validation
cv_ridge <- cv.glmnet(X, y, alpha = 0, weights = weights)
best_lambda <- cv_ridge$lambda.min
# best_lambda <- 69
best_lambda

# Refit the model with the best lambda and weights
final_model <- glmnet(X, y, alpha = 0, lambda = best_lambda, weights = weights)

# Get the coefficients (RAPM values)
bpm_values <- coef(final_model)
round(bpm_values, 3)

preds <- predict.glmnet(final_model, X, type = "response")

# Convert to a data frame for easy viewing
player_bpm$pred_bpm <- preds[, "s0"]
player_bpm <- player_bpm %>%
  relocate(pred_bpm)

View(player_bpm)

###
# season df

# Calculate tot_tog and add it to player_bpm
season_bpm <- player_stats %>%
  mutate(ID = as.character(player_id)) %>%
  group_by(
    ID,
    player = paste(player_first_name, player_last_name),
    player_team,
    season = year(match_date)
  ) %>%
  summarise(
    tot_tog = sum(as.integer(time_on_ground_percentage) / 100, na.rm = TRUE),
    # gp = n(),
    pos = get_mode(player_position),
    across(
      c(
        kicks:effective_disposals,
        goals:free_kicks_against,
        contested_possessions:goal_assists,
        centre_clearances:hitouts_to_advantage,
        intercept_marks:pressure_acts,
        ruck_contests:spoils
      ),
      ~ mean(.x, na.rm = TRUE)
    )
  ) %>%
  left_join(rapm_df) %>%
  # mutate(across(Kicks:Goal.Assists, scale)) %>%
  relocate(player, RAPM, gmz) %>%
  ungroup()

Xseason <- as.matrix(season_bpm %>%
  dplyr::select(-any_of(c("player", "player_team", "season", "gmz", "pos", "ID", "RAPM", "Brownlow.Votes", "tot_tog", "pred_bpm"))))

#
pred_season <- predict.glmnet(final_model, Xseason, type = "response")

# Convert to a data frame for easy viewing
season_bpm$pred_bpm <- pred_season[, "s0"]
season_bpm <- season_bpm %>%
  relocate(pred_bpm)

# best players in season
season_bpm %>%
  filter(season == 2024, tot_tog > 5) %>%
  View()

# best teams in season
season_bpm %>%
  filter(season == 2024) %>%
  group_by(player_team) %>%
  mutate(val = pred_bpm*tot_tog/(tot_tog + 3),
         rnk = rank(-val)) %>%
  ungroup() %>%
  filter(rnk <= 22) %>%
  group_by(player_team) %>%
  summarise(
    tot_p = sum(pred_bpm*tot_tog)/sum(tot_tog/18),
    max_val = sum(pred_bpm),
    plyrs = n()) %>%
  arrange(-max_val)


