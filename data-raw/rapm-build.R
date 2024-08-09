# Load necessary libraries
library(fitzRoy)
library(dplyr)
library(tidyr)
library(glmnet)

# Fetch player statistics
# player_stats <- fitzRoy::fetch_player_stats_afltables(2018:2024)
player_stats <- fitzRoy::fetch_player_stats_fryzigg(2018:2024)

# Prepare the data for modeling
# Summarize the statistics at player-game level
player_game_stats <- player_stats %>%
  group_by(ID) %>%
  mutate(tot_tog = sum(as.integer(Time.on.Ground)/100, na.rm = T),
         ID2 = case_when(
           tot_tog < 5 ~ 'a) < 5',
           tot_tog < 10 ~ 'b) < 10',
           tot_tog < 15 ~ 'c) < 15',
           tot_tog < 20 ~ 'd) < 20',
           TRUE ~ ID
           )
         ) %>%
  ungroup() %>%
  dplyr::mutate(
    score_diff = Home.score-Away.score,
    Time.on.Ground = ifelse(Home.Away=='Home',as.integer(Time.on.Ground)/100,-as.integer(Time.on.Ground)/100)
  ) %>%
  dplyr::select(Season, Round, Home.team, Away.team, ID = ID2, Player, Time.on.Ground, score_diff)

# Convert the data to a wide format where each row represents a game, and each column represents a player's performance
wide_data <- player_game_stats %>% select(-Player) %>%
  tidyr::pivot_wider(names_from = ID, values_from = Time.on.Ground, values_fn = sum)

# Handle missing values (e.g., replace NAs with 0)
wide_data[is.na(wide_data)] <- 0

# Prepare the design matrix and response variable
# Assume the response variable is the point differential
X <- as.matrix(wide_data %>% dplyr::select(-any_of(c('Player','Season','Round','Home.team','Away.team','score_diff','pred_diff'))))
y <- wide_data$score_diff

dim(X)

# Standardize the data
# X_scaled <- scale(X)

# Fit the ridge regression model (RAPM)
lambda_seq <- 10^seq(5, 1, by = -0.1)
# ridge_model <- glmnet(X, y, alpha = 0, lambda = lambda_seq)

# Choose the best lambda using cross-validation
cv_ridge <- cv.glmnet(X, y, alpha = 0)
best_lambda <- cv_ridge$lambda.min
# best_lambda <- 69
best_lambda

# Refit the model with the best lambda
final_model <- glmnet(X, y, alpha = 0, lambda = best_lambda)

# Get the coefficients (RAPM values)
rapm_values <- coef(final_model)

# Convert to a data frame for easy viewing
rapm_df <- data.frame(
  ID = rownames(rapm_values),
  RAPM = as.vector(rapm_values)
)

# Merge with player information
player_info <- player_game_stats %>% dplyr::group_by(ID) %>% dplyr::summarise(player = max(Player), gmz = sum(abs(Time.on.Ground), na.rm = T))
rapm_df <- dplyr::left_join(rapm_df, player_info, by = "ID")

View(rapm_df)

wide_data$pred_diff <- predict.glmnet(final_model,X,type='response')

wide_data <- wide_data %>% select(pred_diff, everything())

View(wide_data %>% select(1:30))

