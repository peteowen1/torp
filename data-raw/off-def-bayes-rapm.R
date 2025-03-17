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
player_stats <- fitzRoy::fetch_player_stats_fryzigg(2003:2024) %>% # 2012 for advanced stats, 2003 for basics
  mutate(
    season = as.factor(year(match_date)),
    tog_adj =
      ifelse(season == 2020,
        (as.integer(time_on_ground_percentage) * 0.8) / 100,
        as.integer(time_on_ground_percentage) / 100
      )
  )
# maybe split 2012:2017 and 2018:2024 and do two regressions

# Prepare the data for modeling
player_game_stats <- player_stats %>%
  group_by(player_id) %>%
  mutate(
    home_away = ifelse(match_home_team == player_team, "home", "away"),
    tot_tog = sum(tog_adj, na.rm = TRUE),
    ID2 = case_when(
      tot_tog < 5 ~ "05lt",
      tot_tog < 10 ~ "10lt",
      tot_tog < 15 ~ "15lt",
      tot_tog < 20 ~ "20lt",
      TRUE ~ as.character(player_id)
    )
  ) %>%
  ungroup() %>%
  group_by(season) %>%
  mutate(
    mu_home = mean(match_home_team_score),
    mu_away = mean(match_away_team_score)
  ) %>%
  ungroup() %>%
  mutate(
    adj_home_score = match_home_team_score - mu_home,
    adj_away_score = match_away_team_score - mu_away
  ) %>%
  select(match_id, season, match_home_team, match_away_team,
    ID = ID2, player_first_name, player_last_name,
    tog_adj, home_away, home = adj_home_score, away = adj_away_score
  )


# Create two rows for each player: one for score_for and one for score_against
player_game_stats_long <- player_game_stats %>%
  pivot_longer(cols = c(home, away), names_to = "score_type", values_to = "score")

# Add suffix to IDs
player_game_stats_long <- player_game_stats_long %>%
  mutate(
    ID = paste0(ID, ifelse(home_away == score_type, "_for", "_against"))
  )

make_wide_data <- function(df, seasons) {
# Convert the data to a wide format where each row represents a game, and each column represents a player's performance
wide_data <- df %>%
  filter(season %in% seasons) %>%
  select(match_id, season, match_home_team, match_away_team, score_type, ID, tog_adj, score) %>%
  pivot_wider(names_from = ID, values_from = tog_adj, values_fn = sum) %>%
  janitor::clean_names()

# Convert season column to multiple columns
wide_data <- wide_data %>%
  mutate(season = as.character(season)) %>% # Ensure season is character
  pivot_wider(names_from = season, values_from = season, values_fill = list(season = 0), names_prefix = "season_", values_fn = list(season = ~1))

# Handle missing values
wide_data[is.na(wide_data)] <- 0

return(wide_data)
}

wd_12_19 <- make_wide_data(player_game_stats_long, 2003:2024)
# wd_16_24 <- make_wide_data(player_game_stats_long, 2012:2024)

# # Prepare the design matrix and response variable
# X <- as.matrix(wide_data %>% select(-any_of(c(
#   "season", "round", "home_team", "away_team", "score_diff", "score", "score_type",
#   "match_id", "match_home_team", "match_away_team"
# ))))
# y <- wide_data$score

# Prepare the data frame for brms
df_brms_12_19 <- wd_12_19 %>%
  select(-any_of(c(
    "season", "round", "home_team", "away_team", "score_diff", "score_type",
    "match_id", "match_home_team", "match_away_team"
  )))

# # Prepare the data frame for brms
# df_brms_16_24 <- wd_16_24 %>%
#   select(-any_of(c(
#     "season", "round", "home_team", "away_team", "score_diff", "score_type",
#     "match_id", "match_home_team", "match_away_team"
#   )))

# Define the model formula
formula <- bf(score ~ 0 + ., center = FALSE)

# Get the prior structure for the model
# prior_structure <- get_prior(formula, data = df_brms, family = gaussian())
# Extract the coefficient names
# coef_names <- prior_structure$coef

# Create prior specifications based on the coefficient names
priors <- c(
  prior(normal(0, 4), class = "b"),
  prior(cauchy(0, 2), class = "sigma")
)

# # Loop through coefficient names to add appropriate priors [takes too long to run model :( ]
# for (coef_name in coef_names) {
#   if (startsWith(coef_name, "x")) {
#     priors <- c(priors, set_prior("normal(0, 4)", class = "b", coef = coef_name))
#   } else if (startsWith(coef_name, "season")) {
#     priors <- c(priors, set_prior("normal(0, 100)", class = "b", coef = coef_name))
#   }
# }

### 2012 to 2017 ----
# Fit the Bayesian ridge regression model with the specified priors
fit_od_12_19 <- brm(formula, data = df_brms_12_19, prior = priors, family = gaussian(), chains = 2, iter = 1000, cores = available_cores, verbose = TRUE)
# Save model
saveRDS(fit_od_12_19, "./data-raw/bayes_od_rapm_12_19.rds")
# Load model
fit_od_12_19 <- readRDS("./data-raw/bayes_od_rapm_12_19.rds")

### 2018 to 2024 ----
# # Fit the Bayesian ridge regression model with the specified priors
# fit_od_16_24 <- brm(formula, data = df_brms_16_24, prior = priors, family = gaussian(), chains = 2, iter = 1000, cores = available_cores, verbose = TRUE)
# # Save model
# saveRDS(fit_od_16_24, "./data-raw/bayes_od_rapm_16_24.rds")
# # Load model
# fit_od_16_24 <- readRDS("./data-raw/bayes_od_rapm_16_24.rds")

get_rapm_df <- function(mdl) {
# Extract posterior samples using as_draws
posterior_samples <- as_draws_df(mdl)

# Get the means of the coefficients and retain their names
coef_means <- posterior_samples %>%
  select(starts_with("b_")) %>%
  summarise(across(everything(), mean))

# Convert to a named numeric vector
coef_means_vector <- as.numeric(coef_means)
names(coef_means_vector) <- colnames(coef_means)

# Get the name of the model object
mdl_name <- deparse(substitute(mdl))

# Create a data frame of coefficients
rapm_df <- data.frame(
  ID = names(coef_means_vector),
  RAPM = coef_means_vector,
  mdl = mdl_name
)

return(rapm_df)
}

rapm_df <- bind_rows(get_rapm_df(fit_od_12_19),
                     # get_rapm_df(fit_od_16_24)
                     )

View(rapm_df)

# Clean up the IDs and create a new column for type
rapm_df <- rapm_df %>%
  mutate(
    ID = gsub("b_x", "", ID),
    Type = ifelse(grepl("for", ID), "orapm", "drapm"),
    ID = gsub("_for|_against", "", ID)
  )

# Pivot to wider format
rapm_wide <- rapm_df %>%
  pivot_wider(names_from = Type, values_from = RAPM, values_fn = mean)

# Merge with player information
player_info <- player_game_stats %>%
  group_by(ID) %>%
  summarize(player = max(paste(player_first_name, player_last_name)),
            gmz = sum(abs(tog_adj), na.rm = TRUE))

rapm_df <- left_join(rapm_wide, player_info, by = "ID") %>%
  mutate(tot_rapm = orapm - drapm)

# View the RAPM values
View(rapm_df)

# Predict the point differentials
pred_diff_12_19 <- predict(fit_od_12_19, newdata = df_brms_12_19, type = "response")
# pred_diff_16_24 <- predict(fit_od_16_24, newdata = df_brms_16_24, type = "response")

# Add predictions to wide_data
wd_12_19$pred_diff <- pred_diff_12_19[, "Estimate"]
MLmetrics::MAE(wd_12_19$pred_diff, wd_12_19$score)

# Add predictions to wide_data
# wd_16_24$pred_diff <- pred_diff_16_24[, "Estimate"]
# MLmetrics::MAE(wd_16_24$pred_diff, wd_16_24$score)


# player_bpm_12_19 ----
player_bpm_12_19 <- player_stats %>%
  filter(season %in% 2003:2024) %>%
  mutate(ID = as.character(player_id)) %>%
  group_by(ID) %>%
  mutate(
    tot_tog = sum(tog_adj, na.rm = TRUE),
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
    tot_tog = sum(tog_adj, na.rm = TRUE),
    avg_tog = mean(tog_adj, na.rm = TRUE),
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
  left_join(rapm_df %>% filter(mdl == 'fit_od_12_19')) %>%
  # mutate(across(Kicks:Goal.Assists, scale)) %>%
  relocate(colnames(rapm_df))

# # player_bpm_16_24
# player_bpm_16_24 <- player_stats %>%
#   filter(season %in% 2016:2024) %>%
#   mutate(ID = as.character(player_id)) %>%
#   group_by(ID) %>%
#   mutate(
#     tot_tog = sum(tog_adj, na.rm = TRUE),
#     ID = case_when(
#       tot_tog < 5 ~ "05lt",
#       tot_tog < 10 ~ "10lt",
#       tot_tog < 15 ~ "15lt",
#       tot_tog < 20 ~ "20lt",
#       TRUE ~ ID
#     )
#   ) %>%
#   ungroup() %>%
#   group_by(ID) %>%
#   summarise(
#     tot_tog = sum(tog_adj, na.rm = TRUE),
#     avg_tog = mean(tog_adj, na.rm = TRUE),
#     pos = get_mode(player_position),
#     across(
#       c(
#         kicks:effective_disposals,
#         goals:free_kicks_against,
#         contested_possessions:goal_assists,
#         centre_clearances:hitouts_to_advantage,
#         intercept_marks:pressure_acts,
#         ruck_contests:spoils
#       ),
#       ~ mean(.x, na.rm = TRUE)
#     )
#   ) %>%
#   left_join(rapm_df %>% filter(mdl == 'fit_od_16_24')) %>%
#   # mutate(across(Kicks:Goal.Assists, scale)) %>%
#   relocate(colnames(rapm_df))

# total df
player_bpm <- bind_rows(player_bpm_12_19,
                        # player_bpm_16_24
                        ) %>%
  filter(!is.na(mdl))

# Prepare the data for glmnet ----
library(glmnet)

model_df <- player_bpm %>% # %>% filter(!str_detect(ID,'lt'))
  select(where(~ all(complete.cases(.))))

# Assume the response variable is the point differential
X <- as.matrix(model_df %>%
                 mutate(across(kicks:goal_assists, ~ . / avg_tog)) %>% ### either spoils or goal_assists
  dplyr::select(-any_of(
    c(
      "player", "gmz", "pos", "ID", "RAPM", "Brownlow.Votes", "tot_tog","avg_tog", "pred_bpm",
      "orapm", "drapm", "tot_rapm", "mdl"
    )
  )))
y1 <- model_df$orapm
y2 <- model_df$drapm
weights <- sqrt(model_df$tot_tog)
weights <- weights / mean(weights)

# Standardize the data
# X_scaled <- scale(X)

# Fit the ridge regression model (RAPM) with weights
# lambda_seq <- 10^seq(5, 1, by = -0.1)
# ridge_model <- glmnet(X, y, alpha = 0, lambda = lambda_seq, weights = weights)

# Choose the best lambda using cross-validation
cv_orapm <- cv.glmnet(X, y1, alpha = 0, weights = weights)
best_lambda_o <- cv_orapm$lambda.min
best_lambda_o

# Refit the model with the best lambda and weights
orapm_model <- glmnet(X, y1, alpha = 0, lambda = best_lambda_o, weights = weights)

# Choose the best lambda using cross-validation
cv_drapm <- cv.glmnet(X, y2, alpha = 0, weights = weights)
best_lambda_d <- cv_drapm$lambda.min
best_lambda_d

# Refit the model with the best lambda and weights
drapm_model <- glmnet(X, y2, alpha = 0, lambda = best_lambda_d, weights = weights)

# Get the coefficients (RAPM values)
obpm_values <- coef(orapm_model)
round(obpm_values, 4)

dbpm_values <- coef(drapm_model)
round(dbpm_values, 4)

#
preds_o <- predict.glmnet(orapm_model, X, type = "response")
preds_d <- predict.glmnet(drapm_model, X, type = "response")

# Convert to a data frame for easy viewing
model_df$pred_obpm <- preds_o[, "s0"]
model_df$pred_dbpm <- preds_d[, "s0"]
model_df <- model_df %>%
  mutate(
    tot_bpm = pred_obpm - pred_dbpm,
    tot_val_g = tot_bpm * avg_tog,
    tot_val = tot_bpm * tot_tog,
    tot_val_repl = (tot_bpm+2) * tot_tog,
  ) %>%
  relocate(tot_bpm, pred_obpm, pred_dbpm,tot_val_g,tot_val,tot_val_repl) %>%
  filter(!is.na(tot_bpm))

View(model_df)

###
# season df

# Calculate tot_tog and add it to model_df
season_bpm <- player_stats %>%
  mutate(ID = as.character(player_id),
         player_position = replace_na(player_position,'missing')) %>%
  group_by(
    ID,
    player = paste(player_first_name, player_last_name),
    player_team,
    season = year(match_date)
  ) %>%
  summarise(
    tot_tog = sum(tog_adj, na.rm = TRUE),
    avg_tog = mean(tog_adj, na.rm = TRUE),
    # gp = n(),
    pos = get_mode(player_position),
    across(
      c(
        kicks:effective_disposals,
        goals:free_kicks_against,
        contested_possessions:goal_assists,
        # centre_clearances:hitouts_to_advantage,
        # intercept_marks:pressure_acts,
        # ruck_contests:spoils
      ),
      ~ mean(.x, na.rm = TRUE)
    )
  ) %>%
  # left_join(rapm_df) %>%
  # mutate(across(Kicks:Goal.Assists, scale)) %>%
  # relocate(colnames(rapm_df)) %>%
  ungroup()

Xseason <- as.matrix(season_bpm %>%
                       select(where(~ all(complete.cases(.)))) %>%
  mutate(across(kicks:goal_assists, ~ . / avg_tog)) %>% ### either spoils or goal_assists
  dplyr::select(-any_of(c(
    "player", "player_team", "season", "gmz", "pos", "ID", "RAPM",
    "Brownlow.Votes", "tot_tog", "avg_tog" ,"pred_bpm", "orapm", "drapm", "tot_rapm","mdl"
  ))))

#
pred_o_season <- predict.glmnet(orapm_model, Xseason, type = "response")
pred_d_season <- predict.glmnet(drapm_model, Xseason, type = "response")

# setdiff(rownames(coef(orapm_model)),colnames(Xseason))
# setdiff(colnames(Xseason),rownames(coef(orapm_model)))

# Convert to a data frame for easy viewing
season_bpm$pred_obpm <- pred_o_season[, "s0"]
season_bpm$pred_dbpm <- pred_d_season[, "s0"]
season_bpm <- season_bpm %>%
  mutate(
    tot_bpm = pred_obpm - pred_dbpm,
    tot_val_g = tot_bpm * avg_tog,
    tot_val = tot_bpm * tot_tog,
    tot_val_repl = (tot_bpm+2) * tot_tog,
    ) %>%
  relocate(tot_bpm, pred_obpm, pred_dbpm,tot_val_g,tot_val,tot_val_repl) %>%
  filter(!is.na(tot_bpm))

View(season_bpm %>% filter(tot_tog > 5))

# best players in season
season_bpm %>%
  group_by(season) %>%
  summarise(
    val = sum(tot_bpm * tot_tog),
    sum(is.na(tot_bpm))
  ) %>% print(n=25)

# best players in season
season_bpm %>%
  filter(season == 2024, tot_tog > 5) %>%
  View()

# best teams in season
season_bpm %>%
  filter(season == 2024) %>%
  group_by(player_team) %>%
  mutate(
    val = tot_bpm * tot_tog / (tot_tog + 3),
    rnk = rank(-val)
  ) %>%
  ungroup() %>%
  filter(rnk <= 22) %>%
  group_by(player_team) %>%
  summarise(
    # tot_p = sum(tot_bpm*tot_tog)/sum(tot_tog/18),
    max_val = sum(val),
    plyrs = n()
  ) %>%
  arrange(-max_val)

# best teams in season
season_bpm %>%
  filter(season == 2024) %>%
  # group_by(player_team) %>%
  mutate(val = tot_bpm * tot_tog / (tot_tog + 3)) %>%
  #        rnk = rank(-val)) %>%
  # ungroup() %>%
  # filter(rnk <= 22) %>%
  group_by(player_team) %>%
  summarise(
    tot_p = sum(tot_bpm * tot_tog) / sum(tot_tog / 18),
    # max_val = sum(ifelse(val>-1,val,0)),
    plyrs = n()
  ) %>%
  arrange(-tot_p)
