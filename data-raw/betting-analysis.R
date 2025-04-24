library(tidyverse)

team_home_df <-
  team_mdl_df %>%
  filter(team_type == 'home',
         !is.na(result.x)) %>%
  mutate(
    line_diff = (pred_score_diff-score_diff)
  )

hist(round(team_home_df$pred_score_diff), breaks = 50)
hist(round(team_home_df$score_diff), breaks = 50)
hist(round(team_home_df$line_diff), breaks = 50)

summary(as.factor(round(team_home_df$line_diff)))/nrow(team_home_df)

n <- 3
odds <- 1.93
odds+(n*.0175)*odds
