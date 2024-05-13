library(tidyverse)
devtools::load_all()

tr <- torp_ratings(2024, get_afl_week("next"))
view(tr)

this_week <- player_game_ratings()
view(this_week)

ssn_24 <- player_season_ratings(2024)
view(ssn_24)
ssn_23 <- player_season_ratings(2023)
view(ssn_23)
ssn_22 <- player_season_ratings(2022)
view(ssn_22)
ssn_21 <- player_season_ratings(2021)
view(ssn_21)

sd(tr$torp_hitout, na.rm = T)

tr %>%
  group_by(position) %>%
  summarise(
    sd = sd(torp, na.rm = T),
    avg = mean(torp, na.rm = T),
    repl = quantile(torp, 0.2, na.rm = T),
    n = n()
  )

tr %>%
  filter(!is.na(torp)) %>%
  group_by(team) %>%
  summarise(
    val = sum(pmax(torp, 0), na.rm = T),
    ply = n(),
    age = sum(age * torp) / sum(torp)
  ) %>%
  arrange(-val)
