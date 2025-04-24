########
library(monashtipr)

# Store password and username
# If wanted - store user/password in Renviron file
# e.g. you can run `usethis::edit_r_environ()` and edit them
# MONASH_USER = xxx
# MONASH_PASS = xxx

user <- Sys.getenv("MONASH_USER")
pass <- Sys.getenv("MONASH_PASS")

### mapping
mapping <- dplyr::bind_cols(
  monash = c(
    "Adelaide",
    "Brisbane",
    "Carlton",
    "Collingwood",
    "Essendon",
    "W_Bulldogs",
    "Fremantle",
    "Geelong",
    "Gold_Coast",
    "G_W_Sydney",
    "Hawthorn",
    "Melbourne",
    "Kangaroos",
    "P_Adelaide",
    "Richmond",
    "St_Kilda",
    "Sydney",
    "W_Coast"
  ),
  afl = c(
    "Adelaide",
    "Brisbane Lions",
    "Carlton",
    "Collingwood",
    "Essendon",
    "Footscray",
    "Fremantle",
    "Geelong",
    "Gold Coast",
    "GWS",
    "Hawthorn",
    "Melbourne",
    "North Melbourne",
    "Port Adelaide",
    "Richmond",
    "St Kilda",
    "Sydney",
    "West Coast"
  )
)
### normal
comp <- "normal"
games <- monashtipr::get_games(user, pass, comp = comp)

games <-
  games %>%
  left_join(
    week_gms %>%
      left_join(mapping, by = c("home_team" = "afl")),
    by = c("Home" = "monash")
  ) %>%
  select(Game, Ground, Home, Away, Margin = pred_margin)

games

monashtipr::submit_tips(games, user, pass, comp = comp)


### probabilistic
comp <- "info"
games <- monashtipr::get_games(user, pass, comp = comp)

games <-
  games %>%
  left_join(
    week_gms %>%
      left_join(mapping, by = c("home_team" = "afl")),
    by = c("Home" = "monash")
  ) %>%
  select(Game, Ground, Home, Away, Probability = pred_win)

games

monashtipr::submit_tips(games, user, pass, comp = comp)

week_gms
