library(tidyverse)
library(fitzRoy)

testdf <- fitzRoy::fetch_player_stats_afl(2023)

testdf$player_name <- as.factor(paste(testdf$player.givenName, testdf$player.surname))
testdf$player_id <- as.factor(paste(testdf$player.playerId))
testdf$how <- testdf$extendedStats.hitoutWinPercentage / 100
testdf$match_id <- as.factor(testdf$providerId)
testdf$team <- as.factor(ifelse(testdf$teamStatus == "home", testdf$home.team.name, testdf$away.team.name))
testdf$opposition <- as.factor(ifelse(testdf$teamStatus == "home", testdf$away.team.name, testdf$home.team.name))

### model
ho_bam <- mgcv::bam(
  how ~
    s(player_id, bs = "re")
    + s(match_id, bs = "re")
    + s(team, bs = "re")
    + s(opposition, bs = "re"),
  data = testdf %>% filter(!is.na(how)), family = binomial(), weights = extendedStats.ruckContests,
  select = TRUE, discrete = TRUE, nthreads = 4
)

mixedup::extract_ranef(ho_bam) %>%
  dplyr::left_join(
    testdf %>%
      group_by(player_id, player_name) %>%
      summarise(
        conts = sum(extendedStats.ruckContests, na.rm = T),
        hos = sum(hitouts, na.rm = T)
      ),
    by = c("group" = "player_id")
  ) %>%
  tibble::view()

# summary(ho_bam)

###########
library(fitzRoy)

id <- "CD_M20230140601"

cookie <- fitzRoy:::get_afl_cookie()

api <- httr::modify_url(
  url = "https://api.afl.com.au",
  path = paste0("/cfs/afl/playerStats/match/", id)
)

resp <- httr::GET(
  url = api,
  httr::add_headers(
    "x-media-mis-token" = cookie
  )
)

cont <- fitzRoy:::parse_resp_afl(resp)
cont
