library(tidyverse)

match_id <- 'CD_M20250140504'
url <- glue::glue("https://api.afl.com.au/cfs/afl/matchItem/{match_id}")

url <- glue::glue("https://api.afl.com.au/cfs/afl/teamStats/match/{match_id}")

path <- '/cfs/afl/statsCentre/teams?competitionId=CD_S2025014&teamIds=CD_T100%2CCD_T1000'
path <- '/cfs/afl/coach/match/CD_M20250140504/teamStats'
path <- '/cfs/afl/matchItem/CD_M20250140504'
path <- '/cfs/afl/matchInterchange/CD_M20250140504'
path <- '/cfs/afl/statsCentre/players?competitionId=CD_S2025014&teamIds=CD_T100%2CCD_T1000'

url <- glue::glue("https://api.afl.com.au{path}")

headers <- c("x-media-mis-token" = get_afl_cookie())

res <- httr::GET(url = url, httr::add_headers(headers))

match_info <- jsonlite::parse_json(httr::content(res, "text", encoding = "UTF-8"))

match_info

# match_info$score$scoreWorm$scoringEvents
data <- match_info$score$scoreWorm$scoringEvents

# Extract periodSeconds and periodNumber
# period_info <- purrr::map_dfr(match_info$score$matchClock$periods, ~ {
#   tibble::tibble(
#     periodNumber = .x$periodNumber,
#     TotalPeriodSeconds = .x$periodSeconds
#   )
