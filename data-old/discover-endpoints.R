library(httr)
library(jsonlite)

# Function to make authenticated GET requests
make_get_request <- function(url, token) {
  response <- GET(
    url = url,
    add_headers("x-media-mis-token" = token)
  )

  status <- status_code(response)

  if (status == 200) {
    cat("✅ Endpoint found:", url, "\n")
    # Optionally, parse and inspect the data
    # data <- fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)
    # print(head(data))
  } else {
    cat("❌ Endpoint not found or inaccessible:", url, "Status:", status, "\n")
  }
}

# Your token
token <- "581391d506571dfd74cb944010c42749"  # Replace with your actual token

# Base URL
base_url <- "https://sapi.afl.com.au/afl/"

# List of potential endpoint patterns
endpoints <- c(
  "matchStats/CD_M20240142308",
  "matchDetails/CD_M20240142308",
  "matchPlays/CD_M20240142308",
  "matchEvents/CD_M20240142308",
  "matchSummary/CD_M20240142308",
  "matchTimeline/CD_M20240142308",
  "matchPlayers/CD_M20240142308",
  "matchAnalytics/CD_M20240142308",
  "matchHighlights/CD_M20240142308",
  "matchPhotos/CD_M20240142308",
  "matchVideos/CD_M20240142308",
  "teamStats/CD_T150",
  "teamDetails/CD_T150",
  "teamPlayers/CD_T150",
  "teamMatches/CD_T150",
  "teamHistory/CD_T150",
  "playerStats/CD_I298290",
  "playerDetails/CD_I298290",
  "playerHistory/CD_I298290",
  "playerMatches/CD_I298290",
  "seasonStats/2024",
  "seasonDetails/2024",
  "seasonMatches/2024",
  "seasonTeams/2024",
  "venueStats/venueId123",        # Replace 'venueId123' with actual venue ID
  "venueDetails/venueId123",      # Replace 'venueId123' with actual venue ID
  "venueMatches/venueId123",      # Replace 'venueId123' with actual venue ID
  "venueTeams/venueId123",        # Replace 'venueId123' with actual venue ID
  "playerAwards/CD_I298290",
  "teamAwards/CD_T150",
  "matchAwards/CD_M20240142308",
  "matchInjuries/CD_M20240142308",
  "playerInjuries/CD_I298290"
)

# Iterate through each endpoint and make GET requests
for (endpoint in endpoints) {
  full_url <- paste0(base_url, endpoint)
  make_get_request(full_url, token)
}

# Optional: Nested Endpoints (Replace placeholders with actual IDs)
nested_endpoints <- c(
  "matches/CD_M20240142308/events",
  "matches/CD_M20240142308/players",
  "teams/CD_T150/players",
  "teams/CD_T150/stats",
  "players/CD_I298290/stats",
  "players/CD_I298290/history",
  "seasons/2024/matches",
  "seasons/2024/teams",
  "venues/venueId123/matches",      # Replace 'venueId123' with actual venue ID
  "venues/venueId123/teams"         # Replace 'venueId123' with actual venue ID
)

# Iterate through nested endpoints
for (endpoint in nested_endpoints) {
  full_url <- paste0(base_url, endpoint)
  make_get_request(full_url, token)
}

# Additional Endpoint Patterns
additional_endpoints <- c(
  "matchPlays/CD_M20240142308/stats",
  "matchPlays/CD_M20240142308/players",
  "matchPlays/CD_M20240142308/analytics",
  "matchPlays/CD_M20240142308/summary",
  "matchPlays/CD_M20240142308/timeline",
  "matchPlays/CD_M20240142308/photos",
  "matchPlays/CD_M20240142308/videos",
  "teams/CD_T150/matchPlays",
  "players/CD_I298290/matchPlays",
  "seasons/2024/matchPlays",
  "venues/venueId123/matchPlays",    # Replace 'venueId123' with actual venue ID
  "players/CD_I990704",
  "players/CD_I990704/stats",
  "players/CD_I990704/matchPlays",
  "teams/CD_T30",
  "teams/CD_T30/stats",
  "teams/CD_T30/matchPlays",
  "seasons/2023",                     # Previous season
  "seasons/2025",                     # Next season
  "matchPlays/CD_M20240142308/insights",
  "matchPlays/CD_M20240142308/replays",
  "matchPlays/CD_M20240142308/comments",
  "matchPlays/CD_M20240142308/live",
  "matchPlays/CD_M20240142308/notifications",
  "matchPlays/CD_M20240142308/feed",
  "matchPlays/CD_M20240142308/history",
  "matchPlays/CD_M20240142308/details",
  "matchPlays/CD_M20240142308/participants",
  "matchPlays/CD_M20240142308/updates",
  "matchPlays/CD_M20240142308/config",
  "matchPlays/CD_M20240142308/preferences"
)

# Iterate through additional endpoints
for (endpoint in additional_endpoints) {
  full_url <- paste0(base_url, endpoint)
  make_get_request(full_url, token)
}
