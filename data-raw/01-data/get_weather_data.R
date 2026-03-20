# Weather Data Backfill ----
# Fetches historical weather for AFL matches using Open-Meteo API
# Joins to fixtures via stadium lat/lon, aggregates over ~3hr match window

library(dplyr)
library(httr2)
library(arrow)
library(lubridate)
devtools::load_all()

# Load data ----

fixtures <- load_fixtures(all = TRUE) |>
  filter(!is.na(utc_start_time)) |>
  mutate(
    venue = torp_replace_venues(venue_name),
    utc_start_time = as.POSIXct(utc_start_time, format = "%Y-%m-%dT%H:%M", tz = "UTC")
  ) |>
  filter(!is.na(utc_start_time)) |>
  select(match_id, season, round_number,
         venue_name, venue, venue_timezone, utc_start_time,
         home_team_name, away_team_name)

all_grounds <- file_reader("stadium_data", "reference-data") |>
  select(venue, Latitude, Longitude) |>
  distinct(venue, .keep_all = TRUE)

fixtures_geo <- fixtures |>
  left_join(all_grounds, by = "venue")

# Check for missing coordinates
missing <- fixtures_geo |> filter(is.na(Latitude))
if (nrow(missing) > 0) {
  cat("Venues missing coordinates:\n")
  print(unique(missing$venue))
}

# Open-Meteo API helper ----

fetch_weather_batch <- function(lat, lon, start_date, end_date) {
  # Open-Meteo historical weather API (free, no key)
  # Returns hourly data for the date range at given coordinates
  resp <- request("https://archive-api.open-meteo.com/v1/archive") |>
    req_url_query(
      latitude = lat,
      longitude = lon,
      start_date = format(start_date, "%Y-%m-%d"),
      end_date = format(end_date, "%Y-%m-%d"),
      hourly = "temperature_2m,precipitation,wind_speed_10m,relative_humidity_2m",
      timezone = "UTC"
    ) |>
    req_retry(max_tries = 3, backoff = ~ 2) |>
    req_perform()

  json <- resp_body_json(resp)

  tibble(
    time = as.POSIXct(unlist(json$hourly$time), format = "%Y-%m-%dT%H:%M", tz = "UTC"),
    temperature_2m = as.numeric(unlist(json$hourly$temperature_2m)),
    precipitation = as.numeric(unlist(json$hourly$precipitation)),
    wind_speed_10m = as.numeric(unlist(json$hourly$wind_speed_10m)),
    relative_humidity_2m = as.numeric(unlist(json$hourly$relative_humidity_2m))
  )
}

# Fetch weather per venue-date batch ----
# Group matches by venue to minimize API calls (one call per venue covers all dates)

# Filter to past matches only (archive API doesn't have future data)
# and exclude venues missing coordinates
fixtures_geo <- fixtures_geo |>
  filter(!is.na(Latitude), utc_start_time < Sys.time()) |>
  mutate(match_date = as.Date(utc_start_time))

venue_groups <- fixtures_geo |>
  group_by(venue, Latitude, Longitude) |>
  summarise(
    min_date = min(match_date),
    max_date = max(match_date),
    n_matches = n(),
    .groups = "drop"
  )

cat("Fetching weather for", nrow(venue_groups), "venues...\n")

all_weather_hourly <- list()

for (i in seq_len(nrow(venue_groups))) {
  vg <- venue_groups[i, ]
  cat(sprintf("[%d/%d] %s (%d matches, %s to %s)\n",
              i, nrow(venue_groups), vg$venue, vg$n_matches,
              vg$min_date, vg$max_date))

  weather <- tryCatch(
    fetch_weather_batch(vg$Latitude, vg$Longitude, vg$min_date, vg$max_date),
    error = function(e) {
      warning(sprintf("Failed for %s: %s", vg$venue, conditionMessage(e)))
      NULL
    }
  )

  if (!is.null(weather)) {
    weather$venue <- vg$venue
    all_weather_hourly[[i]] <- weather
  }

  Sys.sleep(0.5)
}

hourly_weather <- bind_rows(all_weather_hourly)

# Aggregate to match-level ----
# For each match, average weather over a 3-hour window starting at kickoff

match_weather <- fixtures_geo |>
  rowwise() |>
  mutate(
    kickoff_utc = utc_start_time,
    end_utc = utc_start_time + hours(3)
  ) |>
  ungroup()

# Join hourly weather within the match window
match_weather_agg <- match_weather |>
  left_join(hourly_weather, by = "venue", relationship = "many-to-many") |>
  filter(time >= kickoff_utc, time < end_utc) |>
  group_by(match_id, season, round_number,
           venue_name, venue, home_team_name, away_team_name,
           Latitude, Longitude, kickoff_utc) |>
  summarise(
    temp_avg = mean(temperature_2m, na.rm = TRUE),
    precipitation_total = sum(precipitation, na.rm = TRUE),
    wind_avg = mean(wind_speed_10m, na.rm = TRUE),
    humidity_avg = mean(relative_humidity_2m, na.rm = TRUE),
    weather_hours = n(),
    .groups = "drop"
  ) |>
  mutate(
    is_rain = precipitation_total > 0.5,
    is_roof = venue == "Docklands"
  )

# Sanity checks ----

cat("\n--- Summary ---\n")
cat("Total matches with weather:", nrow(match_weather_agg), "\n")
cat("Date range:", as.character(min(match_weather_agg$kickoff_utc)),
    "to", as.character(max(match_weather_agg$kickoff_utc)), "\n")
cat("Rain matches:", sum(match_weather_agg$is_rain), "\n")
cat("Roof matches:", sum(match_weather_agg$is_roof), "\n")
cat("\nTemp range:", round(range(match_weather_agg$temp_avg), 1), "\n")
cat("Precip range:", round(range(match_weather_agg$precipitation_total), 1), "\n")
cat("Wind range:", round(range(match_weather_agg$wind_avg), 1), "\n")

# Save ----

# Upload to torpdata release (loaded via load_weather())
save_to_release(match_weather_agg, "weather_data", "weather-data")
cat("\nUploaded to torpdata weather-data release\n")
