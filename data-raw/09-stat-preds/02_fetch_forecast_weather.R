# 02_fetch_forecast_weather.R
# ===========================
# T2 of the next-game stat model (torpverse docs/plans/STAT-PREDS-PLAN.md).
# Weather as it was FORECAST before each match, not as it happened (Pete, 27 Nov
# 2026): torp's weather-data release holds observed weather only, which is not
# known at decision time T (charter section 1).
#
# Source: Open-Meteo's previous-runs API, which keeps, for every hour, the value
# forecast 1 to 7 days earlier (`<var>_previous_day<N>`). Rain, wind and humidity
# at those leads exist from about 1 Feb 2024 (checked 27 Nov 2026: present for
# 2024-02-01, absent for 2023-12-01), so only matches from 2024 on are fetched;
# earlier seasons stay blank in the feature table.
#
# One request per venue per season (whole season's hours), saved raw and gzipped
# before anything is parsed (CLAUDE.md: save every scraped response raw). A
# re-run re-reads saved responses and fetches only what is missing.
# Per match and lead N: the 3 hours from kickoff, as production's
# get_weather_data.R does (mean temperature, wind, humidity; total rain).
#
# Usage (torp package root, PowerShell):
#   Rscript data-raw/09-stat-preds/02_fetch_forecast_weather.R
# Output: data-raw/cache-stat-preds/forecast_weather.parquet (one row per match
# and lead 1-7). Nothing is uploaded.

suppressMessages({
  library(data.table)
  devtools::load_all(".", quiet = TRUE)
})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")
out_dir <- file.path("data-raw", "cache-stat-preds")
raw_dir <- file.path(out_dir, "forecast-raw")
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

FIRST_DATE <- as.Date("2024-02-01")
VARS <- c("temperature_2m", "precipitation", "wind_speed_10m", "relative_humidity_2m")
LEADS <- 1:7

wx <- as.data.table(load_weather())
wx[, kickoff := as.POSIXct(kickoff_utc, tz = "UTC")]
stopifnot(!anyNA(wx$kickoff), !anyDuplicated(wx$match_id))
m <- wx[as.Date(kickoff) >= FIRST_DATE, .(match_id, season, lat = Latitude, lon = Longitude, kickoff)]
say("matches from ", format(FIRST_DATE), ": ", nrow(m), " | seasons ", paste(sort(unique(m$season)), collapse = ", "))

req <- m[, .(start = as.Date(min(kickoff)), end = as.Date(max(kickoff)) + 1L), by = .(season, lat, lon)]
req[, file := file.path(raw_dir, sprintf("s%d_%.4f_%.4f.json.gz", season, lat, lon))]
say("requests (venue x season): ", nrow(req), " | already saved: ", sum(file.exists(req$file)))

hourly_vars <- paste(as.vector(outer(VARS, LEADS, function(v, n) paste0(v, "_previous_day", n))), collapse = ",")
for (i in which(!file.exists(req$file))) {
  url <- sprintf(paste0("https://previous-runs-api.open-meteo.com/v1/forecast?latitude=%.4f&longitude=%.4f",
                        "&hourly=%s&start_date=%s&end_date=%s&timezone=UTC"),
                 req$lat[i], req$lon[i], hourly_vars, req$start[i], req$end[i])
  resp <- httr::GET(url, httr::timeout(120))
  if (httr::status_code(resp) != 200) stop("HTTP ", httr::status_code(resp), " for ", basename(req$file[i]))
  con <- gzfile(req$file[i], "w"); writeLines(httr::content(resp, "text", encoding = "UTF-8"), con); close(con)
  Sys.sleep(1)
}
stopifnot(all(file.exists(req$file)))

# Parse ----
hourly <- withCallingHandlers(rbindlist(lapply(seq_len(nrow(req)), function(i) {
  con <- gzfile(req$file[i]); on.exit(close(con))
  j <- jsonlite::fromJSON(readLines(con, warn = FALSE))
  h <- as.data.table(lapply(j$hourly[setdiff(names(j$hourly), "time")], function(x) as.numeric(unlist(lapply(x, function(v) if (is.null(v)) NA else v)))))
  h[, time := as.POSIXct(j$hourly$time, format = "%Y-%m-%dT%H:%M", tz = "UTC")]
  h[, `:=`(season = req$season[i], lat = req$lat[i], lon = req$lon[i])]
  h
}), fill = TRUE), warning = function(w) stop("parse warning: ", conditionMessage(w)))
say("hourly rows: ", nrow(hourly))

win <- m[, .(match_id, season, lat, lon, kickoff, end = kickoff + 3 * 3600)]
j <- hourly[win, on = .(season, lat, lon, time >= kickoff, time < end), nomatch = NULL,
            c(list(match_id = i.match_id), mget(grep("_previous_day", names(hourly), value = TRUE)))]
fc <- rbindlist(lapply(LEADS, function(n) {
  col <- function(v) paste0(v, "_previous_day", n)
  j[, .(lead_days = n,
        fc_temp = mean(get(col("temperature_2m")), na.rm = TRUE),
        fc_precip = sum(get(col("precipitation"))),
        fc_wind = mean(get(col("wind_speed_10m")), na.rm = TRUE),
        fc_humidity = mean(get(col("relative_humidity_2m")), na.rm = TRUE),
        fc_hours = .N), by = match_id]
}))
for (v in c("fc_temp", "fc_wind", "fc_humidity")) fc[is.nan(get(v)), (v) := NA_real_]

# Coverage: every match, every lead, a full 3-hour window with values.
cov <- fc[, .(matches = uniqueN(match_id), full_window = mean(fc_hours == 3),
              temp = mean(!is.na(fc_temp)), precip = mean(!is.na(fc_precip)),
              wind = mean(!is.na(fc_wind)), humidity = mean(!is.na(fc_humidity))), by = lead_days]
print(cov)
stopifnot(all(cov$matches == nrow(m)), all(cov$full_window >= 0.99),
          all(as.matrix(cov[, .(temp, precip, wind, humidity)]) >= 0.98))

arrow::write_parquet(fc, file.path(out_dir, "forecast_weather.parquet"))
say("wrote ", file.path(out_dir, "forecast_weather.parquet"), " (", nrow(fc), " rows)")
