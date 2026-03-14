# Setup ----

library(rvest)
library(dplyr)
library(tidyr)
library(osmdata)
library(stringr)
# library(fitzRoy)  # no longer needed

# Scrape Wikipedia ----

url <- "https://en.wikipedia.org/wiki/List_of_Australian_Football_League_grounds"

page <- read_html(url)

tables <- page %>% html_nodes("table.wikitable")

# Process Tables ----

process_current_table <- function(table) {
  df <- table %>%
    html_table(fill = TRUE) %>%
    .[[1]]
  df %>%
    select(Ground, City, `State/territory`, Capacity, `First used`, Games, `Current tenant(s)`) %>%
    rename(
      State_Territory = `State/territory`,
      First_Used = `First used`,
      Current_Tenants = `Current tenant(s)`
    ) %>%
    mutate(across(everything(), ~ str_replace_all(., "\\[.*?\\]", ""))) %>%
    mutate(
      Capacity = as.integer(gsub("[ ,]", "", Capacity)),
      Table = "Current"
    )
}

process_former_table <- function(table) {
  df <- table %>%
    html_table(fill = TRUE) %>%
    .[[1]]
  df %>%
    select(Ground, City, State, Capacity, `First used`, Games, `Tenant(s)`) %>%
    rename(
      State_Territory = State,
      First_Used = `First used`,
      Current_Tenants = `Tenant(s)`
    ) %>%
    mutate(across(everything(), ~ str_replace_all(., "\\[.*?\\]", ""))) %>%
    mutate(
      Capacity = as.integer(gsub("[ ,]", "", Capacity)),
      Table = "Former"
    )
}

process_other_table <- function(table) {
  df <- table %>%
    html_table(fill = TRUE) %>%
    .[[1]]
  df %>%
    select(Ground, City, `State/Country`, Capacity, Games, `Last used`, Uses) %>%
    rename(
      State_Territory = `State/Country`,
      First_Used = `Last used`,
      Current_Tenants = Uses
    ) %>%
    mutate(across(everything(), ~ str_replace_all(., "\\[.*?\\]", ""))) %>%
    mutate(
      Capacity = as.integer(gsub("[ ,]", "", Capacity)),
      Table = "Other"
    )
}

# Process each table
current_grounds <- process_current_table(tables[1])
former_major_grounds <- process_former_table(tables[2])
other_minor_grounds <- process_other_table(tables[3])

all_grounds <- bind_rows(current_grounds, former_major_grounds, other_minor_grounds)

# Geocode Grounds ----

get_lat_lon <- function(place) {
  res <- tryCatch(
    {
      opq(place) %>%
        add_osm_feature(key = "place", value = "square") %>%
        osmdata_sf()
    },
    error = function(e) {
      return(NULL)
    }
  )

  if (!is.null(res) && !is.null(res$bb)) {
    bb <- strsplit(res$bb, ",")[[1]]
    lat <- (as.numeric(bb[1]) + as.numeric(bb[3])) / 2
    lon <- (as.numeric(bb[2]) + as.numeric(bb[4])) / 2
    return(c(lat, lon))
  } else {
    return(c(NA, NA))
  }
}

for (i in 1:nrow(all_grounds)) {
  place <- paste(all_grounds$Ground[i], all_grounds$City[i], all_grounds$State_Territory[i], "Australia", sep = ", ")
  coords <- get_lat_lon(place)
  all_grounds$Latitude[i] <- coords[1]
  all_grounds$Longitude[i] <- coords[2]

  # Print progress
  print(paste("Processed:", all_grounds$Ground[i], "- Row", i, "of", nrow(all_grounds)))
}

all_grounds <-
  all_grounds %>%
  mutate(venue = replace_venues(Ground))

# Manual Coordinate Fixes ----

all_grounds <-
  all_grounds %>%
  mutate(
    Latitude = case_when(
      Ground == "Cazalys Stadium" ~ -16.93557,
      Ground == "Jiangwan Stadium" ~ 31.3076026,
      Ground == "Marrara Oval" ~ -12.4010941,
      Ground == "Riverway Stadium" ~ -19.317617,
      Ground == "Summit Sport and Recreation Park" ~ -35.0783354,
      Ground == "Barossa Park" ~ -34.5991418,
      TRUE ~ Latitude
    ),
    Longitude = case_when(
      Ground == "Cazalys Stadium" ~ 145.74899,
      Ground == "Jiangwan Stadium" ~ 121.5160351,
      Ground == "Marrara Oval" ~ 130.8811236,
      Ground == "Riverway Stadium" ~ 146.7293007,
      Ground == "Summit Sport and Recreation Park" ~ 138.891488,
      Ground == "Barossa Park" ~ 138.8862646,
      TRUE ~ Longitude
    ),
  )

# Save Data ----

bellerive_row <- all_grounds[all_grounds$Ground == "Bellerive Oval", ]
bellerive_row$Ground <- "Ninja Stadium"
bellerive_row$venue <- "Ninja Stadium"
all_grounds <- dplyr::bind_rows(all_grounds, bellerive_row) %>% distinct()

arrow::write_parquet(all_grounds, "./data-raw/outputs/stadium_data.parquet")

print(all_grounds)
