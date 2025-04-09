# app.R

# Load necessary libraries
library(shiny)
library(DT)
library(dplyr)
library(fitzRoy)

resultz <- readRDS(url('https://github.com/peteowen1/torpdata/releases/download/predictions/predictions_2025.rds'))

resultz <- tryCatch(
  readRDS("resultz.rds"),
  error = function(e) {
    message("Error reading resultz.rds: ", e$message)
    resultz  # or some default value
  }
)

# --- Data Processing Section ---
# Fetch tips from Squiggle 2025
squiggle_25 <- fetch_squiggle_data(query = "tips", year = 2025) %>%
  mutate(
    hconfidence = as.numeric(hconfidence),
    confidence  = as.numeric(confidence),
    err         = as.numeric(err),
    bits        = as.numeric(bits),
    margin      = as.numeric(margin),
    hmargin     = as.numeric(hmargin)
  )

# Ensure that your 'resultz' data frame is available in your environment.
# You might load it from a file or have it generated elsewhere.
# For example:
# resultz <- read.csv("path_to_your_resultz_data.csv")

# Process torp_25 data (replace or adjust as necessary to match your data structure)
torp_25 <- resultz %>%
  filter(
    season.x == 2025,
    team_type == 'home'
  ) %>%
  mutate(
    source     = "Torp",
    sourceid   = 999,
    hconfidence = pred_win * 100,
    confidence  = 100 - hconfidence
  ) %>%
  select(
    ateam     = team_name.y,
    source    = source,
    sourceid  = sourceid,
    hteam     = team_name.x,
    hmargin   = pred_score_diff,
    updated   = utcStartTime.x,
    correct   = tips,
    round     = round.roundNumber.x,
    year      = season.x,
    err       = mae,
    bits      = bits,
    hconfidence,
    confidence,
    margin    = pred_score_diff,
    venue     = venue.x,
    date      = utcStartTime.x
  )

# Combine the two datasets
tot_25 <- bind_rows(squiggle_25, torp_25)

# Create a summary table for display
tot_25_summary <- tot_25 %>%
  group_by(Source = source) %>%
  summarise(
    Tips    = sum(correct, na.rm = TRUE),
    Bits    = round(sum(bits, na.rm = TRUE), 2),
    MAE     = round(mean(err, na.rm = TRUE), 2),
    Correct = round(mean(correct, na.rm = TRUE), 2)
  ) %>%
  arrange(-Bits)

# --- Shiny App Section ---
# Define UI
ui <- fluidPage(
  # Custom CSS for a clean, pastel-inspired design
  tags$head(
    tags$style(HTML("
      /* Header styling */
      table.dataTable thead {
        background-color: #A3C4BC;  /* Soft pastel teal */
        color: #FFFFFF;
        font-family: 'Helvetica', 'Arial', sans-serif;
        font-weight: 600;
        font-size: 16px;
      }

      /* Table caption styling */
      caption {
        caption-side: top;
        text-align: center;
        font-size: 26px;
        font-weight: bold;
        color: #444;
        margin-bottom: 10px;
        font-family: 'Helvetica', 'Arial', sans-serif;
      }

      /* Body row styling: alternating soft backgrounds */
      table.dataTable tbody tr:nth-child(even) {
        background-color: #F7F7F7;
      }

      table.dataTable tbody tr:nth-child(odd) {
        background-color: #FFFFFF;
      }

      /* Hover effect for rows */
      table.dataTable tbody tr:hover {
        background-color: #E1E8E2;
      }

      /* Overall table text styling */
      table.dataTable {
        font-family: 'Helvetica', 'Arial', sans-serif;
        font-size: 14px;
      }
    "))
  ),

  DTOutput("interactive_table")
)

server <- function(input, output, session) {
  output$interactive_table <- renderDT({
    datatable(
      tot_25_summary,
      caption = htmltools::tags$caption("Squiggle Models Leaderboard"),
      rownames = FALSE,
      options = list(
        lengthMenu = list(c(10, 20, 30), c("10", "20", "30")),
        pageLength = 30,
        autoWidth = TRUE,
        dom = 'ltip'  # l: length menu, t: table, i: info, p: pagination
      )
    )
  })
}

# Create and run the Shiny app
shinyApp(ui, server)

# --- Deployment Instructions ---
# To deploy this app to shinyapps.io, follow these steps:
#
# 1. Install and load the rsconnect package:
#    install.packages("rsconnect")
   # library(rsconnect)
#
# 2. Set your account information. Replace the placeholders with your details:
   # rsconnect::setAccountInfo(name = "YOUR_SHINYAPPS_ACCOUNT_NAME",
   #                           token = "YOUR_TOKEN_HERE",
   #                           secret = "YOUR_SECRET_HERE")
#
# 3. KEEP THIS COMMENTED OUT - In your R console, set your working directory to the folder containing this app.R,
#    then run:
  # rsconnect::deployApp(appDir = "C:/Users/PeterOwen/OneDrive - bzi.com.au/Documents/torp/shiny/",
  #                      appPrimaryDoc = "squiggle-comparison.R")

#
# Your app will be deployed and you’ll receive a URL that you can share online.
