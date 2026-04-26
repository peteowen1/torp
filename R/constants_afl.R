# AFL League / Team / Field / API Constants
# =========================================
# League-wide constants used across the package: physical field properties,
# game timing, team identity (names/aliases/colours), API endpoints, and
# regular-season structure.

# AFL Field and Game Constants
# ----------------------------

#' Earliest season with data available in torpdata
#' @keywords internal
AFL_MIN_SEASON <- 2021L

#' Nominal playing time per AFL quarter in seconds (excludes stoppages)
#' @keywords internal
AFL_PLAY_QUARTER_SECONDS <- 1200L

#' Nominal playing time per AFL match in seconds (excludes stoppages)
#' @keywords internal
AFL_PLAY_GAME_SECONDS <- 4800L

#' Events that stop the play clock (chain `description` values)
#' @keywords internal
CLOCK_STOPPAGE_TRIGGERS <- c("Goal", "Behind", "Rushed",
                              "Out of Bounds", "Out On Full After Kick",
                              "OOB Throw In")

#' Events that restart the play clock (chain `description` values)
#' @keywords internal
CLOCK_RESTART_EVENTS <- c("Centre Bounce", "Ball Up Call",
                          "Throw In", "Kick In", "Free Advantage",
                          "Free For", "Free For: Before the Bounce",
                          "Free For: In Possession", "Free For: Off The Ball",
                          "Mark", "Contested Mark", "Uncontested Mark",
                          "Mark On Lead")

#' Maximum reasonable seconds between consecutive in-play chain events
#' Used to cap clock deltas when reconstructing play time.
#' @keywords internal
CLOCK_DELTA_CAP <- 30L

#' Width of an AFL goal in metres (between the goal posts)
#' @keywords internal
AFL_GOAL_WIDTH <- 6.4

#' Default duration of one AFL quarter in seconds (including stoppages)
#' Used as fallback when match-specific quarter durations are unavailable.
#' @keywords internal
AFL_QUARTER_DURATION <- 2000

#' Default total game seconds (4 quarters * 2000s)
#' @keywords internal
AFL_TOTAL_GAME_SECONDS <- 8000

#' Number of regulation quarters in an AFL match
#' @keywords internal
AFL_MAX_PERIODS <- 4

#' Maximum value for time-decay scaler in EPR weighting
#' @keywords internal
AFL_TIME_SCALER_MAX <- 4


# AFL Regular Season Structure
# ----------------------------

#' Regular season rounds by year (excludes finals)
#' 2022 and earlier: 23 rounds; 2023 onwards: 24 rounds
#' @keywords internal
AFL_REGULAR_SEASON_ROUNDS <- c(
  "2021" = 23L,
  "2022" = 23L,
  "2023" = 24L,
  "2024" = 24L,
  "2025" = 24L,
  "2026" = 24L
)


# AFL Structural Constants
# ------------------------

#' Number of on-field players per team in a standard AFL match
#' @keywords internal
AFL_TEAM_SIZE <- 18L

#' Default days rest when rest data is unavailable
#' @keywords internal
MATCH_DEFAULT_REST_DAYS <- 21

#' Default TOG fraction for unknown field positions
#' @keywords internal
POSITION_AVG_TOG_DEFAULT <- 0.75

#' Venues with a closed roof (weather features set to neutral)
#' @keywords internal
AFL_ROOF_VENUES <- c("Docklands")

#' Default timezone for AFL matches
#' @keywords internal
AFL_DEFAULT_TIMEZONE <- "Australia/Melbourne"

#' Maximum round number fallback when AFL_REGULAR_SEASON_ROUNDS lookup fails
#' @keywords internal
AFL_MAX_REGULAR_ROUNDS <- 24L


# API & External Service Constants
# --------------------------------

#' AFL public API base URL (v2)
#' @keywords internal
AFL_API_BASE_URL <- "https://aflapi.afl.com.au/afl/v2/"

#' AFL CFS API base URL (fixtures, results, players)
#' @keywords internal
AFL_CFS_API_BASE_URL <- "https://api.afl.com.au/cfs/afl/"

#' AFL chain/play data API base URL
#' @keywords internal
AFL_SAPI_BASE_URL <- "https://sapi.afl.com.au/afl/"

#' Rate limit delay in seconds for Open-Meteo API calls
#' @keywords internal
OPEN_METEO_RATE_LIMIT_SECONDS <- 0.3

#' Default disk cache max age in days
#' @keywords internal
DISK_CACHE_DEFAULT_AGE_DAYS <- 7


# Team Name Constants
# -------------------

#' Canonical AFL team lookup table
#'
#' Data frame with 18 rows mapping canonical short name, full name, and
#' AFL API abbreviation for each current AFL team.
#' @export
AFL_TEAMS <- data.frame(
  name = c(
    "Adelaide Crows", "Brisbane Lions", "Carlton Blues", "Collingwood Magpies",
    "Essendon Bombers", "Fremantle Dockers", "Geelong Cats", "Gold Coast Suns",
    "GWS Giants", "Hawthorn Hawks", "Melbourne Demons", "North Melbourne Kangaroos",
    "Port Adelaide Power", "Richmond Tigers", "St Kilda Saints", "Sydney Swans",
    "West Coast Eagles", "Western Bulldogs"
  ),
  full = c(
    "Adelaide Crows", "Brisbane Lions", "Carlton Blues", "Collingwood Magpies",
    "Essendon Bombers", "Fremantle Dockers", "Geelong Cats", "Gold Coast Suns",
    "GWS Giants", "Hawthorn Hawks", "Melbourne Demons", "North Melbourne Kangaroos",
    "Port Adelaide Power", "Richmond Tigers", "St Kilda Saints", "Sydney Swans",
    "West Coast Eagles", "Western Bulldogs"
  ),
  abbr = c(
    "ADEL", "BL", "CARL", "COLL", "ESS",
    "FRE", "GEEL", "GCFC", "GWS", "HAW",
    "MELB", "NMFC", "PA", "RICH",
    "STK", "SYD", "WCE", "WB"
  ),
  stringsAsFactors = FALSE
)

#' Named vector mapping team name variants to canonical names
#'
#' Maps abbreviations, full names, nicknames, Indigenous round names,
#' and other variants to the canonical short name used in `AFL_TEAMS$name`.
#' @export
AFL_TEAM_ALIASES <- c(

  # --- Abbreviations (AFL API + common) ---
  "ADEL"  = "Adelaide Crows",
  "BL"    = "Brisbane Lions",
  "CARL"  = "Carlton Blues",
  "COLL"  = "Collingwood Magpies",
  "ESS"   = "Essendon Bombers",
  "FRE"   = "Fremantle Dockers",
  "GEEL"  = "Geelong Cats",
  "GCFC"  = "Gold Coast Suns",
  "GWS"   = "GWS Giants",
  "HAW"   = "Hawthorn Hawks",
  "MELB"  = "Melbourne Demons",
  "NMFC"  = "North Melbourne Kangaroos",
  "NM"    = "North Melbourne Kangaroos",
  "PA"    = "Port Adelaide Power",
  "PORT"  = "Port Adelaide Power",
  "RICH"  = "Richmond Tigers",
  "STK"   = "St Kilda Saints",
  "SYD"   = "Sydney Swans",
  "WCE"   = "West Coast Eagles",
  "WB"    = "Western Bulldogs",

  # --- Full names (identity mappings) ---
  "Adelaide Crows"             = "Adelaide Crows",
  "Brisbane Lions"             = "Brisbane Lions",
  "Brisbane Bears"             = "Brisbane Lions",
  "Carlton Blues"              = "Carlton Blues",
  "Collingwood Magpies"        = "Collingwood Magpies",
  "Essendon Bombers"           = "Essendon Bombers",
  "Fremantle Dockers"          = "Fremantle Dockers",
  "Geelong Cats"               = "Geelong Cats",
  "Gold Coast Suns"            = "Gold Coast Suns",
  "GWS Giants"                 = "GWS Giants",
  "Greater Western Sydney"     = "GWS Giants",
  "Greater Western Sydney Giants" = "GWS Giants",
  "GW Sydney Giants"           = "GWS Giants",
  "Hawthorn Hawks"             = "Hawthorn Hawks",
  "Melbourne Demons"           = "Melbourne Demons",
  "North Melbourne Kangaroos"  = "North Melbourne Kangaroos",
  "Port Adelaide Power"        = "Port Adelaide Power",
  "Richmond Tigers"            = "Richmond Tigers",

  "St Kilda Saints"            = "St Kilda Saints",
  "Sydney Swans"               = "Sydney Swans",
  "South Melbourne"            = "Sydney Swans",
  "South Melbourne Swans"      = "Sydney Swans",
  "West Coast Eagles"          = "West Coast Eagles",
  "Western Bulldogs"           = "Western Bulldogs",
  "Footscray"                  = "Western Bulldogs",
  "Footscray Bulldogs"         = "Western Bulldogs",

  # --- Old short names (legacy TORP model output) ---
  "Adelaide"       = "Adelaide Crows",
  "Carlton"        = "Carlton Blues",
  "Collingwood"    = "Collingwood Magpies",
  "Essendon"       = "Essendon Bombers",
  "Fremantle"      = "Fremantle Dockers",
  "Geelong"        = "Geelong Cats",
  "Gold Coast"     = "Gold Coast Suns",
  "Hawthorn"       = "Hawthorn Hawks",
  "Melbourne"      = "Melbourne Demons",
  "North Melbourne" = "North Melbourne Kangaroos",
  "Port Adelaide"  = "Port Adelaide Power",
  "Richmond"       = "Richmond Tigers",
  "St Kilda"       = "St Kilda Saints",
  "Sydney"         = "Sydney Swans",
  "West Coast"     = "West Coast Eagles",

  # --- Nicknames ---
  "Crows"     = "Adelaide Crows",
  "Lions"     = "Brisbane Lions",
  "Bears"     = "Brisbane Lions",

  "Blues"     = "Carlton Blues",
  "Magpies"   = "Collingwood Magpies",
  "Pies"      = "Collingwood Magpies",
  "Bombers"   = "Essendon Bombers",
  "Dockers"   = "Fremantle Dockers",
  "Cats"      = "Geelong Cats",
  "Suns"      = "Gold Coast Suns",
  "SUNS"      = "Gold Coast Suns",
  "Giants"    = "GWS Giants",
  "GIANTS"    = "GWS Giants",
  "Hawks"     = "Hawthorn Hawks",
  "Demons"    = "Melbourne Demons",
  "Kangaroos" = "North Melbourne Kangaroos",
  "Roos"      = "North Melbourne Kangaroos",
  "Power"     = "Port Adelaide Power",
  "Tigers"    = "Richmond Tigers",
  "Saints"    = "St Kilda Saints",
  "Swans"     = "Sydney Swans",
  "Eagles"    = "West Coast Eagles",
  "Bulldogs"  = "Western Bulldogs",

  # --- Indigenous round names ---
  "Kuwarna"          = "Adelaide Crows",
  "Narrm"            = "Melbourne Demons",
  "Walyalup"         = "Fremantle Dockers",
  "Yartapuulti"      = "Port Adelaide Power",
  "Euro-Yroke"       = "St Kilda Saints",
  "Waalitj Marawar"  = "West Coast Eagles",
  "Wallitj Marawar"  = "West Coast Eagles",

  # --- ALL CAPS mascot variants (AFL API team names) ---
  "Adelaide CROWS"             = "Adelaide Crows",
  "Brisbane LIONS"             = "Brisbane Lions",
  "Carlton BLUES"              = "Carlton Blues",
  "Collingwood MAGPIES"        = "Collingwood Magpies",
  "Essendon BOMBERS"           = "Essendon Bombers",
  "Fremantle DOCKERS"          = "Fremantle Dockers",
  "Geelong CATS"               = "Geelong Cats",
  "Gold Coast SUNS"            = "Gold Coast Suns",
  "GWS GIANTS"                 = "GWS Giants",
  "Hawthorn HAWKS"             = "Hawthorn Hawks",
  "Melbourne DEMONS"           = "Melbourne Demons",
  "North Melbourne KANGAROOS"  = "North Melbourne Kangaroos",
  "Port Adelaide POWER"        = "Port Adelaide Power",
  "Richmond TIGERS"            = "Richmond Tigers",
  "St Kilda SAINTS"            = "St Kilda Saints",
  "Sydney SWANS"               = "Sydney Swans",
  "West Coast EAGLES"          = "West Coast Eagles",
  "Western BULLDOGS"           = "Western Bulldogs"
)


# Team Colours
# ------------

#' Primary team colours (hex) for AFL teams
#'
#' Named character vector mapping canonical team names to primary brand colour.
#' @export
AFL_TEAM_COLORS <- c(
  "Adelaide Crows"            = "#002B5C",
  "Brisbane Lions"            = "#A30046",
  "Carlton Blues"              = "#002B5C",

  "Collingwood Magpies"       = "#000000",
  "Essendon Bombers"          = "#CC2031",

  "Fremantle Dockers"         = "#2A0D45",
  "Geelong Cats"              = "#001F3D",
  "Gold Coast Suns"           = "#D4A843",
  "GWS Giants"                = "#F15A22",
  "Hawthorn Hawks"            = "#4D2004",
  "Melbourne Demons"          = "#CC2031",
  "North Melbourne Kangaroos" = "#003D8E",
  "Port Adelaide Power"       = "#008AAB",
  "Richmond Tigers"           = "#FED102",
  "St Kilda Saints"           = "#ED1C24",
  "Sydney Swans"              = "#ED171F",

  "West Coast Eagles"         = "#002B5C",
  "Western Bulldogs"          = "#014896"
)

#' Secondary team colours (hex) for AFL teams
#'
#' Named character vector mapping canonical team names to secondary brand colour.
#' Useful for contrast in two-tone plots (e.g., home vs away fills).
#' @export
AFL_TEAM_COLORS2 <- c(
  "Adelaide Crows"            = "#E21937",
  "Brisbane Lions"            = "#FFB81C",
  "Carlton Blues"              = "#FFFFFF",
  "Collingwood Magpies"       = "#FFFFFF",
  "Essendon Bombers"          = "#000000",
  "Fremantle Dockers"         = "#FFFFFF",
  "Geelong Cats"              = "#FFFFFF",
  "Gold Coast Suns"           = "#E21937",
  "GWS Giants"                = "#363636",
  "Hawthorn Hawks"            = "#FBBF15",
  "Melbourne Demons"          = "#002B5C",
  "North Melbourne Kangaroos" = "#FFFFFF",
  "Port Adelaide Power"       = "#000000",
  "Richmond Tigers"           = "#000000",
  "St Kilda Saints"           = "#000000",
  "Sydney Swans"              = "#FFFFFF",
  "West Coast Eagles"         = "#F2A900",
  "Western Bulldogs"          = "#E21937"
)
