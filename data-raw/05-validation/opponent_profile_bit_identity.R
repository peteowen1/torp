# Is an opponent-profile change output-neutral, bit for bit?
# ==========================================================
#
# `.compute_rolling_stat_profiles()`, `.compute_team_defensive_profiles()` and
# `.compute_rolling_epv_profiles()` all feed PUBLISHED ratings, so a refactor of
# them has to prove itself rather than be argued. This script runs all three on
# one fixture and saves the result; run it against the old sources and the new
# ones and compare with `identical()`.
#
#   Rscript data-raw/05-validation/opponent_profile_bit_identity.R new.rds
#   git checkout <base> -- R/opponent_adjustment.R R/epv_opponent_adjustment.R
#   Rscript data-raw/05-validation/opponent_profile_bit_identity.R old.rds
#   git checkout HEAD -- R/opponent_adjustment.R R/epv_opponent_adjustment.R
#   # then: identical(readRDS("old.rds"), readRDS("new.rds"))
#
# Used 2026-08-11 to establish that extracting `.decay_weight()` and
# `.shrink_to_league()` changed nothing: all three returned identical() on this
# fixture.
#
# Compare with identical(), NOT all.equal(). all.equal() has a default
# tolerance of ~1.5e-8 and would report "TRUE" for a change that moved every
# published rating in its last several digits, which is exactly the claim being
# tested.

suppressMessages(devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE))
args <- commandArgs(trailingOnly = TRUE)
out <- args[[1]]

set.seed(99)
teams <- c("A", "B", "C", "D", "E", "F")
n_matches <- 40
d0 <- as.Date("2025-03-01")

rows <- do.call(rbind, lapply(seq_len(n_matches), function(i) {
  tt <- sample(teams, 2)
  dt <- d0 + cumsum(sample(3:11, 1))[1] * i          # irregular gaps
  data.frame(
    match_id = paste0("M", i),
    team = tt, opponent = rev(tt),
    match_date_rating = dt,
    disposals = round(stats::rgamma(2, shape = 9, scale = 3.7), 4),
    marks = round(stats::rgamma(2, shape = 4, scale = 2.3), 4),
    stringsAsFactors = FALSE
  )
}))
dt <- data.table::as.data.table(rows)

rate_sources <- c(disposals = "disposals", marks = "marks")
cap <- c(0.7, 1.4)

rolling <- .compute_rolling_stat_profiles(
  data.table::copy(dt), lambda_decay = 0.003,
  rate_sources = rate_sources, cap = cap, prior_games = 5)

as_of <- .compute_team_defensive_profiles(
  data.table::copy(dt)[match_date_rating < max(dt$match_date_rating)],
  ref_date = max(dt$match_date_rating), lambda_decay = 0.003,
  rate_sources = rate_sources, cap = cap)

# EPV path: same shape of input, different column names.
pgd <- data.table::data.table(
  match_id = dt$match_id, team = dt$team, opponent = dt$opponent,
  utc_start_time = as.POSIXct(dt$match_date_rating),
  epv_adj = round(stats::rnorm(nrow(dt), 0, 3), 5),
  time_on_ground_percentage = round(stats::runif(nrow(dt), 40, 100), 3)
)
epv <- .compute_rolling_epv_profiles(pgd, lambda_decay = 0.003, prior_games = 5)

saveRDS(list(rolling = rolling, as_of = as_of, epv = epv), out)
cat("saved", out, "-- rolling", nrow(rolling), "rows, as_of", nrow(as_of),
    "rows, epv", nrow(epv), "rows\n")
