% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/player_ratings.R
\name{torp_ratings}
\alias{torp_ratings}
\title{Get TORP ratings}
\usage{
torp_ratings(
  season_val = get_afl_season(type = "next"),
  round_val = get_afl_week(type = "next"),
  decay = 365,
  loading = 1.5,
  prior_games_recv = 4,
  prior_games_disp = 6
)
}
\arguments{
\item{season_val}{The season to calculate ratings for. Default is the next season.}

\item{round_val}{The round to calculate ratings for. Default is the next round.}

\item{decay}{The decay factor for weighting games. Default is 365.}

\item{loading}{The loading factor for TORP calculations. Default is 1.5.}

\item{prior_games_recv}{The number of prior games to consider for receiving. Default is 4.}

\item{prior_games_disp}{The number of prior games to consider for disposal. Default is 6.}
}
\value{
A data frame containing player TORP ratings.
}
\description{
This function calculates TORP (Total Overall Rating Points) for players based on their performance.
}
