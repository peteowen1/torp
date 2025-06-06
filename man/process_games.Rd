% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim-helpers.R
\name{process_games}
\alias{process_games}
\title{Process games for a single round}
\usage{
process_games(sim_teams, sim_games, round_num)
}
\arguments{
\item{sim_teams}{A data frame containing team ratings.}

\item{sim_games}{A data frame containing fixture data.}

\item{round_num}{The round number to process.}
}
\value{
A list containing updated sim_teams and sim_games data frames.
}
\description{
This function processes games for a single round of the season.
}
