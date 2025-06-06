% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_torp_data.R
\name{save_to_release}
\alias{save_to_release}
\title{Save a Data Frame to a GitHub Release via Piggyback}
\usage{
save_to_release(df, file_name, release_tag)
}
\arguments{
\item{df}{A data frame to save.}

\item{file_name}{A string for the file name (without extension).}

\item{release_tag}{The GitHub release tag to associate with the uploaded file.}
}
\value{
No return value. Used for side effects (file upload).
}
\description{
Saves a data frame as an \code{.rds} file and uploads it to a GitHub release using the \code{piggyback} package.
}
\examples{
\dontrun{
save_to_release(my_df, "latest_data", "v1.0.0")
}
}
