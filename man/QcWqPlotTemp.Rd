% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/waterquality.R
\name{QcWqPlotTemp}
\alias{QcWqPlotTemp}
\title{Generate box plots for water temperature for each park and year. Includes annual and 3Yr springs only.}
\usage{
QcWqPlotTemp(
  conn,
  path.to.data,
  park,
  site,
  field.season,
  data.source = "database",
  include.title = TRUE
)
}
\arguments{
\item{conn}{Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.}

\item{path.to.data}{The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.}

\item{park}{Optional. Four-letter park code to filter on, e.g. "MOJA".}

\item{site}{Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".}

\item{field.season}{Optional. Field season name to filter on, e.g. "2019".}

\item{data.source}{Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.}

\item{include.title}{Include plot title? Defaults to TRUE}
}
\value{
Box plots of water temperature data for each park and field season.
}
\description{
Generate box plots for water temperature for each park and year. Includes annual and 3Yr springs only.
}
