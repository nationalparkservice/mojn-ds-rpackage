% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sensors.R
\name{SensorQcSummary}
\alias{SensorQcSummary}
\title{Summarize sensor retrieval and download attempts by park and season}
\usage{
SensorQcSummary(
  conn,
  path.to.data,
  park,
  deployment.field.season,
  data.source = "database"
)
}
\arguments{
\item{conn}{Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.}

\item{path.to.data}{The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.}

\item{park}{Optional. Four-letter park code to filter on, e.g. "MOJA".}

\item{deployment.field.season}{Optional. Field season name to filter on, e.g. "2019".}

\item{data.source}{Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.}
}
\value{
A tibble with columns for park, deployment field season, number of sensors deployed, number of sensors for which retrieval was not attempted, number of sensors for which retrieval was attempted, number of sensors actually retrieved, number of sensors actually downloaded
}
\description{
Summarize sensor retrieval and download attempts by park and season
}
