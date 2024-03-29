% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidbit.R
\name{ProcessTidbiT}
\alias{ProcessTidbiT}
\title{Convert raw TidbiT data into format importable into Aquarius database.}
\usage{
ProcessTidbiT(wy.folder)
}
\arguments{
\item{wy.folder}{Mandatory. WY followed by 4-digit water year, e.g. "WY2022".}
}
\value{
A tibble with columns for date-time, water temperature, water detect, and water binary.
}
\description{
Convert raw TidbiT data into format importable into Aquarius database.
}
\examples{
ProcessTidbiT()
}
