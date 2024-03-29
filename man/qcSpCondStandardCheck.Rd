% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/waterquality.R
\name{qcSpCondStandardCheck}
\alias{qcSpCondStandardCheck}
\title{Check specific conductance calibrations for the use of low conductivity standards at high conductivity springs.}
\usage{
qcSpCondStandardCheck(park, site, field.season)
}
\arguments{
\item{park}{Optional. Four-letter park code to filter on, e.g. "MOJA".}

\item{site}{Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".}

\item{field.season}{Optional. Field season name to filter on, e.g. "2019".}
}
\value{
Tibble
}
\description{
Check specific conductance calibrations for the use of low conductivity standards at high conductivity springs.
}
\examples{
\dontrun{
    qcSpCondStandardCheck()
    qcSpCondStandardCheck(site = "LAKE_P_GET0066")
    qcSpCondStandardCheck(park = c("LAKE", "PARA"), field.season = c("2016", "2017", "2020"))
}
}
