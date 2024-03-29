% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/disturbance-qc.R
\name{HumanUseObservations}
\alias{HumanUseObservations}
\title{Table with human use observations}
\usage{
HumanUseObservations(park, site, field.season)
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
Table with human use observations
}
\examples{
\dontrun{
    HumanUseObservations()
    HumanUseObservations(site = "LAKE_P_DRI0002", field.season = "2019")
    HumanUseObservations(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
}
}
