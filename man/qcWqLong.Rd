% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/waterquality.R
\name{qcWqLong}
\alias{qcWqLong}
\title{Intermediate step used to clean water quality data for stats and plotting functions. Limit data to primary visits of annual and 3Yr springs, and exclude data with "W" and "C" flags.}
\usage{
qcWqLong(park, site, field.season)
}
\arguments{
\item{park}{Optional. Four-letter park code to filter on, e.g. "MOJA".}

\item{site}{Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".}

\item{field.season}{Optional. Field season name to filter on, e.g. "2019".}
}
\value{
Tibble with columns for Park, FieldSeason, SiteCode, VisitDate, Parameter, Units, and Median.
}
\description{
Intermediate step used to clean water quality data for stats and plotting functions. Limit data to primary visits of annual and 3Yr springs, and exclude data with "W" and "C" flags.
}
\examples{
\dontrun{
    
    qcWqLong()
    qcWqLong(site = "LAKE_P_GET0066", field.season = "2019")
    qcWqLong(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
}
}
