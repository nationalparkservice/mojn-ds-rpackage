% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wildlife-qc.R
\name{qcWildlifeObservedNoTypes}
\alias{qcWildlifeObservedNoTypes}
\title{Wildlife observed, no wildlife type specified}
\usage{
qcWildlifeObservedNoTypes(park, site, field.season)
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
Wildlife observed, no wildlife type specified
}
\examples{
\dontrun{
    qcWildlifeObservedNoTypes()
    qcWildlifeObservedNoTypes(site = "LAKE_P_GET0066", field.season = "2019")
    qcWildlifeObservedNoTypes(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
}
}
