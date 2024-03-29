% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/general-qc.R
\name{qcVisitTypeCheck}
\alias{qcVisitTypeCheck}
\title{Return list of sites with any other combination of visit types other than one primary visit for each field season}
\usage{
qcVisitTypeCheck(park, site, field.season)
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
Return list of sites with any other combination of visit types other than one primary visit for each field season
}
\examples{
\dontrun{
    qcVisitTypeCheck()
    qcVisitTypeCheck(park = "DEVA", field.season = c("2018", "2020", "2021"))
}
}
