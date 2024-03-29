% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vegetation-qc.R
\name{qcLifeformPresentNoRank}
\alias{qcLifeformPresentNoRank}
\title{Return list of visits with lifeform present, no rank}
\usage{
qcLifeformPresentNoRank(park, site, field.season)
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
Return list of visits with lifeform present, no rank
}
\examples{
\dontrun{
    qcLifeformPresentNoRank()
    qcLifeformPresentNoRank(site = "LAKE_P_GET0066", field.season = "2019")
    qcLifeformPresentNoRank(park = c("MOJA", "PARA"), field.season = c("2017", "2019", "2020"))
}
}
