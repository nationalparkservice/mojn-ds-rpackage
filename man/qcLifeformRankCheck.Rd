% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vegetation-qc.R
\name{qcLifeformRankCheck}
\alias{qcLifeformRankCheck}
\title{Return list of visits where multiple lifeforms have the same rank, and rank gaps have not been properly entered}
\usage{
qcLifeformRankCheck(park, site, field.season)
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
Return list of visits where multiple lifeforms have the same rank, and rank gaps have not been properly entered
}
\examples{
\dontrun{
    qcLifeformRankCheck()
    qcLifeformRankCheck(site = "JOTR_P_NOR0083", field.season = "2021")
    qcLifeformRankCheck(park = c("JOTR", "MOJA"), field.season = c("2016", "2018", "2021"))
}
}
