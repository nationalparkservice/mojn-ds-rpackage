% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/disturbance-qc.R
\name{LivestockPlot}
\alias{LivestockPlot}
\title{Bar plot with percent of springs with livestock disturbance}
\usage{
LivestockPlot(park, site, field.season)
}
\arguments{
\item{park}{Optional. Four-letter park code to filter on, e.g. "MOJA".}

\item{site}{Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".}

\item{field.season}{Optional. Field season name to filter on, e.g. "2019".}
}
\value{
ggplot bar plot
}
\description{
Bar plot with percent of springs with livestock disturbance
}
\examples{
\dontrun{
    LivestockPlot()
    LivestockPlot(park = c("DEVA", "JOTR"))
}
}
