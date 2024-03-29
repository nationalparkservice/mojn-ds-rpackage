% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discharge-qc.R
\name{FlowCategoriesAnnualPlot}
\alias{FlowCategoriesAnnualPlot}
\title{Summary bar plot of flow categories for annual springs}
\usage{
FlowCategoriesAnnualPlot(park, site, field.season)
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
Summary bar plot of flow categories for annual springs
}
\examples{
\dontrun{
    FlowCategoriesAnnualPlot()
    FlowCategoriesAnnualPlot(park = c("DEVA", "MOJA"), field.season = c("2016", "2018", "2021"))
}
}
