% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{FormatPlot}
\alias{FormatPlot}
\title{Apply some standard formatting to a ggplot object.}
\usage{
FormatPlot(
  data,
  x.col,
  y.col,
  facet.col,
  n.col.facet = 2,
  sample.size.col,
  sample.size.loc,
  plot.title = "",
  sub.title = "",
  facet.as.subtitle = TRUE,
  x.lab = "",
  y.lab = "",
  rotate.x.labs = FALSE,
  ymax,
  ymin,
  xmax,
  xmin,
  transform.x,
  transform.y
)
}
\arguments{
\item{data}{Data frame containing the data to be plotted.}

\item{x.col}{Column name of independent variable. If plot type only requires one variable (e.g. histogram), use only one of x.col or y.col.}

\item{y.col}{Column name of dependent variable. If plot type only requires one variable (e.g. histogram), use only one of x.col or y.col.}

\item{facet.col}{Column to facet on. If this results in only one facet, it will be used as a subtitle instead.}

\item{n.col.facet}{Number of columns of facet grid.}

\item{sample.size.col}{Column containing sample size labels.}

\item{sample.size.loc}{Either 'xaxis' or 'plot'. 'xaxis' will add sample size to each x axis label. 'plot' will add sample size to the facet label (or subtitle, if only one facet).}

\item{plot.title}{The title of the plot.}

\item{sub.title}{Optional custom plot subtitle.}

\item{facet.as.subtitle}{If only one facet, use facet name as subtitle? Defaults to TRUE.}

\item{x.lab}{X axis label.}

\item{y.lab}{Y axis label.}

\item{rotate.x.labs}{Boolean indicating whether to rotate x axis labels 90 degrees.}

\item{ymax}{Optional maximum y limit.}

\item{ymin}{Optional minimum y limit.}

\item{xmax}{Optional maximum x limit.}

\item{xmin}{Optional minimum x limit.}

\item{transform.x}{Optional x axis transformation. One of 'log10', 'sqrt', or 'reverse'.}

\item{transform.y}{Optional y axis transformation. One of 'log10', 'sqrt', or 'reverse'.}
}
\value{
A ggplot object.
}
\description{
Apply some standard formatting to a ggplot object.
}
