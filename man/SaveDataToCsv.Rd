% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{SaveDataToCsv}
\alias{SaveDataToCsv}
\title{Save desert springs analysis views as a set of .csv files}
\usage{
SaveDataToCsv(dest.folder, create.folders = FALSE, overwrite = FALSE)
}
\arguments{
\item{dest.folder}{The folder in which to save the .csv files.}

\item{create.folders}{Should \code{dest.folder} be created automatically if it doesn't exist? Defaults to \code{FALSE}.}

\item{overwrite}{Should existing data be automatically overwritten? Defaults to \code{FALSE}.}
}
\value{
None.
}
\description{
Save desert springs analysis views as a set of .csv files
}
\examples{
\dontrun{
LoadDesertSprings()
SaveDataToCsv("C:/Users/myusername/Documents/R/desert-springs-data", TRUE, TRUE)
}
}
