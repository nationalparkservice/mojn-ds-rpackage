% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{ReadAndFilterData}
\alias{ReadAndFilterData}
\title{Read desert springs data from database or .csv}
\usage{
ReadAndFilterData(park, site, field.season, data.name)
}
\arguments{
\item{park}{Optional. Four-letter park code to filter on, e.g. "MOJA".}

\item{site}{Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".}

\item{field.season}{Optional. Field season name to filter on, e.g. "2019".}

\item{data.name}{The name of the analysis view or the csv file containing the data. E.g. "CalibrationDO", "DischargeVolumetric". See details for full list of data name options.}
}
\value{
A tibble of filtered data.
}
\description{
Read desert springs data from database or .csv
}
\details{
\code{data.name} options are: CalibrationDO, CalibrationpH, CalibrationSpCond, DischargeEstimated, DischargeFlowCondition, DischargeVolumetric, Disturbance, DisturbanceFlowModification, Invasives, Riparian, SensorRetrievalAttempts, SensorsCurrentlyDeployed, Site, Visit, VisitActivity, WaterQualityDO, WaterQualitypH, WaterQualitySpCond, WaterQualityTemperature, Wildlife
}
