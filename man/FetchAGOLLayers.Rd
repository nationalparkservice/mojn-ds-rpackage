% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agol-utils.R
\name{FetchAGOLLayers}
\alias{FetchAGOLLayers}
\title{Read data from the Desert Springs AGOL feature layers. Returns the raw data in its current format on AGOL. Mostly used for data management purposes.}
\usage{
FetchAGOLLayers(
  data_path =
    "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_DS_SpringVisit/FeatureServer",
  lookup_path =
    "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_Lookup_Database/FeatureServer",
  sites_path =
    "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_DS_GRTSDraw/FeatureServer",
  calibration_path =
    "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_Calibration_Database/FeatureServer",
  agol_username = "mojn_data",
  agol_password = keyring::key_get(service = "AGOL", username = "mojn_data")
)
}
\arguments{
\item{data_path}{URL to Desert Springs feature service on AGOL.}

\item{lookup_path}{URL to feature service on AGOL containing Desert Springs lookup tables.}

\item{sites_path}{URL to feature service on AGOL containing sites table}

\item{calibration_path}{URL to feature service on AGOL containing calibration tables.}

\item{agol_username}{Username of headless AGOL account with permissions to view the feature service.}

\item{agol_password}{Password for headless AGOL account.}
}
\value{
A list of tibbles
}
\description{
Read data from the Desert Springs AGOL feature layers. Returns the raw data in its current format on AGOL. Mostly used for data management purposes.
}
