#' Open a Connection to the Desert Springs Database
#'
#' @param use.mojn.default Connect to the live MOJN Desert Springs database? MOJN staff should use this option. Defaults to \code{TRUE}.
#' @param drv DBI driver to use. Defaults to \code{odbc::odbc()}.
#' @param ... Additional arguments to \code{\link[pool]{dbPool}}. Ignored if \code{use.mojn.default} is \code{TRUE}.
#'
#' @return A database connection pool object
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' \dontrun{
#' conn <- OpenDatabaseConnection()
#' }
OpenDatabaseConnection <- function(use.mojn.default = TRUE, drv = odbc::odbc(), ...) {
  if (use.mojn.default) {
    params <- readr::read_csv("M:/MONITORING/DS_Water/Data/Database/ConnectFromR/ds-database-conn.csv") %>%
      as.list()
    params$drv <- drv
    my.pool <- do.call(pool::dbPool, params)
  } else {
    my.pool <- pool::dbPool(drv = drv, ...)
  }

  return(my.pool)
}

#' Close a connection to the Desert Springs Database
#'
#' @param conn A database connection pool object generated from a call to \code{OpenDatabaseConnection()}
#'
#' @return None.
#' @export
#'
#' @examples
#' conn <- OpenDatabaseConnection()
#' CloseDatabaseConnection(conn)
CloseDatabaseConnection <- function(conn) {
  pool::poolClose(conn)
}


#' Get column specifications
#'
#' @return A list of column specifications for each table of data.
#'
#' @examples
#' col.spec <- GetColSpec()
#'
#' # Get the names of all data tables:
#' data.names <- names(GetColSpec())
GetColSpec <- function() {
  col.spec <- list(
    CalibrationDO = readr::cols(
      VisitDate = readr::col_date(),
      CalibrationDate = readr::col_date(),
      BarometricPressure_mmHg = readr::col_double(),
      PreCalibrationReading_percent = readr::col_double(),
      PreCalibrationTemperature_C = readr::col_double(),
      PostCalibrationReading_percent = readr::col_double(),
      PostCalibrationTemperature_C = readr::col_double(),
      .default = readr::col_character()
    ),
    CalibrationpH = readr::cols(
      VisitDate = readr::col_date(),
      CalibrationDate = readr::col_date(),
      StandardValue_pH = readr::col_double(),
      TemperatureCorrectedStd_pH = readr::col_double(),
      PreCalibrationReading_pH = readr::col_double(),
      PreCalibrationTemperature_C = readr::col_double(),
      PostCalibrationReading_pH = readr::col_double(),
      PostCalibrationTemperature_C = readr::col_double(),
      .default = readr::col_character()
    ),
    CalibrationSpCond = readr::cols(
      VisitDate = readr::col_date(),
      CalibrationDate = readr::col_date(),
      StandardValue_microS_per_cm = readr::col_double(),
      PreCalibrationReading_microS_per_cm = readr::col_double(),
      PostCalibrationReading_microS_per_cm = readr::col_double(),
      .default = readr::col_character()
    ),
    DischargeEstimated = readr::cols(
      VisitDate = readr::col_date(),
      .default = readr::col_character()
    ),
    DischargeFlowCondition = readr::cols(
      VisitDate = readr::col_date(),
      SpringbrookLength_m = readr::col_double(),
      SpringbrookWidth_m = readr::col_double(),
      .default = readr::col_character()
    ),
    DischargeVolumetric = readr::cols(
      VisitDate = readr::col_date(),
      ContainerVolume_mL = readr::col_integer(),
      FillTime_seconds = readr::col_double(),
      EstimatedCapture_percent = readr::col_integer(),
      .default = readr::col_character()
    ),
    Disturbance = readr::cols(
      VisitDate = readr::col_date(),
      .default = readr::col_character()
    ),
    DisturbanceFlowModification = readr::cols(
      VisitDate = readr::col_date(),
      .default = readr::col_character()
    ),
    Invasives = readr::cols(
      VisitDate = readr::col_date(),
      .default = readr::col_character()
    ),
    Riparian = readr::cols(
      VisitDate = readr::col_date(),
      Rank = readr::col_integer(),
      .default = readr::col_character()
    ),
    SensorRetrievalAttempts = readr::cols(
      SensorNumber = readr::col_integer(),
      DeploymentDate = readr::col_date(),
      RetrievalDate = readr::col_date(),
      .default = readr::col_character()
    ),
    SensorsCurrentlyDeployed = readr::cols(
      SensorNumber = readr::col_integer(),
      VisitDate = readr::col_date(),
      .default = readr::col_character()
    ),
    Site = readr::cols(
      GRTSOrder = readr::col_integer(),
      Lat_WGS84 = readr::col_double(),
      Lon_WGS84 = readr::col_double(),
      X_UTM_NAD83_11N = readr::col_double(),
      Y_UTM_NAD83_11N = readr::col_double(),
      .default = readr::col_character()
    ),
    Visit = readr::cols(
      VisitDate = readr::col_date(),
      .default = readr::col_character()
    ),
    VisitActivity = readr::cols(
      VisitDate = readr::col_date(),
      SpringbrookWidth_m = readr::col_double(),
      SpringbrookLength_m = readr::col_double(),
      .default = readr::col_character()
    ),
    WaterQualityDO = readr::cols(
      VisitDate = readr::col_date(),
      DissolvedOxygen_percent = readr::col_double(),
      DissolvedOxygen_mg_per_L = readr::col_double(),
      .default = readr::col_character()
    ),
    WaterQualitypH = readr::cols(
      VisitDate = readr::col_date(),
      pH = readr::col_double(),
      .default = readr::col_character()
    ),
    WaterQualitySpCond = readr::cols(
      VisitDate = readr::col_date(),
      SpecificConductance_microS_per_cm = readr::col_double(),
      .default = readr::col_character()
    ),
    WaterQualityTemperature = readr::cols(
      VisitDate = readr::col_date(),
      WaterTemperature_C = readr::col_double(),
      .default = readr::col_character()
    ),
    Wildlife = readr::cols(
      VisitDate = readr::col_date(),
      .default = readr::col_character()
    )
  )

  return(col.spec)
}


#' Read desert springs data from database or .csv
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#' @param data.name The name of the analysis view or the csv file containing the data. E.g. "CalibrationDO", "DischargeVolumetric". See details for full list of data name options.
#'
#' @return A tibble of filtered data.
#'
#' @details \code{data.name} options are: CalibrationDO, CalibrationpH, CalibrationSpCond, DischargeEstimated, DischargeFlowCondition, DischargeVolumetric, Disturbance, DisturbanceFlowModification, Invasives, Riparian, SensorRetrievalAttempts, SensorsCurrentlyDeployed, Site, Visit, VisitActivity, WaterQualityDO, WaterQualitypH, WaterQualitySpCond, WaterQualityTemperature, Wildlife
#'
ReadAndFilterData <- function(conn, path.to.data, park, site, field.season, data.source = "database", data.name) {
  col.spec <- GetColSpec()

  if (!(data.source %in% c("database", "local"))) {
    stop("Please choose either 'database' or 'local' for data.source")
  } else if (data.source == "database") {
    filtered.data <- dplyr::tbl(conn, dbplyr::in_schema("analysis", data.name))
  } else if (data.source == "local") {
    filtered.data <- readr::read_csv(file.path(path.to.data, paste0(data.name, ".csv")), na = "", col_types = col.spec[[data.name]])
  }

  if (!missing(park)) {
    if (!(park %in% (dplyr::select(filtered.data, Park) %>% dplyr::collect())$Park)) {
      stop("Data are not available for the park specified")
    }
    filtered.data %<>%
      dplyr::filter(Park == park)
  }

  if (!missing(site)) {
    if (!(site %in% (dplyr::select(filtered.data, SiteCode) %>% dplyr::collect())$SiteCode)) {
      stop("Data are not available for the site specified")
    }
    filtered.data %<>%
      dplyr::filter(SiteCode == site)
  }

  if (!missing(field.season) & ("FieldSeason" %in% colnames(filtered.data))) {
    if (any(!(field.season %in% (dplyr::select(filtered.data, FieldSeason) %>% dplyr::collect())$FieldSeason))) {
      stop("Data are not available for one or more of the field seasons specified")
    } else {
      filtered.data %<>%
        dplyr::filter(FieldSeason %in% field.season)
    }
  }

  filtered.data %<>%
    dplyr::collect() %>%
    dplyr::mutate_if(is.character, trimws)

  if ("FieldSeason" %in% names(filtered.data)) {
    filtered.data %<>% dplyr::mutate(FieldSeason = as.character(FieldSeason))
  }

  # Accomodate sensor data
  if ("DeploymentFieldSeason" %in% names(filtered.data)) {
    filtered.data %<>% dplyr::mutate(DeploymentFieldSeason = as.character(DeploymentFieldSeason))
  }

  if ("RetrievalFieldSeason" %in% names(filtered.data)) {
    filtered.data %<>% dplyr::mutate(RetrievalFieldSeason = as.character(RetrievalFieldSeason))
  }

  return(filtered.data)
}

#' Save desert springs analysis views as a set of .csv files
#'
#' @param conn A database connection pool object generated from a call to \code{OpenDatabaseConnection()}.
#' @param dest.folder The folder in which to save the .csv files.
#' @param create.folders Should \code{dest.folder} be created automatically if it doesn't exist? Defaults to \code{FALSE}.
#' @param overwrite Should existing data be automatically overwritten? Defaults to \code{FALSE}.
#'
#' @return None.
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- OpenDatabaseConnection()
#' SaveDataToCsv(conn, "C:/Users/myusername/Documents/R/desert-springs-data", TRUE, TRUE)
#' CloseDatabaseConnection(conn)
#' }
SaveDataToCsv <- function(conn, dest.folder, create.folders = FALSE, overwrite = FALSE) {
  analysis.views <- c("CalibrationDO", "CalibrationpH", "CalibrationSpCond", "DischargeEstimated", "DischargeFlowCondition", "DischargeVolumetric", "Disturbance", "DisturbanceFlowModification", "Invasives", "Riparian", "Site", "Visit", "VisitActivity", "WaterQualityDO", "WaterQualitypH", "WaterQualitySpCond", "WaterQualityTemperature", "Wildlife")
  dest.folder <- file.path(dirname(dest.folder), basename(dest.folder)) # Get destination directory in a consistent format. Seems like there should be a better way to do this.
  file.paths <- file.path(dest.folder, paste0(analysis.views, ".csv"))

  # Validate inputs
  if (!dir.exists(dest.folder)) {
    if (create.folders) {
      dir.create(dest.folder, recursive = TRUE)
    } else {
      stop("Destination folder does not exist. To create it automatically, set create.folders to TRUE.")
    }
  }

  if (!overwrite & any(file.exists(file.paths))) {
    stop("Saving data in the folder provided would overwrite existing data. To automatically overwrite existing data, set overwrite to TRUE.")
  }

  # Write each analysis view in the database to csv
  for (view.name in analysis.views) {
    df <- dplyr::tbl(conn, dbplyr::in_schema("analysis", view.name)) %>%
      dplyr::collect()
    readr::write_csv(df, file.path(dest.folder, paste0(view.name, ".csv")), na = "", append = FALSE, col_names = TRUE)
  }
}

#' Raw data dump
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Spring code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A list of dataframes containing raw desert springs data.
#' @export
#'
GetRawData <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  data.dump <- list()
  data.names <- names(GetColSpec())

  for (data.name in data.names) {
    data.dump[[data.name]] <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name)
  }

  return(data.dump)
}

#' Get the name of a site from the site code
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param site.code Spring code to get the name for, e.g. "LAKE_P_HOR0042".
#' @param data.source Character string indicating whether to access data in the desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return The name of the site
#' @export
#'
GetSiteName <- function(conn, path.to.data, site.code, data.source = "database") {
  site <- ReadAndFilterData(conn, path.to.data, site = site.code, data.source = data.source, data.name = "Site")
  site %<>% dplyr::select("SiteCode", "SiteName") %>%
    unique() %>%
    dplyr::filter(SiteCode == site.code)

  return(site$SiteName)
}
