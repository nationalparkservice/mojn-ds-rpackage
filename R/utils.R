#' Open a Connection to the Desert Springs Database
#'
#' @param use.mojn.default Connect to the live MOJN Desert Springs database? MOJN staff should use this option. Defaults to \code{TRUE}.
#' @param drv DBI driver to use. Defaults to \code{odbc::odbc()}.
#' @param ... Additional arguments to \code{\link[pool]{dbPool}}. Ignored if \code{use.mojn.default} is \code{TRUE}.
#'
#' @return A database connection pool object
#' @export
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
      DiscontinuousSpringbrookLength_m = readr::col_double(),
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
    SensorsAllDeployments = readr::cols(
      SensorNumber = readr::col_integer(),
      VisitDate = readr::col_date(),
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
    filtered.data <- dplyr::tbl(conn, dbplyr::in_schema("analysis", data.name)) %>%
      dplyr::collect() %>%
      dplyr::mutate_if(is.character, stringr::str_trim) %>%
      dplyr::mutate_if(is.character, stringr::str_replace_all, pattern = "[\\v]+", replacement = ";  ") %>%
      dplyr::mutate_if(is.character, dplyr::na_if, "")
  } else if (data.source == "local") {
    filtered.data <- readr::read_csv(file.path(path.to.data, paste0(data.name, ".csv")), na = "", col_types = col.spec[[data.name]])
  }

  class(filtered.data) <- c("tbl_df", "tbl", "data.frame")  # R 4.0 fix: makes sure that filtered.data is the same class regardless of where it was read from
  
  if (!missing(park)) {
    filtered.data %<>%
      dplyr::filter(Park %in% park) # Changed to allow filtering of multiple parks
    if (nrow(filtered.data) == 0) {
      warning(paste0(data.name, ": Data are not available for the park specified"))
    }
  }

  if (!missing(site) & nrow(filtered.data) > 0) {
    filtered.data %<>%
      dplyr::filter(SiteCode == site)
    
    if (nrow(filtered.data) == 0) {
      warning(paste0(data.name, ": Data are not available for the site specified"))
    }
  }

  if ("FieldSeason" %in% names(filtered.data)) {
    filtered.data %<>% dplyr::mutate(FieldSeason = as.character(FieldSeason))
  }

  # Accommodate sensor data
  if ("DeploymentFieldSeason" %in% names(filtered.data)) {
    filtered.data %<>% dplyr::mutate(DeploymentFieldSeason = as.character(DeploymentFieldSeason))
  }

  if ("RetrievalFieldSeason" %in% names(filtered.data)) {
    filtered.data %<>% dplyr::mutate(RetrievalFieldSeason = as.character(RetrievalFieldSeason))
  }
  
  if (!missing(field.season) & ("FieldSeason" %in% colnames(filtered.data)) & nrow(filtered.data) > 0) {
    filtered.data %<>%
      dplyr::filter(FieldSeason %in% field.season)
    if (nrow(filtered.data) == 0) {
      warning(paste0(data.name, ": Data are not available for one or more of the field seasons specified"))
    }
  }
  
  if (!missing(field.season) & ("DeploymentFieldSeason" %in% colnames(filtered.data)) & nrow(filtered.data) > 0) {
    filtered.data %<>%
      dplyr::filter(DeploymentFieldSeason %in% field.season)
    if (nrow(filtered.data) == 0) {
      warning(paste0(data.name, ": Data are not available for one or more of the deployment field seasons specified"))
    }
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
  analysis.views <- names(GetColSpec())
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

#' Compute sample size by spring and field season
#'
#' @param data A data frame of data for which sample sizes will be calculated. To group by Park, SiteCode, and/or FieldSeason, be sure to include those columns.
#' @param ... Columns to group by.
#' @param pop Indicates if this is a population size (N) rather than a sample size (n). Defaults to FALSE.
#' 
#' @return A dataframe with a SampleSize column as well as any grouping columns (Park, SiteCode, FieldSeason) that are present in data.
#'
GetSampleSizes <- function(data, ..., pop = FALSE) {
  
  # Check for valid input
  if (nrow(data) == 0) {
    stop("The dataframe provided contains no data")
  }
  
  # Calculate sample size
  sample.size <- data %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(SampleSize = dplyr::n()) %>%
    dplyr::mutate(SampleSizeLabel = paste0("n = ", SampleSize)) %>%
    dplyr::ungroup()
  
  if (pop) {
    sample.size$SampleSizeLabel <- toupper(sample.size$SampleSizeLabel)
  }
  
  sample.size <- dplyr::left_join(data, sample.size)
  
  return(sample.size)
}

#' Apply some standard formatting to a ggplot object.
#'
#' @param plot.title The title of the plot.
#' @param sub.title Optional custom plot subtitle.
#' @param x.lab X axis label.
#' @param y.lab Y axis label.
#' @param rotate.x.labs Boolean indicating whether to rotate x axis labels 90 degrees.
#' @param ymax Optional maximum y limit.
#' @param ymin Optional minimum y limit.
#' @param xmax Optional maximum x limit.
#' @param xmin Optional minimum x limit.
#' @param data Data frame containing the data to be plotted.
#' @param x.col Column name of independent variable. If plot type only requires one variable (e.g. histogram), use only one of x.col or y.col. 
#' @param y.col Column name of dependent variable. If plot type only requires one variable (e.g. histogram), use only one of x.col or y.col.
#' @param facet.col Column to facet on. If this results in only one facet, it will be used as a subtitle instead.
#' @param n.col.facet Number of columns of facet grid.
#' @param sample.size.col Column containing sample size labels.
#' @param sample.size.loc Either 'xaxis' or 'plot'. 'xaxis' will add sample size to each x axis label. 'plot' will add sample size to the facet label (or subtitle, if only one facet).
#' @param facet.as.subtitle If only one facet, use facet name as subtitle? Defaults to TRUE.
#' @param transform.x Optional x axis transformation. One of 'log10', 'sqrt', or 'reverse'.
#' @param transform.y Optional y axis transformation. One of 'log10', 'sqrt', or 'reverse'.
#'
#' @return A ggplot object.
#' 
#' @export
#'
FormatPlot <- function(data, x.col, y.col, facet.col, n.col.facet = 2, sample.size.col, sample.size.loc, plot.title = '', sub.title = '', facet.as.subtitle = TRUE, x.lab = '', y.lab = '', rotate.x.labs = FALSE, ymax, ymin, xmax, xmin, transform.x, transform.y) {
  
  x.col <- dplyr::enquo(x.col)
  facet.col <- dplyr::enquo(facet.col)
  sample.size.col <- dplyr::enquo(sample.size.col)
  
  # Add sample size information to either x axis labels or facet/subtitle
  if (!missing(sample.size.col) & !missing(sample.size.loc)) {
    if (sample.size.loc == 'xaxis') {
      data %<>% dplyr::mutate(!!x.col := paste0(!!x.col, '\n', !!sample.size.col))
    } else if (sample.size.loc == 'plot' & !missing(facet.col)) {
      data %<>% dplyr::mutate(!!facet.col := paste0(!!facet.col, ' (', !!sample.size.col, ')'))
    } else {
      facet.col <- sample.size.col
    }
  }
  
  # Allow for 1 or 2 variables
  if (!missing(y.col) & !missing(x.col)) {
    y.col <- dplyr::enquo(y.col)
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x.col, y = !!y.col))
  } else if (!missing(x.col)) {
    p <- ggplot2::ggplot(data, ggplot2::aes(!!x.col))
  } else if (!missing(y.col)) {
    p <- ggplot2::ggplot(data, ggplot2::aes(!!y.col))
  }
  
  
  # Create facets if >1 event group, otherwise create subtitle
  if (!missing(facet.col)) {
    facets <- unique(dplyr::select(data, !!facet.col))
    if (nrow(facets) > 1) {
      p <- p + ggplot2::facet_wrap(ggplot2::vars(!!facet.col), ncol = n.col.facet, scales = 'free')
    } else if (sub.title == '' & facet.as.subtitle) {
      sub.title <- facets
    }
  }
  
  # Add title and subtitle if not blank
  if (!missing(plot.title) & plot.title != '') {
    p <- p + ggplot2::labs(title = plot.title)
  }
  if (!missing(sub.title) & sub.title != '') {
    p <- p + ggplot2::labs(subtitle = sub.title)
  }
  
  # Add x and y axis titles if not blank
  if (x.lab != "") {
    p <- p + ggplot2::xlab(x.lab)
  } else {
    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }
  
  if (y.lab != "") {
    p <- p + ggplot2::ylab(y.lab)
  } else {
    p <- p + ggplot2::theme(axis.title.y = ggplot2::element_blank())
  }
  
  # Rotate x labels 90 degrees if rotate.x.labs is TRUE
  if (!missing(rotate.x.labs)) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
  }
  
  # Set ymin and ymax if provided
  if (!missing(ymin) & !missing(ymax)) {
    p <- p + ggplot2::expand_limits(y = c(ymin, ymax))
  } else if (!missing(ymax)) {
    p <- p + ggplot2::expand_limits(y = ymax)
  } else if (!missing(ymin)) {
    p <- p + ggplot2::expand_limits(y = ymin)
  }
  
  # Set xmin and xmax if provided
  if (!missing(xmin) & !missing(xmax)) {
    p <- p + ggplot2::expand_limits(x = c(xmin, xmax))
  } else if (!missing(xmax)) {
    p <- p + ggplot2::expand_limits(x = xmax)
  } else if (!missing(xmin)) {
    p <- p + ggplot2::expand_limits(x = xmin)
  }
  
  # Tranform x axis, if transformation specified
  if (!missing(transform.x)) {
    if (transform.x == 'log10') {
      p <- p + ggplot2::scale_x_log10()
    } else if (transform.x == 'sqrt') {
      p <- p + ggplot2::scale_x_sqrt()
    } else if (transform.x == 'reverse') {
      p <- p + ggplot2::scale_x_reverse()
    } else {
      stop(paste0("The x transformation specified, '", transform.x, "' is not a valid option."))
    }
  }
  
  # Transform y axis, if transformation specified
  if (!missing(transform.y)) {
    if (transform.y == 'log10') {
      p <- p + ggplot2::scale_y_log10()
    } else if (transform.y == 'sqrt') {
      p <- p + ggplot2::scale_y_sqrt()
    } else if (transform.y == 'reverse') {
      p <- p + ggplot2::scale_y_reverse()
    } else {
      stop(paste0("The y transformation specified, '", transform.y, "' is not a valid option."))
    }
  }
  
  return(p)
}

#' Test for dataframe equivalence
#'
#' @param result Actual data frame
#' @param expected Expected data frame
#' @param ignore_col_order Ignore order of columns in dataframe? Defaults to FALSE.
#' @param ignore_row_order Ignore order of rows in dataframe? Defaults to TRUE.
#' @param convert Convert similar classes (factor to character, int to double)?
#'
#' @return If test passes, nothing. If it fails, description of failure.
#' 
#' @export
#'
expect_dataframe_equal <- function(result, expected, ignore_col_order = FALSE, ignore_row_order = TRUE, convert = FALSE) {
  test_result <- dplyr::all_equal(result, expected, ignore_col_order = FALSE, ignore_row_order = TRUE, convert = FALSE)
  return(expect_true(test_result, label = test_result))
}