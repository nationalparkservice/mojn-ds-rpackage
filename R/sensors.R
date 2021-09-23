#' Summarize sensor retrieval and download attempts by park and season
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param deployment.field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns for park, deployment field season, number of sensors deployed, number of sensors for which retrieval was not attempted, number of sensors for which retrieval was attempted, number of sensors actually retrieved, number of sensors actually downloaded
#' @export
#'
#' @importFrom magrittr %>% %<>%
qcSensorSummary <- function(conn, path.to.data, park, deployment.field.season, data.source = "database") {
  attempts <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, field.season = deployment.field.season, data.source = data.source, data.name = "SensorRetrievalAttempts")
  deployed.only <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, field.season = deployment.field.season, data.source = data.source, data.name = "SensorsCurrentlyDeployed")

  # Number of sensors that have been deployed but no retrieval (successful or unsuccessful) has been attempted
  deployed.only
  no.attempt <- deployed.only %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::select(Park, DeploymentFieldSeason = FieldSeason) %>%
    dplyr::group_by(Park, DeploymentFieldSeason) %>%
    dplyr::summarize(NoRetrievalAttempted = dplyr::n()) %>%
    dplyr::ungroup()

  # If multiple retrieval attempts have been made on the same sensor, only count the most recent
  latest.attempts <- attempts %>%
    dplyr::select(Park, SiteCode, SensorNumber, SerialNumber, DeploymentDate, RetrievalDate) %>%
    dplyr::group_by(Park, SiteCode, SensorNumber, SerialNumber, DeploymentDate) %>%
    dplyr::summarize(RetrievalDate = max(RetrievalDate, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(attempts, by = c("Park", "SiteCode", "SensorNumber", "SerialNumber", "DeploymentDate", "RetrievalDate"))

  deployed <- latest.attempts %>%
    dplyr::filter(DeploymentVisitType == "Primary") %>%
    dplyr::select(Park, DeploymentFieldSeason) %>%
    dplyr::group_by(Park, DeploymentFieldSeason) %>%
    dplyr::summarise(RetrievalAttempted = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(no.attempt, by = c("Park", "DeploymentFieldSeason")) %>%
    tidyr::replace_na(list(NoRetrievalAttempted = 0, RetrievalAttempted = 0)) %>%
    dplyr::mutate(Deployed = RetrievalAttempted + NoRetrievalAttempted)

  retrieved <- latest.attempts %>%
    dplyr::filter(DeploymentVisitType == "Primary" & SensorRetrieved == "Y") %>%
    dplyr::select(Park, DeploymentFieldSeason) %>%
    dplyr::group_by(Park, DeploymentFieldSeason) %>%
    dplyr::summarise(Retrieved = dplyr::n()) %>%
    dplyr::ungroup()

  downloaded <- latest.attempts %>%
    dplyr::filter(DeploymentVisitType == "Primary" & DownloadResult == "Y") %>%
    dplyr::select(Park, DeploymentFieldSeason) %>%
    dplyr::group_by(Park, DeploymentFieldSeason) %>%
    dplyr::summarise(Downloaded = dplyr::n()) %>%
    dplyr::ungroup()

  summary <- deployed %>%
    dplyr::full_join(retrieved, by = c("Park", "DeploymentFieldSeason")) %>%
    dplyr::full_join(downloaded, by = c("Park", "DeploymentFieldSeason")) %>%
    tidyr::replace_na(list(Retrieved = 0, Downloaded = 0)) %>%
    dplyr::select(Park, DeploymentFieldSeason, Deployed, NoRetrievalAttempted, RetrievalAttempted, Retrieved, Downloaded) %>%
    dplyr::arrange(Park, DeploymentFieldSeason)

  return(summary)
}

#' Plot sensor retrieval results over time as a heatmap.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A ggplot object.
#' @export
#'
#' @importFrom magrittr %>% %<>%
qcSensorHeatmap <- function(conn, path.to.data, park, data.source = "database") {
  attempts <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "SensorRetrievalAttempts")
  attempts %<>%
    filter(DeploymentVisitType == "Primary") %>%
    mutate(SensorResult = if_else(DownloadResult == "Y", "Download successful",
                                  if_else(SensorRetrieved == "Y", "Retrieved, download failed", "Lost")),
           SensorResultOrder = if_else(DownloadResult == "Y", 1,
                                       if_else(SensorRetrieved == "Y", 2, 3)))
  
  plt <- ggplot(attempts, aes(x = DeploymentFieldSeason, 
                              y = reorder(SiteCode, desc(SiteCode)))) + 
    geom_tile(aes(fill = reorder(SensorResult, SensorResultOrder)), color = "white") + 
    scale_fill_manual(values = c("green", "yellow", "red"), name = "Outcome")
  
  return(plt)
}

#' Problems with retrieved sensors
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param deployment.field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble
#' @export
#'
#' @examples
qcSensorProblems <- function(conn, path.to.data, park, deployment.field.season, data.source = "database") {
  attempts <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "SensorRetrievalAttempts")
  
  problems <- attempts %>%
    dplyr::filter(SensorRetrieved == "Y", SensorProblem != "None")
  
  return(problems)
  
}

#' Sensors were retrieved, but download status is unknown
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param deployment.field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble
#' @export
#'
#' @examples
qcSensorDownloads <- function(conn, path.to.data, park, deployment.field.season, data.source = "database") {
  attempts <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "SensorRetrievalAttempts")
  
  nodata <- attempts %>%
    dplyr::filter(SensorRetrieved == "Y", DownloadResult == "ND")
  
  return(nodata)
   
}

#' Sensors were deployed in previous field seasons and are still unaccounted for 
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param deployment.field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.

#' @return A tibble
#' @export
#'
#' @examples
qcMissingSensors <- function(conn, path.to.data, park, deployment.field.season, data.source = "database") {
  deployed <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "SensorsCurrentlyDeployed")
  
  current.date <- Sys.Date()
  
  if(lubridate::month(current.date) >= 10) {
    current.fs <- (lubridate::year(current.date) - 1)
  } else {
    current.fs <- lubridate::year(current.date)
  }
  
  missing <- deployed %>%
    dplyr::filter(FieldSeason != current.fs) %>%
    dplyr::arrange(FieldSeason, SiteCode)
  
  return(missing)
   
}

#' Sensors whose retrieval date is the same as their deployment date
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param deployment.field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble
#' @export
#'
#' @examples
qcSensorDates <- function(conn, path.to.data, park, deployment.field.season, data.source = "database") {
  attempts <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "SensorRetrievalAttempts")
 
  error <- attempts %>%
    dplyr::filter(DeploymentDate == RetrievalDate)
  
  return(error)
  
}


#' Springs with no sensor deployment data for latest field season
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble
#' @export
#'
#' @examples
qcSensorsNoData <- function(conn, path.to.data, park, site, data.source = "database") {
 
  visit <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Visit")
  attempts <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "SensorRetrievalAttempts")
  
  visit.x <- visit %>%
    select(Park, SiteCode, SiteName, SampleFrame) %>%
    filter(SampleFrame == "Annual") %>%
    unique()
  
  attempts.x <- attempts %>%
    filter(RetrievalFieldSeason == "2021") %>%
    select(Park, SiteCode, SiteName, DeploymentDate, DeploymentFieldSeason, RetrievalDate, RetrievalFieldSeason, SensorNumber, SensorRetrieved)
  
  discrepancies <- visit.x %>%
    dplyr::left_join(attempts.x, by = c("Park", "SiteCode", "SiteName")) %>%
    dplyr::filter(is.na(SensorRetrieved))
  
  return(discrepancies)
}