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
    dplyr::arrange(Park, DeploymentFieldSeason) %>%
    dplyr::mutate(Percent_Retrieved = round(Retrieved/RetrievalAttempted*100, 1),
                  Percent_Downloaded = round(Downloaded/Retrieved*100, 1))

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
  visit <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Visit")
  
  sampleframe <- visit %>%
    dplyr::select(SiteCode, SampleFrame)
  
  joined <- attempts %>%
    dplyr::left_join(sampleframe, by = c("SiteCode"))
  
  joined %<>%
    # filter(DeploymentVisitType == "Primary") %>%
    dplyr::filter(SampleFrame == "Annual") %>%
    dplyr::mutate(SensorResult = dplyr::if_else(DownloadResult == "Y", "Download successful",
                                 dplyr::if_else(SensorRetrieved == "Y", "Download failed", "Not retrieved")),
                  SensorResultOrder = dplyr::if_else(DownloadResult == "Y", 1,
                                      dplyr::if_else(SensorRetrieved == "Y", 2, 3)))
  
  plt <- ggplot2::ggplot(joined, aes(x = DeploymentFieldSeason, 
                                     y = reorder(SiteCode, dplyr::desc(SiteCode)),
                                     text = paste("Site Name: ", SiteName,
                                                  "<br>Site Code:", SiteCode,
                                                  "<br>Deployment Field Season:", DeploymentFieldSeason,
                                                  "<br>Sensor Result:", SensorResult))) + 
         geom_tile(aes(fill = reorder(SensorResult, SensorResultOrder)), color = "white") + 
         scale_fill_manual(values = c("seagreen", "gold", "firebrick"), name = "Outcome") +
         xlab("Deployment Field Season") +
         ylab("Spring Site Code") +
         theme(legend.position = "bottom")
  
  return(plt)
}


#' Springs where a sensor was not deployed during a field season
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param deployment.field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble
#' @export
#'
#' @examples
qcSensorsNotDeployed <- function(conn, path.to.data, park, site, deployment.field.season, data.source = "database") {
  
  visit <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Visit")
  attempts <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "SensorRetrievalAttempts")
  deployed <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, field.season = deployment.field.season, data.source = data.source, data.name = "SensorsCurrentlyDeployed")
  
  annual_springs <- visit %>%
    dplyr::select(Park, SiteCode, SiteName, SampleFrame) %>%
    dplyr::filter(SampleFrame == "Annual") %>%
    unique() %>%
    dplyr::select(-SampleFrame)
  
  all_springs <- visit %>%
    dplyr::select(Park, SiteCode, SiteName) %>%
    unique()
  
  deployments_past <- attempts %>%
    dplyr::select(Park, SiteCode, SiteName, DeploymentDate, DeploymentFieldSeason)
  
  deployments_recent <- deployed %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason) %>%
    dplyr::rename(DeploymentDate = VisitDate,
                  DeploymentFieldSeason = FieldSeason)
  
  deployments <- rbind(deployments_past, deployments_recent)
  
  notdeployed <- annual_springs %>%
    dplyr::full_join(deployments, by = c("Park", "SiteCode", "SiteName")) %>%
    tidyr::complete(SiteCode, DeploymentFieldSeason) %>%
    dplyr::select(-Park, -SiteName) %>%
    dplyr::left_join(annual_springs, by = "SiteCode") %>%
    dplyr::select(Park, SiteCode, SiteName, DeploymentFieldSeason, DeploymentDate) %>%
    dplyr::filter(!is.na(Park),
                  is.na(DeploymentDate)) %>%
    dplyr::filter(!(Park == "DEVA" & DeploymentFieldSeason %in% c("2016", "2017")),
                  !(Park == "JOTR" & DeploymentFieldSeason == "2016")) %>%
    dplyr::arrange(SiteCode, DeploymentFieldSeason)
  
  return(notdeployed)
  
}


#' Springs where a sensor was not retrieved during a field season
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param deployment.field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble
#' @export
#'
#' @examples
qcSensorsNotRecovered <- function(conn, path.to.data, park, site, deployment.field.season, data.source = "database") {
  
  visit <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Visit")
  attempts <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "SensorRetrievalAttempts")
  
  annual_springs <- visit %>%
    dplyr::select(Park, SiteCode, SiteName, SampleFrame) %>%
    dplyr::filter(SampleFrame == "Annual") %>%
    unique() %>%
    dplyr::select(-SampleFrame)
  
  all_springs <- visit %>%
    dplyr::select(Park, SiteCode, SiteName, SampleFrame) %>%
    unique()
  
  notretrieved <- attempts %>%
    tidyr::complete(SiteCode, RetrievalFieldSeason) %>%
    dplyr::select(-Park, -SiteName) %>%
    dplyr::left_join(all_springs, by = "SiteCode") %>%
    dplyr::select(Park, SiteCode, SiteName, RetrievalFieldSeason, RetrievalDate, SensorNumber, SerialNumber, SampleFrame, SensorRetrieved) %>%
    dplyr::filter(!(Park == "DEVA" & RetrievalFieldSeason %in% c("2017", "2018")),
                  !(Park == "JOTR" & RetrievalFieldSeason == "2017")) %>%
    dplyr::filter(SensorRetrieved == "N" | is.na(SensorRetrieved)) %>%
    dplyr::filter(!(SampleFrame != "Annual" & is.na(RetrievalDate))) %>%
    dplyr::select(-SensorRetrieved, -SampleFrame) %>%
    dplyr::arrange(SiteCode, RetrievalFieldSeason)
  
  return(notretrieved)
  
}


#' Sensors were retrieved with problems or caveats
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
    dplyr::filter(SensorRetrieved == "Y", !(SensorProblem %in% c("None", "Missing"))) %>%
    dplyr::relocate(DownloadResult, .after = SensorRetrieved) %>%
    dplyr::select(-RetrievalVisitType, -DeploymentVisitType)
  
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
    dplyr::filter(SensorRetrieved == "Y", DownloadResult == "ND") %>%
    dplyr::select(-SensorProblem, -RetrievalVisitType, -DeploymentVisitType)
  
  return(nodata)
   
}


#' Sensors were retrieved but missing, or sensors were not retrieved and not missing
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
  attempts <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "SensorRetrievalAttempts")
  deployed <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "SensorsCurrentlyDeployed")
  
  missing <- attempts %>%
    dplyr::filter((SensorRetrieved == "Y" & SensorProblem == "Missing") | (SensorRetrieved == "N" & SensorProblem != "Missing")) %>%
    dplyr::arrange(SiteCode, DeploymentFieldSeason) %>%
    dplyr::select(Park, SiteCode, SiteName, DeploymentFieldSeason, DeploymentDate, RetrievalFieldSeason, RetrievalDate, SensorRetrieved, DownloadResult, SensorProblem, SensorNumber, SerialNumber)
  
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