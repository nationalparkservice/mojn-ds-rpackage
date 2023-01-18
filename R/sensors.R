#' Summarize sensor retrieval and download attempts by park and season
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param deployment.field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble with columns for park, deployment field season, number of sensors deployed, number of sensors for which retrieval was not attempted, number of sensors for which retrieval was attempted, number of sensors actually retrieved, number of sensors actually downloaded
#' @export
#'
#' @importFrom magrittr %>% %<>%
#' 
#' @examples
#' \dontrun{
#'     qcSensorSummary()
#'     qcSensorSummary(park = "DEVA", deployment.field.season = c("2018", "2020", "2021"))
#' }
qcSensorSummary <- function(park, deployment.field.season) {
  attempts <- ReadAndFilterData(park = park, field.season = deployment.field.season, data.name = "SensorRetrievalAttempts")
  deployments <- ReadAndFilterData(park = park, site = site, field.season = deployment.field.season, data.name = "SensorsAllDeployments")
  deployed.only <- ReadAndFilterData(park = park, field.season = deployment.field.season, data.name = "SensorsCurrentlyDeployed")
  
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
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#'
#' @return ggplot heatmap
#' @export
#'
#' @importFrom magrittr %>% %<>%
#' 
#' @examples 
#' \dontrun{
#'     qcSensorHeatmap()
#'     qcSensorHeatmap(park = c("JOTR", "MOJA"))
#' }
qcSensorHeatmap <- function(park) {
  attempts <- ReadAndFilterData(park = park, data.name = "SensorRetrievalAttempts")
  visit <- ReadAndFilterData(park = park, data.name = "Visit")
  
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
  
  plt <- ggplot2::ggplot(joined, ggplot2::aes(x = DeploymentFieldSeason, 
                                     y = reorder(SiteCode, dplyr::desc(SiteCode)),
                                     text = paste("Site Name: ", SiteName,
                                                  "<br>Site Code:", SiteCode,
                                                  "<br>Deployment Field Season:", DeploymentFieldSeason,
                                                  "<br>Sensor Result:", SensorResult))) + 
         geom_tile(aes(fill = reorder(SensorResult, SensorResultOrder)), color = "white") + 
         scale_fill_manual(values = c("seagreen", "goldenrod2", "orchid4"), name = "Outcome") +
         xlab("Deployment Field Season") +
         ylab("Spring Site Code") +
         theme(legend.position = "bottom")
  
  return(plt)
}


#' Springs where a sensor was not deployed during a field season
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param deployment.field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcSensorsNotDeployed()
#'     qcSensorsNotDeployed(site = "MOJA_P_UNN0014")
#'     qcSensorsNotDeployed(park = c("DEVA", "JOTR"), deployment.field.season = c("2017", "2018", "2021"))
#' }
qcSensorsNotDeployed <- function(park, site, deployment.field.season) {
  visit <- ReadAndFilterData(park = park, site = site, field.season = deployment.field.season, data.name = "Visit")
  deployments <- ReadAndFilterData(park = park, site = site, field.season = deployment.field.season, data.name = "SensorsAllDeployments")

  annual_springs <- visit %>%
    dplyr::select(Park, SiteCode, SiteName, SampleFrame, Panel) %>%
    dplyr::filter(SampleFrame == "Annual" & Panel == "Panel Annual") %>%
    unique() %>%
    dplyr::select(-c("SampleFrame", "Panel"))
  
  all_deployments <- deployments %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SensorNumber) %>%
    dplyr::rename(DeploymentFieldSeason = FieldSeason,
                  DeploymentDate = VisitDate)
  
  notdeployed <- annual_springs %>%
    dplyr::full_join(all_deployments, by = c("Park", "SiteCode", "SiteName")) %>%
    tidyr::complete(SiteCode, DeploymentFieldSeason) %>%
    dplyr::select(-Park, -SiteName) %>%
    dplyr::right_join(annual_springs, by = "SiteCode") %>% # annual springs re-appended to help filter out occasional deployments at non-annual springs
    dplyr::select(Park, SiteCode, SiteName, DeploymentFieldSeason, DeploymentDate) %>%
    dplyr::filter(!is.na(Park),
                  is.na(DeploymentDate)) %>%
    dplyr::filter(!(Park == "DEVA" & DeploymentFieldSeason %in% c("2016", "2017")),
                  !(Park == "JOTR" & DeploymentFieldSeason == "2016")) %>%
    dplyr::arrange(DeploymentFieldSeason, SiteCode) %>%
    dplyr::select(-DeploymentDate)
  
  return(notdeployed)
}


#' Springs where a sensor was not recovered during a field season
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param deployment.field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcRepeatVisits()
#'     qcRepeatVisits(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcRepeatVisits(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#'     
#' }
qcSensorsNotRecovered <- function(park, site, deployment.field.season) {
  visit <- ReadAndFilterData(park = park, site = site, field.season = deployment.field.season, data.name = "Visit")
  attempts <- ReadAndFilterData(park = park, site = site, field.season = deployment.field.season, data.name = "SensorRetrievalAttempts")
  deployments <- ReadAndFilterData(park = park, site = site, field.season = deployment.field.season, data.name = "SensorsAllDeployments")
  deployed.only <- ReadAndFilterData(park = park, field.season = deployment.field.season, data.name = "SensorsCurrentlyDeployed")
  
  annual_springs <- visit %>%
    dplyr::select(Park, SiteCode, SiteName, SampleFrame, Panel) %>%
    dplyr::filter(SampleFrame == "Annual",
                  Panel == "Panel Annual") %>%
    unique() %>%
    dplyr::select(-c("SampleFrame", "Panel"))
  
  all_springs <- visit %>%
    dplyr::select(Park, SiteCode, SiteName, SampleFrame, Panel) %>%
    unique()
  
  all_deployments <- deployments %>%
    dplyr::select(SiteCode, VisitDate, FieldSeason, SerialNumber)

  notrecovered <- attempts %>%
    dplyr::filter(SensorRetrieved != "Y" | (SensorRetrieved == "Y" & SensorProblem == "Missing"))
    
  notrecovered <- attempts %>%
    dplyr::select(-c("Park", "SiteName")) %>%
    tidyr::complete(SiteCode, RetrievalFieldSeason) %>%
    dplyr::left_join(all_springs, by = "SiteCode") %>%
    dplyr::filter(RetrievalFieldSeason != "2023") %>% #temporary
    dplyr::select(Park, SiteCode, SiteName, SampleFrame, Panel, RetrievalFieldSeason, RetrievalDate, RetrievalVisitType, SensorNumber, SerialNumber, SensorRetrieved, SensorProblem, Notes) %>%
    dplyr::filter(!(Park == "DEVA" & RetrievalFieldSeason %in% c("2017", "2018")),
                  !(Park == "JOTR" & RetrievalFieldSeason == "2017")) %>%
    dplyr::filter(SensorRetrieved == "N" | is.na(SensorRetrieved) | SensorProblem == "Missing") %>%
    dplyr::filter(Panel == "Panel Annual") %>%
    dplyr::select(-SampleFrame, -Panel) %>%
    dplyr::arrange(SiteCode, RetrievalFieldSeason) %>%
    dplyr::rename(FieldSeason = RetrievalFieldSeason,
                  VisitDate = RetrievalDate)
  
  return(notrecovered)
}


#' Sensors were retrieved with problems or caveats
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param deployment.field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcRepeatVisits()
#'     qcRepeatVisits(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcRepeatVisits(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
qcSensorProblems <- function(park, deployment.field.season) {
  attempts <- ReadAndFilterData(park = park, field.season = deployment.field.season, data.name = "SensorRetrievalAttempts")
  
  problems <- attempts %>%
    dplyr::filter(SensorRetrieved == "Y", !(SensorProblem %in% c("None", "Missing"))) %>%
    dplyr::relocate(DownloadResult, .after = SensorRetrieved) %>%
    dplyr::select(-RetrievalVisitType, -DeploymentVisitType) %>%
    dplyr::relocate(Park, .before = "SensorNumber") %>%
    dplyr::relocate(SiteCode, .after = "Park") %>%
    dplyr::relocate(SiteName, .after = "SiteCode")
  
  return(problems)
}


#' Sensors were retrieved, but download status is unknown
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param deployment.field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcRepeatVisits()
#'     qcRepeatVisits(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcRepeatVisits(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
qcSensorDownloads <- function(park, deployment.field.season) {
  attempts <- ReadAndFilterData(park = park, field.season = deployment.field.season, data.name = "SensorRetrievalAttempts")
  
  nodata <- attempts %>%
    dplyr::filter(SensorRetrieved == "Y",
                  DownloadResult == "ND" | is.na(DownloadResult)) %>%
    dplyr::select(-SensorProblem, -RetrievalVisitType, -DeploymentVisitType) %>%
    dplyr::relocate(Park, .before = "SensorNumber") %>%
    dplyr::relocate(SiteCode, .after = "Park") %>%
    dplyr::relocate(SiteName, .after = "SiteCode")
  
  return(nodata)
}


#' Sensor was recovered but missing, or sensor was not recovered and not missing, or sensor was not recovered but downloaded
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param deployment.field.season Optional. Field season name to filter on, e.g. "2019".#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcRepeatVisits()
#'     qcRepeatVisits(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcRepeatVisits(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
qcSensorDiscrepancies <- function(park, deployment.field.season) {
  attempts <- ReadAndFilterData(park = park, field.season = deployment.field.season, data.name = "SensorRetrievalAttempts")
  
  missing <- attempts %>%
    dplyr::filter((SensorRetrieved == "Y" & SensorProblem == "Missing") |
                  (SensorRetrieved == "N" & SensorProblem != "Missing") |
                  (SensorRetrieved == "N" & DownloadResult == "Y")) %>%
    dplyr::arrange(SiteCode, DeploymentFieldSeason) %>%
    dplyr::select(Park, SiteCode, SiteName, DeploymentFieldSeason, DeploymentDate, RetrievalFieldSeason, RetrievalDate, SensorRetrieved, DownloadResult, SensorProblem, SensorNumber, SerialNumber, Notes)
  
  return(missing)
}


#' Sensors with unknown ID or serial number
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param deployment.field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcRepeatVisits()
#'     qcRepeatVisits(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcRepeatVisits(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
qcUnknownSensorIDs <- function(park, site, deployment.field.season) {
  deployments <- ReadAndFilterData(park = park, site = site, field.season = deployment.field.season, data.name = "SensorsAllDeployments")
  attempts <- ReadAndFilterData(park = park, field.season = deployment.field.season, data.name = "SensorRetrievalAttempts")
  
  a <- attempts %>%
    dplyr::select(Park, SiteName, SiteCode, SensorNumber, SerialNumber, DeploymentDate, DeploymentFieldSeason, RetrievalDate, RetrievalFieldSeason, Notes)
  
  a_cut <- a %>%
    dplyr::select(-RetrievalDate, RetrievalFieldSeason, Notes)
  
  d <- deployments %>%
    dplyr::select(Park, SiteName, SiteCode, SensorNumber, SerialNumber, VisitDate, FieldSeason) %>%
    dplyr::rename(DeploymentDate = VisitDate,
                  DeploymentFieldSeason = FieldSeason) %>%
    dplyr::anti_join(a_cut) %>%
    dplyr::mutate(RetrievalDate = lubridate::as_date(NA),
                  RetrievalFieldSeason = as.character(NA),
                  Notes = as.character(NA))
    
  sensors <- rbind(a, d)

  unknown <- sensors %>%
    dplyr::filter(SerialNumber %in% c("unknown", "NA", "-99", "-999", "-999"))
  
  return(unknown) 
}


# visit <- desertsprings:::ReadAndFilterData(data.name = "Visit")
# attempts <- desertsprings:::ReadAndFilterData(data.name = "SensorRetrievalAttempts")
# deployments <- desertsprings:::ReadAndFilterData(data.name = "SensorsAllDeployments")
# deployed.only <- desertsprings:::ReadAndFilterData(data.name = "SensorsCurrentlyDeployed")