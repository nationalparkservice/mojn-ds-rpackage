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
#'     
#'     qcSensorSummary()
#'     qcSensorSummary(park = "DEVA", deployment.field.season = c("2018", "2020", "2021"))
#'     
#' }
  qcSensorSummary <- function(park, deployment.field.season) {
    attempts <- ReadAndFilterData(park = park, field.season = deployment.field.season, data.name = "SensorRetrievalAttempts")
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
#'     
#'     qcSensorHeatmap()
#'     qcSensorHeatmap(park = c("JOTR", "MOJA"))
#'     
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
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param deployment.field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     
#'     qcSensorsNotDeployed()
#'     qcSensorsNotDeployed(site = "MOJA_P_UNN0014")
#'     qcSensorsNotDeployed(park = c("DEVA", "JOTR"), deployment.field.season = c("2017", "2018", "2021"))
#'     
#' }
qcSensorsNotDeployed <- function(park, site, deployment.field.season) {
  
  visit <- ReadAndFilterData(site, field.season = deployment.field.season)
  attempts <- ReadAndFilterData(park, site, field.season = deployment.field.season)
  deployed <- ReadAndFilterData(park, site, field.season = deployment.field.season)
  
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
  
  deployments <- rbind(deployments_past, deployments_recent) %>%
    unique()
  
  all_visits <- visit %>%
    dplyr::filter(SampleFrame == "Annual") %>%
    dplyr::select(Park, SiteCode, SiteName, FieldSeason, VisitDate) %>%
    dplyr::rename(DeploymentFieldSeason = FieldSeason)
  
  notdeployed <- annual_springs %>%
    dplyr::full_join(deployments, by = c("Park", "SiteCode", "SiteName")) %>%
    tidyr::complete(SiteCode, DeploymentFieldSeason) %>%
    dplyr::select(-Park, -SiteName) %>%
    dplyr::left_join(annual_springs, by = "SiteCode") %>% # annual springs re-appended to help filter out occasional deployments at non-annual springs
    dplyr::select(Park, SiteCode, SiteName, DeploymentFieldSeason, DeploymentDate) %>%
    dplyr::filter(!is.na(Park),
                  is.na(DeploymentDate)) %>%
    dplyr::filter(!(Park == "DEVA" & DeploymentFieldSeason %in% c("2016", "2017")),
                  !(Park == "JOTR" & DeploymentFieldSeason == "2016")) %>%
    dplyr::arrange(DeploymentFieldSeason, SiteCode) %>%
    dplyr::left_join(all_visits, by = c("Park", "SiteCode", "SiteName", "DeploymentFieldSeason")) %>%
    dplyr::rename(FieldSeason = DeploymentFieldSeason) %>%
    dplyr::relocate(VisitDate, .after = FieldSeason)
  
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
#'     
#'     qcRepeatVisits()
#'     qcRepeatVisits(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcRepeatVisits(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#'     
#' }
qcSensorsNotRecovered <- function(park, site, deployment.field.season) {
  
  visit <- ReadAndFilterData(park, site, field.season = deployment.field.season)
  attempts <- ReadAndFilterData(park, site, field.season = deployment.field.season)
  
  annual_springs <- visit %>%
    dplyr::select(Park, SiteCode, SiteName, SampleFrame) %>%
    dplyr::filter(SampleFrame == "Annual") %>%
    unique() %>%
    dplyr::select(-SampleFrame)
  
  all_springs <- visit %>%
    dplyr::select(Park, SiteCode, SiteName, SampleFrame) %>%
    unique()
  
  notrecovered <- attempts %>%
    tidyr::complete(SiteCode, RetrievalFieldSeason) %>%
    dplyr::select(-Park, -SiteName) %>%
    dplyr::left_join(all_springs, by = "SiteCode") %>%
    dplyr::select(Park, SiteCode, SiteName, RetrievalFieldSeason, RetrievalDate, SensorNumber, SerialNumber, SampleFrame, SensorRetrieved, Notes) %>%
    dplyr::filter(!(Park == "DEVA" & RetrievalFieldSeason %in% c("2017", "2018")),
                  !(Park == "JOTR" & RetrievalFieldSeason == "2017")) %>%
    dplyr::filter(SensorRetrieved == "N" | is.na(SensorRetrieved)) %>%
    dplyr::filter(!(SampleFrame != "Annual" & is.na(RetrievalDate))) %>%
    dplyr::select(-SensorRetrieved, -SampleFrame) %>%
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
#'     
#'     qcRepeatVisits()
#'     qcRepeatVisits(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcRepeatVisits(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#'     
#' }
  qcSensorProblems <- function(park, deployment.field.season) {
    attempts <- ReadAndFilterData(park = park, data.name = "SensorRetrievalAttempts")
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
#'     
#'     qcRepeatVisits()
#'     qcRepeatVisits(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcRepeatVisits(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#'     
#' }
    qcSensorDownloads <- function(park, deployment.field.season) {
      attempts <- ReadAndFilterData(park = park, data.name = "SensorRetrievalAttempts")
  nodata <- attempts %>%
    dplyr::filter(SensorRetrieved == "Y", DownloadResult == "ND") %>%
    dplyr::select(-SensorProblem, -RetrievalVisitType, -DeploymentVisitType) %>%
    dplyr::relocate(Park, .before = "SensorNumber") %>%
    dplyr::relocate(SiteCode, .after = "Park") %>%
    dplyr::relocate(SiteName, .after = "SiteCode")
  
  return(nodata)
   
}


#' Sensors were retrieved but missing, or sensors were not retrieved and not missing
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param deployment.field.season Optional. Field season name to filter on, e.g. "2019".#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     
#'     qcRepeatVisits()
#'     qcRepeatVisits(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcRepeatVisits(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#'     
#' }
#' 
qcMissingSensors <- function(park, deployment.field.season) {
  deployed <- ReadAndFilterData(park = park, data.name = "SensorsCurrentlyDeployed")
  attempts <- ReadAndFilterData(park, site, field.season = deployment.field.season, data.name = "SensorRetrievalAttempts")
  missing <- attempts %>%
    dplyr::filter((SensorRetrieved == "Y" & SensorProblem == "Missing") | (SensorRetrieved == "N" & SensorProblem != "Missing")) %>%
    dplyr::arrange(SiteCode, DeploymentFieldSeason) %>%
    dplyr::select(Park, SiteCode, SiteName, DeploymentFieldSeason, DeploymentDate, RetrievalFieldSeason, RetrievalDate, SensorRetrieved, DownloadResult, SensorProblem, SensorNumber, SerialNumber, Notes)
  
  return(missing)
   
}


#' Sensors whose retrieval date is the same as their deployment date
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param deployment.field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     
#'     qcRepeatVisits()
#'     qcRepeatVisits(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcRepeatVisits(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#'     
#' }
  qcSensorDates <- function(park, deployment.field.season) {
    attempts <- ReadAndFilterData(park = park, data.name = "SensorRetrievalAttempts")

  error <- attempts %>%
    dplyr::filter(DeploymentDate == RetrievalDate) %>%
    dplyr::relocate(Park, .before = "SensorNumber") %>%
    dplyr::relocate(SiteCode, .after = "Park") %>%
    dplyr::relocate(SiteName, .after = "SiteCode")
  
  return(error)
  
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
#'     
#'     qcRepeatVisits()
#'     qcRepeatVisits(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcRepeatVisits(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#'     
#' }
qcUnknownSensorIDs <- function(park, site, deployment.field.season) {
  sensors <- ReadAndFilterData(park, site, field.season = deployment.field.season) %>%
    dplyr::relocate(Park, .before = "SiteCode") %>%
    dplyr::relocate(SensorNumber, .after = "FieldSeason") %>%
    dplyr::relocate(SerialNumber, .after = "SensorNumber") %>%
    dplyr::select(-VisitType)

  unknown <- sensors %>%
    dplyr::filter(SensorNumber < 0 | is.na(SensorNumber) | SerialNumber == "unknown" | is.na(SerialNumber))
  
  return(unknown) 
}