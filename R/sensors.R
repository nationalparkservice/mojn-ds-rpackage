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
SensorQcSummary <- function(conn, path.to.data, park, deployment.field.season, data.source = "database") {
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
SensorQcHeatmap <- function(conn, path.to.data, park, data.source = "database") {
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
