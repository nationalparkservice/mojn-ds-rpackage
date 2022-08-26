#' Overall disturbance is less than any other disturbance category
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     qcOverallDisturbance(conn)
#'     qcOverallDisturbance(conn, site = "LAKE_P_GET0066", field.season = "2019")
#'     qcOverallDisturbance(conn, park = c("DEVA", "JOTR"), field.season = c("2017", "2020", "2021"))
#'     qcOverallDisturbance(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
qcOverallDisturbance <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  disturbance <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Disturbance")
  
  overall <- disturbance %>%
    dplyr::filter((Overall < Roads & Roads != "NoData") |
                    (Overall < HumanUse & HumanUse != "NoData") |
                    (Overall < PlantManagement & PlantManagement != "NoData") |
                    (Overall < HikingTrails & HikingTrails != "NoData") |
                    (Overall < Livestock & Livestock != "NoData") |
                    (Overall < OtherAnthropogenic & OtherAnthropogenic != "NoData") |
                    (Overall < Fire & Fire != "NoData") |
                    (Overall < Flooding & Flooding != "NoData") |
                    (Overall < Wildlife & Wildlife != "NoData") |
                    (Overall < OtherNatural & OtherNatural != "NoData")) %>%
    dplyr::select(Park:Overall) %>%
    dplyr::arrange(FieldSeason, SiteCode)
  
  return(overall)
}


#' Flow modification exists, but no Human Use disturbance recorded
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     qcFlowModNoHuman(conn)
#'     qcFlowModNoHuman(conn, site = "LAKE_P_WIL0061", field.season = "2016")
#'     qcFlowModNoHuman(conn, park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#'     qcFlowModNoHuman(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
qcFlowModNoHuman <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  disturbance <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Disturbance")

  nohuman <- disturbance %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, HumanUse, FlowModificationStatus) %>%
    dplyr::filter(HumanUse == "0" & stringr::str_detect(FlowModificationStatus, "Yes")) %>%
    dplyr::arrange(FieldSeason, SiteCode)
    
  return(nohuman)
}


#' List of springs with active or historical flow modification
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     FlowModStatus(conn)
#'     FlowModStatus(conn, site = "DEVA_P_ARR0137", field.season = "2019")
#'     FlowModStatus(conn, park = c("JOTR", "LAKE"), field.season = c("2017", "2018", "2021"))
#'     FlowModStatus(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
FlowModStatus <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  flowmod <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "DisturbanceFlowModification")
  
  status <- flowmod %>%
    dplyr::filter(stringr::str_detect(FlowModificationStatus, "Yes")) %>%
    dplyr::select(-c("VisitType", "DPL")) %>%
    dplyr::arrange(SiteCode) %>%
    dplyr::group_by(Park, SiteCode, SiteName, VisitDate, FieldSeason, FlowModificationStatus) %>%
    dplyr::mutate(FlowModificationTypes = paste0(ModificationType, collapse = ", ")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("ModificationType")) %>%
    unique()
  
  return(status)
}  

 
#' List of springs that have been given different flow modification status in different field seasons   
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     qcFlowModDiscrepancies(conn)
#'     qcFlowModDiscrepancies(conn, site = "DEVA_P_WAU0880")
#'     qcFlowModDiscrepancies(conn, park = c("DEVA", "JOTR"))
#'     qcFlowModDiscrepancies(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
qcFlowModDiscrepancies <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  flowmod <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.name = "DisturbanceFlowModification") 
  
  discrepancies <- flowmod %>%
    dplyr::select(-c("VisitDate", "ModificationType", "VisitType", "DPL")) %>%
    unique() %>%
    dplyr::group_by(Park, SiteCode, SiteName, FlowModificationStatus) %>%
    dplyr::mutate(FieldSeasons = paste0(FieldSeason, collapse = ", ")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("FieldSeason")) %>%
    dplyr::filter(FlowModificationStatus != "No Data") %>%
    unique() %>%
    dplyr::filter(duplicated(SiteCode) | duplicated(SiteCode, fromLast = TRUE))
  
return(discrepancies)
}


#' Table with count and percent of springs with active and historic flow modification
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     FlowModCount(conn)
#'     FlowModCount(conn, park = c("DEVA", "JOTR"))
#'     FlowModCount(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
FlowModCount <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  flowmod <- desertsprings:::ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "DisturbanceFlowModification") 
  site <- desertsprings:::ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Site")
  
  sampleframe <- site %>%
    dplyr::select(Park, SiteCode, SiteName, GRTSOrder, SiteStatus, SampleFrame) %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr") & SiteStatus == "T-S") %>%
    dplyr::select(-c("GRTSOrder", "SiteStatus", "SampleFrame")) %>%
    unique()
  
  count <- sampleframe %>%
    dplyr::left_join(flowmod, by = c("Park", "SiteCode", "SiteName")) %>%
    dplyr::filter(VisitType %in% c("Primary", NA)) %>%
    dplyr::select(-c(VisitDate, FieldSeason, ModificationType, VisitType, DPL)) %>%
    unique() %>%
    dplyr::mutate(FlowModificationStatus = ifelse(is.na(FlowModificationStatus), "No Data", FlowModificationStatus)) %>%
    dplyr::mutate(Rank = ifelse(FlowModificationStatus == "Yes - One or more active", 4,
                              ifelse(FlowModificationStatus == "Yes - All inactive", 3,
                                    ifelse(FlowModificationStatus == "Yes - Unknown if active", 2,
                                          ifelse(FlowModificationStatus == "None", 1, 
                                              ifelse(FlowModificationStatus == "No Data", 0, NA)))))) %>%
    dplyr::group_by(Park, SiteCode, SiteName) %>%
    dplyr::slice(which.max(Rank)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(SiteCode) %>%
    dplyr::select(-Rank) %>%
    dplyr::group_by(Park, FlowModificationStatus) %>%
    dplyr::mutate(Count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(SiteCode, SiteName)) %>%
    unique()
  
  total <- aggregate(Count ~ Park, count, sum) %>%
    dplyr::rename(Total = Count)
    
 percent <- count %>%
    dplyr::left_join(total, by = "Park") %>%
    dplyr::mutate(Percent = round((Count / Total) * 100, 1)) %>%
    dplyr::select(-Total)

return(percent)
}


#' Bar plot with percent of springs with active and historic flow modification
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return ggplot bar plot
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     FlowModPlot(conn)
#'     FlowModPlot(conn, park = c("DEVA", "JOTR"))
#'     FlowModPlot(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
FlowModPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  percent <- FlowModCount(conn, path.to.data, park, site, field.season, data.source)

  percent %<>% dplyr::filter(Park != "CAMO")
  
  plot <- ggplot2::ggplot(percent, aes(x = Park, y = Percent, fill = FlowModificationStatus)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("No data" = "gray",
                                 "None" = "seagreen",
                                 "Yes - All inactive" = "gold",
                                 "Yes - One or more active" = "firebrick"),
                      name = "Flow Modification")
    
  return(plot)
}


#' Table with count and percent of springs with human use and livestock disturbance
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     DisturbanceCount(conn)
#'     DisturbanceCount(conn, park = c("DEVA", "JOTR"))
#'     DisturbanceCount(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
DisturbanceCount <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  disturbance <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Disturbance")
  flowmod <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "DisturbanceFlowModification")
  site <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Site")
  
  sampleframe <- site %>%
    dplyr::select(Park, SiteCode, SiteName, GRTSOrder, SiteStatus, SampleFrame) %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr") & SiteStatus == "T-S") %>%
    dplyr::select(-c("GRTSOrder", "SiteStatus", "SampleFrame")) %>%
    unique()
  
  disturb <- sampleframe %>%
    dplyr::left_join(disturbance, by = c("Park", "SiteCode", "SiteName")) %>%
    dplyr::filter(VisitType %in% c("Primary", NA)) %>%
    dplyr::select(-c(VisitType, DPL, Notes)) %>%
    unique() %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, HumanUse, Livestock) %>%
    dplyr::mutate(HumanUse = ifelse(((HumanUse > 0) & !(HumanUse == "NoData")), 1, 0)) %>%
    dplyr::mutate(Livestock = ifelse(((Livestock > 0) & !(Livestock == "NoData")), 1, 0)) %>%
    dplyr::group_by(Park, SiteCode, SiteName) %>%
    dplyr::summarize(Livestock = sum(Livestock), HumanUse = sum(HumanUse)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(HumanUse = ifelse(HumanUse > 0, 1, 0)) %>%
    dplyr::mutate(Livestock = ifelse(Livestock > 0, 1, 0)) %>%
    dplyr::group_by(Park) %>%
    dplyr::summarize(LivestockCount = sum(Livestock, na.rm = TRUE), HumanUseCount = sum(HumanUse, na.rm = TRUE), Total = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(LivestockPercent = round((LivestockCount/Total)*100, 1),
                  HumanUsePercent = round((HumanUseCount/Total)*100, 1)) %>%
    dplyr::mutate(LivestockCount = as.integer(LivestockCount),
                  HumanUseCount = as.integer(HumanUseCount))%>%
    dplyr::select(-Total)
  
  return(disturb)
}


#' Table with human use observations
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     HumanUseObservations(conn)
#'     HumanUseObservations(conn, site = "LAKE_P_DRI0002", field.season = "2019")
#'     HumanUseObservations(conn, park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#'     HumanUseObservations(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
HumanUseObservations <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  disturbance <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Disturbance")
  
  humanobs <- disturbance %>%
    dplyr::filter(HumanUse > 0,
                  HumanUse != "NoData") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, HumanUse, Notes)
  
  return(humanobs)
}


#' Bar plot with percent of springs with human use
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return ggplot bar plot
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     HumanUsePlot(conn)
#'     HumanUsePlot(conn, park = c("DEVA", "JOTR"))
#'     HumanUsePlot(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
HumanUsePlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  disturb <- DisturbanceCount(conn, path.to.data, park, site, field.season, data.source)
  
  humanplot <- ggplot2::ggplot(disturb, aes(x = Park, y = HumanUsePercent))+
    geom_bar(stat = "identity") +
    scale_y_continuous(limits = c(0, 100))
  
  return(humanplot)
}


#' Map of human use observations
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return leaflet map
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     HumanUseMap(conn)
#'     HumanUseMap(conn, site = "LAKE_P_DRI0002", field.season = "2019")
#'     HumanUseMap(conn, park = c("DEVA", "MOJA"), field.season = c("2017", "2018", "2021"))
#'     HumanUseMap(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
HumanUseMap <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  disturbance <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Disturbance")
  site <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Site")
  
  coords <- site %>%
    dplyr::select(SiteCode, SampleFrame, Lat_WGS84, Lon_WGS84, X_UTM_NAD83_11N, Y_UTM_NAD83_11N)
  
  humandata <- disturbance %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, HumanUse, Notes) %>%
    dplyr::mutate(Observed = dplyr::case_when(HumanUse == "0" ~ "No",
                                              HumanUse %in% c("1", "2", "3", "4") ~ "Yes",
                                              TRUE ~ "NA")) %>%
    dplyr::filter(Observed == "Yes") %>%
    dplyr::inner_join(coords, by = "SiteCode") %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr")) %>%
    dplyr::mutate(Year = as.numeric(FieldSeason)) %>%
    dplyr::relocate(Year, .after = FieldSeason)
  
  humandata$Observed <- factor(humandata$Observed, levels = c("Yes"))
  
  humandata %<>% dplyr::arrange(FieldSeason)
  
  pal <- leaflet::colorFactor(palette = c("red"),
                              domain = humandata$Observed)
  
  # Make NPS map Attribution
  NPSAttrib <-
    htmltools::HTML(
      "<a href='https://www.nps.gov/npmap/disclaimer/'>Disclaimer</a> |
      &copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a>
      &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors |
      <a class='improve-park-tiles'
      href='http://insidemaps.nps.gov/places/editor/#background=mapbox-satellite&map=4/-95.97656/39.02772&overlays=park-tiles-overlay'
      target='_blank'>Improve Park Tiles</a>"
    )
  
  NPSbasic = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSimagery = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck72fwp2642dv07o7tbqinvz4/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSslate = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpvc2e0avf01p9zaw4co8o/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSlight = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpia2u0auf01p9vbugvcpv/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  
  width <- 700
  height <- 700
  
  sd <- crosstalk::SharedData$new(humandata)
  year_filter <- crosstalk::filter_slider("year",
                                          "",
                                          sd,
                                          column = ~Year,
                                          ticks = TRUE,
                                          width = width,
                                          step = 1,
                                          sep = "",
                                          pre = "WY",
                                          post = NULL,
                                          dragRange = TRUE)
  
  lsmap <- leaflet::leaflet(sd, width = width, height = height) %>%
    leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
    leaflet::addScaleBar('bottomright') %>%
    leaflet::addCircleMarkers(lng = ~Lon_WGS84,
                              lat = ~Lat_WGS84,
                              popup = paste ("Name: ", humandata$SiteName, "<br>",
                                             "Sample Frame: ", humandata$SampleFrame, "<br>",
                                             "Water Year: ", humandata$FieldSeason, "<br>",
                                             "Human Disturbance Category: ", humandata$HumanUse, "<br>",
                                             "Notes: ", humandata$Notes),
                              radius = 5,
                              stroke = TRUE,
                              weight = 1,
                              color = "black",
                              fillOpacity = 1,
                              fillColor = ~pal(Observed),
                              group = ~Observed) %>%
    leaflet::addLegend(pal = pal,
                       values = ~Observed,
                       title = "Human Use Disturbance",
                       opacity = 1,
                       position = "bottomleft") %>%
    leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                              options=leaflet::layersControlOptions(collapsed = FALSE))
  
  humanmap <- crosstalk::bscols(list(year_filter,
                                     lsmap))
  
  return(humanmap)
}


#' Table with livestock observations
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     LivestockObservations(conn)
#'     LivestockObservations(conn, site = "MOJA_P_WIL0224", field.season = "2019")
#'     LivestockObservations(conn, park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#'     LivestockObservations(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
LivestockObservations <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  disturbance <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Disturbance")
  
  livestockobs <- disturbance %>%
    dplyr::filter(Livestock > 0,
                  Livestock != "NoData") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, Livestock, Notes)
  
  return(livestockobs)
}


#' Bar plot with percent of springs with livestock disturbance
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return ggplot bar plot
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     LivestockPlot(conn)
#'     LivestockPlot(conn, park = c("DEVA", "JOTR"))
#'     LivestockPlot(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
LivestockPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  disturb <- DisturbanceCount(conn, path.to.data, park, site, field.season, data.source)
  
  disturb %<>%
    dplyr::filter(Park != "CAMO")
  
  livestockplot <- ggplot2::ggplot(disturb, aes(x = Park, y = LivestockPercent))+
    geom_bar(stat = "identity") +
    scale_y_continuous(limits = c(0, 100)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(vjust = 0.5, hjust = 0.5))
  
  return(livestockplot)
}


#' Map of livestock disturbance observations
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return leaflet map
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     LivestockMap(conn)
#'     LivestockMap(conn, site = "MOJA_P_WIL0224", field.season = "2019")
#'     LivestockMap(conn, park = c("DEVA", "MOJA"), field.season = c("2017", "2018", "2021"))
#'     LivestockMap(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
LivestockMap <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  disturbance <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Disturbance")
  site <- desertsprings:::ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Site")
  
  coords <- site %>%
    dplyr::select(SiteCode, SampleFrame, Lat_WGS84, Lon_WGS84, X_UTM_NAD83_11N, Y_UTM_NAD83_11N)
  
  livestockdata <- disturbance %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, Livestock, Notes) %>%
    dplyr::mutate(Observed = dplyr::case_when(Livestock == "0" ~ "No",
                                              Livestock %in% c("1", "2", "3", "4") ~ "Yes",
                                              TRUE ~ "NA")) %>%
    dplyr::filter(Observed == "Yes") %>%
    dplyr::inner_join(coords, by = "SiteCode") %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr")) %>%
    dplyr::mutate(Year = as.numeric(FieldSeason)) %>%
    dplyr::relocate(Year, .after = FieldSeason)
  
  livestockdata$Observed <- factor(livestockdata$Observed, levels = c("Yes"))
  
  livestockdata %<>% dplyr::arrange(FieldSeason)
  
  pal <- leaflet::colorFactor(palette = c("red"),
                              domain = livestockdata$Observed)
  
  # Make NPS map Attribution
  NPSAttrib <-
    htmltools::HTML(
      "<a href='https://www.nps.gov/npmap/disclaimer/'>Disclaimer</a> |
      &copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a>
      &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors |
      <a class='improve-park-tiles'
      href='http://insidemaps.nps.gov/places/editor/#background=mapbox-satellite&map=4/-95.97656/39.02772&overlays=park-tiles-overlay'
      target='_blank'>Improve Park Tiles</a>"
    )
  
  NPSbasic = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSimagery = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck72fwp2642dv07o7tbqinvz4/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSslate = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpvc2e0avf01p9zaw4co8o/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSlight = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpia2u0auf01p9vbugvcpv/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  
  width <- 700
  height <- 700
  
  sd <- crosstalk::SharedData$new(livestockdata)
  year_filter <- crosstalk::filter_slider("year",
                                          "",
                                          sd,
                                          column = ~Year,
                                          ticks = TRUE,
                                          width = width,
                                          step = 1,
                                          sep = "",
                                          pre = "WY",
                                          post = NULL,
                                          dragRange = TRUE)
  
  lsmap <- leaflet::leaflet(sd, width = width, height = height) %>%
    leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
    leaflet::addScaleBar('bottomright') %>%
    leaflet::addCircleMarkers(lng = ~Lon_WGS84,
                              lat = ~Lat_WGS84,
                              popup = paste ("Name: ", livestockdata$SiteName, "<br>",
                                             "Sample Frame: ", livestockdata$SampleFrame, "<br>",
                                             "Water Year: ", livestockdata$FieldSeason, "<br>",
                                             "Livestock Disturbance Category: ", livestockdata$Livestock, "<br>",
                                             "Notes: ", livestockdata$Notes),
                              radius = 5,
                              stroke = TRUE,
                              weight = 1,
                              color = "black",
                              fillOpacity = 1,
                              fillColor = ~pal(Observed),
                              group = ~Observed) %>%
    leaflet::addLegend(pal = pal,
                       values = ~Observed,
                       title = "Livestock Disturbance",
                       opacity = 1,
                       position = "bottomleft") %>%
    leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                              options=leaflet::layersControlOptions(collapsed = FALSE))
  
  livestockmap <- crosstalk::bscols(list(year_filter,
                                    lsmap))
  
  return(livestockmap)
}


#################### Functions for Desert Springs PowerPoint -- not for final data package

LivestockPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  disturb <- DisturbanceCount(conn, path.to.data, park, site, field.season, data.source)
  
  disturb %<>%
    dplyr::filter(Park != "CAMO")
  
  livestockplot <- ggplot2::ggplot(disturb, aes(x = Park, y = LivestockPercent))+
    geom_bar(stat = "identity") +
    scale_y_continuous(limits = c(0, 100)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(vjust = 0.5, hjust = 0.5, size = 20), #
                   axis.text.y = ggplot2::element_text(size = 20), #
                   axis.title.x = ggplot2::element_text(size = 24), #
                   axis.title.y = ggplot2::element_text(size = 24)) #
  
  return(livestockplot)
}