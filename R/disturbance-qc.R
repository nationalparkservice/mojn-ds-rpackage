#' Intermediate step used to reformat disturbance categories into numerical values that can be read by other functions.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
qcDisturbanceFormatted <- function(park, site, field.season) {
  disturbance <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Disturbance")
  
  formatted <- disturbance %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::mutate_at(c("Roads",
                       "HumanUse",
                       "PlantManagement",
                       "HikingTrails",
                       "Livestock",
                       "OtherAnthropogenic",
                       "Fire",
                       "Flooding",
                       "Wildlife",
                       "OtherNatural",
                       "Overall"),
                       list(~dplyr::case_when(. == ">0 - 25%" ~ "1",
                                              . == ">25 - 50%" ~ "2",
                                              . == ">50 - 75%" ~ "3",
                                              . == ">75 - 100%" ~ "4",
                                              . == "0" ~ "0",
                                              TRUE ~ "NoData"))) %>%
    dplyr::mutate(FlowModificationStatus = dplyr::case_when(is.na(FlowModificationStatus) ~ "NoData",
                                                            FlowModificationStatus == "No Data" ~ "NoData",
                                                            TRUE ~ FlowModificationStatus)) %>%
    dplyr::select(-DPL)

  return(formatted)
}


#' Intermediate step used to reformat flow modification types that can be read by other functions.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
qcFlowModFormatted <- function(park, site, field.season) {
  flowmod <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "DisturbanceFlowModification")
  
  formatted <- flowmod %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::mutate(ModificationType = dplyr::case_when(ModificationType == "Exc" ~ "Excavation",
                                                       TRUE ~ ModificationType)) %>%
    dplyr::mutate(FlowModificationStatus = dplyr::case_when(is.na(FlowModificationStatus) ~ "NoData",
                                                            FlowModificationStatus == "No Data" ~ "NoData",
                                                            TRUE ~ FlowModificationStatus)) %>%
    dplyr::select(-DPL)
  
  return(formatted)
}


#' Overall disturbance is less than any other disturbance category
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcOverallDisturbance()
#'     qcOverallDisturbance(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcOverallDisturbance(park = c("DEVA", "JOTR"), field.season = c("2017", "2020", "2021"))
#' }
qcOverallDisturbance <- function(park, site, field.season) {
  formatted <- qcDisturbanceFormatted(park = park, site = site, field.season = field.season)
  
  overall <- formatted %>%
    dplyr::filter((Overall < Roads & Roads != "NoData") |
                  (Overall < HumanUse & HumanUse != "NoData") |
                  (Overall < PlantManagement & PlantManagement != "NoData") |
                  (Overall < HikingTrails & HikingTrails != "NoData") |
                  (Overall < Livestock & Livestock != "NoData") |
                  (Overall < OtherAnthropogenic & OtherAnthropogenic != "NoData") |
                  (Overall < Fire & Fire != "NoData") |
                  (Overall < Flooding & Flooding != "NoData") |
                  (Overall < Wildlife & Wildlife != "NoData") |
                  (Overall < OtherNatural & OtherNatural != "NoData") |
                  (Overall == "NoData" & !(FieldSeason %in% c("2016", "2017")))) %>%
    dplyr::select(Park:Overall) %>%
    dplyr::select(-SampleFrame, -Panel) %>%
    dplyr::arrange(FieldSeason, SiteCode)
  
  return(overall)
}


#' Flow modification exists, but no Human Use disturbance recorded
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcFlowModNoHuman()
#'     qcFlowModNoHuman(site = "LAKE_P_WIL0061", field.season = "2016")
#'     qcFlowModNoHuman(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
qcFlowModNoHumanUse <- function(park, site, field.season) {
  formatted <- qcDisturbanceFormatted(park = park, site = site, field.season = field.season)
  
  nohuman <- formatted %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, HumanUse, FlowModificationStatus) %>%
    dplyr::filter(HumanUse == "0" & stringr::str_detect(FlowModificationStatus, "Yes")) %>%
    dplyr::arrange(FieldSeason, SiteCode)
    
  return(nohuman)
}


#' List of springs with active or historical flow modification
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     FlowModStatus()
#'     FlowModStatus(site = "DEVA_P_ARR0137", field.season = "2019")
#'     FlowModStatus(park = c("JOTR", "LAKE"), field.season = c("2017", "2018", "2021"))
#' }
FlowModStatus <- function(park, site, field.season) {
  formatted <- qcFlowModFormatted(park = park, site = site, field.season = field.season)
  
  status <- formatted %>%
    dplyr::filter(stringr::str_detect(FlowModificationStatus, "Yes")) %>%
    dplyr::select(-VisitType) %>%
    dplyr::arrange(SiteCode) %>%
    dplyr::group_by(Park, SiteCode, SiteName, VisitDate, FieldSeason, FlowModificationStatus) %>%
    dplyr::mutate(FlowModificationTypes = paste0(ModificationType, collapse = ", ")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-ModificationType, -SampleFrame, -Panel) %>%
    unique()
  
  return(status)
}  

 
#' List of springs that have been given different flow modification status in different field seasons   
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcFlowModDiscrepancies()
#'     qcFlowModDiscrepancies(site = "DEVA_P_WAU0880")
#'     qcFlowModDiscrepancies(park = c("DEVA", "JOTR"))
#' }
qcFlowModDiscrepancies <- function(park, site, field.season) {
  formatted <- qcFlowModFormatted(park = park, site = site, field.season = field.season)
  
  discrepancies <- formatted %>%
    dplyr::select(-c("VisitDate", "ModificationType", "VisitType", "SampleFrame", "Panel")) %>%
    unique() %>%
    dplyr::group_by(Park, SiteCode, SiteName, FlowModificationStatus) %>%
    dplyr::mutate(FieldSeasons = paste0(sort(FieldSeason), collapse = ", ")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-FieldSeason) %>%
    dplyr::filter(FlowModificationStatus != "NoData") %>%
    unique() %>%
    dplyr::filter(duplicated(SiteCode) | duplicated(SiteCode, fromLast = TRUE)) %>%
    dplyr::arrange(SiteCode)
  
  return(discrepancies)
}


#' List of springs where no flow modification was observed but a modification type was selected or where flow modification was observed but no modification type was selected
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcFlowModTypes()
#'     qcFlowModTypes(site = "DEVA_P_WAU0880")
#'     qcFlowModTypes(park = c("MOJA", "LAKE"))
#' }
qcFlowModTypes <- function(park, site, field.season) {
  formatted <- qcFlowModFormatted(park = park, site = site, field.season = field.season)

  nomod <- formatted %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, FlowModificationStatus, ModificationType) %>%
    dplyr::filter((FlowModificationStatus %in% c("NoData", "None") & !is.na(ModificationType)) |
                  (stringr::str_detect(FlowModificationStatus, "Yes") & is.na(ModificationType))) %>%
    dplyr::arrange(FieldSeason, SiteCode)
  
  return(nomod)
}


#' Table with count and percent of springs with active and historic flow modification
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     FlowModCount()
#'     FlowModCount(park = c("DEVA", "JOTR"))
#' }
FlowModCount <- function(park, site, field.season) {
  formatted <- qcFlowModFormatted(park = park, site = site, field.season = field.season)

  site <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Site") %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr"),
                  Panel %in% c("A", "B", "C", "D")) %>%
    dplyr::select(Park, SiteCode, SiteName)
  
  count <- formatted %>%
    dplyr::filter(VisitType %in% c("Primary", NA),
                  Panel %in% c("Panel Annual", "Panel B", "Panel C", "Panel D")) %>%
    dplyr::select(-c(VisitDate, FieldSeason, ModificationType, VisitType, SampleFrame, Panel)) %>%
    unique() %>%
    dplyr::full_join(site, by = c("Park", "SiteCode", "SiteName"), multiple = "all", relationship = "many-to-many") %>%
    dplyr::mutate(FlowModificationStatus = dplyr::case_when(FlowModificationStatus == "NoData" ~ "No data",
                                                            is.na(FlowModificationStatus) ~ "No data",
                                                            TRUE ~ FlowModificationStatus)) %>%
    dplyr::mutate(Rank = ifelse(FlowModificationStatus == "Yes - One or more active", 4,
                              ifelse(FlowModificationStatus == "Yes - All inactive", 3,
                                    ifelse(FlowModificationStatus == "Yes - Unknown if active", 2,
                                          ifelse(FlowModificationStatus == "None", 1, 
                                              ifelse(FlowModificationStatus == "No data", 0, NA)))))) %>%
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
    dplyr::left_join(total, by = "Park", multiple = "all") %>%
    dplyr::mutate(Percent = round((Count / Total) * 100, 1)) %>%
    dplyr::select(-Total)

  return(percent)
}


#' Bar plot with percent of springs with active and historic flow modification
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return ggplot bar plot
#' @export
#'
#' @examples
#' \dontrun{
#'     FlowModPlot()
#'     FlowModPlot(park = c("DEVA", "JOTR"))
#' }
FlowModPlot <- function(park, site, field.season) {
  percent <- FlowModCount(park = park, site = site, field.season = field.season)

  percent %<>% dplyr::filter(Park != "CAMO")
  
  plot <- ggplot2::ggplot(percent, ggplot2::aes(x = Park,
                                                y = Percent,
                                                fill = FlowModificationStatus,
                                                text = paste0("Park: ", Park, "<br>",
                                                              "Flow Modification: ", FlowModificationStatus, "<br>",
                                                              "Percent: ", Percent))) +
    ggplot2::geom_bar(stat = "identity", color = "white") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_fill_manual(values = c("No data" = "gray70",
                                          "None" = "royalblue",
                                          "Yes - All inactive" = "goldenrod2",
                                          "Yes - Unknown if active" = "darkorange1",
                                          "Yes - One or more active" = "firebrick"),
                               name = "Flow Modification") +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(Percent > 4.5, paste0(Percent, "%"), ""),
                           color = FlowModificationStatus),
                       position = ggplot2::position_stack(vjust = 0.5),
                       size = 4,
                       show.legend = FALSE) +
    ggplot2::scale_color_manual(values = c("black", "white", "black", "white", "white"),
                                breaks = c("No data", "None", "Yes - All inactive", "Yes - Unknown if active", "Yes - One or more active")) +
    ggplot2::guides(color = "none")
    
  return(plot)
}


#' Table with count and percent of springs with human use and livestock disturbance
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     DisturbanceCount()
#'     DisturbanceCount(park = c("DEVA", "JOTR"))
#' }
DisturbanceCount <- function(park, site, field.season) {
  formatted <- qcDisturbanceFormatted(park = park, site = site, field.season = field.season)

  disturb <- formatted %>%
    dplyr::filter(VisitType %in% c("Primary", NA),
                  Panel %in% c("Panel Annual", "Panel B", "Panel C", "Panel D")) %>%
    dplyr::select(-VisitType) %>%
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
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     HumanUseObservations()
#'     HumanUseObservations(site = "LAKE_P_DRI0002", field.season = "2019")
#'     HumanUseObservations(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
HumanUseObservations <- function(park, site, field.season) {
  formatted <- qcDisturbanceFormatted(park = park, site = site, field.season = field.season)
  
  humanobs <- formatted %>%
    dplyr::filter(HumanUse > 0,
                  HumanUse != "NoData") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, HumanUse, Notes) %>%
    dplyr::mutate(HumanUse = dplyr::case_when(HumanUse == "1" ~ ">0 - 25%",
                                              HumanUse == "2" ~ ">25 - 50%",
                                              HumanUse == "3" ~ ">50 - 75%",
                                              HumanUse == "4" ~ ">75 - 100%",
                                              HumanUse == "0" ~ "0%",
                                              TRUE ~ HumanUse))
  
  return(humanobs)
}


#' Bar plot with percent of springs with human use
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return ggplot bar plot
#' @export
#'
#' @examples
#' \dontrun{
#'     HumanUsePlot()
#'     HumanUsePlot(park = c("DEVA", "JOTR"))
#' }
HumanUsePlot <- function(park, site, field.season) {
  count <- DisturbanceCount(park = park, site = site, field.season = field.season)
  
  count  %<>%
    dplyr::filter(Park != "CAMO")
  
  humanplot <- ggplot2::ggplot(count, ggplot2::aes(x = Park, y = HumanUsePercent))+
    geom_bar(stat = "identity") +
    scale_y_continuous(limits = c(0, 100)) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(HumanUsePercent, "%")),
                       vjust = -1,
                       size = 4,
                       show.legend = FALSE)
  
  return(humanplot)
}


#' Map of human use observations
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return leaflet map
#' @export
#'
#' @examples
#' \dontrun{
#'     HumanUseMap()
#'     HumanUseMap(site = "LAKE_P_DRI0002", field.season = "2019")
#'     HumanUseMap(park = c("DEVA", "MOJA"), field.season = c("2017", "2018", "2021"))
#' }
HumanUseMap <- function(park, site, field.season) {
  formatted <- qcDisturbanceFormatted(park = park, site = site, field.season = field.season)
  site <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Site")
  
  coords <- site %>%
    dplyr::select(SiteCode, Lat_WGS84, Lon_WGS84, X_UTM_NAD83_11N, Y_UTM_NAD83_11N)
  
  humandata <- formatted %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, HumanUse, Notes) %>%
    dplyr::mutate(Observed = dplyr::case_when(HumanUse == "0" ~ "No",
                                              HumanUse %in% c("1", "2", "3", "4") ~ "Yes",
                                              TRUE ~ "NA")) %>%
    dplyr::filter(Observed == "Yes") %>%
    dplyr::left_join(coords, by = "SiteCode", multiple = "all", relationship = "many-to-one") %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr") & Panel %in% c("A", "B", "C", "D")) %>%
    dplyr::mutate(Year = as.numeric(FieldSeason)) %>%
    dplyr::relocate(Year, .after = FieldSeason)

  humandata$Observed <- factor(humandata$Observed, levels = c("Yes"))
  
  humandata %<>% dplyr::arrange(FieldSeason)
  
  pal <- leaflet::colorFactor(palette = c("firebrick"),
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
  
  # width <- 700
  # height <- 700
  
  sd <- crosstalk::SharedData$new(humandata)
  year_filter <- crosstalk::filter_checkbox(id = "year-hu",
                                            label = "Water Year",
                                            sharedData = sd,
                                            group = ~Year,
                                            # width = width,
                                            inline = TRUE)
  
  humanmap <- leaflet::leaflet(sd
                            # , width = width, height = height
                            ) %>%
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
  
  if (missing(field.season)) {
    humanmap <- crosstalk::bscols(list(year_filter, humanmap))
  } else if (!missing(field.season) & length(field.season) == 1) {
    # do nothing
  } else {
    humanmap <- crosstalk::bscols(list(year_filter, humanmap))
  }
  
  return(humanmap)
}


#' Table with livestock observations
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     LivestockObservations()
#'     LivestockObservations(site = "MOJA_P_WIL0224", field.season = "2019")
#'     LivestockObservations(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
LivestockObservations <- function(park, site, field.season) {
  formatted <- qcDisturbanceFormatted(park = park, site = site, field.season = field.season)
  
  livestockobs <- formatted %>%
    dplyr::filter(Livestock > 0,
                  Livestock != "NoData") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, Livestock, Notes) %>%
    dplyr::mutate(Livestock = dplyr::case_when(Livestock == "1" ~ ">0 - 25%",
                                               Livestock == "2" ~ ">25 - 50%",
                                               Livestock == "3" ~ ">50 - 75%",
                                               Livestock == "4" ~ ">75 - 100%",
                                               Livestock == "0" ~ "0%",
                                               TRUE ~ Livestock))
  
  return(livestockobs)
}


#' Bar plot with percent of springs with livestock disturbance
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return ggplot bar plot
#' @export
#'
#' @examples
#' \dontrun{
#'     LivestockPlot()
#'     LivestockPlot(park = c("DEVA", "JOTR"))
#' }
LivestockPlot <- function(park, site, field.season) {
  count <- DisturbanceCount(park = park, site = site, field.season = field.season)
  
  count %<>%
    dplyr::filter(Park != "CAMO")
  
  livestockplot <- ggplot2::ggplot(count, ggplot2::aes(x = Park, y = LivestockPercent)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(limits = c(0, 100)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(vjust = 0.5, hjust = 0.5)) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(LivestockPercent, "%")),
                       vjust = -1,
                       size = 4,
                       show.legend = FALSE)
  
  return(livestockplot)
}


#' Map of livestock disturbance observations
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return leaflet map
#' @export
#'
#' @examples
#' \dontrun{
#'     LivestockMap()
#'     LivestockMap(site = "MOJA_P_WIL0224", field.season = "2019")
#'     LivestockMap(park = c("DEVA", "MOJA"), field.season = c("2017", "2018", "2021"))
#' }
LivestockMap <- function(interactive, park, site, field.season) {
  formatted <- qcDisturbanceFormatted(park = park, site = site, field.season = field.season)
  site <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Site")
  
  coords <- site %>%
    dplyr::select(SiteCode, Lat_WGS84, Lon_WGS84, X_UTM_NAD83_11N, Y_UTM_NAD83_11N)
  
  livestockdata <- formatted %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, Livestock, Notes) %>%
    dplyr::mutate(Observed = dplyr::case_when(Livestock == "0" ~ "No",
                                              Livestock %in% c("1", "2", "3", "4") ~ "Yes",
                                              TRUE ~ "NA")) %>%
    dplyr::filter(Observed == "Yes") %>%
    dplyr::left_join(coords, by = "SiteCode", multiple = "all", relationship = "many-to-one") %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr") & Panel %in% c("A", "B", "C", "D")) %>%
    dplyr::mutate(Year = as.numeric(FieldSeason)) %>%
    dplyr::relocate(Year, .after = FieldSeason)
  
  livestockdata$Observed <- factor(livestockdata$Observed, levels = c("Yes"))
  
  livestockdata %<>% dplyr::arrange(FieldSeason)
  
  pal <- leaflet::colorFactor(palette = c("firebrick"),
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
  
  # width <- 700
  # height <- 700
  
  sd <- crosstalk::SharedData$new(livestockdata)
  year_filter <- crosstalk::filter_checkbox(id = "year-ls",
                                            label = "Water Year",
                                            sharedData = sd,
                                            group = ~Year,
                                            # width = width,
                                            inline = TRUE)
  
  lsmap <- leaflet::leaflet(sd
                            #, width = width, height = height
                            ) %>%
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

  if (missing(field.season)) {
    lsmap <- crosstalk::bscols(list(year_filter, lsmap))
  } else if (!missing(field.season) & length(field.season) == 1) {
    # do nothing
  } else {
    lsmap <- crosstalk::bscols(list(year_filter, lsmap))
  }
  
  return(lsmap)
}