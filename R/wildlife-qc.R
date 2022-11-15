#' Wildlife observed, no wildlife type specified
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
#'     
#'     qcWildlifeObservedNoTypes()
#'     qcWildlifeObservedNoTypes(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcWildlifeObservedNoTypes(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
  qcWildlifeObservedNoTypes <- function(park, site, field.season) {
    wildlife <- ReadAndFilterData(park = park,data.name = "Wildlife")
  
  observed.notype <- wildlife %>%
    dplyr::filter(IsWildlifeObserved == "Yes",
                  WildlifeType %in% c("No Data", NA)) %>%
    dplyr::arrange(SiteCode, FieldSeason) %>%
    dplyr::select(-c(VisitType, DPL))
  
  return(observed.notype)
}


#' Wildlife observed and wildlife type specified, no evidence recorded
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
#'     
#'     qcWildlifeObservedNoEvidence()
#'     qcWildlifeObservedNoEvidence(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcWildlifeObservedNoEvidence(park = c("DEVA", "JOTR"), field.season = c("2017", "2020", "2021"))
#' }
  qcWildlifeObservedNoEvidence <- function(park, site, field.season) {
    wildlife <- ReadAndFilterData(park = park, data.name = "Wildlife")

  type.noevidence <- wildlife %>%
    dplyr::filter(IsWildlifeObserved == "Yes",
                  DirectObservation != "Yes",
                  Scat != "Yes",
                  Tracks != "Yes",
                  Shelter != "Yes",
                  Foraging != "Yes",
                  Vocalization != "Yes",
                  OtherEvidence != "Yes") %>%
    dplyr::arrange(SiteCode, FieldSeason) %>%
    dplyr::select(-c(VisitType, DPL))
  
  return(type.noevidence)
}

#' Table of springs with evidence of ungulate (sheep and deer) activity
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
#'     
#'     UngulatesEvidence()
#'     UngulatesEvidence(site = "LAKE_P_COR0023", field.season = "2020")
#'     UngulatesEvidence(park = c("DEVA", "JOTR"), field.season = c("2017", "2020", "2021"))
#' }
    UngulatesEvidence <- function(park, site, field.season) {
      wildlife <- ReadAndFilterData(park = park, data.name = "Wildlife")
  
  ungulates <- wildlife %>%
    dplyr::filter(WildlifeType == "Ungulate") %>%
    dplyr::arrange(SiteCode, FieldSeason) %>%
    dplyr::select(-c(VisitType, DPL))
  
  return(ungulates)
}


#' Map of springs with evidence of ungulate (sheep and deer) activity
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
#'     
#'     UngulatesMap()
#'     UngulatesMap(site = "LAKE_P_COR0023")
#'     UngulatesMap(park = c("DEVA", "MOJA"), field.season = c("2019", "2021"))
#' }
UngulatesMap <- function(park, site, field.season) {
  wildlife <- ReadAndFilterData(park = park, data.name = "Wildlife")
  site <- ReadAndFilterData(park = park, data.name = "Site")
  
  coords <- site %>%
    dplyr::select(SiteCode, SampleFrame, Lat_WGS84, Lon_WGS84, X_UTM_NAD83_11N, Y_UTM_NAD83_11N)
  
  ungulatedata <- wildlife %>%
    dplyr::filter(WildlifeType == "Ungulate") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, WildlifeType, DirectObservation, Scat, Tracks, Shelter, Foraging, Vocalization, OtherEvidence, Notes) %>%
    dplyr::inner_join(coords, by = "SiteCode") %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr")) %>%
    dplyr::mutate(Observed = dplyr::case_when(WildlifeType == "Ungulate" ~ "Yes",
                                              is.na(WildlifeType) ~ "No",
                                              TRUE ~ "No")) %>%
    dplyr::filter(Observed == "Yes")  %>%
    dplyr::mutate(Year = as.numeric(FieldSeason)) %>%
    dplyr::relocate(Year, .after = FieldSeason)
  
  ungulatedata$Observed <- factor(ungulatedata$Observed, levels = c("Yes"))
  
  ungulatedata %<>% dplyr::arrange(FieldSeason)
  
  pal <- leaflet::colorFactor(palette = c("red"),
                     domain = ungulatedata$Observed)
  
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
  
  sd <- crosstalk::SharedData$new(ungulatedata)
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
  
  ungmap <- leaflet::leaflet(sd, width = width, height = height) %>%
    leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
    leaflet::addScaleBar('bottomright') %>%
    leaflet::addCircleMarkers(lng = ~Lon_WGS84,
                              lat = ~Lat_WGS84,
                              popup = paste ("Name: ", ungulatedata$SiteName, "<br>",
                                             "Sample Frame: ", ungulatedata$SampleFrame, "<br>",
                                             "Field Season: ", ungulatedata$FieldSeason, "<br>",
                                             "Direct Observation: ", ungulatedata$DirectObservation, "<br>",
                                             "Scat: ", ungulatedata$Scat, "<br>",
                                             "Tracks: ", ungulatedata$Tracks, "<br>",
                                             "Shelter: ", ungulatedata$Shelter, "<br>",
                                             "Foraging: ", ungulatedata$Foraging, "<br>",
                                             "Vocalization: ", ungulatedata$Vocalization, "<br>",
                                             "Other Evidence: ", ungulatedata$OtherEvidence, "<br>",
                                             "Notes: ", ungulatedata$Notes),
                              radius = 5,
                              stroke = TRUE,
                              weight = 1,
                              color = "black",
                              fillOpacity = 1,
                              fillColor = ~pal(Observed),
                              group = ~Observed) %>%
    leaflet::addLegend(pal = pal,
                       values = ~Observed,
                       title = "Ungulates Evidence",
                       opacity = 1,
                       position = "bottomleft") %>%
    leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                              options=leaflet::layersControlOptions(collapsed = FALSE))
  
  ungulatemap <- crosstalk::bscols(list(year_filter,
                                   ungmap))
  
  return(ungulatemap)
}