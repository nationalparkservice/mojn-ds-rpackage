#' Wildlife observed, no wildlife type specified
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns for 
#' @export
#'
qcWildlifeObservedNoTypes <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  wildlife <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Wildlife")
  
  observed.notype <- wildlife %>%
    dplyr::filter(IsWildlifeObserved == "Yes",
                  WildlifeType %in% c("No Data", NA)) %>%
    dplyr::arrange(SiteCode, FieldSeason) %>%
    dplyr::select(-c(VisitType, DPL))
  
  return(observed.notype)
}


#' Wildlife observed and wildlife type specified, no evidence recorded
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return
#' @export
#'
qcWildlifeObservedNoEvidence <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  wildlife <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Wildlife")
  
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
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return
#' @export
#'
UngulatesEvidence <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  wildlife <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Wildlife")
  
  ungulates <- wildlife %>%
    dplyr::filter(WildlifeType == "Ungulate") %>%
    dplyr::arrange(SiteCode, FieldSeason) %>%
    dplyr::select(-c(VisitType, DPL))
  
  return(ungulates)
}


#' Map of springs with evidence of ungulate (sheep and deer) activity
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
UngulatesMap <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  wildlife <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Wildlife")
  site <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Site")
  
  coords <- site %>%
    select(SiteCode, SampleFrame, Lat_WGS84, Lon_WGS84, X_UTM_NAD83_11N, Y_UTM_NAD83_11N)
  
  ungulatedata <- wildlife %>%
    dplyr::filter(WildlifeType == "Ungulate") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, WildlifeType, DirectObservation, Scat, Tracks, Shelter, Foraging, Vocalization, OtherEvidence, Notes) %>%
    dplyr::inner_join(coords, by = "SiteCode") %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr")) %>%
    dplyr::mutate(Observed = case_when(WildlifeType == "Ungulate" ~ "Yes",
                                       is.na(WildlifeType) ~ "No",
                                       TRUE ~ "No")) %>%
    dplyr::filter(Observed == "Yes")  %>%
    dplyr::mutate(Year = as.numeric(FieldSeason)) %>%
    dplyr::relocate(Year, .after = FieldSeason)
  
  ungulatedata$Observed <- factor(ungulatedata$Observed, levels = c("Yes"))
  
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
  
  width <- 800
  height <- 800
  
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
                                             "Direct Observation: ", ungulatedata$DirectObservation, "<br>",
                                             "Scat: ", ungulatedata$Scat, "<br>",
                                             "Tracks: ", ungulatedata$Tracks, "<br>",
                                             "Shelter: ", ungulatedata$Shelter, "<br>",
                                             "Foraging: ", ungulatedata$Foraging, "<br>",
                                             "Vocalization: ", ungulatedata$Vocalization, "<br>",
                                             "Other Evidence: ", ungulatedata$OtherEvidence, "<br>",
                                             "Notes: ", ungulatedata$Notes),
                              radius = 6,
                              stroke = FALSE,
                              fillOpacity = 1,
                              color = ~pal(Observed),
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