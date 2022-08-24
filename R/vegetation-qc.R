#' Return list of visits with vegetation observed, no lifeform present
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
qcVegPresentNoLifeforms <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  veg <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Riparian")
  
  vegnolife <- veg %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::filter(IsVegetationObserved == "Y" & is.na(LifeForm)) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, IsVegetationObserved, LifeForm) %>%
    dplyr::arrange(FieldSeason, Park, SiteCode)
  
  return(vegnolife)
}


#' Return list of visits with no vegetation observed, lifeform present
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
qcNoVegLifeformsPresent <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  veg <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Riparian")
  
  noveglife <- veg %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::filter(IsVegetationObserved == "N" & !is.na(LifeForm)) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, IsVegetationObserved, LifeForm) %>%
    dplyr::arrange(FieldSeason, Park, SiteCode)
  
  return(noveglife)
}


#' Return list of visits with lifeform present, no rank
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
qcLifeformPresentNoRank <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  veg <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Riparian")
  
  lifenorank <- veg %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::filter(!is.na(LifeForm) & is.na(Rank)) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, LifeForm, Rank) %>%
    dplyr::arrange(FieldSeason, Park, SiteCode)
  
  return(lifenorank)
}


#' Return list of visits where multiple lifeforms have the same rank, and rank gaps have not been properly entered
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
qcLifeformRankCheck <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  veg <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Riparian")
  
  rankcheck <- veg %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, LifeForm, Rank) %>%
    dplyr::group_by(Park, SiteCode, SiteName, VisitDate, FieldSeason, Rank) %>%
    dplyr::mutate(Count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Park, SiteCode, SiteName, VisitDate, FieldSeason, Rank, Count) %>%
    dplyr::mutate(LifeForms = paste0(LifeForm, collapse = ", ")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-LifeForm) %>%
    unique() %>%
    dplyr::group_by(Park, SiteCode, SiteName, VisitDate, FieldSeason) %>%
    dplyr::arrange(Rank, .by_group = T) %>%
    dplyr::mutate(Diff = c(diff(Rank), NA)) %>%
    dplyr::relocate(Diff, .after = Count) %>%
    dplyr::filter(any(Count != Diff)) %>%
    dplyr::ungroup()
  
  return(rankcheck)
}


#' Table with summary of life form presence and rank
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#' @return
#' @export
#'
LifeformsPresence <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  veg <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Riparian")
  
  veg.summary <- veg %>%
    dplyr::filter(VisitType == "Primary",
                  !is.na(LifeForm)) %>%
    dplyr::select(Park, SiteCode, FieldSeason, LifeForm, Rank) %>%
    dplyr::count(Park, FieldSeason, LifeForm) %>%
    dplyr::rename(Observations = n)
  
  return(veg.summary)
  
}


#' Bar plot showing the distribution of the number of life form categories at springs by park
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return
#' @export
#'
LifeformsPerSpringPlot <- function(conn, path.to.data, park, field.season, data.source = "database") {
  veg <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, field.season = field.season, data.source = data.source, data.name = "Riparian")
  
  veg %<>% dplyr::filter(Park != "CAMO")
  
  veg.sums <- veg %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::count(Park, SiteCode, SiteName, VisitDate, FieldSeason) %>%
    dplyr::rename(LifeFormCount = n) %>%
    dplyr::ungroup()
  
  veg.sums.all <- veg %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::count(Park, SiteCode, SiteName, VisitDate, FieldSeason) %>%
    dplyr::rename(LifeFormCount = n) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Park, LifeFormCount) %>%
    dplyr::summarize(Occurences = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Park, LifeFormCount) %>%
    dplyr::mutate(LifeFormCount = as.factor(LifeFormCount))
  
  veg.stats.all <- veg.sums %>%
    dplyr::group_by(Park) %>%
    dplyr::summarize(Mean = round(mean(LifeFormCount), 2),
                     Median = round(median(LifeFormCount), 2))
  
  veg.sums.latest <- veg %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::count(Park, SiteCode, SiteName, VisitDate, FieldSeason) %>%
    dplyr::rename(LifeFormCount = n) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Park, SiteCode, SiteName) %>%
    dplyr::filter(VisitDate == max(VisitDate)) %>%
    dplyr::ungroup() %>%
    unique() %>%
    dplyr::group_by(Park, LifeFormCount) %>%
    dplyr::summarize(Occurences = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Park, LifeFormCount) %>%
    dplyr::mutate(LifeFormCount = as.factor(LifeFormCount))
  
  veg.sums.lake <- veg %>%
    dplyr::filter(VisitType == "Primary",
                  Park == "LAKE") %>%
    dplyr::count(Park, SiteCode, SiteName, VisitDate, FieldSeason) %>%
    dplyr::rename(LifeFormCount = n) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Park, FieldSeason, LifeFormCount) %>%
    dplyr::summarize(Occurences = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Park, LifeFormCount) %>%
    dplyr::mutate(LifeFormCount = as.factor(LifeFormCount))
    
  veg.barplot.all <- ggplot2::ggplot(veg.sums.all,
                                 aes(x = LifeFormCount, y = Occurences,
                                     text = paste("Lifeform Count: ", LifeFormCount,
                                                  "<br>Occurences:", Occurences))) +
    geom_bar(stat = "identity") +
    facet_grid(Park ~ .) +
    xlab("Number of Different Vegetation Life Form Categories") +
    ylab("Number of Occurences (All Field Seasons)") +
    scale_x_discrete(breaks = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")) +
    geom_vline(data = veg.stats.all, aes(xintercept = Mean, color = "Mean"), linetype = "longdash", size = 1) +
    geom_vline(data = veg.stats.all, aes(xintercept = Median, color = "Median"), size = 1) +
    scale_color_manual(name = "Stats", values = c(Median = "black", Mean = "red"))
  
  veg.barplot.latest <- ggplot2::ggplot(veg.sums.latest,
                                     aes(x = LifeFormCount, y = Occurences)) +
    geom_bar(stat = "identity") +
    facet_grid(Park ~ .) +
    xlab("Number of Different Vegetation Life Form Categories") +
    ylab("Number of Occurences (Latest Field Season with Data") +
    scale_x_discrete(breaks = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
  
  return(veg.barplot.all)
}


#' Bar plot showing the most common life form categories at springs by park
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return
#' @export
#'
MostCommonLifeformsPlot <- function(conn, path.to.data, park, field.season, data.source = "database") {
  veg <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, field.season = field.season, data.source = data.source, data.name = "Riparian")

  veg %<>% dplyr::filter(Park != "CAMO")
  
  veg.types <- veg %>%
    dplyr::filter(VisitType == "Primary",
                  !is.na(LifeForm)) %>%
    dplyr::count(Park, LifeForm) %>%
    dplyr::rename(Observations = n,
                  LifeFormCategory = LifeForm)
  
  veg.types.barplot <- ggplot2::ggplot(veg.types,
                                       aes(tidytext::reorder_within(LifeFormCategory, Observations, Park), Observations,
                                           text = paste("Lifeform Category: ", LifeFormCategory,
                                                        "<br>Observations:", Observations))) +
    tidytext::scale_x_reordered() +
    geom_col() +
    facet_grid(Park ~ ., scales = "free", space = "free") +
    coord_flip() +
    theme(panel.grid.major.y = element_blank()) +
    ylab("Number of Observations at Springs") +
    xlab("Vegetation Life Form Category") +
    scale_y_continuous(expand = expansion(mult = c(0, .1)))
  
  
  veg.types.year<- veg %>%
    dplyr::filter(VisitType == "Primary",
                  !is.na(LifeForm),
                  Park == "LAKE",
                  FieldSeason %in% c("2016", "2019", "2022")) %>%
    dplyr::count(Park, FieldSeason, LifeForm) %>%
    dplyr::rename(Observations = n,
                  LifeFormCategory = LifeForm)
  
  veg.types.year.barplot <- ggplot2::ggplot(veg.types.year,
                                       aes(x = tidytext::reorder_within(LifeFormCategory, Observations, Park),
                                           y = Observations,
                                           fill = FieldSeason,
                                           text = paste("Lifeform Category: ", LifeFormCategory,
                                                        "<br>Observations:", Observations))) +
    tidytext::scale_x_reordered() +
    geom_bar(stat = "identity", position = position_dodge()) +
    coord_flip() +
    theme(panel.grid.major.y = element_blank(),
          axis.text.x = ggplot2::element_text(vjust = 0.5, hjust = 0.5, size = 20), #
          axis.text.y = ggplot2::element_text(size = 20), #
          axis.title.x = ggplot2::element_text(size = 24), #
          axis.title.y = ggplot2::element_text(size = 24),
          legend.text = ggplot2::element_text (size = 20),
          legend.title = element_text(size = 20),
          legend.position = "bottom") +
    ylab("Number of Observations at Springs") +
    xlab("Vegetation Life Form Category") +
    scale_y_continuous(expand = expansion(mult = c(0, .1)))
  
  veg.types.year.barplot
  
  veg.types.med <- veg %>%
    dplyr::filter(VisitType == "Primary",
                  !is.na(LifeForm),
                  Park == "LAKE",
                  FieldSeason %in% c("2016", "2019", "2022")) %>%
    dplyr::count(Park, FieldSeason, LifeForm) %>%
    dplyr::rename(Observations = n,
                  LifeFormCategory = LifeForm) %>%
    dplyr::group_by(Park, LifeFormCategory) %>%
    dplyr::summarize(Observations = median(Observations)) %>%
    dplyr::ungroup()
  
  veg.types.med.barplot <- ggplot2::ggplot(veg.types.med,
                                            aes(x = tidytext::reorder_within(LifeFormCategory, Observations, Park),
                                                y = Observations,
                                                text = paste("Lifeform Category: ", LifeFormCategory,
                                                             "<br>Observations:", Observations))) +
    tidytext::scale_x_reordered() +
    geom_bar(stat = "identity", position = position_dodge()) +
    coord_flip() +
    theme(panel.grid.major.y = element_blank(),
          axis.text.x = ggplot2::element_text(vjust = 0.5, hjust = 0.5, size = 20), #
          axis.text.y = ggplot2::element_text(size = 20), #
          axis.title.x = ggplot2::element_text(size = 24), #
          axis.title.y = ggplot2::element_text(size = 24)) +
    ylab("Number of Observations at Springs") +
    xlab("Vegetation Life Form Category") +
    scale_y_continuous(expand = expansion(mult = c(0, .1)))
  
  veg.types.med.barplot
  
  
  
  return(veg.types.barplot)  
}


#' Table of tamarisk, fountain grass, rabbitsfoot grass, date palm, and fan palm (non-JOTR) observations
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
InvasivePlants <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  invasives <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Invasives")
  
  targetinvasives <- invasives %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, USDAPlantsCode, ScientificName, InRiparianVegBuffer, Notes) %>%
    dplyr::filter(USDAPlantsCode %in% c("PESE3", "PHDA4", "POMO5", "TARA", "WAFI")) %>%
    dplyr::arrange(SiteCode, FieldSeason)
  
  return(targetinvasives)
}


#' Map of tamarisk, fountain grass, rabbitsfoot grass, date palm, and fan palm (non-JOTR) observations
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
InvasivePlantsMap <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  invasives <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Invasives")
  site <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Site")
  
  coords <- site %>%
    dplyr::select(SiteCode, SampleFrame, Lat_WGS84, Lon_WGS84, X_UTM_NAD83_11N, Y_UTM_NAD83_11N)
  
  invasivesdata <- invasives %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, InvasivesObserved, InRiparianVegBuffer, USDAPlantsCode, ScientificName, Notes) %>%
    dplyr::inner_join(coords, by = "SiteCode") %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr")) %>%
    dplyr::mutate(PlantInfo = dplyr::case_when(InvasivesObserved == "Y" & USDAPlantsCode %in% c("TARA", "PHDA4", "WAFI", "PESE3", "POMO5") ~ ScientificName,
                                               InvasivesObserved == "Y" & !(USDAPlantsCode %in% c("TARA", "PHDA4", "WAFI", "PESE3", "POMO5")) & !(is.na(USDAPlantsCode)) ~ "Other",
                                               InvasivesObserved == "N" ~ "None",
                                               TRUE ~ "None")) %>%
    dplyr::filter(PlantInfo != "None") %>%
    dplyr::mutate(Year = as.numeric(FieldSeason)) %>%
    dplyr::relocate(Year, .after = FieldSeason)
  
  invasivesdata$PlantInfo <- factor(invasivesdata$PlantInfo, levels = c("Tamarix ramosissima", "Phoenix dactylifera", "Washingtonia filifera", "Pennisetum setaceum", "Polypogon monspeliensis", "Other"))
  
  pal <- leaflet::colorFactor(palette = c("chartreuse4", "gold", "cornflowerblue", "salmon", "darkorchid", "gray"),
                              domain = invasivesdata$PlantInfo)
  
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
  
  sd <- crosstalk::SharedData$new(invasivesdata)
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
  
  invmap <- leaflet::leaflet(sd, height = height, width = width) %>%
    leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
    leaflet::addScaleBar('bottomright') %>%
    leaflet::addCircleMarkers(lng = ~Lon_WGS84,
                              lat = ~Lat_WGS84,
                              popup = paste ("Name: ", invasivesdata$SiteName, "<br>",
                                             "Sample Frame: ", invasivesdata$SampleFrame, "<br>",
                                             "Scientific Name: ", invasivesdata$ScientificName, "<br>",
                                             "USDA Plants Code: ", invasivesdata$USDAPlantsCode, "<br>",
                                             "In Buffer: ", invasivesdata$InRiparianVegBuffer, "<br>",
                                             "Notes: ", invasivesdata$Notes),
                              radius = 5,
                              stroke = TRUE,
                              weight = 1,
                              color = "black",
                              fillOpacity = 1,
                              fillColor = ~pal(PlantInfo),
                              group = ~PlantInfo) %>%
    leaflet::addLegend(pal = pal,
                       values = ~PlantInfo,
                       title = "Invasive Plants",
                       opacity = 1,
                       position = "bottomleft") %>%
    leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                              overlayGroups = ~PlantInfo,
                              options=leaflet::layersControlOptions(collapsed = FALSE))
  
  invasivesmap <- crosstalk::bscols(list(year_filter,
                                    invmap))
  
  return(invasivesmap)
}


#################### Functions for Desert Springs PowerPoint -- not for final data package

