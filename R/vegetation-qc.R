#' Return list of visits with vegetation observed, no lifeform present
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
#'     qcVegPresentNoLifeforms()
#'     qcVegPresentNoLifeforms(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcVegPresentNoLifeforms(park = c("MOJA", "PARA"), field.season = c("2017", "2019", "2020"))
#' }
qcVegPresentNoLifeforms <- function(park, site, field.season) {
  veg <- ReadAndFilterData(park = park, site = site, field.season = field.season,  data.name = "Vegetation")

  vegnolife <- veg %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::filter(IsVegetationObserved == "Y" & is.na(LifeForm)) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, IsVegetationObserved, LifeForm) %>%
    dplyr::arrange(FieldSeason, Park, SiteCode)
  
  return(vegnolife)
}


#' Return list of visits with no vegetation observed, lifeform present
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
#'     qcNoVegLifeformsPresent()
#'     qcNoVegLifeformsPresent(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcNoVegLifeformsPresent(park = c("MOJA", "PARA"), field.season = c("2017", "2019", "2020"))
#' }
qcNoVegLifeformsPresent <- function(park, site, field.season) {
  veg <- ReadAndFilterData(park = park, site = site, field.season = field.season,  data.name = "Vegetation")
  
  noveglife <- veg %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::filter(IsVegetationObserved == "N" & !is.na(LifeForm)) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, IsVegetationObserved, LifeForm) %>%
    dplyr::arrange(FieldSeason, Park, SiteCode)
  
  return(noveglife)
}


#' Return list of visits with lifeform present, no rank
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
#'     qcLifeformPresentNoRank()
#'     qcLifeformPresentNoRank(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcLifeformPresentNoRank(park = c("MOJA", "PARA"), field.season = c("2017", "2019", "2020"))
#' }
qcLifeformPresentNoRank <- function(park, site, field.season) {
  veg <- ReadAndFilterData(park = park, site = site, field.season = field.season,  data.name = "Vegetation")
  
  lifenorank <- veg %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::filter(!is.na(LifeForm) & is.na(Rank)) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, LifeForm, Rank) %>%
    dplyr::arrange(FieldSeason, Park, SiteCode)
  
  return(lifenorank)
}


#' Return list of visits where multiple lifeforms have the same rank, and rank gaps have not been properly entered
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
#'     qcLifeformRankCheck()
#'     qcLifeformRankCheck(site = "JOTR_P_NOR0083", field.season = "2021")
#'     qcLifeformRankCheck(park = c("JOTR", "MOJA"), field.season = c("2016", "2018", "2021"))
#' }
qcLifeformRankCheck <- function(park, site, field.season) {
  veg <- ReadAndFilterData(park = park, site = site, field.season = field.season,  data.name = "Vegetation")
  
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


#' Return list of lifeform types that were duplicated during data entry
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
#'     qcVegDuplicates()
#'     qcVegDuplicates(park = c("JOTR", "MOJA"), field.season = c("2016", "2018", "2021"))
#' }
qcVegDuplicates <- function(park, site, field.season) {
  veg <- ReadAndFilterData(park = park, site = site, field.season = field.season,  data.name = "Vegetation")
  
  veg.dupes <- veg %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, LifeForm) %>%
    dplyr::group_by(Park, SiteCode, SiteName, VisitDate, FieldSeason, LifeForm) %>%
    dplyr::summarize(Count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Count > 1)
  
  return(veg.dupes)
}


#' Return list of invasive plants that were duplicated during data entry
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
#'     qcVegDuplicates()
#'     qcVegDuplicates(park = c("JOTR", "MOJA"), field.season = c("2016", "2018", "2021"))
#' }
qcInvasiveDuplicates <- function(park, site, field.season) {
  inv <- ReadAndFilterData(park = park, site = site, field.season = field.season,  data.name = "InvasivePlants")
  
  inv.dupes <- inv %>%
    dplyr::filter(VisitType == "Primary",
                  USDAPlantsCode != "UNK") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, USDAPlantsCode) %>%
    dplyr::group_by(Park, SiteCode, SiteName, VisitDate, FieldSeason, USDAPlantsCode) %>%
    dplyr::summarize(Count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Count > 1)
  
  return(inv.dupes)
}


#' Count of total lifeform categories observed at each spring visit
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     LifeformsPerSpringPlot()
#'     LifeformsPerSpringPlot(park = c("MOJA", "PARA"), field.season = c("2017", "2019", "2020"))
#' }
LifeformCounts <- function(park, field.season) {
  veg <- ReadAndFilterData(park = park, field.season = field.season,  data.name = "Vegetation")
  site <- ReadAndFilterData(park = park, field.season = field.season, data.name = "Site")
  
  site <- site |>
    dplyr::select(SiteCode, SampleFrame)
  
  veg.sums <- veg |>
    dplyr::inner_join(site, by = c("SiteCode", "SampleFrame"), multiple = "all") |>
    dplyr::filter(VisitType == "Primary",
                  Panel %in% c("Panel Annual", "Panel B", "Panel C", "Panel D")) |>
    dplyr::count(Park, SiteCode, SiteName, VisitDate, FieldSeason) |>
    dplyr::rename(LifeFormCount = n) |>
    dplyr::ungroup() |>
    dplyr::arrange(SiteCode, FieldSeason)
  
  return(veg.sums)  
}

#' Boxplot of the median and distribution of the count of total lifeform categories observed at each park during each field season
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return ggplot object
#' @export
#'
#' @examples
PlotLifeformCounts <- function(park,field.season) {
  veg.sums.plot <- LifeformCounts(park = park, field.season = field.season) |>
    GetSampleSizes(Park, FieldSeason)|>
    dplyr::filter(Park != "CAMO") |>
    dplyr::filter(dplyr::case_when(Park %in% c("LAKE", "MOJA") ~ FieldSeason %in% c("2016", "2019", "2022", "2026"),
                                   Park %in% c("JOTR") ~ FieldSeason %in% c("2017", "2020", "2023"),
                                   Park %in% c("PARA") ~ FieldSeason %in% c("2017", "2023"),
                                   Park %in% c("DEVA") ~ FieldSeason %in% c("2018", "2025"),
                                   TRUE ~ FieldSeason %in% c("2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025", "2026")))
    
  
  veg.boxplot <- FormatPlot(data = veg.sums.plot,
                            x.col = FieldSeason,
                            y.col = LifeFormCount,
                            facet.col = Park,
                            sample.size.col = SampleSizeLabel,
                            sample.size.loc = "xaxis") +
    ggplot2::geom_boxplot() +
    ggplot2::geom_dotplot(binaxis = "y", stackdir = "center", alpha = 0.08) +
    ggplot2::stat_summary(fun = mean, geom = "point", shape = 4, size = 3, color = "red") +
    ggplot2::scale_y_continuous(limits = c(0,NA), breaks = seq(0,11,2)) +
    ggplot2::xlab("Field Season") +
    ggplot2::ylab("Count of life form categories")
  
  return(veg.boxplot)
}


#' Number of observations of vegetation life form categories at each annual and three-year spring across all field seasons
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @returns
#' @export
#'
#' @examples
LifeformsBySite <- function(park, site, field.season) {
  veg <- ReadAndFilterData(park = park, site = site, field.season = field.season,  data.name = "Vegetation")
  site <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Site")
  
  veg <- veg |>
    dplyr::filter(Panel %in% c("Panel Annual", "Panel B", "Panel C", "Panel D"))
  site <- site |>
    dplyr::select(SiteCode, SampleFrame)
  
  lifeforms_bysite <- veg |>
    dplyr::filter(VisitType == "Primary",
                  !is.na(LifeForm)) |>
    dplyr::count(Park, SiteCode, SiteName, SampleFrame, Panel, LifeForm) |>
    dplyr::rename(Observations = n,
                  LifeFormCategory = LifeForm)

  return(lifeforms_bysite)  
}


#' Number of observations of vegetation life form categories for annual and three-year springs at each park during each field season
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @returns
#' @export
#'
#' @examples
LifeformsByYear <- function(park, field.season) {
  veg <- ReadAndFilterData(park = park, field.season = field.season,  data.name = "Vegetation")
  site <- ReadAndFilterData(park = park, field.season = field.season, data.name = "Site")
  
  veg <- veg |>
    dplyr::filter(Panel %in% c("Panel Annual", "Panel B", "Panel C", "Panel D"))
  site <- site |>
    dplyr::select(SiteCode, SampleFrame)
 
  lifeforms_byyear <- veg |>
    dplyr::filter(VisitType == "Primary",
                  !is.na(LifeForm)) |>
    dplyr::count(Park, FieldSeason, LifeForm) |>
    dplyr::rename(Observations = n,
                  LifeFormCategory = LifeForm) |>
    dplyr::arrange(Park, FieldSeason, LifeFormCategory)

  return(lifeforms_byyear)
}

#' Bar plot of the number of annual and three-year springs in each park where a life form category has been observed across all field seasons
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @returns ggplot object
#' @export
#'
#' @examples
PlotLifeformsByPark <- function(park, field.season) {
  
  lifeforms <- LifeformsBySite(park = park, field.season = field.season) |>
    dplyr::group_by(Park, LifeFormCategory) |>
    dplyr::summarize(Observations = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(Park != "CAMO")
  
  lifeforms_barplot <- ggplot2::ggplot(lifeforms,
                                       ggplot2::aes(x = tidytext::reorder_within(LifeFormCategory, Observations, Park),
                                                    y = Observations,
                                                    text = paste("Lifeform Category: ", LifeFormCategory,
                                                                 "<br>Observations:", Observations))) +
    tidytext::scale_x_reordered() +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::coord_flip() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(vjust = 0.5, hjust = 0.5, color = "black"), #
                   axis.text.y = ggplot2::element_text(, color = "black"), #
                   axis.title.x = ggplot2::element_text(, color = "black"), #
                   axis.title.y = ggplot2::element_text(, color = "black")) +
    ggplot2::ylab("Number of springs where life form has been observed") +
    ggplot2::xlab("Vegetation Life Form Category") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .1))) +
    ggplot2::facet_grid(Park~.,
                        scales = "free_y") +
    ggplot2::geom_text(ggplot2::aes(label = paste0(Observations)),
                       position = ggplot2::position_dodge(width = 1),
                       vjust = 0.4,
                       hjust = -0.2,
                       size = 3,
                       show.legend = FALSE)
  
  return(lifeforms_barplot)
}

#' Line plot of the percentage of annual and three-year springs where a life form category has been observed at each park during each field season
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @returns ggplot object
#' @export
#'
#' @examples
PlotLifeformsByYear <- function(park, field.season) {
  
  lifeforms <- LifeformsByYear(park = park, field.season = field.season) |>
    dplyr::filter(Park != "CAMO") |>
    dplyr::filter(dplyr::case_when(Park %in% c("LAKE", "MOJA") ~ FieldSeason %in% c("2016", "2019", "2022", "2026"),
                                   Park %in% c("JOTR") ~ FieldSeason %in% c("2017", "2020", "2023"),
                                   Park %in% c("PARA") ~ FieldSeason %in% c("2017", "2023"),
                                   Park %in% c("DEVA") ~ FieldSeason %in% c("2018", "2025"),
                                   TRUE ~ FieldSeason %in% c("2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025", "2026"))) |>
    dplyr::mutate(Percentage = dplyr::case_when(Park == "DEVA" ~ Observations/80*100,
                                                Park %in% c("JOTR") ~ Observations/35*100,
                                                Park %in% c("MOJA", "PARA") ~ Observations/45*100,
                                                Park == "LAKE" ~ Observations/43*100))
  
  fieldseason <- unique(as.integer(lifeforms$FieldSeason)) |> sort()
  
  lifeforms_lineplot <- ggplot2::ggplot(lifeforms,
                                        ggplot2::aes(x = as.integer(FieldSeason),
                                                     y = Percentage,
                                                     color = LifeFormCategory,
                                                     group = LifeFormCategory,
                                                     text = paste("Lifeform Category: ", LifeFormCategory,
                                                                  "<br>Percentage:", Percentage,
                                                                  "<br>Observations:", Observations))) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_x_continuous(breaks = seq(min(fieldseason), max(fieldseason), by = 1)) + 
    ggplot2::facet_grid(Park~.) +
    khroma::scale_color_discreterainbow() +
    ggplot2::xlab("Field Season") +
    ggplot2::ylab("Percentage of springs where a life form has been observed")
  
  return(lifeforms_lineplot)
}

#' Table of tamarisk, fountain grass, rabbitsfoot grass, date palm, and fan palm (non-JOTR) observations
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
#'     InvasivePlants()
#'     InvasivePlants(site = "LAKE_P_HOR0042", field.season = "2020")
#'     InvasivePlants(park = c("MOJA", "PARA"), field.season = c("2017", "2019", "2020"))
#' }
InvasivePlants <- function(park, site, field.season) {
  invasives <- ReadAndFilterData(park = park, site = site, field.season = field.season,  data.name = "InvasivePlants")
  
  targetinvasives <- invasives %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, USDAPlantsCode, ScientificName, InRiparianVegBuffer, Notes) %>%
    dplyr::filter(USDAPlantsCode %in% c("PESE3", "PHDA4", "POMO5", "TARA", "WAFI")) %>%
    dplyr::arrange(SiteCode, FieldSeason)
  
  return(targetinvasives)
}


#' Map of tamarisk, fountain grass, rabbitsfoot grass, date palm, and fan palm (non-JOTR) observations
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
#'     InvasivePlantsMap()
#'     InvasivePlantsMap(site = "LAKE_P_GET0066", field.season = "2019")
#'     InvasivePlantsMap(park = c("MOJA", "PARA"), field.season = c("2017", "2019", "2020"))
#' }
InvasivePlantsMap <- function(park, site, field.season) {
  invasives <- ReadAndFilterData(park = park, site = site, field.season = field.season,  data.name = "InvasivePlants")
  site <- ReadAndFilterData(park = park, site = site, field.season = field.season,  data.name = "Site")
  
  coords <- site %>%
    dplyr::select(SiteCode, Lat_WGS84, Lon_WGS84, X_UTM_NAD83_11N, Y_UTM_NAD83_11N)
  
  invasivesdata <- invasives %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, InvasivesObserved, InRiparianVegBuffer, USDAPlantsCode, ScientificName, Notes) %>%
    dplyr::inner_join(coords, by = c("SiteCode"), multiple = "all", relationship = "many-to-one") %>%
    # dplyr::filter(SampleFrame %in% c("Annual", "3Yr")) %>%
    dplyr::mutate(PlantInfo = dplyr::case_when(InvasivesObserved == "Y" & USDAPlantsCode %in% c("TARA", "PHDA4", "WAFI", "PESE3", "POMO5") ~ ScientificName,
                                               InvasivesObserved == "Y" & !(USDAPlantsCode %in% c("TARA", "PHDA4", "WAFI", "PESE3", "POMO5")) & !(is.na(USDAPlantsCode)) ~ "Other",
                                               InvasivesObserved == "N" ~ "None",
                                               TRUE ~ "None")) %>%
    dplyr::filter(PlantInfo != "None") %>%
    dplyr::mutate(Year = as.numeric(FieldSeason)) %>%
    dplyr::relocate(Year, .after = FieldSeason) %>%
    dplyr::filter(!(Park == "JOTR" & USDAPlantsCode == "WAFI"))
   
  invasivesdata$PlantInfo <- factor(invasivesdata$PlantInfo, levels = c("Pennisetum setaceum", "Phoenix dactylifera", "Polypogon monspeliensis", "Tamarix ramosissima", "Washingtonia filifera", "Other"))
  
  invasivesdata %<>% dplyr::arrange(FieldSeason, desc(PlantInfo))
  
  pal <- leaflet::colorFactor(palette = c("#5DC863FF", "#440154FF", "#21908CFF", "#FDE725FF", "#3B528BFF", "gray"),
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
  
  # width <- 800
  # height <- 400
  
  sd <- crosstalk::SharedData$new(invasivesdata)
  year_filter <- crosstalk::filter_checkbox(id = "year-inv",
                                            label = "Water Year",
                                            sharedData = sd,
                                            group = ~Year,
                                            # width = width,
                                            inline = TRUE)
  
  invmap <- leaflet::leaflet(sd
                             #, height = height, width = width
                             ) %>%
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
                              overlayGroups = c("Pennisetum setaceum", "Phoenix dactylifera", "Polypogon monspeliensis", "Tamarix ramosissima", "Washingtonia filifera", "Other"),
                              options=leaflet::layersControlOptions(collapsed = FALSE))
 
  if (missing(field.season)) {
    invmap <- crosstalk::bscols(list(year_filter, invmap))
  } else if (!missing(field.season) & length(field.season) == 1) {
    # do nothing
  } else {
    invmap <- crosstalk::bscols(list(year_filter, invmap))
  }
  
  return(invmap)
}