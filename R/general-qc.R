#' Calculate completeness (\% of planned sites visited)
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble with columns for park, field season, sample frame (i.e., annual, 3Yr), monitoring status (i.e., sampled), count of springs monitored, and percent of springs monitored.
#' @export
#' 
#'
#' @examples
#' \dontrun{
#'     qcCompleteness()
#'     qcCompleteness(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcCompleteness(park = "DEVA", field.season = c("2018", "2020", "2021"))
#' }
  qcCompleteness <- function(park, site, field.season) {
    
    completeness <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Visit")
    site <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Site")

  
  df1 <- site %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr"),
                  SiteStatus == "T-S") %>%
    dplyr::select(Park,
                  SiteCode,
                  SiteName,
                  SampleFrame)
  
  df2 <- completeness %>%
    dplyr::filter(SampleFrame %in% c("Annual","3Yr"),
                  VisitType == "Primary",
                  MonitoringStatus == "Sampled") %>%
    dplyr::select(Park, SiteCode, SiteName, FieldSeason, SampleFrame, MonitoringStatus) %>%
    dplyr::group_by(Park, FieldSeason) %>%
    dplyr::mutate(Triennial = dplyr::case_when(Park %in% c("LAKE", "MOJA") & (as.numeric(FieldSeason) - 2016) %% 3 == 0  ~ "Y",
                                               Park %in% c("JOTR", "PARA") & (as.numeric(FieldSeason) - 2017) %% 3 == 0  ~ "Y",
                                               Park %in% c("DEVA") & (as.numeric(FieldSeason) - 2018) %% 3 == 0 ~ "Y",
                                               Park %in% c("CAMO") & (as.numeric(FieldSeason) - 2017) %% 3 == 0 ~ "Y",
                                               TRUE ~ "N")) %>%
    # dplyr::filter(Triennial == "Y") %>%
    dplyr::ungroup() %>%
    dplyr::select(Park, FieldSeason, SampleFrame) %>%
    unique()
  
  expected <- df1 %>%
    dplyr::left_join(df2)
  
  samplestatus <- completeness %>%
    dplyr::filter(SampleFrame %in% c("Annual","3Yr"),
                  VisitType == "Primary",
                  MonitoringStatus == "Sampled") %>%
    dplyr::select(Park, SiteCode, SiteName, FieldSeason, SampleFrame, MonitoringStatus) %>%
    dplyr::right_join(expected, by = c("Park", "SiteCode", "SiteName", "SampleFrame", "FieldSeason")) %>%
    dplyr::mutate(MonitoringStatus = dplyr::case_when(is.na(MonitoringStatus) ~ "Not Sampled",
                                                      TRUE ~ MonitoringStatus)) %>%
    dplyr::group_by(Park, FieldSeason, SampleFrame, MonitoringStatus) %>%
    dplyr::summarize(Count = dplyr::n()) %>%
    dplyr::mutate(Percent = dplyr::case_when(Park == "DEVA" & SampleFrame == "3Yr" ~ Count/60*100,
                                             Park == "DEVA" & SampleFrame == "Annual" ~ Count/20*100,
                                             Park %in% c("JOTR", "LAKE", "MOJA", "PARA") & SampleFrame == "Annual" ~ Count/10*100,
                                             Park %in% c("MOJA", "PARA") & SampleFrame == "3Yr" ~ Count/35*100,
                                             Park == "JOTR" & SampleFrame == "3Yr" ~ Count/25*100,
                                             Park == "LAKE" & SampleFrame == "3Yr" ~ Count/33*100,
                                             Park == "CAMO" & SampleFrame == "3Yr" ~ Count/2*100,
                                             TRUE ~ as.double(NA))) %>%
    dplyr::mutate(Percent = round(Percent, 3)) %>%
    dplyr::arrange(Park, FieldSeason, desc(SampleFrame)) %>%
    dplyr::ungroup() %>%
    dplyr::select(Park, FieldSeason, SampleFrame, MonitoringStatus, Count, Percent)
  
  return(samplestatus)

}

#' Generate stacked bar plot for completeness (\% of planned sites visited).
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return ggplot stacked bar plot
#' @export
#' 
#' @examples
#' \dontrun{
#'     qcCompletenessPlot()
#'     qcCompletenessPlot(park = "DEVA", field.season = c("2018", "2020", "2021"))
#' }
  qcCompletenessPlot <- function(park, site, field.season) {
    
    completecount <- qcCompleteness(park = park, site = site, field.season = field.season)
    
  df2 <- completecount %>%
        dplyr::mutate(SampleStatus = paste(SampleFrame, MonitoringStatus, sep = " - ")) %>%
        dplyr::filter(Park != "CAMO")
  
  completeness.plot <- ggplot(df2, aes(fill = SampleStatus, x = FieldSeason, y = Count)) +
    geom_bar(position = "stack", stat = "identity") +
    xlab("Park") +
    ylab("Number of Springs Monitored") + 
    facet_grid(~Park, space = "free_x") +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_y_continuous(breaks = seq(0, 80, 10)) +
    scale_fill_manual(values = c("rosybrown2", "firebrick", "lightblue", "steelblue"))

    return(completeness.plot)    
}


#' Return list of site visits that have any data categorized as "Raw" or "Provisional"
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
#'     qcDPLCheck()
#'     qcDPLCheck(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcDPLCheck(park = "DEVA", field.season = c("2018", "2020", "2021"))
#' }
    qcDPLCheck <- function(park, site, field.season) {
      
      visit <- ReadAndFilterData(park = park, data.name = "Visit")
      flowcondition <- ReadAndFilterData(park = park, data.name = "DischargeFlowCondition")
      estimated <- ReadAndFilterData(park = park, data.name = "DischargeEstimated")
      volumetric <- ReadAndFilterData(park = park, data.name = "DischargeVolumetric")
      disturbance <- ReadAndFilterData(park = park, data.name = "Disturbance")
      flowmod <- ReadAndFilterData(park = park, data.name = "DisturbanceFlowModification")
      invasives <- ReadAndFilterData(park = park, data.name = "Invasives")
      riparian <- ReadAndFilterData(park = park, data.name = "Riparian")
      wildlife <- ReadAndFilterData(park = park, data.name = "Wildlife")
      temp <- ReadAndFilterData(park = park, data.name = "WaterQualityTemperature")
      ph <- ReadAndFilterData(park = park, data.name = "WaterQualitypH")
      spcond <-ReadAndFilterData(park = park, data.name = "WaterQualitySpCond")
      do <- ReadAndFilterData(park = park, data.name = "WaterQualityDO")

visit.DPL <- visit %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, VisitType, DPL) %>%
  dplyr::rename(Visit = DPL)
flowcondition.DPL <- flowcondition %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(FlowCondition = DPL) %>%
  dplyr::distinct()
estimated.DPL <- estimated %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(EstimatedDischarge = DPL) %>%
  dplyr::distinct()
volumetric.DPL <- volumetric %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(VolumetricDischarge = DPL) %>%
  dplyr::distinct()
disturbance.DPL <- disturbance %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(Disturbance = DPL) %>%
  dplyr::distinct()
flowmod.DPL <- flowmod %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(FlowModification = DPL) %>%
  dplyr::distinct()
wildlife.DPL <- wildlife %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(Wildlife = DPL) %>%
  dplyr::distinct()
riparian.DPL <- riparian %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(Riparian = DPL) %>%
  dplyr::distinct()
invasives.DPL <- invasives %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(Invasives = DPL) %>%
  dplyr::distinct()
temp.DPL <- temp %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(Temperature = DPL) %>%
  dplyr::distinct()
ph.DPL <- ph %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(pH = DPL) %>%
  dplyr::distinct()
spcond.DPL <- spcond %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(SpCond = DPL) %>%
  dplyr::distinct()
do.DPL <- do %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(DisOxygen = DPL) %>%
  dplyr::distinct()

dpl <- visit.DPL %>%
  dplyr::left_join(flowcondition.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(estimated.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(volumetric.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(disturbance.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(flowmod.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(wildlife.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(riparian.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(invasives.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(temp.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(ph.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(spcond.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(do.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  unique() %>%
  dplyr::filter_all(dplyr::any_vars(. %in% c("Raw", "Provisional"))) %>%
  dplyr::arrange(FieldSeason, Park, SiteCode)
 
  return(dpl) 
}


#' Return list of springs that have been given different classifications
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
#'     qcSpringTypeDiscrepancies()
#'     qcSpringTypeDiscrepancies(site = "LAKE_P_GET0066")
#'     qcSpringTypeDiscrepancies(park = "DEVA", field.season = c("2018", "2021"))
#' }
      qcSpringTypeDiscrepancies <- function(park, site, field.season) {
        visit <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Visit")
  discrepancies <- visit %>%
    dplyr::filter(VisitType == "Primary", MonitoringStatus == "Sampled") %>%
    dplyr::arrange(SiteCode, VisitDate) %>%
    dplyr::select(Park, SiteCode, SiteName, FieldSeason, SpringType) %>%
    dplyr::group_by(Park, SiteCode, SiteName, SpringType) %>%
    dplyr::mutate(FieldSeasons = paste0(FieldSeason, collapse = ", ")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("FieldSeason")) %>%
    unique() %>%
    dplyr::filter(duplicated(SiteCode) | duplicated(SiteCode, fromLast = TRUE))

   return(discrepancies)
}


#' Return list of dates that each spring has been visited 
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
#'     qcVisitDate()
#'     qcVisitDate(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcVisitDate(park = "DEVA", field.season = c("2018", "2020", "2021"))
#' }
        qcVisitDate <- function(park, site, field.season) {
          visit <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Visit") 
  visit.dates <- visit %>%
    dplyr::filter(VisitType == "Primary", MonitoringStatus == "Sampled", SampleFrame %in% c("Annual", "3Yr")) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame) %>%
    dplyr::mutate(VisitDate = as.POSIXct(VisitDate)) %>%
    dplyr::mutate(Month = as.factor(format(VisitDate, "%b"))) %>%
    dplyr::mutate(Day = as.integer(format(VisitDate, "%d"))) %>%
    dplyr::mutate(Month = factor(Month, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))) %>%
    dplyr::arrange(Month, Day) %>%
    dplyr::mutate(Date = paste(Month, Day, sep = " ")) %>%
    dplyr::mutate(Date = paste0(Date, " (", FieldSeason, ")")) %>%
    dplyr::select(Park, SiteCode, SiteName, SampleFrame, Date) %>%
    dplyr::group_by(Park, SiteCode, SiteName, SampleFrame) %>%
    dplyr::mutate(VisitDates = paste0(Date, collapse = ", ")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-Date) %>%
    dplyr::arrange(SiteCode) %>%
    unique()
  
  return(visit.dates)
}


#' Generate timeline of dates that each spring has been visited
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return ggplot timeline
#' @export
#'
#' @examples
#' \dontrun{

#'     qcVisitDateTimelines()
#'     qcVisitDateTimelines(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcVisitDateTimelines(park = "DEVA", field.season = c("2018", "2020", "2021"))
#' }
  qcVisitDateTimelines <- function(park, site, field.season) {
     visit <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Visit") 

  grouping_vars <- c("Park", "FieldSeason", "SiteCode") # Set grouping vars here so that we can add the facet column if needed
  median_grouping_vars <- c("Park", "SiteName", "SiteCode")
  
  visit.dates <- visit %>%
    dplyr::filter(VisitType == "Primary", MonitoringStatus == "Sampled", SampleFrame %in% c("Annual", "3Yr")) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame) %>%
    dplyr::mutate(Month = as.factor(format(VisitDate, "%b"))) %>%
    dplyr::mutate(MonthNum = as.integer(format(VisitDate, "%m"))) %>%
    dplyr::mutate(Day = as.integer(format(VisitDate, "%d"))) %>%
    dplyr::mutate(Month = factor(Month, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))) %>%
    dplyr::mutate(Date = paste(Month, Day, sep = " ")) %>%
    dplyr::mutate(Year = dplyr::case_when(Month %in% c("Oct", "Nov", "Dec") ~ 2019,
                                          TRUE ~ 2020)) %>%
    dplyr::mutate(VisitMonthDay = lubridate::ymd(paste(Year, Month, Day, sep ="-")),
                  pt_tooltip = paste(Date, as.character(lubridate::year(VisitDate))),
                  Event_mmdd = as.Date(paste0(as.character(lubridate::month(VisitMonthDay)), '-', as.character(lubridate::day(VisitMonthDay)), '-', as.character(Year)), format = "%m-%d-%Y")) %>%
    dplyr::arrange(VisitDate) %>%
    dplyr::filter(SiteCode != "JOTR_P_BLA0045")
  
  median.dates <- visit.dates %>%
    dplyr::group_by(dplyr::across(median_grouping_vars)) %>%
    dplyr::summarise(Median.Date = median(Event_mmdd),
                     Max = max(Event_mmdd),
                     Min = min(Event_mmdd),
                     Spread = max(Event_mmdd) - min(Event_mmdd)) %>%
    dplyr::mutate(med_tooltip = format(Median.Date, "Median: %b %d"),
                  min_tooltip = format(Min, "Median: %b %d"),
                  max_tooltip = format(Max, "Median: %b %d")) %>%
    dplyr::ungroup()

  visit.dates.df <- as.data.frame(visit.dates)
  
  plt <- FormatPlot(data = visit.dates.df,
                    x.col = Event_mmdd,
                    y.col = factor(SiteCode),
                    facet.col = Park,
                    facet.as.subtitle = FALSE,
                    n.col.facet = 1,
                    x.lab = "Date",
                    y.lab = "Spring Code") +
    suppressWarnings(ggplot2::geom_point(ggplot2::aes(color = FieldSeason,
                                                      text = paste0("Site Name: ", SiteName, "<br>",
                                                                   "Site Code: ", SiteCode, "<br>",
                                                                   "Visit Date: ", pt_tooltip, "<br>",
                                                                   "Field Season: ", FieldSeason)),
                                         alpha = 0.7)) + # Using text aesthetic to make tooltips work with plotly. This generates a warning so we have to suppress it.
    ggplot2::geom_line(alpha = 0.4) +
    suppressWarnings(ggplot2::geom_point(ggplot2::aes(x = Median.Date,
                                                      shape = "median",
                                                      text = paste0("Site Name: ", SiteName, "<br>",
                                                                    "Site Code: ", SiteCode, "<br>",
                                                                    "Median Date: ", med_tooltip, "<br>",
                                                                    "Range (Days): ", Spread, "<br>",
                                                                    "Earliest: ", min_tooltip, "<br>",
                                                                    "Latest: ", max_tooltip)),
                                         data = median.dates,
                                         size = 1,
                                         alpha = 0.7)) +
    ggplot2::scale_shape_manual(values = c("median" = 3)) + # Do this so that the median symbol shows up in the legend
    ggplot2::labs(color = "FieldSeason",
                  shape = NULL) +
    ggplot2::scale_x_date(date_breaks = "1 month",
                          date_labels = "%b %e",
                          limits = c(lubridate::floor_date(min(visit.dates$Event_mmdd), "month"),
                                     lubridate::ceiling_date(max(visit.dates$Event_mmdd), "month"))) +
    ggplot2::scale_y_discrete(limits = rev)
  
  return(plt)
  
}


#' Return list of springs that were not sampled during a field season when they were intended to be monitored
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
#'     qcNotSampled()
#'     qcNotSampled(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcNotSampled(park = "DEVA", field.season = c("2018", "2020", "2021"))
#' }
    qcNotSampled <- function(park, site, field.season) {
      visit <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Visit") 
      
      notsampled <- visit %>%
        dplyr::filter(MonitoringStatus != "Sampled") %>%
        dplyr::select(-c("DPL", "SpringType"))
  return(notsampled)
}


#' Return list of springs that were visited multiple times during a field season
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
#'     qcRepeatVisits()
#'     qcRepeatVisits(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcRepeatVisits(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
qcRepeatVisits <- function(park, site, field.season) {
  visit <- ReadAndFilterData(park, site, field.season) 
  
  repeats <- visit %>%
    dplyr::group_by(SiteCode, FieldSeason) %>%
    dplyr::mutate(Duplicated = dplyr::n() > 1) %>%
    dplyr::filter(Duplicated == TRUE | VisitType != "Primary") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, VisitType, MonitoringStatus, Notes) %>%
    dplyr::arrange(FieldSeason, SiteCode, VisitDate)
  
  return(repeats)
   
}


#' Map of annual and 3-year desert springs monitoring locations
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
#'     LocationMap()
#'     LocationMap(site = "LAKE_P_GET0066")
#'     LocationMap(park = c("DEVA", "MOJA"))
#' }
    LocationMap <- function(park, site, field.season) {
      site <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Site")

  
  coords <- site %>%
    dplyr::select(Park, SiteCode, SiteName, GRTSOrder, SiteStatus, SampleFrame, Lat_WGS84, Lon_WGS84, X_UTM_NAD83_11N, Y_UTM_NAD83_11N) %>%
    dplyr::mutate(SampleFrameSimple = dplyr::case_when(SampleFrame == "Annual" ~ "Annual",
                                                       SampleFrame == "3Yr" ~ "3Yr",
                                                       TRUE ~ "Other")) %>%
    dplyr::mutate(SampleFrameRadius = dplyr::case_when(SampleFrameSimple == "Annual" ~ 5,
                                                       SampleFrameSimple == "3Yr" ~ 5,
                                                       TRUE ~ 3))

  coords$SampleFrameSimple <- factor(coords$SampleFrameSimple, levels = c("Annual", "3Yr", "Other"))
  
  coords  %<>% dplyr::arrange(desc(SampleFrameSimple))
  
  pal <- leaflet::colorFactor(palette = c("royalblue1", "red", "gold"),
                              domain = coords$SampleFrameSimple)
  
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
  
  sitemap <- leaflet::leaflet(coords, height = height, width = width) %>%
    leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
    leaflet::addScaleBar('bottomright') %>%
    leaflet::addCircleMarkers(lng = ~Lon_WGS84,
                              lat = ~Lat_WGS84,
                              popup = paste ("Name: ", coords$SiteName, "<br>",
                                             "Sample Frame: ", coords$SampleFrame, "<br>",
                                             "Latitude (UTM): ", coords$X_UTM_NAD83_11N, "<br>",
                                             "Longitude (UTM): ", coords$Y_UTM_NAD83_11N, "<br>",
                                             "Latitude (Decimal Degrees): ", coords$Lat_WGS84, "<br>",
                                             "Longitude (Decimal Degrees): ", coords$Lon_WGS84),
                              radius = ~SampleFrameRadius,
                              stroke = TRUE,
                              color = "black",
                              weight = 2,
                              fillOpacity = 0.8,
                              fillColor = ~pal(SampleFrameSimple),
                              group = ~SampleFrameSimple) %>%
    leaflet::addLegend(pal = pal,
                       values = ~SampleFrameSimple,
                       title = "Sample Frame",
                       opacity = 1,
                       position = "bottomleft") %>%
    leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                              overlayGroups = ~SampleFrameSimple,
                              options=leaflet::layersControlOptions(collapsed = FALSE))
  
  return(sitemap)
  
}