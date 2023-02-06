#' Calculate median discharge and count of fill times for volumetric method
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
#'     VolumetricMedian()
#'     VolumetricMedian(site = "LAKE_P_GRA0058", field.season = "2019")
#'     VolumetricMedian(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
VolumetricMedian  <- function(park, site, field.season) {
  volumetric <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "DischargeVolumetric")
  
  calculated <- volumetric %>%
    dplyr::mutate(Discharge_L_per_s = ((ContainerVolume_mL/1000)/FillTime_seconds)*(100/EstimatedCapture_percent))
  
  summarized <- calculated %>%
    dplyr::group_by(Park, SiteCode, SiteName, VisitDate, FieldSeason) %>%
    dplyr::summarize(Discharge_L_per_s = median(Discharge_L_per_s),
                     Count = sum(!is.na(FillTime_seconds))) %>%
    dplyr::arrange(FieldSeason, SiteCode) %>%
    dplyr::ungroup()
  
  return(summarized) 
}


#' Join flow condition, estimated discharge, and volumetric discharge data into one table
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
#'     SpringDischarge()
#'     SpringDischarge(site = "LAKE_P_GRA0058", field.season = "2019")
#'     SpringDischarge(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
SpringDischarge <- function(park, site, field.season) {
    discharge <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "DischargeFlowCondition")
    estimated <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "DischargeEstimated")
    median <- VolumetricMedian(park = park, site = site, field.season = field.season)
    visit <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Visit")
  
  sampleframe <- visit %>%
    dplyr::select(SiteCode, VisitDate, SampleFrame, Panel)
  
  joined <- discharge %>%
    dplyr::left_join(estimated, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "FlowCondition", "VisitType", "DPL")) %>%
    dplyr::left_join(median, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason")) %>%
    dplyr::left_join(sampleframe, by = c("SiteCode", "VisitDate")) %>%
    dplyr::select(-DPL) %>%
    dplyr::relocate(SampleFrame, .after = FieldSeason) %>%
    dplyr::relocate(Panel, .after = SampleFrame) %>%
    dplyr::relocate(VisitType, .after = Panel) %>%
    dplyr::rename(VolDischarge_L_per_s = Discharge_L_per_s) %>%
    dplyr::relocate(VolDischarge_L_per_s, .after = FlowCondition) %>%
    dplyr::relocate(DischargeClass_L_per_s, .after = VolDischarge_L_per_s) %>%
    dplyr::mutate(VolDischarge_L_per_s = round(VolDischarge_L_per_s, 2)) %>%
    dplyr::arrange(FieldSeason, SiteCode) %>%
    dplyr::select(-Count) %>%
    dplyr::rename(DischargeNotes = Notes) %>%
    
 
  return(joined)
}


#' Spring is dry, estimated discharge > 0 or volumetric discharge > 0 or springbrook dimension > 0
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
#'     qcSpringDryWater()
#'     qcSpringDryWater(site = "LAKE_P_GRA0058", field.season = "2019")
#'     qcSpringDryWater(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
qcSpringDryWater <- function(park, site, field.season) {
    
  joined <- SpringDischarge(park = park, site = site, field.season = field.season)
   
  dry <- joined %>%
    dplyr::filter(FlowCondition == "dry" & (DischargeClass_L_per_s != "0 L/s" | VolDischarge_L_per_s > 0 | SpringbrookLength_m > 0 | SpringbrookWidth_m > 0)) %>%
    dplyr::arrange(FieldSeason, SiteCode) %>%
    dplyr::select(-SpringbrookType, -DiscontinuousSpringbrookLengthFlag, -DiscontinuousSpringbrookLength_m)

  return(dry)               
}


#' Spring is not dry, estimated discharge = 0 or volumetric discharge = 0
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
#'     qcSpringNotDryNoDischarge()
#'     qcSpringNotDryNoDischarge(site = "LAKE_P_GRA0058", field.season = "2019")
#'     qcSpringNotDryNoDischarge(park = c("DEVA", "JOTR"), field.season = c("2017", "2019", "2020"))
#' }
qcSpringNotDryNoDischarge <- function(park, site, field.season) {
  joined <- SpringDischarge(park = park, site = site, field.season = field.season)
  
  nodischarge <- joined %>%
    dplyr::filter(FlowCondition != "dry" & ((DischargeClass_L_per_s == "0 L/s" | VolDischarge_L_per_s == 0))) %>%
    dplyr::arrange(FieldSeason, SiteCode)
  
  return(nodischarge)
}  


#' Spring is not dry, springbrook dimensions = 0
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
#'     qcSpringNotDryNoSpringbrook()
#'     qcSpringNotDryNoSpringbrook(site = "DEVA_P_ARR0137", field.season = "2019")
#'     qcSpringNotDryNoSpringbrook(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
qcSpringNotDryNoSpringbrook <- function(park, site, field.season) {
  joined <- SpringDischarge(park = park, site = site, field.season = field.season)

  nobrook <- joined %>%
    dplyr::filter(!(FlowCondition %in% c("dry", "wet soil only")) & (SpringbrookLength_m == 0 | SpringbrookWidth_m == 0)) %>%
    dplyr::arrange(FieldSeason, SiteCode)
  
  return(nobrook)
}


#' Spring is not dry, estimated discharge = 0 or volumetric discharge = 0 or springbrook dimensions = 0
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
#'     qcSpringNotDryNoWater()
#'     qcSpringNotDryNoWater(site = "DEVA_P_ARR0137", field.season = "2019")
#'     qcSpringNotDryNoWater(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
qcSpringNotDryNoWater <- function(park, site, field.season) {
  joined <- SpringDischarge(park = park, site = site, field.season = field.season)

  nodischarge <- joined %>%
    dplyr::filter(FlowCondition != "dry" & ((DischargeClass_L_per_s == "0 L/s" | VolDischarge_L_per_s == 0))) %>%
    dplyr::arrange(FieldSeason, SiteCode)
  
  nobrook <- joined %>%
    dplyr::filter(!(FlowCondition %in% c("dry", "wet soil only")) & (SpringbrookLength_m == 0 | SpringbrookWidth_m == 0)) %>%
    dplyr::arrange(FieldSeason, SiteCode)
  
  nowater <- rbind(nodischarge, nobrook) %>%
    dplyr::arrange(FieldSeason, SiteCode) %>%
    unique() %>%
    dplyr::select(-SpringbrookType, -DiscontinuousSpringbrookLengthFlag, -DiscontinuousSpringbrookLength_m)
  
  return(nowater)
}


#' Volumetric or estimated discharge data are missing
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
#'     qcDischargeMissing()
#'     qcDischargeMissing(site = "LAKE_P_GET0066", field.season = "2016")
#'     qcDischargeMissing(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
qcDischargeMissing <- function(park, site, field.season) {
  joined <- SpringDischarge(park = park, site = site, field.season = field.season)

  dischargemissing <- joined %>%
    dplyr::filter(is.na(VolDischarge_L_per_s) & is.na(DischargeClass_L_per_s)) %>%
    dplyr::arrange(FieldSeason, SiteCode) %>%
    dplyr::select(-SpringbrookLengthFlag, -SpringbrookLength_m, -SpringbrookWidth_m, -SpringbrookType, -DiscontinuousSpringbrookLengthFlag, -DiscontinuousSpringbrookLength_m)
  
  
  return(dischargemissing)
}


#' Volumetric method was used, but there is no container volume, percent of flow, or fill time recorded
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
#'     qcVolumetricMissing()
#'     qcVolumetricMissing(site = "LAKE_P_GRA0058", field.season = "2019")
#'     qcVolumetricMissing(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
qcVolumetricMissing <- function(park, site, field.season) {
  volumetric <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "DischargeVolumetric")

  missing <- volumetric %>%
    dplyr::filter(is.na(ContainerVolume_mL) | is.na(FillTime_seconds) | is.na(EstimatedCapture_percent)) %>%
    dplyr::select(-VisitType, -DPL)
  
  return(missing)
}  


#' Volumetric method was used, but there are fewer than five fill times
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
#'     qcVolumetricFillEvents()
#'     qcVolumetricFillEvents(site = "MOJA_P_MAR0147", field.season = "2019")
#'     qcVolumetricFillEvents(park = c("DEVA", "MOJA"), field.season = c("2017", "2019", "2021"))
#' }
qcVolumetricFillEvents <- function(park, site, field.season) {
  median <- VolumetricMedian(park = park, site = site, field.season = field.season)

  fills <- median %>%
    dplyr::filter(0 < Count & Count < 5) %>%
    dplyr::mutate(Discharge_L_per_s = round(Discharge_L_per_s, 3))
  
  return(fills)
}


#' Volumetric method was used, but the median fill time is less than 5 seconds
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
#'     qcVolumetricTimes()
#'     qcVolumetricTimes(site = "LAKE_P_GRA0058", field.season = "2019")
#'     qcVolumetricTimes(park = c("DEVA", "MOJA"), field.season = c("2017", "2018", "2019"))
#' }
qcVolumetricTimes <- function(park, site, field.season) {
  volumetric <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "DischargeVolumetric")
  
  times <- volumetric %>%
    dplyr::group_by(Park, SiteCode, SiteName, VisitDate, FieldSeason) %>%
    dplyr::summarize(MedianFillTime_s = median(FillTime_seconds)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(MedianFillTime_s < 5) %>%
    dplyr::arrange(FieldSeason, SiteCode)
  
  return(times)
}
  

#' Continuous surface water length > discontinuous surface water length
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
#'     qcContinuousLength()
#'     qcContinuousLength(site = "LAKE_P_GRA0058", field.season = "2019")
#'     qcContinuousLength(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
qcContinuousLength <- function(park, site, field.season) {
  joined <- SpringDischarge(park = park, site = site, field.season = field.season)

  discontinuous <- joined %>%
    dplyr::filter((SpringbrookLengthFlag == "Length > 50 meters" & DiscontinuousSpringbrookLengthFlag == "Length <= 50 meters and was measured.") | (SpringbrookLengthFlag == "Length <= 50 meters and was measured." & DiscontinuousSpringbrookLengthFlag == "Length <= 50 meters and was measured." & (SpringbrookLength_m > DiscontinuousSpringbrookLength_m)))

  return(discontinuous)  
}


#' Summary table of flow categories for continuous springbrooks: dry, wet soil, <10 m, 10-50 m, >50 m
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
#'     FlowCategoriesContinuous()
#'     FlowCategoriesContinuous(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
FlowCategoriesContinuous <- function(park, site, field.season) {
  joined <- SpringDischarge(park = park, site = site, field.season = field.season)
  site <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Site")
  
  panel <- site %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr"),
                  Panel %in% c("A", "B", "C", "D")) %>%
    dplyr::mutate(Panel = dplyr::case_when(Panel == "A" ~ "Panel Annual",
                                           Panel == "B" ~ "Panel B",
                                           Panel == "C" ~ "Panel C",
                                           Panel == "D" ~ "Panel D",
                                           TRUE ~ NA_character_)) %>%
    dplyr::select(Park, SiteCode, SiteName, SampleFrame, Panel)
  
  categorized <- joined %>%
    dplyr::filter(VisitType == "Primary",
                  SampleFrame != "Rejected") %>%
    unique() %>%
    dplyr::mutate(FlowCategory = dplyr::case_when(SpringbrookLengthFlag == "Length > 50 meters" ~ "> 50 m",
                                                  FlowCondition == "dry" ~ "Dry",
                                                  FlowCondition == "wet soil only" | (!(FlowCondition %in% c("dry", "wet soil only")) & (SpringbrookLength_m == 0 | SpringbrookWidth_m == 0)) ~ "Wet Soil",
                                                  SpringbrookLength_m > 0 & SpringbrookLength_m < 10 ~ "< 10 m",
                                                  SpringbrookLengthFlag == "Length <= 50 meters and was measured." & (SpringbrookLength_m >= 10 & SpringbrookLength_m <= 50) ~ "10 - 50 m",
                                                  TRUE ~ "No Data")) %>%
    dplyr::select(Park, SiteCode, SiteName, FieldSeason, SampleFrame, Panel, FlowCategory) %>%
    dplyr::full_join(panel, by = c("Park", "SiteCode", "SiteName", "SampleFrame", "Panel")) %>%
    tidyr::complete(tidyr::nesting(Park, SiteCode, SiteName, SampleFrame, Panel), FieldSeason) %>%
    dplyr::filter(!is.na(FieldSeason)) %>%
    dplyr::filter(!(Park == "DEVA" & FieldSeason %in% c("2016", "2017")),
                  !(Park %in% c("JOTR", "CAMO") & FieldSeason == "2016")) %>%
    dplyr::filter(Panel == "Panel Annual" | (Panel == "Panel B" & (as.numeric(FieldSeason) - 2016) %% 3 == 0) | (Panel == "Panel C" & (as.numeric(FieldSeason) - 2017) %% 3 == 0) | (Panel == "Panel D" & (as.numeric(FieldSeason) - 2018) %% 3 == 0)) %>%
    dplyr::group_by(Park, FieldSeason, SampleFrame, Panel, FlowCategory) %>%
    dplyr::summarize(Count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(FlowCategory = dplyr::case_when(is.na(FlowCategory) ~ "No Data",
                                                  TRUE ~ FlowCategory)) %>%
    dplyr::arrange(Park, FieldSeason, SampleFrame, FlowCategory)
  
  return(categorized)
}


#' Summary table of flow categories for discontinuous springbrooks: dry, wet soil, <10 m, 10-50 m, >50 m
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
#'     FlowCategoriesDiscontinuous()
#'     FlowCategoriesDiscontinuous(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
FlowCategoriesDiscontinuous <- function(park, site, field.season) {
  joined <- SpringDischarge(park = park, site = site, field.season = field.season)
  site <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Site")
  
  panel <- site %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr"),
                  Panel %in% c("A", "B", "C", "D")) %>%
    dplyr::mutate(Panel = dplyr::case_when(Panel == "A" ~ "Panel Annual",
                                           Panel == "B" ~ "Panel B",
                                           Panel == "C" ~ "Panel C",
                                           Panel == "D" ~ "Panel D",
                                           TRUE ~ NA_character_)) %>%
    dplyr::select(Park, SiteCode, SiteName, SampleFrame, Panel)
  
  categorized <- joined %>%
    dplyr::filter(VisitType == "Primary",
                  SampleFrame != "Rejected") %>%
    unique() %>%
    dplyr::mutate(FlowCategory = dplyr::case_when(FlowCondition == "dry" ~ "Dry",
                                                  FlowCondition == "wet soil only" | (!(FlowCondition %in% c("dry", "wet soil only")) & (SpringbrookLength_m == 0 | SpringbrookWidth_m == 0)) ~ "Wet Soil",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLength_m > 0 & DiscontinuousSpringbrookLength_m < 10) | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLength_m > 0 & SpringbrookLength_m < 10) ~ "< 10 m",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLengthFlag == "Length <= 50 meters and was measured." & (DiscontinuousSpringbrookLength_m >= 10 & DiscontinuousSpringbrookLength_m <= 50)) | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLengthFlag == "Length <= 50 meters and was measured." & (SpringbrookLength_m >= 10 & SpringbrookLength_m <= 50)) ~ "10 - 50 m",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLengthFlag == "Length > 50 meters") | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLengthFlag == "Length > 50 meters") ~ "> 50 m",
                                                  TRUE ~ "No Data")) %>%
    dplyr::select(Park, SiteCode, SiteName, FieldSeason, SampleFrame, Panel, FlowCategory) %>%
    dplyr::full_join(panel, by = c("Park", "SiteCode", "SiteName", "SampleFrame", "Panel")) %>%
    tidyr::complete(tidyr::nesting(Park, SiteCode, SiteName, SampleFrame, Panel), FieldSeason) %>%
    dplyr::filter(!is.na(FieldSeason)) %>%
    dplyr::filter(!(Park == "DEVA" & FieldSeason %in% c("2016", "2017")),
                  !(Park %in% c("JOTR", "CAMO") & FieldSeason == "2016")) %>%
    dplyr::filter(Panel == "Panel Annual" | (Panel == "Panel B" & (as.numeric(FieldSeason) - 2016) %% 3 == 0) | (Panel == "Panel C" & (as.numeric(FieldSeason) - 2017) %% 3 == 0) | (Panel == "Panel D" & (as.numeric(FieldSeason) - 2018) %% 3 == 0)) %>%
    dplyr::group_by(Park, FieldSeason, SampleFrame, Panel, FlowCategory) %>%
    dplyr::summarize(Count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(FlowCategory = dplyr::case_when(is.na(FlowCategory) ~ "No Data",
                                                  TRUE ~ FlowCategory)) %>%
    dplyr::arrange(Park, FieldSeason, SampleFrame, FlowCategory)
  
  return(categorized)
}


#' Summary bar plot of flow categories for annual springs
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
#'     FlowCategoriesAnnualPlot()
#'     FlowCategoriesAnnualPlot(park = c("DEVA", "MOJA"), field.season = c("2016", "2018", "2021"))
#' }
FlowCategoriesAnnualPlot <- function(park, site, field.season) {
  data <- FlowCategoriesDiscontinuous(park = park, site = site, field.season = field.season)
  
  data$FlowCategory <- factor(data$FlowCategory, levels = c("No Data", "> 50 m", "10 - 50 m", "< 10 m", "Wet Soil", "Dry"))
  
  plot <- ggplot2::ggplot(data %>% dplyr::filter(SampleFrame == "Annual"),
                          ggplot2::aes(x = FieldSeason, y = Count, fill = FlowCategory)) +
    ggplot2::geom_bar(stat = "identity",
                      color = "white") +
    ggplot2::scale_fill_manual(values = c("No Data" = "gray70",
                                          "Dry" = "firebrick",
                                          "Wet Soil" = "goldenrod2",
                                          "< 10 m" = "#ABC1FF",
                                          "10 - 50 m" = "royalblue1",
                                          "> 50 m" = "navy")) +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.x = ggplot2::element_text(angle = 90,
                                                       vjust = 0.5)) +
    ggplot2::labs(x = "Field Season",
                  y = "Number of Springs", 
                  fill = "Flow Category") +
    ggplot2::scale_y_continuous(breaks=seq(0,20,2))
  
  if (length(unique(data$Park)) == 1) {
    return(plot)
  } else {
    return(plot + ggplot2::facet_grid(~Park))
  }
}


#' Summary bar plot of flow categories for 3-yr springs
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
#'     FlowCategoriesThreeYearPlot()
#'     FlowCategoriesThreeYearPlot(park = c("DEVA", "MOJA"), field.season = c("2017", "2018", "2019"))
#' }
FlowCategoriesThreeYearPlot <- function(park, site, field.season) {
  data <- FlowCategoriesDiscontinuous(park = park, site = site, field.season = field.season)
  
  data$FlowCategory <- factor(data$FlowCategory, levels = c("No Data", "> 50 m", "10 - 50 m", "< 10 m", "Wet Soil", "Dry"))
  
  plot <- ggplot2::ggplot(data %>% dplyr::filter(SampleFrame == "3Yr", Park != "CAMO"),
                          ggplot2::aes(x = FieldSeason, y = Count, fill = FlowCategory)) +
    ggplot2::geom_bar(stat = "identity",
                      color = "white") +
    ggplot2::scale_fill_manual(values = c("No Data" = "gray70",
                                          "Dry" = "firebrick",
                                          "Wet Soil" = "goldenrod2",
                                          "< 10 m" = "#ABC1FF",
                                          "10 - 50 m" = "royalblue1",
                                          "> 50 m" = "navy")) +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::labs(x = "Field Season",
                  y = "Number of Springs", 
                  fill = "Flow Category") +
    ggplot2::scale_y_continuous(breaks=seq(0,80,10))
  
  if (length(unique(data$Park)) == 1) {
    return(plot)
  } else {
    return(plot + ggplot2::facet_grid(~Park, scales = "free", space = "free_x"))
  }
}


#' Summary heat map of flow categories for annual springs
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return ggplot heat map plot
#' @export
#'
#' @examples
#' \dontrun{
#'     FlowCategoriesAnnualHeatMap()
#'     FlowCategoriesAnnualHeatMap(site = "LAKE_P_GRA0058", field.season = "2019")
#'     FlowCategoriesAnnualHeatMap(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
FlowCategoriesAnnualHeatMap <- function(park, site, field.season) {
  joined <- SpringDischarge(park = park, site = site, field.season = field.season)

  data <- joined %>%
    dplyr::filter(VisitType == "Primary",
                  Panel == "Panel Annual") %>%
    dplyr::mutate(FlowCategory = dplyr::case_when(FlowCondition == "dry" ~ "Dry",
                                                  FlowCondition == "wet soil only" | (!(FlowCondition %in% c("dry", "wet soil only")) & (SpringbrookLength_m == 0 | SpringbrookWidth_m == 0)) ~ "Wet Soil",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLength_m > 0 & DiscontinuousSpringbrookLength_m < 10) | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLength_m > 0 & SpringbrookLength_m < 10) ~ "< 10 m",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLengthFlag == "Length <= 50 meters and was measured." & (DiscontinuousSpringbrookLength_m >= 10 & DiscontinuousSpringbrookLength_m <= 50)) | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLengthFlag == "Length <= 50 meters and was measured." & (SpringbrookLength_m >= 10 & SpringbrookLength_m <= 50)) ~ "10 - 50 m",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLengthFlag == "Length > 50 meters") | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLengthFlag == "Length > 50 meters") ~ "> 50 m",
                                                  TRUE ~ "NA"))
    
  data$FlowCategory <- factor(data$FlowCategory, levels = c("> 50 m", "10 - 50 m", "< 10 m", "Wet Soil", "Dry"))
  
  heatmap <- ggplot2::ggplot(data %>% dplyr::filter(SampleFrame == "Annual"), ggplot2::aes(x = FieldSeason, 
                                                                                           y = reorder(SiteCode, dplyr::desc(SiteCode)),
                                                                                           fill = FlowCategory,
                                                                                           text = paste("Site Name: ", SiteName,
                                                                                                        "<br>Site Code:", SiteCode,
                                                                                                        "<br>Field Season:", FieldSeason,
                                                                                                        "<br>Flow Category:", FlowCategory))) + 
    ggplot2::geom_tile(color = "white") + 
    ggplot2::scale_fill_manual(values = c("Dry" = "firebrick",
                                          "Wet Soil" = "goldenrod2",
                                          "< 10 m" = "#ABC1FF",
                                          "10 - 50 m" = "royalblue1",
                                          "> 50 m" = "navy"),
                               name = "Flow Category") +
    ggplot2::labs(x = "Field Season",
                  y = "Annual Spring") +
    ggplot2::theme(legend.position = "bottom")
  
  if (length(unique(data$Park)) == 1) {
    return(heatmap)
  } else {
    return(heatmap + ggplot2::facet_grid(Park~., scales = "free", space = "free_y"))
  }
}


#' Summary heat map of flow categories for 3-yr springs
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return ggplot heat map plot
#' @export
#'
#' @examples
#' \dontrun{
#'     FlowCategoriesThreeYearHeatMap()
#'     FlowCategoriesThreeYearHeatMap(site = "PARA_P_WHI0054", field.season = "2020")
#'     FlowCategoriesThreeYearHeatMap(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
FlowCategoriesThreeYearHeatMap <- function(park, site, field.season) {
  joined <- SpringDischarge(park = park, site = site, field.season = field.season)

  data <- joined %>%
    dplyr::filter(VisitType == "Primary",
                  Panel %in% c("Panel B", "Panel C", "Panel D")) %>%
    dplyr::mutate(FlowCategory = dplyr::case_when(FlowCondition == "dry" ~ "Dry",
                                                  FlowCondition == "wet soil only" | (!(FlowCondition %in% c("dry", "wet soil only")) & (SpringbrookLength_m == 0 | SpringbrookWidth_m == 0)) ~ "Wet Soil",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLength_m > 0 & DiscontinuousSpringbrookLength_m < 10) | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLength_m > 0 & SpringbrookLength_m < 10) ~ "< 10 m",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLengthFlag == "Length <= 50 meters and was measured." & (DiscontinuousSpringbrookLength_m >= 10 & DiscontinuousSpringbrookLength_m <= 50)) | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLengthFlag == "Length <= 50 meters and was measured." & (SpringbrookLength_m >= 10 & SpringbrookLength_m <= 50)) ~ "10 - 50 m",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLengthFlag == "Length > 50 meters") | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLengthFlag == "Length > 50 meters") ~ "> 50 m",
                                                  TRUE ~ "NA")) %>%
    dplyr::mutate(Visit = ifelse((Park %in% c("LAKE", "MOJA") & FieldSeason == "2016") | (Park %in% c("PARA", "JOTR", "CAMO") & FieldSeason == "2017") | (Park %in% c("DEVA") & FieldSeason == "2018"), "First",
                            ifelse((Park %in% c("LAKE", "MOJA", "CAMO") & FieldSeason == "2019") | (Park %in% c("PARA", "JOTR") & FieldSeason == "2020") | (Park %in% c("DEVA") & FieldSeason == "2021"), "Second",
                               ifelse((Park %in% c("LAKE", "MOJA", "CAMO") & FieldSeason == "2022") | (Park %in% c("PARA", "JOTR") & FieldSeason == "2023") | (Park %in% c("DEVA") & FieldSeason == "2024"), "Third", NA)))) %>%
    dplyr::filter(!is.na(Visit))
  
  data$FlowCategory <- factor(data$FlowCategory, levels = c("> 50 m", "10 - 50 m", "< 10 m", "Wet Soil", "Dry"))
  
  heatmap <- ggplot2::ggplot(data %>% dplyr::filter(SampleFrame == "3Yr", Park != "CAMO"),
                             ggplot2::aes(x = Visit, 
                                          y = reorder(SiteCode, dplyr::desc(SiteCode)),
                                          fill = FlowCategory,
                                          text = paste("Site Name: ", SiteName,
                                                       "<br>Site Code:", SiteCode,
                                                       "<br>Field Season:", FieldSeason,
                                                       "<br>Flow Category:", FlowCategory))) + 
    ggplot2::geom_tile(color = "white") + 
    ggplot2::scale_fill_manual(values = c("Dry" = "firebrick",
                                          "Wet Soil" = "goldenrod2",
                                          "< 10 m" = "#ABC1FF",
                                          "10 - 50 m" = "royalblue1",
                                          "> 50 m" = "navy"),
                               name = "Flow Category") +
    ggplot2::labs(x = "Revisit Cycle",
                  y = "Three-Year Spring") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::facet_grid(Park~., scales = "free", space = "free_y")
  
  if (length(unique(data$Park)) == 1) {
    return(heatmap)
  } else {
    return(heatmap + ggplot2::facet_grid(Park~., scales = "free", space = "free_y"))
  }
}


#' Map of spring flow categories for latest field season at each park
#'
#' @param interactive Optional. Choose "yes" or "no." Yes will allow the user to toggle between field seasons of data. No will show only the latest field season of data. If no argument is entered, function will default to "no" for greater accessibility.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return leaflet map
#' @export
#'
#' @examples
#' \dontrun{
#'     FlowCategoriesMap(interactive = "no")
#'     FlowCategoriesMap(interactive = "yes", site = "LAKE_P_GRA0058", field.season = "2019")
#'     FlowCategoriesMap(interactive = "yes", park = c("DEVA", "MOJA"), field.season = c("2017", "2018", "2021"))
#' }
FlowCategoriesMap <- function(interactive, park, site, field.season) {
  discharge <- SpringDischarge(park = park, site = site, field.season = field.season)
  site <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Site")
  
  coords <- site %>%
    dplyr::select(SiteCode, Lat_WGS84, Lon_WGS84, X_UTM_NAD83_11N, Y_UTM_NAD83_11N)
  
  flowcat <- discharge %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::mutate(FlowCategory = dplyr::case_when(FlowCondition == "dry" ~ "Dry",
                                                  FlowCondition == "wet soil only" | (!(FlowCondition %in% c("dry", "wet soil only")) & (SpringbrookLength_m == 0 | SpringbrookWidth_m == 0)) ~ "Wet Soil",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLength_m > 0 & DiscontinuousSpringbrookLength_m < 10) | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLength_m > 0 & SpringbrookLength_m < 10) ~ "< 10 m",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLengthFlag == "Length <= 50 meters and was measured." & (DiscontinuousSpringbrookLength_m >= 10 & DiscontinuousSpringbrookLength_m <= 50)) | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLengthFlag == "Length <= 50 meters and was measured." & (SpringbrookLength_m >= 10 & SpringbrookLength_m <= 50)) ~ "10 - 50 m",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLengthFlag == "Length > 50 meters") | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLengthFlag == "Length > 50 meters") ~ "> 50 m",
                                                  TRUE ~ "NA")) %>%
    dplyr::filter(FlowCategory != "NA") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, FlowCondition, FlowCategory, SpringbrookLength_m, DiscontinuousSpringbrookLength_m, VolDischarge_L_per_s, DischargeClass_L_per_s) %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr")) %>%
    dplyr::arrange(Park, FieldSeason, SampleFrame, FlowCategory) %>%
    dplyr::left_join(coords, by = "SiteCode") %>%
    dplyr::mutate(Year = as.numeric(FieldSeason)) %>%
    dplyr::relocate(Year, .after = FieldSeason)
  
  if (!missing(interactive)) {
      if (interactive %in% c("Yes", "yes", "Y", "y")) {
      } else {
        if (!missing(field.season)) {
          flowcat %<>%
            dplyr::filter(FieldSeason == field.season)
        } else {
          flowcat %<>%
            dplyr::filter(FieldSeason == max(FieldSeason))  
        }
      }
  } else {      
    if (!missing(field.season)) {
        flowcat %<>%
          dplyr::filter(FieldSeason == field.season)
     } else {
        flowcat %<>%
          dplyr::filter(FieldSeason == max(FieldSeason))  
     }
  }
  
  flowcat$FlowCategory <- factor(flowcat$FlowCategory, levels = c("> 50 m", "10 - 50 m", "< 10 m", "Wet Soil", "Dry"))
  
  flowcat %<>% dplyr::arrange(FieldSeason, dplyr::desc(FlowCategory))
  
  pal <- leaflet::colorFactor(palette = c("Dry" = "firebrick",
                                          "Wet Soil" = "goldenrod2",
                                          "< 10 m" = "#ABC1FF",
                                          "10 - 50 m" = "royalblue1",
                                          "> 50 m" = "navy"),
                              domain = flowcat$FlowCategory)
  
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
  
  sd <- crosstalk::SharedData$new(flowcat)
  year_filter <- crosstalk::filter_slider("year",
                                          "",
                                          sd,
                                          column = ~Year,
                                          ticks = TRUE,
                                          # width = width,
                                          step = 1,
                                          sep = "",
                                          pre = "WY",
                                          post = NULL,
                                          dragRange = TRUE)
  
  flowmap <- leaflet::leaflet(sd
                              # , height = height, width = width
                              ) %>%
    leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
    leaflet::addScaleBar('bottomright') %>%
    leaflet::addCircleMarkers(lng = ~Lon_WGS84,
                              lat = ~Lat_WGS84,
                              popup = paste ("Name: ", flowcat$SiteName, "<br>",
                                             "Sample Frame: ", flowcat$SampleFrame, "<br>",
                                             "Field Season: ", flowcat$FieldSeason, "<br>",
                                             "Flow Category: ", flowcat$FlowCategory, "<br>",
                                             "Estimated Discharge (L/s): ", flowcat$DischargeClass_L_per_s, "<br>",
                                             "Volumetric Discharge (L/s): ", round(flowcat$VolDischarge_L_per_s, 3)),
                              radius = 5,
                              stroke = TRUE,
                              color = "black",
                              weight = 1,
                              fillOpacity = 1,
                              fillColor = ~pal(FlowCategory),
                              group = ~FlowCategory) %>%
    leaflet::addLegend(pal = pal,
                       values = ~FlowCategory,
                       title = "Flow Category",
                       opacity = 1,
                       position = "bottomleft") %>%
    leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                              overlayGroups = ~FlowCategory,
                              options=leaflet::layersControlOptions(collapsed = FALSE))
  
  if (!missing(interactive)) {
    if (interactive %in% c("Yes", "yes", "Y", "y")) {
      if(missing(field.season)) {
        flowmap <- crosstalk::bscols(list(year_filter,
                                     flowmap))
      } else {
      }  
    } else {
    }
  } else {
  }
     
  return(flowmap)
}


#' Box plot of springbrook lengths for annual springs at each park and field season
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return ggplot box plot
#' @export
#'
#' @examples
#' \dontrun{
#'     SpringbrookLengthsAnnualPlot()
#'     SpringbrookLengthsAnnualPlot(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
SpringbrookLengthsAnnualPlot <- function(park, site, field.season) {
  joined <- SpringDischarge(park = park, site = site, field.season = field.season)

  discontinuous <- joined %>%
    dplyr::mutate(SpringbrookLength_m = dplyr::if_else(SpringbrookLengthFlag == ">50m", 50, SpringbrookLength_m)) %>%
    dplyr::mutate(NewSpringbrookLength_m = dplyr::if_else(!is.na(DiscontinuousSpringbrookLength_m), DiscontinuousSpringbrookLength_m, SpringbrookLength_m))
    
  plot <- ggplot2::ggplot(discontinuous %>% dplyr::filter(SampleFrame == "Annual"), ggplot2::aes(x = FieldSeason, y = NewSpringbrookLength_m)) +
    geom_boxplot() +
    facet_grid(~Park, scales = "free", space = "free_x") +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "Field Season",
         y = "Springbrook Length (m)")
  
  return(plot)
  
}


#' Box plot of springbrook lengths for three-year springs at each park and field season
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return ggplot box plot
#' @export
#'
#' @examples
#' \dontrun{
#'     SpringbrookLengthsThreeYearPlot()
#'     SpringbrookLengthsThreeYearPlot(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
SpringbrookLengthsThreeYearPlot <- function(park, site, field.season) {
  joined <- SpringDischarge(park = park, site = site, field.season = field.season)
  
  discontinuous <- joined %>%
    dplyr::mutate(SpringbrookLength_m = dplyr::if_else(SpringbrookLengthFlag == ">50m", 50, SpringbrookLength_m)) %>%
    dplyr::mutate(NewSpringbrookLength_m = dplyr::if_else(!is.na(DiscontinuousSpringbrookLength_m), DiscontinuousSpringbrookLength_m, SpringbrookLength_m))
  
  plot <- ggplot2::ggplot(discontinuous %>% dplyr::filter(SampleFrame == "3Yr"), ggplot2::aes(x = FieldSeason, y = NewSpringbrookLength_m)) +
    geom_boxplot() +
    facet_grid(~Park, scales = "free", space = "free_x") +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "Field Season",
         y = "Springbrook Length (m)")
  
  return(plot)
  
}