# Function NYI: Volumetric method, median, max, min times

VolumetricMedian  <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  
  volumetric <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "DischargeVolumetric")
  
  calculated <- volumetric %>%
    dplyr::mutate(Discharge_L_per_s = ((ContainerVolume_mL/1000)/FillTime_seconds)*(EstimatedCapture_percent/100))
  
  summarized <- calculated %>%
    dplyr::group_by(Park, SiteCode, SiteName, VisitDate, FieldSeason) %>%
    dplyr::summarize(Discharge_L_per_s = median(Discharge_L_per_s), Count = n()) %>%
    dplyr::arrange(FieldSeason, SiteCode) %>%
    dplyr::ungroup()
  
  return(summarized) 
}

# Joined discharge table

SpringDischarge <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  
  discharge <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "DischargeFlowCondition")
  estimated <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "DischargeEstimated")
  median <- VolumetricMedian(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  
  joined <- discharge %>%
    left_join(estimated, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "FlowCondition", "VisitType", "DPL")) %>%
    left_join(median, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason")) %>%
    dplyr::select(-Count) %>%
    dplyr::rename(VolDischarge_L_per_s = Discharge_L_per_s) %>%
    dplyr::arrange(FieldSeason, SiteCode)
 
  return(joined)
}


# Function NYI: Spring is dry, estimated discharge >0 or volumetric discharge >0 or springbrook dimension > 0

qcSpringDryDischarge <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  
  joined <- SpringDischarge(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
   
  dry <- joined %>%
    dplyr::filter(FlowCondition == "dry" & (DischargeClass_L_per_s != "0 L/s" | VolDischarge_L_per_s > 0 | SpringbrookLength_m > 0 | SpringbrookWidth_m > 0)) %>%
    dplyr::arrange(FieldSeason, SiteCode)
   
  return(dry)               
}


# Function NYI: Spring is not dry, estimated discharge = 0 or volumetric discharge = 0

qcSpringNotDryNoDischarge <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  
  joined <- SpringDischarge(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  
  nodischarge <- joined %>%
    dplyr::filter(FlowCondition != "dry" & (DischargeClass_L_per_s == "0 L/s" | VolDischarge_L_per_s == 0)) %>%
    dplyr::arrange(FieldSeason, SiteCode)
  
  return(nodischarge)
}  


# Function NYI: Spring is not dry, springbrook dimensions = 0
  
qcSpringNotDryNoSpringbrook <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  
  joined <- SpringDischarge(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  
  nobrook <- joined %>%
    dplyr::filter(!(FlowCondition %in% c("dry", "wet soil only")) & (SpringbrookLength_m == 0 | SpringbrookWidth_m == 0)) %>%
    dplyr::arrange(FieldSeason, SiteCode)
  
  return(nobrook)
  
}


# Function NYI: Volumetric method, but no container volume, percent of flow, or fill times

qcVolumetricMissing <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  
  volumetric <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "DischargeVolumetric")
  
  missing <- volumetric %>%
    dplyr::filter(is.na(ContainerVolume_mL) | is.na(FillTime_seconds) | is.na(EstimatedCapture_percent))
  
  return(missing)
}  


# Function NYI: Volumetric method, fewer than five fill times

qcVolumetricFillEvents <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  
  median <- VolumetricMedian(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  
  fills <- median %>%
    dplyr::filter(Count < 5)
  
  return(fills)
}


# Function NYI: Volumetric method, median time less than 5 seconds

qcVolumetricTimes <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

  volumetric <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "DischargeVolumetric")

  times <- volumetric %>%
    dplyr::group_by(Park, SiteCode, SiteName, VisitDate, FieldSeason) %>%
    dplyr::summarize(MedianFillTime_s = median(FillTime_seconds)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(MedianFillTime_s < 5) %>%
    dplyr::arrange(FieldSeason, SiteCode)
  
  return(times)
}
  
  
# Function NYI: Continuous surface water length > discontinuous surface water length

qcContinuousLength <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
 
  joined <- SpringDischarge(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
   
  discontinuous <- joined %>%
    dplyr::filter((SpringbrookLengthFlag == ">50 m" & DiscontinuousSpringbrookLengthFlagId == "2") | (SpringbrookLengthFlag == "Measured" & DiscontinuousSpringbrookLengthFlagId == "2" & (SpringbrookLength_m > DiscontinuousSpringbrookLength_m)))

  return(discontinuous)  
}

# Function NYI: Summary table of discharge categories: dry, wet soil, <10 m, 10-50 m, >50 m

FlowCategories <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
 
  joined <- SpringDischarge(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  visit <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "Visit")
  
  sampleframe <- visit %>%
    select(SiteCode, VisitDate, SampleFrame)
  
  categorized <- joined %>%
    dplyr::left_join(sampleframe, by = c("SiteCode", "VisitDate")) %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::mutate(FlowCategory = ifelse(SpringbrookLengthFlag == ">50m", "> 50 m",
                                    ifelse(FlowCondition == "dry", "Dry",
                                      ifelse(FlowCondition == "wet soil only" | (!(FlowCondition %in% c("dry", "wet soil only")) & (SpringbrookLength_m == 0 | SpringbrookWidth_m == 0)), "Wet Soil",
                                        ifelse(SpringbrookLength_m > 0 & SpringbrookLength_m < 10, "< 10 m",
                                            ifelse(SpringbrookLengthFlag == "Measured" & (SpringbrookLength_m >= 10 & SpringbrookLength_m <= 50), "10 - 50 m", NA)))))) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, FlowCondition, FlowCategory) %>%
    dplyr::group_by(Park, FieldSeason, SampleFrame, FlowCategory) %>%
    dplyr::summarize(Count = n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr")) %>%
    dplyr::arrange(Park, FieldSeason, SampleFrame, FlowCategory)
  
  return(categorized)
}

# Function NYI: Summary bar plot of flow categories for annual springs

FlowCategoriesAnnualPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  
  data <- DischargeCategories(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  
  data$FlowCategory <- factor(data$FlowCategory, levels = c("> 50 m", "10 - 50 m", "< 10 m", "Wet Soil", "Dry"))
  
  plot <- ggplot2::ggplot(data %>% filter(SampleFrame == "Annual"), aes(x = FieldSeason, y = Count, fill = FlowCategory)) +
    geom_bar(stat = "identity") +
    facet_grid(~Park) +
    scale_fill_manual(values = c("Dry" = "red",
                                 "Wet Soil" = "gold",
                                 "< 10 m" = "lightskyblue",
                                 "10 - 50 m" = "royalblue1",
                                 "> 50 m" = "navy")) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 90)) +
    labs(x = "Field Season",
         y = "Number of Springs", 
         fill = "Flow Category")
  
  return(plot)
}


# Function NYI: Summary bar plot of flow categories for 3-yr springs

FlowCategoriesThreeYearPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  
  data <- DischargeCategories(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  
  data$FlowCategory <- factor(data$FlowCategory, levels = c("> 50 m", "10 - 50 m", "< 10 m", "Wet Soil", "Dry"))
  
  plot <- ggplot2::ggplot(data %>% filter(SampleFrame == "3Yr"), aes(x = FieldSeason, y = Count, fill = FlowCategory)) +
    geom_bar(stat = "identity") +
    facet_grid(~Park, scale = "free", space = "free_x") +
    scale_fill_manual(values = c("Dry" = "red",
                                 "Wet Soil" = "gold",
                                 "< 10 m" = "lightskyblue",
                                 "10 - 50 m" = "royalblue1",
                                 "> 50 m" = "navy")) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 90)) +
    labs(x = "Field Season",
         y = "Number of Springs", 
         fill = "Flow Category")
  
  return(plot)
}


# Function NYI: Summary heat map of flow categories for annual springs

FlowCategoriesAnnualHeatMap <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  
  joined <- SpringDischarge(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  visit <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "Visit")
  
  sampleframe <- visit %>%
    select(SiteCode, VisitDate, SampleFrame)
  
  data <- joined %>%
    dplyr::left_join(sampleframe, by = c("SiteCode", "VisitDate")) %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::mutate(FlowCategory = ifelse(SpringbrookLengthFlag == ">50m", "> 50 m",
                                        ifelse(FlowCondition == "dry", "Dry",
                                               ifelse(FlowCondition == "wet soil only" | (!(FlowCondition %in% c("dry", "wet soil only")) & (SpringbrookLength_m == 0 | SpringbrookWidth_m == 0)), "Wet Soil",
                                                      ifelse(SpringbrookLength_m > 0 & SpringbrookLength_m < 10, "< 10 m",
                                                             ifelse(SpringbrookLengthFlag == "Measured" & (SpringbrookLength_m >= 10 & SpringbrookLength_m <= 50), "10 - 50 m", NA))))))
    
  data$FlowCategory <- factor(data$FlowCategory, levels = c("> 50 m", "10 - 50 m", "< 10 m", "Wet Soil", "Dry"))
  
  heatmap <- ggplot2::ggplot(data %>% filter(SampleFrame == "Annual"), aes(x = FieldSeason, 
                                                                    y = reorder(SiteCode, desc(SiteCode)),
                                                                    fill = FlowCategory)) + 
    geom_tile(color = "white") + 
    scale_fill_manual(values = c("navy", "royalblue1", "lightskyblue", "gold", "red"), name = "Flow Category") +
    labs(x = "Field Season",
         y = "Annual Spring") +
    theme(legend.position = "bottom") +
    facet_grid(Park~., scale = "free", space = "free_y")
  
  return(heatmap)
}


# Function NYI: Summary heat map of flow categories for 3-yr springs

FlowCategoriesThreeYearHeatMap <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  
  joined <- SpringDischarge(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  visit <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "Visit")
  
  sampleframe <- visit %>%
    select(SiteCode, VisitDate, SampleFrame)
  
  data <- joined %>%
    dplyr::left_join(sampleframe, by = c("SiteCode", "VisitDate")) %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::mutate(FlowCategory = ifelse(SpringbrookLengthFlag == ">50m", "> 50 m",
                                        ifelse(FlowCondition == "dry", "Dry",
                                               ifelse(FlowCondition == "wet soil only" | (!(FlowCondition %in% c("dry", "wet soil only")) & (SpringbrookLength_m == 0 | SpringbrookWidth_m == 0)), "Wet Soil",
                                                      ifelse(SpringbrookLength_m > 0 & SpringbrookLength_m < 10, "< 10 m",
                                                             ifelse(SpringbrookLengthFlag == "Measured" & (SpringbrookLength_m >= 10 & SpringbrookLength_m <= 50), "10 - 50 m", NA)))))) %>%
    dplyr::mutate(Visit = ifelse((Park %in% c("LAKE", "MOJA") & FieldSeason == "2016") | (Park %in% c("PARA", "JOTR", "CAMO") & FieldSeason == "2017") | (Park %in% c("DEVA") & FieldSeason == "2018"), "First",
                            ifelse((Park %in% c("LAKE", "MOJA", "CAMO") & FieldSeason == "2019") | (Park %in% c("PARA", "JOTR") & FieldSeason == "2020") | (Park %in% c("DEVA") & FieldSeason == "2021"), "Second",
                               ifelse((Park %in% c("LAKE", "MOJA", "CAMO") & FieldSeason == "2022") | (Park %in% c("PARA", "JOTR") & FieldSeason == "2023") | (Park %in% c("DEVA") & FieldSeason == "2024"), "Third", NA))))
  
  data$FlowCategory <- factor(data$FlowCategory, levels = c("> 50 m", "10 - 50 m", "< 10 m", "Wet Soil", "Dry"))
  
  heatmap <- ggplot2::ggplot(data %>% filter(SampleFrame == "3Yr"), aes(x = Visit, 
                                                                           y = reorder(SiteCode, desc(SiteCode)),
                                                                           fill = FlowCategory)) + 
    geom_tile(color = "white") + 
    scale_fill_manual(values = c("navy", "royalblue1", "lightskyblue", "gold", "red"), name = "Flow Category") +
    labs(x = "Revisit Cycle",
         y = "Three-Year Spring") +
    theme(legend.position = "bottom") +
    facet_grid(Park~., scale = "free", space = "free_y")
  
  return(heatmap)
}


# Function NYI: Map of discharge categories for latest field season at each park

# DischargeCategoriesMap