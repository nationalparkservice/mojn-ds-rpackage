# Calculate median discharge; join and clean flow condition, springbrook length, and discharge data; assign springbrook length to categories
# DischargeEstimated, DischargeFlowCondition, DischargeVolumetric,

QcFlowCat <- function(park, site, field.season) {
  volumetric <- desertsprings:::ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "DischargeVolumetric")
  estimated <- desertsprings:::ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "DischargeEstimated")
  flowcondition <- desertsprings:::ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "DischargeFlowCondition")
  
  # Find median fill time.
  volumetric.median <- volumetric %>%
    dplyr::group_by(Park, SiteCode, SiteName, VisitDate, FieldSeason, FlowCondition, ContainerVolume_mL, EstimatedCapture_percent, VisitType, DPL) %>%
    dplyr::summarize(MedianFillTime_seconds = median(FillTime_seconds)) %>%
    dplyr::relocate(MedianFillTime_seconds, .after = ContainerVolume_mL)
  
  # Calculate discharge.
  volumetric.discharge <- volumetric.median %>%
    dplyr::mutate(VolumetricDischarge_L_per_s = ContainerVolume_mL/1000/MedianFillTime_seconds*100/EstimatedCapture_percent) %>%
    dplyr::relocate(VolumetricDischarge_L_per_s, .after = EstimatedCapture_percent) %>%
    dplyr::ungroup()
  
  # Join volumetric and estimated discharge to flow condition.
  flowcondition.joined <- flowcondition %>%
    dplyr::left_join(dplyr::select(volumetric.discharge, VolumetricDischarge_L_per_s, c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "FlowCondition", "VisitType", "DPL")), by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "FlowCondition", "VisitType", "DPL")) %>%
    dplyr::left_join(dplyr::select(estimated, DischargeClass_L_per_s, c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "FlowCondition", "VisitType", "DPL")), by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "FlowCondition", "VisitType", "DPL")) %>%
    dplyr::relocate(VolumetricDischarge_L_per_s, .after = SpringbrookWidth_m) %>%
    dplyr::relocate(DischargeClass_L_per_s, .after = VolumetricDischarge_L_per_s)
    
  # Assign springbrook length categories. 1 = dry, 2 = wet soil, 3 = springbrook < 10 m, 4 = springbrook 10 - 50 m, 5 = springbrook > 50 m.
  fc.cat <- flowcondition.joined %>%
    dplyr::mutate(FlowCategory = ifelse(FlowCondition == "dry", "Dry",
                                   ifelse(FlowCondition == "wet soil only", "Wet Soil",
                                     ifelse(FlowCondition %in% c("standing water", "flowing", "flood") &
                                            SpringbrookLength_m < 10 &
                                            SpringbrookLengthFlag == "Measured", "< 10 m",
                                       ifelse(FlowCondition %in% c("standing water", "flowing", "flood") & 
                                              SpringbrookLength_m >= 10 & 
                                              SpringbrookLength_m <= 50 &
                                                SpringbrookLengthFlag == "Measured", "10 - 50 m",
                                         ifelse(FlowCondition %in% c("standing water", "flowing", "flood") &
                                                SpringbrookLengthFlag == ">50m", "> 50 m", NA))))))
  
  return(fc.cat)
}

# Discharge > 0, but flow condition dry. Flow condition not dry, but discharge = 0 or no estimate/volumetric measurement.

QcFlowCheck <- function(park, site, field.season) {
  fc.data <- QcFlowCat(park, site, field.season)
  
  fc.check.1 <- fc.data %>%
    dplyr::filter(FlowCondition == "dry" & VolumetricDischarge_L_per_s != 0)
  
  fc.check.2 <- fc.data %>%
    dplyr::filter(FlowCondition == "dry" & DischargeClass_L_per_s != "0 L/s")
  
  fc.check.3 <- fc.data %>%
    dplyr::filter(FlowCondition == "dry" & SpringbrookLength_m != 0)
  
  fc.check.4 <- fc.data %>%
    dplyr::filter(!FlowCondition %in% c("dry", "wet soil only") & VolumetricDischarge_L_per_s == 0)
  
  fc.check.5 <- fc.data %>%
    dplyr::filter(FlowCondition != "dry" & DischargeClass_L_per_s == "0 L/s")
  
  fc.check.6 <- fc.data %>%
    dplyr::filter(!FlowCondition %in% c("dry", "wet soil only") & SpringbrookLength_m == 0)
  
}

# Limited to annual and 3-year springs as well as primary visits.

QcFlowTidy <- function(park, site, field.season) {
  fc.cat.data <- QcFlowCat(park, site, field.season)
  
  flow.visits <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Visit")
  
  fc.tidy <- fc.cat.data %>%
    dplyr::left_join(dplyr::select(flow.visits, SampleFrame, c("Park", "FieldSeason", "SiteCode", "VisitDate")), by = c("Park", "FieldSeason", "SiteCode", "VisitDate")) %>%
    dplyr::filter(VisitType == "Primary", SampleFrame %in% c("Annual", "3Yr"))
  
  return(fc.tidy)
  
}

# Generate table of springbrook length categories



# Plot springbrook length categories

QcFcPlot <- function(park, site, field.season) {
  fc.plot.data <- QcFlowTidy(park, site, field.season)
  
  df <- fc.plot.data %>%
    dplyr::group_by(Park, FieldSeason, FlowCategory) %>%
    dplyr::filter(Park != "CAMO") %>%
    dplyr::summarize(Count = dplyr::n()) %>%
    dplyr::ungroup()
  
  fc.plot <- ggplot2::ggplot(df, aes(fill = factor(FlowCategory, levels=c("> 50 m", "10 - 50 m", "< 10 m", "Wet Soil", "Dry")), x = FieldSeason, y = Count)) +
    ggplot2::geom_bar(position = "stack", stat = "identity") +
    ggplot2::xlab("") +
    ggplot2::ylab("Number of Springs") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::facet_wrap(~Park, scales = "free", ncol = 1) +
    ggplot2::scale_fill_manual(values=c("blue", "dodgerblue", "skyblue1", "darkgoldenrod1", "red2", "gray40"), na.value ="gray40") +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(limits = rev(levels(as.factor(df$FieldSeason)))) +
    ggplot2::labs(fill = "Flow Category") +
    ggplot2::theme(legend.position="top", legend.box = "horizontal") +
    ggplot2::guides(fill = guide_legend(nrow = 1)) +
    ggplot2::ylim(0,80) +
    ggplot2::scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80))
  
  return(fc.plot)
}
# Just plot annual and/or 3-year springs

# Map springbrook length categories