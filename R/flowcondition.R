# Calculate median discharge; join and clean flow condition, springbrook length, and discharge data; assign springbrook length to categories
# DischargeEstimated, DischargeFlowCondition, DischargeVolumetric,

QcFlow <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  volumetric <- desertsprings:::ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "DischargeVolumetric")
  estimated <- desertsprings:::ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "DischargeEstimated")
  flowcondition <- desertsprings:::ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "DischargeFlowCondition")
  
  volumetric.test <- volumetric %>%
    dplyr::group_by(Park, SiteCode, SiteName, VisitDate, FieldSeason, FlowCondition, ContainerVolume_mL, EstimatedCapture_percent, VisitType, DPL) %>%
    dplyr::summarize(MedianFillTime_seconds = median(FillTime_seconds)) %>%
    dplyr::summarize(VolumetricDischarge_L/s = ((ContainerVolume_mL/1000)/MedianFillTime_seconds)*(100/EstimatedCapture_percent))
    dplyr::relocate(MedianFillTime_seconds, .after = EstimatedCapture_percent)
  
  
  
  
  return(QcFlow)
  
}

# Plot springbrook length categories

# Map springbrook length categories