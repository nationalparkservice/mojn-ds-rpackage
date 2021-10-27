# Overall disturbance < any other disturbance category
qcOverallDisturbance <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  disturbance <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Disturbance")
  
  overall <- disturbance %>%
    dplyr::filter((Overall < Roads & Roads != "NoData") |
                    (Overall < HumanUse & HumanUse != "NoData") |
                    (Overall < PlantManagement & PlantManagement != "NoData") |
                    (Overall < HikingTrails & HikingTrails != "NoData") |
                    (Overall < Livestock & Livestock != "NoData") |
                    (Overall < OtherAnthropogenic & OtherAnthropogenic != "NoData") |
                    (Overall < Fire & Fire != "NoData") |
                    (Overall < Flooding & Flooding != "NoData") |
                    (Overall < Wildlife & Wildlife != "NoData") |
                    (Overall < OtherNatural & OtherNatural != "NoData")) %>%
    dplyr::select(Park:Overall) %>%
    dplyr::arrange(FieldSeason, Park, SiteCode)
  
  return(overall)
}

# Flow modification exists, but no Human Use disturbance
qcFlowModNoHuman <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  disturbance <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Disturbance")

  nohuman <- disturbance %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, HumanUse, FlowModificationStatus) %>%
    dplyr::filter(HumanUse == "0" & stringr::str_detect(FlowModificationStatus, "Yes")) %>%
    dplyr::arrange(FieldSeason, Park, SiteCode)
    
  return(nohuman)
}


# List of springs with active or historic flow modification
FlowModStatus <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  flowmod <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "DisturbanceFlowModification")
  
  status <- flowmod %>%
    dplyr::filter(stringr::str_detect(FlowModificationStatus, "Yes")) %>%
    dplyr::select(-c("VisitType", "DPL")) %>%
    dplyr::arrange(SiteCode) %>%
    dplyr::group_by(Park, SiteCode, SiteName, VisitDate, FieldSeason, FlowModificationStatus) %>%
    dplyr::mutate(FlowModificationTypes = paste0(ModificationType, collapse = ", ")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("ModificationType")) %>%
    unique()
  
  return(status)
}  
  
# List of springs that have been given different flow modification status in different field seasons   
qcFlowModDiscrepancies <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  flowmod <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "DisturbanceFlowModification") 
  
  discrepancies <- flowmod %>%
    dplyr::select(-c("VisitDate", "ModificationType", "VisitType", "DPL")) %>%
    unique() %>%
    dplyr::group_by(Park, SiteCode, SiteName, FlowModificationStatus) %>%
    dplyr::mutate(FieldSeasons = paste0(FieldSeason, collapse = ", ")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("FieldSeason")) %>%
    dplyr::filter(FlowModificationStatus != "No Data") %>%
    unique() %>%
    dplyr::filter(duplicated(SiteCode) | duplicated(SiteCode, fromLast = TRUE))
  
return(discrepancies)
}


# Table with count and percent of springs with active and historic flow modification
FlowModCount <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  flowmod <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "DisturbanceFlowModification") 
  site <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Site")
  
  sampleframe <- site %>%
    select(Park, SiteCode, SiteName, GRTSOrder, SiteStatus, SampleFrame) %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr") & SiteStatus == "T-S") %>%
    dplyr::select(-c("GRTSOrder", "SiteStatus", "SampleFrame")) %>%
    unique()
  
  count <- sampleframe %>%
    dplyr::left_join(flowmod, by = c("Park", "SiteCode", "SiteName")) %>%
    dplyr::filter(VisitType %in% c("Primary", NA)) %>%
    dplyr::select(-c(VisitDate, FieldSeason, ModificationType, VisitType, DPL)) %>%
    unique() %>%
    dplyr::mutate(FlowModificationStatus = ifelse(is.na(FlowModificationStatus), "No Data", FlowModificationStatus)) %>%
    dplyr::mutate(Rank = ifelse(FlowModificationStatus == "Yes - One or more active", 4,
                              ifelse(FlowModificationStatus == "Yes - All inactive", 3,
                                    ifelse(FlowModificationStatus == "Yes - Unknown if active", 2,
                                          ifelse(FlowModificationStatus == "None", 1, 
                                              ifelse(FlowModificationStatus == "No Data", 0, NA)))))) %>%
    dplyr::group_by(Park, SiteCode, SiteName) %>%
    dplyr::slice(which.max(Rank)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(SiteCode) %>%
    dplyr::select(-Rank) %>%
    dplyr::group_by(Park, FlowModificationStatus) %>%
    dplyr::mutate(Count = n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(SiteCode, SiteName)) %>%
    unique()
  
  total <- aggregate(Count ~ Park, count, sum) %>%
    dplyr::rename(Total = Count)
    
 percent <- count %>%
    dplyr::left_join(total, by = "Park") %>%
    dplyr::mutate(Percent = (Count / Total) * 100)

return(percent)
}


# Bar plot with percent of springs with active and historic flow modification
FlowModPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  percent <- FlowModPercent(conn = conn, path.to.data =  path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)

  plot <- ggplot2::ggplot(percent, aes(x = Park, y = Percent, fill = FlowModificationStatus))+
    geom_bar(stat = "identity")
    
  return(plot)
}


# Table with count and percent of springs with human use and livestock disturbance
DisturbanceCount <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  disturbance <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Disturbance")
  flowmod <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "DisturbanceFlowModification")
  
  site <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Site")
  
  sampleframe <- site %>%
    select(Park, SiteCode, SiteName, GRTSOrder, SiteStatus, SampleFrame) %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr") & SiteStatus == "T-S") %>%
    dplyr::select(-c("GRTSOrder", "SiteStatus", "SampleFrame")) %>%
    unique()
  
  disturb <- sampleframe %>%
    dplyr::left_join(disturbance, by = c("Park", "SiteCode", "SiteName")) %>%
    dplyr::filter(VisitType %in% c("Primary", NA)) %>%
    dplyr::select(-c(VisitType, DPL, Notes)) %>%
    unique() %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, HumanUse, Livestock) %>%
    dplyr::mutate(HumanUse = ifelse(HumanUse > 0, 1, 0)) %>%
    dplyr::mutate(Livestock = ifelse(Livestock > 0, 1, 0)) %>%
    dplyr::group_by(Park, SiteCode, SiteName) %>%
    dplyr::summarize(Livestock = sum(Livestock), HumanUse = sum(HumanUse)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(HumanUse = ifelse(HumanUse > 0, 1, 0)) %>%
    dplyr::mutate(Livestock = ifelse(Livestock > 0, 1, 0)) %>%
    dplyr::group_by(Park) %>%
    dplyr::summarize(LivestockCount = sum(Livestock), HumanUseCount = sum(HumanUse), Total = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(LivestockPercent = (LivestockCount/Total)*100, HumanUsePercent = (HumanUseCount/Total)*100)
  
  return(disturb)
}


# Bar plot with percent of springs with human use
HumanUsePlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  disturb <- DisturbanceCount(conn = conn, path.to.data =  path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  
  plot <- ggplot2::ggplot(disturb, aes(x = Park, y = HumanUsePercent))+
    geom_bar(stat = "identity") +
    scale_y_continuous(limits = c(0, 100))
  
  return(humanplot)
}


# Bar plot with percent of springs with livestock disturbance
LivestockPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  disturb <- DisturbanceCount(conn = conn, path.to.data =  path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  
  plot <- ggplot2::ggplot(disturb, aes(x = Park, y = LivestockPercent))+
    geom_bar(stat = "identity") +
    scale_y_continuous(limits = c(0, 100))
  
  return(livestockplot)
}


# Table with human use observations
HumanUseObservations <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  disturbance <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Disturbance")
  
  humanobs <- disturbance %>%
    dplyr::filter(Livestock > 0) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, Livestock, Notes)
  
  return(humanobs)
}


# Function NYI: Map of human use observations
HumanUseMap <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  humanobs <- HumanUseObservations(conn = conn, path.to.data =  path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  
  return(humanmap)
}


# Table with livestock observations
LivestockObservations <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  disturbance <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Disturbance")

  livestockobs <- disturbance %>%
    dplyr::filter(Livestock > 0) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, Livestock, Notes)
  
  return(livestockobs)
}


# Function NYI: Map of livestock observations
LivestockMap <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  livestockobs <- HumanUseObservations(conn = conn, path.to.data =  path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  
  return(livestockmap)
}


# Function NYI: Table with disturbance rankings 
DisturbanceRank <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  disturbance <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Disturbance")
  
  
  return(disturbrank)
}


# Function NYI: Box plot with disturbance rankings
DisturbanceRankPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  disturbance <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Disturbance")
  
  return(disturbrankplot)
}