# Function NYI: Wildlife observed, no wildlife types
qcOverallDisturbance <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  wildlife <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Wildlife")
  
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

# Function NYI: Wildlife type specified, no observations

# Function NYI: Table with ungulate observations

# Function NYI: Map of ungulate observations