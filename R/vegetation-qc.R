# Vegetation observed, no lifeform present
qcVegPresentNoLifeforms <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  veg <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Riparian")
  
  vegnolife <- veg %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::filter(IsVegetationObserved == "Y" & is.na(LifeForm)) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, IsVegetationObserved, LifeForm) %>%
    dplyr::arrange(FieldSeason, Park, SiteCode)
  
  return(vegnolife)
}


# No vegetation observed, lifeform present
qcVegPresentNoLifeforms <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  veg <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Riparian")
  
  noveglife <- veg %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::filter(IsVegetationObserved == "N" & !is.na(LifeForm)) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, IsVegetationObserved, LifeForm) %>%
    dplyr::arrange(FieldSeason, Park, SiteCode)
  
  return(noveglife)
}


# Lifeform present, no rank
qcLifeformPresentNoRank <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  veg <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Riparian")
  
  lifenorank <- veg %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::filter(!is.na(LifeForm) & is.na(Rank)) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, LifeForm, Rank) %>%
    dplyr::arrange(FieldSeason, Park, SiteCode)
  
  return(lifenorank)
}


# If multiple lifeforms have the same rank, then rank gaps have been properly entered
qcLifeformRankCheck <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  veg <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Riparian")
  
  rankcheck <- veg %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, LifeForm, Rank) %>%
    dplyr::group_by(Park, SiteCode, SiteName, VisitDate, FieldSeason, Rank) %>%
    dplyr::mutate(Count = n()) %>%
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


# Function NYI: Table with summary of lifeform presence and rank

# Function NYI: Bar plot and/or box plot with summary of lifeform presence and rank

# Function NYI: Table of tamarisk, fountain grass, date palm, and fan palm (non-JOTR) observations
InvasivePlants <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  invasives <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Invasives")
  
  targetinvasives <- invasives %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, USDAPlantsCode, ScientificName, InRiparianVegBuffer, Notes) %>%
    dplyr::filter(USDAPlantsCode %in% c("PESE3", "PHDA4", "TARA", "WAFI"))
  
  return(targetinvasives)
}


# Function NYI: Map of tamarisk, fountain grass, date palm, and fan palm (non-JOTR) observations