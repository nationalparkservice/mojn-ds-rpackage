#' Wrangle AGOL data into a set of dataframes structured for use in this package.
#'
#' @param agol_layers The list of tibbles returned by `FetchAGOLLayers()`
#'
#' @return A list of tibbles
#' 
WrangleAGOLData <- function(agol_layers) {
  data <- list()
  
  # Clean up visit table and replace numeric keys with meaningful values
  visit <- agol_layers$visit %>%
    dplyr::select(-SiteCodeText) %>%
    dplyr::rename(VisitTypeCode = VisitType,
                  SpringTypeCode = SpringType,
                  GPSCode = GPS,
                  CameraCode = Camera,
                  CameraCardCode = CameraCard,
                  SensorTypeDepCode = SensorTypeDep,
                  FlowConditionCode = FlowCondition,
                  WQInstrumentCode = WQInstrument,
                  pHInstrumentCode = pHInstrument,
                  DOInstrumentCode = DOInstrument,
                  SpCondInstrumentCode = SpCondInstrument,
                  TemperatureInstrumentCode = TemperatureInstrument,
                  RoadsCode = Roads,
                  HumanUseCode = HumanUse,
                  PlantManagementCode = PlantManagement,
                  HikingTrailsCode = HikingTrails,
                  LivestockCode = Livestock,
                  FireCode = Fire,
                  FloodingCode = Flooding,
                  WildlifeCode = Wildlife,
                  OverallCode = Overall) %>%
    dplyr::mutate(EstimatedDischarge_L_per_sec = as.integer(EstimatedDischarge_L_per_sec)) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_Site, name, SiteName = sitename, GRTSPanelID), by = c("SiteCode"= "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_GRTSPanel, ID, SampleFrame = name), by = c("GRTSPanelID" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_VisitType, name, VisitType = label), by = c("VisitTypeCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_MonitoringStatus, name, MonitoringStatus = label), by = c("Status" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_SpringType, name, SpringType = label), by = c("SpringTypeCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_GPSUnit, name, GPS = label), by = c("GPSCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_Camera, name, Camera = label), by = c("CameraCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_CameraCard, name, CameraCard = label), by = c("CameraCardCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_FlowCondition, name, FlowCondition = label), by = c("FlowConditionCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_DS_SensorModel, name, SensorTypeDep = label), by = c("SensorTypeDepCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_DS_Sensor, name, SensorDep = label), by = c("SensorIDDep" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_SpringbrookLengthFlag, name, SpringbrookLengthFlag = label), by = c("SPBKLength" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_SpringbrookLengthFlag, name, DiscontinuousSpringbrookLengthFlag = label), by = c("DiscontinuousSPBKLength" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DischargeEstimatedClass, name, DischargeClass_L_per_s = label), by = c("EstimatedDischarge_L_per_sec" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_WaterQualityDataCollected, name, WQDataCollected = label), by = c("WasWaterQualityDataCollected" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, name, WQInstrument = label), by = c("WQInstrumentCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, name, pHInstrument = label), by = c("pHInstrumentCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, name, DOInstrument = label), by = c("DOInstrumentCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, name, SpCondInstrument = label), by = c("SpCondInstrumentCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, name, TemperatureInstrument = label), by = c("TemperatureInstrumentCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_FlowModificationStatus, name, FlowModificationStatus = label), by = c("FlowModification" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, Roads = label), by = c("RoadsCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, HumanUse = label), by = c("HumanUseCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, PlantManagement = label), by = c("PlantManagementCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, HikingTrails = label), by = c("HikingTrailsCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, Livestock = label), by = c("LivestockCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, OtherAnthropogenic = label), by = c("Other_Anthro" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, Fire = label), by = c("FireCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, Flooding = label), by = c("FloodingCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, Wildlife = label), by = c("WildlifeCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, OtherNatural = label), by = c("Other_Natural" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, Overall = label), by = c("OverallCode" = "name")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_Shared_YesNo, name, IsWildlifeObserved = label), by = c("Waswildlifeobserved" = "name")) %>%
    dplyr::mutate(VisitDate = lubridate::as_date(DateTime),
                  FieldSeason = ifelse(lubridate::month(VisitDate) <= 10, lubridate::year(VisitDate), lubridate::year(VisitDate) + 1)) %>%
    dplyr::rename(IsSensorRetrieved = SensorRetrieved,
                  IsVegetationObserved = WasRiparianVegetationObserved,
                  InvasivesObserved = WereInvasivesObserved,
                  pH_DataQualityFlag = pH_Flag,
                  Temp_C_DataQualityFlag = Temp_C_Flag,
                  SpCond_microS_DataQualityFlag = SpCond_microS_Flag,
                  DO_DataQualityFlag = DO_Flag,
                  GPSUnit = GPS,
                  visitglobalid = globalid,
                  SpringbrookType = SPBKType,
                  SpringbrookLength_m = Length_m,
                  SpringbrookWidth_m = Width_m,
                  DiscontinuousSpringbrookLength_m = DiscontinuousLength_m,
                  EstimatedCapture_percent = EstimatedCapture_Percent,
                  Notes = SpringComments,
                  Protocol = ProtocolID,
                  DPL = DataProcessingLevel)
  
  # ----- Calibration data -----
  # TODO
  
  # ----- Photos -----
  # TODO
  rep_photos_int <- agol_layers$repeats_int %>%
    dplyr::select(repphotoglobalid = parentglobalid) %>%
    dplyr::mutate(OriginalFilePath = "TBD",
                  RenamedFilePath = "TBD")
  
  rep_photos_ext <- agol_layers$repeats_ext %>%
    dplyr::select(repphotoglobalid = parentglobalid) %>%
    dplyr::mutate(OriginalFilePath = "TBD",
                  RenamedFilePath = "TBD")
  
  rep_photo_files <- dplyr::bind_rows(rep_photos_ext, rep_photos_int) %>%
    dplyr::mutate(IsLibraryPhoto = "TBD")
  
  rep_photos <- agol_layers$repeats %>%
    dplyr::select(repphotoglobalid = globalid, visitglobalid = parentglobalid, PhotoType, Notes = PhotoNotes_ExternalCamera) %>%
    dplyr::mutate(UtmX_m = 0, UtmY_m = 0,
                  PhotoSOP = "RPT") %>%
    dplyr::left_join(rep_photo_files, by ="repphotoglobalid")
  
  data$Photos <- visit %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, MonitoringStatus, SpringType, SampleFrame, DPL, Camera, CameraCard, GPSUnit, visitglobalid) %>%
    dplyr::mutate(DateTaken = VisitDate) %>%
    dplyr::inner_join(rep_photos, by = "visitglobalid") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, MonitoringStatus, SpringType, SampleFrame, DPL, Camera, CameraCard, DateTaken, 
                  PhotoType, IsLibraryPhoto, OriginalFilePath, RenamedFilePath,GPSUnit, UtmX_m, UtmY_m, Notes, PhotoSOP)
  
  # ----- DischargeEstimated -----
  data$DischargeEstimated <- visit %>%
    dplyr::filter(DischargeMethod == "EST") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, FlowCondition, DischargeClass_L_per_s, VisitType, DPL)
  
  # ----- DischargeFlowCondition -----
  data$DischargeFlowCondition <- visit %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, FlowCondition, SpringbrookType, SpringbrookLengthFlag, SpringbrookLength_m, SpringbrookWidth_m, DiscontinuousSpringbrookLengthFlag, DiscontinuousSpringbrookLength_m, VisitType, DPL, Notes, Notes, SpringbrookNotes = DischargeNotes)
  
  # ----- DischargeVolumetric -----
  vol <- agol_layers$fill_time %>%
    dplyr::select(visitglobalid = parentglobalid, FillTime_seconds = FillTime_sec)
  
  data$DischargeVolumetric <- visit %>%
    dplyr::filter(DischargeMethod == "VOL") %>%
    dplyr::left_join(vol, by = "visitglobalid") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, FlowCondition, ContainerVolume_mL, EstimatedCapture_percent, VisitType, DPL)
  
  # ----- Disturbance -----
  data$Disturbance <- visit %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, Roads, HumanUse, PlantManagement, HikingTrails, Livestock, OtherAnthropogenic, Fire, Flooding, Wildlife, OtherNatural, Overall, FlowModificationStatus, VisitType, DPL)
  
  # ----- DisturbanceFlowModification -----
  flow_mod <- agol_layers$disturbance_flow_mod %>%
    dplyr::select(visitglobalid = parentglobalid, ModificationType)
  
  data$DisturbanceFlowModification <- visit %>%
    dplyr::left_join(flow_mod, by = "visitglobalid") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, FlowModificationStatus, ModificationType, VisitType, DPL)
  
  # ----- Invasives -----
  invasives <- agol_layers$invasives %>%
    dplyr::select(visitglobalid = parentglobalid, InvasiveSpecies, RiparianVegBuffer) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_DS_Taxon, name, ScientificName = scientificname), by = c("InvasiveSpecies" = "name")) %>%
    dplyr::select(visitglobalid, InRiparianVegBuffer = RiparianVegBuffer, USDAPlantsCode = InvasiveSpecies, ScientificName)
  
  data$Invasives <- visit %>%
    dplyr::left_join(invasives, by = "visitglobalid") %>%
    dplyr::left_join(agol_layers$MOJN_Ref_DS_ParkTaxonProtectedStatus, by = c("USDAPlantsCode" = "Taxon", "Park" = "parkname")) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, InvasivesObserved, InRiparianVegBuffer, USDAPlantsCode, ScientificName, VisitType, ProtectedStatus = ProtectedStatusCode, DPL, Notes = InvasiveNotes)
  
  
  # ----- Riparian -----
  riparian <- agol_layers$riparian_veg %>%
    dplyr::select(visitglobalid = parentglobalid, LifeForm = lifeformname, DominantSpecies)
  
  riparian_visit <- visit %>%
    dplyr::select(visitglobalid, Park, SiteCode, SiteName, VisitDate, FieldSeason, IsVegetationObserved, MistletoePresent, `Woody >4m` = WoodyGT4m, `Woody 2-4m` = Woody2to4m, `Woody <2m` = WoodyLT2m, Forb, Rush, Grass, Reed, Sedge, Cattail, Bryophyte, `Non-Plant` = NonPlant, VisitType, DPL, Notes = RiparianVegetationNotes) %>%
    tidyr::pivot_longer(c(`Woody >4m`, `Woody 2-4m`, `Woody <2m`, Forb, Rush, Grass, Reed, Sedge, Cattail, Bryophyte, `Non-Plant`), names_to = "LifeForm", values_to = "Rank") %>%
    dplyr::filter(Rank != 12)
  
  data$Riparian <- riparian_visit %>%
    dplyr::left_join(riparian, by = c("visitglobalid", "LifeForm")) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, IsVegetationObserved, MistletoePresent, LifeForm, Rank, DominantSpecies, VisitType, DPL, Notes)
    
    
  # ----- SensorRetrievalAttempts -----
  sensor_retrieval <- agol_layers$sensor_retrieval %>%
    dplyr::select(visitglobalid = parentglobalid, RetrievalTime, DownloadSuccessful, UploadSuccessful, Notes = SensorRetrieveNotes, SensorProblem, SensorIDRet)
  
  sensor_deployment <- visit %>%
    dplyr::filter(IsSensorSpring == "Y") %>%
    dplyr::select(visitglobalid, DeploymentFieldSeason = FieldSeason, DeploymentDate = VisitDate, SiteCode, SensorDeployed, SensorDep, DeploymentTime, SensorDeployNote)
  
  data$SensorRetrievalAttempts <- visit %>%
    dplyr::filter(IsSensorSpring == "Y") %>%
    dplyr::select(visitglobalid, RetrievalFieldSeason = FieldSeason, SiteName, SiteCode, Park, RetrievalDate = VisitDate, SensorRetrieved = IsSensorRetrieved) %>%
    dplyr::left_join(sensor_retrieval, by = "visitglobalid") %>%
    dplyr::left_join(sensor_deployment, by = c("SiteCode", "SensorIDRet" = "SensorDep")) %>%
    dplyr::filter(DeploymentDate < RetrievalDate) %>%
    dplyr::mutate(time_since_dep = RetrievalDate - DeploymentDate) %>%
    dplyr::group_by(SensorIDRet, time_since_dep) %>%
    dplyr::filter(time_since_dep == min(time_since_dep)) %>%
    dplyr::ungroup()
  
  # ----- SensorsCurrentlyDeployed -----
  
  # ----- SensorsAllDeployments -----
  
  # ----- Site -----
  # TODO
  
  # ----- Visit -----
  data$Visit <- visit %>%
    dplyr::select(Park,
                  SiteCode,
                  SiteName,
                  VisitDate,
                  FieldSeason,
                  SampleFrame,
                  VisitType,
                  MonitoringStatus,
                  SpringType,
                  Notes = SpringComments,
                  DPL
    )
  
  # ----- VisitActivity -----
  # data$VisitActivity <- data$Visit %>%
  
  
  # ----- WaterQualityDO -----
  
  # ----- WaterQualitypH -----
  
  # ----- WaterQualitySpCond -----
  
  # ----- WaterQualityTemperature -----
  
  # ----- Wildlife -----
  
  return(data)
}


#' Read data from the Desert Springs AGOL feature layers.
#'
#' @param data_path URL to Desert Springs feature service on AGOL.
#' @param lookup_path URL to feature service on AGOL containing Desert Springs lookup tables.
#' @param agol_username Username of headless AGOL account with permissions to view the feature service.
#' @param agol_password Password for headless AGOL account.
#'
#' @return A list of tibbles
#'
FetchAGOLLayers <- function(data_path, lookup_path, agol_username, agol_password) {
  # Get a token with a headless account
  token_resp <- httr::POST("https://nps.maps.arcgis.com/sharing/rest/generateToken",
                           body = list(username = agol_username,
                                       password = agol_password,
                                       referer = 'https://irma.nps.gov',
                                       f = 'json'),
                           encode = "form")
  agol_token <- jsonlite::fromJSON(httr::content(token_resp, type="text", encoding = "UTF-8"))
  
  agol_layers <- list()
  
  # Fetch lookup tables from lookup feature service
  lookup_names <- httr::GET(paste0(lookup_path, "/layers"),
                            query = list(where="1=1",
                                         outFields="*",
                                         f="JSON",
                                         token=agol_token$token))
  lookup_names <- jsonlite::fromJSON(httr::content(lookup_names, type = "text", encoding = "UTF-8"))
  lookup_names <- lookup_names$tables %>%
    dplyr::select(id, name) %>%
    dplyr::filter(grepl("MOJN_(Lookup|Ref)(_Lookup|_Ref)?_(DS|Shared)", name))  # (_Lookup|_Ref)? is to accommodate weirdly named Camera lookup - can be removed once fixed in AGOL
  
  lookup_layers <- lapply(lookup_names$id, function(id) {
    df <- httr::GET(paste0(lookup_path, "/", id, "/query"),
                    query = list(where="1=1",
                                 outFields="*",
                                 f="JSON",
                                 token=agol_token$token))
    df <- jsonlite::fromJSON(httr::content(df, type = "text", encoding = "UTF-8"))
    df <- df$features$attributes %>%
      tibble::as_tibble()
    return(df)
  })
  names(lookup_layers) <- lookup_names$name
  
  # Fetch each layer in the DS feature service
  
  # ----- MOJN_DS_SpringVisit - visit-level data -----
  visit <- httr::GET(paste0(data_path, "/0/query"),
                     query = list(where="1=1",
                                  outFields="*",
                                  f="JSON",
                                  token=agol_token$token))
  
  visit <- jsonlite::fromJSON(httr::content(visit, type = "text", encoding = "UTF-8"))
  agol_layers$visit <- visit$features$attributes %>%
    tibble::as_tibble() %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles")) %>%
    dplyr::mutate(DateTime = as.POSIXct(DateTime/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- Repeats - repeat photos -----
  repeats <- httr::GET(paste0(data_path, "/1/query"),
                       query = list(where="1=1",
                                    outFields="*",
                                    f="JSON",
                                    token=agol_token$token))
  
  repeats <- jsonlite::fromJSON(httr::content(repeats, type = "text", encoding = "UTF-8"))
  agol_layers$repeats <- cbind(repeats$features$attributes, repeats$features$geometry) %>%
    dplyr::mutate(wkid = repeats$spatialReference$wkid) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- InvasivePlants - invasive plant data -----
  invasives <- httr::GET(paste0(data_path, "/2/query"),
                         query = list(where="1=1",
                                      outFields="*",
                                      f="JSON",
                                      token=agol_token$token))
  
  invasives <- jsonlite::fromJSON(httr::content(invasives, type = "text", encoding = "UTF-8"))
  agol_layers$invasives <- cbind(invasives$features$attributes, invasives$features$geometry) %>%
    dplyr::mutate(wkid = invasives$spatialReference$wkid) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- Observers -----
  observers <- httr::GET(paste0(data_path, "/3/query"),
                         query = list(where="1=1",
                                      outFields="*",
                                      f="JSON",
                                      token=agol_token$token))
  
  observers <- jsonlite::fromJSON(httr::content(observers, type = "text", encoding = "UTF-8"))
  agol_layers$observers <- observers$features$attributes %>%
    tibble::as_tibble() %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- SensorRetrieval -----
  sensor_retrieval <- httr::GET(paste0(data_path, "/4/query"),
                                query = list(where="1=1",
                                             outFields="*",
                                             f="JSON",
                                             token=agol_token$token))
  
  sensor_retrieval <- jsonlite::fromJSON(httr::content(sensor_retrieval, type = "text", encoding = "UTF-8"))
  agol_layers$sensor_retrieval <- sensor_retrieval$features$attributes %>%
    tibble::as_tibble() %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- RepeatPhotos_Internal - repeat photos taken on internal device camera -----
  repeats_int <- httr::GET(paste0(data_path, "/5/query"),
                           query = list(where="1=1",
                                        outFields="*",
                                        f="JSON",
                                        token=agol_token$token))
  
  repeats_int <- jsonlite::fromJSON(httr::content(repeats_int, type = "text", encoding = "UTF-8"))
  agol_layers$repeats_int <- repeats_int$features$attributes %>%
    tibble::as_tibble()
  
  # ----- RepeatPhotos_External - repeat photos taken on external camera -----
  repeats_ext <- httr::GET(paste0(data_path, "/6/query"),
                           query = list(where="1=1",
                                        outFields="*",
                                        f="JSON",
                                        token=agol_token$token))
  
  repeats_ext <- jsonlite::fromJSON(httr::content(repeats_ext, type = "text", encoding = "UTF-8"))
  agol_layers$repeats_ext <- repeats_ext$features$attributes %>%
    tibble::as_tibble()
  
  # ----- FillTime - volumetric discharge fill time -----
  fill_time <- httr::GET(paste0(data_path, "/7/query"),
                         query = list(where="1=1",
                                      outFields="*",
                                      f="JSON",
                                      token=agol_token$token))
  
  fill_time <- jsonlite::fromJSON(httr::content(fill_time, type = "text", encoding = "UTF-8"))
  agol_layers$fill_time <- fill_time$features$attributes %>%
    tibble::as_tibble() %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- FlowModTypes - flow modifications observed -----
  disturbance_flow_mod <- httr::GET(paste0(data_path, "/8/query"),
                                    query = list(where="1=1",
                                                 outFields="*",
                                                 f="JSON",
                                                 token=agol_token$token))
  
  disturbance_flow_mod <- jsonlite::fromJSON(httr::content(disturbance_flow_mod, type = "text", encoding = "UTF-8"))
  agol_layers$disturbance_flow_mod <- disturbance_flow_mod$features$attributes %>%
    tibble::as_tibble() %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- WildlifeRepeat - wildlife observations -----
  wildlife <- httr::GET(paste0(data_path, "/9/query"),
                        query = list(where="1=1",
                                     outFields="*",
                                     f="JSON",
                                     token=agol_token$token))
  
  wildlife <- jsonlite::fromJSON(httr::content(wildlife, type = "text", encoding = "UTF-8"))
  agol_layers$wildlife <- wildlife$features$attributes %>%
    tibble::as_tibble() %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- VegImageRepeat - riparian veg photo data -----
  riparian_veg  <- httr::GET(paste0(data_path, "/10/query"),
                             query = list(where="1=1",
                                          outFields="*",
                                          f="JSON",
                                          token=agol_token$token))
  
  riparian_veg  <- jsonlite::fromJSON(httr::content(riparian_veg, type = "text", encoding = "UTF-8"))
  agol_layers$riparian_veg  <- riparian_veg$features$attributes %>%
    tibble::as_tibble() %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- InternalCamera - riparian veg photos taken on internal device camera -----
  riparian_veg_int <- httr::GET(paste0(data_path, "/11/query"),
                                query = list(where="1=1",
                                             outFields="*",
                                             f="JSON",
                                             token=agol_token$token))
  
  riparian_veg_int <- jsonlite::fromJSON(httr::content(riparian_veg_int, type = "text", encoding = "UTF-8"))
  agol_layers$riparian_veg_int <- riparian_veg_int$features$attributes %>%
    tibble::as_tibble()
  
  # ----- ExternalCameraFiles - riparian veg photos taken on external camera -----
  riparian_veg_ext <- httr::GET(paste0(data_path, "/12/query"),
                                query = list(where="1=1",
                                             outFields="*",
                                             f="JSON",
                                             token=agol_token$token))
  
  riparian_veg_ext <- jsonlite::fromJSON(httr::content(riparian_veg_ext, type = "text", encoding = "UTF-8"))
  agol_layers$riparian_veg_ext <- riparian_veg_ext$features$attributes %>%
    tibble::as_tibble()
  
  # ----- InvImageRepeat - invasive veg photos taken on internal device camera -----
  invasives_int <- httr::GET(paste0(data_path, "/13/query"),
                             query = list(where="1=1",
                                          outFields="*",
                                          f="JSON",
                                          token=agol_token$token))
  
  invasives_int <- jsonlite::fromJSON(httr::content(invasives_int, type = "text", encoding = "UTF-8"))
  agol_layers$invasives_int <- invasives_int$features$attributes %>%
    tibble::as_tibble()
  
  # ----- ExternalCameraFilesInv - invasive veg photos taken on external camera -----
  invasives_ext  <- httr::GET(paste0(data_path, "/14/query"),
                              query = list(where="1=1",
                                           outFields="*",
                                           f="JSON",
                                           token=agol_token$token))
  
  invasives_ext  <- jsonlite::fromJSON(httr::content(invasives_ext , type = "text", encoding = "UTF-8"))
  agol_layers$invasives_ext  <- invasives_ext $features$attributes %>%
    tibble::as_tibble()
  
  # ----- AdditionalPhotos2 - info about additional photos -----
  additional_photos<- httr::GET(paste0(data_path, "/15/query"),
                                query = list(where="1=1",
                                             outFields="*",
                                             f="JSON",
                                             token=agol_token$token))
  
  additional_photos <- jsonlite::fromJSON(httr::content(additional_photos, type = "text", encoding = "UTF-8"))
  agol_layers$additional_photos <- additional_photos$features$attributes %>%
    tibble::as_tibble()
  
  # ----- AdditionalPhotoInternal - additional photos taken on internal camera -----
  additional_photos_int<- httr::GET(paste0(data_path, "/16/query"),
                                    query = list(where="1=1",
                                                 outFields="*",
                                                 f="JSON",
                                                 token=agol_token$token))
  
  additional_photos_int <- jsonlite::fromJSON(httr::content(additional_photos_int, type = "text", encoding = "UTF-8"))
  agol_layers$additional_photos_int <- additional_photos_int$features$attributes %>%
    tibble::as_tibble()
  
  # ----- AddtionalPhotoExternal - additional photos taken on external camera -----
  additional_photos_ext<- httr::GET(paste0(data_path, "/17/query"),
                                    query = list(where="1=1",
                                                 outFields="*",
                                                 f="JSON",
                                                 token=agol_token$token))
  
  additional_photos_ext <- jsonlite::fromJSON(httr::content(additional_photos_ext, type = "text", encoding = "UTF-8"))
  agol_layers$additional_photos_ext <- additional_photos_ext$features$attributes %>%
    tibble::as_tibble()
  
  agol_layers <- c(agol_layers, lookup_layers)
  
  agol_layers <- lapply(agol_layers, function(data_table) {
    data_table %>% dplyr::mutate(dplyr::across(where(is.character), function(x) {
      x %>%
        utf8::utf8_encode() %>%  # Encode text as UTF-8 - this prevents a lot of parsing issues in R
        trimws(whitespace = "[ \\t\\r\\n\\h\\v]") %>%  # Trim leading and trailing whitespace
        dplyr::na_if("")  # Replace empty strings with NA
    }))
    col_names <- names(data_table)
    name_and_label <- grepl("(name)|(label)", col_names, ignore.case = TRUE)
    names(data_table)[name_and_label] <- tolower(names(data_table[name_and_label]))
    
    return(data_table)
  })
  
  return(agol_layers)
}