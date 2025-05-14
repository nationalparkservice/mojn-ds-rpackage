#' View sensors with unknown ID/serial number
#'
#' @return Dataframe of sensors omitted from data due to missing serial numbers
#' @export
#'
unk_sensors <- function() {
  get("unk_sensors", envir = pkg_globals)
}

#' Wrangle AGOL data into a set of dataframes structured for use in this package.
#'
#' @return A list of tibbles
#' 
WrangleAGOLData <- function() {
  
  agol_layers <- FetchAGOLLayers()
  data <- list()
  
  # Clean up visit table and replace numeric keys with meaningful values
  visit <- agol_layers$visit |>
    dplyr::select(-SiteCodeText) |>
    dplyr::rename(VisitTypeCode = VisitType,
                  SpringTypeCode = SpringType,
                  SecondarySpringTypeCode = SecondarySpringType,
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
                  OverallCode = Overall) |>
    dplyr::mutate(EstimatedDischarge_L_per_sec = as.integer(EstimatedDischarge_L_per_sec)) |>
    dplyr::left_join(dplyr::select(agol_layers$sites, name, SiteName = sitename, SiteCode, SampleFrame, PanelCode = PanelGroup), by = c("SiteCode" = "SiteCode")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_Panel, name, Panel = label), by = c("PanelCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_VisitType, name, VisitType = label), by = c("VisitTypeCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_MonitoringStatus, name, MonitoringStatus = label), by = c("Status" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_SpringType, name, SpringType = label), by = c("SpringTypeCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_SpringType, name, SecondarySpringType = label), by = c("SecondarySpringTypeCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_GPSUnit, name, GPS = label), by = c("GPSCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_Camera, name, Camera = label), by = c("CameraCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_CameraCard, name, CameraCard = label), by = c("CameraCardCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_FlowCondition, name, FlowCondition = label), by = c("FlowConditionCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_DS_SensorModel, name, SensorTypeDep = label), by = c("SensorTypeDepCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_DS_Sensor, name, SensorDep = label), by = c("SensorIDDep" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_SpringbrookLengthFlag, name, SpringbrookLengthFlag = label), by = c("SPBKLength" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_SpringbrookLengthFlag, name, DiscontinuousSpringbrookLengthFlag = label), by = c("DiscontinuousSPBKLength" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DischargeEstimatedClass, name, DischargeClass_L_per_s = label), by = c("EstimatedDischarge_L_per_sec" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_WaterQualityDataCollected, name, WQDataCollected = label), by = c("WasWaterQualityDataCollected" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, name, WQInstrument = label), by = c("WQInstrumentCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, name, pHInstrument = label), by = c("pHInstrumentCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, name, DOInstrument = label), by = c("DOInstrumentCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, name, SpCondInstrument = label), by = c("SpCondInstrumentCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, name, TemperatureInstrument = label), by = c("TemperatureInstrumentCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_FlowModificationStatus, name, FlowModificationStatus = label), by = c("FlowModification" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, Roads = label), by = c("RoadsCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, HumanUse = label), by = c("HumanUseCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, PlantManagement = label), by = c("PlantManagementCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, HikingTrails = label), by = c("HikingTrailsCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, Livestock = label), by = c("LivestockCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, OtherAnthropogenic = label), by = c("Other_Anthro" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, Fire = label), by = c("FireCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, Flooding = label), by = c("FloodingCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, Wildlife = label), by = c("WildlifeCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, OtherNatural = label), by = c("Other_Natural" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_DisturbanceClass, name, Overall = label), by = c("OverallCode" = "name")) |>
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_Shared_YesNo, name, IsWildlifeObserved = label), by = c("Waswildlifeobserved" = "name")) |>
    dplyr::mutate(VisitDate = lubridate::as_date(DateTime),
                  FieldSeason = ifelse(lubridate::month(VisitDate) < 10, lubridate::year(VisitDate), lubridate::year(VisitDate) + 1)) |>
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
  
  # ----- CalibrationDO -----
  data$CalibrationDO <- visit %>%
    dplyr::filter(grepl("DO",ParametersCollected)) %>%
    dplyr::inner_join(agol_layers$CalibrationDO, by = "DOUniqueID") %>%
    dplyr::mutate(StartTime = format(as.POSIXct(DateTime), format = "%H:%M:%S")) %>%
    dplyr::mutate(CalibrationTime = format(as.POSIXct(DateTime), format = "%H:%M:%S")) %>%
    dplyr::mutate(CalibrationDate = lubridate::as_date(CalibrationDate.y)) %>%
    dplyr::left_join(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, by = c("DOInstrumentID" = "name")) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, StartTime, FieldSeason, VisitType, CalibrationDate, 
                  CalibrationTime, DOInstrument = label, BarometricPressure_mmHg, PreCalibrationReading_percent, 
                  PreCalibrationTemperature_C, PostCalibrationReading_percent, PostCalibrationTemperature_C,
                  Notes = Notes.y)
  
  # ----- CalibrationpH -----
  data$CalibrationpH <- visit %>%
    dplyr::filter(grepl("pH",ParametersCollected)) %>%
    dplyr::select(visitglobalid, pHUniqueID_7, pHUniqueID_10, pHUniqueID_4) %>%
    tidyr::pivot_longer(cols=dplyr::starts_with("pHUniqueID_"),
                        values_to = "pHUniqueID", names_to = NULL) %>%
    dplyr::left_join(visit, by = "visitglobalid") %>%
    dplyr::mutate(StartTime = format(as.POSIXct(DateTime), format = "%H:%M:%S")) %>%
    dplyr::inner_join(agol_layers$CalibrationpH, by = "pHUniqueID") %>%
    dplyr::mutate(StartTime = format(as.POSIXct(DateTime), format = "%H:%M:%S")) %>%
    dplyr::mutate(CalibrationTime = format(as.POSIXct(DateTime), format = "%H:%M:%S")) %>%
    dplyr::mutate(CalibrationDate = lubridate::as_date(CalibrationDate.y)) %>%
    dplyr::left_join(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, by = c("pHInstrumentID" = "name")) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, StartTime, FieldSeason, VisitType, CalibrationDate, 
                  CalibrationTime, pHInstrument = label, StandardValue_pH, TemperatureCorrectedStd_pH,
                  PreCalibrationReading_pH, PreCalibrationTemperature_C, PostCalibrationReading_pH, 
                  PostCalibrationTemperature_C, Notes = Notes.y, pHUniqueID)
  
  # ----- CalibrationSpCond -----
  data$CalibrationSpCond <- visit %>%
    dplyr::filter(grepl("SpCond",ParametersCollected)) %>%
    dplyr::inner_join(agol_layers$CalibrationSpCond, by = "SpCondUniqueID") %>%
    dplyr::mutate(StartTime = format(as.POSIXct(DateTime), format = "%H:%M:%S")) %>%
    dplyr::mutate(CalibrationTime = format(as.POSIXct(DateTime), format = "%H:%M:%S")) %>%
    dplyr::mutate(CalibrationDate = lubridate::as_date(CalibrationDate.y)) %>%
    dplyr::left_join(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, by = c("SpCondInstrumentID" = "name")) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, StartTime, FieldSeason, VisitType, CalibrationDate, 
                  CalibrationTime, SpCondInstrument = label, StandardValue_microS_per_cm, 
                  PreCalibrationReading_microS_per_cm = PreCalibrationReading_microS_pe,
                  PostCalibrationReading_microS_per_cm = PostCalibrationReading_microS_p, Notes = Notes.y)
  
  # ----- Photos -----
  rep_photos_int <- agol_layers$repeats_int %>%
    dplyr::select(repphotoglobalid = parentglobalid, OriginalFilePath, renamedfilepath)
  
  rep_photos_ext <- agol_layers$repeats_ext %>%
    dplyr::select(repphotoglobalid = parentglobalid, OriginalFilePath, renamedfilepath)
  
  rep_photo_files <- dplyr::bind_rows(rep_photos_ext, rep_photos_int) %>%
    dplyr::mutate(IsLibraryPhoto = "TBD")
  
  rep_photos <- agol_layers$repeats %>%
    dplyr::select(repphotoglobalid = globalid, visitglobalid = parentglobalid, PhotoType, Notes = PhotoNotes_ExternalCamera) %>%
    dplyr::mutate(UtmX_m = 0, UtmY_m = 0,
                  PhotoSOP = "RPT") %>%
    dplyr::left_join(rep_photo_files, by ="repphotoglobalid")
  
  veg_photos_int <- agol_layers$riparian_veg_int %>%
    dplyr::select(vegphotoglobalid = parentglobalid, OriginalFilePath, renamedfilepath)
  
  veg_photos_ext <- agol_layers$riparian_veg_ext %>%
    dplyr::select(vegphotoglobalid = parentglobalid, OriginalFilePath, renamedfilepath)
  
  veg_photo_files <- dplyr::bind_rows(veg_photos_ext, veg_photos_int) %>%
    dplyr::mutate(IsLibraryPhoto = "TBD")
   
  veg_photos <- agol_layers$riparian_veg %>%
    dplyr::select(vegphotoglobalid = globalid, visitglobalid = parentglobalid, PhotoType = LifeForm) %>%
    dplyr::mutate(UtmX_m = 0, UtmY_m = 0,
                  PhotoSOP = "RVG") %>%
    dplyr::left_join(veg_photo_files, by ="vegphotoglobalid")
  
  inv_photos_ext <- agol_layers$invasives_ext %>%
    dplyr::select(invphotoglobalid = parentglobalid, OriginalFilePath, renamedfilepath)
  
  inv_photos_int <- agol_layers$invasives_int %>%
    dplyr::select(invphotoglobalid = parentglobalid, OriginalFilePath, renamedfilepath)
  
  inv_photo_files <- dplyr::bind_rows(inv_photos_ext, inv_photos_int) %>%
    dplyr::mutate(IsLibraryPhoto = "TBD")
  
  inv_photos <- agol_layers$invasives %>%
    dplyr::select(invphotoglobalid = globalid, visitglobalid = parentglobalid, PhotoType = InvasiveSpeciesCode) %>%
    dplyr::mutate(UtmX_m = 0, UtmY_m = 0,
                  PhotoSOP = "INV") %>%
    dplyr::left_join(inv_photo_files, by ="invphotoglobalid")
  
  add_photos_ext <- agol_layers$additional_photos_ext %>%
    dplyr::select(addphotoglobalid = parentglobalid, OriginalFilePath, renamedfilepath)
  
  add_photos_int <- agol_layers$additional_photos_int %>%
    dplyr::select(addphotoglobalid = parentglobalid, OriginalFilePath, renamedfilepath)
  
  add_photo_files <- dplyr::bind_rows(add_photos_ext, add_photos_int)
  
  add_photos <- agol_layers$additional_photos %>%
    dplyr::select(addphotoglobalid = globalid, visitglobalid = parentglobalid, PhotoType = AdditionalPhotoType, IsLibraryPhoto) %>%
    dplyr::mutate(UtmX_m = 0, UtmY_m = 0,
                  PhotoSOP = "MSC") %>%
    dplyr::left_join(add_photo_files, by ="addphotoglobalid")
  
  photos <- dplyr::bind_rows(veg_photos, rep_photos, inv_photos, add_photos)
  
  data$Photo <- visit %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, MonitoringStatus, SpringType, SampleFrame, DPL, Camera, CameraCard, GPSUnit, visitglobalid) %>%
    dplyr::mutate(DateTaken = VisitDate) %>%
    dplyr::inner_join(photos, by = "visitglobalid") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, MonitoringStatus, SpringType, SampleFrame, DPL, Camera, CameraCard, DateTaken, 
                  PhotoType, IsLibraryPhoto, OriginalFilePath, RenamedFilePath = renamedfilepath,GPSUnit, UtmX_m, UtmY_m, Notes, PhotoSOP)
  
  # ----- DischargeEstimated -----
  data$DischargeEstimated <- visit %>%
    dplyr::filter(DischargeMethod == "EST") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, Panel, FlowCondition, DischargeClass_L_per_s, VisitType)
  
  # ----- DischargeFlowCondition -----
  data$DischargeFlowCondition <- visit %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, Panel, FlowCondition, SpringbrookType, SpringbrookLengthFlag, SpringbrookLength_m, SpringbrookWidth_m, DiscontinuousSpringbrookLengthFlag, DiscontinuousSpringbrookLength_m, VisitType, Notes, SpringbrookNotes = DischargeNotes)
  
  # ----- DischargeVolumetric -----
  vol <- agol_layers$fill_time %>%
    dplyr::select(visitglobalid = parentglobalid, FillTime_seconds = FillTime_sec)
  
  data$DischargeVolumetric <- visit %>%
    dplyr::filter(DischargeMethod == "VOL") %>%
    dplyr::left_join(vol, by = "visitglobalid") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, Panel, FlowCondition, ContainerVolume_mL, FillTime_seconds, EstimatedCapture_percent, VisitType)
  
  # ----- Disturbance -----
  data$Disturbance <- visit %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, Panel, Roads, HumanUse, PlantManagement, HikingTrails, Livestock, OtherAnthropogenic, Fire, Flooding, Wildlife, OtherNatural, Overall, FlowModificationStatus, VisitType, Notes = DisturbanceNotes)
  
  # ----- DisturbanceFlowModification -----
  flow_mod <- agol_layers$disturbance_flow_mod %>%
    dplyr::select(visitglobalid = parentglobalid, ModificationType)
  
  data$DisturbanceFlowModification <- visit %>%
    dplyr::left_join(flow_mod, by = "visitglobalid") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, Panel, FlowModificationStatus, ModificationType, VisitType)
  
  # ----- Invasives -----
  invasives <- agol_layers$invasives %>%
    dplyr::select(visitglobalid = parentglobalid, InvasiveSpecies, RiparianVegBuffer) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_DS_Taxon, name, ScientificName = scientificname), by = c("InvasiveSpecies" = "name")) %>%
    dplyr::select(visitglobalid, InRiparianVegBuffer = RiparianVegBuffer, USDAPlantsCode = InvasiveSpecies, ScientificName)
  
  data$Invasives <- visit %>%
    dplyr::left_join(invasives, by = "visitglobalid") %>%
    dplyr::left_join(agol_layers$MOJN_Ref_DS_ParkTaxonProtectedStatus, by = c("USDAPlantsCode" = "Taxon", "Park" = "parkname")) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, Panel, InvasivesObserved, InRiparianVegBuffer, USDAPlantsCode, ScientificName, VisitType, ProtectedStatus = ProtectedStatusCode, Notes = InvasiveNotes)
  
  # ----- Riparian -----
  riparian <- agol_layers$riparian_veg %>%
    dplyr::select(visitglobalid = parentglobalid, LifeForm = lifeformname, DominantSpecies)
  
  riparian_visit <- visit %>%
    dplyr::select(visitglobalid, Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, Panel, IsVegetationObserved, MistletoePresent, `Woody >4m` = WoodyGT4m, `Woody 2-4m` = Woody2to4m, `Woody <2m` = WoodyLT2m, Forb, Rush, Grass, Reed, Sedge, Cattail, Bryophyte, `Non-Plant` = NonPlant, VisitType, Notes = RiparianVegetationNotes) %>%
    tidyr::pivot_longer(c(`Woody >4m`, `Woody 2-4m`, `Woody <2m`, Forb, Rush, Grass, Reed, Sedge, Cattail, Bryophyte, `Non-Plant`), names_to = "LifeForm", values_to = "Rank") %>%
    dplyr::group_by(visitglobalid) %>%
    dplyr::mutate(no_veg = all(is.na(Rank)),
                  LifeForm = ifelse(no_veg, NA, LifeForm)) %>%
    dplyr::ungroup() %>%
    unique() %>%
    dplyr::filter((!is.na(LifeForm) & (Rank != 12)) | (is.na(LifeForm) & is.na(Rank))) %>%
    dplyr::select(-no_veg)
  
  data$Riparian <- riparian_visit %>%
    dplyr::left_join(riparian, by = c("visitglobalid", "LifeForm")) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, Panel, IsVegetationObserved, MistletoePresent, LifeForm, Rank, DominantSpecies, VisitType, Notes)
    
  # ----- SensorRetrievalAttempts -----
  sensor_retrieval <- agol_layers$sensor_retrieval %>%
    dplyr::select(visitglobalid = parentglobalid, RetrievalTime, DownloadSuccessful, UploadSuccessful, Notes = SensorRetrieveNotes, SensorProblem, SensorIDRet)
  
  retrieval_visit <- visit %>%
    dplyr::select(visitglobalid, IsSensorSpring, RetrievalFieldSeason = FieldSeason, SiteName, SiteCode, Park, RetrievalDate = VisitDate, RetrievalVisitType = VisitType, SensorRetrieved = IsSensorRetrieved)
  
  sensor_deployment <- visit %>%
    # dplyr::filter(IsSensorSpring == "Y") %>%
    dplyr::select(visitglobalid, DeploymentFieldSeason = FieldSeason, DeploymentDate = VisitDate, SiteCode, SiteName, Park, IsSensorSpring, DeploymentVisitType = VisitType, SensorDeployed, SensorDep, SensorIDDep, DeploymentTime, SensorDeployNote)
  
  sensorRetrieval_visit <- sensor_retrieval %>%
    dplyr::inner_join(retrieval_visit, by = "visitglobalid")
  
  # data$SensorRetrievalAttempts <- visit %>%
  #   dplyr::filter(IsSensorSpring == "Y") %>%
  #   dplyr::select(visitglobalid, RetrievalFieldSeason = FieldSeason, SiteName, SiteCode, Park, RetrievalDate = VisitDate, SensorRetrieved = IsSensorRetrieved) %>%
  #   dplyr::left_join(sensor_retrieval, by = "visitglobalid") %>%
  #   dplyr::left_join(sensor_deployment, by = c("SiteCode", "SensorIDRet" = "SensorIDDep")) %>%
  #   dplyr::filter(DeploymentDate < RetrievalDate) %>%
  #   # dplyr::mutate(time_since_dep = RetrievalDate - DeploymentDate) %>%
  #   # dplyr::group_by(SensorIDRet, time_since_dep) %>%
  #   # dplyr::filter(time_since_dep == min(time_since_dep)) %>%
  #   dplyr::ungroup()
  data$SensorRetrievalAttempts <- sensor_retrieval %>%
    dplyr::inner_join(retrieval_visit, by = "visitglobalid") %>%
    dplyr::filter(IsSensorSpring == "Y") %>%
    dplyr::inner_join(sensor_deployment, by = c("SiteCode", "SensorIDRet" = "SensorIDDep"), relationship = "many-to-many") %>%
    dplyr::left_join(agol_layers$MOJN_Ref_DS_Sensor, by = c("SensorIDRet" = "name")) %>%
    dplyr::rename(SensorNumber = label) %>%
    dplyr::filter(DeploymentDate < RetrievalDate) %>%
    dplyr::select(SensorNumber, SerialNumber, DeploymentDate, DeploymentFieldSeason, RetrievalDate, RetrievalFieldSeason, SiteName = SiteName.x, SiteCode, Park = Park.x, SensorRetrieved, SensorProblem, DownloadResult = DownloadSuccessful, RetrievalVisitType, DeploymentVisitType, Notes)
  
  unk_sensors <- data$SensorRetrievalAttempts %>% 
    dplyr::filter(is.na(SerialNumber) | grepl("-9+", SerialNumber)) %>%
    dplyr::select(SensorNumber, SerialNumber, DeploymentDate, RetrievalDate, SiteName, SiteCode, SensorRetrieved) %>%
    unique()
  
  if(nrow(unk_sensors) > 0) {
    warn <- paste("Omitted", nrow(unk_sensors), "sensor retrievals due to missing serial numbers. Call `unk_sensors()` to see a list of omitted sensors.")
    assign("unk_sensors", unk_sensors, pkg_globals)
    warning(warn)
  }
  
  data$SensorRetrievalAttempts %>% 
    dplyr::filter(!is.na(SerialNumber) & !grepl("-9+", SerialNumber))
  
  # ----- SensorsCurrentlyDeployed -----
   data$SensorsCurrentlyDeployed <- sensor_deployment %>%
    dplyr::inner_join(sensorRetrieval_visit, by = c("SiteCode", "SensorIDDep" = "SensorIDRet", "DeploymentDate" = "RetrievalDate"), relationship = "many-to-many") %>%
    dplyr::filter(SensorDeployed == "Y") %>%
    dplyr::left_join(agol_layers$MOJN_Ref_DS_Sensor, by = c("SensorIDDep" = "name")) %>%
    dplyr::rename(SensorNumber = label) %>%
    dplyr::select(SensorNumber, SerialNumber, SiteCode, SiteName = SiteName.x, 
                  VisitDate = DeploymentDate, FieldSeason = DeploymentFieldSeason, Park = Park.x, VisitType = DeploymentVisitType, Notes = SensorDeployNote)

  # ----- SensorsAllDeployments -----
  data$SensorsAllDeployments <- sensor_deployment %>%
    dplyr::inner_join(visit, by = c("visitglobalid" = "visitglobalid")) %>%
    dplyr::filter(IsSensorSpring.x == "Y", SensorDeployed.x == "Y") %>%
    dplyr::left_join(agol_layers$MOJN_Ref_DS_Sensor, by = c("SensorIDDep.y" = "name")) %>%
    dplyr::rename(SensorNumber = label) %>%
    dplyr::select(SensorNumber, SerialNumber, SiteCode = SiteCode.x, SiteName = SiteName.x, 
                  VisitDate, FieldSeason, Park = Park.x, VisitType, Notes = SensorDeployNote.x, SensorDeployed = SensorDeployed.x)
  
  # ----- Site -----
  data$Site <- agol_layers$sites %>%
    dplyr::select(Park,
                  Subunit,
                  SiteCode,
                  SiteName = sitename,
                  SiteDescription,
                  EcologicalDescription,
                  LogisticalDescription,
                  DriveDescription,
                  HikeDescription,
                  HikeDistance_m,
                  HikeTime_min,
                  GRTSDraw,
                  GRTSOrder, 
                  SiteStatus,
                  SampleFrame,
                  Panel = PanelGroup,
                  SiteProtectedStatus,
                  Lat_WGS84,
                  Lon_WGS84,
                  X_UTM_NAD83_11N, 
                  Y_UTM_NAD83_11N)
  
  # ----- Visit -----
  data$Visit <- visit %>%
    dplyr::select(Park,
                  SiteCode,
                  SiteName,
                  VisitDate,
                  FieldSeason,
                  SampleFrame,
                  Panel,
                  VisitType,
                  MonitoringStatus,
                  SpringType,
                  SecondarySpringType,
                  Notes,
                  DPL
    )
  
  # ----- VisitActivity -----
  yn <- agol_layers$MOJN_Lookup_Shared_YesNo$label
  names(yn) <- agol_layers$MOJN_Lookup_Shared_YesNo$name
  wq_collected <- agol_layers$MOJN_Lookup_DS_WaterQualityDataCollected$label
  names(wq_collected) <- agol_layers$MOJN_Lookup_DS_WaterQualityDataCollected$name
  data$VisitActivity <- visit %>%
    dplyr::select(Park,
                  SiteCode,
                  SiteName,
                  VisitDate,
                  FieldSeason,
                  SpringType,
                  SecondarySpringType,
                  FlowCondition, 
                  WQDataCollected = WasWaterQualityDataCollected, 
                  InvasivesObserved,
                  RiparianObserved = IsVegetationObserved,
                  MistletoePresent,
                  WildlifeObserved = Waswildlifeobserved,
                  SpringbrookLength_Class = SpringbrookLengthFlag,
                  SpringbrookWidth_m,
                  SpringbrookLength_m,
                  SampleFrame,
                  VisitType,
                  MonitoringStatus,
                  WaterQualityNotes = WQNotes) %>%
    dplyr::mutate(WQDataCollected = wq_collected[WQDataCollected],
                  InvasivesObserved = yn[InvasivesObserved],
                  RiparianObserved = yn[RiparianObserved],
                  MistletoePresent = yn[MistletoePresent],
                  WildlifeObserved = yn[WildlifeObserved])
  
  
  # ----- WaterQualityDO -----
  percent <- visit %>%
         dplyr::filter(WQDataCollected == "Yes") %>%
         dplyr::select(visitglobalid, WQDataCollected,
                       DissolvedOxygen_percent_1, DissolvedOxygen_percent_2, DissolvedOxygen_percent_3) %>%
         tidyr::pivot_longer(cols = dplyr::starts_with("DissolvedOxygen_"),
                             values_to = "DissolvedOxygen_percent", names_to = NULL) %>%
         dplyr::right_join(visit, by = "visitglobalid") %>%
         dplyr::select(visitglobalid, Park, SiteCode, SiteName, VisitDate, FieldSeason,
                  WQDataCollected = WQDataCollected.y, DissolvedOxygen_percent)
  
  percent <- dplyr::mutate(percent, ID = 1:nrow(percent))
  
  mg <- visit %>% 
    dplyr::filter(WQDataCollected == "Yes") %>%
    dplyr::select(visitglobalid,
                  DissolvedOxygen_mg_per_L_1, DissolvedOxygen_mg_per_L_2, DissolvedOxygen_mg_per_L_3) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("DissolvedOxygen_"),
                        values_to = "DissolvedOxygen_mg_per_L", names_to = NULL) %>%
    dplyr::right_join(visit, by = "visitglobalid") %>%
    dplyr::select(visitglobalid, WQNotes, DO_DataQualityFlag, DOInstrument, SampleFrame, Panel,
                  VisitType, MonitoringStatus, DissolvedOxygen_mg_per_L)
  
  mg <- dplyr::mutate(mg, ID = 1:nrow(mg))
  
  WaterQualityDO <- mg %>%
    dplyr::inner_join(percent, by = c("visitglobalid", "ID")) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, Panel, WQDataCollected, 
                  DissolvedOxygen_percent, DissolvedOxygen_mg_per_L, DataQualityFlag = DO_DataQualityFlag, 
                  DataQualityFlagNote = WQNotes, DOInstrument, VisitType, MonitoringStatus)
    
  wqDO_filterRepeats <- WaterQualityDO %>%
    dplyr::filter(is.na(DissolvedOxygen_percent), is.na(DissolvedOxygen_mg_per_L)) %>%
    unique()
  
  WaterQualityDO <- WaterQualityDO %>%
    dplyr::filter_at(dplyr::vars(DissolvedOxygen_mg_per_L, DissolvedOxygen_percent), dplyr::any_vars(!is.na(.)))
  
  data$WaterQualityDO <- dplyr::bind_rows(wqDO_filterRepeats, WaterQualityDO)
  
  # ----- WaterQualitypH -----
 WaterQualitypH <- visit %>%
    dplyr::select(visitglobalid, pH_1, pH_2, pH_3) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("pH_"),
                        values_to = "pH", names_to = NULL) %>%
    dplyr::right_join(visit, by = "visitglobalid") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, Panel, WQDataCollected, pH, DataQualityFlag = pH_DataQualityFlag, 
                  DataQualityFlagNote = WQNotes, pHInstrument, VisitType, MonitoringStatus) 
    
 wqpH_filterRepeats <- WaterQualitypH %>%
   dplyr::filter(is.na(pH)) %>%
   unique()
 
 WaterQualitypH <- WaterQualitypH %>%
   dplyr::filter(!is.na(pH))
 
 data$WaterQualitypH <- dplyr::bind_rows(wqpH_filterRepeats, WaterQualitypH)
 
  
  # ----- WaterQualitySpCond -----
  WaterQualitySpCond <- visit %>%
    dplyr::select(visitglobalid, SpecificConductance_microS_1, SpecificConductance_microS_2, SpecificConductance_microS_3) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("SpecificConductance_"),
                        values_to = "SpecificConductance_microS_per_cm", names_to = NULL) %>%
    dplyr::right_join(visit, by = "visitglobalid") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, Panel, WQDataCollected, 
                  SpecificConductance_microS_per_cm, DataQualityFlag = SpCond_microS_DataQualityFlag, DataQualityFlagNote = WQNotes, 
                  SpCondInstrument, VisitType, MonitoringStatus)
 
 wqSpCond_filterRepeats <- WaterQualitySpCond %>%
   dplyr::filter(is.na(SpecificConductance_microS_per_cm))%>%
   unique()
 
 WaterQualitySpCond <- WaterQualitySpCond %>%
   dplyr::filter(!is.na(SpecificConductance_microS_per_cm))
 
 data$WaterQualitySpCond <- dplyr::bind_rows(WaterQualitySpCond, wqSpCond_filterRepeats)
  
  # ----- WaterQualityTemperature -----
  WaterQualityTemperature <- visit %>%
    dplyr::select(visitglobalid, Temperature_C_1, Temperature_C_2, Temperature_C_3) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("Temperature_"),
                        values_to = "WaterTemperature_C", names_to = NULL) %>%
    dplyr::right_join(visit, by = "visitglobalid") %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, Panel, WQDataCollected, WaterTemperature_C, 
                  DataQualityFlag = Temp_C_DataQualityFlag, DataQualityFlagNote = WQNotes, TemperatureInstrument, 
                  VisitType, MonitoringStatus)
 
 wqTemp_filterRepeats <- WaterQualityTemperature %>%
   dplyr::filter(is.na(WaterTemperature_C)) %>%
   unique()
 
 WaterQualityTemperature <- WaterQualityTemperature %>%
   dplyr::filter(!is.na(WaterTemperature_C))
 
 data$WaterQualityTemperature <- dplyr::bind_rows(WaterQualityTemperature, wqTemp_filterRepeats)
  
  # ----- Wildlife -----
  data$Wildlife <- agol_layers$wildlife |>
    dplyr::inner_join(visit, by = c("parentglobalid" = "visitglobalid")) |>
    dplyr::filter(VisitDate > "2018-10-01",
                  !(VisitType %in% c("Training", "Dummy"))) |>
    dplyr::mutate(WildlifeType = dplyr::case_when(WildlifeType == "11" ~ "UNGU",
                                                  TRUE ~ WildlifeType)) |>
    dplyr::left_join(agol_layers$MOJN_Lookup_DS_WildlifeType, by = c("WildlifeType" = "name")) |>
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, Panel, VisitType, IsWildlifeObserved, WildlifeType = label, DirectObservation, Scat, Tracks, Shelter, Foraging, Vocalization, OtherEvidence, Notes = Species_Notes) |>
    dplyr::mutate(DirectObservation = dplyr::case_when(DirectObservation == "1" ~ "Y",
                                                       DirectObservation == "2" ~ "N",
                                                       DirectObservation == "3" ~ "ND",
                                                       TRUE ~ DirectObservation)) |>
   dplyr::mutate(Scat = dplyr::case_when(Scat == "1" ~ "Y",
                                         Scat == "2" ~ "N",
                                         Scat == "3" ~ "ND",
                                         TRUE ~ Scat)) |>
   dplyr::mutate(Tracks = dplyr::case_when(Tracks == "1" ~ "Y",
                                           Tracks == "2" ~ "N",
                                           Tracks == "3" ~ "ND",
                                           TRUE ~ Tracks)) |>
   dplyr::mutate(Shelter = dplyr::case_when(Shelter == "1" ~ "Y",
                                            Shelter == "2" ~ "N",
                                            Shelter == "3" ~ "ND",
                                            TRUE ~ Shelter)) |>
   dplyr::mutate(Foraging = dplyr::case_when(Foraging == "1" ~ "Y",
                                             Foraging == "2" ~ "N",
                                             Foraging == "3" ~ "ND",
                                             TRUE ~ Foraging)) |>
   dplyr::mutate(Vocalization = dplyr::case_when(Vocalization == "1" ~ "Y",
                                                 Vocalization == "2" ~ "N",
                                                 Vocalization == "3" ~ "ND",
                                                 TRUE ~ Vocalization)) |>
   dplyr::mutate(OtherEvidence = dplyr::case_when(OtherEvidence == "1" ~ "Y",
                                                  OtherEvidence == "2" ~ "N",
                                                  OtherEvidence == "3" ~ "ND",
                                                  TRUE ~ OtherEvidence)) |>
    dplyr::mutate(DirectObservation = yn[DirectObservation],
                  Scat = yn[Scat],
                  Tracks = yn[Tracks],
                  Shelter = yn[Shelter],
                  Foraging = yn[Foraging],
                  Vocalization = yn[Vocalization],
                  OtherEvidence = yn[OtherEvidence]) |>
   dplyr::filter(!is.na(WildlifeType))

  return(data)
}


#' Read data from the Desert Springs AGOL feature layers. Returns the raw data in its current format on AGOL. Mostly used for data management purposes. 
#'
#' @param data_path URL to Desert Springs feature service on AGOL.
#' @param lookup_path URL to feature service on AGOL containing Desert Springs lookup tables.
#' @param sites_path URL to feature service on AGOL containing sites table
#' @param calibration_path URL to feature service on AGOL containing calibration tables.
#' @param agol_username Username of headless AGOL account with permissions to view the feature service.
#' @param agol_password Password for headless AGOL account.
#'
#' @return A list of tibbles
#' @export 
#'
FetchAGOLLayers <- function(data_path = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_DS_SpringVisit/FeatureServer",
                            sites_path = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_DS_GRTSDraw/FeatureServer",
                            calibration_path = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_Calibration_Database/FeatureServer",
                            agol_username = "mojn_data",
                            agol_password = keyring::key_get(service = "AGOL", username = "mojn_data")) {
  
  # Get a token with a headless account
  token_resp <- httr::POST("https://nps.maps.arcgis.com/sharing/rest/generateToken",
                           body = list(username = agol_username,
                                       password = agol_password,
                                       referer = 'https://irma.nps.gov',
                                       f = 'json'),
                           encode = "form")
  agol_token <- jsonlite::fromJSON(httr::content(token_resp, type="text", encoding = "UTF-8"))
  
  agol_layers <- list()
  
  # Fetch sites table
  agol_layers$sites <- fetchAllRecords(sites_path, 0, token = agol_token$token)
  
  # Fetch lookup CSVs from AGOL
  agol_layers$MOJN_Lookup_DS_DataQualityFlag <- fetchagol:::fetchHostedCSV(item_id = "c2345c5ab1a64699951a8ec654db549a", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Lookup_DS_DischargeEstimatedClass <- fetchagol:::fetchHostedCSV(item_id = "05a5c9c100ce4501adff3785a1c3a76c", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Lookup_DS_DischargeEstimatedFlag <- fetchagol:::fetchHostedCSV(item_id = "f8777b4dfddf449a98a96847434a242e", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Lookup_DS_DisturbanceClass <- fetchagol:::fetchHostedCSV(item_id = "cd209a9d16b24b60a633e33dc9e2a9e7", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Lookup_DS_FlowCondition <- fetchagol:::fetchHostedCSV(item_id = "c299a235a40a4be5a58e014c80dfe9f6", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Lookup_DS_FlowModificationStatus <- fetchagol:::fetchHostedCSV(item_id = "baafc4b891d2412a894812161f207ad8", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Lookup_DS_LifeForm <- fetchagol:::fetchHostedCSV(item_id = "b2d30c9528bf4902a28aa4b3ddd9df47", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Lookup_DS_ModificationType <- fetchagol:::fetchHostedCSV(item_id = "869ecebca75244538da6c00904d67ca6", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Lookup_DS_MonitoringStatus <- fetchagol:::fetchHostedCSV(item_id = "0c64a67c244341c097b8e711183d9016", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Lookup_DS_Panel <- fetchagol:::fetchHostedCSV(item_id = "5541e4e08ae542169817e25bd37f930b", token = agol_token, root = "nps.maps.arcgis.com")
  # Park
  # PersonnelRole
  # ProtectedStatus
  agol_layers$MOJN_Lookup_DS_RepeatPhotoType <- fetchagol:::fetchHostedCSV(item_id = "3dc984689613483e8a2f38e18291121f", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Lookup_DS_RiparianVegetationBuffer <- fetchagol:::fetchHostedCSV(item_id = "a103c986a72248d3b79057b3293cf5d9", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Lookup_DS_SampleFrame <- fetchagol:::fetchHostedCSV(item_id = "6828a4d0aa5843658353a91bf0984227", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Lookup_DS_SensorProblem <- fetchagol:::fetchHostedCSV(item_id = "85cc253494dc43d6898b5217355241d3", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Lookup_DS_SpringbrookLengthFlag <- fetchagol:::fetchHostedCSV(item_id = "eb9c768d5b7c4371874c0070f2af9e24", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Lookup_DS_SpringType <- fetchagol:::fetchHostedCSV(item_id = "bdfab3633f6d4eb2bfcf66fdff0ef44f", token = agol_token, root = "nps.maps.arcgis.com")
  # Subunit
  agol_layers$MOJN_Lookup_DS_TaxonomicReferenceAuthority <- fetchagol:::fetchHostedCSV(item_id = "c4a19d99a7ef421cb4006c4a4d16ee8f", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Lookup_DS_TaxonomicStandard <- fetchagol:::fetchHostedCSV(item_id = "afc60202b9c74a4fa35879dd218152bc", token = agol_token, root = "nps.maps.arcgis.com")
  # UTMZone
  agol_layers$MOJN_Lookup_DS_VisitType <- fetchagol:::fetchHostedCSV(item_id = "2944fdadcbf04d18a2da82518324d7fa", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Lookup_DS_WaterQualityDataCollected <- fetchagol:::fetchHostedCSV(item_id = "3960bd25174e43f0a0b8cb68d506f30d", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Lookup_DS_WildlifeEvidence <- fetchagol:::fetchHostedCSV(item_id = "e66c52a0e9d54b1f84bffeeddd12aa8d", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Lookup_DS_WildlifeEvidencePresent <- fetchagol:::fetchHostedCSV(item_id = "0e9510b02d1d4cccae4fbd055fb4b745", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Lookup_DS_WildlifeType <- fetchagol:::fetchHostedCSV(item_id = "37bee5b9080b447b8fda1e5d5a0ce025", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Lookup_Shared_YesNo <- fetchagol:::fetchHostedCSV(item_id = "9db4e760b780478d97d69afcac328f60", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Ref_DS_ParkTaxonProtectedStatus <- fetchagol:::fetchHostedCSV(item_id = "60b6a18a6d0b4a64a6d17c045661ffb2", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Ref_DS_PhotoDescriptionCode <- fetchagol:::fetchHostedCSV(item_id = "b0b0e503a1de49aaa8b12cc06c620e26", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Ref_DS_Protocol <- fetchagol:::fetchHostedCSV(item_id = "1ddf1b417bae44e0956ea0ef5227080b", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Ref_DS_Sensor <- fetchagol:::fetchHostedCSV(item_id = "3cb4d9394cf64946b8c45d3a8dcb9deb", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Ref_DS_SensorModel <- fetchagol:::fetchHostedCSV(item_id = "f72bfd6a06dd464c8c5d88d144f8370c", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Ref_DS_Taxon <- fetchagol:::fetchHostedCSV(item_id = "1d592d383cbc4a6cb69210a0373e5834", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Ref_Shared_Camera <- fetchagol:::fetchHostedCSV(item_id = "59cc84d570034b659a4a3ffb4ce3917d", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Ref_Shared_CameraCard <- fetchagol:::fetchHostedCSV(item_id = "27740e23c88449418ea8c7df4e5dd169", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Ref_Shared_DataCollectionDevice <- fetchagol:::fetchHostedCSV(item_id = "302cd9983c84445baef4526112ccdd17", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Ref_Shared_GPSUnit <- fetchagol:::fetchHostedCSV(item_id = "67ed3cc830e24128820a35fbaee8f7c2", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Ref_Shared_Personnel <- fetchagol:::fetchHostedCSV(item_id = "032650ca06154f389d7e16470180dea3", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Ref_Shared_Plants <- fetchagol:::fetchHostedCSV(item_id = "34afae93786a4fe89a817a6af2c2da34", token = agol_token, root = "nps.maps.arcgis.com")
  agol_layers$MOJN_Ref_Shared_WaterQualityInstrument <- fetchagol:::fetchHostedCSV(item_id = "8036e3174ef44dd2924056d0bbb20af6", token = agol_token, root = "nps.maps.arcgis.com")
  
  #Fetch each layer in the Calibration feature service
  agol_layers$CalibrationSpCond <- fetchAllRecords(calibration_path, 3, token = agol_token$token)
  agol_layers$CalibrationpH <- fetchAllRecords(calibration_path, 4, token = agol_token$token)
  agol_layers$CalibrationDO <- fetchAllRecords(calibration_path, 5, token = agol_token$token)
    
  # Fetch each layer in the DS feature service
  
  # ----- MOJN_DS_SpringVisit - visit-level data -----
  agol_layers$visit <- fetchAllRecords(data_path, 0, token = agol_token$token) %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles")) %>%
    dplyr::mutate(DateTime = as.POSIXct(DateTime/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- Repeats - repeat photos -----
  agol_layers$repeats <- fetchAllRecords(data_path, 1, token = agol_token$token, geometry = TRUE) %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- InvasivePlants - invasive plant data -----
  agol_layers$invasives <- fetchAllRecords(data_path, 2, token = agol_token$token, geometry = TRUE) %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- Observers -----
  agol_layers$observers <- fetchAllRecords(data_path, 3, token = agol_token$token) %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- SensorRetrieval -----
  agol_layers$sensor_retrieval <- fetchAllRecords(data_path, 4, token = agol_token$token) %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- RepeatPhotos_Internal - repeat photos taken on internal device camera -----
  agol_layers$repeats_int <- fetchAllRecords(data_path, 5, token = agol_token$token)
  
  # ----- RepeatPhotos_External - repeat photos taken on external camera -----
  agol_layers$repeats_ext <- fetchAllRecords(data_path, 6, token = agol_token$token)
  
  # ----- FillTime - volumetric discharge fill time -----
  agol_layers$fill_time <- fetchAllRecords(data_path, 7, token = agol_token$token) %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- FlowModTypes - flow modifications observed -----
  agol_layers$disturbance_flow_mod <- fetchAllRecords(data_path, 8, token = agol_token$token) %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- WildlifeRepeat - wildlife observations -----
  agol_layers$wildlife <- fetchAllRecords(data_path, 9, token = agol_token$token) %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- VegImageRepeat - riparian veg photo data -----
  agol_layers$riparian_veg  <- fetchAllRecords(data_path, 10, token = agol_token$token) %>%
    dplyr::mutate(EditDate = as.POSIXct(EditDate/1000, origin = "1970-01-01", tz = "America/Los_Angeles"))
  
  # ----- InternalCamera - riparian veg photos taken on internal device camera -----
  agol_layers$riparian_veg_int <- fetchAllRecords(data_path, 11, token = agol_token$token)
  
  # ----- ExternalCameraFiles - riparian veg photos taken on external camera -----
  agol_layers$riparian_veg_ext <- fetchAllRecords(data_path, 12, token = agol_token$token)
  
  # ----- InvImageRepeat - invasive veg photos taken on internal device camera -----
  agol_layers$invasives_int <- fetchAllRecords(data_path, 13, token = agol_token$token)
  
  # ----- ExternalCameraFilesInv - invasive veg photos taken on external camera -----
  agol_layers$invasives_ext <- fetchAllRecords(data_path, 14, token = agol_token$token)
  
  # ----- AdditionalPhotos2 - info about additional photos -----
  agol_layers$additional_photos <- fetchAllRecords(data_path, 15, token = agol_token$token)
  
  # ----- AdditionalPhotoInternal - additional photos taken on internal camera -----
  agol_layers$additional_photos_int <- fetchAllRecords(data_path, 16, token = agol_token$token)
  
  # ----- AddtionalPhotoExternal - additional photos taken on external camera -----
  agol_layers$additional_photos_ext <- fetchAllRecords(data_path, 17, token = agol_token$token)
  
  # agol_layers <- c(agol_layers, lookup_layers) - DEPRECATED
  
  agol_layers <- lapply(agol_layers, function(data_table) {
    data_table <- data_table %>%
      dplyr::mutate(dplyr::across(where(is.character), function(x) {
        x %>%
          utf8::utf8_encode() %>%  # Encode text as UTF-8 - this prevents a lot of parsing issues in R
          trimws() %>%  # Trim leading and trailing whitespace
          dplyr::na_if("")  # Replace empty strings with NA
      }))
    col_names <- names(data_table)
    name_and_label <- grepl("(name)|(label)", col_names, ignore.case = TRUE)
    names(data_table)[name_and_label] <- tolower(names(data_table[name_and_label]))
    
    return(data_table)
  })
  
  return(agol_layers)
}

#' Fetch tabular data from AGOL
#' 
#' Retrieves tabular data from AGOL layers and tables, even when number of rows exceeds maximum record count.
#'
#' @param data_path Feature service URL
#' @param layer_number Layer number
#' @param token Authentication token (optional)
#' @param geometry Include spatial data columns? Works with points, not tested with other geometry types
#' @param where Query clause specifying a subset of rows (optional; defaults to all rows). See AGOL REST API documentation.
#' @param outFields String indicating which fields to return (optional; defaults to all fields). See AGOL REST API documentation.
#'
#' @return A tibble
#' @export
#'
fetchAllRecords <- function(data_path, layer_number, token, geometry = FALSE, where = "1=1", outFields = "*") {
  result <- tibble::tibble()
  exc_transfer <- TRUE
  offset <- nrow(result)
  
  qry <- list(where = where,
              outFields = outFields,
              f = "JSON",
              resultOffset = offset)
  
  if (!missing(token)) {
    qry$token <- token
  }
  
  while(exc_transfer) {
    resp <- httr::GET(paste0(data_path, "/", layer_number, "/query"),
                      query = qry)
    
    content <- jsonlite::fromJSON(httr::content(resp, type = "text", encoding = "UTF-8"))
    
    if ("exceededTransferLimit" %in% names(content)) {
      exc_transfer <- content$exceededTransferLimit
    } else {
      exc_transfer <- FALSE
    }
    
    if (geometry) {
      partial_result <- cbind(content$features$attributes, content$features$geometry) %>%
        dplyr::mutate(wkid = content$spatialReference$wkid) %>%
        tibble::as_tibble()
    } else {
      partial_result <- tibble::as_tibble(content$features$attributes)
    }
    result <- rbind(result, partial_result)
    offset <- nrow(result)
    qry$resultOffset <- offset
  }
  return(result)
}
