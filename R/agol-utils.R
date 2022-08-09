#' Wrangle AGOL data into a set of dataframes structured for use in this package.
#'
#' @param agol_layers The list of tibbles returned by `FetchAGOLLayers()`
#'
#' @return A list of tibbles
#' 
WrangleAGOLData <- function(agol_layers) {
  data <- list()
  
  # Clean up visit table and replace numeric keys with meaningful values
  agol_layers$visit <- agol_layers$visit %>%
    dplyr::select(-SiteCodeText) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_Site, Code, SiteName = Name, GRTSPanelID), by = c("SiteCode"= "Code")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_GRTSPanel, ID, SampleFrame = Code), by = c("GRTSPanelID" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_VisitType, ID, VisitTypeLabel = Label), by = c("VisitType" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_MonitoringStatus, ID, MonitoringStatus = Label), by = c("Status" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_SpringType, ID, SpringType = Label), by = c("SpringType" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_UTMZone, ID, UTMZone = Code), by = c("UTMZone" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_HorizontalDatum, ID, Datum = Code), by = c("Datum" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_GPSUnit, ID, GPS = Label), by = c("GPS" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Lookup_Shared_Camera, ID, Camera = Label), by = c("Camera" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_CameraCard, ID, CameraCard = Label), by = c("CameraCard" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_IsSensorRetrieved, ID, IsSensorRetrieved = Code), by = ("SensorRetrieved" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_FlowCondition, ID, FlowCondition = Label), by = c("FlowCondition" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_DS_SensorModel, ID, SensorTypeDep = Label), by = c("SensorTypeDep" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_DS_Sensor, ID, SensorDep = Label), by = c("SensorIDDep" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_FlowCondition, ID, FlowCondition = Label), by = c("FlowCondition" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_SpringbrookLengthFlag, ID, SpringbrookLength_Class = Label), by = c("SPBKLength" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_WaterQualityDataCollected, ID, WQDataCollected = Label), by = c("WasWaterQualityDataCollected" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, ID, WQInstrument = Label), by = c("WQInstrument" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, ID, pHInstrument = Label), by = c("pHInstrument" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, ID, DOInstrument = Label), by = c("DOInstrument" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, ID, SpCondInstrument = Label), by = c("SpCondInstrument" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Ref_Shared_WaterQualityInstrument, ID, TemperatureInstrument = Label), by = c("TemperatureInstrument" = "ID")) %>%
    dplyr::
    
    
    
  
  # ----- Calibration data -----
  # TODO
  
  # ----- Photos -----
  # TODO
  
  # ----- DischargeEstimated -----
  
  # ----- DischargeFlowCondition -----
  
  # ----- DischargeVolumetric -----
  
  # ----- Disturbance -----
  
  # ----- DisturbanceFlowModification -----
  
  # ----- Invasives -----
  
  # ----- Riparian -----
  
  # ----- SensorRetrievalAttempts -----
  
  # ----- SensorsCurrentlyDeployed -----
  
  # ----- SensorsAllDeployments -----
  
  # ----- Site -----
  # TODO
  
  # ----- Visit -----
  data$Visit <- agol_layers$visit %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_Site, Code, SiteName = Name, GRTSPanelID), by = c("SiteCode"= "Code")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_GRTSPanel, ID, SampleFrame = Code), by = c("GRTSPanelID" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_VisitType, ID, VisitTypeLabel = Label), by = c("VisitType" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_MonitoringStatus, ID, MonitoringStatus = Label), by = c("Status" = "ID")) %>%
    dplyr::left_join(dplyr::select(agol_layers$MOJN_Lookup_DS_SpringType, ID, SpringType = Label), by = c("SpringType" = "ID")) %>%
    dplyr::select(Park,
                  SiteCode,
                  SiteName,
                  VisitDate = DateTime,
                  SampleFrame,
                  VisitType = VisitTypeLabel,
                  MonitoringStatus,
                  SpringType,
                  Notes = SpringComments
                  ) %>%
    dplyr::mutate(DPL = "TO BE ADDED")  # TODO
  
  # ----- VisitActivity -----
  # data$VisitActivity <- data$Visit %>%
    
  
  # ----- WaterQualityDO -----
  
  # ----- WaterQualitypH -----
  
  # ----- WaterQualitySpCond -----
  
  # ----- WaterQualityTemperature -----
  
  # ----- Wildlife -----
  
  
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
  
  return(agol_layers)
}