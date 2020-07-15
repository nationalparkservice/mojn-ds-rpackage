#' Calculate median values for each water quality paramenter for each site visit.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#' @param data.name The name of the analysis view or the csv file containing the data. E.g. "CalibrationDO", "DischargeVolumetric". See details for full list of data name options.
#'
#' @return A tibble with columns for park, field season, site code, visit date, and the median values, flags, and flag notes for temperature, specific conductance, pH, and dissolved oxygen.
#' @export
#'
QcWqMedian <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  
  temp <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "WaterQualityTemperature")
  spcond <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "WaterQualitySpCond")
  ph <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "WaterQualitypH")
  do <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "WaterQualityDO")
  
  temp.med <- temp %>%
    dplyr::filter(VisitType == "Primary" & MonitoringStatus == "Sampled") %>%
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, DataQualityFlag, DataQualityFlagNote) %>%
    dplyr::summarise(TempMedian = median(WaterTemperature_C)) %>%
    dplyr::rename(TempFlag = DataQualityFlag, TempFlagNote = DataQualityFlagNote) %>%
    dplyr::arrange(SiteCode)
  
  spcond.med <- spcond %>%
    dplyr::filter(VisitType == "Primary" & MonitoringStatus == "Sampled") %>%
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, DataQualityFlag, DataQualityFlagNote) %>%
    dplyr::summarise(SpCondMedian = median(SpecificConductance_microS_per_cm)) %>%
    dplyr::rename(SpCondFlag = DataQualityFlag, SpCondFlagNote = DataQualityFlagNote) %>%
    dplyr::arrange(SiteCode)
  
  ph.med <- ph %>%
    dplyr::filter(VisitType == "Primary" & MonitoringStatus == "Sampled") %>%
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, DataQualityFlag, DataQualityFlagNote) %>%
    dplyr::summarise(pHMedian = median(pH)) %>%
    dplyr::rename(pHFlag = DataQualityFlag, pHFlagNote = DataQualityFlagNote) %>%
    dplyr::arrange(SiteCode)
  
  do.med <- do %>%
    dplyr::filter(VisitType == "Primary" & MonitoringStatus == "Sampled") %>%
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, DataQualityFlag, DataQualityFlagNote) %>%
    dplyr::summarise(DOPercentMedian = median(DissolvedOxygen_percent), DOmgLMedian = median(DissolvedOxygen_mg_per_L)) %>%
    dplyr::rename(DOFlag = DataQualityFlag, DOFlagNote = DataQualityFlagNote) %>%
    dplyr::arrange(SiteCode)
  
  wq.med <- temp.med %>%
    dplyr::left_join(spcond.med, by= c("Park", "FieldSeason", "SiteCode", "VisitDate")) %>%
    dplyr::left_join(ph.med, by= c("Park", "FieldSeason", "SiteCode", "VisitDate")) %>%
    dplyr::left_join(do.med, by= c("Park", "FieldSeason", "SiteCode", "VisitDate"))
  
  return(wq.med)
  
}

#' Perform sanity check and compile list of potentially incorrect or outlier water quality values.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns for Park, FieldSeason, SiteCode, VisitDate, Parameter, Units, Median, Flag, and FlagNote.
#' @export
#'
QcWqSanity <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  
  wq.sanity.predata <- QcWqMedian(conn, path.to.data, park, site, field.season, data.source = "database")
  
  temp.sanity <- wq.sanity.predata %>%
    dplyr::filter(TempMedian > 30) %>%
    dplyr::ungroup() %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, TempMedian, TempFlag, TempFlagNote) %>%
    tibble::add_column(Parameter = "Temp", Units = "C", .after = "VisitDate") %>%
    dplyr::rename(Median = TempMedian, Flag = TempFlag, FlagNote = TempFlagNote)
  
  spcond.sanity <- wq.sanity.predata %>%
    dplyr::filter(SpCondMedian > 20000) %>%
    dplyr::ungroup() %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, SpCondMedian, SpCondFlag, SpCondFlagNote) %>%
    tibble::add_column(Parameter = "SpCond", Units = "uS/cm", .after = "VisitDate") %>%
    dplyr::rename(Median = SpCondMedian, Flag = SpCondFlag, FlagNote = SpCondFlagNote)
 
  ph.sanity <- wq.sanity.predata %>%
    dplyr::filter(pHMedian > 10 | pHMedian < 6) %>%
    dplyr::ungroup() %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, pHMedian, pHFlag, pHFlagNote) %>%
    tibble::add_column(Parameter = "pH", Units = "units", .after = "VisitDate") %>%
    dplyr::rename(Median = pHMedian, Flag = pHFlag, FlagNote = pHFlagNote)
  
  do.percent.sanity <- wq.sanity.predata %>%
    dplyr::filter(DOPercentMedian > 110 | DOPercentMedian < 2) %>%
    dplyr::ungroup() %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, DOPercentMedian, DOFlag, DOFlagNote) %>%
    tibble::add_column(Parameter = "DO", Units = "%", .after = "VisitDate") %>%
    dplyr::rename(Median = DOPercentMedian, Flag = DOFlag, FlagNote = DOFlagNote)
  
  do.mgl.sanity <- wq.sanity.predata %>%
    dplyr::filter(DOmgLMedian > 12) %>%
    dplyr::ungroup() %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, DOmgLMedian, DOFlag, DOFlagNote) %>%
    tibble::add_column(Parameter = "DO", Units = "mg/L", .after = "VisitDate") %>%
    dplyr::rename(Median = DOmgLMedian, Flag = DOFlag, FlagNote = DOFlagNote)

  wq.sanity <- rbind(temp.sanity, spcond.sanity, ph.sanity, do.percent.sanity, do.mgl.sanity)
  return(wq.sanity)  
  
}

#' Compile list of water quality values that have data quality flags.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns for Park, FieldSeason, SiteCode, VisitDate, Parameter, Units, Median, Flag, and FlagNote.
#' @export
#'
QcWqFlags <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  
  wq.flags.predata <- QcWqMedian(conn, path.to.data, park, site, field.season, data.source = "database")
    
  temp.flags <- wq.flags.predata %>%
    dplyr::filter(TempFlag %in% c("I", "W", "C")) %>%
    dplyr::ungroup() %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, TempMedian, TempFlag, TempFlagNote) %>%
    tibble::add_column(Parameter = "Temp", Units = "C", .after = "VisitDate") %>%
    dplyr::rename(Median = TempMedian, Flag = TempFlag, FlagNote = TempFlagNote)
  
  spcond.flags <- wq.flags.predata %>%
    dplyr::filter(SpCondFlag %in% c("I", "W", "C")) %>%
    dplyr::ungroup() %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, SpCondMedian, SpCondFlag, SpCondFlagNote) %>%
    tibble::add_column(Parameter = "SpCond", Units = "uS/cm", .after = "VisitDate") %>%
    dplyr::rename(Median = SpCondMedian, Flag = SpCondFlag, FlagNote = SpCondFlagNote)
  
  ph.flags <- wq.flags.predata %>%
    dplyr::filter(pHFlag %in% c("I", "W", "C")) %>%
    dplyr::ungroup() %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, pHMedian, pHFlag, pHFlagNote) %>%
    tibble::add_column(Parameter = "pH", Units = "units", .after = "VisitDate") %>%
    dplyr::rename(Median = pHMedian, Flag = pHFlag, FlagNote = pHFlagNote)
  
  do.percent.flags <- wq.flags.predata %>%
    dplyr::filter(DOFlag %in% c("I", "W", "C")) %>%
    dplyr::ungroup() %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, DOPercentMedian, DOFlag, DOFlagNote) %>%
    tibble::add_column(Parameter = "DO", Units = "%", .after = "VisitDate") %>%
    dplyr::rename(Median = DOPercentMedian, Flag = DOFlag, FlagNote = DOFlagNote)
 
  do.mgl.flags <- wq.flags.predata %>%
    dplyr::filter(DOFlag %in% c("I", "W", "C")) %>%
    dplyr::ungroup() %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, DOmgLMedian, DOFlag, DOFlagNote) %>%
    tibble::add_column(Parameter = "DO", Units = "mg/L", .after = "VisitDate") %>%
    dplyr::rename(Median = DOmgLMedian, Flag = DOFlag, FlagNote = DOFlagNote)
  
  wq.flags <- rbind(temp.flags, spcond.flags, ph.flags, do.percent.flags, do.mgl.flags)
  return(wq.flags) 
  
}

#' Intermediate step used to clean water quality data for stats and plotting functions. Limit data to annual and 3Yr springs, and exclude data with "W" and "C" flags.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns for Park, FieldSeason, SiteCode, VisitDate, Parameter, Units, and Median.
#' @export
#'
#' @examples
QcWqCleaned <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  
  wq.cleaned.predata <- QcWqMedian(conn, path.to.data, park, site, field.season, data.source = "database")
  wq.visits <- ReadAndFilterData(conn = conn, path.to.data =  path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "Visit")
  
  wq.cleaned.data <- wq.cleaned.predata %>%
    left_join(select(wq.visits, SampleFrame, c("Park", "FieldSeason", "SiteCode", "VisitDate")), by = c("Park", "FieldSeason", "SiteCode", "VisitDate"))
  
  temp.cleaned <- wq.cleaned.data %>%
    dplyr::ungroup() %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, SampleFrame, TempMedian, TempFlag) %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr"), !TempFlag %in% c("W", "C")) %>%
    dplyr::group_by(Park, FieldSeason) %>%
    tibble::add_column(Parameter = "Temp", Units = "C", .after = "SampleFrame") %>%
    dplyr::rename(Median = TempMedian)
  
  spcond.cleaned <- wq.cleaned.data %>%
    dplyr::ungroup() %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, SampleFrame, SpCondMedian, SpCondFlag) %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr"), !SpCondFlag %in% c("W", "C")) %>%
    dplyr::group_by(Park, FieldSeason) %>%
    tibble::add_column(Parameter = "SpCond", Units = "uS/cm", .after = "SampleFrame") %>%
    dplyr::rename(Median = SpCondMedian)
  
  ph.cleaned <- wq.cleaned.data %>%
    dplyr::ungroup() %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, SampleFrame, pHMedian, pHFlag) %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr"), !pHFlag %in% c("W", "C")) %>%
    dplyr::group_by(Park, FieldSeason) %>%
    tibble::add_column(Parameter = "pH", Units = "Units", .after = "SampleFrame")  %>%
    dplyr::rename(Median = pHMedian)
  
  do.percent.cleaned <- wq.cleaned.data %>%
    dplyr::ungroup() %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, SampleFrame, DOPercentMedian, DOFlag) %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr"), !DOFlag %in% c("W", "C"), DOPercentMedian < 110) %>%
    dplyr::group_by(Park, FieldSeason) %>%
    tibble::add_column(Parameter = "DO", Units = "%", .after = "SampleFrame") %>%
    dplyr::rename(Median = DOPercentMedian)
  
  do.mgl.cleaned <- wq.cleaned.data %>%
    dplyr::ungroup() %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, SampleFrame, DOmgLMedian, DOFlag) %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr"), !DOFlag %in% c("W", "C"), DOmgLMedian < 12) %>%
    dplyr::group_by(Park, FieldSeason) %>%
    tibble::add_column(Parameter = "DO", Units = "mg/L", .after = "SampleFrame") %>%
    dplyr::rename(Median = DOmgLMedian)
  
  wq.cleaned.joined <- rbind(temp.cleaned, spcond.cleaned, ph.cleaned, do.percent.cleaned, do.mgl.cleaned)
  
  wq.cleaned <- wq.cleaned.joined %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, Parameter, Units, Median)
  
  return(wq.cleaned)
  
}

#' Calculate quartile values for each water quality parameter for each park and year. Includes annual and 3Yr springs only.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns for Park; FieldSeason; Parameter; Units; and 0%, 25%, 50%, 75%, and 100% quantiles.
#' @export
#'
#' @examples
#' 
QcWqStats <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  
  wq.stats.predata <- desertsprings:::QcWqCleaned(conn, path.to.data, park, site, field.season, data.source = "database")
  
  wq.stats <- wq.stats.predata %>%
    dplyr::group_by(Park, FieldSeason, Parameter, Units) %>%
    dplyr::summarise(stats = list(quantile(Median, type = 6, na.rm = TRUE))) %>% 
    tidyr::unnest_wider(stats)
  
  return(wq.stats)
  
}

#' Generate box and whisker plot for each water quality parameter for each park and year. Includes annual and 3Yr springs only.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return Box and whisker plots of the four water quality parameters for each park and field season.
#' @export
#'
#' @examples
QcWqPlots <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  
  wq.plots <- QcWqCleaned(conn, path.to.data, park, site, field.season, data.source = "database")
  
  wq.plots.temp <- ggplot(subset(wq.plots, Parameter == "Temp" & !Park == "CAMO"), aes(x=FieldSeason, y=Median)) + 
    geom_boxplot() +
    xlab("") + 
    ylab("Water Temperature (C)") + 
    theme(axis.text.x=element_text(angle=90)) +
    facet_grid(~Park,scales="free")
  
  wq.plots.spcond <- ggplot(subset(wq.plots, Parameter == "SpCond" & !Park == "CAMO"), aes(x=FieldSeason, y=Median)) + 
    geom_boxplot() +
    xlab("") + 
    ylab("Specific Conductance (uS/cm)") + 
    theme(axis.text.x=element_text(angle=90)) +
    facet_grid(~Park,scales="free") +
    scale_y_log10(breaks = c(200, 500, 1000, 2000, 5000, 10000, 25000), limits = c(200, 25000))
  
  wq.plots.spcond.ms <- ggplot(subset(wq.plots, Parameter == "SpCond" & !Park == "CAMO"), aes(x=FieldSeason, y=Median/1000)) + 
    geom_boxplot() +
    xlab("") + 
    ylab("Specific Conductance (mS/cm)") + 
    theme(axis.text.x=element_text(angle=90)) +
    facet_grid(~Park,scales="free") +
    scale_y_log10(breaks = c(0.2, 0.5, 1, 2, 5, 10, 25), labels = c(0.2, 0.5, 1, 2, 5, 10, 25), limits = c(0.2, 25))
  
  wq.plots.ph <- ggplot(subset(wq.plots, Parameter == "pH" & !Park == "CAMO"), aes(x=FieldSeason, y=Median)) + 
    geom_boxplot() +
    xlab("") +
    ylab("pH") + 
    theme(axis.text.x=element_text(angle=90)) +
    facet_grid(~Park,scales="free")
  
  wq.plots.do.pct <- ggplot(subset(wq.plots, Parameter == "DO" & Units == "%" & !Park == "CAMO"), aes(x=FieldSeason, y=Median)) + 
    geom_boxplot() +
    xlab("") +
    ylab("Dissolved Oxygen (%)") + 
    theme(axis.text.x=element_text(angle=90)) +
    facet_grid(~Park,scales="free") +
    ylim(0,100)
  
  wq.plots.do.mgl <- ggplot(subset(wq.plots, Parameter == "DO" & Units == "mg/L" & !Park == "CAMO"), aes(x=FieldSeason, y=Median)) + 
    geom_boxplot() +
    xlab("") +
    ylab("Dissolved Oxygen (mg/L)") + 
    theme(axis.text.x=element_text(angle=90)) +
    facet_grid(~Park,scales="free")
   
  grid.arrange(wq.plots.temp, wq.plots.spcond.ms, wq.plots.ph, wq.plots.do.mgl, ncol=1)
  
}