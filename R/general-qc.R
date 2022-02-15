#' Calculate completeness (\% of planned sites visited).
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#' @param data.name The name of the analysis view or the csv file containing the data. E.g. "CalibrationDO", "DischargeVolumetric". See details for full list of data name options.
#'
#' @return A tibble with columns for park, field season, sample frame (i.e., annual, 3Yr), monitoring status (i.e., sampled), count of springs monitored, and percent of springs monitored.
#' @export
#' 
qcCompleteness <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

  completeness <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "Visit")
  df <- completeness %>%
    dplyr::group_by(Park, FieldSeason, SampleFrame, VisitType, MonitoringStatus) %>%
    dplyr::summarize(Count = dplyr::n()) %>%
    dplyr::filter(SampleFrame %in% c("Annual","3Yr") & VisitType == "Primary" & !MonitoringStatus %in% c("Not sampled - No spring found","Not sampled - Inaccessible")) %>%
    dplyr::mutate(Percent = ifelse(Park == "DEVA" & SampleFrame == "3Yr", Count/60*100,
                       ifelse(Park == "DEVA" & SampleFrame == "Annual", Count/20*100,
                         ifelse(Park %in% c("JOTR", "LAKE", "MOJA", "PARA") & SampleFrame == "Annual", Count/10*100,
                           ifelse(Park %in% c("MOJA", "PARA") & SampleFrame == "3Yr", Count/35*100,
                             ifelse(Park == "JOTR" & SampleFrame == "3Yr", Count/25*100,
                               ifelse(Park == "LAKE" & SampleFrame == "3Yr", Count/30*100, NA
                                 ))))))) %>%
    dplyr::mutate(Percent = round(Percent, 3))
  
  return(df %>%
    dplyr::arrange(Park,FieldSeason,desc(SampleFrame)) %>%
    dplyr::ungroup() %>%
    dplyr::select(Park, FieldSeason, SampleFrame, MonitoringStatus, Count, Percent))

}

#' Generate stacked bar plot for completeness (\% of planned sites visited).
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A stacked bar graph showing the count of annual and 3Yr springs monitored for each park and field season.
#' @export
#' 
qcCompletenessPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

  completeness.plot <- qcCompleteness(conn = conn, path.to.data =  path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  df <- completeness.plot %>%
    mutate(SampleStatus = paste(SampleFrame, MonitoringStatus, sep=" - "))

  ggplot(df, aes(fill=SampleStatus,x=FieldSeason,y=Count)) +
    geom_bar(position="stack",stat="identity") +
    xlab("Park") +
    ylab("Number of Springs Monitored") + 
    facet_grid(~Park,scales="free") +
    theme(axis.text.x=element_text(angle=90)) +
    scale_y_continuous(breaks=seq(0,80,10))
  
}


#' Return list of site visits that have any data categorized as "Raw" or "Provisional"
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble
#' @export
#'
#' @examples
qcDPLCheck <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  
  visit <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Visit")
  flowcondition <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "DischargeFlowCondition")
  estimated <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "DischargeEstimated")
  volumetric <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "DischargeVolumetric")
  disturbance <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Disturbance")
  flowmod <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "DisturbanceFlowModification")
  invasives <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Invasives")
  riparian <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Riparian")
  wildlife <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "Wildlife")
  temp <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "WaterQualityTemperature")
  ph <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "WaterQualitypH")
  spcond <-ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "WaterQualitySpCond")
  do <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, data.source = data.source, data.name = "WaterQualityDO")

visit.DPL <- visit %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, VisitType, DPL) %>%
  dplyr::rename(Visit = DPL)
flowcondition.DPL <- flowcondition %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(FlowCondition = DPL) %>%
  distinct()
estimated.DPL <- estimated %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(EstimatedDischarge = DPL) %>%
  distinct()
volumetric.DPL <- volumetric %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(VolumetricDischarge = DPL) %>%
  distinct()
disturbance.DPL <- disturbance %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(Disturbance = DPL) %>%
  distinct()
flowmod.DPL <- flowmod %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(FlowModification = DPL) %>%
  distinct()
wildlife.DPL <- wildlife %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(Wildlife = DPL) %>%
  distinct()
riparian.DPL <- riparian %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(Riparian = DPL)
invasives.DPL <- invasives %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(Invasives = DPL)
temp.DPL <- temp %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(Temperature = DPL) %>%
  distinct()
ph.DPL <- ph %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(pH = DPL) %>%
  distinct()
spcond.DPL <- spcond %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(SpCond = DPL) %>%
  distinct()
do.DPL <- do %>%
  dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
  dplyr::rename(DisOxygen = DPL) %>%
  distinct()

dpl <- visit.DPL %>%
  dplyr::left_join(flowcondition.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(estimated.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(volumetric.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(disturbance.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(flowmod.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(wildlife.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(riparian.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(invasives.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(temp.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(ph.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(spcond.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  dplyr::left_join(do.DPL, by = c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
  unique() %>%
  dplyr::filter_all(any_vars(. %in% c("Raw", "Provisional"))) %>%
  dplyr::arrange(FieldSeason, Park, SiteCode)
 
  return(dpl) 
}

#' Return list of springs that have been given different classifications
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble
#' @export
#'
#' @examples
qcSpringTypeDiscrepancies <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  visit <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "Visit") 
  
  discrepancies <- visit %>%
    dplyr::filter(VisitType == "Primary", MonitoringStatus == "Sampled") %>%
    dplyr::arrange(SiteCode, VisitDate) %>%
    dplyr::select(Park, SiteCode, SiteName, FieldSeason, SpringType) %>%
    dplyr::group_by(Park, SiteCode, SiteName, SpringType) %>%
    dplyr::mutate(FieldSeasons = paste0(FieldSeason, collapse = ", ")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("FieldSeason")) %>%
    unique() %>%
    dplyr::filter(duplicated(SiteCode) | duplicated(SiteCode, fromLast = TRUE))

   return(discrepancies)
}

#' Return list of dates that each spring has been visited 
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble
#' @export
#'
#' @examples
qcVisitDate <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  visit <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "Visit") 
 
  visit.dates <- visit %>%
    dplyr::filter(VisitType == "Primary", MonitoringStatus == "Sampled", SampleFrame %in% c("Annual", "3Yr")) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame) %>%
    dplyr::mutate(VisitDate = as.POSIXct(VisitDate)) %>%
    dplyr::mutate(Month = as.factor(format(VisitDate, "%b"))) %>%
    dplyr::mutate(Day = as.integer(format(VisitDate, "%d"))) %>%
    dplyr::mutate(Month = factor(Month, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))) %>%
    dplyr::arrange(Month, Day) %>%
    dplyr::mutate(Date = paste(Month, Day, sep = " ")) %>%
    dplyr::mutate(Date = paste0(Date, " (", FieldSeason, ")")) %>%
    dplyr::select(Park, SiteCode, SiteName, SampleFrame, Date) %>%
    dplyr::group_by(Park, SiteCode, SiteName, SampleFrame) %>%
    dplyr::mutate(VisitDates = paste0(Date, collapse = ", ")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-Date) %>%
    dplyr::arrange(SiteCode)
  
  return(visit.dates)
}

#' Plot timeline of dates that each spring has been visited 
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return
#' @export
#'
#' @examples
qcVisitDatePlots <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  visit <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "Visit") 
  
  visit.dates <- visit %>%
    dplyr::filter(VisitType == "Primary", MonitoringStatus == "Sampled", SampleFrame %in% c("Annual", "3Yr")) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame) %>%
    dplyr::mutate(Month = as.factor(format(VisitDate, "%b"))) %>%
    dplyr::mutate(MonthNum = as.integer(format(VisitDate, "%m"))) %>%
    dplyr::mutate(Day = as.integer(format(VisitDate, "%d"))) %>%
    dplyr::mutate(Month = factor(Month, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))) %>%
    dplyr::mutate(Date = paste(Month, Day, sep = " ")) %>%
    dplyr::mutate(Year = case_when(Month %in% c("Oct", "Nov", "Dec") ~ 2019,
                                   TRUE ~ 2020)) %>%
    dplyr::mutate(VisitDate = lubridate::ymd(paste(Year, Month, Day, sep ="-"))) %>%
    dplyr::mutate(VisitDate = as.POSIXct(VisitDate)) %>%
    dplyr::arrange(VisitDate) %>%
    dplyr::filter(SiteCode != "JOTR_P_BLA0045")

  positions <- c(0.5, -0.5, 1.0, -1.0, 1.25, -1.25, 1.5, -1.5) 
  directions <- c(1, -1) 
  
  line_pos <- data.frame(
    "VisitDate" = unique(visit.dates$VisitDate),
    "position" = rep(positions, length.out = length(unique(visit.dates$VisitDate))),
    "direction" = rep(directions, length.out = length(unique(visit.dates$VisitDate))))
  
  visit.dates <- merge(x = visit.dates, y = line_pos, by = "VisitDate", all = TRUE)
  
  month_date_range <- seq(as.POSIXct(strptime("2019-10-01 00:00:00", "%Y-%m-%d %H:%M:%S")), as.POSIXct(strptime("2020-09-30 00:00:00", "%Y-%m-%d %H:%M:%S")), by = 'month')
  month_format <- format(month_date_range, '%b')
  month_df <- data.frame(month_date_range, month_format)
  
  text_offset <- 0.1 

  absolute_value <- (abs(visit.dates$position)) 
  text_position <- absolute_value + text_offset
  
  visit.dates$text_position <- text_position * visit.dates$direction 
  
  timeline <- ggplot2::ggplot(visit.dates,
                              aes(x = VisitDate, y = position, label = Date)) +
    ggplot2::geom_hline(yintercept = 0, alpha = 0.5, linetype = "dashed") +
    ggplot2::geom_segment(data = visit.dates, aes(y = position, yend = 0, xend = VisitDate), color = 'black', size = 0.2) +
    ggplot2::geom_point(aes(y = position), size = 3) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.line.y=element_blank(),
                   axis.text.y=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   axis.ticks.y=element_blank(),
                   axis.text.x =element_blank(),
                   axis.ticks.x =element_blank(),
                   axis.line.x =element_blank(),
                   legend.position = "bottom") +
    ggplot2::geom_text(data=month_df, aes(x=month_date_range, y = -0.1,label = month_format), size = 3.5, vjust = 0.5, color = 'black', angle = 90) +
    ggplot2::geom_text(aes(y = text_position, label = FieldSeason), size = 3.5) +
    ggplot2::facet_wrap(vars(SiteCode), ncol = 1)
  
  return(timeline)
}

qcNotSampled <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  visit <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "Visit") 

  notsampled <- visit %>%
    dplyr::filter(MonitoringStatus != "Sampled") %>%
    dplyr::select(-c("DPL", "SpringType"))
  
  return(notsampled)
}