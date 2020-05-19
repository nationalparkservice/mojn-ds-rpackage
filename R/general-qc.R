#' Calculate completeness and print table (\% of planned sites visited)
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#' @param data.name The name of the analysis view or the csv file containing the data. E.g. "CalibrationDO", "DischargeVolumetric". See details for full list of data name options.
#'
#' @return A tibble.
#' @export
#' 
QcCompleteness <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

  completeness <- ReadAndFilterData(conn = conn, path.to.data =  path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "Visit")
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

#' Plot completeness (\% of planned sites visited)
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble.
#' @export
#' 
QcCompletenessPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

  completeness.plot <- QcCompleteness(conn = conn, path.to.data =  path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  df <- completeness.plot %>%
    mutate(SampleStatus = paste(SampleFrame, MonitoringStatus, sep=" - "))

  ggplot(df, aes(fill=SampleStatus,x=Park,y=Count)) +
    geom_bar(position="stack",stat="identity") +
    xlab("Park") +
    ylab("Number of Springs Monitored") + 
    facet_grid(~FieldSeason,scales="free") +
    theme(axis.text.x=element_text(angle=90)) +
    scale_y_continuous(breaks=seq(0,80,10))
  
}