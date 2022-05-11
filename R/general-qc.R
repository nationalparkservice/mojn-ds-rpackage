#' Calculate completeness (\% of planned sites visited)
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
  site <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "Site")
  
  df1 <- site %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr"),
                  SiteStatus == "T-S") %>%
    dplyr::select(Park,
                  SiteCode,
                  SiteName,
                  SampleFrame)
  
  
  df2 <- completeness %>%
    dplyr::filter(SampleFrame %in% c("Annual","3Yr"),
                  VisitType == "Primary",
                  MonitoringStatus == "Sampled") %>%
    dplyr::select(Park, SiteCode, SiteName, FieldSeason, SampleFrame, MonitoringStatus) %>%
    dplyr::group_by(Park, FieldSeason) %>%
    dplyr::mutate(Triennial = case_when(Park %in% c("LAKE", "MOJA") & (as.numeric(FieldSeason) - 2016) %% 3 == 0  ~ "Y",
                                        Park %in% c("JOTR", "PARA") & (as.numeric(FieldSeason) - 2017) %% 3 == 0  ~ "Y",
                                        Park %in% c("DEVA") & (as.numeric(FieldSeason) - 2018) %% 3 == 0 ~ "Y",
                                        Park %in% c("CAMO") & (as.numeric(FieldSeason) - 2017) %% 3 == 0 ~ "Y",
                                        TRUE ~ "N")) %>%
    # dplyr::filter(Triennial == "Y") %>%
    dplyr::ungroup() %>%
    dplyr::select(Park, FieldSeason, SampleFrame) %>%
    unique()
  
  expected <- df1 %>%
    dplyr::left_join(df2)
  
  
  samplestatus <- completeness %>%
    dplyr::filter(SampleFrame %in% c("Annual","3Yr"),
                  VisitType == "Primary",
                  MonitoringStatus == "Sampled") %>%
    dplyr::select(Park, SiteCode, SiteName, FieldSeason, SampleFrame, MonitoringStatus) %>%
    dplyr::right_join(expected, by = c("Park", "SiteCode", "SiteName", "SampleFrame", "FieldSeason")) %>%
    dplyr::mutate(MonitoringStatus = case_when(is.na(MonitoringStatus) ~ "Not Sampled",
                                               TRUE ~ MonitoringStatus)) %>%
    dplyr::group_by(Park, FieldSeason, SampleFrame, MonitoringStatus) %>%
    dplyr::summarize(Count = dplyr::n()) %>%
    dplyr::mutate(Percent = dplyr::case_when(Park == "DEVA" & SampleFrame == "3Yr" ~ Count/60*100,
                                             Park == "DEVA" & SampleFrame == "Annual" ~ Count/20*100,
                                             Park %in% c("JOTR", "LAKE", "MOJA", "PARA") & SampleFrame == "Annual" ~ Count/10*100,
                                             Park %in% c("MOJA", "PARA") & SampleFrame == "3Yr" ~ Count/35*100,
                                             Park == "JOTR" & SampleFrame == "3Yr" ~ Count/25*100,
                                             Park == "LAKE" & SampleFrame == "3Yr" ~ Count/33*100,
                                             Park == "CAMO" & SampleFrame == "3Yr" ~ Count/2*100,
                                             TRUE ~ as.double(NA))) %>%
    dplyr::mutate(Percent = round(Percent, 3)) %>%
    dplyr::arrange(Park, FieldSeason, desc(SampleFrame)) %>%
    dplyr::ungroup() %>%
    dplyr::select(Park, FieldSeason, SampleFrame, MonitoringStatus, Count, Percent)
  
  return(samplestatus)

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

  completecount <- qcCompleteness(conn = conn, path.to.data =  path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)

  df2 <- completecount %>%
        dplyr::mutate(SampleStatus = paste(SampleFrame, MonitoringStatus, sep = " - ")) %>%
        dplyr::filter(Park != "CAMO")

  
  
  completeness.plot <- ggplot(df2, aes(fill = SampleStatus, x = FieldSeason, y = Count)) +
    geom_bar(position = "stack", stat = "identity") +
    xlab("Park") +
    ylab("Number of Springs Monitored") + 
    facet_grid(~Park, space = "free_x") +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_y_continuous(breaks = seq(0, 80, 10)) +
    scale_fill_manual(values = c("rosybrown2", "firebrick", "lightblue", "steelblue"))

    return(completeness.plot)    
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
    dplyr::arrange(SiteCode) %>%
    unique()
  
  return(visit.dates)
}


#' Apply some standard formatting to a ggplot object
#'
#' @param plot_title The title of the plot.
#' @param sub_title Optional custom plot subtitle.
#' @param x_lab X axis label.
#' @param y_lab Y axis label.
#' @param rotate_x_labs Boolean indicating whether to rotate x axis labels 90 degrees.
#' @param ymax Optional maximum y limit.
#' @param ymin Optional minimum y limit.
#' @param xmax Optional maximum x limit.
#' @param xmin Optional minimum x limit.
#' @param data Data frame containing the data to be plotted.
#' @param x_col Column name of independent variable. If plot type only requires one variable (e.g. histogram), use only one of x_col or y_col.
#' @param y_col Column name of dependent variable. If plot type only requires one variable (e.g. histogram), use only one of x_col or y_col.
#' @param facet_col Column to facet on. If this results in only one facet, it will be used as a subtitle instead.
#' @param n_col_facet Number of columns of facet grid.
#' @param facet_scales String indicating whether x and/or y scales should be fixed in a facetted plot.
#' @param sample_size_col Column containing sample size labels.
#' @param sample_size_loc Either 'xaxis' or 'plot'. 'xaxis' will add sample size to each x axis label. 'plot' will add sample size to the facet label (or subtitle, if only one facet).
#' @param facet_as_subtitle If only one facet, use facet name as subtitle? Defaults to TRUE.
#' @param transform_x Optional x axis transformation. One of 'log10', 'sqrt', or 'reverse'.
#' @param transform_y Optional y axis transformation. One of 'log10', 'sqrt', or 'reverse'.
#'
#' @return A ggplot object
#'
#' @export
#'
FormatPlot <- function(data, x_col, y_col, facet_col, n_col_facet = 2, facet_scales = c("fixed", "free_x", "free_y", "free"), sample_size_col, sample_size_loc, plot_title = '', sub_title = '', facet_as_subtitle = TRUE, x_lab = '', y_lab = '', rotate_x_labs = FALSE, ymax, ymin, xmax, xmin, transform_x, transform_y) {
  facet_scales <- match.arg(facet_scales)
  # Add sample size information to either x axis labels or facet/subtitle
  if (!missing(sample_size_col) && !missing(sample_size_loc)) {
    sample_size_col <- dplyr::enquo(sample_size_col)
    if (sample_size_loc == 'xaxis') {
      data %<>% dplyr::mutate(!!x_col := paste0(!!x_col, '\n', !!sample_size_col))
    } else if (sample_size_loc == 'plot' && !missing(facet_col)) {
      data %<>% dplyr::mutate(!!dplyr::enquo(facet_col) := paste0(!!dplyr::enquo(facet_col), ' (', !!sample_size_col, ')'))
    } else {
      facet_col <- sample_size_col
    }
  }
  # Allow for 1 or 2 variables
  if (!missing(y_col) && !missing(x_col)) {
    y_col <- dplyr::enquo(y_col)
    x_col <- dplyr::enquo(x_col)
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_col, y = !!y_col))
  } else if (!missing(x_col)) {
    x_col <- dplyr::enquo(x_col)
    p <- ggplot2::ggplot(data, ggplot2::aes(!!x_col))
  } else if (!missing(y_col)) {
    y_col <- dplyr::enquo(y_col)
    p <- ggplot2::ggplot(data, ggplot2::aes(!!y_col))
  }
  # Create facets if >1 event group, otherwise create subtitle
  if (!missing(facet_col)) {
    facet_col <- dplyr::enquo(facet_col)
    facets <- unique(dplyr::select(data, !!facet_col))
    if (nrow(facets) > 1) {
      p <- p + ggplot2::facet_wrap(ggplot2::vars(!!facet_col), ncol = n_col_facet, scales = facet_scales)
    } else if (sub_title == '' & facet_as_subtitle) {
      sub_title <- facets
    }
  }
  # Add title and subtitle if not blank
  if (!missing(plot_title) && plot_title != "") {
    p <- p + ggplot2::labs(title = plot_title)
  }
  if (!missing(sub_title) && sub_title != "") {
    p <- p + ggplot2::labs(subtitle = sub_title)
  }
  # Add x and y axis titles if not blank
  if (x_lab != "") {
    p <- p + ggplot2::xlab(x_lab)
  } else {
    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }
  if (y_lab != "") {
    p <- p + ggplot2::ylab(y_lab)
  } else {
    p <- p + ggplot2::theme(axis.title.y = ggplot2::element_blank())
  }
  # Rotate x labels 90 degrees if rotate_x_labs is TRUE
  if (!missing(rotate_x_labs)) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
  }
  # Set ymin and ymax if provided
  if (!missing(ymin) && !missing(ymax)) {
    p <- p + ggplot2::expand_limits(y = c(ymin, ymax))
  } else if (!missing(ymax)) {
    p <- p + ggplot2::expand_limits(y = ymax)
  } else if (!missing(ymin)) {
    p <- p + ggplot2::expand_limits(y = ymin)
  }
  # Set xmin and xmax if provided
  if (!missing(xmin) && !missing(xmax)) {
    p <- p + ggplot2::expand_limits(x = c(xmin, xmax))
  } else if (!missing(xmax)) {
    p <- p + ggplot2::expand_limits(x = xmax)
  } else if (!missing(xmin)) {
    p <- p + ggplot2::expand_limits(x = xmin)
  }
  # Tranform x axis, if transformation specified
  if (!missing(transform_x)) {
    if (transform_x == 'log10') {
      p <- p + ggplot2::scale_x_log10()
    } else if (transform_x == 'sqrt') {
      p <- p + ggplot2::scale_x_sqrt()
    } else if (transform_x == 'reverse') {
      p <- p + ggplot2::scale_x_reverse()
    } else {
      stop(paste0("The x transformation specified, '", transform_x, "' is not a valid option."))
    }
  }
  # Transform y axis, if transformation specified
  if (!missing(transform_y)) {
    if (transform_y == 'log10') {
      p <- p + ggplot2::scale_y_log10()
    } else if (transform_y == 'sqrt') {
      p <- p + ggplot2::scale_y_sqrt()
    } else if (transform_y == 'reverse') {
      p <- p + ggplot2::scale_y_reverse()
    } else {
      stop(paste0("The y transformation specified, '", transform_y, "' is not a valid option."))
    }
  }
  return(p)
}


#' Generate timeline of dates that each spring has been visited
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
qcVisitDateTimelines <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  visit <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "Visit") 

  grouping_vars <- c("Park", "FieldSeason", "SiteCode") # Set grouping vars here so that we can add the facet column if needed
  median_grouping_vars <- c("Park", "SiteName", "SiteCode")
  
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
    dplyr::mutate(VisitMonthDay = lubridate::ymd(paste(Year, Month, Day, sep ="-")),
                  pt_tooltip = paste(Date, as.character(lubridate::year(VisitDate))),
                  Event_mmdd = as.Date(paste0(as.character(lubridate::month(VisitMonthDay)), '-', as.character(lubridate::day(VisitMonthDay)), '-', as.character(Year)), format = "%m-%d-%Y")) %>%
    dplyr::arrange(VisitDate) %>%
    dplyr::filter(SiteCode != "JOTR_P_BLA0045")
  
  median.dates <- visit.dates %>%
    dplyr::group_by(dplyr::across(median_grouping_vars)) %>%
    dplyr::summarise(Median.Date = median(Event_mmdd),
                     Max = max(Event_mmdd),
                     Min = min(Event_mmdd),
                     Spread = max(Event_mmdd) - min(Event_mmdd)) %>%
    dplyr::mutate(med_tooltip = format(Median.Date, "Median: %b %d"),
                  min_tooltip = format(Min, "Median: %b %d"),
                  max_tooltip = format(Max, "Median: %b %d")) %>%
    dplyr::ungroup()

  plt <- FormatPlot(data = visit.dates,
                    x_col = Event_mmdd,
                    y_col = factor(SiteCode),
                    plot_title = "Timeline of Spring Visits",
                    x_lab = "Date",
                    y_lab = "Spring Code") +
    suppressWarnings(ggplot2::geom_point(ggplot2::aes(color = FieldSeason,
                                                      text = paste0("Site Name: ", SiteName, "<br>",
                                                                   "Site Code: ", SiteCode, "<br>",
                                                                   "Visit Date: ", pt_tooltip, "<br>",
                                                                   "Field Season: ", FieldSeason)),
                                         alpha = 0.7)) + # Using text aesthetic to make tooltips work with plotly. This generates a warning so we have to suppress it.
    ggplot2::geom_line(alpha = 0.4) +
    suppressWarnings(ggplot2::geom_point(ggplot2::aes(x = Median.Date,
                                                      shape = "median",
                                                      text = paste0("Site Name: ", SiteName, "<br>",
                                                                    "Site Code: ", SiteCode, "<br>",
                                                                    "Median Date: ", med_tooltip, "<br>",
                                                                    "Range (Days): ", Spread, "<br>",
                                                                    "Earliest: ", min_tooltip, "<br>",
                                                                    "Latest: ", max_tooltip)),
                                         data = median.dates,
                                         size = 1,
                                         alpha = 0.7)) +
    ggplot2::scale_shape_manual(values = c("median" = 3)) + # Do this so that the median symbol shows up in the legend
    ggplot2::labs(color = "FieldSeason",
                  shape = NULL) +
    ggplot2::scale_x_date(date_breaks = "1 month",
                          date_labels = "%b %e",
                          limits = c(lubridate::floor_date(min(visit.dates$Event_mmdd), "month"),
                                     lubridate::ceiling_date(max(visit.dates$Event_mmdd), "month"))) +
    ggplot2::scale_y_discrete(limits = rev)
  
  return(plt)
  
}


#' Return list of springs that were not sampled during a field season when they were intended to be monitored
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
qcNotSampled <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  visit <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "Visit") 

  notsampled <- visit %>%
    dplyr::filter(MonitoringStatus != "Sampled") %>%
    dplyr::select(-c("DPL", "SpringType"))
  
  return(notsampled)
}