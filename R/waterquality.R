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
WqMedian <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  temp <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "WaterQualityTemperature")
  spcond <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "WaterQualitySpCond")
  ph <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "WaterQualitypH")
  do <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "WaterQualityDO")

  wq.visits <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "Visit")

  temp.med <- temp %>%
    dplyr::left_join(dplyr::select(wq.visits, SampleFrame, c("Park", "FieldSeason", "SiteCode", "VisitDate")), by = c("Park", "FieldSeason", "SiteCode", "VisitDate")) %>%
    dplyr::filter(MonitoringStatus == "Sampled") %>%
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, DataQualityFlag, DataQualityFlagNote) %>%
    dplyr::summarise(TempMedian = median(WaterTemperature_C)) %>%
    dplyr::rename(TempFlag = DataQualityFlag, TempFlagNote = DataQualityFlagNote) %>%
    dplyr::arrange(SiteCode)

  spcond.med <- spcond %>%
    dplyr::left_join(dplyr::select(wq.visits, SampleFrame, c("Park", "FieldSeason", "SiteCode", "VisitDate")), by = c("Park", "FieldSeason", "SiteCode", "VisitDate")) %>%
    dplyr::filter(MonitoringStatus == "Sampled") %>%
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, DataQualityFlag, DataQualityFlagNote) %>%
    dplyr::summarise(SpCondMedian = median(SpecificConductance_microS_per_cm)) %>%
    dplyr::rename(SpCondFlag = DataQualityFlag, SpCondFlagNote = DataQualityFlagNote) %>%
    dplyr::arrange(SiteCode)

  ph.med <- ph %>%
    dplyr::left_join(dplyr::select(wq.visits, SampleFrame, c("Park", "FieldSeason", "SiteCode", "VisitDate")), by = c("Park", "FieldSeason", "SiteCode", "VisitDate")) %>%
    dplyr::filter(MonitoringStatus == "Sampled") %>%
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, DataQualityFlag, DataQualityFlagNote) %>%
    dplyr::summarise(pHMedian = median(pH)) %>%
    dplyr::rename(pHFlag = DataQualityFlag, pHFlagNote = DataQualityFlagNote) %>%
    dplyr::arrange(SiteCode)

  do.med <- do %>%
    dplyr::left_join(dplyr::select(wq.visits, SampleFrame, c("Park", "FieldSeason", "SiteCode", "VisitDate")), by = c("Park", "FieldSeason", "SiteCode", "VisitDate")) %>%
    dplyr::filter(MonitoringStatus == "Sampled") %>%
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, DataQualityFlag, DataQualityFlagNote) %>%
    dplyr::summarise(DOPercentMedian = median(DissolvedOxygen_percent), DOmgLMedian = median(DissolvedOxygen_mg_per_L)) %>%
    dplyr::rename(DOFlag = DataQualityFlag, DOFlagNote = DataQualityFlagNote) %>%
    dplyr::arrange(SiteCode)

  wq.med <- temp.med %>%
    dplyr::left_join(spcond.med, by = c("Park", "FieldSeason", "SiteCode", "VisitDate", "VisitType", "SampleFrame")) %>%
    dplyr::left_join(ph.med, by = c("Park", "FieldSeason", "SiteCode", "VisitDate", "VisitType", "SampleFrame")) %>%
    dplyr::left_join(do.med, by = c("Park", "FieldSeason", "SiteCode", "VisitDate", "VisitType", "SampleFrame")) %>%
    dplyr::ungroup()

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
qcWqSanity <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  wq.sanity.predata <- QcWqMedian(conn, path.to.data, park, site, field.season, data.source)

  temp.sanity <- wq.sanity.predata %>%
    dplyr::filter(TempMedian > 30) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, TempMedian, TempFlag, TempFlagNote) %>%
    tibble::add_column(Parameter = "Temp", Units = "C", .after = "SampleFrame") %>%
    dplyr::rename(Median = TempMedian, Flag = TempFlag, FlagNote = TempFlagNote)

  spcond.sanity <- wq.sanity.predata %>%
    dplyr::filter(SpCondMedian > 20000) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, SpCondMedian, SpCondFlag, SpCondFlagNote) %>%
    tibble::add_column(Parameter = "SpCond", Units = "uS/cm", .after = "SampleFrame") %>%
    dplyr::rename(Median = SpCondMedian, Flag = SpCondFlag, FlagNote = SpCondFlagNote)

  ph.sanity <- wq.sanity.predata %>%
    dplyr::filter(pHMedian > 10 | pHMedian < 6) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, pHMedian, pHFlag, pHFlagNote) %>%
    tibble::add_column(Parameter = "pH", Units = "units", .after = "SampleFrame") %>%
    dplyr::rename(Median = pHMedian, Flag = pHFlag, FlagNote = pHFlagNote)

  do.percent.sanity <- wq.sanity.predata %>%
    dplyr::filter(DOPercentMedian > 110 | DOPercentMedian < 2) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, DOPercentMedian, DOFlag, DOFlagNote) %>%
    tibble::add_column(Parameter = "DO", Units = "%", .after = "SampleFrame") %>%
    dplyr::rename(Median = DOPercentMedian, Flag = DOFlag, FlagNote = DOFlagNote)

  do.mgl.sanity <- wq.sanity.predata %>%
    dplyr::filter(DOmgLMedian > 12) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, DOmgLMedian, DOFlag, DOFlagNote) %>%
    tibble::add_column(Parameter = "DO", Units = "mg/L", .after = "SampleFrame") %>%
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
qcWqFlags <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  wq.flags.predata <- QcWqMedian(conn, path.to.data, park, site, field.season, data.source)

  temp.flags <- wq.flags.predata %>%
    dplyr::filter(TempFlag %in% c("I", "W", "C")) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, TempMedian, TempFlag, TempFlagNote) %>%
    tibble::add_column(Parameter = "Temp", Units = "C", .after = "SampleFrame") %>%
    dplyr::rename(Median = TempMedian, Flag = TempFlag, FlagNote = TempFlagNote)

  spcond.flags <- wq.flags.predata %>%
    dplyr::filter(SpCondFlag %in% c("I", "W", "C")) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, SpCondMedian, SpCondFlag, SpCondFlagNote) %>%
    tibble::add_column(Parameter = "SpCond", Units = "uS/cm", .after = "SampleFrame") %>%
    dplyr::rename(Median = SpCondMedian, Flag = SpCondFlag, FlagNote = SpCondFlagNote)

  ph.flags <- wq.flags.predata %>%
    dplyr::filter(pHFlag %in% c("I", "W", "C")) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, pHMedian, pHFlag, pHFlagNote) %>%
    tibble::add_column(Parameter = "pH", Units = "units", .after = "SampleFrame") %>%
    dplyr::rename(Median = pHMedian, Flag = pHFlag, FlagNote = pHFlagNote)

  do.percent.flags <- wq.flags.predata %>%
    dplyr::filter(DOFlag %in% c("I", "W", "C")) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, DOPercentMedian, DOFlag, DOFlagNote) %>%
    tibble::add_column(Parameter = "DO", Units = "%", .after = "SampleFrame") %>%
    dplyr::rename(Median = DOPercentMedian, Flag = DOFlag, FlagNote = DOFlagNote)

  do.mgl.flags <- wq.flags.predata %>%
    dplyr::filter(DOFlag %in% c("I", "W", "C")) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, DOmgLMedian, DOFlag, DOFlagNote) %>%
    tibble::add_column(Parameter = "DO", Units = "mg/L", .after = "SampleFrame") %>%
    dplyr::rename(Median = DOmgLMedian, Flag = DOFlag, FlagNote = DOFlagNote)

  wq.flags <- rbind(temp.flags, spcond.flags, ph.flags, do.percent.flags, do.mgl.flags)

  return(wq.flags)
}

#' Intermediate step used to clean water quality data for stats and plotting functions. Limit data to primary visits of annual and 3Yr springs, and exclude data with "W" and "C" flags.
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
qcWqLong <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  wq.cleaned.data <- QcWqMedian(conn, path.to.data, park, site, field.season, data.source)

  temp.cleaned <- wq.cleaned.data %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr"), !(TempFlag %in% c("W", "C")), VisitType %in% c("Primary")) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, SampleFrame, TempMedian) %>%
    dplyr::group_by(Park, FieldSeason) %>%
    tibble::add_column(Parameter = "Temp", Units = "C", .after = "SampleFrame") %>%
    dplyr::rename(Median = TempMedian)

  spcond.cleaned <- wq.cleaned.data %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr"), !(SpCondFlag %in% c("W", "C")), VisitType %in% c("Primary")) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, SampleFrame, SpCondMedian) %>%
    dplyr::group_by(Park, FieldSeason) %>%
    tibble::add_column(Parameter = "SpCond", Units = "uS/cm", .after = "SampleFrame") %>%
    dplyr::rename(Median = SpCondMedian)

  ph.cleaned <- wq.cleaned.data %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr"), !(pHFlag %in% c("W", "C")), VisitType %in% c("Primary")) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, SampleFrame, pHMedian) %>%
    dplyr::group_by(Park, FieldSeason) %>%
    tibble::add_column(Parameter = "pH", Units = "units", .after = "SampleFrame") %>%
    dplyr::rename(Median = pHMedian)

  do.percent.cleaned <- wq.cleaned.data %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr"), !(DOFlag %in% c("W", "C")), VisitType %in% c("Primary"), (DOPercentMedian < 110 | is.na(DOPercentMedian))) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, SampleFrame, DOPercentMedian) %>%
    dplyr::group_by(Park, FieldSeason) %>%
    tibble::add_column(Parameter = "DO", Units = "%", .after = "SampleFrame") %>%
    dplyr::rename(Median = DOPercentMedian)

  do.mgl.cleaned <- wq.cleaned.data %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr"), !(DOFlag %in% c("W", "C")), VisitType %in% c("Primary"), (DOmgLMedian < 12 | is.na(DOmgLMedian))) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, SampleFrame, DOmgLMedian) %>%
    dplyr::group_by(Park, FieldSeason) %>%
    tibble::add_column(Parameter = "DO", Units = "mg/L", .after = "SampleFrame") %>%
    dplyr::rename(Median = DOmgLMedian)

  wq.cleaned <- rbind(temp.cleaned, spcond.cleaned, ph.cleaned, do.percent.cleaned, do.mgl.cleaned) %>%
    dplyr::ungroup()

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
WqStats <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  wq.stats.predata <- qcWqLong(conn, path.to.data, park, site, field.season, data.source)

  wq.stats <- wq.stats.predata %>%
    dplyr::group_by(Park, FieldSeason, Parameter, Units) %>%
    dplyr::summarise(stats = list(quantile(Median, type = 6, na.rm = TRUE))) %>%
    tidyr::unnest_wider(stats) %>%
    dplyr::ungroup()

  wq.stats[wq.stats$Parameter == "DO" & wq.stats$Units == "%", ] %<>% dplyr::mutate_if(is.double, ~ round(., 1))
  wq.stats[wq.stats$Parameter == "DO" & wq.stats$Units == "mg/L", ] %<>% dplyr::mutate_if(is.double, ~ round(., 2))
  wq.stats[wq.stats$Parameter == "SpCond", ] %<>% dplyr::mutate_if(is.double, ~ round(., 0))
  wq.stats[wq.stats$Parameter == "pH", ] %<>% dplyr::mutate_if(is.double, ~ round(., 2))
  wq.stats[wq.stats$Parameter == "Temp", ] %<>% dplyr::mutate_if(is.double, ~ round(., 1))

  return(wq.stats)
}

#' Generate box plots for water temperature for each park and year. Includes annual and 3Yr springs only.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#' @param include.title Include plot title? Defaults to TRUE
#'
#' @return Box plots of water temperature data for each park and field season.
#' @export
#'
WqPlotTemp <- function(conn, path.to.data, park, site, field.season, data.source = "database", include.title = TRUE) {
  
  wq.plot <- qcWqLong(conn, path.to.data, park, site, field.season, data.source) %>%
    dplyr::filter(Parameter == "Temp" & Park != "CAMO" & !is.na(Median)) %>%
    GetSampleSizes(Park, FieldSeason)
  
  wq.plot.temp <- FormatPlot(
    data = wq.plot,
    x.col = FieldSeason,
    y.col = Median,
    facet.col = Park,
    sample.size.col = SampleSizeLabel,
    sample.size.loc = "xaxis",
    plot.title = dplyr::if_else(include.title, "Water Temperature", ""),
    facet.as.subtitle = include.title,
    x.lab = "Field Season",
    y.lab = "Temperature (C)"
  ) +
    ggplot2::geom_boxplot()

  return(wq.plot.temp)
}

#' Generate box plots for specific conductance for each park and year. Includes annual and 3Yr springs only.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return Box plots of specific conductance data for each park and field season.
#' @export
#'
WqPlotSpCond <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  wq.plot <- qcWqLong(conn, path.to.data, park, site, field.season, data.source)

  wq.plot.spcond <- ggplot2::ggplot(subset(wq.plot, Parameter == "SpCond" & !Park == "CAMO"), ggplot2::aes(x = FieldSeason, y = Median)) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("") +
    ggplot2::ylab("Specific Conductance (uS/cm)") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::facet_grid(~Park, scales = "free") +
    ggplot2::scale_y_log10(breaks = c(200, 500, 1000, 2000, 5000, 10000, 25000), limits = c(200, 25000))

  return(wq.plot.spcond)
}

#' Generate box plots for pH for each park and year. Includes annual and 3Yr springs only.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return Box plots of pH data for each park and field season.
#' @export
#'
WqPlotPH <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  wq.plot <- qcWqLong(conn, path.to.data, park, site, field.season, data.source)

  wq.plot.ph <- ggplot2::ggplot(subset(wq.plot, Parameter == "pH" & !Park == "CAMO"), ggplot2::aes(x = FieldSeason, y = Median)) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("") +
    ggplot2::ylab("pH") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::facet_grid(~Park, scales = "free")

  return(wq.plot.ph)
}

#' Generate box plots for percent dissolved oxygen for each park and year. Includes annual and 3Yr springs only.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return Box plots of dissolved oxygen (percent) data for each park and field season.
#' @export
#'
WqPlotDOPct <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  wq.plot <- qcWqLong(conn, path.to.data, park, site, field.season, data.source)

  wq.plot.do.pct <- ggplot2::ggplot(subset(wq.plot, Parameter == "DO" & Units == "%" & !Park == "CAMO"), ggplot2::aes(x = FieldSeason, y = Median)) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("") +
    ggplot2::ylab("Dissolved Oxygen (%)") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::facet_grid(~Park, scales = "free") +
    ggplot2::ylim(0, 100)

  return(wq.plot.do.pct)
}

#' Generate box plots for concentration dissolved oxygen for each park and year. Includes annual and 3Yr springs only.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return Box plots of dissolved oxygen (mg/L) data for each park and field season.
#' @export
#'
WqPlotDOmgL <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  wq.plot <- qcWqLong(conn, path.to.data, park, site, field.season, data.source)

  wq.plot.do.mgl <- ggplot2::ggplot(subset(wq.plot, Parameter == "DO" & Units == "mg/L" & !Park == "CAMO"), ggplot2::aes(x = FieldSeason, y = Median)) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("") +
    ggplot2::ylab("Dissolved Oxygen (mg/L)") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::facet_grid(~Park, scales = "free")

  return(wq.plot.do.mgl)
}

#' Generate grid of box plots for core water quality parameters for each park and year. Includes annual and 3Yr springs only.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return Grid of box plots of water quality parameter data (temp C, spcond mS/cm, pH, DO mg/L) for each park and field season.
#' @export
#'
WqPlotGrid <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  wq.plot.temp <- WqPlotTemp(conn, path.to.data, park, site, field.season, data.source)
  wq.plot.ph <- WqPlotPH(conn, path.to.data, park, site, field.season, data.source)
  wq.plot.do.mgl <- WqPlotDOmgL(conn, path.to.data, park, site, field.season, data.source)

  wq.plot <- qcWqLong(conn, path.to.data, park, site, field.season, data.source)

  wq.plot.spcond.ms <- ggplot2::ggplot(subset(wq.plot, Parameter == "SpCond" & !Park == "CAMO"), ggplot2::aes(x = FieldSeason, y = Median / 1000)) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("") +
    ggplot2::ylab("Specific Conductance (mS/cm)") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::facet_grid(~Park, scales = "free") +
    ggplot2::scale_y_log10(breaks = c(0.2, 0.5, 1, 2, 5, 10, 25), labels = c(0.2, 0.5, 1, 2, 5, 10, 25), limits = c(0.2, 25))

  wq.plot.grid <- gridExtra::grid.arrange(wq.plot.temp, wq.plot.spcond.ms, wq.plot.ph, wq.plot.do.mgl, ncol = 1)

  return(wq.plot.grid)
}