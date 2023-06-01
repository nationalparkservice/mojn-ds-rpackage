#' Calculate median values for each water quality parameter for each site visit.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble with columns for park, field season, site code, visit date, and the median values, flags, and flag notes for temperature, specific conductance, pH, and dissolved oxygen.
#' @export
#'
#' @examples
#' \dontrun{
#'     WqMedian()
#'     WqMedian(site = "LAKE_P_GET0066", field.season = "2019")
#'     WqMedian(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
  WqMedian <- function(park, site, field.season) {
    temp <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "WaterQualityTemperature")
    spcond <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "WaterQualitySpCond")
    ph <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "WaterQualitypH")
    do <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "WaterQualityDO")
    
    wq.visits <- ReadAndFilterData( park = park, site = site, field.season = field.season, data.name = "Visit")

  temp.med <- temp %>%
    dplyr::left_join(dplyr::select(wq.visits, SampleFrame, c("Park", "FieldSeason", "SiteCode", "VisitDate")), by = c("Park", "FieldSeason", "SiteCode", "VisitDate"), multiple = "all") %>%
    dplyr::filter(MonitoringStatus == "Sampled") %>%
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, DataQualityFlag, DataQualityFlagNote) %>%
    dplyr::summarise(TemperatureMedian_C = median(WaterTemperature_C),
                     TemperatureCount = sum(!is.na(WaterTemperature_C))) %>%
    dplyr::rename(TemperatureFlag = DataQualityFlag, TemperatureFlagNote = DataQualityFlagNote) %>%
    dplyr::arrange(SiteCode)

  spcond.med <- spcond %>%
    dplyr::left_join(dplyr::select(wq.visits, SampleFrame, c("Park", "FieldSeason", "SiteCode", "VisitDate")), by = c("Park", "FieldSeason", "SiteCode", "VisitDate"), multiple = "all") %>%
    dplyr::filter(MonitoringStatus == "Sampled") %>%
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, DataQualityFlag, DataQualityFlagNote) %>%
    dplyr::summarise(SpCondMedian_microS_per_cm = median(SpecificConductance_microS_per_cm),
                     SpCondCount = sum(!is.na(SpecificConductance_microS_per_cm))) %>%
    dplyr::rename(SpCondFlag = DataQualityFlag, SpCondFlagNote = DataQualityFlagNote) %>%
    dplyr::arrange(SiteCode)

  ph.med <- ph %>%
    dplyr::left_join(dplyr::select(wq.visits, SampleFrame, c("Park", "FieldSeason", "SiteCode", "VisitDate")), by = c("Park", "FieldSeason", "SiteCode", "VisitDate"), multiple = "all") %>%
    dplyr::filter(MonitoringStatus == "Sampled") %>%
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, DataQualityFlag, DataQualityFlagNote) %>%
    dplyr::summarise(pHMedian = median(pH),
                     pHCount = sum(!is.na(pH))) %>%
    dplyr::rename(pHFlag = DataQualityFlag, pHFlagNote = DataQualityFlagNote) %>%
    dplyr::arrange(SiteCode)

  do.med <- do %>%
    dplyr::left_join(dplyr::select(wq.visits, SampleFrame, c("Park", "FieldSeason", "SiteCode", "VisitDate")), by = c("Park", "FieldSeason", "SiteCode", "VisitDate"), multiple = "all") %>%
    dplyr::filter(MonitoringStatus == "Sampled") %>%
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, DataQualityFlag, DataQualityFlagNote) %>%
    dplyr::summarise(DOMedian_Percent = median(DissolvedOxygen_percent),
                     DOPercentCount = sum(!is.na(DissolvedOxygen_percent)),
                     DOMedian_mg_per_L = median(DissolvedOxygen_mg_per_L),
                     DOmgLCount = sum(!is.na(DissolvedOxygen_mg_per_L))) %>%
    dplyr::rename(DOFlag = DataQualityFlag, DOFlagNote = DataQualityFlagNote) %>%
    dplyr::arrange(SiteCode)

  wq.med <- temp.med %>%
    dplyr::left_join(spcond.med, by = c("Park", "FieldSeason", "SiteCode", "VisitDate", "VisitType", "SampleFrame"), multiple = "all") %>%
    dplyr::left_join(ph.med, by = c("Park", "FieldSeason", "SiteCode", "VisitDate", "VisitType", "SampleFrame"), multiple = "all") %>%
    dplyr::left_join(do.med, by = c("Park", "FieldSeason", "SiteCode", "VisitDate", "VisitType", "SampleFrame"), multiple = "all") %>%
    dplyr::ungroup() %>%
    dplyr::filter(TemperatureCount != 0 | SpCondCount != 0 | pHCount != 0 | DOPercentCount != 0 | DOmgLCount != 0)

  return(wq.med)
}


#' Perform sanity check and compile list of potentially incorrect or outlier water quality values.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' 
#'
#' @return Tibble with columns for Park, FieldSeason, SiteCode, VisitDate, Parameter, Units, Median, Flag, and FlagNote.
#' @export
#'
#' @examples
#' \dontrun{
#'     qcWqSanity()
#'     qcWqSanity(site = "LAKE_P_HOT0065", field.season = "2019")
#'     qcWqSanity(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
qcWqSanity <- function(park, site, field.season) {
  wq.sanity.predata <- WqMedian(park = park, site = site, field.season = field.season)

  temp.sanity <- wq.sanity.predata %>%
    dplyr::filter(TemperatureMedian_C > 30) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, TemperatureMedian_C, TemperatureFlag, TemperatureFlagNote) %>%
    tibble::add_column(Parameter = "Temperature", Units = "C", .after = "SampleFrame") %>%
    dplyr::mutate(SanityNote = "Temperature > 30 C") %>%
    dplyr::rename(Value = TemperatureMedian_C, Flag = TemperatureFlag, FlagNote = TemperatureFlagNote)

  spcond.sanity <- wq.sanity.predata %>%
    dplyr::filter(SpCondMedian_microS_per_cm > 20000) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, SpCondMedian_microS_per_cm, SpCondFlag, SpCondFlagNote) %>%
    tibble::add_column(Parameter = "SpCond", Units = "uS/cm", .after = "SampleFrame") %>%
    dplyr::mutate(SanityNote = "Specific conductance > 20000 uS/cm") %>%
    dplyr::rename(Value = SpCondMedian_microS_per_cm, Flag = SpCondFlag, FlagNote = SpCondFlagNote)

  ph.sanity <- wq.sanity.predata %>%
    dplyr::filter(pHMedian > 10 | pHMedian < 6) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, pHMedian, pHFlag, pHFlagNote) %>%
    tibble::add_column(Parameter = "pH", Units = "units", .after = "SampleFrame") %>%
    dplyr::mutate(SanityNote = dplyr::case_when(pHMedian > 10 ~ "pH > 10",
                                                pHMedian < 6 ~ "pH < 6",
                                                TRUE ~ "NA")) %>%
    dplyr::rename(Value = pHMedian, Flag = pHFlag, FlagNote = pHFlagNote)

  do.percent.sanity <- wq.sanity.predata %>%
    dplyr::filter(DOMedian_Percent > 100 | DOMedian_Percent < 2) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, DOMedian_Percent, DOFlag, DOFlagNote) %>%
    tibble::add_column(Parameter = "DO", Units = "%", .after = "SampleFrame") %>%
    dplyr::mutate(SanityNote = dplyr::case_when(DOMedian_Percent > 100 ~ "Dissolved oxygen > 100 %",
                                                DOMedian_Percent < 2 ~ "Dissolved oxygen < 2 %",
                                                TRUE ~ "NA")) %>%
    dplyr::rename(Value = DOMedian_Percent, Flag = DOFlag, FlagNote = DOFlagNote)

  do.mgl.sanity <- wq.sanity.predata %>%
    dplyr::filter(DOMedian_mg_per_L > 14) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, DOMedian_mg_per_L, DOFlag, DOFlagNote) %>%
    tibble::add_column(Parameter = "DO", Units = "mg/L", .after = "SampleFrame") %>%
    dplyr::mutate(SanityNote = "Dissolved oxygen > 14 mg/L") %>%
    dplyr::rename(Value = DOMedian_mg_per_L, Flag = DOFlag, FlagNote = DOFlagNote)

  wq.sanity <- rbind(temp.sanity, spcond.sanity, ph.sanity, do.percent.sanity, do.mgl.sanity) %>%
    dplyr::relocate(SanityNote, .after = "Value") %>%
    dplyr::select(-Flag, -FlagNote)

  return(wq.sanity)
}


#' Compile list of water quality values that have data quality flags.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' 
#'
#' @return Tibble with columns for Park, FieldSeason, SiteCode, VisitDate, Parameter, Units, Median, Flag, and FlagNote.
#' @export
#'
#' @examples
#' \dontrun{
#'     qcWqFlags()
#'     qcWqFlags(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcWqFlags(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
qcWqFlags <- function(park, site, field.season) {
  wq.flags.predata <- WqMedian(park = park, site = site, field.season = field.season)

  temp.flags <- wq.flags.predata %>%
    dplyr::filter(TemperatureFlag %in% c("I", "W", "C")) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, TemperatureMedian_C, TemperatureFlag, TemperatureFlagNote) %>%
    tibble::add_column(Parameter = "Temperature", Units = "C", .after = "SampleFrame") %>%
    dplyr::rename(Value = TemperatureMedian_C, Flag = TemperatureFlag, FlagNote = TemperatureFlagNote)

  spcond.flags <- wq.flags.predata %>%
    dplyr::filter(SpCondFlag %in% c("I", "W", "C")) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, SpCondMedian_microS_per_cm, SpCondFlag, SpCondFlagNote) %>%
    tibble::add_column(Parameter = "SpCond", Units = "uS/cm", .after = "SampleFrame") %>%
    dplyr::rename(Value = SpCondMedian_microS_per_cm, Flag = SpCondFlag, FlagNote = SpCondFlagNote)

  ph.flags <- wq.flags.predata %>%
    dplyr::filter(pHFlag %in% c("I", "W", "C")) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, pHMedian, pHFlag, pHFlagNote) %>%
    tibble::add_column(Parameter = "pH", Units = "units", .after = "SampleFrame") %>%
    dplyr::rename(Value = pHMedian, Flag = pHFlag, FlagNote = pHFlagNote)

  do.percent.flags <- wq.flags.predata %>%
    dplyr::filter(DOFlag %in% c("I", "W", "C")) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, DOMedian_Percent, DOFlag, DOFlagNote) %>%
    tibble::add_column(Parameter = "DO", Units = "%", .after = "SampleFrame") %>%
    dplyr::rename(Value = DOMedian_Percent, Flag = DOFlag, FlagNote = DOFlagNote)

  do.mgl.flags <- wq.flags.predata %>%
    dplyr::filter(DOFlag %in% c("I", "W", "C")) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, DOMedian_mg_per_L, DOFlag, DOFlagNote) %>%
    tibble::add_column(Parameter = "DO", Units = "mg/L", .after = "SampleFrame") %>%
    dplyr::rename(Value = DOMedian_mg_per_L, Flag = DOFlag, FlagNote = DOFlagNote)

  wq.flags <- rbind(temp.flags, spcond.flags, ph.flags, do.percent.flags, do.mgl.flags) %>%
    dplyr::arrange(factor(Flag, levels = c("C", "W", "I")), Parameter, Units, Park, FieldSeason, SampleFrame, SiteCode)

  return(wq.flags)
}


#' Intermediate step used to clean water quality data for stats and plotting functions. Limit data to primary visits of annual and 3Yr springs, and exclude data with "W" and "C" flags.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' 
#'
#' @return Tibble with columns for Park, FieldSeason, SiteCode, VisitDate, Parameter, Units, and Median.
#' @export
#'
#' @examples
#' \dontrun{
#'     
#'     qcWqLong()
#'     qcWqLong(site = "LAKE_P_GET0066", field.season = "2019")
#'     qcWqLong(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
qcWqLong <- function(park, site, field.season) {
  wq.cleaned.data <- WqMedian(park = park, site = site, field.season = field.season)

  temp.cleaned <- wq.cleaned.data %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr"), !(TemperatureFlag %in% c("W", "C")), VisitType %in% c("Primary")) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, SampleFrame, TemperatureMedian_C) %>%
    dplyr::group_by(Park, FieldSeason) %>%
    tibble::add_column(Parameter = "Temperature", Units = "C", .after = "SampleFrame") %>%
    dplyr::rename(Value = TemperatureMedian_C)

  spcond.cleaned <- wq.cleaned.data %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr"), !(SpCondFlag %in% c("W", "C")), VisitType %in% c("Primary")) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, SampleFrame, SpCondMedian_microS_per_cm) %>%
    dplyr::group_by(Park, FieldSeason) %>%
    tibble::add_column(Parameter = "SpCond", Units = "uS/cm", .after = "SampleFrame") %>%
    dplyr::rename(Value = SpCondMedian_microS_per_cm)

  ph.cleaned <- wq.cleaned.data %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr"), !(pHFlag %in% c("W", "C")), VisitType %in% c("Primary")) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, SampleFrame, pHMedian) %>%
    dplyr::group_by(Park, FieldSeason) %>%
    tibble::add_column(Parameter = "pH", Units = "units", .after = "SampleFrame") %>%
    dplyr::rename(Value = pHMedian)

  do.percent.cleaned <- wq.cleaned.data %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr"), !(DOFlag %in% c("W", "C")), VisitType %in% c("Primary"), (DOMedian_Percent < 120 | is.na(DOMedian_Percent))) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, SampleFrame, DOMedian_Percent) %>%
    dplyr::group_by(Park, FieldSeason) %>%
    tibble::add_column(Parameter = "DO", Units = "%", .after = "SampleFrame") %>%
    dplyr::rename(Value = DOMedian_Percent)

  do.mgl.cleaned <- wq.cleaned.data %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr"), !(DOFlag %in% c("W", "C")), VisitType %in% c("Primary"), (DOMedian_mg_per_L < 14 | is.na(DOMedian_mg_per_L))) %>%
    dplyr::select(Park, FieldSeason, SiteCode, VisitDate, SampleFrame, DOMedian_mg_per_L) %>%
    dplyr::group_by(Park, FieldSeason) %>%
    tibble::add_column(Parameter = "DO", Units = "mg/L", .after = "SampleFrame") %>%
    dplyr::rename(Value = DOMedian_mg_per_L)

  wq.cleaned <- rbind(temp.cleaned, spcond.cleaned, ph.cleaned, do.percent.cleaned, do.mgl.cleaned) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Park != "CAMO",
                  !is.na(Value))

  return(wq.cleaned)
}


#' Calculate quartile values for each water quality parameter for each park and year. Includes annual and 3Yr springs only.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' 
#'
#' @return Tibble with columns for Park; FieldSeason; Parameter; Units; and 0%, 25%, 50%, 75%, and 100% quantiles.
#' @export
#'
#' @examples
#' \dontrun{
#'     WqStats()
#'     WqStats(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
WqStats <- function(park, site, field.season) {
  wq.stats.predata <- qcWqLong(park = park, site = site, field.season = field.season)
  wq.stats <- wq.stats.predata %>%
    dplyr::group_by(Park, FieldSeason, Parameter, Units) %>%
    dplyr::summarize(stats = list(quantile(Value, type = 6, na.rm = TRUE)),
                     Count = dplyr::n()) %>%
    tidyr::unnest_wider(stats) %>%
    dplyr::ungroup() %>%
    dplyr::rename(Minimum = `0%`,
                  FirstQuartile = `25%`,
                  Median = `50%`,
                  ThirdQuartile = `75%`,
                  Maximum = `100%`)
  
  wq.stats[wq.stats$Parameter == "DO" & wq.stats$Units == "%", ] %<>% dplyr::mutate_if(is.double, ~ round(., 1))
  wq.stats[wq.stats$Parameter == "DO" & wq.stats$Units == "mg/L", ] %<>% dplyr::mutate_if(is.double, ~ round(., 2))
  wq.stats[wq.stats$Parameter == "SpCond", ] %<>% dplyr::mutate_if(is.double, ~ round(., 0))
  wq.stats[wq.stats$Parameter == "pH", ] %<>% dplyr::mutate_if(is.double, ~ round(., 2))
  wq.stats[wq.stats$Parameter == "Temperature", ] %<>% dplyr::mutate_if(is.double, ~ round(., 1))
  
  return(wq.stats)
}


#' Generate box plots for water temperature for each park and 3-year cycle. Includes annual and 3Yr springs only.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param include.title Include plot title? Defaults to TRUE
#'
#' @return ggplot box plots
#' @export
#'
#' @examples
#' \dontrun{
#'     WqPlotTemp()
#'     WqPlotTemp(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
WqPlotTemp <- function(park, site, field.season, include.title = FALSE) {
  wq.plot <- qcWqLong(park = park, site = site, field.season = field.season) %>%
    dplyr::filter(Parameter == "Temperature") %>%
    dplyr::filter(dplyr::case_when(Park %in% c("LAKE", "MOJA") ~ FieldSeason %in% c("2016", "2019", "2022", "2025"),
                                   Park %in% c("JOTR", "PARA") ~ FieldSeason %in% c("2017", "2020", "2023"),
                                   Park %in% c("DEVA") ~ FieldSeason %in% c("2018", "2021", "2024"),
                                   TRUE ~ FieldSeason %in% c("2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"))) %>%
    GetSampleSizes(Park, FieldSeason)
    
  wq.plot.temp <- FormatPlot(
    data = wq.plot,
    x.col = FieldSeason,
    y.col = Value,
    facet.col = Park,
    sample.size.col = SampleSizeLabel,
    sample.size.loc = "xaxis",
    facet.as.subtitle = FALSE,
    # plot.title = dplyr::if_else(include.title, "Water Temperature", ""),
    # facet.as.subtitle = include.title,
    x.lab = "Field Season",
    y.lab = "Temperature (C)"
    ) +
    ggplot2::geom_boxplot() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(vjust = 0.5, hjust = 0.5))

  if (length(unique(wq.plot$Park)) == 1) {
    return(wq.plot.temp)
  } else {
    return(wq.plot.temp + ggplot2::facet_grid(~Park, scales = "free"))
  }
}


#' Generate box plots for specific conductance for each park and year in units of uS/cm. Includes annual and 3Yr springs only.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param include.title Include plot title? Defaults to TRUE
#'
#' @return ggplot box plots
#' @export
#'
#' @examples
#' \dontrun{
#'     
#'     WqPlotSpCond()
#'     WqPlotSpCond(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
WqPlotSpCond <- function(park, site, field.season, include.title = FALSE) {
  wq.plot <- qcWqLong(park = park, site = site, field.season = field.season) %>%
    dplyr::filter(Parameter == "SpCond") %>%
    dplyr::filter(dplyr::case_when(Park %in% c("LAKE", "MOJA") ~ FieldSeason %in% c("2016", "2019", "2022", "2025"),
                                   Park %in% c("JOTR", "PARA") ~ FieldSeason %in% c("2017", "2020", "2023"),
                                   Park %in% c("DEVA") ~ FieldSeason %in% c("2018", "2021", "2024"),
                                   TRUE ~ FieldSeason %in% c("2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"))) %>%
    GetSampleSizes(Park, FieldSeason) #
  
  wq.plot.spcond <- FormatPlot(
    data = wq.plot,
    x.col = FieldSeason,
    y.col = Value,
    facet.col = Park,
    sample.size.col = SampleSizeLabel,
    sample.size.loc = "xaxis",
    facet.as.subtitle = FALSE,
    x.lab = "Field Season",
    y.lab = "Specific Cond. (uS/cm)"
  ) +
    ggplot2::geom_boxplot() + 
    ggplot2::scale_y_log10(breaks = c(100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000),
                           limits = c(100, 100000),
                           labels = scales::comma) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(vjust = 0.5, hjust = 0.5))
  
  if (length(unique(wq.plot$Park)) == 1) {
    return(wq.plot.spcond)
  } else {
    return(wq.plot.spcond + ggplot2::facet_grid(~Park, scales = "free"))
  }
}


#' Generate box plots for specific conductance for each park and year in units of mS/cm. Includes annual and 3Yr springs only.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param include.title Include plot title? Defaults to TRUE
#'
#' @return Box plots of specific conductance data for each park and field season.
#' @export
#'
WqPlotSpCondmS <- function(park, site, field.season, include.title = FALSE) {
  wq.plot <- qcWqLong(park = park, site = site, field.season = field.season) %>%
    dplyr::filter(Parameter == "SpCond" & Park != "CAMO" & !is.na(Median)) %>%
    GetSampleSizes(Park, FieldSeason)
  
  wq.plot.spcond.ms <- FormatPlot(
    data = wq.plot,
    x.col = FieldSeason,
    y.col = Median / 1000,
    facet.col = Park,
    sample.size.col = SampleSizeLabel,
    sample.size.loc = "xaxis",
    facet.as.subtitle = FALSE,
    x.lab = "Field Season",
    y.lab = "Specific Conductance (mS/cm)"
  ) +
    ggplot2::geom_boxplot() + 
    ggplot2::scale_y_log10(breaks = c(0.2, 0.5, 1, 2, 5, 10, 25, 100), labels = c(0.2, 0.5, 1, 2, 5, 10, 25, 100), limits = c(0.2, 100))
  
  if (length(unique(wq.plot$Park)) == 1) {
    return(wq.plot.spcond.ms)
  } else {
    return(wq.plot.spcond.ms + ggplot2::facet_grid(~Park, scales = "free"))
  }
}


#' Generate box plots for pH for each park and year. Includes annual and 3Yr springs only.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param include.title Include plot title? Defaults to TRUE
#'
#' @return ggplot box plots
#' @export
#'
#' @examples
#' \dontrun{
#'     WqPlotPH()
#'     WqPlotPH(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
WqPlotPH <- function(park, site, field.season, include.title = FALSE) {
  wq.plot <- qcWqLong(park = park, site = site, field.season = field.season) %>%
    dplyr::filter(Parameter == "pH") %>%
    dplyr::filter(dplyr::case_when(Park %in% c("LAKE", "MOJA") ~ FieldSeason %in% c("2016", "2019", "2022", "2025"),
                                   Park %in% c("JOTR", "PARA") ~ FieldSeason %in% c("2017", "2020", "2023"),
                                   Park %in% c("DEVA") ~ FieldSeason %in% c("2018", "2021", "2024"),
                                   TRUE ~ FieldSeason %in% c("2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"))) %>%
    GetSampleSizes(Park, FieldSeason)
  
  wq.plot.ph <- FormatPlot(
    data = wq.plot,
    x.col = FieldSeason,
    y.col = Value,
    facet.col = Park,
    sample.size.col = SampleSizeLabel,
    sample.size.loc = "xaxis",
    facet.as.subtitle = FALSE,
    x.lab = "Park",
    y.lab = "pH"
  ) +
    ggplot2::geom_boxplot() + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(vjust = 0.5, hjust = 0.5))
  
  if (length(unique(wq.plot$Park)) == 1) {
    return(wq.plot.ph)
  } else {
    return(wq.plot.ph + ggplot2::facet_grid(~Park, scales = "free"))
  }  
}


#' Generate box plots for concentration dissolved oxygen for each park and year. Includes annual and 3Yr springs only.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param include.title Include plot title? Defaults to TRUE
#'
#' @return ggplot box plots
#' @export
#'
#' @examples
#' \dontrun{
#'     
#'     WqPlotDOmgL()
#'     WqPlotDOmgL(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
WqPlotDOmgL <- function(park, site, field.season, include.title = FALSE) {
  wq.plot <- qcWqLong(park = park, site = site, field.season = field.season) %>%
    dplyr::filter(Parameter == "DO",
                  Units == "mg/L") %>%
    dplyr::filter(dplyr::case_when(Park %in% c("LAKE", "MOJA") ~ FieldSeason %in% c("2016", "2019", "2022", "2025"),
                                   Park %in% c("JOTR", "PARA") ~ FieldSeason %in% c("2017", "2020", "2023"),
                                   Park %in% c("DEVA") ~ FieldSeason %in% c("2018", "2021", "2024"),
                                   TRUE ~ FieldSeason %in% c("2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"))) %>%
    GetSampleSizes(Park, FieldSeason)
  
  wq.plot.do.mgl <- FormatPlot(
    data = wq.plot,
    x.col = FieldSeason,
    y.col = Value,
    facet.col = Park,
    sample.size.col = SampleSizeLabel,
    sample.size.loc = "xaxis",
    facet.as.subtitle = FALSE,
    x.lab = "Park",
    y.lab = "Dissolved Oxygen (mg/L)"
  ) +
    ggplot2::geom_boxplot() + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(vjust = 0.5, hjust = 0.5))
  
  if (length(unique(wq.plot$Park)) == 1) {
    return(wq.plot.do.mgl)
  } else {
    return(wq.plot.do.mgl + ggplot2::facet_grid(~Park, scales = "free"))
  }
}


#' Generate box plots for percent dissolved oxygen for each park and year. Includes annual and 3Yr springs only.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param include.title Include plot title? Defaults to TRUE
#'
#' @return ggplot box plots
#' @export
#'
#' @examples
#' \dontrun{
#'     WqPlotDOPct()
#'     WqPlotDOPct(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#' }
WqPlotDOPct <- function(park, site, field.season, include.title = FALSE) {
  wq.plot <- qcWqLong(park = park, site = site, field.season = field.season) %>%
    dplyr::filter(Parameter == "DO" & Units == "%") %>%
    dplyr::filter(dplyr::case_when(Park %in% c("LAKE", "MOJA") ~ FieldSeason %in% c("2016", "2019", "2022", "2025"),
                                    Park %in% c("JOTR", "PARA") ~ FieldSeason %in% c("2017", "2020", "2023"),
                                    Park %in% c("DEVA") ~ FieldSeason %in% c("2018", "2021", "2024"),
                                    TRUE ~ FieldSeason %in% c("2016", "2017", "2018", "2019", "2020", "2021", "2022", "2024"))) %>%
    GetSampleSizes(Park, FieldSeason)
  
  wq.plot.do.pct <- FormatPlot(
    data = wq.plot,
    x.col = FieldSeason,
    y.col = Value,
    facet.col = Park,
    sample.size.col = SampleSizeLabel,
    sample.size.loc = "xaxis",
    facet.as.subtitle = FALSE,
    x.lab = "Park",
    y.lab = "Dissolved Oxygen (%)"
  ) +
    ggplot2::geom_boxplot() + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(vjust = 0.5, hjust = 0.5)) + 
    ggplot2::ylim(0, 100)
  
  if (length(unique(wq.plot$Park)) == 1) {
    return(wq.plot.do.pct)
  } else {
    return(wq.plot.do.pct + ggplot2::facet_grid(~Park, scales = "free"))
  }
}


#' Generate grid of box plots for core water quality parameters for each park and year. Includes annual and 3Yr springs only.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' 
#'
#' @return Grid of ggplot box plots
#' @export
#'
#' @examples
#' \dontrun{
#'     WqPlotGrid()
#'     WqPlotGrid(park = c("DEVA", "JOTR"), field.season = c("2017", "2018", "2021"))
#'     WqPlotGrid(path.to.data = "path/to/data" = "local")
#'     CloseDatabaseection()
#' }
  WqPlotGrid <- function(park, site, field.season) {
    wq.plot.temp <- WqPlotTemp(park = park, site = site, field.season = field.season)
    wq.plot.ph <- WqPlotPH(park = park, site = site, field.season = field.season)
    wq.plot.spcond.ms <- WqPlotSpCondmS(park = park, site = site, field.season = field.season)
    wq.plot.do.mgl <- WqPlotDOmgL(park = park, site = site, field.season = field.season)
  
  wq.plot.grid <- gridExtra::grid.arrange(wq.plot.temp, wq.plot.spcond, wq.plot.ph, wq.plot.do.mgl, ncol = 1)

  return(wq.plot.grid)
  }
  
#' Check dissolved oxygen calibrations for the use of non-local dissolved oxygen percent.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcLocalDOCheck()
#'     qcLocalDOCheck(site = "PARA_P_COY0069")
#'     qcLocalDOCheck(park = c("LAKE", "PARA"), field.season = c("2017", "2019", "2020"))
#' }
qcLocalDOCheck <- function(park, site, field.season) {
  do <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "CalibrationDO")
  
  do.check <- do %>%
    dplyr::filter(99.5 > PostCalibrationReading_percent | 100.5 < PostCalibrationReading_percent) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, DOInstrument, PreCalibrationReading_percent, PostCalibrationReading_percent) %>%
    dplyr::rename(PreCalDO_percent = PreCalibrationReading_percent,
                  PostCalDO_percent = PostCalibrationReading_percent) %>%
    dplyr::mutate(PreCalDO_percent = round(PreCalDO_percent, 1),
                  PostCalDO_percent = round(PostCalDO_percent, 1)) %>%
    unique() %>%
    dplyr::arrange(SiteCode, FieldSeason)
  
  
  return(do.check)
}


#' Check specific conductance calibrations for the use of low conductivity standards at high conductivity springs.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcSpCondStandardCheck()
#'     qcSpCondStandardCheck(site = "LAKE_P_GET0066")
#'     qcSpCondStandardCheck(park = c("LAKE", "PARA"), field.season = c("2016", "2017", "2020"))
#' }
qcSpCondStandardCheck <- function(park, site, field.season) {
  sc <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "CalibrationSpCond")
  med <- WqMedian(park, site, field.season)
  
  sc.sel <- sc %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SpCondInstrument, StandardValue_microS_per_cm)
  
  med.sc <- med %>%
    dplyr::select(Park, SiteCode, VisitDate, FieldSeason, SpCondMedian_microS_per_cm) %>%
    dplyr::rename(SpCondMedian = SpCondMedian_microS_per_cm) %>%
    dplyr::filter(!is.na(SpCondMedian))
  
  sc.joined <- med.sc %>%
    dplyr::left_join(sc.sel, by = c("Park", "SiteCode", "VisitDate", "FieldSeason"), multiple = "all") %>%
    dplyr::relocate(SiteName, .after = SiteCode) %>%
    dplyr::relocate(SpCondInstrument, .after = FieldSeason) %>%
    dplyr::rename(SpCondStandard = StandardValue_microS_per_cm) %>%
    dplyr::filter((SpCondMedian > 5000 & SpCondStandard < 5000) | (SpCondMedian > 25000 & SpCondStandard < 25000)) %>%
    dplyr::arrange(SiteCode, FieldSeason)
  
  return(sc.joined)
}


#' Check that instruments were calibrated within 1 day of site visit
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcCalibrationTimes()
#'     qcCalibrationTimes(site = "LAKE_P_GET0066")
#'     qcCalibrationTimes(park = c("LAKE", "PARA"), field.season = c("2016", "2017", "2020"))
#' }
qcCalibrationTimes <- function(park, site, field.season) {
  ph <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "CalibrationpH")
  sc <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "CalibrationSpCond")
  do <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "CalibrationDO")

  ph_data <- ph %>%
    dplyr::select(SiteCode, FieldSeason, VisitDate, StartTime, CalibrationDate, CalibrationTime, pHInstrument) %>%
    dplyr::rename(VisitTime = StartTime,
                  Instrument = pHInstrument) %>%
    dplyr::mutate(Parameter = "pH") %>%
    unique()
    
  sc_data <- sc %>%
    dplyr::select(SiteCode, FieldSeason, VisitDate, StartTime, CalibrationDate, CalibrationTime, SpCondInstrument) %>%
    dplyr::rename(VisitTime = StartTime,
                  Instrument = SpCondInstrument) %>%
    dplyr::mutate(Parameter = "SpCond") 
  
  do_data <- do %>%
    dplyr::select(SiteCode, FieldSeason, VisitDate, StartTime, CalibrationDate, CalibrationTime, DOInstrument) %>%
    dplyr::rename(VisitTime = StartTime,
                  Instrument = DOInstrument) %>%
    dplyr::mutate(Parameter = "DO")
  
  cal_data <- dplyr::bind_rows(ph_data, sc_data, do_data) %>%
    dplyr::mutate(VisitDateTime = as.POSIXct(paste(VisitDate, VisitTime), format="%Y-%m-%d %H:%M:%S"),
                  CalibrationDateTime = as.POSIXct(paste(CalibrationDate, CalibrationTime), format="%Y-%m-%d %H:%M:%S")) %>%
    dplyr::relocate(VisitDateTime, .after = "FieldSeason") %>%
    dplyr::relocate(CalibrationDateTime, .after = "VisitTime") %>%
    dplyr::select(-c("VisitTime", "CalibrationTime")) %>%
    dplyr::mutate(Flag = dplyr::case_when(VisitDate != CalibrationDate ~ "date",
                                          (VisitDate == CalibrationDate) & (VisitDateTime < CalibrationDateTime) ~ "time",
                                          TRUE ~ NA_character_)) %>%
    dplyr::filter(!is.na(Flag)) %>%
    dplyr::mutate(DaysBefore = difftime(VisitDate, CalibrationDate, units = "days")) %>%
    unique()
  
  all_data <- dplyr::bind_rows(ph_data, sc_data, do_data) %>%
    dplyr::mutate(VisitDateTime = as.POSIXct(paste(VisitDate, VisitTime), format="%Y-%m-%d %H:%M:%S"),
                  CalibrationDateTime = as.POSIXct(paste(CalibrationDate, CalibrationTime), format="%Y-%m-%d %H:%M:%S")) %>%
    dplyr::relocate(VisitDateTime, .after = "FieldSeason") %>%
    dplyr::relocate(CalibrationDateTime, .after = "VisitTime") %>%
    dplyr::select(-c("VisitTime", "CalibrationTime")) %>%
    unique()
  
  return(cal_data)
}


#' Check for missing calibration unique IDs
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcUniqueIDMissing()
#'     qcUniqueIDMissing(site = "LAKE_P_GET0066")
#'     qcUniqueIDMissing(park = c("LAKE", "PARA"), field.season = c("2016", "2017", "2020"))
#' }
qcUniqueIDMissing <- function(park, site, field.season) {
  ph <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "CalibrationpH")
  sc <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "CalibrationSpCond")
  do <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "CalibrationDO")
  
  ph_data <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "WaterQualitypH")
  sc_data <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "WaterQualitySpCond")
  do_data <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "WaterQualityDO")
  
  ph_cal <- ph %>%
    dplyr::select(SiteCode, VisitDate, CalibrationDate, StandardValue_pH) %>%
    dplyr::rename(Standard = "StandardValue_pH") %>%
    dplyr::group_by(SiteCode, VisitDate, CalibrationDate) %>%
    dplyr::summarize(Standard = paste(Standard, collapse = ", ")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Parameter = "pH")
  
  sc_cal <- sc %>%
    dplyr::select(SiteCode, VisitDate, CalibrationDate, StandardValue_microS_per_cm) %>%
    dplyr::rename(Standard = "StandardValue_microS_per_cm") %>%
    dplyr::mutate(Parameter = "SpCond",
                  Standard = as.character(Standard))
  
  do_cal <- do %>%
    dplyr::select(SiteCode, VisitDate, CalibrationDate) %>%
    dplyr::mutate(Standard = as.character(100),
                  Parameter = "DO")
  
  ph_site <- ph_data %>%
    dplyr::filter(!is.na(pH)) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, pH) %>%
    dplyr::group_by(Park, SiteCode, SiteName, VisitDate, FieldSeason) %>%
    dplyr::summarize(Value = median(pH)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Parameter = "pH")
  
  sc_site <- sc_data %>%
    dplyr::filter(!is.na(SpecificConductance_microS_per_cm)) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SpecificConductance_microS_per_cm) %>%
    dplyr::group_by(Park, SiteCode, SiteName, VisitDate, FieldSeason) %>%
    dplyr::summarize(Value = median(SpecificConductance_microS_per_cm)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Parameter = "SpCond")
  
  do_site <- do_data %>%
    dplyr::filter(!is.na(DissolvedOxygen_percent) | !is.na(DissolvedOxygen_mg_per_L)) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, DissolvedOxygen_mg_per_L) %>%
    dplyr::group_by(Park, SiteCode, SiteName, VisitDate, FieldSeason) %>%
    dplyr::summarize(Value = median(DissolvedOxygen_mg_per_L)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Parameter = "DO",
                  Value = dplyr::case_when(is.na(Value) ~ -999,
                                           TRUE ~ Value))
  
  site_data <- rbind(ph_site, sc_site, do_site)
  cal_data <- rbind(ph_cal, sc_cal, do_cal)
 
  joined <- site_data %>%
    dplyr::full_join(cal_data, by = c("SiteCode", "VisitDate", "Parameter"), multiple = "all") %>%
    dplyr::filter(is.na(Standard) | is.na(CalibrationDate))
   
  return(joined)
}

#' Map of water temperature at springs with surface water
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return leaflet map
#' @export
#'
#' @examples
#' \dontrun{
#'     WqMapTemp()
#'     WqMapTemp(site = "LAKE_P_GET0066", field.season = "2019")
#'     WqMapTemp(park = c("MOJA", "PARA"), field.season = c("2017", "2019", "2020"))
#' }
WqMapTemp <- function(park, site, field.season) {
  data <- qcWqLong(park = park, site = site, field.season = field.season)
  site <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Site")
  
  coords <- site %>%
    dplyr::select(SiteCode, SiteName, SampleFrame, Lat_WGS84, Lon_WGS84, X_UTM_NAD83_11N, Y_UTM_NAD83_11N)
  
  wqdata <- data %>%
    dplyr::select(Park, SiteCode, VisitDate, FieldSeason, Parameter, Units, Value) %>%
    dplyr::inner_join(coords, by = "SiteCode", multiple = "all") %>%
    dplyr::relocate(SiteName, .after = SiteCode) %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr")) %>%
    dplyr::mutate(Measurement = paste0(Parameter, "_", Units)) %>%
    dplyr::relocate(Measurement, .after = Units) %>%
    dplyr::filter(!is.na(Value)) %>%
    dplyr::filter(Parameter == "Temperature") %>%
    dplyr::mutate(Bin = dplyr::case_when(Value < 5 ~ "< 5",
                                         Value >= 5 & Value < 10 ~ "5 - 10",
                                         Value >= 10 & Value < 15 ~ "10 - 15",
                                         Value >= 15 & Value < 20 ~ "15 - 20",
                                         Value >= 20 & Value < 30 ~ "20 - 30",
                                         Value >= 30 & Value < 40 ~ "30 - 40",
                                         Value >= 40 ~ "> 40",
                                         TRUE ~ "NA")) %>%
    dplyr::mutate(Year = as.numeric(FieldSeason)) %>%
    dplyr::relocate(Year, .after = FieldSeason)
  
  wqdata$Bin <- factor(wqdata$Bin, levels = c("< 5", "5 - 10", "10 - 15", "15 - 20", "20 - 30", "30 - 40", "> 40"))
  
  wqdata %<>% dplyr::arrange(FieldSeason, Value)
  
  pal <- leaflet::colorFactor(palette = "RdYlBu",
                              domain = wqdata$Bin,
                              rev = TRUE)
  
  # Make NPS map Attribution
  NPSAttrib <-
    htmltools::HTML(
      "<a href='https://www.nps.gov/npmap/disclaimer/'>Disclaimer</a> |
      &copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a>
      &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors |
      <a class='improve-park-tiles'
      href='http://insidemaps.nps.gov/places/editor/#background=mapbox-satellite&map=4/-95.97656/39.02772&overlays=park-tiles-overlay'
      target='_blank'>Improve Park Tiles</a>"
    )
  
  NPSbasic = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSimagery = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck72fwp2642dv07o7tbqinvz4/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSslate = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpvc2e0avf01p9zaw4co8o/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSlight = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpia2u0auf01p9vbugvcpv/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  
  # width <- 700
  # height <- 600
  
  sd <- crosstalk::SharedData$new(wqdata)
  year_filter <- crosstalk::filter_checkbox(id = "year-t",
                                            label = "Water Year",
                                            sharedData = sd,
                                            group = ~Year,
                                            # width = width,
                                            inline = TRUE)
  
  wqmaptemp <- leaflet::leaflet(sd
                                #, height = height, width = width
                                ) %>%
    leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
    leaflet::addScaleBar('bottomright') %>%
    leaflet::addCircleMarkers(lng = ~Lon_WGS84,
                              lat = ~Lat_WGS84,
                              popup = paste ("Name: ", wqdata$SiteName, "<br>",
                                             "Sample Frame: ", wqdata$SampleFrame, "<br>",
                                             "Field Season: ", wqdata$FieldSeason, "<br>",
                                             "Parameter: ", wqdata$Parameter, "<br>",
                                             "Units: ", wqdata$Units, "<br>",
                                             "Value: ", wqdata$Value),
                              radius = 5,
                              stroke = TRUE,
                              weight = 1,
                              color = "black",
                              fillOpacity = 1,
                              fillColor = ~pal(Bin),
                              group = ~Bin) %>%
    leaflet::addLegend(pal = pal,
                       values = ~Bin,
                       title = "Temperature (C)",
                       opacity = 1,
                       position = "bottomleft") %>%
    leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                              overlayGroups = c("< 5", "5 - 10", "10 - 15", "15 - 20", "20 - 30", "30 - 40", "> 40"),
                              options=leaflet::layersControlOptions(collapsed = FALSE))

  if (missing(field.season)) {
    wqmaptemp <- crosstalk::bscols(list(year_filter, wqmaptemp))
  } else if (!missing(field.season) & length(field.season) == 1) {
    # do nothing
  } else {
   wqmaptemp <- crosstalk::bscols(list(year_filter, wqmaptemp))
  }
  
  return(wqmaptemp)
}


#' Map of specific conductance at springs with surface water
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return leaflet map
#' @export
#'
#' @examples
#' \dontrun{
#'     WqMapSpCond()
#'     WqMapSpCond(site = "LAKE_P_GET0066", field.season = "2019")
#'     WqMapSpCond(park = c("MOJA", "PARA"), field.season = c("2017", "2019", "2020"))
#' }
WqMapSpCond <- function(park, site, field.season) {
  data <- qcWqLong(park = park, site = site, field.season = field.season)
  site <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Site")
  coords <- site %>%
    dplyr::select(SiteCode, SiteName, SampleFrame, Lat_WGS84, Lon_WGS84, X_UTM_NAD83_11N, Y_UTM_NAD83_11N)
  
  wqdata <- data %>%
    dplyr::select(Park, SiteCode, VisitDate, FieldSeason, Parameter, Units, Value) %>%
    dplyr::inner_join(coords, by = "SiteCode", multiple = "all") %>%
    dplyr::relocate(SiteName, .after = SiteCode) %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr")) %>%
    dplyr::mutate(Measurement = paste0(Parameter, "_", Units)) %>%
    dplyr::relocate(Measurement, .after = Units) %>%
    dplyr::filter(!is.na(Value)) %>%
    dplyr::filter(Parameter == "SpCond") %>%
    dplyr::mutate(Bin = dplyr::case_when(Value < 200 ~ "< 200",
                                         Value >= 200 & Value < 500 ~ "200 - 500",
                                         Value >= 500 & Value < 1000 ~ "500 - 1000",
                                         Value >= 1000 & Value < 2000 ~ "1000 - 2000",
                                         Value >= 2000 & Value < 5000 ~ "2000 - 5000",
                                         Value >= 5000 & Value < 10000 ~ "5000 - 10000",
                                         Value >= 10000 ~ "> 10000",
                                         TRUE ~ "NA")) %>%
    dplyr::mutate(Year = as.numeric(FieldSeason)) %>%
    dplyr::relocate(Year, .after = FieldSeason)

  
  wqdata$Bin <- factor(wqdata$Bin, levels = c("< 200", "200 - 500", "500 - 1000", "1000 - 2000", "2000 - 5000", "5000 - 10000", "> 10000"))
  
  wqdata %<>% dplyr::arrange(FieldSeason, Value)

  pal <- leaflet::colorFactor(palette = "Reds",
                              domain = wqdata$Bin,
                              rev = FALSE)
  
  # Make NPS map Attribution
  NPSAttrib <-
    htmltools::HTML(
      "<a href='https://www.nps.gov/npmap/disclaimer/'>Disclaimer</a> |
      &copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a>
      &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors |
      <a class='improve-park-tiles'
      href='http://insidemaps.nps.gov/places/editor/#background=mapbox-satellite&map=4/-95.97656/39.02772&overlays=park-tiles-overlay'
      target='_blank'>Improve Park Tiles</a>"
    )
  
  NPSbasic = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSimagery = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck72fwp2642dv07o7tbqinvz4/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSslate = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpvc2e0avf01p9zaw4co8o/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSlight = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpia2u0auf01p9vbugvcpv/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  
  # width <- 700
  # height <- 600
  
  sd <- crosstalk::SharedData$new(wqdata)
  year_filter <- crosstalk::filter_checkbox(id = "year-sc",
                                            label = "Water Year",
                                            sharedData = sd,
                                            group = ~Year,
                                            # width = width,
                                            inline = TRUE)
  
  wqmapspcond <- leaflet::leaflet(sd
                                  #, height = height, width = width
                                  ) %>%
    leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
    leaflet::addScaleBar('bottomright') %>%
    leaflet::addCircleMarkers(lng = ~Lon_WGS84,
                              lat = ~Lat_WGS84,
                              popup = paste ("Name: ", wqdata$SiteName, "<br>",
                                             "Sample Frame: ", wqdata$SampleFrame, "<br>",
                                             "Field Season: ", wqdata$FieldSeason, "<br>",
                                             "Parameter: ", wqdata$Parameter, "<br>",
                                             "Units: ", wqdata$Units, "<br>",
                                             "Value: ", wqdata$Value),
                              radius = 5,
                              stroke = TRUE,
                              weight = 1,
                              color = "black",
                              fillOpacity = 1,
                              fillColor = ~pal(Bin),
                              group = ~Bin) %>%
    leaflet::addLegend(pal = pal,
                       values = ~Bin,
                       title = "Specific Conductance (uS/cm)",
                       opacity = 1,
                       position = "bottomleft") %>%
    leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                              overlayGroups = c("< 200", "200 - 500", "500 - 1000", "1000 - 2000", "2000 - 5000", "5000 - 10000", "> 10000"),
                              options=leaflet::layersControlOptions(collapsed = FALSE))
 
  if (missing(field.season)) {
    wqmapspcond <- crosstalk::bscols(list(year_filter, wqmapspcond))
  } else if (!missing(field.season) & length(field.season) == 1) {
    # do nothing
  } else {
    wqmapspcond <- crosstalk::bscols(list(year_filter, wqmapspcond))
  }

  return(wqmapspcond)
  
}


#' Map of pH at springs with surface water
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return leaflet map
#' @export
#'
#' @examples
#' \dontrun{
#'     WqMapPH()
#'     WqMapPH(site = "LAKE_P_GET0066", field.season = "2019")
#'     WqMapPH(park = c("MOJA", "PARA"), field.season = c("2017", "2019", "2020"))
#' }
WqMapPH <- function(park, site, field.season) {
  data <- qcWqLong(park = park, site = site, field.season = field.season)
  site <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Site")
  
  coords <- site %>%
    dplyr::select(SiteCode, SiteName, SampleFrame, Lat_WGS84, Lon_WGS84, X_UTM_NAD83_11N, Y_UTM_NAD83_11N)
  
  wqdata <- data %>%
    dplyr::select(Park, SiteCode, VisitDate, FieldSeason, Parameter, Units, Value) %>%
    dplyr::inner_join(coords, by = "SiteCode", multiple = "all") %>%
    dplyr::relocate(SiteName, .after = SiteCode) %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr")) %>%
    dplyr::mutate(Measurement = paste0(Parameter, "_", Units)) %>%
    dplyr::relocate(Measurement, .after = Units) %>%
    dplyr::filter(!is.na(Value)) %>%
    dplyr::filter(Parameter == "pH") %>%
    dplyr::mutate(Bin = dplyr::case_when(Value < 6.5 ~ "< 6.5",
                                         Value >= 6.5 & Value < 7 ~ "6.5 - 7",
                                         Value >= 7 & Value < 7.5 ~ "7 - 7.5",
                                         Value >= 7.5 & Value < 8 ~ "7.5 - 8",
                                         Value >= 8 & Value < 8.5 ~ "8 - 8.5",
                                         Value >= 8.5 & Value < 9 ~ "8.5 - 9",
                                         Value >= 9 ~ "> 9",
                                         TRUE ~ "NA")) %>%
    dplyr::mutate(Year = as.numeric(FieldSeason)) %>%
    dplyr::relocate(Year, .after = FieldSeason)
  
  
  wqdata$Bin <- factor(wqdata$Bin, levels = c("< 6.5", "6.5 - 7", "7 - 7.5", "7.5 - 8", "8 - 8.5", "8.5 - 9", "> 9"))
  
  wqdata %<>% dplyr::arrange(FieldSeason, desc(Value))
  
  pal <- leaflet::colorFactor(palette = c("#FDAE61", "#FFFFBF", "#E0F3F8", "#ABD9E9", "#71ADD1", "#4575B4", "#313695"),
                              domain = wqdata$Bin,
                              rev = FALSE)
  
  # Make NPS map Attribution
  NPSAttrib <-
    htmltools::HTML(
      "<a href='https://www.nps.gov/npmap/disclaimer/'>Disclaimer</a> |
      &copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a>
      &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors |
      <a class='improve-park-tiles'
      href='http://insidemaps.nps.gov/places/editor/#background=mapbox-satellite&map=4/-95.97656/39.02772&overlays=park-tiles-overlay'
      target='_blank'>Improve Park Tiles</a>"
    )
  
  NPSbasic = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSimagery = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck72fwp2642dv07o7tbqinvz4/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSslate = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpvc2e0avf01p9zaw4co8o/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSlight = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpia2u0auf01p9vbugvcpv/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  
  # width <- 700
  # height <- 700
  
  sd <- crosstalk::SharedData$new(wqdata)
  year_filter <- crosstalk::filter_checkbox(id = "year-ph",
                                            label = "Water Year",
                                            sharedData = sd,
                                            group = ~Year,
                                            # width = width,
                                            inline = TRUE)
  
  wqmapph <- leaflet::leaflet(sd
                              #, height = height, width = width
                              ) %>%
    leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
    leaflet::addScaleBar('bottomright') %>%
    leaflet::addCircleMarkers(lng = ~Lon_WGS84,
                              lat = ~Lat_WGS84,
                              popup = paste ("Name: ", wqdata$SiteName, "<br>",
                                             "Sample Frame: ", wqdata$SampleFrame, "<br>",
                                             "Field Season: ", wqdata$FieldSeason, "<br>",
                                             "Parameter: ", wqdata$Parameter, "<br>",
                                             "Units: ", wqdata$Units, "<br>",
                                             "Value: ", wqdata$Value),
                              radius = 5,
                              stroke = TRUE,
                              weight = 1,
                              color = "black",
                              fillOpacity = 1,
                              fillColor = ~pal(Bin),
                              group = ~Bin) %>%
    leaflet::addLegend(pal = pal,
                       values = ~Bin,
                       title = "pH",
                       opacity = 1,
                       position = "bottomleft") %>%
    leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                              overlayGroups = c("< 6.5", "6.5 - 7", "7 - 7.5", "7.5 - 8", "8 - 8.5", "8.5 - 9", "> 9"),
                              options=leaflet::layersControlOptions(collapsed = FALSE))
 
  if (missing(field.season)) {
    wqmapph <- crosstalk::bscols(list(year_filter, wqmapph))
  } else if (!missing(field.season) & length(field.season) == 1) {
    # do nothing
  } else {
    wqmapph <- crosstalk::bscols(list(year_filter, wqmapph))
  }

  return(wqmapph)
}


#' Map of dissolved oxygen concentration at springs with surface water
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return leaflet map
#' @export
#'
#' @examples
#' \dontrun{
#'     WqMapDO()
#'     WqMapDO(site = "LAKE_P_GET0066", field.season = "2019")
#'     WqMapDO(park = c("MOJA", "PARA"), field.season = c("2017", "2019", "2020"))
#' }
WqMapDO <- function(park, site, field.season) {
  data <- qcWqLong(park = park, site = site, field.season = field.season)
  site <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Site")
  
  coords <- site %>%
    dplyr::select(SiteCode, SiteName, SampleFrame, Lat_WGS84, Lon_WGS84, X_UTM_NAD83_11N, Y_UTM_NAD83_11N)
  
  wqdata <- data %>%
    dplyr::select(Park, SiteCode, VisitDate, FieldSeason, Parameter, Units, Value) %>%
    dplyr::inner_join(coords, by = "SiteCode", multiple = "all") %>%
    dplyr::relocate(SiteName, .after = SiteCode) %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr")) %>%
    dplyr::mutate(Measurement = paste0(Parameter, "_", Units)) %>%
    dplyr::relocate(Measurement, .after = Units) %>%
    dplyr::filter(!is.na(Value)) %>%
    dplyr::filter(Parameter == "DO",
                  Units == "mg/L") %>%
    dplyr::mutate(Bin = dplyr::case_when(Value < 2 ~ "< 2",
                                         Value >= 2 & Value < 4 ~ "2 - 4",
                                         Value >= 4 & Value < 6 ~ "4 - 6",
                                         Value >= 6 & Value < 8 ~ "6 - 8",
                                         Value >= 8 & Value < 10 ~ "8 - 10",
                                         Value >= 10 ~ "> 10",
                                         TRUE ~ "NA")) %>%
    dplyr::mutate(Year = as.numeric(FieldSeason)) %>%
    dplyr::relocate(Year, .after = FieldSeason)
  
  wqdata$Bin <- factor(wqdata$Bin, levels = c("< 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10", "> 10"))
  
  wqdata %<>% dplyr::arrange(FieldSeason, Value)
  
  pal <- leaflet::colorFactor(palette = "Blues",
                              domain = wqdata$Bin,
                              rev = FALSE)
  
  # Make NPS map Attribution
  NPSAttrib <-
    htmltools::HTML(
      "<a href='https://www.nps.gov/npmap/disclaimer/'>Disclaimer</a> |
      &copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a>
      &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors |
      <a class='improve-park-tiles'
      href='http://insidemaps.nps.gov/places/editor/#background=mapbox-satellite&map=4/-95.97656/39.02772&overlays=park-tiles-overlay'
      target='_blank'>Improve Park Tiles</a>"
    )
  
  NPSbasic = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSimagery = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck72fwp2642dv07o7tbqinvz4/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSslate = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpvc2e0avf01p9zaw4co8o/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSlight = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpia2u0auf01p9vbugvcpv/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  
  # width <- 700
  # height <- 700
  
  sd <- crosstalk::SharedData$new(wqdata)
  year_filter <- crosstalk::filter_checkbox(id = "year-do",
                                            label = "Water Year",
                                            sharedData = sd,
                                            group = ~Year,
                                            # width = width,
                                            inline = TRUE)
  
  wqmapdo <- leaflet::leaflet(sd
                              # , height = height, width = width
                              ) %>%
    leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
    leaflet::addScaleBar('bottomright') %>%
    leaflet::addCircleMarkers(lng = ~Lon_WGS84,
                              lat = ~Lat_WGS84,
                              popup = paste ("Name: ", wqdata$SiteName, "<br>",
                                             "Sample Frame: ", wqdata$SampleFrame, "<br>",
                                             "Field Season: ", wqdata$FieldSeason, "<br>",
                                             "Parameter: ", wqdata$Parameter, "<br>",
                                             "Units: ", wqdata$Units, "<br>",
                                             "Value: ", wqdata$Value),
                              radius = 5,
                              stroke = TRUE,
                              weight = 1,
                              color = "black",
                              fillOpacity = 1,
                              fillColor = ~pal(Bin),
                              group = ~Bin) %>%
    leaflet::addLegend(pal = pal,
                       values = ~Bin,
                       title = "Dissolved Oxygen (mg/L)",
                       opacity = 1,
                       position = "bottomleft") %>%
    leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                              overlayGroups = c("< 2", "2 - 4", "4 - 6", "6 - 8", "8 - 10", "> 10"),
                              options=leaflet::layersControlOptions(collapsed = FALSE))
  
  if (missing(field.season)) {
    wqmapdo <- crosstalk::bscols(list(year_filter, wqmapdo))
  } else if (!missing(field.season) & length(field.season) == 1) {
    # do nothing
  } else {
    wqmapdo <- crosstalk::bscols(list(year_filter, wqmapdo))
  }
  
  return(wqmapdo)
}