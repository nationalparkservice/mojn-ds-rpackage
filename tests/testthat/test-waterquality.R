context("Water_quality")
LoadDesertSprings(here::here("tests", "testthat", "test_data"))


test_that("WqMedian works as expected", {

  actual_rows <- nrow(WqMedian() %>% dplyr::filter(Park == "LAKE"))
  expect_equal(actual_rows, 64)
  
  actual_cols <- colnames(WqMedian())
  expected_cols <- c("Park", "FieldSeason", "SiteCode", "VisitDate", "VisitType", "SampleFrame", "TemperatureFlag", "TemperatureFlagNote", "TemperatureMedian_C", "TemperatureCount", "SpCondFlag", "SpCondFlagNote", "SpCondMedian_microS_per_cm", "SpCondCount", "pHFlag", "pHFlagNote", "pHMedian", "pHCount", "DOFlag", "DOFlagNote", "DOMedian_Percent", "DOPercentCount", "DOMedian_mg_per_L", "DOmgLCount")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- WqMedian()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_dbl <- WqMedian()
  expect_equal(typeof(actual_date$TemperatureMedian_C), "double")
  expect_equal(typeof(actual_date$SpCondMedian_microS_per_cm), "double")
  expect_equal(typeof(actual_date$pHMedian), "double")
  expect_equal(typeof(actual_date$DOMedian_Percent), "double")
  expect_equal(typeof(actual_date$DOMedian_mg_per_L), "double")
  
  actual_medians <- WqMedian() %>% dplyr::filter(SiteCode == "DEVA_P_BEN0606", FieldSeason == "2020") %>% dplyr::select(TemperatureMedian_C, SpCondMedian_microS_per_cm, pHMedian, DOMedian_Percent, DOMedian_mg_per_L)
  expected_medians <- tibble::as_tibble_row(c(TemperatureMedian_C = as.double(16.4), SpCondMedian_microS_per_cm = as.double(2844), pHMedian = as.double(3.53), DOMedian_Percent = as.double(114.6), DOMedian_mg_per_L = as.double(10.45)))
  expect_equal(actual_medians, expected_medians)
  
})


test_that("qcWqSanity works as expected", {
  
  actual_rows <- nrow(qcWqSanity())
  expect_equal(actual_rows, 80)
  
  actual_cols <- colnames(qcWqSanity())
  expected_cols <- c("Park", "FieldSeason", "SiteCode", "VisitDate", "VisitType", "SampleFrame", "Parameter", "Units", "Value", "SanityNote")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcWqSanity()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_dbl <- qcWqSanity()
  expect_equal(typeof(actual_dbl$Value), "double")
  
  actual_value <- qcWqSanity() %>% dplyr::filter(SiteCode == "PARA_P_LOW0152", FieldSeason == "2021") %>% dplyr::select(Parameter, Units, Value)
  expected_value <- tibble::as_tibble_row(c(Parameter = as.character("DO"), Units = as.character("%"), Value = as.double(123.8))) %>% dplyr::mutate(Value = as.double(Value))
  expect_equal(actual_value, expected_value)
  
})


test_that("qcWqFlags works as expected", {
  
  actual_rows <- nrow(qcWqFlags())
  expect_equal(actual_rows, 106)
  
  actual_cols <- colnames(qcWqFlags())
  expected_cols <- c("Park", "FieldSeason", "SiteCode", "VisitDate", "VisitType", "SampleFrame", "Parameter", "Units", "Value", "Flag", "FlagNote")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcWqFlags()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_dbl <- qcWqFlags()
  expect_equal(typeof(actual_dbl$Value), "double")
  
  actual_value <- qcWqFlags() %>% dplyr::filter(SiteCode == "LAKE_P_GET0066", FieldSeason == "2019") %>% dplyr::select(Parameter, Units, Value, Flag)
  expected_value <- tibble::as_tibble_row(c(Parameter = as.character("pH"), Units = as.character("units"), Value = as.double(7.05), Flag = as.character("W"))) %>% dplyr::mutate(Value = as.double(Value))
  expect_equal(actual_value, expected_value)
  
})


test_that("qcWqLong works as expected", {
  
  actual_rows <- nrow(qcWqLong() %>% dplyr::filter(Park == "DEVA", FieldSeason == "2018"))
  expect_equal(actual_rows, 182)
  
  actual_cols <- colnames(qcWqLong())
  expected_cols <- c("Park", "FieldSeason", "SiteCode", "VisitDate", "SampleFrame", "Parameter", "Units", "Value")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcWqLong()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_dbl <- qcWqLong()
  expect_equal(typeof(actual_dbl$Value), "double")
  
  actual_value <- qcWqLong() %>% dplyr::filter(SiteCode == "LAKE_P_DRI0002", FieldSeason == "2016") %>% dplyr::select(SiteCode, Parameter, Units, Value)
  expected_value <- tibble::tibble(SiteCode = as.character(c("LAKE_P_DRI0002", "LAKE_P_DRI0002", "LAKE_P_DRI0002")),
                                   Parameter = as.character(c("Temperature", "SpCond", "pH")),
                                   Units = as.character(c("C", "uS/cm", "units")),
                                   Value = as.double(c(12.6, 1805, 7.68)))
  expect_equal(actual_value, expected_value)
  
})


test_that("WqStats works as expected", {
  
  actual_rows <- nrow(WqStats())
  expect_equal(actual_rows, 135)
  
  actual_cols <- colnames(WqStats())
  expected_cols <- c("Park", "FieldSeason", "Parameter", "Units", "Minimum", "FirstQuartile", "Median", "ThirdQuartile", "Maximum", "Count")
  expect_equal(actual_cols, expected_cols)
  
  actual_dbl <- WqStats()
  expect_equal(unique(sapply(actual_dbl[, 5:9], typeof)), "double")
  
  actual_stats <- WqStats() %>% dplyr::filter(Park == "MOJA", FieldSeason == "2019", Parameter == "Temperature") %>% dplyr::select(Minimum, FirstQuartile, Median, ThirdQuartile, Maximum)
  expected_stats <- tibble::as_tibble_row(c(Minimum = as.double(6.3), FirstQuartile = as.double(8.5), Median = as.double(10.4), ThirdQuartile = as.double(13.4), Maximum = as.double(19.2)))
  expect_equal(actual_stats, expected_stats)
  
})


test_that("qcLocalDOCheck returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcLocalDOCheck())
  expect_equal(actual_rows, 165)
  
  actual_cols <- colnames(qcLocalDOCheck())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "DOInstrument", "PreCalDO_percent", "PostCalDO_percent")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcLocalDOCheck()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_dbl <- qcLocalDOCheck()
  expect_equal(unique(sapply(actual_dbl[, 7:8], typeof)), "double")
  
})


test_that("qcSpCondStandardCheck works as expected", {
  
  actual_rows <- nrow(qcSpCondStandardCheck())
  expect_equal(actual_rows, 21)
  
  actual_cols <- colnames(qcSpCondStandardCheck())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "SpCondInstrument", "SpCondMedian", "SpCondStandard")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcSpCondStandardCheck()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_dbl <- qcSpCondStandardCheck()
  expect_equal(unique(sapply(actual_dbl[, 7:8], typeof)), "double")

})