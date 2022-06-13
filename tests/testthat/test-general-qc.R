context("Completeness")

# dummy.completeness <- tibble::tibble(Park = c("DEVA", "DEVA", "DEVA", "DEVA", "LAKE", "LAKE", "LAKE", "PARA"),
#                                      Subunit = c("DEVA Unknown", "DEVA Unknown", "DEVA Unknown", "DEVA Unknown", "LAKE Unknown", "LAKE Unknown", "LAKE Unknown", "PARA Unknown"),
#                                      SiteCode = c("DEVA_P_SPR001", "DEVA_P_SPR002", "DEVA_P_SPR003", "DEVA_P_SPR003", "LAKE_P_SPR001", "LAKE_P_SPR002", "LAKE_P_SPR003", "PARA_P_SPR001"),
#                                      SiteName = c("DEVA 1", "DEVA 2", "DEVA 3", "DEVA 3", "LAKE 1", "LAKE 2", "LAKE 3", "PARA 1"),
#                                      VisitDate = c("2018-12-03", "2019-01-21", "2018-12-04", "2019-01-21", "2019-11-20", "2019-11-20", "2019-12-05", "2019-04-17"),
#                                      FieldSeason = c("2019", "2019", "2019", "2019", "2020", "2020", "2020", "2019"),
#                                      SampleFrame = c("Annual", "Over", "3Yr", "3Yr", "Rejected", "Annual", "Annual", "3Yr"),
#                                      VisitType = c("Primary", "Primary", "Primary", "Replicate", "Primary", "Primary", "Primary", "Primary"),
#                                      MonitoringStatus = c("Sampled", "Sampled", "Sampled", "Sampled", "Not sampled - No spring found", "Sampled", "Sampled", "Sampled"),
#                                      SpringType = c("Rheocrene", "Limnocrene", "Rheocrene", "Rheocrene", NA, "Rheocrene", "Rheocrene", "Hillslope"))
# 
# dir <- "temp-test-csv"
# dir.create(dir)
# readr::write_csv(dummy.completeness, file.path(dir, "Visit.csv"))
# 
# test_that("QcCompleteness works as expected", {
#   expected <- tibble::tibble(Park = c("DEVA", "DEVA", "LAKE", "PARA"),
#                              FieldSeason = c("2019", "2019", "2020", "2019"),
#                              SampleFrame = c("Annual", "3Yr", "Annual", "3Yr"),
#                              MonitoringStatus = c("Sampled", "Sampled", "Sampled", "Sampled"),
#                              Count = as.integer(c(1, 1, 2, 1)),
#                              Percent = c(1/20*100, 1/60*100, 2/10*100, 1/35*100))
#   expected$Percent <- round(expected$Percent, 3)
#   result <- QcCompleteness(path.to.data = dir, data.source = "local")
#   result_DEVA <- QcCompleteness(path.to.data = dir, data.source = "local", park = "DEVA")
#   result_2019 <- QcCompleteness(path.to.data = dir, data.source = "local", field.season = "2019")
#   result_CAMO <- QcCompleteness(path.to.data = dir, data.source = "local", park = "CAMO")
#   expect_dataframe_equal(result, expected)
#   expect_dataframe_equal(result_DEVA, dplyr::filter(expected, Park == "DEVA"))
#   expect_dataframe_equal(result_2019, dplyr::filter(expected, FieldSeason == "2019"))
#   expect_equal(nrow(result_CAMO), 0)
# })


test_that("qcCompleteness works as expected", {
  
  actual_rows <- nrow(qcCompleteness(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expect_equal(actual_rows, 49)
  
  actual_cols <- colnames(qcCompleteness(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_cols <- c("Park", "FieldSeason", "SampleFrame", "MonitoringStatus", "Count", "Percent")
  expect_equal(actual_cols, expected_cols)
  
  actual_int <- qcCompleteness(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local")
  expect_equal(class(actual_int$Count), "integer")
  
  actual_dbl <- qcCompleteness(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local")
  expect_equal(typeof(actual_dbl$Percent), "double")
  
  actual_count <- (qcCompleteness(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local") %>% dplyr::filter(Park == "DEVA", FieldSeason == "2018", SampleFrame == "3Yr", MonitoringStatus == "Sampled") %>% dplyr::select(Count))[1,]
  expected_count <- as_tibble(as.integer(59)) %>% dplyr::rename(Count = value)
  expect_equal(actual_count, expected_count)
  
  actual_percent <- (qcCompleteness(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local") %>% dplyr::filter(Park == "DEVA", FieldSeason == "2018", SampleFrame == "3Yr", MonitoringStatus == "Sampled") %>% dplyr::select(Percent))[1,]
  expected_percent <- as_tibble(as.double(59*100/60)) %>% dplyr::rename(Percent = value)
  expect_equal(actual_count, expected_count)
  
})


test_that("qcDPLCheck returns correct number of rows and columns", {

  actual_rows <- nrow(qcDPLCheck(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local") %>% filter(FieldSeason == "2018"))
  expect_equal(actual_rows, 125)
  
  actual_cols <- colnames(qcDPLCheck(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "SampleFrame", "VisitType", "Visit", "FlowCondition", "EstimatedDischarge", "VolumetricDischarge", "Disturbance", "FlowModification", "Wildlife", "Riparian", "Invasives", "Temperature", "pH", "SpCond", "DisOxygen")
  expect_equal(actual_cols, expected_cols)
    
})


test_that("qcSpringTypeDiscrepancies returns correct number of rows and columns", {

  actual_rows <- nrow(qcSpringTypeDiscrepancies(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expect_equal(actual_rows, 76)
  
  actual_cols <- colnames(qcDPLCheck(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_cols <- c("Park", "SiteCode", "SiteName", "SpringType", "FieldSeasons")
  expect_equal(actual_cols, expected_cols)
    
})


test_that("qcVisitDate returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcVisitDate(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expect_equal(actual_rows, 246)
  
  actual_cols <- colnames(qcDPLCheck(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_cols <- c("Park", "SiteCode", "SiteName", "SampleFrame", "VisitDates")
  expect_equal(actual_cols, expected_cols)
  
})


test_that("qcNotSampled returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcNotSampled(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expect_equal(actual_rows, 35)
  
  actual_cols <- colnames(qcNotSampled(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "SampleFrame", "VisitType", "MonitoringStatus", "Notes")
  expect_equal(actual_cols, expected_cols)
  
})

# Remove temporary CSV files
# unlink(dir, recursive = TRUE)
