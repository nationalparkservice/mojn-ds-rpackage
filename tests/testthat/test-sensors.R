context("Sensor functions")

# dummy.attempts <- tibble::tibble(Park = c("JOTR", "JOTR", "LAKE", "LAKE"),
#                          SiteCode = c("JOTR_P_SPR0001", "JOTR_P_SPR0002", "LAKE_P_SPR0001", "LAKE_P_SPR0001"),
#                          SiteName = c("JOTR 1", "JOTR 2", "LAKE 1", "LAKE 1"),
#                          DeploymentDate = c("2018-12-01", "2018-12-02", "2017-12-01", "2018-11-01"),
#                          RetrievalDate = c("2019-12-01", "2019-12-02", "2018-12-01", "2019-11-01"),
#                          DeploymentFieldSeason = c("2019", "2019", "2018", "2019"),
#                          RetrievalFieldSeason = c("2020", "2020", "2019", "2020"),
#                          SensorNumber = c(1:4),
#                          SerialNumber = c("1", "2", "3", "4"),
#                          SensorRetrieved = c("Y", "Y", "N", "Y"),
#                          SensorProblem = c("None", "Damaged", "Missing", "None"),
#                          DownloadResult = c("Y", "N", "N", "Y"),
#                          RetrievalVisitType = rep("Primary", 4),
#                          DeploymentVisitType = rep("Primary", 4))
# 
# dummy.deployed <- tibble::tibble(Park = c("JOTR", "MOJA"),
#                          SiteCode = c("JOTR_P_SPR0003", "MOJA_P_SPR0001"),
#                          SiteName = c("JOTR 3", "MOJA 1"),
#                          VisitDate = c("2018-11-01", "2016-11-01"),
#                          FieldSeason = c("2019", "2017"),
#                          SensorNumber = c(5, 6),
#                          SerialNumber = c("5", "6"),
#                          VisitType = rep("Primary", 2))
# 
# dir <- "temp-test-csv"
# dir.create(dir)
# readr::write_csv(dummy.attempts, file.path(dir, "SensorRetrievalAttempts.csv"))
# readr::write_csv(dummy.deployed, file.path(dir, "SensorsCurrentlyDeployed.csv"))
# 
# test_that("SensorQcSummary works as expected", {
#   expected <- tibble::tibble(Park = c("JOTR", "LAKE", "LAKE", "MOJA"),
#                      DeploymentFieldSeason = c("2019", "2018", "2019", "2017"),
#                      Deployed = c(3, 1, 1, 1),
#                      NoRetrievalAttempted = c(1, 0, 0, 1),
#                      RetrievalAttempted = c(2, 1, 1, 0),
#                      Retrieved = c(2, 0, 1, 0),
#                      Downloaded = c(1, 0, 1, 0))
#   result <- SensorQcSummary(path.to.data = dir, data.source = "local")
#   result_LAKE <- SensorQcSummary(path.to.data = dir, park = "LAKE", data.source = "local")
#   result_2019 <- SensorQcSummary(path.to.data = dir, deployment.field.season = "2019", data.source = "local")
#   expect_dataframe_equal(result, expected)
#   expect_dataframe_equal(result_LAKE, dplyr::filter(expected, Park == "LAKE"))
#   expect_dataframe_equal(result_2019, dplyr::filter(expected, DeploymentFieldSeason == "2019"))
# })
# 
# # Remove temporary csv's
# unlink(dir, recursive = TRUE)

test_that("qcSensorSummary works as expected", {
  
  actual_rows <- nrow(qcSensorSummary(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expect_equal(actual_rows, 27)
  
  actual_cols <- colnames(qcSensorSummary(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_cols <- c("Park", "DeploymentFieldSeason", "Deployed", "NoRetrievalAttempted", "RetrievalAttempted", "Retrieved", "Downloaded", "Percent_Retrieved", "Percent_Downloaded")
  expect_equal(actual_cols, expected_cols)
  
  actual_dbl <- qcSensorSummary(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local")
  expect_equal(unique(sapply(actual_dbl[, 3:9], typeof)), "double")

  actual_counts <- qcSensorSummary(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local") %>% dplyr::filter(Park == "JOTR", DeploymentFieldSeason == "2018") %>% dplyr::select(Deployed, Retrieved, Downloaded)
  expected_counts <- as_tibble_row(c(Deployed = as.double(10), Retrieved = as.double(9), Downloaded = as.double(4)))
  expect_equal(actual_counts, expected_counts)
  
  actual_percents <- qcSensorSummary(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local") %>% dplyr::filter(Park == "JOTR", DeploymentFieldSeason == "2018") %>% dplyr::select(Percent_Retrieved, Percent_Downloaded)
  expected_percents <- as_tibble_row(c(Percent_Retrieved = round(as.double(9*100/10), 1), Percent_Downloaded = round(as.double(4*100/9), 1)))
  expect_equal(actual_percents, expected_percents)
  
})


test_that("qcSensorProblems returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcSensorProblems(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expect_equal(actual_rows, 67)
  
  actual_cols <- colnames(qcSensorProblems(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_cols <- c("SensorNumber", "SerialNumber", "DeploymentDate", "DeploymentFieldSeason", "RetrievalDate", "RetrievalFieldSeason", "SiteName", "SiteCode", "Park", "SensorRetrieved", "DownloadResult", "SensorProblem")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcSensorProblems(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local")
  expect_equal(class(actual_date$DeploymentDate), "Date")
  expect_equal(class(actual_date$RetrievalDate), "Date")
  
  actual_int <- qcSensorProblems(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local")
  expect_equal(class(actual_int$SensorNumber), "integer")
  
})


test_that("qcSensorDownloads returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcSensorDownloads(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expect_equal(actual_rows, 11)
  
  actual_cols <- colnames(qcSensorDownloads(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_cols <- c("SensorNumber", "SerialNumber", "DeploymentDate", "DeploymentFieldSeason", "RetrievalDate", "RetrievalFieldSeason", "SiteName", "SiteCode", "Park", "SensorRetrieved", "DownloadResult")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcSensorDownloads(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local")
  expect_equal(class(actual_date$DeploymentDate), "Date")
  expect_equal(class(actual_date$RetrievalDate), "Date")
  
  actual_int <- qcSensorDownloads(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local")
  expect_equal(class(actual_int$SensorNumber), "integer")
  
})


test_that("qcMissingSensors returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcMissingSensors(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expect_equal(actual_rows, 68)
  
  actual_cols <- colnames(qcMissingSensors(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_cols <- c("SensorNumber", "SerialNumber", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "Park", "Notes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcMissingSensors(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local")
  expect_equal(class(actual_date$VisitDate), "VisitDate")
  
  actual_int <- qcMissingSensors(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local")
  expect_equal(class(actual_int$SensorNumber), "integer")
  
})


test_that("qcSensorDates returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcSensorDates(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expect_equal(actual_rows, 1)
  
  actual_cols <- colnames(qcSensorDates(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_cols <- c("SensorNumber", "SerialNumber", "DeploymentDate", "DeploymentFieldSeason", "RetrievalDate", "RetrievalFieldSeason", "SiteName", "SiteCode", "Park", "SensorRetrieved", "SensorProblem", "DownloadResult")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcSensorDates(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local")
  expect_equal(class(actual_date$DeploymentDate), "Date")
  expect_equal(class(actual_date$RetrievalDate), "Date")
  
  actual_int <- qcSensorDates(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local")
  expect_equal(class(actual_int$SensorNumber), "integer")
  
})


test_that("qcSensorsNoData returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcSensorsNoData(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expect_equal(actual_rows, 59)
  
  actual_cols <- colnames(qcSensorsNoData(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_cols <- c("Park", "SiteCode", "SiteName", "SampleFrame", "DeploymentDate", "DeploymentFieldSeason", "RetrievalDate", "RetrievalFieldSeason", "SensorNumber", "SensorRetrieved")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcSensorsNoData(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local")
  expect_equal(class(actual_date$DeploymentDate), "Date")
  expect_equal(class(actual_date$RetrievalDate), "Date")
  
  actual_int <- qcSensorsNoData(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local")
  expect_equal(class(actual_int$SensorNumber), "integer")
  
})