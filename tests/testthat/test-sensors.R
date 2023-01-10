context("Sensor functions")
LoadDesertSprings(here::here("tests", "testthat", "test_data"))


test_that("qcSensorSummary works as expected", {
  
  actual_rows <- nrow(qcSensorSummary())
  expect_equal(actual_rows, 27)
  
  actual_cols <- colnames(qcSensorSummary())
  expected_cols <- c("Park", "DeploymentFieldSeason", "Deployed", "NoRetrievalAttempted", "RetrievalAttempted", "Retrieved", "Downloaded", "Percent_Retrieved", "Percent_Downloaded")
  expect_equal(actual_cols, expected_cols)
  
  actual_int <- qcSensorSummary()
  expect_equal(class(actual_int$Deployed), "integer")
  expect_equal(class(actual_int$NoRetrievalAttempted), "integer")
  expect_equal(class(actual_int$RetrievalAttempted), "integer")
  expect_equal(class(actual_int$Retrieved), "integer")
  expect_equal(class(actual_int$Downloaded), "integer")
  
  actual_dbl <- qcSensorSummary()
  expect_equal(unique(sapply(actual_dbl[, 8:9], typeof)), "double")

  actual_counts <- qcSensorSummary() %>% dplyr::filter(Park == "JOTR", DeploymentFieldSeason == "2018") %>% dplyr::select(Deployed, Retrieved, Downloaded)
  expected_counts <- tibble::as_tibble_row(c(Deployed = as.integer(10), Retrieved = as.integer(9), Downloaded = as.integer(4)))
  expect_equal(actual_counts, expected_counts)
  
  actual_percents <- qcSensorSummary() %>% dplyr::filter(Park == "JOTR", DeploymentFieldSeason == "2018") %>% dplyr::select(Percent_Retrieved, Percent_Downloaded)
  expected_percents <- tibble::as_tibble_row(c(Percent_Retrieved = round(as.double(9*100/10), 1),
                                               Percent_Downloaded = round(as.double(4*100/9), 1)))
  expect_equal(actual_percents, expected_percents)
  
})


test_that("qcSensorProblems returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcSensorProblems())
  expect_equal(actual_rows, 67)
  
  actual_cols <- colnames(qcSensorProblems())
  expected_cols <- c("Park", "SiteCode", "SiteName", "SensorNumber", "SerialNumber", "DeploymentDate", "DeploymentFieldSeason", "RetrievalDate", "RetrievalFieldSeason", "SensorRetrieved", "DownloadResult", "SensorProblem", "Notes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcSensorProblems()
  expect_equal(class(actual_date$DeploymentDate), "Date")
  expect_equal(class(actual_date$RetrievalDate), "Date")
  
  actual_int <- qcSensorProblems()
  expect_equal(class(actual_int$SensorNumber), "integer")
  
})


test_that("qcSensorDownloads returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcSensorDownloads())
  expect_equal(actual_rows, 12)
  
  actual_cols <- colnames(qcSensorDownloads())
  expected_cols <- c("Park", "SiteCode", "SiteName", "SensorNumber", "SerialNumber", "DeploymentDate", "DeploymentFieldSeason", "RetrievalDate", "RetrievalFieldSeason", "SensorRetrieved", "DownloadResult", "Notes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcSensorDownloads()
  expect_equal(class(actual_date$DeploymentDate), "Date")
  expect_equal(class(actual_date$RetrievalDate), "Date")
  
  actual_int <- qcSensorDownloads()
  expect_equal(class(actual_int$SensorNumber), "integer")
  
})


test_that("qcMissingSensors returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcMissingSensors())
  expect_equal(actual_rows, 3)
  
  actual_cols <- colnames(qcMissingSensors())
  expected_cols <- c("Park", "SiteCode", "SiteName", "DeploymentFieldSeason", "DeploymentDate", "RetrievalFieldSeason", "RetrievalDate", "SensorRetrieved", "DownloadResult", "SensorProblem", "SensorNumber", "SerialNumber", "Notes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcMissingSensors()
  expect_equal(class(actual_date$DeploymentDate), "Date")
  
  actual_int <- qcMissingSensors()
  expect_equal(class(actual_int$SensorNumber), "integer")
  
})


test_that("qcSensorDates returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcSensorDates())
  expect_equal(actual_rows, 0)
  
  actual_cols <- colnames(qcSensorDates())
  expected_cols <- c("Park", "SiteCode", "SiteName", "SensorNumber", "SerialNumber", "DeploymentDate", "DeploymentFieldSeason", "RetrievalDate", "RetrievalFieldSeason", "SensorRetrieved", "SensorProblem", "DownloadResult", "RetrievalVisitType", "DeploymentVisitType", "Notes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcSensorDates()
  expect_equal(class(actual_date$DeploymentDate), "Date")
  expect_equal(class(actual_date$RetrievalDate), "Date")
  
  actual_int <- qcSensorDates()
  expect_equal(class(actual_int$SensorNumber), "integer")
  
})


test_that("qcSensorsNotDeployed returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcSensorsNotDeployed())
  expect_equal(actual_rows, 13)
  
  actual_cols <- colnames(qcSensorsNotDeployed())
  expected_cols <- c("Park", "SiteCode", "SiteName", "DeploymentFieldSeason")
  expect_equal(actual_cols, expected_cols)
 
  actual_deployment <- qcSensorsNotDeployed() %>% dplyr::filter(SiteCode == "PARA_P_LIN0020") %>% dplyr::select(SiteCode, DeploymentFieldSeason)
  expected_deployment <- tibble::tibble(SiteCode = c("PARA_P_LIN0020", "PARA_P_LIN0020"),
                                        DeploymentFieldSeason = c("2016", "2020"))  
  expect_equal(actual_deployment, expected_deployment)
  
})


test_that("qcSensorsNotRecovered returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcSensorsNotRecovered())
  expect_equal(actual_rows, 103)
  
  actual_cols <- colnames(qcSensorsNotRecovered())
  expected_cols <- c("Park", "SiteCode", "SiteName", "FieldSeason", "VisitDate", "RetrievalVisitType", "SensorNumber", "SerialNumber", "SensorRetrieved", "SensorProblem", "Notes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcSensorsNotRecovered()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_int <- qcSensorsNotRecovered()
  expect_equal(class(actual_int$SensorNumber), "integer")
  
  actual_retrieval <- qcSensorsNotRecovered() %>% dplyr::filter(SiteCode == "DEVA_P_LCM0255") %>% dplyr::select(FieldSeason, VisitDate, SensorNumber, SerialNumber)
  expected_retrieval <- tibble::tibble(FieldSeason = c("2019", "2020"),
                                       VisitDate = as.Date(c(NA, "2020-01-21")),
                                       SensorNumber = as.integer(c(NA, 238)),
                                       SerialNumber = c(NA, "unknown"))  
  expect_equal(actual_retrieval, expected_retrieval)
  
})


test_that("qcUnknownSensorIDs returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcUnknownSensorIDs())
  expect_equal(actual_rows, 9)
  
  actual_cols <- colnames(qcUnknownSensorIDs())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "SensorNumber", "SerialNumber", "Notes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcUnknownSensorIDs()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_int <- qcUnknownSensorIDs()
  expect_equal(class(actual_int$SensorNumber), "integer")
  
})