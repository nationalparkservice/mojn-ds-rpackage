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
  expect_equal(actual_rows, 11)
  
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
  expected_cols <- c("Park", "SiteCode", "SiteName", "FieldSeason", "VisitDate", "DeploymentDate")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcSensorsNotDeployed()
  expect_equal(class(actual_date$VisitDate), "Date")
  expect_equal(class(actual_date$DeploymentDate), "Date")
  
  actual_deployment <- qcSensorsNotDeployed() %>% dplyr::filter(SiteCode == "PARA_P_LIN0020") %>% dplyr::select(FieldSeason, DeploymentDate)
  expected_deployment <- tibble::tibble(FieldSeason = c("2016", "2020"),
                                        DeploymentDate = as.Date(c(NA, NA)))  
  expect_equal(actual_deployment, expected_deployment)
  
})


test_that("qcSensorsNotRecovered returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcSensorsNotRecovered())
  expect_equal(actual_rows, 102)
  
  actual_cols <- colnames(qcSensorsNotRecovered())
  expected_cols <- c("Park", "SiteCode", "SiteName", "FieldSeason", "VisitDate", "SensorNumber", "SerialNumber", "Notes")
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