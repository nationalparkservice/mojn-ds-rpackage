context("Sensor functions")

dummy.attempts <- tibble::tibble(Park = c("JOTR", "JOTR", "LAKE", "LAKE"),
                         SiteCode = c("JOTR_P_SPR0001", "JOTR_P_SPR0002", "LAKE_P_SPR0001", "LAKE_P_SPR0001"),
                         SiteName = c("JOTR 1", "JOTR 2", "LAKE 1", "LAKE 1"),
                         DeploymentDate = c("2018-12-01", "2018-12-02", "2017-12-01", "2018-11-01"),
                         RetrievalDate = c("2019-12-01", "2019-12-02", "2018-12-01", "2019-11-01"),
                         DeploymentFieldSeason = c("2019", "2019", "2018", "2019"),
                         RetrievalFieldSeason = c("2020", "2020", "2019", "2020"),
                         SensorNumber = c(1:4),
                         SerialNumber = c("1", "2", "3", "4"),
                         SensorRetrieved = c("Y", "Y", "N", "Y"),
                         SensorProblem = c("None", "Damaged", "Missing", "None"),
                         DownloadResult = c("Y", "N", "N", "Y"),
                         RetrievalVisitType = rep("Primary", 4),
                         DeploymentVisitType = rep("Primary", 4))

dummy.deployed <- tibble::tibble(Park = c("JOTR", "MOJA"),
                         SiteCode = c("JOTR_P_SPR0003", "MOJA_P_SPR0001"),
                         SiteName = c("JOTR 3", "MOJA 1"),
                         VisitDate = c("2018-11-01", "2016-11-01"),
                         FieldSeason = c("2019", "2017"),
                         SensorNumber = c(5, 6),
                         SerialNumber = c("5", "6"),
                         VisitType = rep("Primary", 2))

dir <- "temp-test-csv"
dir.create(dir)
readr::write_csv(dummy.attempts, file.path(dir, "SensorRetrievalAttempts.csv"))
readr::write_csv(dummy.deployed, file.path(dir, "SensorsCurrentlyDeployed.csv"))

test_that("SensorQcSummary works as expected", {
  expected <- tibble::tibble(Park = c("JOTR", "LAKE", "LAKE", "MOJA"),
                     DeploymentFieldSeason = c("2019", "2018", "2019", "2017"),
                     Deployed = c(3, 1, 1, 1),
                     NoRetrievalAttempted = c(1, 0, 0, 1),
                     RetrievalAttempted = c(2, 1, 1, 0),
                     Retrieved = c(2, 0, 1, 0),
                     Downloaded = c(1, 0, 1, 0))
  result <- SensorQcSummary(path.to.data = dir, data.source = "local")
  expect_mapequal(result, expected)
})

# Remove temporary csv's
unlink(dir, recursive = TRUE)
