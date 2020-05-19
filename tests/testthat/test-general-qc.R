context("Completeness")

dummy.completeness <- tibble::tibble(Park = c("DEVA", "DEVA", "DEVA", "DEVA", "LAKE", "LAKE", "LAKE", "PARA"),
                                     Subunit = c("DEVA Unknown", "DEVA Unknown", "DEVA Unknown", "DEVA Unknown", "LAKE Unknown", "LAKE Unknown", "LAKE Unknown", "PARA Unknown"),
                                     SiteCode = c("DEVA_P_SPR001", "DEVA_P_SPR002", "DEVA_P_SPR003", "DEVA_P_SPR003", "LAKE_P_SPR001", "LAKE_P_SPR002", "LAKE_P_SPR003", "PARA_P_SPR001"),
                                     SiteName = c("DEVA 1", "DEVA 2", "DEVA 3", "DEVA 3", "LAKE 1", "LAKE 2", "LAKE 3", "PARA 1"),
                                     VisitDate = c("2018-12-03", "2019-01-21", "2018-12-04", "2019-01-21", "2019-11-20", "2019-11-20", "2019-12-05", "2019-04-17"),
                                     FieldSeason = c("2019", "2019", "2019", "2019", "2020", "2020", "2020", "2019"),
                                     SampleFrame = c("Annual", "Over", "3Yr", "3Yr", "Rejected", "Annual", "Annual", "3Yr"),
                                     VisitType = c("Primary", "Primary", "Primary", "Replicate", "Primary", "Primary", "Primary", "Primary"),
                                     MonitoringStatus = c("Sampled", "Sampled", "Sampled", "Sampled", "Not sampled - No spring found", "Sampled", "Sampled", "Sampled"),
                                     SpringType = c("Rheocrene", "Limnocrene", "Rheocrene", "Rheocrene", NA, "Rheocrene", "Rheocrene", "Hillslope"))

dir <- "temp-test-csv"
dir.create(dir)
readr::write_csv(dummy.completeness, file.path(dir, "Visit.csv"))

test_that("QcCompleteness works as expected", {
  expected <- tibble::tibble(Park = c("DEVA", "DEVA", "LAKE", "PARA"),
                             FieldSeason = c("2019", "2019", "2020", "2019"),
                             SampleFrame = c("Annual", "3Yr", "Annual", "3Yr"),
                             MonitoringStatus = c("Sampled", "Sampled", "Sampled", "Sampled"),
                             Count = as.integer(c(1, 1, 2, 1)),
                             Percent = c(1/20*100, 1/60*100, 2/10*100, 1/35*100))
  expected$Percent <- round(expected$Percent, 3)
  result <- QcCompleteness(path.to.data = dir, data.source = "local")
  result_DEVA <- QcCompleteness(path.to.data = dir, data.source = "local", park = "DEVA")
  result_2019 <- QcCompleteness(path.to.data = dir, data.source = "local", field.season = "2019")
  expect_mapequal(result, expected)
})

# Remove temporary csv's
unlink(dir, recursive = TRUE)
