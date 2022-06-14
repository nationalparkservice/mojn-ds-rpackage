context("Discharge")

test_that("VolumetricMedian works as expected", {
  
  actual_rows <- nrow(VolumetricMedian(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expect_equal(actual_rows, 36)
  
  actual_cols <- colnames(VolumetricMedian(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "Discharge_L_per_s", "Count")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- VolumetricMedian(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local")
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_dbl <- VolumetricMedian(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local")
  expect_equal(typeof(actual_dbl$VisitDate), "double")
  
  actual_discharge <- VolumetricMedian(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local") %>% dplyr::filter(SiteCode == "LAKE_P_SAL0019", FieldSeason == "2016") %>% select(Discharge_L_per_s)
  expected_discharge <- tibble(Discharge_L_per_s = as.double(4.968944))
  expect_equal(actual_discharge, expected_discharge, tolerance = 0.0001)
    
})


test_that("SpringDischarge", {
  
})


test_that("qcSpringDryWater", {
  
})


test_that("qcSpringNotDryNoDischarge", {
  
})


test_that("qcSpringNotDryNoSpringbrook", {
  
})


test_that("qcSpringNotDryNoWater", {
  
})


test_that("qcDischargeMissing", {
  
})


test_that("qcVolumetricMissing", {
  
})


test_that("qcVolumetricFillEvents", {
  
})


test_that("qcVolumetricTimes", {
  
})


test_that("qcContinuousLength", {
  
})


test_that("FlowCategoriesContinuous", {
  
})


test_that("FlowCategoriesDiscontinuous", {
  
})