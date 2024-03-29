context("Discharge")
LoadDesertSprings(dir)


test_that("VolumetricMedian works as expected", {
  
  actual_rows <- nrow(VolumetricMedian(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 53)
  
  actual_cols <- colnames(VolumetricMedian())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "SampleFrame", "Panel", "VisitType", "DPL", "Discharge_L_per_s", "Count")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- VolumetricMedian()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_dbl <- VolumetricMedian()
  expect_equal(typeof(actual_dbl$VisitDate), "double")
  
  actual_discharge <- VolumetricMedian() %>% dplyr::filter(SiteCode == "LAKE_P_SAL0019", FieldSeason == "2016") %>% dplyr::select(Discharge_L_per_s)
  expected_discharge <- tibble::tibble(Discharge_L_per_s = as.double(4.968944))
  expect_equal(actual_discharge, expected_discharge, tolerance = 0.0001)
    
})


test_that("SpringDischarge returns expected number of rows and columns", {
  actual_rows <- nrow(SpringDischarge(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 818)
  
  actual_cols <- colnames(SpringDischarge())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "SampleFrame", "Panel", "VisitType", "FlowCondition", "VolDischarge_L_per_s", "DischargeClass_L_per_s", "SpringbrookType", "SpringbrookLengthFlag", "SpringbrookLength_m", "SpringbrookWidth_m", "DiscontinuousSpringbrookLengthFlag", "DiscontinuousSpringbrookLength_m","DischargeNotes", "SpringbrookNotes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- SpringDischarge()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_dbl <- SpringDischarge()
  expect_equal(typeof(actual_dbl$VolDischarge_L_per_s), "double")
  expect_equal(typeof(actual_dbl$SpringbrookLength_m), "double")
  expect_equal(typeof(actual_dbl$SpringbrookWidth_m), "double")
  expect_equal(typeof(actual_dbl$DiscontinuousSpringbrookLength_m), "double")
  
})


test_that("qcSpringDryWater returns expected number of rows and columns", {
  
  actual_rows <- nrow(qcSpringDryWater(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 0)
  
  actual_cols <- colnames(qcSpringDryWater())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "FlowCondition", "VolDischarge_L_per_s", "DischargeClass_L_per_s", "SpringbrookLengthFlag", "SpringbrookLength_m", "SpringbrookWidth_m", "DischargeNotes", "SpringbrookNotes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcSpringDryWater()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_dbl <- qcSpringDryWater()
  expect_equal(typeof(actual_dbl$VolDischarge_L_per_s), "double")
  expect_equal(typeof(actual_dbl$SpringbrookLength_m), "double")
  expect_equal(typeof(actual_dbl$SpringbrookWidth_m), "double")
  
})


test_that("qcSpringNotDryNoDischarge returns expected number of rows and columns", {

  actual_rows <- nrow(qcSpringNotDryNoDischarge(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 9)
  
  actual_cols <- colnames(qcSpringNotDryNoDischarge())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "SampleFrame", "Panel", "VisitType", "FlowCondition", "VolDischarge_L_per_s", "DischargeClass_L_per_s", "SpringbrookType", "SpringbrookLengthFlag", "SpringbrookLength_m", "SpringbrookWidth_m", "DiscontinuousSpringbrookLengthFlag", "DiscontinuousSpringbrookLength_m", "DischargeNotes", "SpringbrookNotes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcSpringNotDryNoDischarge()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_dbl <- qcSpringNotDryNoDischarge()
  expect_equal(typeof(actual_dbl$VolDischarge_L_per_s), "double")
  expect_equal(typeof(actual_dbl$SpringbrookLength_m), "double")
  expect_equal(typeof(actual_dbl$SpringbrookWidth_m), "double")
  expect_equal(typeof(actual_dbl$DiscontinuousSpringbrookLength_m), "double")
    
})


test_that("qcSpringNotDryNoSpringbrook returns expected number of rows and columns", {
  
  actual_rows <- nrow(qcSpringNotDryNoSpringbrook(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 16)
  
  actual_cols <- colnames(qcSpringNotDryNoSpringbrook())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "SampleFrame", "Panel", "VisitType", "FlowCondition", "VolDischarge_L_per_s", "DischargeClass_L_per_s", "SpringbrookType", "SpringbrookLengthFlag", "SpringbrookLength_m", "SpringbrookWidth_m", "DiscontinuousSpringbrookLengthFlag", "DiscontinuousSpringbrookLength_m", "DischargeNotes", "SpringbrookNotes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcSpringNotDryNoSpringbrook()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_dbl <- qcSpringNotDryNoSpringbrook()
  expect_equal(typeof(actual_dbl$VolDischarge_L_per_s), "double")
  expect_equal(typeof(actual_dbl$SpringbrookLength_m), "double")
  expect_equal(typeof(actual_dbl$SpringbrookWidth_m), "double")
  expect_equal(typeof(actual_dbl$DiscontinuousSpringbrookLength_m), "double")
  
})


test_that("qcSpringNotDryNoWater returns expected number of rows and columns", {
  
  actual_rows <- nrow(qcSpringNotDryNoWater(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 24)
  
  actual_cols <- colnames(qcSpringNotDryNoWater())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "FlowCondition", "VolDischarge_L_per_s", "DischargeClass_L_per_s", "SpringbrookLengthFlag", "SpringbrookLength_m", "SpringbrookWidth_m", "DischargeNotes", "SpringbrookNotes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcSpringNotDryNoWater()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_dbl <- qcSpringNotDryNoWater()
  expect_equal(typeof(actual_dbl$VolDischarge_L_per_s), "double")
  expect_equal(typeof(actual_dbl$SpringbrookLength_m), "double")
  expect_equal(typeof(actual_dbl$SpringbrookWidth_m), "double")

})


test_that("qcDischargeMissing returns expected number of rows and columns", {
  
  actual_rows <- nrow(qcDischargeMissing(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 111)
  
  actual_cols <- colnames(qcDischargeMissing())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "FlowCondition", "VolDischarge_L_per_s", "DischargeClass_L_per_s", "DischargeNotes", "SpringbrookNotes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcDischargeMissing()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_dbl <- qcDischargeMissing()
  expect_equal(typeof(actual_dbl$VolDischarge_L_per_s), "double")
  
})


test_that("qcVolumetricMissing returns expected number of rows and columns", {
  
  actual_rows <- nrow(qcVolumetricMissing(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 0)
  
  actual_cols <- colnames(qcVolumetricMissing())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "FlowCondition", "ContainerVolume_mL", "FillTime_seconds", "EstimatedCapture_percent")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcVolumetricMissing()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_dbl <- qcVolumetricMissing()
  expect_equal(typeof(actual_dbl$FillTime_seconds), "double")
  expect_equal(typeof(actual_dbl$EstimatedCapture_percent), "double")

  actual_int <- qcVolumetricMissing()
  expect_equal(class(actual_int$ContainerVolume_mL), "integer")
  
})


test_that("qcVolumetricFillEvents works as expected", {

  actual_rows <- nrow(qcVolumetricFillEvents(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 5)
  
  actual_cols <- colnames(qcVolumetricFillEvents())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "Discharge_L_per_s", "Count")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcVolumetricFillEvents()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_dbl <- qcVolumetricFillEvents()
  expect_equal(typeof(actual_dbl$Discharge_L_per_s), "double")
  
  actual_int <- qcVolumetricFillEvents()
  expect_equal(class(actual_int$Count), "integer")
  
  actual_count <- qcVolumetricFillEvents() %>% dplyr::filter(SiteCode == "MOJA_P_MAR0147", FieldSeason == "2019") %>% dplyr::select(Count)
  expected_count <- tibble::tibble(Count = as.integer(2))
  expect_equal(actual_count, expected_count)
  
})


test_that("qcVolumetricTimes works as expected", {
  
  actual_rows <- nrow(qcVolumetricTimes(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 19)
  
  actual_cols <- colnames(qcVolumetricTimes())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "MedianFillTime_s")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcVolumetricTimes()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_dbl <- qcVolumetricTimes()
  expect_equal(typeof(actual_dbl$MedianFillTime_s), "double")
  
  actual_median <- qcVolumetricTimes() %>% dplyr::filter(SiteCode == "PARA_P_LIM0080", FieldSeason == "2020") %>% dplyr::select(MedianFillTime_s)
  expected_median <- tibble::tibble(MedianFillTime_s = as.double(3.015))
  expect_equal(actual_median, expected_median)
  
})


test_that("qcContinuousLength returns expected number of rows and columns", {
  
  actual_rows <- nrow(qcContinuousLength(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 0)
  
  actual_cols <- colnames(qcContinuousLength())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "SampleFrame", "Panel", "VisitType", "FlowCondition", "VolDischarge_L_per_s", "DischargeClass_L_per_s", "SpringbrookType", "SpringbrookLengthFlag", "SpringbrookLength_m", "SpringbrookWidth_m", "DiscontinuousSpringbrookLengthFlag", "DiscontinuousSpringbrookLength_m", "DischargeNotes", "SpringbrookNotes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcContinuousLength()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_dbl <- qcContinuousLength()
  expect_equal(typeof(actual_dbl$VolDischarge_L_per_s), "double")
  expect_equal(typeof(actual_dbl$SpringbrookLength_m), "double")
  expect_equal(typeof(actual_dbl$SpringbrookWidth_m), "double")
  expect_equal(typeof(actual_dbl$DiscontinuousSpringbrookLength_m), "double")
  
})


test_that("FlowCategoriesContinuous works as expected", {
  
  actual_rows <- nrow(FlowCategoriesContinuous(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 189)
  
  actual_cols <- colnames(FlowCategoriesContinuous())
  expected_cols <- c("Park", "FieldSeason", "SampleFrame", "Panel", "FlowCategory", "Count")
  expect_equal(actual_cols, expected_cols)
  
  actual_int <- FlowCategoriesContinuous()
  expect_equal(class(actual_int$Count), "integer")
  
  actual_count <- FlowCategoriesContinuous() %>% dplyr::filter(Park == "DEVA", FieldSeason == "2021") %>% dplyr::select(SampleFrame, FlowCategory, Count)
  expected_count <- tibble::tibble(SampleFrame = c("3Yr", "3Yr", "3Yr", "3Yr", "3Yr", "3Yr", "Annual", "Annual", "Annual", "Annual", "Annual"),
                                    FlowCategory = c("10 - 50 m", "< 10 m", "> 50 m", "Dry", "No Data", "Wet Soil", "10 - 50 m", "< 10 m", "> 50 m", "Dry", "Wet Soil"),
                                    Count = as.integer(c(5, 7, 2, 8, 35, 3, 7, 5, 4, 3, 1)))
  expect_equal(actual_count, expected_count)
  
})


test_that("FlowCategoriesDiscontinuous works as expected", {
  
  actual_rows <- nrow(FlowCategoriesDiscontinuous(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 193)
  
  actual_cols <- colnames(FlowCategoriesDiscontinuous())
  expected_cols <- c("Park", "FieldSeason", "SampleFrame", "Panel", "FlowCategory", "Count")
  expect_equal(actual_cols, expected_cols)
  
  actual_int <- FlowCategoriesDiscontinuous()
  expect_equal(class(actual_int$Count), "integer")
  
  actual_count <- FlowCategoriesDiscontinuous() %>% dplyr::filter(Park == "DEVA", FieldSeason == "2021") %>% dplyr::select(SampleFrame, FlowCategory, Count)
  expected_count <- tibble::tibble(SampleFrame = c("3Yr", "3Yr", "3Yr", "3Yr", "3Yr", "3Yr", "Annual", "Annual", "Annual", "Annual", "Annual"),
                                    FlowCategory = c("10 - 50 m", "< 10 m", "> 50 m", "Dry", "No Data", "Wet Soil", "10 - 50 m", "< 10 m", "> 50 m", "Dry", "Wet Soil"),
                                    Count = as.integer(c(3, 3, 8, 8, 35, 3, 6, 5, 5, 3, 1)))
  expect_equal(actual_count, expected_count)
  
})