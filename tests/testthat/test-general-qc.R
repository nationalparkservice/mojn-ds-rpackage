context("Completeness")
LoadDesertSprings(here::here("tests", "testthat", "test_data"))


test_that("qcCompleteness works as expected", {
  
  actual_rows <- nrow(qcCompleteness())
  expect_equal(actual_rows, 49)
  
  actual_cols <- colnames(qcCompleteness())
  expected_cols <- c("Park", "FieldSeason", "SampleFrame", "MonitoringStatus", "Count", "Percent")
  expect_equal(actual_cols, expected_cols)
  
  actual_int <- qcCompleteness()
  expect_equal(class(actual_int$Count), "integer")
  
  actual_dbl <- qcCompleteness()
  expect_equal(typeof(actual_dbl$Percent), "double")
  
  actual_count <- (qcCompleteness() %>% dplyr::filter(Park == "DEVA", FieldSeason == "2018", SampleFrame == "3Yr", MonitoringStatus == "Sampled") %>% dplyr::select(Count))[1,]
  expected_count <- tibble::as_tibble(as.integer(59)) %>% dplyr::rename(Count = value)
  expect_equal(actual_count, expected_count)
  
  actual_percent <- (qcCompleteness() %>% dplyr::filter(Park == "DEVA", FieldSeason == "2018", SampleFrame == "3Yr", MonitoringStatus == "Sampled") %>% dplyr::select(Percent))[1,]
  expected_percent <- tibble::as_tibble(as.double(59*100/60)) %>% dplyr::rename(Percent = value)
  expect_equal(actual_count, expected_count)
  
})


test_that("qcDPLCheck returns correct number of rows and columns", {

  actual_rows <- nrow(qcDPLCheck() %>% dplyr::filter(FieldSeason == "2018"))
  expect_equal(actual_rows, 125)
  
  actual_cols <- colnames(qcDPLCheck())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "SampleFrame", "VisitType", "Visit", "FlowCondition", "EstimatedDischarge", "VolumetricDischarge", "Disturbance", "FlowModification", "Wildlife", "Riparian", "Invasives", "Temperature", "pH", "SpCond", "DisOxygen")
  expect_equal(actual_cols, expected_cols)
    
})


test_that("qcSpringTypeDiscrepancies returns correct number of rows and columns", {

  actual_rows <- nrow(qcSpringTypeDiscrepancies())
  expect_equal(actual_rows, 76)
  
  actual_cols <- colnames(qcSpringTypeDiscrepancies())
  expected_cols <- c("Park", "SiteCode", "SiteName", "SpringType", "FieldSeasons")
  expect_equal(actual_cols, expected_cols)
    
})


test_that("qcVisitsBySite works as expected", {
  
  actual_rows <- nrow(qcVisitsBySite())
  expect_equal(actual_rows, 246)
  
  actual_cols <- colnames(qcVisitsBySite())
  expected_cols <- c("Park", "SiteCode", "SiteName", "SampleFrame", "VisitDates")
  expect_equal(actual_cols, expected_cols)
  
  actual_character <- qcVisitsBySite()
  expect_equal(class(actual_character$VisitDates), "character")
  
  actual_values <- qcVisitsBySite() %>% dplyr::filter(SiteCode == "LAKE_P_HOR0042") %>% dplyr::select(VisitDates)
  expected_values <- tibble::as_tibble(as.character("Oct 25 (2021), Oct 27 (2020), Nov 1 (2017), Nov 4 (2019), Nov 27 (2018), Apr 5 (2016)")) %>% dplyr::rename(VisitDates = value)
  expect_equal(actual_values, expected_values)
  
  
})


test_that("qcVisitsByDate works as expected", {
  
  actual_rows <- nrow(qcVisitsByDate())
  expect_equal(actual_rows, 148)
  
  actual_cols <- colnames(qcVisitsByDate())
  expected_cols <- c("Date", "WY2021", "WY2020", "WY2019", "WY2018", "WY2017", "WY2016")
  expect_equal(actual_cols, expected_cols)
  
  actual_character <- qcVisitsByDate()
  expect_equal(class(actual_character$Date), "character")
  expect_equal(class(actual_character$WY2021), "character")
  expect_equal(class(actual_character$WY2019), "character")
  
  actual_values <- qcVisitsByDate() %>% dplyr::filter(Date == "Nov 1")
  expected_values <- tibble::as_tibble_row(c(Date = "Nov 1",
                                             WY2021 = "DEVA_P_JAC0189",
                                             WY2020 = NA_character_,
                                             WY2019 = NA_character_,
                                             WY2018 = NA_character_,
                                             WY2017 = "LAKE_P_HOR0042, LAKE_P_SUG0011",
                                             WY2016 = NA_character_))
  expect_equal(actual_values, expected_values)
  
})


test_that("qcNotSampled returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcNotSampled())
  expect_equal(actual_rows, 35)
  
  actual_cols <- colnames(qcNotSampled())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "SampleFrame", "VisitType", "MonitoringStatus", "Notes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcNotSampled()
  expect_equal(class(actual_date$VisitDate), "Date")
  
})


test_that("qcRepeatVisits returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcRepeatVisits())
  expect_equal(actual_rows, 8)
  
  actual_cols <- colnames(qcRepeatVisits())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "SampleFrame", "VisitType", "MonitoringStatus", "Notes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcRepeatVisits()
  expect_equal(class(actual_date$VisitDate), "Date")
  
})