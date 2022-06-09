test_that("qcWildlifeObservedNoTypes returns expected number of rows and columns", {

  actual_rows <- nrow(qcWildlifeObservedNoTypes(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_rows <- 2
  
  expect_equal(actual_rows, expected_rows)
  
  actual_cols <- colnames(qcWildlifeObservedNoTypes(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "IsWildlifeObserved", "WildlifeType", "DirectObservation", "Scat", "Tracks", "Shelter", "Foraging", "Vocalization", "OtherEvidence", "Notes")
  
  expect_equal(actual_cols, expected_cols)
    
})


test_that("qcWildlifeObservedNoEvidence returns expected number of rows and columns", {

  actual_rows <- nrow(qcWildlifeObservedNoEvidence(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_rows <- 3
  
  expect_equal(actual_rows, expected_rows)
  
  actual_cols <- colnames(qcWildlifeObservedNoEvidence(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "IsWildlifeObserved", "WildlifeType", "DirectObservation", "Scat", "Tracks", "Shelter", "Foraging", "Vocalization", "OtherEvidence", "Notes")
  
  expect_equal(actual_cols, expected_cols)
  
    
})


test_that("UngulatesEvidence returns expected number of rows and columns", {

  actual_rows <- nrow(UngulatesEvidence(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local") %>% dplyr::filter(FieldSeason == "2020"))
  expected_rows <- 37

  expect_equal(actual_rows, expected_rows)

  actual_cols <- colnames(UngulatesEvidence(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "IsWildlifeObserved", "WildlifeType", "DirectObservation", "Scat", "Tracks", "Shelter", "Foraging", "Vocalization", "OtherEvidence", "Notes")

  expect_equal(actual_cols, expected_cols)
  
  actual_type <- as.character(unique(UngulatesEvidence(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local") %>% dplyr::select(WildlifeType)))
  expected_type <- "Ungulate"
  
  expect_equal(actual_type, expected_type)
    
})