context("Wildlife")
LoadDesertSprings(here::here("tests", "testthat", "test_data"))
test_that("qcWildlifeObservedNoTypes returns expected number of rows and columns", {

  actual_rows <- nrow(qcWildlifeObservedNoTypes())
  expect_equal(actual_rows, 2)
  
  actual_cols <- colnames(qcWildlifeObservedNoTypes())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "IsWildlifeObserved", "WildlifeType", "DirectObservation", "Scat", "Tracks", "Shelter", "Foraging", "Vocalization", "OtherEvidence", "Notes")
  expect_equal(actual_cols, expected_cols)
    
})


test_that("qcWildlifeObservedNoEvidence returns expected number of rows and columns", {

  actual_rows <- nrow(qcWildlifeObservedNoEvidence())
  expect_equal(actual_rows, 3)
  
  actual_cols <- colnames(qcWildlifeObservedNoEvidence())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "IsWildlifeObserved", "WildlifeType", "DirectObservation", "Scat", "Tracks", "Shelter", "Foraging", "Vocalization", "OtherEvidence", "Notes")
  expect_equal(actual_cols, expected_cols)
    
})


test_that("UngulatesEvidence returns expected number of rows and columns", {

  actual_rows <- nrow(UngulatesEvidence() %>% dplyr::filter(FieldSeason == "2020"))
  expect_equal(actual_rows, 37)

  actual_cols <- colnames(UngulatesEvidence())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "IsWildlifeObserved", "WildlifeType", "DirectObservation", "Scat", "Tracks", "Shelter", "Foraging", "Vocalization", "OtherEvidence", "Notes")
  expect_equal(actual_cols, expected_cols)
  
  actual_type <- as.character(unique(UngulatesEvidence() %>% dplyr::select(WildlifeType)))
  expect_equal(actual_type, "Ungulate")
  
  actual_factor <- UngulatesEvidence()
  expect_false(any(sapply(actual_factor, class) == "factor"))
  
  actual_date <- UngulatesEvidence()
  expect_equal(class(actual_date$VisitDate), "Date")
    
})