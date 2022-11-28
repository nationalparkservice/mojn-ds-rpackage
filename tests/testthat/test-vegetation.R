context("Vegetation")
LoadDesertSprings(here::here("tests", "testthat", "test_data"))
test_that("qcVegPresentNoLifeforms returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcVegPresentNoLifeforms())
  expect_equal(actual_rows, 0)
  
  actual_cols <- colnames(qcVegPresentNoLifeforms())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "IsVegetationObserved", "LifeForm")
  expect_equal(actual_cols, expected_cols)
  
})


test_that("qcNoVegLifeformsPresent returns correct number of rows and columns", {
 
  actual_rows <- nrow(qcNoVegLifeformsPresent())
  expect_equal(actual_rows, 0)
  
  actual_cols <- colnames(qcNoVegLifeformsPresent())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "IsVegetationObserved", "LifeForm")
  expect_equal(actual_cols, expected_cols)
   
})


test_that("qcLifeformPresentNoRank works as expected", {

  actual_rows <- nrow(qcLifeformPresentNoRank())
  expect_equal(actual_rows, 0)
  
  actual_cols <- colnames(qcLifeformPresentNoRank())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "LifeForm", "Rank")
  expect_equal(actual_cols, expected_cols)
  
  actual_int <- qcLifeformPresentNoRank()
  expect_equal(class(actual_int$Rank), "integer")

})


test_that("qcLifeformRankCheck works as expected", {

  actual_rows <- nrow(qcLifeformRankCheck())
  expect_equal(actual_rows, 29)
  
  actual_cols <- colnames(qcLifeformRankCheck())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "Rank", "Count", "Diff", "LifeForms")
  expect_equal(actual_cols, expected_cols)
  
  actual_int <- qcLifeformRankCheck()
  expect_equal(class(actual_int$Rank), "integer")
  expect_equal(class(actual_int$Count), "integer")
  expect_equal(class(actual_int$Diff), "integer")
  
  actual_lifeforms <- qcLifeformRankCheck() %>% dplyr::filter(SiteCode == "DEVA_P_LIT0005", FieldSeason == "2021", Rank == 6) %>% dplyr::select(LifeForms)
  expected_lifeforms <- tibble::as_tibble(as.character("Sedge, Bryophyte")) %>% dplyr::rename(LifeForms = value)
  expect_equal(actual_lifeforms, expected_lifeforms)
   
})


test_that("LifeformsPresence works as expected", {

  actual_rows <- nrow(LifeformsPresence())
  expect_equal(actual_rows, 267)
  
  actual_cols <- colnames(LifeformsPresence())
  expected_cols <- c("Park", "FieldSeason", "LifeForm", "Observations")
  expect_equal(actual_cols, expected_cols)
  
  actual_int <- LifeformsPresence()
  expect_equal(class(actual_int$Observations), "integer")
  
  actual_observations <- LifeformsPresence() %>% dplyr::filter(Park == "PARA", FieldSeason == "2020", LifeForm == "Bryophyte") %>% dplyr::select(Observations)
  expected_observations <- tibble::as_tibble(as.integer(6)) %>% dplyr::rename(Observations = value)
  expect_equal(actual_observations, expected_observations)
    
})


test_that("InvasivePlants works as expected", {
  
  actual_rows <- nrow(InvasivePlants())
  expect_equal(actual_rows, 142)
  
  actual_cols <- colnames(InvasivePlants())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "USDAPlantsCode", "ScientificName", "InRiparianVegBuffer", "Notes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- InvasivePlants()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_code <- InvasivePlants() %>% dplyr::filter(SiteCode == "JOTR_P_COT0294", FieldSeason == "2020") %>% dplyr::select(USDAPlantsCode)
  expected_code <- tibble::as_tibble(as.character(c("PHDA4", "TARA"))) %>% dplyr::rename(USDAPlantsCode = value)
  expect_equal(actual_code, expected_code)
  
})