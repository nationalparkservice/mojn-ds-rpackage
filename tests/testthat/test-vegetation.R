context("Vegetation")

test_that("qcVegPresentNoLifeforms returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcVegPresentNoLifeforms(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expect_equal(actual_rows, 0)
  
  actual_cols <- colnames(qcVegPresentNoLifeforms(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "IsVegetationObserved", "LifeForm")
  expect_equal(actual_cols, expected_cols)
  
})


test_that("qcNoVegLifeformsPresent returns correct number of rows and columns", {
 
  actual_rows <- nrow(qcNoVegLifeformsPresent(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expect_equal(actual_rows, 0)
  
  actual_cols <- colnames(qcNoVegLifeformsPresent(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "IsVegetationObserved", "LifeForm")
  expect_equal(actual_cols, expected_cols)
   
})


test_that("qcLifeformPresentNoRank works as expected", {

  actual_rows <- nrow(qcLifeformPresentNoRank(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expect_equal(actual_rows, 0)
  
  actual_cols <- colnames(qcLifeformPresentNoRank(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "LifeForm", "Rank")
  expect_equal(actual_cols, expected_cols)
  
  actual_int <- qcLifeformPresentNoRank(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local")
  expect_equal(class(actual_int$Rank), "integer")

})


test_that("qcLifeformRankCheck works as expected", {

  actual_rows <- nrow(qcLifeformRankCheck(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expect_equal(actual_rows, 29)
  
  actual_cols <- colnames(qcLifeformRankCheck(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "Rank", "Count", "Diff", "LifeForms")
  expect_equal(actual_cols, expected_cols)
  
  actual_int <- qcLifeformRankCheck(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local")
  expect_equal(class(actual_int$Rank), "integer")
  expect_equal(class(actual_int$Count), "integer")
  expect_equal(class(actual_int$Diff), "integer")
  
  actual_lifeforms <- qcLifeformRankCheck(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local") %>% dplyr::filter(SiteCode == "DEVA_P_LIT0005", FieldSeason == "2021", Rank == 6) %>% dplyr::select(LifeForms)
  expected_lifeforms <- tibble::as_tibble(as.character("Sedge, Bryophyte")) %>% dplyr::rename(LifeForms = value)
  expect_equal(actual_lifeforms, expected_lifeforms)
   
})


test_that("LifeformsPresence works as expected", {

  actual_rows <- nrow(LifeformsPresence(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expect_equal(actual_rows, 267)
  
  actual_cols <- colnames(LifeformsPresence(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_cols <- c("Park", "FieldSeason", "LifeForm", "Observations")
  expect_equal(actual_cols, expected_cols)
  
  actual_int <- LifeformsPresence(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local")
  expect_equal(class(actual_int$Observations), "integer")
  
  actual_observations <- LifeformsPresence(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local") %>% dplyr::filter(Park == "PARA", FieldSeason == "2020", LifeForm == "Bryophyte") %>% dplyr::select(Observations)
  expected_observations <- tibble::as_tibble(as.integer(6)) %>% dplyr::rename(Observations = value)
  expect_equal(actual_observations, expected_observations)
    
})


test_that("InvasivePlants works as expected", {
  
  actual_rows <- nrow(InvasivePlants(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expect_equal(actual_rows, 125)
  
  actual_cols <- colnames(InvasivePlants(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local"))
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "USDAPlantsCode", "ScientificName", "InRiparianVegBuffer", "Notes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- InvasivePlants(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local")
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_code <- InvasivePlants(path.to.data = here::here("tests", "testthat", "test_data"), data.source = "local") %>% dplyr::filter(SiteCode == "JOTR_P_COT0294", FieldSeason == "2020") %>% dplyr::select(USDAPlantsCode)
  expected_code <- tibble::as_tibble(as.character(c("PHDA4", "TARA"))) %>% dplyr::rename(USDAPlantsCode = value)
  expect_equal(actual_code, expected_code)
  
})