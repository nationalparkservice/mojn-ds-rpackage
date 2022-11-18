context("Utility functions")
LoadDesertSprings(here::here("tests", "testthat", "test_data"))
test_that("GetSiteName retrieves the correct spring name for the spring code provided", {
  
  expect_equal(GetSiteName(site.code = "LAKE_P_HOR0042"), "Horsethief Canyon")
  expect_equal(GetSiteName(site.code = "JOTR_P_COT0294"), "Cottonwood Palm Spring")
  expect_warning(GetSiteName(site.code = "asdf"), "Site: Data are not available for the site specified")


})
