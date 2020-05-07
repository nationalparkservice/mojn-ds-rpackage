context("Utility functions")

test_that("GetSiteName retrieves the correct spring name for the spring code provided", {
  conn <- OpenDatabaseConnection()

  expect_equal(GetSiteName(conn, site.code = "LAKE_P_HOR0042"), "Horsethief Canyon")
  expect_equal(GetSiteName(conn, site.code = "JOTR_P_COT0294"), "Cottonwood Palm Spring")
  expect_error(GetSiteName(conn, site.code = "asdf"), "Data are not available for the site specified")

  CloseDatabaseConnection(conn)
})
