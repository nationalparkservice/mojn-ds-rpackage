context("Utility functions")

test_that("GetSiteName retrieves the correct spring name for the spring code provided", {
  skip_if_not(dir.exists('M:/MONITORING/DS_Water/Data/Database/ConnectFromR/ds-database-conn.csv'), message = "Skipped - no VPN connection")
  
  conn <- OpenDatabaseConnection()

  expect_equal(GetSiteName(conn, site.code = "LAKE_P_HOR0042"), "Horsethief Canyon")
  expect_equal(GetSiteName(conn, site.code = "JOTR_P_COT0294"), "Cottonwood Palm Spring")
  expect_warning(GetSiteName(conn, site.code = "asdf"), "Site: Data are not available for the site specified")

  CloseDatabaseConnection(conn)
})
