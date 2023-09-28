context("Database connection")

skip_if_not(file.exists('M:/MONITORING/DS_Water/Data/Database/ConnectFromR/ds-database-conn.csv'), message = "Skipped - no VPN connection")

test_that("Connection to MOJN DS database is successful", {
  conn <- OpenDatabaseConnection()
  result <- pool::dbGetQuery(conn, "SELECT TOP 1 Park FROM analysis.Site WHERE Park = 'DEVA'")
  CloseDatabaseConnection(conn)

  expect_equal(result$Park, "DEVA")
})
