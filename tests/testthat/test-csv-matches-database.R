context("Reading from database and csv")

# Write temporary csv files
LoadDesertSprings()
dir <- here::here("tests", "testthat", "temp-test-csv")
SaveDataToCsv(dir, create.folders = TRUE, overwrite = TRUE)


data.names <- names(GetColSpec())

for (d.name in data.names) {
  test_that(paste0(d.name, ".csv matches data read from database"), {
    LoadDesertSprings()
    db <- ReadAndFilterData(data.name = d.name)
    LoadDesertSprings(here::here("tests", "testthat", "temp-test-csv"))
    csv <- ReadAndFilterData(data.name = d.name)
    
    expect_dataframe_equal(db, csv)
  })
}

# Remove temporary csv's
unlink(dir, recursive = TRUE)
