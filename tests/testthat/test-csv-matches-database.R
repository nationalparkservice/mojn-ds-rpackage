context("Reading from database and csv")

data.names <- names(GetColSpec())

for (d.name in data.names) {
  test_that(paste0(d.name, ".csv matches data read from database"), {
    db <- ReadAndFilterData(data.name = d.name)
    LoadDesertSprings(dir)
    csv <- ReadAndFilterData(data.name = d.name)
    
    expect_dataframe_equal(db, csv)
  })
}