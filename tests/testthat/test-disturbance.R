context("Disturbance")
LoadDesertSprings(dir)


test_that("qcDisturbanceFormatted works as expected", {
  
  actual_rows <- nrow(qcDisturbanceFormatted(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 809)
  
  actual_cols <- colnames(qcDisturbanceFormatted())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "SampleFrame", "Panel", "Roads", "HumanUse", "PlantManagement", "HikingTrails", "Livestock", "OtherAnthropogenic", "Fire", "Flooding", "Wildlife", "OtherNatural", "Overall", "FlowModificationStatus", "VisitType", "Notes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcDisturbanceFormatted()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_values <- qcDisturbanceFormatted() %>% dplyr::filter(SiteCode == "JOTR_P_BUZ0084", FieldSeason == "2017") %>% dplyr::select(SiteCode, FieldSeason, Roads, HumanUse, Flooding, Overall, FlowModificationStatus)
  expected_values <- tibble::as_tibble_row(c(SiteCode = "JOTR_P_BUZ0084",
                                             FieldSeason = "2017",
                                             Roads = "0",
                                             HumanUse = "NoData",
                                             Flooding = "1",
                                             Overall = "1",
                                             FlowModificationStatus = "None"))
  expect_equal(actual_values, expected_values)
  
})


test_that("qcFlowModFormatted works as expected", {
  
  actual_rows <- nrow(qcFlowModFormatted(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 958)
  
  actual_cols <- colnames(qcFlowModFormatted())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "SampleFrame", "Panel", "FlowModificationStatus", "ModificationType", "VisitType")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcFlowModFormatted()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_values <- qcFlowModFormatted() %>% dplyr::filter(SiteCode == "LAKE_P_SAU0022", FieldSeason == "2019") %>% dplyr::select(SiteCode, FieldSeason, FlowModificationStatus, ModificationType)
  expected_values <- tibble::as_tibble(data.frame(SiteCode = c("LAKE_P_SAU0022", "LAKE_P_SAU0022", "LAKE_P_SAU0022"),
                                                  FieldSeason = c("2019", "2019", "2019"),
                                                  FlowModificationStatus = c("Yes - One or more active", "Yes - One or more active", "Yes - One or more active"),
                                                  ModificationType = c("Pipe", "Dam", "Excavation")))
  expect_equal(actual_values, expected_values)
  
})


test_that("qcOverallDisturbance returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcOverallDisturbance(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 3)
  
  actual_cols <- colnames(qcOverallDisturbance())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "Roads", "HumanUse", "PlantManagement", "HikingTrails", "Livestock", "OtherAnthropogenic", "Fire", "Flooding", "Wildlife", "OtherNatural", "Overall")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcOverallDisturbance()
  expect_equal(class(actual_date$VisitDate), "Date")
  
})


test_that("qcFlowModNoHumanUse returns correct number of rows and columns", {
  
  actual_rows <- nrow(qcFlowModNoHumanUse(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 34)
  
  actual_cols <- colnames(qcFlowModNoHumanUse())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "HumanUse", "FlowModificationStatus")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- qcFlowModNoHumanUse()
  expect_equal(class(actual_date$VisitDate), "Date")
  
})


test_that("FlowModStatus works as expected", {
  
  actual_rows <- nrow(FlowModStatus(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 283)
  
  actual_cols <- colnames(FlowModStatus())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "FlowModificationStatus", "FlowModificationTypes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- FlowModStatus()
  expect_equal(class(actual_date$VisitDate), "Date")
  
  actual_flowmods <- FlowModStatus() %>% dplyr::filter(SiteCode == "MOJA_P_DOV0090", FieldSeason == "2019") %>% dplyr::select(FlowModificationStatus, FlowModificationTypes)
  expected_flowmods <- tibble::as_tibble_row(c(FlowModificationStatus = as.character("Yes - One or more active"), FlowModificationTypes = as.character("Pipe, Qanat")))
  expect_equal(actual_flowmods, expected_flowmods)
  
})


test_that("qcFlowModDiscrepancies works as expected", {
  
  actual_rows <- nrow(qcFlowModDiscrepancies(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 95)
  
  actual_cols <- colnames(qcFlowModDiscrepancies())
  expected_cols <- c("Park", "SiteCode", "SiteName", "FlowModificationStatus", "FieldSeasons")
  expect_equal(actual_cols, expected_cols)
  
  actual_fs <- qcFlowModDiscrepancies() %>% dplyr::filter(SiteCode == "MOJA_P_TAL0187", FlowModificationStatus == "None") %>% dplyr::select(FlowModificationStatus, FieldSeasons)
  expected_fs <- tibble::as_tibble_row(c(FlowModificationStatus = as.character("None"), FieldSeasons = as.character("2016, 2019, 2020, 2021")))
  expect_equal(actual_fs, expected_fs)
  
})


test_that("qcFlowModTypes works as expected", {
  
  actual_rows <- nrow(qcFlowModTypes(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 2)
  
  actual_cols <- colnames(qcFlowModTypes())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "FlowModificationStatus", "ModificationType")
  expect_equal(actual_cols, expected_cols)
  
  actual_type <- qcFlowModTypes() %>% dplyr::filter(SiteCode == "MOJA_P_TAL0187", FieldSeason == "2016") %>% dplyr::select(FlowModificationStatus, ModificationType)
  expected_type <- tibble::as_tibble_row(c(FlowModificationStatus = as.character("None"), ModificationType = as.character("Dam")))
  expect_equal(actual_type, expected_type)
  
})


test_that("FlowModCount works as expected", {
  
  actual_rows <- nrow(FlowModCount(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 17)
  
  actual_cols <- colnames(FlowModCount())
  expected_cols <- c("Park", "FlowModificationStatus", "Count", "Percent")
  expect_equal(actual_cols, expected_cols)
  
  actual_int <- FlowModCount()
  expect_equal(class(actual_int$Count), "integer")
  
  actual_dbl <- FlowModCount()
  expect_equal(typeof(actual_dbl$Percent), "double")
  
  actual_count <- FlowModCount() %>% dplyr::filter(Park == "MOJA", FlowModificationStatus == "Yes - All inactive") %>% dplyr::select(Count)
  expected_count <- tibble::as_tibble(as.integer(8)) %>% dplyr::rename(Count = value)
  expect_equal(actual_count, expected_count)
  
  actual_percent <- FlowModCount() %>% dplyr::filter(Park == "MOJA", FlowModificationStatus == "Yes - All inactive") %>% dplyr::select(Percent)
  expected_percent <- tibble::as_tibble(as.double(9*100/45)) %>% dplyr::rename(Percent = value)
  expect_equal(actual_count, expected_count)
  
})


test_that("DisturbanceCount works as expected", {
  
  actual_rows <- nrow(DisturbanceCount(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 6)
  
  actual_cols <- colnames(DisturbanceCount())
  expected_cols <- c("Park", "LivestockCount", "HumanUseCount", "LivestockPercent", "HumanUsePercent")
  expect_equal(actual_cols, expected_cols)
  
  actual_int <- DisturbanceCount()
  expect_equal(class(actual_int$LivestockCount), "integer")
  expect_equal(class(actual_int$HumanUseCount), "integer")
  
  actual_dbl <- DisturbanceCount()
  expect_equal(typeof(actual_dbl$LivestockPercent), "double")
  expect_equal(typeof(actual_dbl$HumanUsePercent), "double")
  
  actual_count <- DisturbanceCount() %>% dplyr::filter(Park == "MOJA") %>% dplyr::select(LivestockCount, HumanUseCount)
  expected_count <- tibble::as_tibble_row(c(LivestockCount = as.integer(24), HumanUseCount = as.integer(33)))
  expect_equal(actual_count, expected_count)
  
  actual_percent <- DisturbanceCount() %>% dplyr::filter(Park == "MOJA") %>% dplyr::select(LivestockPercent, HumanUsePercent)
  expected_percent <- tibble::as_tibble_row(c(LivestockPercent = round(as.double(24*100/45), 1), HumanUsePercent = round(as.double(33*100/45), 1)))
  expect_equal(actual_percent, expected_percent)
  
})


test_that("HumanUseObservations returns correct number of rows and columns", {
  
  actual_rows <- nrow(HumanUseObservations(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 356)
  
  actual_cols <- colnames(HumanUseObservations())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "HumanUse", "Notes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- HumanUseObservations()
  expect_equal(class(actual_date$VisitDate), "Date")
  
})


test_that("LivestockObservations returns correct number of rows and columns", {
  
  actual_rows <- nrow(LivestockObservations(field.season = c("2016", "2017", "2018", "2019", "2020", "2021", "2022")))
  expect_equal(actual_rows, 145)
  
  actual_cols <- colnames(LivestockObservations())
  expected_cols <- c("Park", "SiteCode", "SiteName", "VisitDate", "FieldSeason", "Livestock", "Notes")
  expect_equal(actual_cols, expected_cols)
  
  actual_date <- LivestockObservations()
  expect_equal(class(actual_date$VisitDate), "Date")
  
})