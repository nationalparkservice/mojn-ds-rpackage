
FullSpringDischarge <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  
name <- "C:/Users/jbailard/Documents/R/desertsprings/inventorydata/qSumB_SpringInventoryEvents_20220216.csv"
name2 <- "C:/Users/jbailard/Documents/R/desertsprings/inventorydata/DSVisitRawData.csv"

mon <- SpringDischarge(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
inv <- readr::read_csv(name, show_col_types = FALSE)
sites <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "Site")
visit <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "Visit")
wy22 <- readr::read_csv(name2, show_col_types = FALSE)

coords <- sites %>%
  dplyr::select(SiteCode, Lat_WGS84, Lon_WGS84, X_UTM_NAD83_11N, Y_UTM_NAD83_11N)

type <- visit %>%
  dplyr::select(SiteCode, VisitDate, SpringType)

inv.app <- sites %>%
  dplyr::select(SiteCode, SampleFrame)

inv.tidy <- inv %>%
  dplyr::mutate(VolDischarge_L_per_s = Discharge_liters_per_minute / 60) %>%
  dplyr::mutate(SpringType = SpringHabitat_Label) %>%
  dplyr::rename("Park" = "Unit_Code",
                "SiteCode" = "MOJN_SpringCode",
                "SiteName" = "LocationName",
                "VisitDate" = "EventDate",
                "DischargeClass_L_per_s" = "DischargeClass_Description",
                "SpringbrookWidth_m" = "WaterWidth_m",
                "VisitType" = "VisitType_Label",
                "FlowCondition" = "SpringHabitat_Label") %>%
  dplyr::mutate(FieldSeason = NA,
                SpringbrookType = NA,
                SpringbrookLengthFlag = NA,
                DiscontinuousSpringbrookLengthFlag = NA,
                DiscontinuousSpringbrookLength_m = NA) %>%
  dplyr::left_join(inv.app, by = "SiteCode") %>%
  dplyr::relocate(SampleFrame, .after = "FieldSeason") %>%
  dplyr::select(Park,
                SiteCode,
                SiteName,
                VisitDate,
                FieldSeason,
                SampleFrame,
                VisitType,
                SpringType,
                FlowCondition,
                VolDischarge_L_per_s,
                DischargeClass_L_per_s,
                SpringbrookType,
                SpringbrookLengthFlag,
                SpringbrookLength_m,
                SpringbrookWidth_m,
                DiscontinuousSpringbrookLengthFlag,
                DiscontinuousSpringbrookLength_m,
                Notes,
                ElevationDEM_m) %>%
  dplyr::mutate(VisitDate = as.Date(VisitDate, format = "%m/%d/%Y")) %>%
  dplyr::mutate(SpringbrookLength_m = dplyr::case_when(SiteCode == "MOJA_P_WHI0220" ~ 3,
                                                       SiteCode == "MOJA_P_GOL0111" ~ 2,
                                                       SiteCode == "MOJA_P_OAK0157" ~ 1,
                                                       TRUE ~ SpringbrookLength_m)) %>%
  dplyr::mutate(SpringbrookWidth_m = dplyr::case_when(SiteCode == "MOJA_P_WHI0220" ~ 0,
                                                      SiteCode == "MOJA_P_GOL0111" ~ 0,
                                                      SiteCode == "MOJA_P_OAK0157" ~ as.double(NA_integer_),
                                                      TRUE ~ SpringbrookWidth_m)) %>%
  dplyr::mutate(FlowCondition = dplyr::case_when(FlowCondition == "Dry" ~ "dry",
                                                 SiteCode == "PARA_P_LAV0006" ~ "dry",
                                                 SiteCode == "PARA_P_ORS0041" ~ "dry",
                                                 SiteCode == "PARA_P_EDS0199" ~ "dry",
                                                 SiteCode == "MOJA_P_COT0063" ~ "dry",
                                                 SiteCode == "MOJA_P_GOL0107" ~ "dry",
                                                 SiteCode == "MOJA_P_RUF0171" ~ "dry",
                                                 SiteCode == "MOJA_P_SIL0178" ~ "dry",
                                                 SiteCode == "MOJA_P_TWI0190" ~ "dry",
                                                 SiteCode == "CAMO_P_QUA0302" ~ "dry",
                                                 SiteCode == "MOJA_P_UNN0030" ~ NA_character_,
                                                 SiteCode == "DEVA_P_NOG0740" ~ NA_character_,
                                                 TRUE ~ "standing water")) %>%
  dplyr::mutate(SpringType = dplyr::case_when(SpringType == "Dry" ~ NA_character_,
                                              SpringType == "[No Data]" ~ NA_character_,
                                              TRUE ~ as.character(SpringType))) %>%
  dplyr::mutate(VisitType = dplyr::case_when(VisitType == "First Visit" ~ "Primary",
                                             VisitType == "Revisit" ~ "Replicate",
                                             VisitType == "No Visit" ~ NA_character_,
                                             TRUE ~ "Primary")) %>%
  dplyr::mutate(DischargeClass_L_per_s = dplyr::case_when(DischargeClass_L_per_s == "<= 1 L/sec" ~ "<1.0 L/s",
                                                          DischargeClass_L_per_s == "1.1 L/sec to 10 L/sec" ~ "1.0 - <10 L/s",
                                                          DischargeClass_L_per_s == ">= 10 L/sec" ~ ">10 L/s",
                                                          TRUE ~ NA_character_)) %>%
  dplyr::filter(!(SiteCode == "LAKE_P_FER0090"))


mon.app <- inv.tidy %>%
  dplyr::select(SiteCode, ElevationDEM_m)

mon.tidy <- mon %>%
  dplyr::left_join(type, by = c("SiteCode", "VisitDate")) %>%
  dplyr::relocate(SpringType, .after = "VisitType") %>%
  dplyr::left_join(mon.app, by = "SiteCode") %>%
  dplyr::select(-DPL) %>%
  dplyr::filter(!(Park == "CAMO" & FieldSeason == "2017"))

wy22.app <- mon.tidy %>%
  dplyr::select(SiteCode, SiteName, SampleFrame, ElevationDEM_m)

new_row <- c("LAKE_P_PUP0007", "Pupfish Hot Spring", "3Yr", 252)
new_row2 <- c("LAKE_P_ARI0003", "Arizona Seep", "3Yr", 238)

wy22.app %<>%
  rbind(new_row) %>%
  rbind(new_row2) %>%
  dplyr::mutate(ElevationDEM_m = as.numeric(ElevationDEM_m))
  
wy22.tidy <- wy22 %>%
  dplyr::select(Park, SiteCode, DateTime, VisitType, SpringType, FlowCondition, VolDischarge_L_per_s, DischargeClass_L_per_s, SpringbrookType, SpringbrookLengthFlag, SpringbrookLength_m, SpringbrookWidth_m, DiscontinuousSpringbrookLengthFlag, DiscontinuousSpringbrookLength_m, Notes) %>%
  dplyr::left_join(wy22.app, by = "SiteCode") %>%
  dplyr::relocate(SiteName, .after = "SiteCode") %>%
  dplyr::relocate(SampleFrame, .before = "VisitType") %>%
  dplyr::mutate(Park = dplyr::case_when(Park == "Lake Mead NRA" ~ "LAKE",
                                        Park == "Joshua Tree NP" ~ "JOTR",
                                        Park == "Death Valley NP" ~ "DEVA",
                                        Park == "Mojave NPRES" ~ "MOJA",
                                        Park == "Parashant NM" ~ "PARA",
                                        Park == "Castle Mountains NM" ~ "CAMO",
                                        TRUE ~ NA_character_)) %>%
  dplyr::mutate(VisitDate = sub(" .*", "", DateTime)) %>%
  dplyr::mutate(VisitDate = as.Date(VisitDate, "%m/%d/%Y")) %>%
  dplyr::relocate(VisitDate, .after = SiteName) %>%
  dplyr::select(-DateTime) %>%
  dplyr::filter(VisitDate > "2021-10-01") %>%
  dplyr::mutate(FieldSeason = "2022") %>%
  dplyr::relocate(FieldSeason, .after = "VisitDate") %>%
  dplyr::mutate(SpringbrookLength_m = as.numeric(SpringbrookLength_m),
                VolDischarge_L_per_s = as.numeric(VolDischarge_L_per_s),
                SpringbrookType = as.character(SpringbrookType),
                DiscontinuousSpringbrookLengthFlag = as.character(DiscontinuousSpringbrookLengthFlag),
                DiscontinuousSpringbrookLength_m = as.numeric(DiscontinuousSpringbrookLength_m))
  
full <- rbind(mon.tidy, inv.tidy, wy22.tidy) %>%
  dplyr::mutate(ElevationDEM_ft = ElevationDEM_m * 3.2808) %>%
  dplyr::filter(VisitType == "Primary",
                SampleFrame %in% c("Annual", "3Yr", "Over")) %>%
  dplyr::mutate(SpringbrookLength_m = dplyr::case_when(is.na(SpringbrookLength_m) & FlowCondition == "dry" ~ 0,
                                                       TRUE ~ SpringbrookLength_m)) %>%
  dplyr::mutate(SpringbrookLengthFlag = dplyr::case_when(is.na(SpringbrookLengthFlag) & SpringbrookLength_m > 50 ~ ">50m",
                                                         is.na(SpringbrookLengthFlag) & SpringbrookLength_m <= 50 ~ "Measured",
                                                         TRUE ~ SpringbrookLengthFlag)) %>%
  dplyr::mutate(SpringbrookLength_m = dplyr::case_when(SpringbrookLengthFlag == ">50m" ~ as.double(NA_integer_),
                                                       TRUE ~ SpringbrookLength_m)) %>%
  dplyr::mutate(VolDischarge_L_per_s = round(VolDischarge_L_per_s, 3)) %>%
  dplyr::mutate(FieldSeason = dplyr::case_when(is.na(FieldSeason) & VisitDate >= lubridate::ymd("2004-10-01") & VisitDate <= lubridate::ymd("2005-09-30") ~ "2005",
                                               is.na(FieldSeason) & VisitDate >= lubridate::ymd("2005-10-01") & VisitDate <= lubridate::ymd("2006-09-30") ~ "2006",
                                               is.na(FieldSeason) & VisitDate >= lubridate::ymd("2006-10-01") & VisitDate <= lubridate::ymd("2007-09-30") ~ "2007",
                                               is.na(FieldSeason) & VisitDate >= lubridate::ymd("2016-10-01") & VisitDate <= lubridate::ymd("2017-09-30") ~ "2017",
                                               is.na(VisitDate) & Park == "MOJA" ~ "2006",
                                               TRUE ~ FieldSeason)) %>%
  unique()

return(full)

}



FullFlowCategoriesDiscontinuous <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  
  joined <- FullSpringDischarge(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  
  categorized <- joined %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::mutate(FlowCategory = dplyr::case_when(FlowCondition == "dry" ~ "Dry",
                                                  FlowCondition == "wet soil only" | (!(FlowCondition %in% c("dry", "wet soil only")) & (SpringbrookLength_m == 0 | SpringbrookWidth_m == 0)) ~ "Wet Soil",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLength_m > 0 & DiscontinuousSpringbrookLength_m < 10) | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLength_m > 0 & SpringbrookLength_m < 10) ~ "< 10 m",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLengthFlag == "Measured" & (DiscontinuousSpringbrookLength_m >= 10 & DiscontinuousSpringbrookLength_m <= 50)) | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLengthFlag == "Measured" & (SpringbrookLength_m >= 10 & SpringbrookLength_m <= 50)) ~ "10 - 50 m",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLengthFlag == ">50m") | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLengthFlag == ">50m") ~ "> 50 m",
                                                  TRUE ~ "NA")) %>%
    dplyr::mutate(FlowCategory = dplyr::case_when(SiteCode == "PARA_P_COY0069" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "JOTR_P_EAS0160" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "JOTR_P_PIN0216" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "JOTR_P_QUE0109" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "MOJA_P_WIL0222" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "MOJA_P_BUD0021" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "MOJA_P_BUD0021" & FieldSeason == "2016" ~ "< 10 m",
                                                  SiteCode == "MOJA_P_HOR0121" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "LAKE_P_WHI0030" & FieldSeason == "2007" ~ "< 10 m",
                                                  SiteCode == "LAKE_P_LOS0009" & FieldSeason == "2007" ~ "< 10 m",
                                                  SiteCode == "LAKE_P_NEV0035" & FieldSeason == "2007" ~ "< 10 m",
                                                  SiteCode == "LAKE_P_BLA0053" & FieldSeason == "2007" ~ "< 10 m",
                                                  SiteCode == "LAKE_P_ARI0003" & FieldSeason == "2007" ~ "< 10 m",
                                                  SiteCode == "DEVA_P_HOL0706" & FieldSeason == "2005" ~ "< 10 m",
                                                  SiteCode == "DEVA_P_EAS0201" & FieldSeason == "2005" ~ "< 10 m",
                                                  SiteCode == "DEVA_P_SED1050" & FieldSeason == "2005" ~ "< 10 m",
                                                  SiteCode == "DEVA_P_MES0218" & FieldSeason == "2005" ~ "10 - 50 m",
                                                  SiteCode == "DEVA_P_SAL0168" & FieldSeason == "2005" ~ "> 50 m",
                                                  SiteCode == "DEVA_P_HUN0746" & FieldSeason == "2005" ~ "> 50 m",
                                                  SiteCode == "DEVA_P_HUN0486" & FieldSeason == "2005" ~ "> 50 m",
                                                  SiteCode == "DEVA_P_FLY0494" & FieldSeason == "2005" ~ "> 50 m",
                                                  SiteCode == "DEVA_P_HAL0809" & FieldSeason == "2006" ~ "> 50 m",
                                                  TRUE ~ FlowCategory)) %>%
    dplyr::mutate(SampleFrame = dplyr::case_when(SiteCode == "MOJA_P_WHI0220" ~ "Over",
                                                SiteCode == "MOJA_P_CUT0081" ~ "Over",
                                              # SiteCode == "LAKE_P_ARI0003" ~ "Inactive",
                                              # SiteCode == "LAKE_P_NEV0035" ~ "Inactive",
                                                SiteCode == "JOTR_P_BLA0045" ~ "Inactive",
                                                TRUE ~ SampleFrame)) %>%
    dplyr::select(Park, SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, FlowCondition, FlowCategory) %>%
    dplyr::group_by(Park, FieldSeason, SampleFrame, FlowCategory) %>%
    dplyr::summarize(Count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(SampleFrame %in% c("Annual", "3Yr")) %>%
    dplyr::arrange(Park, FieldSeason, SampleFrame, FlowCategory)

  
  return(categorized)
}

FullFlowCategoriesAnnualPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  data <- FullFlowCategoriesDiscontinuous(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  
  data$FlowCategory <- factor(data$FlowCategory, levels = c("> 50 m", "10 - 50 m", "< 10 m", "Wet Soil", "Dry"))
  
  data %<>% filter(!is.na(FlowCategory)) %>%
    dplyr::mutate(FieldSeason = dplyr::case_when(FieldSeason %in% c("2005", "2006", "2007") ~ "2000s",
                                                 TRUE ~ FieldSeason))
  
  plot <- ggplot2::ggplot(data %>% filter(SampleFrame == "Annual"), aes(x = FieldSeason, y = Count, fill = FlowCategory)) +
    geom_bar(stat = "identity") +
    facet_grid(~Park) +
    scale_fill_manual(values = c("Dry" = "red",
                                 "Wet Soil" = "gold",
                                 "< 10 m" = "lightskyblue",
                                 "10 - 50 m" = "royalblue1",
                                 "> 50 m" = "navy")) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 90)) +
    labs(x = "Field Season",
         y = "Number of Springs", 
         fill = "Flow Category")
  
  return(plot)
}


FullFlowCategoriesThreeYearPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  data <- FullFlowCategoriesDiscontinuous(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  
  data$FlowCategory <- factor(data$FlowCategory, levels = c("> 50 m", "10 - 50 m", "< 10 m", "Wet Soil", "Dry"))
  
  data %<>% filter(!is.na(FlowCategory)) %>%
    dplyr::mutate(FieldSeason = dplyr::case_when(FieldSeason %in% c("2005", "2006", "2007") ~ "2000s",
                                                 TRUE ~ FieldSeason))
  
  plot <- ggplot2::ggplot(data %>% filter(SampleFrame == "3Yr"), aes(x = FieldSeason, y = Count, fill = FlowCategory)) +
    geom_bar(stat = "identity") +
    facet_grid(~Park, scales = "free", space = "free_x") +
    scale_fill_manual(values = c("Dry" = "red",
                                 "Wet Soil" = "gold",
                                 "< 10 m" = "lightskyblue",
                                 "10 - 50 m" = "royalblue1",
                                 "> 50 m" = "navy")) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 90)) +
    labs(x = "Field Season",
         y = "Number of Springs", 
         fill = "Flow Category")
  
  return(plot)
}

FullFlowCategoriesAnnualHeatMap <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  joined <- FullSpringDischarge(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  
  data <- joined %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::mutate(FlowCategory = dplyr::case_when(FlowCondition == "dry" ~ "Dry",
                                                  FlowCondition == "wet soil only" | (!(FlowCondition %in% c("dry", "wet soil only")) & (SpringbrookLength_m == 0 | SpringbrookWidth_m == 0)) ~ "Wet Soil",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLength_m > 0 & DiscontinuousSpringbrookLength_m < 10) | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLength_m > 0 & SpringbrookLength_m < 10) ~ "< 10 m",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLengthFlag == "Measured" & (DiscontinuousSpringbrookLength_m >= 10 & DiscontinuousSpringbrookLength_m <= 50)) | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLengthFlag == "Measured" & (SpringbrookLength_m >= 10 & SpringbrookLength_m <= 50)) ~ "10 - 50 m",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLengthFlag == ">50m") | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLengthFlag == ">50m") ~ "> 50 m",
                                                  TRUE ~ "NA")) %>%
    dplyr::mutate(FlowCategory = dplyr::case_when(SiteCode == "PARA_P_COY0069" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "JOTR_P_EAS0160" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "JOTR_P_PIN0216" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "JOTR_P_QUE0109" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "MOJA_P_WIL0222" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "MOJA_P_BUD0021" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "MOJA_P_BUD0021" & FieldSeason == "2016" ~ "< 10 m",
                                                  SiteCode == "MOJA_P_HOR0121" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "LAKE_P_WHI0030" & FieldSeason == "2007" ~ "< 10 m",
                                                  SiteCode == "LAKE_P_LOS0009" & FieldSeason == "2007" ~ "< 10 m",
                                                  SiteCode == "LAKE_P_NEV0035" & FieldSeason == "2007" ~ "< 10 m",
                                                  SiteCode == "LAKE_P_BLA0053" & FieldSeason == "2007" ~ "< 10 m",
                                                  SiteCode == "LAKE_P_ARI0003" & FieldSeason == "2007" ~ "< 10 m",
                                                  SiteCode == "DEVA_P_HOL0706" & FieldSeason == "2005" ~ "< 10 m",
                                                  SiteCode == "DEVA_P_EAS0201" & FieldSeason == "2005" ~ "< 10 m",
                                                  SiteCode == "DEVA_P_SED1050" & FieldSeason == "2005" ~ "< 10 m",
                                                  SiteCode == "DEVA_P_MES0218" & FieldSeason == "2005" ~ "10 - 50 m",
                                                  SiteCode == "DEVA_P_SAL0168" & FieldSeason == "2005" ~ "> 50 m",
                                                  SiteCode == "DEVA_P_HUN0746" & FieldSeason == "2005" ~ "> 50 m",
                                                  SiteCode == "DEVA_P_HUN0486" & FieldSeason == "2005" ~ "> 50 m",
                                                  SiteCode == "DEVA_P_FLY0494" & FieldSeason == "2005" ~ "> 50 m",
                                                  SiteCode == "DEVA_P_HAL0809" & FieldSeason == "2006" ~ "> 50 m",
                                                  TRUE ~ FlowCategory))
  
  data$FlowCategory <- factor(data$FlowCategory, levels = c("> 50 m", "10 - 50 m", "< 10 m", "Wet Soil", "Dry"))
  
  data %<>% filter(!is.na(FlowCategory)) %>%
    dplyr::mutate(FieldSeason = dplyr::case_when(FieldSeason %in% c("2005", "2006", "2007") ~ "Inventory",
                                                 TRUE ~ FieldSeason))
  
  data$FieldSeason <- factor(data$FieldSeason, levels = c("Inventory", "2016", "2017", "2018", "2019", "2020", "2021", "2022"))
  
  heatmap <- ggplot2::ggplot(data %>% filter(SampleFrame == "Annual"), aes(x = FieldSeason, 
                                                                           y = reorder(SiteCode, desc(SiteCode)),
                                                                           fill = FlowCategory)) + 
    geom_tile(color = "white") + 
    scale_fill_manual(values = c("navy", "royalblue1", "lightskyblue", "gold", "red"), name = "Flow Category") +
    labs(x = "Field Season",
         y = "Annual Spring") +
    theme(legend.position = "bottom") +
    facet_grid(Park~., scales = "free", space = "free_y")
  
  return(heatmap)
}


FullFlowCategoriesThreeYearHeatMap <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  joined <- FullSpringDischarge(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  
  data <- joined %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::mutate(FlowCategory = dplyr::case_when(FlowCondition == "dry" ~ "Dry",
                                                  FlowCondition == "wet soil only" | (!(FlowCondition %in% c("dry", "wet soil only")) & (SpringbrookLength_m == 0 | SpringbrookWidth_m == 0)) ~ "Wet Soil",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLength_m > 0 & DiscontinuousSpringbrookLength_m < 10) | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLength_m > 0 & SpringbrookLength_m < 10) ~ "< 10 m",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLengthFlag == "Measured" & (DiscontinuousSpringbrookLength_m >= 10 & DiscontinuousSpringbrookLength_m <= 50)) | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLengthFlag == "Measured" & (SpringbrookLength_m >= 10 & SpringbrookLength_m <= 50)) ~ "10 - 50 m",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLengthFlag == ">50m") | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLengthFlag == ">50m") ~ "> 50 m",
                                                  TRUE ~ "NA")) %>%
    dplyr::mutate(FlowCategory = dplyr::case_when(SiteCode == "PARA_P_COY0069" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "JOTR_P_EAS0160" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "JOTR_P_PIN0216" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "JOTR_P_QUE0109" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "MOJA_P_WIL0222" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "MOJA_P_BUD0021" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "MOJA_P_BUD0021" & FieldSeason == "2016" ~ "< 10 m",
                                                  SiteCode == "MOJA_P_HOR0121" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "LAKE_P_WHI0030" & FieldSeason == "2007" ~ "< 10 m",
                                                  SiteCode == "LAKE_P_LOS0009" & FieldSeason == "2007" ~ "< 10 m",
                                                  SiteCode == "LAKE_P_NEV0035" & FieldSeason == "2007" ~ "< 10 m",
                                                  SiteCode == "LAKE_P_BLA0053" & FieldSeason == "2007" ~ "< 10 m",
                                                  SiteCode == "LAKE_P_ARI0003" & FieldSeason == "2007" ~ "< 10 m",
                                                  SiteCode == "DEVA_P_HOL0706" & FieldSeason == "2005" ~ "< 10 m",
                                                  SiteCode == "DEVA_P_EAS0201" & FieldSeason == "2005" ~ "< 10 m",
                                                  SiteCode == "DEVA_P_SED1050" & FieldSeason == "2005" ~ "< 10 m",
                                                  SiteCode == "DEVA_P_MES0218" & FieldSeason == "2005" ~ "10 - 50 m",
                                                  SiteCode == "DEVA_P_SAL0168" & FieldSeason == "2005" ~ "> 50 m",
                                                  SiteCode == "DEVA_P_HUN0746" & FieldSeason == "2005" ~ "> 50 m",
                                                  SiteCode == "DEVA_P_HUN0486" & FieldSeason == "2005" ~ "> 50 m",
                                                  SiteCode == "DEVA_P_FLY0494" & FieldSeason == "2005" ~ "> 50 m",
                                                  SiteCode == "DEVA_P_HAL0809" & FieldSeason == "2006" ~ "> 50 m",
                                                  TRUE ~ FlowCategory)) %>%
    dplyr::mutate(SampleFrame = dplyr::case_when(SiteCode == "MOJA_P_WHI0220" ~ "Over",
                                                 SiteCode == "MOJA_P_CUT0081" ~ "Over",
                                                 SiteCode == "LAKE_P_ARI0003" ~ "Inactive",
                                                 SiteCode == "LAKE_P_NEV0035" ~ "Inactive",
                                                 SiteCode == "JOTR_P_BLA0045" ~ "Inactive",
                                                 TRUE ~ SampleFrame)) %>%
    dplyr::mutate(Visit = dplyr::case_when((Park %in% c("LAKE", "MOJA") & FieldSeason == "2016") | (Park %in% c("PARA", "JOTR", "CAMO") & FieldSeason == "2017") | (Park %in% c("DEVA") & FieldSeason == "2018") ~ "First",
                                           (Park %in% c("LAKE", "MOJA", "CAMO") & FieldSeason == "2019") | (Park %in% c("PARA", "JOTR") & FieldSeason == "2020") | (Park %in% c("DEVA") & FieldSeason == "2021") ~ "Second",
                                           (Park %in% c("LAKE", "MOJA", "CAMO") & FieldSeason == "2022") | (Park %in% c("PARA", "JOTR") & FieldSeason == "2023") | (Park %in% c("DEVA") & FieldSeason == "2024") ~ "Third",
                                           FieldSeason %in% c("2005", "2006", "2007") ~ "Inventory",
                                           TRUE ~ NA_character_))
   
  data$FlowCategory <- factor(data$FlowCategory, levels = c("> 50 m", "10 - 50 m", "< 10 m", "Wet Soil", "Dry"))
  
  data$Visit <- factor(data$Visit, levels = c("Inventory", "First", "Second", "Third"))
  
  data %<>% filter(!is.na(FlowCategory))
  
  heatmap <- ggplot2::ggplot(data %>% filter(SampleFrame == "3Yr"), aes(x = Visit, 
                                                                        y = reorder(SiteCode, desc(SiteCode)),
                                                                        fill = FlowCategory)) + 
    geom_tile(color = "white") + 
    scale_fill_manual(values = c("navy", "royalblue1", "lightskyblue", "gold", "red"), name = "Flow Category") +
    labs(x = "Revisit Cycle",
         y = "Three-Year Spring") +
    theme(legend.position = "bottom") +
    facet_grid(Park~., scales = "free", space = "free_y")
  
  return(heatmap)
}


TempElevPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  inv <- readr::read_csv(name, show_col_types = FALSE)
  sites <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "Site")
  wq <- WqMedian(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  
  coords <- sites %>%
    dplyr::select(SiteCode, Lat_WGS84, Lon_WGS84, X_UTM_NAD83_11N, Y_UTM_NAD83_11N)

  elev <- inv %>%
    dplyr::rename("SiteCode" = "MOJN_SpringCode") %>%
    dplyr::select(SiteCode, ElevationDEM_m) %>%
    dplyr::mutate(ElevationDEM_ft = ElevationDEM_m * 3.2808)
  
  wq.elev <- wq %>%
    dplyr::left_join(elev, by = "SiteCode") %>%
    dplyr::filter(Park != "CAMO")
  
  temp.elev.plot <- ggplot2::ggplot(wq.elev,
                                    aes(x = TempMedian,
                                    y = ElevationDEM_ft,
                                    color = Park,
                                    shape = Park
                                    # label = SiteCode,
                                    # label2 = FieldSeason
                                    )) +
    geom_point(size = 4) +
    geom_smooth(method = "lm"
                # aes(fill = Park)
                    ) +
    scale_color_brewer(palette = "Dark2") +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 28, color = "black"),
          axis.title = element_text(size = 28, face = "bold"),
          legend.text = element_text(size = 28),
          legend.title = element_text(size = 28, face = "bold"),
          axis.title.x = element_text(margin = margin (t = 10)),
          axis.title.y = element_text(margin = margin (r = 15)),
          legend.key = element_blank()) +
    labs(x = "Water Temperature (C)",
         y = "Elevation of Spring (ft)") +
    ylim(min = -500, 8000)

  return(temp.elev.plot)  
}


SpCondElevPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  inv <- readr::read_csv(name, show_col_types = FALSE)
  sites <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "Site")
  wq <- WqMedian(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  
  coords <- sites %>%
    dplyr::select(SiteCode, Lat_WGS84, Lon_WGS84, X_UTM_NAD83_11N, Y_UTM_NAD83_11N)
  
  elev <- inv %>%
    dplyr::rename("SiteCode" = "MOJN_SpringCode") %>%
    dplyr::select(SiteCode, ElevationDEM_m) %>%
    dplyr::mutate(ElevationDEM_ft = ElevationDEM_m * 3.2808)
  
  wq.elev <- wq %>%
    dplyr::left_join(elev, by = "SiteCode") %>%
    dplyr::filter(!(SiteCode == "LAKE_P_SAU0022" & FieldSeason == "2016")) %>%
    dplyr::filter(Park != "CAMO")
  
  spcond.elev.plot <- ggplot2::ggplot(wq.elev,
                                      aes(x = SpCondMedian,
                                          y = ElevationDEM_ft,
                                          color = Park,
                                          shape = Park
                                          # label = paste(SiteCode, FieldSeason)
                                          )) +
    geom_point() +
    geom_smooth(method = "lm") +
    scale_x_log10(labels = comma) +
    geom_point(size = 4) +
    geom_smooth(method = "lm"
                # aes(fill = Park)
    ) +
    scale_color_brewer(palette = "Dark2") +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 28, color = "black"),
          axis.title = element_text(size = 28, face = "bold"),
          legend.text = element_text(size = 28),
          legend.title = element_text(size = 28, face = "bold"),
          axis.title.x = element_text(margin = margin (t = 10)),
          axis.title.y = element_text(margin = margin (r = 15)),
          legend.key = element_blank()) +
    labs(x = "Specific Conductance (uS/cm)",
         y = "Elevation of Spring (ft)") +
    ylim(min = -500, 8000)
  
  return(spcond.elev.plot)    
  
}


FullFlowCategoriesPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  data <- FullFlowCategoriesDiscontinuous(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  
  data$FlowCategory <- factor(data$FlowCategory, levels = c("> 50 m", "10 - 50 m", "< 10 m", " ", "Wet Soil", "Dry"))
  
  data %<>% filter(!is.na(FlowCategory)) %>%
    dplyr::filter((Park %in% c("LAKE", "MOJA") & FieldSeason %in% c("2016", "2019", "2022")) |
                  (Park %in% c("JOTR", "PARA") & FieldSeason %in% c("2017", "2020", "2023")) |
                  (Park %in% c("DEVA") & FieldSeason %in% c("2018", "2021", "2024")) |
                  (FieldSeason %in% c("2005", "2006", "2007"))) %>%
    dplyr::mutate(FieldSeason = dplyr::case_when(FieldSeason %in% c("2005", "2006", "2007") ~ "Inventory",
                                                 TRUE ~ FieldSeason))
  
  data$FieldSeason <- factor(data$FieldSeason, levels = c("Inventory", "2016", "2017", "2018", "2019", "2020", "2021", "2022"))
  
  plot <- ggplot2::ggplot(data %>% filter(Park == "MOJA"), aes(x = FieldSeason, y = Count, fill = FlowCategory)) +
    geom_bar(stat = "identity") +
    facet_grid(~Park) +
    scale_fill_manual(values = c("Dry" = "red",
                                 "Wet Soil" = "gold",
                                 " " = "white",
                                 "< 10 m" = "lightskyblue",
                                 "10 - 50 m" = "royalblue1",
                                 "> 50 m" = "navy")) +
    theme(legend.position = "bottom",
          # axis.text.x = element_text(angle = 90),
          axis.text = element_text(size = 28, color = "black"),
          axis.title = element_text(size = 28, face = "bold"),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20, face = "bold"),
          axis.title.x = element_text(margin = margin (t = 10)),
          axis.title.y = element_text(margin = margin (r = 15)),
          legend.key = element_blank()) +
    labs(x = "Field Season",
         y = "Number of Springs", 
         fill = "Flow Category") +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  
  return(plot)
  
}


FullFlowCategoriesHeatMap <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  joined <- FullSpringDischarge(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source)
  
  data <- joined %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::mutate(FlowCategory = dplyr::case_when(FlowCondition == "dry" ~ "Dry",
                                                  FlowCondition == "wet soil only" | (!(FlowCondition %in% c("dry", "wet soil only")) & (SpringbrookLength_m == 0 | SpringbrookWidth_m == 0)) ~ "Wet Soil",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLength_m > 0 & DiscontinuousSpringbrookLength_m < 10) | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLength_m > 0 & SpringbrookLength_m < 10) ~ "< 10 m",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLengthFlag == "Measured" & (DiscontinuousSpringbrookLength_m >= 10 & DiscontinuousSpringbrookLength_m <= 50)) | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLengthFlag == "Measured" & (SpringbrookLength_m >= 10 & SpringbrookLength_m <= 50)) ~ "10 - 50 m",
                                                  (SpringbrookType == "D" & DiscontinuousSpringbrookLengthFlag == ">50m") | ((SpringbrookType != "D" | is.na(SpringbrookType)) & SpringbrookLengthFlag == ">50m") ~ "> 50 m",
                                                  TRUE ~ "NA")) %>%
    dplyr::mutate(FlowCategory = dplyr::case_when(SiteCode == "PARA_P_COY0069" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "JOTR_P_EAS0160" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "JOTR_P_PIN0216" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "JOTR_P_QUE0109" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "MOJA_P_WIL0222" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "MOJA_P_BUD0021" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "MOJA_P_BUD0021" & FieldSeason == "2016" ~ "< 10 m",
                                                  SiteCode == "MOJA_P_HOR0121" & FieldSeason == "2006" ~ "< 10 m",
                                                  SiteCode == "LAKE_P_WHI0030" & FieldSeason == "2007" ~ "< 10 m",
                                                  SiteCode == "LAKE_P_LOS0009" & FieldSeason == "2007" ~ "< 10 m",
                                                  SiteCode == "LAKE_P_NEV0035" & FieldSeason == "2007" ~ "< 10 m",
                                                  SiteCode == "LAKE_P_BLA0053" & FieldSeason == "2007" ~ "< 10 m",
                                                  SiteCode == "LAKE_P_ARI0003" & FieldSeason == "2007" ~ "< 10 m",
                                                  SiteCode == "DEVA_P_HOL0706" & FieldSeason == "2005" ~ "< 10 m",
                                                  SiteCode == "DEVA_P_EAS0201" & FieldSeason == "2005" ~ "< 10 m",
                                                  SiteCode == "DEVA_P_SED1050" & FieldSeason == "2005" ~ "< 10 m",
                                                  SiteCode == "DEVA_P_MES0218" & FieldSeason == "2005" ~ "10 - 50 m",
                                                  SiteCode == "DEVA_P_SAL0168" & FieldSeason == "2005" ~ "> 50 m",
                                                  SiteCode == "DEVA_P_HUN0746" & FieldSeason == "2005" ~ "> 50 m",
                                                  SiteCode == "DEVA_P_HUN0486" & FieldSeason == "2005" ~ "> 50 m",
                                                  SiteCode == "DEVA_P_FLY0494" & FieldSeason == "2005" ~ "> 50 m",
                                                  SiteCode == "DEVA_P_HAL0809" & FieldSeason == "2006" ~ "> 50 m",
                                                  TRUE ~ FlowCategory)) %>%
    dplyr::mutate(SampleFrame = dplyr::case_when(SiteCode == "MOJA_P_WHI0220" ~ "Over",
                                                 SiteCode == "MOJA_P_CUT0081" ~ "Over",
                                               # SiteCode == "LAKE_P_ARI0003" ~ "Inactive",
                                               # SiteCode == "LAKE_P_NEV0035" ~ "Inactive",
                                                 SiteCode == "JOTR_P_BLA0045" ~ "Inactive",
                                                 TRUE ~ SampleFrame)) %>%
    dplyr::mutate(Visit = dplyr::case_when((Park %in% c("LAKE", "MOJA") & FieldSeason == "2016") | (Park %in% c("PARA", "JOTR", "CAMO") & FieldSeason == "2017") | (Park %in% c("DEVA") & FieldSeason == "2018") ~ "First",
                                           (Park %in% c("LAKE", "MOJA", "CAMO") & FieldSeason == "2019") | (Park %in% c("PARA", "JOTR") & FieldSeason == "2020") | (Park %in% c("DEVA") & FieldSeason == "2021") ~ "Second",
                                           (Park %in% c("LAKE", "MOJA", "CAMO") & FieldSeason == "2022") | (Park %in% c("PARA", "JOTR") & FieldSeason == "2023") | (Park %in% c("DEVA") & FieldSeason == "2024") ~ "Third",
                                           FieldSeason %in% c("2005", "2006", "2007") ~ "Inventory",
                                           TRUE ~ NA_character_))
  
  data$FlowCategory <- factor(data$FlowCategory, levels = c("> 50 m", "10 - 50 m", "< 10 m", "Wet Soil", "Dry"))
  
  data$Visit <- factor(data$Visit, levels = c("Inventory", "First", "Second", "Third"))
  
  data %<>% filter(!is.na(FlowCategory))
  
  heatmap <- ggplot2::ggplot(data %>% filter(Park == "MOJA",
                                             SampleFrame %in% c("Annual", "3Yr"),
                                             Visit %in% c("Inventory", "First", "Second", "Third")),
                             aes(x = Visit, 
                                 y = reorder(SiteCode, desc(SiteCode)),
                                 fill = FlowCategory)) + 
    geom_tile(color = "white") + 
    scale_fill_manual(values = c("navy", "royalblue1", "lightskyblue", "gold", "red"), name = "Flow Category") +
    labs(x = "Revisit Cycle",
         y = "Spring Code") +
    theme(legend.position = "bottom",
          # axis.text.x = element_text(angle = 90),
          axis.text.x = element_text(size = 28, color = "black"),
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title = element_text(size = 28, face = "bold"),
          legend.text = element_text(size = 28),
          legend.title = element_text(size = 28, face = "bold"),
          axis.title.x = element_text(margin = margin (t = 10)),
          axis.title.y = element_text(margin = margin (r = 15))) +
    guides(fill = guide_legend(reverse = TRUE)) +
    facet_grid(Park~., scales = "free", space = "free_y")
  
  return(heatmap)
}