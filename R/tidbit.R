#' Convert raw TidbiT data into format importable into Aquarius database.
#' 
#'
#' @return A tibble with columns for date-time, water temperature, water detect, and water binary.
#' @export
#'
#' @examples ProcessTidbiT()
ProcessTidbiT <- function() {
  # Enter water year of folder to process raw files.
  wy <- "WY2021"
  
  # Create variables for file paths.
  raw_folder <- paste0("M:\\MONITORING\\DS_Water\\Data\\LoggerData\\",wy,"\\Raw")
  csv_list <- list.files(raw_folder, pattern = "csv")
  file_paths <- paste0(raw_folder,"\\",csv_list)
  
  for(path in file_paths) {
    
    # Import raw CSV file of HOBO TidbiT logger data.
    import <- read.csv(path, header = TRUE, skip = 1, na.strings = c(""," "))
    
    # Standardize column names.
    import %<>% dplyr::rename_if(stringr::str_detect(names(.), "Date") & stringr::str_detect(names(.), "08"), ~paste0("DateTime_GMT0800"))
    import %<>% dplyr::rename_if(stringr::str_detect(names(.), "Date") & stringr::str_detect(names(.), "07"), ~paste0("DateTime_GMT0700"))
    import %<>% dplyr::rename_if(stringr::str_detect(names(.), "Temp") & stringr::str_detect(names(.), "F"), ~paste0("Temp_F"))
    import %<>% dplyr::rename_if(stringr::str_detect(names(.), "Temp") & stringr::str_detect(names(.), "C"), ~paste0("Temp_C"))
    import %<>% dplyr::rename_if(stringr::str_detect(names(.), "Water.Detect"), ~paste0("Water_Detect"))
    
    if(!"Water_Detect" %in% colnames(import)) 
    {
      import$Water_Detect <- NA
    }
    
    # Fill NAs below each recorded "In" or "Out" value to match.
    export <- import %>%
      tidyr::fill(Water_Detect)
    
    # Determine whether the first Water.Detect value is "In" or "Out."
    top <- export$Water_Detect[!is.na(export$Water_Detect)]
    top <- top[1]
    
    # Water.Detect values are only recorded when there is a change in status.
    # This means that NAs before the first recorded "In" or "Out" value should be opposite of that value.
    # If first recorded detection is "In," change the top NA to "Out."
    # If first recorded detection is "Out," change the top NA to "In."
    if(is.na(export$Water_Detect[1]) & top %in% "In") {
      export$Water_Detect[1] <- "Out"
    } else {
      if(is.na(export$Water_Detect[1]) & top %in% "Out") {
        export$Water_Detect[1] <- "In"
      }
    }
    
    # Apply fill to remaining NAs, if applicable.
    export <- export %>%
      tidyr::fill(Water_Detect)
    
    # Create column where "In" and "Out" become 1 and 0, respectively.
    # Aquarius database requires numerical data for upload.
    export <- export %>%
      dplyr::mutate(Water_Binary = ifelse(Water_Detect %in% "In", 1, 
                                          ifelse(Water_Detect %in% "Out", 0, NA)))
    
    # If temperature data exist in units of F, convert to C.
    export$Temp_C <-if("Temp_F" %in% names(export)) {
      (export$Temp_F - 32) * 5/9
    } else {
      export$Temp_C
    }
    
    # Reorder and select new columns in data frame.
    export <- export %>%
      dplyr::relocate(Temp_C, .before = Water_Detect) %>%
      dplyr::relocate(Water_Binary, .after = Water_Detect) %>%
      dplyr::select(contains("DateTime"), contains("Temp"), contains("Water"))
    
    # Save processed HOBO TidbiT logger data to CSV file.
    new_file_path <- stringr::str_replace(path, ".csv", "_PROCESSED.csv")
    processed_path <- stringr::str_replace(new_file_path, "\\\\Raw\\\\", "\\\\Processed\\\\")
    readr::write_csv(export, processed_path, append = FALSE, col_names = TRUE) # If you run this twice in the same year, it will overwrite the processed folder.
    
    return(export)
  }
}