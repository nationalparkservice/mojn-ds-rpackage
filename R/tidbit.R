#' Convert raw TidbiT data into format importable into Aquarius database.
#' 
#' To do: path to import as one argument, path to export as second argument, option to write to CSV or not as third argument
#' To do: "for loop" to apply function to all CSVs in Raw folder
#' 
#' @param path.to.data 
#' @param data.source 
#'
#' @return A tibble with columns for date-time, water temperature, water detect, and water binary.
#' @export
#'
#' @examples
ProcessTidbiT <- function(path.to.data, data.source = "database") {
  
# Import raw CSV file of HOBO TidbiT logger data.
import <- read.csv("M:\MONITORING\DS_Water\Data\LoggerData\WY2021\Raw\xxxxxx.csv", header = TRUE, skip = 2, na.strings=c(""," "))
  
# Fill NAs below each recorded "In" or "Out" value to match.
export <- import %>%
    tidyr::fill(Water.Detect)
  
# Determine whether the first Water.Detect value is "In" or "Out."
top <- export$Water.Detect[!is.na(export$Water.Detect)]
top <- top[1]
  
# Water.Detect values are only recorded when there is a change in status.
# This means that NAs before the first recorded "In" or "Out" value should be opposite of that value.
# If first recorded detection is "In," change the top NA to "Out."
# If first recorded detection is "Out," change the top NA to "In."
if(is.na(export$Water.Detect[1]) & top %in% "In") {
    export$Water.Detect[1] <- "Out"
    } else {
      if(is.na(export$Water.Detect[1]) & top %in% "Out") {
          export$Water.Detect[1] <- "In"
  }
}
  
# Apply fill to remaining NAs, if applicable.
export <- export %>%
    tidyr::fill(Water.Detect)
  
# Create column where "In" and "Out" become 1 and 0, respectively.
# Aquarius database requires numerical data for upload.
export <- export %>%
    dplyr::mutate(Water.Binary = ifelse(Water.Detect %in% "In", 1, 
                                        ifelse(Water.Detect %in% "Out", 0, NA)))
  
# If temperature data exist in units of F, convert to C.
export$Temp....C. <-if("Temp....F." %in% names(export)) {
    (export$Temp....F. - 32) * 5/9
    } else {
      export$Temp....C.
}
  
# Reorder new columns in data frame.
export <- export %>%
    dplyr::relocate(Temp....C., .before = Water.Detect) %>%
    dplyr::relocate(Water.Binary, .after = Water.Detect)
  
# Save processed HOBO TidbiT logger data to CSV file.
write.csv(export, "M:\MONITORING\DS_Water\Data\LoggerData\WY2021\Processed\xxxxxx_processed.csv", row.names = FALSE)
  
return(export)

}