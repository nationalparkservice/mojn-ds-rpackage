# Load data from AGOL
# Assumes that you are using keyring to store the pw for the headless account
LoadDesertSprings(agol_username = "mojn_hydro", agol_password = keyring::key_get("agol", "mojn_hydro"))

# Write data to temporary csv files for testing
dir <- withr::local_tempdir(clean = FALSE)
SaveDataToCsv(dir, create.folders = TRUE, overwrite = TRUE)

withr::defer(unlink(dir, recursive = TRUE), testthat::teardown_env())