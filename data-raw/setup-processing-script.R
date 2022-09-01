# SETUP script
dir_setup()

#set folderpaths
ndoh_folderpath <- 'data-raw/NDOH'
reference_folder <- "data-raw/Reference Files"
ndoh_filepath <- ndoh_folderpath %>% glamr::return_latest()

#check to ensure that the most recent ndoh_file is the you want to use
print(ndoh_filepath)

#Load MFL as is
mfl_new_df <- googlesheets4::read_sheet(mfl_new_id)

#then grab the clean MFL
df_fac <- clean_mfl()


# IMPORT NDOH

#first for non-KP indics
ndoh_all <- import_ndoh(filepath = ndoh_filepath, qtr = "Q3", kp = FALSE)

#check for facility alignment between MFL and NDOH - okay if they are all Correctional
validate_ndoh(ndoh_all)

ndoh_clean <- tidy_ndoh(ndoh_all, kp = FALSE)

# kp ----------------------------

ndoh_kp <- import_ndoh(filepath = ndoh_filepath,qtr = "Q3", kp = TRUE)
validate_ndoh(ndoh_kp)
ndoh_clean_kp <- tidy_ndoh(ndoh_kp, kp = TRUE)

