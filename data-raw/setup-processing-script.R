# SETUP script
dir_setup()

#set folderpaths
folderpath <- 'data-raw/NDOH'
ndoh_filepath <- folderpath %>% glamr::return_latest()

#Load MFL as is
mfl_new_df <- googlesheets4::read_sheet(mfl_new_id)

#then grab the clean MFL
df_fac <- clean_mfl()


# IMPORT NDOH

#first for non-KP indics
ndoh_all <- import_ndoh(qtr = "Q3", kp = FALSE)

ndoh_kp <- import_ndoh(qtr = "Q3", kp = TRUE)
