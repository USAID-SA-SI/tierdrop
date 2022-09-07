# SETUP script -----------------------------------------

#Step 1: Step up directories and standard folders
dir_setup()


#set folderpaths
ndoh_folderpath <- 'data-raw/NDOH'
reference_folder <- "data-raw/Reference Files"
msd_folder <- "data-raw/MSD-Genie"
ndoh_filepath <- ndoh_folderpath %>% glamr::return_latest()
msd_filepath <- msd_folder %>% glamr::return_latest()

#store some locals
fiscal_quarter <- "FY22Q3"
curr_qtr <- "Q3"

#check to ensure that the most recent ndoh_file is the you want to use
print(ndoh_filepath)
print(msd_filepath)

#load secrets
glamr::load_secrets()

# MFL --------------------------------------------------------

#Load MFL as is
mfl_new_df <- googlesheets4::read_sheet(mfl_new_id, sheet = "MFL")

#then grab the clean MFL
df_fac <- clean_mfl()

# LOAD MECHS --------------------------------------------------

#read the most recent MSD from the Genie folder
df_genie <- msd_folder %>%
  glamr::return_latest() %>%
  gophr::read_msd()


#first, let's pull down all the mech code / mech uid information from DATIM
# Note - this may take a few minutes to pull in
mechs <- pull_mech_uid(ou_sel = "South Africa")

# just for now because of wifi issues

mechs <- glamr::si_path() %>%
  glamr::return_latest("mechs") %>%
  readr::read_csv()

mechs <- mechs %>%
  dplyr::mutate(mech_code = as.character(mech_code))

#Now, we are going to use the mech data from DATIM, join with facility-level mech-data from the MSD
# to get a list of all the facilities reported most recents and the corresponding mech data

# Sometimes, you'll have some extra mechs to encode manually - for example, in FY22Q3, there were
# two MATCH facilities that were not in DATIM yet, so we had to add these manually. If so, select
# `grab_mech_data(df, extra_mechs = TRUE)` and follow the prompt to ensure the data is saved correctly.
# Note - errors will likely be due to file path issues
mech_df <- grab_mech_data(mech_df = mechs, msd_df = df_genie, extra_mechs = TRUE)



# IMPORT NDOH

#Parameters:
  #filepath
  #qtr
  #KP
  #

#first for non-KP indics
ndoh_all <- import_ndoh(filepath = ndoh_filepath, qtr = "Q3", kp = FALSE)

#check for facility alignment between MFL and NDOH - okay if they are all Correctional
validate_ndoh(ndoh_all)

ndoh_clean <- tidy_ndoh(ndoh_all, kp = FALSE)


#map additonal disaggs

map_disaggs(df = ndoh_clean, all_indic = TRUE)
map_disaggs(ndoh_clean, "PrEP_CT", "TestResult", FALSE)
map_disaggs(ndoh_clean, "PrEP_CT", "Age/Sex", FALSE)
map_disaggs(ndoh_clean, "TX_RTT", "Age/Sex/HIVStatus", FALSE)
map_disaggs(ndoh_clean, "TX_RTT", "ARTNoContactReasonIIT", FALSE)

ndoh_post_processing(kp = FALSE, export_type = "Import")

ndoh_post_processing(kp = FALSE, export_type = "Validation")


# kp ----------------------------

ndoh_kp <- import_ndoh(filepath = ndoh_filepath,qtr = "Q3", kp = TRUE)
validate_ndoh(ndoh_kp)
ndoh_clean_kp <- tidy_ndoh(ndoh_kp, kp = TRUE)

#check this for KP when on wifi

map_disaggs(df = ndoh_clean_kp, all_indic = TRUE)
map_disaggs(ndoh_clean_kp, "PrEP_CT", "TestResult", FALSE)
map_disaggs(ndoh_clean_kp, "PrEP_CT", "Age/Sex", FALSE)

# NO TX_RTT FOR key pops


ndoh_post_processing(kp = TRUE, export_type = "Import")
ndoh_post_processing(kp = TRUE, export_type = "Validation")

