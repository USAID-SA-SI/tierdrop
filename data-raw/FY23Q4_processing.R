# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  FY23Q4 DATIM Processing
# LICENSE:  MIT
# DATE:     2023-10-18
# UPDATED:

# DEPENDENCIES ------------------------------------------------------------

library(glamr)
library(tidyverse)
library(gophr)
library(glue)
library(readxl)
library(googlesheets4)
library(tierdrop)

# SETUP script -----------------------------------------

#Step 1: Step up directories and standard folders
dir_setup()

#store some locals (change this to a grab metadata later as well)
get_meta("FY23Q4")


#set folderpaths (@to-do turn this into a get_metadata() function later)
ndoh_folderpath <- 'data-raw/NDOH'
reference_folder <- "data-raw/Reference Files"
msd_folder <- "data-raw/MSD-Genie"
import_folder <- "data-raw/Import Files"
ndoh_filepath <- ndoh_folderpath %>% glamr::return_latest(fiscal_quarter)
msd_filepath <- msd_folder %>% glamr::return_latest()



#check to ensure that the most recent ndoh_file is the you want to use
print(ndoh_filepath)
print(msd_filepath)

#load secrets
glamr::load_secrets()

import_vars <- c("mech_uid", "orgUnit_uid",	"dataElement_uid",
                 "categoryOptionCombo_uid",	"value",	"period")

partner_vars <- c("mech_uid", "orgUnit_uid", "Facility","dataElement",	"dataElement_uid", "categoryOptionComboName",
                  "categoryOptionCombo_uid",	"value",	"period")


validation_vars <- c("period","Province", "District","SubDistrict", "Facility",
                     "orgUnit_uid", "mech_code", "mech_uid",
                     "indicator", "numeratordenom", "Sex",
                     "CoarseAgeGroup", "Result","dataElement", "dataElement_uid", "categoryOptionComboName",
                     "categoryOptionCombo_uid", "value")


df_map_distinct <- googlesheets4::read_sheet(disagg_map_id) %>%
  dplyr::rename("Test Result/Outcome/Duration" = "Test Resuts/Outcome/Duration",
                "DSD_TA" = "Support Type")

#read the most recent MSD from the Genie folder
df_genie <- msd_folder %>%
  glamr::return_latest() %>%
  gophr::read_psd()

# MFL ---------------------------------------------

mfl_new_df <- googlesheets4::read_sheet(mfl_fy23_id, sheet = "MFL_FY23")

#get mech info from MFL
mech_mfl <- mfl_new_df %>%
  dplyr::filter(!is.na(OU2name)) %>%
  janitor::clean_names() %>%
  dplyr::select(ou5name, datim_uid,partner, mechanism_i_d, mechanism_uid) %>%
  rename(sitename = ou5name,
         facilityuid = datim_uid,
         prime_partner_name = partner,
         mech_code = mechanism_i_d,
         mech_uid = mechanism_uid)

#get DSD/TA breakdown
df_fac <- clean_mfl() %>%
  rename(DSD_TA = dsd_ta)
