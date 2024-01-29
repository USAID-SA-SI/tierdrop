# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  FY24Q1 DATIM Processing
# LICENSE:  MIT
# DATE:     2024-01-29
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
get_meta("FY24Q1")


#set folderpaths (@to-do turn this into a get_metadata() function later)
ndoh_filepath <- ndoh_folderpath %>% glamr::return_latest(fiscal_quarter)
msd_filepath <- msd_folder %>% glamr::return_latest()

#check to ensure that the most recent ndoh_file is the you want to use
print(ndoh_filepath)
print(msd_filepath)

#load secrets
glamr::load_secrets()

#indicator mapping file
df_map_distinct <- googlesheets4::read_sheet(disagg_map_id) %>%
  dplyr::rename("Test Result/Outcome/Duration" = "Test Resuts/Outcome/Duration",
                "DSD_TA" = "Support Type")

#ARVDISP mapping file
arv_map <- googlesheets4::read_sheet(disagg_map_id, sheet = "ARVDISP_FY23") %>%
  mutate(RegimenCode = as.character(RegimenCode))

#read the most recent MSD from the Genie folder
df_genie <- msd_folder %>%
  glamr::return_latest() %>%
  gophr::read_psd()

# MFL ---------------------------------------------

mfl_new_df <- googlesheets4::read_sheet(mfl_fy24_id, sheet = "MFL_FY24_Q1")

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
df_fac <- clean_mfl(mfl_period = "FY24Q1") %>%
  rename(DSD_TA = dsd_ta)

#adjust TX_TB_D names for now
#names_tx_tb_d <- c(standard_names, "CoarseAgeGroup",  "Sex", "Result","Total")

# NDOH ---------------------------------------------------------------------

#if this breaks, check on tab names (adjust PrEP_NEW in the file itself)
# update for ou5uid
ndoh_all <- import_ndoh(filepath = ndoh_filepath, qtr = curr_qtr, kp = FALSE)
ndoh_all_kp <- import_ndoh(filepath = ndoh_filepath, qtr = curr_qtr, kp = TRUE)

#what facilities are in NDOH but not in MFL? ADdress MFL qc as needed
# update for ou5uid
validate_ndoh(ndoh_all)
validate_ndoh(ndoh_all_kp)

# TIDY NDOH -----------------------------------------------------------

#TIDY
ndoh_clean <- tidy_ndoh(ndoh_all, kp = FALSE) %>%
  select(-c(Code))

#aggregate TB_STAT
ndoh_tb_stat <- ndoh_clean %>%
  filter(indicator == "TB_STAT",
         numeratordenom == "N",
         `Test Result/Outcome/Duration` %in% c("New Negative", 'New Positive')) %>%
  #filter(CoarseAgeGroup == "<1")
  mutate(CoarseAgeGroup = ifelse(CoarseAgeGroup =="<1", "1-4", CoarseAgeGroup)) %>%
  group_by(usaid_facility, ou5uid, datim_uid, new_ou5_code, period, DSD_TA, Province, SubDistrict, District, Facility, `Test Result/Outcome/Duration`,
           Sex, CoarseAgeGroup, Result, indicator, CD4, VL_BIN, numeratordenom) %>%
  dplyr::summarise(dplyr::across(tidyselect::starts_with("Total"), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

ndoh_clean <-  ndoh_clean %>%
  filter(!(indicator == "TB_STAT" & numeratordenom == "N" &
             `Test Result/Outcome/Duration` %in% c("New Negative", 'New Positive'))) %>%
  rbind(ndoh_tb_stat)

ndoh_clean_kp <- tidy_ndoh(ndoh_all_kp, kp = TRUE) %>%
  select(-c(Code))

# MAP -------------------------------------------------------------------------------------------


