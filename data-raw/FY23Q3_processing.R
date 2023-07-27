# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  FY23Q3 DATIM Processing
# LICENSE:  MIT
# DATE:     2023-07-25
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
fiscal_quarter <- "FY23Q3"
import_period_style <- "2023Q2"
curr_qtr <- "Q3"
today <- lubridate::today()


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

# df_fac <- df_fac %>%
#   mutate(new_ou5_code = case_when(usaid_facility ==
#                                     "ec Matatiele Community Clinic" ~ "1870684",
#                                   usaid_facility ==
#                                     "kz Empangeni Clinic" ~ "4348504",
#                                   usaid_facility ==
#                                     "kz Khandisa Clinic" ~ "4364152",
#                                   usaid_facility ==
#                                     "kz Luwamba Clinic" ~ "4527574",
#                                   # usaid_facility ==
#                                   #   "kz Mvutshini Clinic (uMlalazi)" ~ "442706",
#                                   usaid_facility ==
#                                     "lp Naledi Clinic" ~ "5217420",
#                                   usaid_facility ==
#                                     "mp Naas CHC" ~ "6189131",
#                                   usaid_facility ==
#                                     "mp Rockdale CHC" ~ "6258774",
#                                   TRUE ~ new_ou5_code))


# NDOH ---------------------------------------------------------------------

#if this breaks, check on tab names (adjust PrEP_NEW in the file itself)
ndoh_all <- import_ndoh(filepath = ndoh_filepath, qtr = curr_qtr, kp = FALSE)
ndoh_all_kp <- import_ndoh(filepath = ndoh_filepath, qtr = curr_qtr, kp = TRUE)


#what facilities are in NDOH but not in MFL? ADdress MFL qc as needed
validate_ndoh(ndoh_all)
validate_ndoh(ndoh_all_kp)

# adjust NDOH codes and MFL codes as needed, and then re-run to check again
# Goal: we only want to see Correctional facilities in the validation

# TIDY NDOH -----------------------------------------------------------

#TIDY
ndoh_clean <- tidy_ndoh(ndoh_all, kp = FALSE)

#aggregate TB_STAT
ndoh_tb_stat <- ndoh_clean %>%
  filter(indicator == "TB_STAT",
         numeratordenom == "N",
         `Test Result/Outcome/Duration` %in% c("New Negative", 'New Positive')) %>%
  #filter(CoarseAgeGroup == "<1")
  mutate(CoarseAgeGroup = ifelse(CoarseAgeGroup =="<1", "1-4", CoarseAgeGroup)) %>%
  group_by(usaid_facility, ou5uid, datim_uid, new_ou5_code, period, DSD_TA, Province, SubDistrict, District, Facility, `Test Result/Outcome/Duration`,
           Sex, CoarseAgeGroup, Result, indicator, numeratordenom) %>%
  dplyr::summarise(dplyr::across(tidyselect::starts_with("Total"), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

ndoh_clean <-  ndoh_clean %>%
  filter(!(indicator == "TB_STAT" & numeratordenom == "N" &
             `Test Result/Outcome/Duration` %in% c("New Negative", 'New Positive'))) %>%
  rbind(ndoh_tb_stat)

ndoh_clean_kp <- tidy_ndoh(ndoh_all_kp, kp = TRUE)

# MAP -------------------------------------------------------------------------------------------

#Map dataelements and mechs
df_mapped <- ndoh_post_processing(ndoh_clean %>% filter(!(indicator == "TX TB_Denom" & numeratordenom == "D")),
                                  kp = FALSE, export_type = "Validation")

df_mapped_kp <- ndoh_post_processing(ndoh_clean_kp, kp = TRUE, export_type = "Validation")


#bind together
df_final <- dplyr::bind_rows(df_mapped,
                             df_mapped_kp) %>%
  distinct() %>%
  filter(!is.na(dataElement)) %>%
  mutate(mech_code = as.character(mech_code))

df_final %>%
  janitor::get_dupes()

# CLEAN UP ------------------------------------------------------------

#Step 1: Filter out PrEP for Harry Gwala, Capricorn and Mopani; filter out all of MATCH PrEP for now

df_final_clean <- df_final %>%
  filter(!(District == "kz Harry Gwala District Municipality" & indicator %in% c("PrEP_CT", "PrEP_NEW"))) %>%
  filter(!(District %in% c("lp Capricorn District Municipality",
                           "lp Mopani District Municipality")
           & indicator %in% c("PrEP_CT", "PrEP_NEW"))) %>%
  filter(!(mech_code == "81902" & indicator %in% c("PrEP_CT", "PrEP_NEW")))


# Step 2: Keep only certain sites for MATCH prep sites
match_prep_q2 <- reference_folder %>%
  return_latest("PrEP Facilities MatCH Q2 reporting") %>%
  read_excel() %>%
  janitor::clean_names()


#grab uids for prep MATCH sites
match_prep_uids <- match_prep_q2 %>%
  # filter(prep_facility == "YES") %>%
  left_join(df_fac %>% select(usaid_facility, datim_uid),
            by = c("supported_facilities" = "usaid_facility")) %>%
  distinct(datim_uid) %>%
  pull()


#MATCH PREP IMPORT FILE
import_MATCH_prep <- df_final %>%
  filter((orgUnit_uid %in% match_prep_uids
          & indicator %in% c("PrEP_CT", "PrEP_NEW"))) %>%
  mutate(period = import_period_style)

# BIND WITH REST ---------------------------------------------------------
#for partner review
tier_final_partner <- bind_rows(df_final_clean %>% select(all_of(partner_vars)),
                                #tb_all_final%>% select(all_of(partner_vars)),
                                import_MATCH_prep %>% select(all_of(partner_vars))
) %>%
  mutate(period = import_period_style) %>%
  filter(!is.na(orgUnit_uid), #beatty mobile 5 and senorita hospital
         !is.na(mech_uid))

#for import file
tier_final_import <- bind_rows(df_final_clean %>% select(all_of(import_vars)),
                             #  tb_all_final%>% select(all_of(import_vars)),
                               import_MATCH_prep %>% select(all_of(import_vars))) %>%
  mutate(period = import_period_style) %>%
  filter(!is.na(orgUnit_uid), #beatty mobile 5 and senorita hospital
         !is.na(mech_uid))

#check for dupes
tier_final_import %>%
  select(import_vars) %>%
  janitor::get_dupes()

#EXPORT
today <- lubridate::today()

tier_final_import %>%
  readr::write_csv(glue::glue("{import_folder}/{fiscal_quarter}_TIER_Import_File_{today}.csv"))


#Partner files
Broadreach_import <- partner_import(df = tier_final_partner, 70287)
RTC_import <- partner_import(df = tier_final_partner, 70290)
ANOVA_import <- partner_import(df = tier_final_partner, 70310)
MATCH_import <- partner_import(df = tier_final_partner, 81902)
WRHI_import <- partner_import(df = tier_final_partner, 70301)

