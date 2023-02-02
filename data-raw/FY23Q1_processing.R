# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  FY23Q1 DATIM Processing
# REF ID:   94e4a2bc
# LICENSE:  MIT
# DATE:     2023-01-20
# UPDATED:

# DEPENDENCIES ------------------------------------------------------------

library(glamr)
library(tidyverse)
library(glitr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(readxl)
library(googlesheets4)
library(tierdrop)

# SETUP script -----------------------------------------

#Step 1: Step up directories and standard folders
dir_setup()

#store some locals (change this to a grab metadata later as well)
fiscal_quarter <- "FY23Q1"
curr_qtr <- "Q1"
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

validation_vars <- c("period","Province", "District","SubDistrict", "Facility",
                     "orgUnit_uid", "mech_code", "mech_uid", 'mech_name',
                     "indicator", "numeratordenom", "Sex",
                     "CoarseAgeGroup", "Result","dataElement", "dataElement_uid", "categoryOptionComboName",
                     "categoryOptionCombo_uid", "value")

# MFL ---------------------------------------------

df_fac <- clean_mfl() %>%
  rename(DSD_TA = dsd_ta)


#address MFL weirdness (mp beatty will need to be added manually)
  #manually change the misaligned codes to what the codes are in the NDOH
  # this is NOT best practice and should not be a standard moving forward

df_fac <- df_fac %>%
  mutate(new_ou5_code = case_when(usaid_facility ==
                                 "ec Matatiele Community Clinic" ~ "1870684",
                                 usaid_facility ==
                                 "kz Empangeni Clinic" ~ "4348504",
                                 usaid_facility ==
                                 "kz Khandisa Clinic" ~ "4364152",
                                 usaid_facility ==
                                 "kz Luwamba Clinic" ~ "4527574",
                                 usaid_facility ==
                                 "kz Mvutshini Clinic (uMlalazi)" ~ "442706",
                                 usaid_facility ==
                                 "lp Naledi Clinic" ~ "5217420",
                                 usaid_facility ==
                                 "mp Naas CHC" ~ "6189131",
                                 usaid_facility ==
                                 "mp Rockdale CHC" ~ "6258774",
                               TRUE ~ new_ou5_code)) %>%
  mutate(usaid_facility = case_when(usaid_facility ==
                                      "mp Emalahleni Mobile 5" ~ "mp Beatty Mobile 5",
                                    TRUE ~ usaid_facility))
# %>%
#   mutate(new_ou5_code = case_when(usaid_facility ==
#                                       "mp Beatty Mobile 5" ~ "6389072",
#                                     TRUE ~ usaid_facility))




#read the most recent MSD from the Genie folder
df_genie <- msd_folder %>%
  glamr::return_latest() %>%
  gophr::read_msd()


#first, let's pull down all the mech code / mech uid information from DATIM
# Note - this may take a few minutes to pull in

#mechs <- pull_mech_uid(ou_sel = "South Africa")

# just for now because of wifi issues

mechs <- glamr::si_path() %>%
  glamr::return_latest("mechs") %>%
  readr::read_csv()

mechs <- mechs %>%
  dplyr::mutate(mech_code = as.character(mech_code))



# NDOH ---------------------------------------------------------------------

#use version on q4-update branch
ndoh_all <- import_ndoh(filepath = ndoh_filepath, qtr = "Q1", kp = FALSE)
ndoh_all_kp <- import_ndoh(filepath = ndoh_filepath, qtr = "Q1", kp = TRUE)

#validate - check NDOH/MFL triangulation
# TODO: update validate_ndoh()

#what facilities are in NDOH but not in MFL? ADdress MFL qc as needed
ndoh_code <- unique(ndoh_all$Code)
kp_code <- unique(ndoh_all_kp$Code)
mfl_code <- unique(df_fac$new_ou5_code)
code_list <- setdiff(ndoh_code, mfl_code)
kp_code_list <- setdiff(kp_code, mfl_code)

# all correctinal
missing_sites <- ndoh_all %>%
  filter(Code %in% code_list) %>% distinct(Facility) %>% pull()


missing_sites_kp <- ndoh_all_kp %>%
  filter(Code %in% kp_code_list) %>% distinct(Facility) %>% pull()

df_fac %>%
  filter(usaid_facility %in% missing_sites) %>%
  distinct(usaid_facility, new_ou5_code) %>%
  arrange(usaid_facility)


df_fac %>%
  filter(usaid_facility %in% missing_sites_kp) %>%
  distinct(usaid_facility, new_ou5_code) %>%
  arrange(usaid_facility)

#TIDY
ndoh_clean <- tidy_ndoh(ndoh_all, kp = FALSE)

#aggregate TB_STAT
ndoh_tb_stat <- ndoh_clean %>%
  filter(indicator == "TB_STAT",
         numeratordenom == "N",
         `Test Result/Outcome/Duration` %in% c("New Negative", 'New Positive')) %>%
  #filter(CoarseAgeGroup == "<1")
  mutate(CoarseAgeGroup = ifelse(CoarseAgeGroup =="<1", "1-4", CoarseAgeGroup)) %>%
  group_by(usaid_facility, ou5uid, datim_uid, new_ou5_code, period, DSD_TA,
           Province, SubDistrict, District, Facility, `Test Result/Outcome/Duration`,
           Sex, CoarseAgeGroup, Result, indicator, numeratordenom) %>%
  dplyr::summarise(dplyr::across(tidyselect::starts_with("Total"), sum, na.rm = TRUE), .groups = "drop")

ndoh_clean <-  ndoh_clean %>%
  filter(!(indicator == "TB_STAT" & numeratordenom == "N" &
             `Test Result/Outcome/Duration` %in% c("New Negative", 'New Positive'))) %>%
  rbind(ndoh_tb_stat)

ndoh_clean_kp <- tidy_ndoh(ndoh_all_kp, kp = TRUE)


mech_df <- grab_mech_data(mech_df = mechs, msd_df = df_genie, extra_mechs = FALSE)


# MAP
df_mapped <- ndoh_post_processing(ndoh_clean %>% filter(!(indicator == "TX_TB" & numeratordenom == "D")), kp = FALSE, export_type = "Validation")
df_mapped_kp <- ndoh_post_processing(ndoh_clean_kp, kp = TRUE, export_type = "Validation")


#bind together
df_final <- dplyr::bind_rows(df_mapped,
                             df_mapped_kp) %>%
  distinct() %>%
  filter(!is.na(dataElement))

df_final %>%
  janitor::get_dupes()

#filter out Harry Gwala for PrEP
#Remove PrEP_NEW & PrEP_CURR from Capricorn & Mopani.(ANOVA)

df_final <- df_final %>%
  filter(!(District == "kz Harry Gwala District Municipality" & indicator %in% c("PrEP_CT", "PrEP_NEW"))) %>%
  filter(!(District %in% c("lp Capricorn District Municipality",
                         "lp Mopani District Municipality")
         & indicator %in% c("PrEP_CT", "PrEP_NEW")))


import_final <- df_final %>%
  select(import_vars) %>%
  mutate(period = recode(period, "FY23Q1" = "2022Q4")) #functionalize this in next tier update

# df_final_all <- dplyr::bind_rows(df_mapped,
#                                  df_mapped_kp) %>%
#   filter(!is.na(dataElement))



#Check
import_final %>%
  #select(import_vars) %>%
  janitor::get_dupes()



# EXPORT ----------------------------------------------


df_final %>%
  readr::write_csv(glue::glue("{import_folder}/{fiscal_quarter}_TIER_Import_File_v3_{today}_ALL.csv"))


import_final %>%
  readr::write_csv(glue::glue("{import_folder}/{fiscal_quarter}_TIER_Import_File_v3_{today}.csv"))



# #filter out Q4 tx_curr
# tier_final_consolidated <- tier_final_consolidated %>%
#   filter(!(Facility %in% use_q3_txcurr & indicator == "TX_CURR"))

#Partner files
Broadreach_import <- partner_import(df = import_final, 70287)
RTC_import <- partner_import(df = import_final, 70290)
ANOVA_import <- partner_import(df = import_final, 70310)
MATCH_import <- partner_import(df = import_final, 81902)
WRHI_import <- partner_import(df = import_final, 70301)
