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
df_map_distinct <- googlesheets4::read_sheet(new_disagg_map_id) %>%
  dplyr::rename("Test Result/Outcome/Duration" = "Test Resuts/Outcome/Duration",
                "DSD_TA" = "Support Type")


# #read the most recent MSD from the Genie folder
# df_genie <- msd_folder %>%
#   glamr::return_latest() %>%
#   gophr::read_psd()

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
  select(-c(Code, VL_BIN, Result))

ndoh_clean_kp <- tidy_ndoh(ndoh_all_kp, kp = TRUE) %>%
  select(-c(Code, Result))

# MAP -------------------------------------------------------------------------------------------

#indicator mapping file
df_map_distinct <- googlesheets4::read_sheet(new_disagg_map_id, col_types = "c") %>%
  dplyr::rename("Test Result/Outcome/Duration" = "Test Resuts/Outcome/Duration",
                "DSD_TA" = "Support Type")

#Map dataelements and mechs - ignore RTT for now
df_mapped <- ndoh_post_processing(ndoh_clean %>% filter(!(indicator == "TX_TB_Denom" & numeratordenom == "D")),
                                  kp = FALSE, export_type = "Validation")

df_mapped_kp <- ndoh_post_processing(ndoh_clean_kp, kp = TRUE, export_type = "Validation")


#just TX_CURR and PVLS with missing sex
df_mapped %>%
  distinct() %>%
  filter(is.na(dataElement))

#just TX_CURR and PVLS with missing sex
df_mapped_kp %>%
  distinct() %>%
  filter(is.na(dataElement))

#bind together and filter out those that did not have mappings
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
                                #ndoh_arv_final %>% select(all_of(partner_vars)),
                               # tb_all_final%>% select(all_of(partner_vars)),
                                import_MATCH_prep %>% select(all_of(partner_vars))
) %>%
  mutate(period = import_period_style) %>%
  filter(!is.na(orgUnit_uid), #beatty mobile 5 and senorita hospital
         !is.na(mech_uid))

#for import file
tier_final_import <- bind_rows(df_final_clean %>% select(all_of(import_vars)),
                              # ndoh_arv_final %>% select(all_of(import_vars)),
                              # tb_all_final%>% select(all_of(import_vars)),
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
  readr::write_csv(glue::glue("{import_folder}/{fiscal_quarter}_TIER_Import_File_v1_{today}.csv"))

tier_final_partner %>%
  readr::write_csv(glue::glue("{import_folder}/{fiscal_quarter}_TIER_Import_File_v1_{today}_VERIFY.csv"))


#Partner files
Broadreach_import <- partner_import(df = tier_final_partner, "70287")
RTC_import <- partner_import(df = tier_final_partner, "70290")
ANOVA_import <- partner_import(df = tier_final_partner, "70310")
MATCH_import <- partner_import(df = tier_final_partner, "81902")
WRHI_import <- partner_import(df = tier_final_partner, "70301")





# #one missing - sex is missing for facility
# rtt_reason <- ndoh_clean_kp %>%
#   filter(indicator == "TX_RTT",
#          str_detect(`Test Result/Outcome/Duration`, "IIT")) %>%
#   map_disaggs(., "TX_RTT", "ARTNoContactReasonIIT", FALSE) %>%
#   mutate(Sex = NA,
#          CoarseAgeGroup = NA) %>%
#   relocate(CoarseAgeGroup, .before = Total) %>%
#   relocate(Sex, .before = CoarseAgeGroup) %>%
#   mutate(Sex = as.character(Sex),
#          CoarseAgeGroup = as.character(CoarseAgeGroup))
#
# # test2 <- rtt_reason %>%
# #   mutate(Sex = NA,
# #          CoarseAgeGroup = NA) %>%
# #   relocate(CoarseAgeGroup, .before = Total) %>%
# #   relocate(Sex, .before = CoarseAgeGroup) %>% names()
#
#
# rtt_cd4 <- ndoh_clean_kp %>%
#   filter(indicator == "TX_RTT",
#          !str_detect(`Test Result/Outcome/Duration`, "IIT")) %>%
#   #count(`Test Result/Outcome/Duration`)
#   map_disaggs(., "TX_RTT", "Age/Sex/CD4/HIVStatus", FALSE)

#
# d
# rbind(df_all, rtt_cd4, rtt_reason)
#
#
#
# disaggregate <- "ARTNoContactReasonIIT"
#
# test2 <- ndoh_clean %>%
#   filter(indicator == "TX_RTT",
#          str_detect(`Test Result/Outcome/Duration`, "IIT")) %>%
#   dplyr::group_by(.[,select_vars]) %>%
#   dplyr::summarise(dplyr::across(tidyselect::starts_with("Total"), sum, na.rm = TRUE), .groups = "drop") %>%
#   dplyr::left_join(df_map_distinct %>%
#                      dplyr::select(-c(unselect_vars)) %>%
#                      dplyr::filter(stringr::str_detect(dataElement, disaggregate)), by = c(group_vars)) %>%
#   dplyr::distinct() %>%
#   dplyr::left_join(mech_mfl, by = c("datim_uid" = "facilityuid"))
#
#
# #select(c("Sex", "CoarseAgeGroup")) %>%
# #  ndoh_post_processing(kp = FALSE, export_type = "Validation")
#
# tb_return <- ndoh_clean_tb %>%
#   filter(tb_disagg == "Specimen Return") %>%
#   select(-c(tb_disagg)) %>%
#   ndoh_post_processing(kp = FALSE, export_type = "Validation") %>%
#   filter(str_detect(dataElement, "Return"))
