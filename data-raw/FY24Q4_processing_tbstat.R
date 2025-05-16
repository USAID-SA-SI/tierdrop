# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  FY24Q2 DATIM Processing
# LICENSE:  MIT
# DATE:     2024-04-02
# UPDATED:Clement  & Karishma 2024-05-08
# UPDATE FOR CLEAN: 2024-12-05

# DEPENDENCIES ------------------------------------------------------------

library(glamr)
library(tidyverse)
library(gophr)
library(glue)
library(readxl)
library(googlesheets4)
#library(tierdrop)
devtools::load_all(".")

# SETUP script -----------------------------------------

#Step 1: Step up directories and standard folders
dir_setup()

#store some locals (change this to a grab metadata later as well)
get_meta("FY24Q4")


#set folderpaths (@to-do turn this into a get_metadata() function later)
#ndoh_filepath <- ndoh_folderpath %>% glamr::return_latest("joined")
ndoh_filepath_new <- ndoh_folderpath %>% glamr::return_latest("MER Reporting FY24Q4 TB STAT Revision_101224")


#check to ensure that the most recent ndoh_file is the you want to use
print(ndoh_filepath_new)


#load secrets
glamr::load_secrets()

#indicator mapping file
df_map_distinct <- googlesheets4::read_sheet(new_disagg_map_id) %>%
  dplyr::rename("Test Result/Outcome/Duration" = "Test Resuts/Outcome/Duration",
                "DSD_TA" = "Support Type")

arv_map <- googlesheets4::read_sheet(new_disagg_map_id, sheet = "ARVDISP_FY23") %>%
  mutate(RegimenCode = as.character(RegimenCode))


# #read the most recent MSD from the Genie folder
# df_genie <- msd_folder %>%
#   glamr::return_latest() %>%
#   gophr::read_psd()

# MFL ---------------------------------------------

mfl_new_df <- googlesheets4::read_sheet(mfl_fy24_id, sheet = "MFL_FY24_Q4")

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
df_fac <- clean_mfl(mfl_period = "FY24Q4") %>%
  rename(DSD_TA = dsd_ta)

#adjust TX_TB_D names for now
#names_tx_tb_d <- c(standard_names, "CoarseAgeGroup",  "Sex", "Result","Total")

# NDOH ---------------------------------------------------------------------

# #import NDOH dataframe
# ndoh_all <- import_ndoh(filepath = ndoh_filepath, qtr = curr_qtr, kp = FALSE) %>%
# filter(Facility %ni% c("fs Beatrix Clinic",
#                        "fs Harmony South Joel Occupational Health Centre",
#                        "fs Harmony South Target Occupational Health Centre",
#                        "kz Turton Mobile 4"))
#
# #ndoh_all_kp <- import_ndoh(filepath = ndoh_filepath, qtr = curr_qtr, kp = TRUE)
#
# #what facilities are in NDOH but not in MFL? ADdress MFL qc as needed
# validate_ndoh(ndoh_all)
# #validate_ndoh(ndoh_all_kp)

# FY24Qc - RUN THIS INSTEAD

ndoh_all <- import_ndoh2(filepath = ndoh_filepath_new, qtr = curr_qtr, kp = FALSE) %>%
  filter(Facility %ni% c("fs Beatrix Clinic",
                         "fs Harmony South Joel Occupational Health Centre",
                         "fs Harmony South Target Occupational Health Centre",
                         "kz Turton Mobile 4"))

#ndoh_all <- test_df
validate_ndoh(ndoh_all)

write_csv(ndoh_all, "data-raw/FY24Q4c_ndoh_all_input.csv")


# TIDY NDOH -----------------------------------------------------------

#join NDOH to MFL
ndoh_join <- df_fac %>%
  dplyr::left_join(ndoh_all,  by = c("ou5uid" = "UID"))


#Munge and clean up NDOH names
ndoh_clean <- ndoh_join %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TB_STAT_Denom" = "TB_STAT_D",
                                          "TB_STAT_Numer" = "TB_STAT_N"),
                numeratordenom = ifelse(stringr::str_detect(indicator, "_D"), "D", "N"),
                FineAgeGroup = ifelse(indicator %ni% c("TX_CURR", "TX_PVLS_D", "TX_PVLS_N", "TX_NEW", "TX_ML", "TX_RTT") & FineAgeGroup %in% c("50-54", "55-59", "60-64", "65+"),
                                      "50+", FineAgeGroup),
                indicator = dplyr::recode(indicator,"TB_STAT_N" = "TB_STAT",
                                          "TB_STAT_D" = "TB_STAT")) %>%
  dplyr::mutate(Sex = stringr::str_to_title(Sex)) %>%
  rename(SubDistrict = `Sub district`)


#address over50 disagg issue (need to reclassify age groups above 50 as "50+")
ndoh_over50 <- ndoh_clean %>%
  dplyr::filter(FineAgeGroup == "50+") %>%
  dplyr::group_by(usaid_facility, ou5uid, datim_uid, new_ou5_code, period, DSD_TA,
                  Province, District, SubDistrict, Facility, `Test Result/Outcome/Duration`,
                  Sex, FineAgeGroup, indicator, numeratordenom) %>%
  dplyr::summarise(dplyr::across(tidyselect::starts_with("Total"), sum, na.rm = TRUE), .groups = "drop")

ndoh_clean <- ndoh_clean %>%
  dplyr::filter(FineAgeGroup != "50+") %>%
  dplyr::bind_rows(ndoh_over50)


# MAP -------------------------------------------------------------------------------------------

# pre-work before mpaping because the file keeps changing

ndoh_clean <- ndoh_clean %>%
  mutate(`Test Result/Outcome/Duration` = case_when(indicator == "TB_STAT" & `Test Result/Outcome/Duration` == "Newly Tested Positive" ~ "New Positive",
                                                    indicator == "TB_STAT" & `Test Result/Outcome/Duration` == "Recently Tested Negative" ~ "Recent Negative (within last 6 weeks)",
                                                    #str_detect(indicator, "TX_TB") & `Test Result/Outcome/Duration` == "Previously on ART" ~ "Previously On ART",
                                                    TRUE ~ `Test Result/Outcome/Duration`))

ndoh_clean %>%
  count(indicator, `Test Result/Outcome/Duration`) %>% filter(str_detect(indicator, "TX_TB"))

#Previously on ART


#Map dataelements and mechs
df_mapped <- ndoh_post_processing(ndoh_clean %>%
                                    select(-c(Code)) %>%
                                    mutate(indicator=if_else(indicator=="TX_TB_Numer","TX_TB",indicator)) %>%
                                    filter(!(indicator == "TX_TB_Denom" & numeratordenom == "D")),
                                  kp = FALSE, export_type = "Validation")

df_mapped2 <- df_mapped %>%
  #select(period: numeratordenom,Sex:value) %>%
  mutate(mech_code = as.character(mech_code)) %>%
  group_by_if(is.character) %>%
  summarise(value=sum(value,na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(dataElement)) %>% janitor::get_dupes(mech_uid, dataElement_uid, categoryOptionCombo_uid, dataElement_uid) %>% View()

#' df_mapped_kp <- ndoh_post_processing(ndoh_clean_kp, kp = TRUE, export_type = "Validation") %>%
#'   #' Added a grouping value to account for 1-4 CD4 results
#'   select(period: numeratordenom,Sex:value) %>%
#'   group_by_if(is.character) %>%
#'   summarise(value=sum(value,na.rm = TRUE))

# do a check to see what is not getting mapped
#6 sites with missing sex / other disaggs for TX indicators
df_mapped2 %>%
  distinct() %>%
  filter(is.na(dataElement))

# df_mapped_kp %>%
#   distinct() %>%
#   filter(is.na(dataElement))


#bind together and filter out those that did not have mappings
df_final <- dplyr::bind_rows(df_mapped2
                             # ,
                             # df_mapped_kp
) %>%
  #distinct() %>%
  filter(!is.na(dataElement))

#10 obs for TX_NEW <1 and 1-4
df_final %>%
  janitor::get_dupes(mech_uid ,orgUnit_uid,dataElement_uid,categoryOptionCombo_uid)

# dupes2 <- df_mapped2 %>%
#   filter(!is.na(dataElement_uid)) %>%
# #  select(import_vars) %>%
#   janitor::get_dupes("mech_uid","orgUnit_uid","dataElement_uid","categoryOptionCombo_uid","value","period")


# CLEAN UP ------------------------------------------------------------

#Step 1: Filter out PrEP for Harry Gwala, Capricorn and Mopani; filter out all of MATCH PrEP for now
#filter out WC - use WC data  from nonTIER file
df_final_clean <- df_final %>%
  mutate(mech_code =as.integer(mech_code)) %>%
  filter(!(District == "kz Harry Gwala District Municipality" & indicator %in% c("PrEP_CT", "PrEP_NEW"))) %>%
  filter(Province != "wc Western Cape Province")

# BIND WITH REST ---------------------------------------------------------
#for partner review
tier_final_partner <- df_final_clean %>% select(all_of(partner_vars)) %>%
  mutate(period = import_period_style) %>%
  filter(!is.na(orgUnit_uid), #beatty mobile 5 and senorita hospital
         !is.na(mech_uid))

#for import file
tier_final_import <-  df_final_clean %>% select(all_of(import_vars)) %>%
  mutate(period = import_period_style) %>%
  filter(!is.na(orgUnit_uid), #beatty mobile 5 and senorita hospital
         !is.na(mech_uid))

#check for dupes
dupes <- tier_final_import %>%
  select(import_vars) %>%
  janitor::get_dupes(mech_uid ,orgUnit_uid,dataElement_uid,categoryOptionCombo_uid, period) %>%
  pull(orgUnit_uid)

#filter to the sites with dupes and do another group_by
tier_final_import_dupes <- tier_final_import %>%
  select(import_vars) %>%
  filter(orgUnit_uid %in% dupes) %>%
  group_by(mech_uid, orgUnit_uid, dataElement_uid, categoryOptionCombo_uid, period) %>%
  summarise(value=sum(value,na.rm = TRUE)) %>%
  ungroup()

#now bind it back
tier_final_import_joined <- tier_final_import %>%
  select(import_vars) %>%
  filter(orgUnit_uid %ni% dupes) %>%
  rbind(tier_final_import_dupes)

tier_final_import_joined %>%
  janitor::get_dupes(mech_uid ,orgUnit_uid,dataElement_uid,categoryOptionCombo_uid, period)


#EXPORT
today <- lubridate::today()
version <- "v1_TB_STAT"

tier_final_import_joined %>%
  readr::write_csv(glue::glue("{import_folder}/{fiscal_quarter}_TIER_Import_File_{version}_FINAL_{today}.csv"))



tier_final_partner %>%
  readr::write_csv(glue::glue("{import_folder}/{fiscal_quarter}_TIER_Import_File_{version}_REVIEW_{today}.csv"))


#Partner files
Broadreach_import <- partner_import(df = tier_final_partner, 70287)
RTC_import <- partner_import(df = tier_final_partner, 70290)
ANOVA_import <- partner_import(df = tier_final_partner, 70310)
ANOVA_Limpopo_import <- partner_import(df = tier_final_partner, 87577)
MATCH_import <- partner_import(df = tier_final_partner, 87576)
MATCH_KZN_import <- partner_import(df = tier_final_partner, 87575 )
WRHI_import <- partner_import(df = tier_final_partner, 70301)

