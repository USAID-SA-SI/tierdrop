# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  FY24Q2 DATIM Processing
# LICENSE:  MIT
# DATE:     2024-04-02
# UPDATED:Clement  & Karishma

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
get_meta("FY24Q2")


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

mfl_new_df <- googlesheets4::read_sheet(mfl_fy24_id, sheet = "MFL_FY24_Q2")

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
df_fac <- clean_mfl(mfl_period = "FY24Q2") %>%
  rename(DSD_TA = dsd_ta)

#adjust TX_TB_D names for now
#names_tx_tb_d <- c(standard_names, "CoarseAgeGroup",  "Sex", "Result","Total")

# NDOH ---------------------------------------------------------------------

#FIX TB_STAT

#if this breaks, check on tab names (adjust PrEP_NEW in the file itself)
# update for ou5uid
ndoh_all <- import_ndoh(filepath = ndoh_filepath, qtr = curr_qtr, kp = FALSE)
sheet_namesndoh_all_kp <- import_ndoh(filepath = ndoh_filepath, qtr = curr_qtr, kp = TRUE)

#what facilities are in NDOH but not in MFL? ADdress MFL qc as needed
# update for ou5uid
validate_ndoh(ndoh_all)
validate_ndoh(sheet_namesndoh_all_kp)

# TIDY NDOH -----------------------------------------------------------

#TIDY
ndoh_clean <- tidy_ndoh(ndoh_all, kp = FALSE) %>%
  select(-c(Code, VL_BIN))


ndoh_pvls <- ndoh_clean %>%
  filter(indicator == "TX_PVLS",
         numeratordenom == "N") %>%
  group_by(usaid_facility, ou5uid, datim_uid, new_ou5_code, period, DSD_TA, Province, SubDistrict, District, Facility, `Test Result/Outcome/Duration`,
           Sex, CoarseAgeGroup, indicator, numeratordenom) %>%
  dplyr::summarise(dplyr::across(tidyselect::starts_with("Total"), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

ndoh_clean <- ndoh_clean %>%
  filter(!(indicator == "TX_PVLS" & numeratordenom == "N")) %>%
  bind_rows(ndoh_pvls)


#KP
ndoh_clean_kp <- tidy_ndoh(sheet_namesndoh_all_kp, kp = TRUE) %>%
  select(-c(Code, Result))

ndoh_clean_kp_pvls <- ndoh_clean_kp %>%
  filter(indicator == "TX_PVLS",
         numeratordenom == "N") %>%
  group_by(usaid_facility, ou5uid, datim_uid, new_ou5_code, period, DSD_TA, Province, SubDistrict, District, Facility, `Test Result/Outcome/Duration`,
           Sex, CoarseAgeGroup, indicator, numeratordenom) %>%
  dplyr::summarise(dplyr::across(tidyselect::starts_with("Total"), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

ndoh_clean_kp <- ndoh_clean_kp %>%
  filter(!(indicator == "TX_PVLS" & numeratordenom == "N")) %>%
  bind_rows(ndoh_clean_kp_pvls)


#### TB Indicators


#clean up the NDOH variable names and tidy df

ndoh_tb <- ndoh_all  %>%
  filter(indicator %in% c("TX_TB_Denom", "TX_TB_Denom_TestType", "TX_TB_Denom_Pos"))

ndoh_join_tb <- df_fac %>%
  tidylog::left_join(ndoh_tb,  by = c("ou5uid" = "UID")) %>%
  select(-c(Code))

ndoh_clean_tb <- ndoh_join_tb %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_TB_Denom" = "TX_TB_D",
                                          "TX_TB_Denom_Pos" = "TX_TB_Pos_D",
                                          "TX_TB_Denom_TestType" = "TX_TB_TestType_D"),
                numeratordenom = ifelse(stringr::str_detect(indicator, "_D"), "D", "N"),
                               tb_disagg = case_when(indicator == "TX_TB_D" ~ "Age/Sex/TBScreen",
                                      indicator == "TX_TB_Pos_D" ~ "Specimen Return",
                                      indicator == "TX_TB_TestType_D" ~ "Specimen Sent Total"),
                indicator = dplyr::recode(indicator, "TB_PREV_D" = "TB_PREV",
                                          "TB_PREV_N" = "TB_PREV",
                                          "TB_STAT_N" = "TB_STAT",
                                          "TB_STAT_D" = "TB_STAT",
                                          "TX_PVLS_D" = "TX_PVLS",
                                          "TX_PVLS_N" = "TX_PVLS",
                                          "TX_TB_N" = "TX_TB",
                                          "TX_TB_D" = "TX_TB",
                                          "TX_TB_Pos_D" = "TX_TB",
                                          "TX_TB_TestType_D" ="TX_TB"))

#one missing - sex is missing for facility
tb_age_sex <- ndoh_clean_tb %>%
  filter(tb_disagg == "Age/Sex/TBScreen") %>%
  select(-c(tb_disagg,CD4,VL_BIN)) %>%
  ndoh_post_processing(kp = FALSE, export_type = "Validation")

tb_return <- ndoh_clean_tb %>%
  filter(tb_disagg == "Specimen Return") %>%
  select(-c(tb_disagg,CD4,VL_BIN)) %>%
  ndoh_post_processing(kp = FALSE, export_type = "Validation") %>%
  filter(str_detect(dataElement, "Return"))

tb_testtype <- ndoh_clean_tb %>%
  filter(tb_disagg == "Specimen Sent Total") %>%
  select(-c(tb_disagg,CD4,VL_BIN)) %>%
  ndoh_post_processing(kp = FALSE, export_type = "Validation")

tb_sent <- ndoh_clean_tb %>%
  filter(tb_disagg == "Specimen Sent Total") %>%
  dplyr::group_by(usaid_facility, ou5uid, datim_uid, new_ou5_code, period, DSD_TA,
                  Province, District, SubDistrict, Facility,
                  Sex, CoarseAgeGroup, Result, indicator, numeratordenom) %>%
  dplyr::summarise(dplyr::across(tidyselect::starts_with("Total"), sum, na.rm = TRUE), .groups = "drop")

#grab column names for NDOH
col_names <- tb_sent %>%
  names()

col_names <- col_names[col_names %ni% c("Total")]
group_vars <- c("Sex", "CoarseAgeGroup", "Result", "DSD_TA")
group_vars <- col_names[col_names %ni% c("usaid_facility", "ou5uid", "datim_uid",
                                         'new_ou5_code', 'period', 'Province', 'District',
                                         'SubDistrict', 'Facility')]

tb_sent_map <- tb_sent %>%
  dplyr::left_join(df_map_distinct %>%
                     # rename(DSD_TA = `Support Type`) %>%
                     dplyr::select(-c(`Test Result/Outcome/Duration`)) %>%
                     dplyr::filter(stringr::str_detect(dataElement, "Specimen Sent/HIVStatus")), by = c(group_vars)) %>%
  dplyr::distinct() %>%
  dplyr::left_join(mech_mfl, by = c("datim_uid" = "facilityuid")) %>%
  rename(mech_name = prime_partner_name,
         value = Total,
         orgUnit_uid = datim_uid) %>%
  select(validation_vars)

#Remove the Result from the Validation_vars for the code below to Run.
validation_vars <- validation_vars[validation_vars != "Result"]


tb_all <- bind_rows(tb_age_sex, tb_return, tb_testtype) %>%
  filter(!is.na(dataElement)) %>%
  select(validation_vars)

tb_all_final <- bind_rows(tb_all, tb_sent_map)


# MAP -------------------------------------------------------------------------------------------

#Map dataelements and mechs
df_mapped <- ndoh_post_processing(ndoh_clean %>%
                                    mutate(indicator=if_else(indicator=="TX_TB_Numer","TX_TB",indicator)) %>%
                                    filter(!(indicator == "TX_TB_Denom" & numeratordenom == "D")),
                                  kp = FALSE, export_type = "Validation")

df_mapped_kp <- ndoh_post_processing(ndoh_clean_kp, kp = TRUE, export_type = "Validation")

# do a check to see what is not getting mapped
  #6 sites with missing sex / other disaggs for TX indicators
df_mapped %>%
  distinct() %>%
  filter(is.na(dataElement))

df_mapped_kp %>%
  distinct() %>%
  filter(is.na(dataElement))


#bind together and filter out those that did not have mappings
df_final <- dplyr::bind_rows(df_mapped,
                             df_mapped_kp) %>%
  #distinct() %>%
  filter(!is.na(dataElement)) %>%
  mutate(mech_code = as.character(mech_code))

df_final %>%
  janitor::get_dupes()


# ARVDISP ------------------------------------------------------------

#check for facilities in but not in MFL
#import_arvdisp(ndoh_filepath) %>% validate_ndoh()

#import arvdisp tab + tidy / map disaggs (and condense / aggregate to dataElement / categoryoptionCombo)
  #note: this function may take some time to run
#ndoh_arvdisp <- import_arvdisp(ndoh_filepath) %>% tidy_map_arvdisp()

#partner review file format
# ndoh_arv_final <- ndoh_arvdisp %>%
#   rename(
#     value = Packs,
#     orgUnit_uid = datim_uid) %>%
#   filter(!is.na(dataElement_uid),
#          !is.na(mech_uid),
#          !is.na(orgUnit_uid)) %>%
#   select(partner_vars)
#
# #import file format
# ndoh_arv_final %>%
#   select(import_vars)

# CLEAN UP ------------------------------------------------------------

#Step 1: Filter out PrEP for Harry Gwala, Capricorn and Mopani; filter out all of MATCH PrEP for now
df_final_clean <- df_final %>%
  mutate(mech_code =as.integer(mech_code)) %>%
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
  mutate(period = import_period_style,
         mech_code=as.integer(mech_code))


# BIND WITH REST ---------------------------------------------------------
#for partner review
tier_final_partner <- bind_rows(df_final_clean %>% select(all_of(partner_vars))%>%
select(all_of(partner_vars)),
tb_all_final%>% select(all_of(partner_vars)),
import_MATCH_prep %>% select(all_of(partner_vars))) %>%
  mutate(period = import_period_style) %>%
  filter(!is.na(orgUnit_uid), #beatty mobile 5 and senorita hospital
         !is.na(mech_uid))

#for import file
tier_final_import <- bind_rows(df_final_clean %>% select(all_of(import_vars)),
                               tb_all_final%>% select(all_of(import_vars)),
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
  readr::write_csv(glue::glue("{import_folder}/{fiscal_quarter}_TIER_Import_File_v2_{today}.csv"))


#Partner files
Broadreach_import <- partner_import(df = tier_final_partner, 70287)
RTC_import <- partner_import(df = tier_final_partner, 70290)
ANOVA_import <- partner_import(df = tier_final_partner, 70310)
ANOVA_Limpopo_import <- partner_import(df = tier_final_partner, 87577)
MATCH_import <- partner_import(df = tier_final_partner, 87576)
MATCH_KZN_import <- partner_import(df = tier_final_partner, 87575 )
WRHI_import <- partner_import(df = tier_final_partner, 70301)


