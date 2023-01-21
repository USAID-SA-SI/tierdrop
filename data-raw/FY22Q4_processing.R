# AUTHOR:   K. Srikanth | USAID
# PURPOSE:
# REF ID:   94e4a2bc
# LICENSE:  MIT
# DATE:     2022-10-23
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


#set folderpaths
ndoh_folderpath <- 'data-raw/NDOH'
reference_folder <- "data-raw/Reference Files"
msd_folder <- "data-raw/MSD-Genie"
import_folder <- "data-raw/Import Files"
ndoh_filepath <- ndoh_folderpath %>% glamr::return_latest()
msd_filepath <- msd_folder %>% glamr::return_latest()

#store some locals
fiscal_quarter <- "FY22Q4"
curr_qtr <- "Q4"

#check to ensure that the most recent ndoh_file is the you want to use
print(ndoh_filepath)
print(msd_filepath)

#load secrets
glamr::load_secrets()

misaligned_sites <-  c("mp Naas CHC",
                       "mp Rockdale CHC",
                       "ec Matatiele Community Clinic",
                       "kz Empangeni Clinic",
                       "kz Khandisa Clinic",
                       "kz Luwamba Clinic",
                       "kz Port Shepstone Mobile 3")

import_vars <- c("mech_uid", "orgUnit_uid",	"dataElement_uid",
                 "categoryOptionCombo_uid",	"value",	"period")

validation_vars <- c("period","Province", "District","SubDistrict", "Facility",
                     "orgUnit_uid", "mech_code", "mech_uid", 'mech_name',
                     "indicator", "numeratordenom", "Sex",
                     "CoarseAgeGroup", "Result","dataElement", "dataElement_uid", "categoryOptionComboName",
                     "categoryOptionCombo_uid", "value")

remove_sites <- reference_folder %>%
  return_latest("FY22Q4-Facility-Change.xlsx") %>%
  read_excel(sheet = "filter")

rename_sites <- reference_folder %>%
  return_latest("FY22Q4-Facility-Change.xlsx") %>%
  read_excel(sheet = "rename")


# MFL --------------------------------------------------------

q4_id <- "1G2u0Rw0EgsExfPDjKIXzzB31OLzAVhkdPb5yz1lOauU"

#Load MFL as is
mfl_new_df <- googlesheets4::read_sheet(q4_id, sheet = "MFL_FY22Q4")

#tidy MFL
df_fac <- mfl_new_df %>%
  dplyr::filter(!is.na(OU2name)) %>%
  janitor::clean_names() %>%
  dplyr::select(ou5name, ou5uid, datim_uid, new_ou5_code, tidyselect::starts_with("fy22")) %>%
  tidyr::pivot_longer(cols = tidyselect::starts_with("fy22"), names_to = "period", values_to = "DSD_TA") %>%
  dplyr::mutate(period = stringr::str_sub(period, start = 1, end = 6) %>% toupper(),
                new_ou5_code = as.character(new_ou5_code),
                DSD_TA = ifelse(DSD_TA == "DSD+Roving TA", "DSD", DSD_TA)) %>%
  dplyr::rename(usaid_facility = ou5name,
                # mech_code = mechanism_i_d,
                # mech_uid = mechanism_uid,
                # mech_name = partner
                ) %>%
  filter(!is.na(new_ou5_code),
         !(usaid_facility == "kz Mvoti Clinic" & is.na(DSD_TA))) %>%
  mutate(usaid_facility = recode(usaid_facility, "lp Matsotsosela Clinic" = "lp Matsotsosela clinic"))


df_fac


# NDOH ---------------------------------------------------------------------

#use version on q4-update branch
ndoh_all <- import_ndoh(filepath = ndoh_filepath, qtr = "Q4", kp = FALSE)
ndoh_all_kp <- import_ndoh(filepath = ndoh_filepath, qtr = "Q4", kp = TRUE)

#validate - check NDOH/MFL triangulation
# TODO: update validate_ndoh()

#what facilities are in NDOH but not in MFL? All correctional facilities
ndoh_code <- unique(ndoh_all$Code)
kp_code <- unique(ndoh_all_kp$Code)
mfl_code <- unique(df_fac$new_ou5_code)
code_list <- setdiff(ndoh_code, mfl_code)
kp_code_list <- setdiff(kp_code, mfl_code)


NDOH_MFL_triangulation <- ndoh_all %>% dplyr::filter(Code %in% code_list) %>%
  dplyr::distinct(Facility, Code) %>%
  dplyr::pull(Facility)

#
# ndoh_all %>%
#   dplyr::left_join(mfl_new_df %>%
#                      dplyr::mutate(OU5name = recode(OU5name, "lp Matsotsosela Clinic" = "lp Matsotsosela clinic")) %>%
#                      dplyr::select(OU5name, `New_OU5 Code`), by = c("Facility" = "OU5name"))

#no facilities in filter out
filter_out <- remove_sites %>% distinct() %>% pull()
rename_old <- rename_sites %>% distinct(old_name) %>% pull()
rename_new <- rename_sites %>% distinct(new_name) %>% pull()

rename <- rename_sites %>% distinct(old_name) %>% pull()
ndoh_all %>% filter(Facility %in% filter_out)
ndoh_all %>% filter(Facility %in% rename_old) %>% distinct(Facility) %>% pull()

ndoh_rename <- ndoh_all %>%
  left_join(rename_sites, by = c("Facility" = "old_name")) %>%
  mutate(Facility = ifelse(!is.na(new_name), new_name, Facility),
         Code = ifelse(!is.na(new_name), new_code, Code))

ndoh_rename_old <- ndoh_rename %>%
  filter(Facility %ni% rename_new)

ndoh_rename_new <- ndoh_rename %>%
  filter(Facility %in% rename_new) %>%
  group_by(Province, District, SubDistrict, Facility, Code,
           `Test Result/Outcome/Duration`, Sex, CoarseAgeGroup, Result, indicator) %>%
    summarise(across(c(starts_with("Total")), sum, na.rm = TRUE), .groups = "drop")

ndoh_all <- bind_rows(ndoh_rename_old, ndoh_rename_new) %>%
  select(-c(new_name, new_code))

# #no facilities in filter out
# filter_out <- remove_sites %>% distinct() %>% pull()
# rename <- rename_sites %>% distinct(old_name) %>% pull()
# ndoh_all_kp %>% filter(Facility %in% filter_out)
# ndoh_all_kp %>% filter(Facility %in% rename) %>% distinct(Facility) %>% pull()
#
# ndoh_all %>%
#   left_join(rename_sites, by = c("Facility" = "old_name")) %>%
#   mutate(Facility = ifelse(!is.na(new_name), new_name, Facility)) %>%
#   select(-c(new_name, new_uid))

#TIDY
ndoh_clean <- tidy_ndoh(ndoh_all, kp = FALSE)
ndoh_clean_kp <- tidy_ndoh(ndoh_all_kp, kp = TRUE)

# MAP
df_mapped <- ndoh_post_processing(ndoh_clean %>% filter(!(indicator == "TX_TB" & numeratordenom == "D")), kp = FALSE, export_type = "Validation")
df_mapped_kp <- ndoh_post_processing(ndoh_clean_kp, kp = TRUE, export_type = "Validation")

#Check
df_final %>%
  select(import_vars) %>%
  janitor::get_dupes()

  #filter(is.na(dataElement)) %>% view()
  count(Facility)

#bind together
df_final <- dplyr::bind_rows(df_mapped,
                             df_mapped_kp) %>%
  filter(!is.na(dataElement)) %>%
  select(import_vars)


  #TX_TB ---------------------------------------------------------

#clean up the NDOH variable names and tidy df

ndoh_tb <- ndoh_all  %>%
  filter(indicator %in% c("TX TB_D", "TX TB_D_TestType", "TX TB_D_Pos", "TX TB_D_TestType"))

ndoh_join_tb <- df_fac %>%
  dplyr::left_join(ndoh_tb,  by = c("new_ou5_code" = "Code"))

#Munge and clean up NDOH names
ndoh_clean_tb <- ndoh_join_tb %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX TB_D" = "TX_TB_D",
                                          "TX TB_D_Pos" = "TX_TB_Pos_D",
                                          "TX TB_D_TestType" = "TX_TB_TestType_D"),
                numeratordenom = ifelse(stringr::str_detect(indicator, "_D"), "D", "N"),
                CoarseAgeGroup = ifelse(indicator != "TX_CURR" & CoarseAgeGroup %in% c("50-54", "55-59", "60-64", "65+"),
                                        "50+", CoarseAgeGroup),
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
  select(-c(tb_disagg)) %>%
  ndoh_post_processing(kp = FALSE, export_type = "Validation")

tb_return <- ndoh_clean_tb %>%
  filter(tb_disagg == "Specimen Return") %>%
  select(-c(tb_disagg)) %>%
  ndoh_post_processing(kp = FALSE, export_type = "Validation") %>%
  filter(str_detect(dataElement, "Return"))

tb_testtype <- ndoh_clean_tb %>%
  filter(tb_disagg == "Specimen Sent Total") %>%
  select(-c(tb_disagg)) %>%
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


import_vars <- c("mech_uid", "orgUnit_uid",	"dataElement_uid",
                 "categoryOptionCombo_uid",	"value",	"period")

validation_vars <- c("period","Province", "District","SubDistrict", "Facility",
               "orgUnit_uid", "mech_code", "mech_uid", 'mech_name',
               "indicator", "numeratordenom", "Sex",
               "CoarseAgeGroup", "Result","dataElement", "dataElement_uid", "categoryOptionComboName",
               "categoryOptionCombo_uid", "value")

tb_sent_map <- tb_sent %>%
  dplyr::left_join(df_map_distinct %>%
                    # rename(DSD_TA = `Support Type`) %>%
                     dplyr::select(-c(`Test Result/Outcome/Duration`)) %>%
                     dplyr::filter(stringr::str_detect(dataElement, "Specimen Sent/HIVStatus")), by = c(group_vars)) %>%
  dplyr::distinct() %>%
  dplyr::left_join(mech_df, by = c("datim_uid" = "facilityuid")) %>%
  rename(mech_name = prime_partner_name,
         value = Total,
         orgUnit_uid = datim_uid) %>%
  select(import_vars)


tb_all <- bind_rows(tb_age_sex, tb_return, tb_testtype) %>%
  filter(!is.na(dataElement)) %>%
  select(import_vars)

tb_all_final <- bind_rows(tb_all, tb_sent_map)
# # select(-c(tb_disagg)) %>%
#  ndoh_post_processing(kp = FALSE, export_type = "Validation")

use_q3_txcurr <- c("kz Marburg Clinic",
                   "kz KwaMbunde Clinic",
                   "mp Kameelpoortnek Clinic",
                   "gp Charlotte Maxeke Hospital",
                   "lp Maphalle Clinic",
                   "lp Shotong Clinic")

#filter out Harry Gwala for PrEP and BRCH facilities for TX_CURR q4
df_final <- df_final %>%
  filter(!(District == "kz Harry Gwala District Municipality" & indicator %in% c("PrEP_CT", "PrEP_NEW"))) %>%
  filter(!(Facility %in% use_q3_txcurr & indicator == "TX_CURR")) %>%
  select(import_vars)


# BIND WITH REST
tier_final_consolidated <- bind_rows(df_final, tb_all_final)

tier_final_consolidated %>%
  select(import_vars) %>%
  janitor::get_dupes()

today <- lubridate::today()

tier_final_consolidated %>%
  readr::write_csv(glue::glue("{import_folder}/{fiscal_quarter}_TIER_Import_File_v2_{today}.csv"))

#broadreach facility - issues



#Partner files
Broadreach_import <- partner_import(df = tier_final_consolidated, 70287)
RTC_import <- partner_import(df = tier_final_consolidated, 70290)
ANOVA_import <- partner_import(df = tier_final_consolidated, 70310)
MATCH_import <- partner_import(df = tier_final_consolidated, 81902)
WRHI_import <- partner_import(df = tier_final_consolidated, 70301)


  #ARVDISP --------------------------------------------------------------

#ARVDISP - use Given's file -----------------------------------------------------

#MER mapping file
arvdisp_mapping <- reference_folder %>%
  return_latest("RTC_DATIM MER TIER Results Consolidated_Workfile.xlsx") %>%
  read_xlsx(sheet= "ARVDispense")

df_arvdisp <- readxl::read_excel(ndoh_filepath, sheet = "ARVDISP")

#now, filter to usaid districts and pull facilities with 4 digit codes
df_arvdisp <- df_arvdisp %>%
  filter(District %in% usaid_dsp_district) %>%
  mutate(code_num = str_length(Code)) %>%
  group_by(Province, District, SubDistrict, Facility) %>%
  arrange(desc(code_num)) %>%
  left_join(mfl_new_df %>%
              filter(
                !(OU5name == "kz Mvoti Clinic" & is.na(`Old_OU5Code`))) %>%
              dplyr::mutate(OU5name = recode(OU5name, "lp Matsotsosela Clinic" = "lp Matsotsosela clinic")) %>%
              dplyr::select(OU5name, `New_OU5 Code`), by = c("Facility" = "OU5name")) %>%
  mutate(`New_OU5 Code` = as.character(`New_OU5 Code`),
         Code = as.character(Code),
         Code = ifelse(code_num < 7, `New_OU5 Code`, Code),
         Code = ifelse(Facility %in% misaligned_sites, `New_OU5 Code`, Code)) %>%
  fill(Code) %>%
  ungroup() %>%
  # count(Facility, Code) %>% view()
  select(-c(code_num, `New_OU5 Code`)) %>%
  mutate(Code = ifelse(Facility == "lp Naledi Clinic", 383651, Code),
    Code = as.character(Code))

#no facilities in filter out
filter_out <- remove_sites %>% distinct() %>% pull()
rename_old <- rename_sites %>% distinct(old_name) %>% pull()
rename_new <- rename_sites %>% distinct(new_name) %>% pull()

rename <- rename_sites %>% distinct(old_name) %>% pull()
df_arvdisp %>% filter(Facility %in% filter_out)
df_arvdisp %>% filter(Facility %in% rename_old) %>% distinct(Facility) %>% pull()

ndoh_rename_arv <- df_arvdisp %>%
  left_join(rename_sites, by = c("Facility" = "old_name")) %>%
  mutate(Facility = ifelse(!is.na(new_name), new_name, Facility),
         Code = ifelse(!is.na(new_name), new_code, Code))

ndoh_rename_old_arv <- ndoh_rename_arv %>%
  filter(Facility %ni% rename_new)

ndoh_rename_new_arv <- ndoh_rename_arv %>%
  filter(Facility %in% rename_new) %>%
  group_by(Province, District, SubDistrict, Facility, Code,
           Sex, CoarseAgeGroup, RegimenCode, Packs) %>%
  summarise(across(c(starts_with("Total")), sum, na.rm = TRUE), .groups = "drop")

df_arvdisp <- bind_rows(ndoh_rename_old_arv, ndoh_rename_new_arv) %>%
  select(-c(new_name, new_code))

#join NDOH to MFL
join_arvdisp <- df_fac %>%
  left_join(df_arvdisp,  by = c("new_ou5_code" = "Code"))

#CHECK******
#what facilities are in NDOH but not in MFL?
arvdisp_code <- unique(df_arvdisp$Code)
mfl_code <- unique(df_fac$new_ou5_code)
arv_code_list <- setdiff(arvdisp_code, mfl_code)

df_arvdisp %>% filter(Code %in% arv_code_list) %>%
  distinct(Facility) %>%
  pull()

df_arv_clean <- join_arvdisp %>%
  mutate(indicator = "SC_ARVDISP",
         RegimenCode = recode(RegimenCode, "13.0" = "13"))

#create indicator and N/D variable in mapping file
arv_map_clean <- arvdisp_mapping %>%
  # count(`Datim UID`) %>%
  mutate(indicator = str_extract(dataElement, "[^ (]+")) %>%
  select(-c(Packs))

#now map
arv_map_distinct <- arv_map_clean %>% distinct(CoarseAgeGroup, RegimenCode, indicator,
                                               dataElement, dataElement_uid, categoryOptionComboName, categoryOptionCombo_uid)

# now, map in the additional regimen code

arv_additional_map <- reference_folder %>%
  return_latest("Dr Molapo_missing-regimen-code-FY22Q4_final") %>%
  read_excel() %>%
  select(dataelementname, dataelement_uid,
         Category, categoryoptioncombo_uid,`Missing RegimenCode`, CoarseAgeGroup) %>%
  mutate(indicator = "SC_ARVDISP") %>%
  rename(dataElement = dataelementname,
         dataElement_uid  = dataelement_uid,
         categoryOptionComboName = Category,
         categoryOptionCombo_uid =  categoryoptioncombo_uid,
         RegimenCode= `Missing RegimenCode`)

arv_map_distinct <- bind_rows(arv_map_distinct, arv_additional_map)


df_arv_mapped <- df_arv_clean %>%
  left_join(arv_map_distinct, by = c("CoarseAgeGroup", "RegimenCode", "indicator"))


#NOT MAPPED ---
arv_not_mapped <- df_arv_mapped %>% filter(is.na(dataElement), !is.na(Packs)) %>%
  select(indicator, DSD_TA, District, Facility, CoarseAgeGroup, RegimenCode, dataElement)

write_csv(arv_not_mapped, "Dataout/arv-distinct-missing-disaggs.csv")

df_arv_final <-  df_arv_mapped %>%
  left_join(mech_df, by = c("datim_uid" = "facilityuid"))
# %>%view()
#   left_join(mech_xwalk, by = c('mech_code')) %>%
#   select(-c(mech_name.y)) %>%
#   rename(mech_name = mech_name.x)

my_arv_validation <- df_arv_final %>%
  select(period,Province, District, Community, Facility, datim_uid,
         mech_name, mech_code, mech_uid, primepartner, indicator,
         CoarseAgeGroup, RegimenCode,
         dataElement, dataElement_uid, categoryOptionComboName, categoryOptionCombo_uid, Packs) %>%
  rename(SubDistrict = Community,
         value = Packs,
         orgUnit_uid = datim_uid)

import_file_clean_arv <- df_arv_final %>%
  select(mech_code, mech_uid, Facility, datim_uid, dataElement , dataElement_uid, categoryOptionCombo_uid,
         Packs, SubDistrict, District) %>%
  mutate(period = fiscal_quarter) %>%
  rename(value = Packs,
         #SubDistrict = Community,
         #mech_uid = mechanism_uid,
         orgUnit_uid = datim_uid,
         orgUnitName = Facility) %>%
  filter(!is.na(dataElement_uid)) %>%
  select(import_vars)

# EXPORT ----------------------------------------------

tier_final <- tier_final_consolidated %>%
  select(import_vars) %>%
  rbind(import_file_clean_arv)

tier_final %>%
  readr::write_csv(glue::glue("{import_folder}/{fiscal_quarter}_TIER_Import_File_v3_{today}.csv"))



# #filter out Q4 tx_curr
# tier_final_consolidated <- tier_final_consolidated %>%
#   filter(!(Facility %in% use_q3_txcurr & indicator == "TX_CURR"))

#Partner files
Broadreach_import <- partner_import(df = tier_final, 70287)
RTC_import <- partner_import(df = tier_final, 70290)
ANOVA_import <- partner_import(df = tier_final, 70310)
MATCH_import <- partner_import(df = tier_final, 81902)
WRHI_import <- partner_import(df = tier_final, 70301)

# remove TB_ART from everyone except BRCH and MATCH

RTC_import %>%
  filter((dataElement_uid %in% c("Qc1AaYpKsjs", "Szuf9YjHjTL")))
