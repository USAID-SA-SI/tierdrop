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
fiscal_quarter <- "FY23Q2"
import_period_style <- "2023Q1"
curr_qtr <- "Q2"
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

# MFL ---------------------------------------------

mfl_new_df <- googlesheets4::read_sheet(mfl_fy23_id, sheet = "MFL_FY23")

df_fac <- clean_mfl() %>%
  rename(DSD_TA = dsd_ta)

df_fac <- df_fac %>%
  mutate(new_ou5_code = case_when(usaid_facility ==
                                    "ec Matatiele Community Clinic" ~ "1870684",
                                  usaid_facility ==
                                    "kz Empangeni Clinic" ~ "4348504",
                                  usaid_facility ==
                                    "kz Khandisa Clinic" ~ "4364152",
                                  usaid_facility ==
                                    "kz Luwamba Clinic" ~ "4527574",
                                  # usaid_facility ==
                                  #   "kz Mvutshini Clinic (uMlalazi)" ~ "442706",
                                  usaid_facility ==
                                    "lp Naledi Clinic" ~ "5217420",
                                  usaid_facility ==
                                    "mp Naas CHC" ~ "6189131",
                                  usaid_facility ==
                                    "mp Rockdale CHC" ~ "6258774",
                                  TRUE ~ new_ou5_code))

#read the most recent MSD from the Genie folder
df_genie <- msd_folder %>%
  glamr::return_latest() %>%
  gophr::read_psd()


#first, let's pull down all the mech code / mech uid information from DATIM
# Note - this may take a few minutes to pull in

mechs <- pull_mech_uid(ou_sel = "South Africa")

# just for now because of wifi issues

mechs <- glamr::si_path() %>%
  glamr::return_latest("mechs") %>%
  readr::read_csv()

mechs <- mechs %>%
  dplyr::mutate(mech_code = as.character(mech_code))

mech_df <- grab_mech_data(mech_df = mechs, msd_df = df_genie, curr_fy= 2023, extra_mechs = FALSE)


# NDOH ---------------------------------------------------------------------

#if this breaks, check on tab names
ndoh_all <- import_ndoh(filepath = ndoh_filepath, qtr = "Q2", kp = FALSE)
ndoh_all_kp <- import_ndoh(filepath = ndoh_filepath, qtr = "Q2", kp = TRUE)

#address duplicate codes in NDOH file manually
ndoh_all <- ndoh_all %>%
  mutate(Code = case_when(Facility ==
                                    "ec Matatiele Community Clinic" ~ "1870684",
                          Facility ==
                                    "kz Empangeni Clinic" ~ "4348504",
                          Facility ==
                                    "kz Khandisa Clinic" ~ "4364152",
                          Facility ==
                                    "kz Luwamba Clinic" ~ "4527574",
                                  # usaid_facility ==
                                  #   "kz Mvutshini Clinic (uMlalazi)" ~ "442706",
                          Facility ==
                                    "lp Naledi Clinic" ~ "5217420",
                          Facility ==
                                    "mp Naas CHC" ~ "6189131",
                          Facility ==
                                    "mp Rockdale CHC" ~ "6258774",
                                  TRUE ~ Code))


#validate - check NDOH/MFL triangulation
# TODO: update validate_ndoh()

#what facilities are in NDOH but not in MFL? ADdress MFL qc as needed
ndoh_code <- unique(ndoh_all$Code)
kp_code <- unique(ndoh_all_kp$Code)
mfl_code <- unique(df_fac$new_ou5_code)
code_list <- setdiff(ndoh_code, mfl_code)
kp_code_list <- setdiff(kp_code, mfl_code)


# all correctional sites after manual mutates
missing_sites <- ndoh_all %>%
  filter(Code %in% code_list) %>% distinct(Facility) %>% pull()

flag_sites <-df_fac %>%
  filter(usaid_facility %in% missing_sites) %>%
  distinct(usaid_facility, new_ou5_code) %>%
  arrange(usaid_facility) %>%
  pull(usaid_facility)

ndoh_all %>%
  filter(Facility %in% flag_sites) %>%
  distinct(Facility, Code) %>%
  arrange(Facility)

missing_sites_kp <- ndoh_all_kp %>%
  filter(Code %in% kp_code_list) %>% distinct(Facility) %>% pull()


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
df_mapped <- ndoh_post_processing(ndoh_clean %>% filter(!(indicator == "TX TB_Denom" & numeratordenom == "D")),
                                  kp = FALSE, export_type = "Validation")

df_mapped_kp <- ndoh_post_processing(ndoh_clean_kp, kp = TRUE, export_type = "Validation")


#bind together
df_final <- dplyr::bind_rows(df_mapped,
                             df_mapped_kp) %>%
  distinct() %>%
  filter(!is.na(dataElement))

df_final %>%
  janitor::get_dupes()


#TX_TB ---------------------------------------------------------

#clean up the NDOH variable names and tidy df

ndoh_tb <- ndoh_all  %>%
  filter(indicator %in% c("TX TB_Denom", "TX TB_Denom_TestType", "TX TB_Denom_Pos", "TX TB_Denom_TestType"))

ndoh_join_tb <- df_fac %>%
  tidylog::left_join(ndoh_tb,  by = c("new_ou5_code" = "Code"))

#Munge and clean up NDOH names
ndoh_clean_tb <- ndoh_join_tb %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX TB_Denom" = "TX_TB_D",
                                          "TX TB_Denom_Pos" = "TX_TB_Pos_D",
                                          "TX TB_Denom_TestType" = "TX_TB_TestType_D"),
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
  select(all_of(validation_vars))


tb_all <- bind_rows(tb_age_sex, tb_return, tb_testtype) %>%
  filter(!is.na(dataElement)) %>%
  select(validation_vars)

tb_all_final <- bind_rows(tb_all, tb_sent_map)


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
      #select(import_vars) %>%
      mutate(period = recode(period, "FY23Q2" = "2023Q1"))

# BIND WITH REST ---------------------------------------------------------
    #for partner review
    tier_final_partner <- bind_rows(df_final_clean %>% select(partner_vars),
                                         tb_all_final%>% select(partner_vars),
                                         import_MATCH_prep %>% select(partner_vars)
                                    # ,
                                    #      import_file_clean_arv_agg %>% select(partner_vars)
                                    ) %>%
      mutate(period = recode(period, "FY23Q2" = "2023Q1")) %>%
      filter(!is.na(orgUnit_uid), #beatty mobile 5 and senorita hospital
             !is.na(mech_uid))

    #for import file
    tier_final_import <- bind_rows(df_final_clean %>% select(import_vars),
                                    tb_all_final%>% select(import_vars),
                                    import_MATCH_prep %>% select(import_vars)
                                   # ,
                                   #  import_file_clean_arv_agg %>% select(import_vars)
                                   ) %>%
      mutate(period = recode(period, import_period_style)) %>%
      filter(!is.na(orgUnit_uid), #beatty mobile 5 and senorita hospital
             !is.na(mech_uid))


    #for TX RTT and ML
    df_final %>%
      filter(indicator %in% c("TX_ML", "TX_RTT")) %>%
      select(import_vars) %>%
      mutate(period = recode(period, "FY23Q2" = "2023Q1")) %>%
      filter(!is.na(orgUnit_uid), #beatty mobile 5 and senorita hospital
             !is.na(mech_uid)) %>%
      readr::write_csv(glue::glue("{import_folder}/{fiscal_quarter}_TX_RTT_ML_Import_File_v1_{today}.csv"))


    tier_final_import %>%
      select(import_vars) %>%
      janitor::get_dupes()

    #EXPORT
    today <- lubridate::today()

    tier_final_consolidated %>%
      readr::write_csv(glue::glue("{import_folder}/{fiscal_quarter}_TIER_Import_File_v4_{today}.csv"))


    #Partner files
    Broadreach_import <- partner_import(df = tier_final_partner, 70287)
    RTC_import <- partner_import(df = tier_final_partner, 70290)
    ANOVA_import <- partner_import(df = tier_final_partner, 70310)
    MATCH_import <- partner_import(df = tier_final_partner, 81902)
    WRHI_import <- partner_import(df = tier_final_partner, 70301)

  # #ARVDISP ---------------------------------------------------------------
  #
  #   #MER mapping file
  #   arvdisp_mapping <- reference_folder %>%
  #     return_latest("RTC_DATIM MER TIER Results Consolidated_Workfile.xlsx") %>%
  #     read_xlsx(sheet= "ARVDispense")
  #
  #   df_arvdisp <- readxl::read_excel(ndoh_filepath, sheet = "ARVDISP")
  #
  #   #now, filter to usaid districts and pull facilities with 4 digit codes
  #   df_arvdisp_clean <- df_arvdisp %>%
  #     dplyr::mutate(District = dplyr::recode(District,
  #                                            "fs Thabo Mofutsanyana District Municipality" = "fs Thabo Mofutsanyane District Municipality")) %>%
  #     filter(District %in% usaid_dsp_district) %>%
  #     mutate(code_num = str_length(Code)) %>%
  #     group_by(Province, District, SubDistrict, Facility) %>%
  #     arrange(desc(code_num)) %>%
  #     left_join(mfl_new_df %>%
  #                 filter(
  #                   !(OU5name == "kz Mvoti Clinic" & is.na(`Old_OU5Code`))) %>%
  #                 dplyr::mutate(OU5name = recode(OU5name, "lp Matsotsosela Clinic" = "lp Matsotsosela clinic")) %>%
  #                 dplyr::select(OU5name, `New_OU5 Code`), by = c("Facility" = "OU5name")) %>%
  #     mutate(`New_OU5 Code` = as.character(`New_OU5 Code`),
  #            Code = as.character(Code),
  #            Code = ifelse(code_num < 7, `New_OU5 Code`, Code)
  #            # ,
  #            # Code = ifelse(Facility %in% misaligned_sites, `New_OU5 Code`, Code)
  #            ) %>%
  #     fill(Code) %>%
  #     ungroup() %>%
  #     # count(Facility, Code) %>% view()
  #     select(-c(code_num, `New_OU5 Code`)) %>%
  #     mutate(
  #       #Code = ifelse(Facility == "lp Naledi Clinic", 383651, Code),
  #            Code = as.character(Code))
  #
  #   # #no facilities in filter out
  #   # filter_out <- remove_sites %>% distinct() %>% pull()
  #   # rename_old <- rename_sites %>% distinct(old_name) %>% pull()
  #   # rename_new <- rename_sites %>% distinct(new_name) %>% pull()
  #   #
  #   # rename <- rename_sites %>% distinct(old_name) %>% pull()
  #   # df_arvdisp %>% filter(Facility %in% filter_out)
  #   # df_arvdisp %>% filter(Facility %in% rename_old) %>% distinct(Facility) %>% pull()
  #   #
  #   # ndoh_rename_arv <- df_arvdisp_clean %>%
  #   #   left_join(rename_sites, by = c("Facility" = "old_name")) %>%
  #   #   mutate(Facility = ifelse(!is.na(new_name), new_name, Facility),
  #   #          Code = ifelse(!is.na(new_name), new_code, Code))
  #   #
  #   # ndoh_rename_old_arv <- ndoh_rename_arv %>%
  #   #   filter(Facility %ni% rename_new)
  #
  #   ndoh_rename_new_arv <- df_arvdisp_clean %>%
  #   #  filter(Facility %in% rename_new) %>%
  #     group_by(Province, District, SubDistrict, Facility, Code,
  #              Sex, CoarseAgeGroup, RegimenCode, Packs) %>%
  #     summarise(across(c(starts_with("Total")), sum, na.rm = TRUE), .groups = "drop")
  #
  #   # df_arvdisp <- bind_rows(ndoh_rename_old_arv, ndoh_rename_new_arv) %>%
  #   #   select(-c(new_name, new_code))
  #
  #   #join NDOH to MFL
  #   join_arvdisp <- df_fac %>%
  #     left_join(ndoh_rename_new_arv,  by = c("new_ou5_code" = "Code"))
  #
  #   #CHECK******
  #   #what facilities are in NDOH but not in MFL?
  #   arvdisp_code <- unique(df_arvdisp$Code)
  #   mfl_code <- unique(df_fac$new_ou5_code)
  #   arv_code_list <- setdiff(arvdisp_code, mfl_code)
  #
  #   df_arvdisp %>% filter(Code %in% arv_code_list) %>%
  #     distinct(Facility) %>%
  #     pull()
  #
  #   df_arv_clean <- join_arvdisp %>%
  #     mutate(indicator = "SC_ARVDISP",
  #            RegimenCode = recode(RegimenCode, "13.0" = "13"))
  #
  #   #create indicator and N/D variable in mapping file
  #   arv_map_clean <- arvdisp_mapping %>%
  #     # count(`Datim UID`) %>%
  #     mutate(indicator = str_extract(dataElement, "[^ (]+")) %>%
  #     select(-c(Packs))
  #
  #   #now map
  #   arv_map_distinct <- arv_map_clean %>% distinct(CoarseAgeGroup, RegimenCode, indicator,
  #                                                  dataElement, dataElement_uid, categoryOptionComboName, categoryOptionCombo_uid)
  #
  #   # now, map in the additional regimen code
  #
  #   arv_additional_map <- reference_folder %>%
  #     return_latest("Dr Molapo_missing-regimen-code-FY22Q4_final") %>%
  #     read_excel() %>%
  #     select(dataelementname, dataelement_uid,
  #            Category, categoryoptioncombo_uid,`Missing RegimenCode`, CoarseAgeGroup) %>%
  #     mutate(indicator = "SC_ARVDISP") %>%
  #     rename(dataElement = dataelementname,
  #            dataElement_uid  = dataelement_uid,
  #            categoryOptionComboName = Category,
  #            categoryOptionCombo_uid =  categoryoptioncombo_uid,
  #            RegimenCode= `Missing RegimenCode`)
  #
  #   arv_map_distinct <- bind_rows(arv_map_distinct, arv_additional_map)
  #
  #
  #   df_arv_mapped <- df_arv_clean %>%
  #     left_join(arv_map_distinct, by = c("CoarseAgeGroup", "RegimenCode", "indicator"))
  #
  #
  #   #NOT MAPPED ---
  #   arv_not_mapped <- df_arv_mapped %>% filter(is.na(dataElement), !is.na(Packs)) %>%
  #     select(indicator, DSD_TA, District, Facility, CoarseAgeGroup, RegimenCode, dataElement)
  #
  #   write_csv(arv_not_mapped, "Dataout/arv-distinct-missing-disaggs.csv")
  #
  #   df_arv_final <-  df_arv_mapped %>%
  #     left_join(mech_df, by = c("datim_uid" = "facilityuid"))
  #
  #   #aggregate
  #   df_arv_agg_final <- df_arv_final %>%
  #     group_by(usaid_facility, datim_uid, period, Province, District, SubDistrict,
  #              Facility, indicator, dataElement, dataElement_uid, categoryOptionComboName,
  #              categoryOptionCombo_uid, sitename, mech_code, prime_partner_name,
  #              mech_uid) %>%
  #     summarise(across(c(starts_with("Packs")), sum, na.rm = TRUE), .groups = "drop")
  #
  #   #6243
  #
  #   # %>%view()
  #   #   left_join(mech_xwalk, by = c('mech_code')) %>%
  #   #   select(-c(mech_name.y)) %>%
  #   #   rename(mech_name = mech_name.x)
  #
  #   my_arv_validation <- df_arv_final %>%
  #     select(period,Province, District, Community, Facility, datim_uid,
  #            mech_name, mech_code, mech_uid, primepartner, indicator,
  #            CoarseAgeGroup, RegimenCode,
  #            dataElement, dataElement_uid, categoryOptionComboName, categoryOptionCombo_uid, Packs) %>%
  #     rename(SubDistrict = Community,
  #            value = Packs,
  #            orgUnit_uid = datim_uid)
  #
  #   import_file_clean_arv_agg <- df_arv_agg_final %>%
  #     select(mech_code, mech_uid, Facility, datim_uid, dataElement , dataElement_uid, categoryOptionCombo_uid,
  #            Packs, SubDistrict, District) %>%
  #     mutate(period = fiscal_quarter) %>%
  #     rename(value = Packs,
  #            #SubDistrict = Community,
  #            #mech_uid = mechanism_uid,
  #            orgUnit_uid = datim_uid,
  #            orgUnitName = Facility) %>%
  #     filter(!is.na(dataElement_uid)) %>%
  #     select(import_vars)

