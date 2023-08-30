#ARVDISP ---------------------------------------------------------------

#MER mapping file
arvdisp_mapping <- reference_folder %>%
  return_latest("RTC_DATIM MER TIER Results Consolidated_Workfile.xlsx") %>%
  read_xlsx(sheet= "ARVDispense")

df_arvdisp <- readxl::read_excel(ndoh_filepath, sheet = "ARVDISP")

#now, filter to usaid districts and pull facilities with 4 digit codes
df_arvdisp_clean <- df_arvdisp %>%
  dplyr::mutate(District = dplyr::recode(District,
                                         "fs Thabo Mofutsanyana District Municipality" = "fs Thabo Mofutsanyane District Municipality")) %>%
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
         Code = ifelse(code_num < 7, `New_OU5 Code`, Code)
         # ,
         # Code = ifelse(Facility %in% misaligned_sites, `New_OU5 Code`, Code)
  ) %>%
  fill(Code) %>%
  ungroup() %>%
  # count(Facility, Code) %>% view()
  select(-c(code_num, `New_OU5 Code`)) %>%
  mutate(
    #Code = ifelse(Facility == "lp Naledi Clinic", 383651, Code),
    Code = as.character(Code))

# #no facilities in filter out
# filter_out <- remove_sites %>% distinct() %>% pull()
# rename_old <- rename_sites %>% distinct(old_name) %>% pull()
# rename_new <- rename_sites %>% distinct(new_name) %>% pull()
#
# rename <- rename_sites %>% distinct(old_name) %>% pull()
# df_arvdisp %>% filter(Facility %in% filter_out)
# df_arvdisp %>% filter(Facility %in% rename_old) %>% distinct(Facility) %>% pull()
#
# ndoh_rename_arv <- df_arvdisp_clean %>%
#   left_join(rename_sites, by = c("Facility" = "old_name")) %>%
#   mutate(Facility = ifelse(!is.na(new_name), new_name, Facility),
#          Code = ifelse(!is.na(new_name), new_code, Code))
#
# ndoh_rename_old_arv <- ndoh_rename_arv %>%
#   filter(Facility %ni% rename_new)
#
# ndoh_rename_new_arv <- df_arvdisp_clean %>%
#   #  filter(Facility %in% rename_new) %>%
#   group_by(Province, District, SubDistrict, Facility, Code,
#            Sex, CoarseAgeGroup, RegimenCode, Packs) %>%
#   summarise(across(c(starts_with("Total")), sum, na.rm = TRUE), .groups = "drop")

#what facilities are in NDOH but not in MFL? all correctional
ndoh_code <- unique(df_arvdisp_clean$Code)
mfl_code <- unique(df_fac$new_ou5_code)
code_list <- setdiff(ndoh_code, mfl_code)


# all correctional sites after manual mutates
missing_sites_arv <- df_arvdisp_clean %>%
  filter(Code %in% code_list) %>% distinct(Facility) %>% pull()


#join NDOH to MFL
join_arvdisp <- df_fac %>%
  tidylog::left_join(df_arvdisp_clean,  by = c("new_ou5_code" = "Code"))

df_arv_clean <- join_arvdisp %>%
  mutate(indicator = "SC_ARVDISP",
         RegimenCode = recode(RegimenCode, "13.0" = "13"))

#MAPPING ------------------------------------------------------------

#option 1 - google file

arv_map <- googlesheets4::read_sheet(disagg_map_id, sheet = "ARVDISP_FY23") %>%
  mutate(RegimenCode = as.character(RegimenCode))

# df_arv_clean %>%
#   filter(!is.na(Facility)) %>%
#   left_join(arv_map, by = c("CoarseAgeGroup", "RegimenCode", "indicator")) %>%
#   filter(is.na(categoryOptionComboName))

#option 2 - CDC file---
cdc_map <- reference_folder %>%
  return_latest("CDC_refSC_ARVDISP mapping") %>%
  read_excel() %>%
  rename(categoryOptionComboName = `PEPFAR Combo`)


df_arv_mapped <- df_arv_clean %>%
  filter(!is.na(Facility)) %>%
  left_join(arv_map, by = c("CoarseAgeGroup", "RegimenCode", "indicator")) %>%
  mutate(dataElement = ifelse(is.na(dataElement), "SC_ARVDISP (N, NoApp, DispensedARVBottles): Dispensed ARV bottles", dataElement),
         dataElement_uid = ifelse(is.na(dataElement_uid), "jjXWGplLXqF", dataElement_uid)) %>%
  mutate(categoryOptionComboName = case_when(CoarseAgeGroup %in% c(">=15") & is.na(categoryOptionComboName) ~ "ARV Bottles - Other (Adult)",
                                             CoarseAgeGroup %in% c("<15") & is.na(categoryOptionComboName) ~ "ARV Bottles - Other (Pediatric)",
                                             TRUE ~ categoryOptionComboName)) %>%
  left_join(arv_map %>% select(categoryOptionComboName, categoryOptionCombo_uid) %>% distinct(), by = c("categoryOptionComboName")) %>%
  select(-c(categoryOptionCombo_uid.x)) %>%
  rename(categoryOptionCombo_uid =categoryOptionCombo_uid.y) %>%
  left_join(mech_df, by = c("datim_uid" = "facilityuid"))


# df_arv_mapped <- df_arv_clean %>%
#   filter(!is.na(Facility)) %>% #sites in MFL that are not ARVDISP
#   mutate(categoryOptionComboName = case_when(
#             indicator %in% c("SC_ARVDISP") & grepl(c("A3E"),        RegimenCode) & CoarseAgeGroup %in% c("<15") ~        "ARV Bottles - Other (Pediatric)",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("A3E"),        RegimenCode) & CoarseAgeGroup %in% c(">=15")~        "ARV Bottles - Other (Adult)",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("A3L"),        RegimenCode)                     ~        "ARV Bottles - LPV/r 40/10 (Pediatrics)",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("A3N"),        RegimenCode) & CoarseAgeGroup %in% c("<15") ~        "ARV Bottles - NVP (Pediatric)",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("A3N"),        RegimenCode) & CoarseAgeGroup %in% c(">=15")~        "ARV Bottles - NVP (Adult)",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("S3E"),        RegimenCode) & CoarseAgeGroup %in% c("<15") ~        "ARV Bottles - Other (Pediatric)",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("A3O"),        RegimenCode) & CoarseAgeGroup %in% c("<15") ~        "ARV Bottles - Other (Pediatric)",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("Z3O"),        RegimenCode) & CoarseAgeGroup %in% c("<15") ~        "ARV Bottles - Other (Pediatric)",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("S3E"),        RegimenCode) & CoarseAgeGroup %in% c(">=15")~        "ARV Bottles - Other (Adult)",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("A3O"),        RegimenCode) & CoarseAgeGroup %in% c(">=15")~        "ARV Bottles - Other (Adult)",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("Z3O"),        RegimenCode) & CoarseAgeGroup %in% c(">=15")~        "ARV Bottles - Other (Adult)",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("S3L"),        RegimenCode) & CoarseAgeGroup %in% c("<15") ~        "ARV Bottles - LPV/r 40/10 (Pediatrics)",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("S3L"),        RegimenCode) & CoarseAgeGroup %in% c(">=15")~        "ARV Bottles - Other (Adult)",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("T3N"),        RegimenCode) & CoarseAgeGroup %in% c(">=15")~        "ARV Bottles - NVP (Adult)",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("T3O","TFO"),  RegimenCode)                     ~        "ARV Bottles - TLD 30-count",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("TFE"),        RegimenCode)                     ~        "ARV Bottles - TLE 600/TEE",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("TFL"),        RegimenCode)                     ~        "ARV Bottles - Other (Adult)",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("TFN"),        RegimenCode)                     ~        "ARV Bottles - NVP (Adult)",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("Z3E"),        RegimenCode)                     ~        "ARV Bottles - Other (Adult)",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("Z3L"),        RegimenCode) & CoarseAgeGroup %in% c("<15") ~        "ARV Bottles - LPV/r 40/10 (Pediatrics)",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("Z3L"),        RegimenCode) & CoarseAgeGroup %in% c(">=15")~        "ARV Bottles - Other (Adult)",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("Z3N"),        RegimenCode) & CoarseAgeGroup %in% c("<15") ~        "ARV Bottles - NVP (Pediatric)",
#
#             indicator %in% c("SC_ARVDISP") & grepl(c("Z3N"),        RegimenCode) & CoarseAgeGroup %in% c(">=15")~        "ARV Bottles - NVP (Adult)",
#             indicator %in% c("SC_ARVDISP") & CoarseAgeGroup %in% c(">=15")                                      ~        "ARV Bottles - Other (Adult)",
#
#             indicator %in% c("SC_ARVDISP") & CoarseAgeGroup %in% c("<15")                                       ~        "ARV Bottles - Other (Pediatric)"))




df_arv_collapse <- df_arv_mapped %>%
  group_by(usaid_facility, datim_uid, period, Province, District, SubDistrict,
           Facility, indicator, dataElement, dataElement_uid, categoryOptionComboName,
           categoryOptionCombo_uid, sitename, mech_code, prime_partner_name,
           mech_uid) %>%
  summarise(across(c(starts_with("Packs")), sum, na.rm = TRUE), .groups = "drop")

my_arv_validation <- df_arv_collapse %>%
  select(period,Province, District, Facility, datim_uid, mech_code, mech_uid, prime_partner_name, indicator,
         dataElement, dataElement_uid, categoryOptionComboName, categoryOptionCombo_uid, Packs) %>%
  rename(
         value = Packs,
         orgUnit_uid = datim_uid)

import_file_clean_arv_agg <- df_arv_collapse %>%
  select(mech_code, mech_uid, Facility, datim_uid, dataElement , dataElement_uid,categoryOptionComboName, categoryOptionCombo_uid,
         Packs, District) %>%
  mutate(period = "2023Q1") %>%
  rename(value = Packs,
         #SubDistrict = Community,
         #mech_uid = mechanism_uid,
         orgUnit_uid = datim_uid) %>%
  filter(!is.na(dataElement_uid),
         !is.na(mech_uid),
         !is.na(orgUnit_uid)) %>%
  select(partner_vars)


import_file_clean_arv_agg %>%
  select(import_vars) %>%
  readr::write_csv(glue::glue("{import_folder}/{fiscal_quarter}_ARVDISP_Import_File_v2_{today}.csv"))
