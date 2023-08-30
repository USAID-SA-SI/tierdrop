#IMPORT -------------------------------------------------


#issue with file - use this for delete file for cleaning window
match_prep_q1 <- reference_folder %>%
  return_latest("PrEP Facilities MatCH_FY23Q1") %>%
  read_excel() %>%
  rename(facilityuid = ...3,
         sitename = FACILITY)

#original file used to filter out match facilities
match_no_prep_sites <- match_no_prep_q1 %>%
  distinct(facilityuid) %>%
  pull()

# IMPORT FILE -----------------------------------------------

#use this file for PrEP facilities import file for FY23Q1c
match_FY23Q1_prep_cleaning <- reference_folder %>%
  return_latest("PrEP Facilities_MatCH_CLEANING_20230307") %>%
  read_excel() %>%
  rename(prep_facility = `Prep facility (YES/NO)`)

#filter prep faclities and join datim uids
new_match_prep_cleaning <- match_FY23Q1_prep_cleaning %>%
  filter(prep_facility == "YES") %>%
  left_join(df_fac %>% select(usaid_facility, datim_uid),
            by = c("OrgUnit" = "usaid_facility")) %>%
  distinct(datim_uid) %>%
  pull()





#import file for MATCH facilities that report prep
import_uid_prep <- df_final %>%
  filter((orgUnit_uid %in% new_match_prep_cleaning
          & indicator %in% c("PrEP_CT", "PrEP_NEW"))) %>%
  distinct(orgUnit_uid) %>%
  pull()

setdiff(new_match_prep_cleaning, import_uid_prep)

#MATCH PREP IMPORT FILE
import_MATCH_prep <- df_final %>%
  filter((orgUnit_uid %in% new_match_prep_cleaning
          & indicator %in% c("PrEP_CT", "PrEP_NEW"))) %>%
  select(import_vars) %>%
  mutate(period = recode(period, "FY23Q1" = "2022Q4"))


write_csv(import_MATCH_prep, glue::glue("{import_folder}/{fiscal_quarter}_MATCH_CLEANING_ImportFile_v1_{today}.csv"))



#DELETE FILE ----------------------------------------------------------

#- use original match file for no prep sites:
# these are the sites that did make it in that we need a delete file for

no_prep_match_uids <- match_FY23Q1_prep_cleaning %>%
  filter(prep_facility == "NO") %>%
  left_join(df_fac %>% select(usaid_facility, datim_uid),
            by = c("OrgUnit" = "usaid_facility")) %>%
  distinct(datim_uid) %>%
  pull()

df_final_all_match_prep_filtereout <- df_final %>%
  filter(!(orgUnit_uid %in% match_no_prep_sites
           & indicator %in% c("PrEP_CT", "PrEP_NEW"))) %>%
  #select(import_vars) %>%
  mutate(period = recode(period, "FY23Q1" = "2022Q4"))


delete_file <- df_final_all_match_prep_filtereout %>%
  filter(mech_uid == 'Sm6Y3REDZ42',
         str_detect(indicator, "PrEP"),
         orgUnit_uid %in% no_prep_match_uids) %>%
   select(import_vars) %>%
   mutate(period = recode(period, "FY23Q1" = "2022Q4"))

write_csv(delete_file,
          glue::glue("{import_folder}/{fiscal_quarter}_MATCH_PREP_DELETE_FILE_v1_{today}.csv"))

#Check
import_final %>%
  #select(import_vars) %>%
  janitor::get_dupes()


# CONSOLIDATE_---------------------------------------------------------

anova_import <- import_folder %>%
  return_latest("70310_FY23Q1_TIER_Import_File_v1_2023-03-02_Clean-Up - Heleen Greyling") %>%
  read_csv()


consolidated_clean_import <- bind_rows(import_MATCH_prep, anova_import)


write_csv(consolidated_clean_import,
          glue::glue("{import_folder}/{fiscal_quarter}clean_CONSOLIDATE_IMPORTFILE_v2_{today}.csv"))
