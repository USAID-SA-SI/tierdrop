#ARVDISP ---------------------------------------------------------------

#MER mapping file

library(glamr)
library(tidyverse)
library(gophr)
library(glue)
library(readxl)
library(googlesheets4)
library(tierdrop)


dir_setup()

#store some locals (change this to a grab metadata later as well)
get_meta("FY24Q2")
arvdisp_mapping <- reference_folder %>%
  return_latest("data-raw/Reference Files/RTC_DATIM MER TIER Results Consolidated_Workfilev2.xlsx") %>%
  read_xlsx(sheet= "ARVDispense")

df_arvdisp <- readxl::read_excel(ndoh_filepath, sheet = "ARVDISP") %>%
  rename(SubDistrict = `Sub District`,
         RegimenCode = `Regimen Code`)

#now, filter to usaid districts and pull facilities with 4 digit codes
df_arvdisp_clean <- df_arvdisp %>%
  dplyr::mutate(District = dplyr::recode(District,
                                         "fs Thabo Mofutsanyana District Municipality" = "fs Thabo Mofutsanyane District Municipality")) %>%
  filter(District %in% usaid_dsp_district) %>%
  group_by(Province, District, SubDistrict, Facility) %>%
  # left_join(mfl_new_df %>%
  #             filter(
  #               !(OU5name == "kz Mvoti Clinic" & is.na(`Old_OU5Code`))) %>%
  #             dplyr::mutate(OU5name = recode(OU5name, "lp Matsotsosela Clinic" = "lp Matsotsosela clinic")) %>%
  #             dplyr::select(OU5name, `New_OU5 Code`), by = c("Facility" = "OU5name")) %>%
  ungroup() %>%
  mutate(
    Code = as.character(Code))

#check for facilities in but not in MFL
import_arvdisp(ndoh_filepath) %>% validate_ndoh()

ndoh_arvdisp <- import_arvdisp(ndoh_filepath) %>% tidy_map_arvdisp()


#MAPPING ------------------------------------------------------------

#option 1 - google file

arv_map <- googlesheets4::read_sheet(disagg_map_id, sheet = "ARVDISP_FY23") %>%
  mutate(RegimenCode = as.character(RegimenCode))

#partner review file format
ndoh_arv_final <- ndoh_arvdisp %>%
  rename(
    value = Packs,
    orgUnit_uid = datim_uid) %>%
  filter(!is.na(dataElement_uid),
         !is.na(mech_uid),
         !is.na(orgUnit_uid)) %>%
  select(partner_vars)

#import file format
ndoh_arv_final %>%
  select(import_vars) %>%
  readr::write_csv(glue::glue("{import_folder}/{fiscal_quarter}_ARVDISP_Import_File_v2_{today}.csv"))
