
# Title: South Africa Import Aggregation
# Author: Karishma Srikanth
# Last updated: Nov 3, 2022
# Purpose:Combine TIER and NONTIER Imports (+ HRH and LAB)

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

# set folderpaths
dsd_folderpath <- "Data/DSD TA"
ndoh_folderpath <- "Data/NDOH Import"
fy22files_folderpath <- "Data/Files to be added to FY22Q2 TX_RTT import"
template_folderpath <- "Data/Import Files"
genie_folderpath <- "Data/Genie"
data_folder <- "Data/"
partner_files <- "data-raw/Import Files"

today <- lubridate::today()

# import msd
df_msd <- si_path() %>%
  return_latest('MER_Structured_Datasets_Site_IM_FY20-23_20220916_v2_2_South Africa') %>%
  read_msd()

# ================================

NONTIER_Q4_name <- partner_files %>%
  return_latest("Q4_USAID_SA_Import_NonTier_revised_2022-11-03") %>%
  read_csv() %>%
  select(mech_code, mech_uid, orgUnitName, orgUnit_uid, dataElement, dataElement_uid, categoryOptionComboName,
         categoryOptionCombo_uid, value)


NONTIER_Q4 <- partner_files %>%
  return_latest("Q4_USAID_SA_Import_NonTier_revised_2022-11-03") %>%
  read_csv() %>%
  select(mech_code, mech_uid, orgUnit_uid, dataElement, dataElement_uid, categoryOptionComboName,
         categoryOptionCombo_uid, value)

nontier_import <- NONTIER_Q4 %>%
  select(mech_uid, orgUnit_uid,dataElement_uid,
         categoryOptionCombo_uid, value)

nontier_import%>%
  janitor::get_dupes()

MATCH_TIER_Q3 <-partner_files %>%
  return_latest("81902_FY22Q4_TIER_Import_File_v4_2022-11-03") %>%
  read_csv()

ANOVA_TIER_Q3 <- partner_files %>%
  return_latest("70310_FY22Q4_TIER_Import_File_v4_2022-11-03") %>%
  read_csv()

BR_TIER_Q3 <-partner_files %>%
  return_latest("70287_FY22Q4_TIER_Import_File_v4_2022-11-03") %>%
  read_csv()

RTC_TIER_Q3 <-partner_files %>%
  return_latest("70290_FY22Q4_TIER_Import_File_v4_2022-11-03") %>%
  read_csv()

WRHI_TIER_Q3 <-partner_files %>%
  return_latest("70301_FY22Q4_TIER_Import_File_v4_2022-11-03") %>%
  read_csv()

# #Make a site map for the districts and facilities
#
# match_mechs <- data_folder %>%
#   return_latest("match_fy22q3_facility-mech") %>%
#   read_xlsx()
#
# msd_mechs <- df_msd %>%
#   filter(
#     #funding_agency == "USAID",
#     #    fiscal_year == 2022,
#     mech_code %in% c(70310, 70287, 81902, 70290, 70301, 80007)) %>%
#   count(sitename, facilityuid, mech_code, prime_partner_name)
#
# #SUBDISTRICT
#
# community_mechs <- df_msd %>%
#   filter(
#     #  funding_agency == "USAID",
#     #  fiscal_year == 2022,
#     mech_code %in% c(70310, 70287, 81902, 70290, 70301, 80007)) %>%
#   count(community, communityuid, mech_code, prime_partner_name)
#
# msd_mechs <- msd_mechs %>%
#   rbind(match_mechs)
#
# site_map <- msd_mechs %>%
#   select(sitename, facilityuid) %>%
#   distinct() %>%
#   rename(orgName = sitename, orgUnit_uid = facilityuid)
#
# community_map <- community_mechs %>%
#   select(community, communityuid) %>%
#   distinct() %>%
#   rename(orgName = community, orgUnit_uid = communityuid)
#
# #map in additional 2 facilities not coming frm MSD
# rtc_addition <- data_folder %>%
#   return_latest("additional-datim-facility-mapping-consolidation_20220805") %>%
#   read_xlsx()
#
# full_map <- bind_rows(site_map, community_map, rtc_addition) %>%
#   rename(orgUnitName = orgName)

#remove orgunitname from TIER files and bind
TIER_consolidated <-
  bind_rows(MATCH_TIER_Q3,
            ANOVA_TIER_Q3,
            BR_TIER_Q3,
            RTC_TIER_Q3,
            WRHI_TIER_Q3) %>%
#  select(-c(orgUnitName, SubDistrict, District)) %>%
  #dplyr::mutate(`Period`="2022Q3") %>%
  drop_na()

FY22Q4_Consolidated_Import_Final <- bind_rows(TIER_consolidated, nontier_import) %>%
  #janitor::get_dupes() %>% view()
  distinct()
# %>%
#   left_join(full_map, by  = c("orgUnit_uid"))

FY22Q4_Consolidated_Import_Final <- FY22Q4_Consolidated_Import_Final %>%
 # select(-c(orgUnitName)) %>%
  distinct() %>%
  mutate(period = recode(period, "FY22Q4" = '2022Q3'))
# %>%
#   filter(dataElement %ni% c("TB_ART (N, DSD, Age/Sex/NewExistingArt/HIVStatus): TB/HIV on ART",
#                             "TB_ART (N, TA, Age/Sex/NewExistingArt/HIVStatus): TB/HIV on ART")) %>%
#   relocate(orgUnitName, .before = orgUnit_uid)

# FY22Q4_Consolidated_Import_Final %>%
#   group_by(dataElement_uid, orgUnit_uid, categoryOptionCombo_uid, mech_uid, period) %>%
#   summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop")

# ADD LAB and HRH -----------------------------------------------------------------------------

Lab_import <- partner_files %>%
  return_latest("FY22Q4_LAB_PTCQI_import_20221103_Final") %>%
  read_csv() %>%
  distinct() %>%
  rename(orgUnit_uid = OrgUnit,
         dataElement_uid= Dataelement,
         categoryOptionCombo_uid = CategoryOptionCombo,
         mech_uid = AttributeOptionCombo,
         value = Value,
         period = Period) %>%
  select(mech_uid, orgUnit_uid,dataElement_uid,
         categoryOptionCombo_uid, value, period)


HRH_import <- partner_files %>%
  return_latest("HRH") %>%
  read_csv() %>%
  mutate(period = recode(period, "FY22Q4" = '2022Q3')) %>%
  rename(orgUnit_uid = datim_uid,
         dataElement_uid= dataelement_uid,
         categoryOptionCombo_uid = categoryoptioncombo_uid)




FY22Q4_Final_import_All <- bind_rows(FY22Q4_Consolidated_Import_Final,HRH_import, Lab_import)

#check for duplicates
FY22Q4_Final_import_All %>% janitor::get_dupes()



write_csv(FY22Q4_Final_import_All, glue("{partner_files}/FY22Q4_USAID_SA_Import_FINALv2_{today}.csv"))



# FY22Q3_Consolidated_Import_Final %>%
#   filter(mech_code == 70287) %>%
#   janitor::get_dupes()
#
# NONTIER_Q3 %>%
#   filter(mech_code %in% c(70287)) %>%
#   count(mech_code, dataElement) %>% view()



fy22q3_import_validation <- FY22Q3_Consolidated_Import_Final %>%
  filter(value != 0)

write_csv(fy22q3_import_validation, paste0("Dataout/FY22Q3_USAID_SA_Import_VALIDATION_NO_ZERO", format(Sys.time(), "%d-%b-%Y"), "_FINALv2.csv"))

write_csv(non_tier_duplicates, "Dataout/non-tier-duplication-errors.csv")
