# PROJECT:  SA-SI-MER- NONTIER [IN PROGRESS]
# AUTHOR:   Vanessa Da Costa| USAID
# PURPOSE:  Q3 MER Processing- NONTIER Indicators
# LICENSE:  MIT
# DATE:   2022-07-27
# UPDATED: 2022-08-01
# NOTE: Adapted from SA-SI-MER Script by Karishma Srikanth


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(readxl)
library(googlesheets4)

# IMPORT ------------------------------------------------------------------

today <- lubridate::today()
folderpath <- "data-raw/Import Files/NON_TIER"
dataout <- "data-raw/Import Files"
resource_folder <- "data-raw/q4_prep"

list.files(folderpath)


#Aggregate final NON-TIER import files for all DSP partners
ANOVA <- folderpath %>%
  return_latest("FY23Q2_ANOVA_nonTIER_v1_20230426 - Heleen Greyling.xlsx") %>%
  read_excel(sheet="import") %>%
  mutate(primepartner = "ANOVA HEALTH INSTITUTE",
         mech_code = as.numeric(mech_code))

MATCH<- folderpath %>%
  return_latest("MATCH_FY23Q2_nonTIER_V2_20230504 - Farzana Alli.xlsx") %>%
  read_excel(sheet="import") %>%
  mutate(primepartner = "MATERNAL ADOLESCENT AND CHILD HEALTH INSTITUTE NPC")

#REMOVE TB_STAT BEFORE READING IN FILE
BRCH<- folderpath %>%
  return_latest("FY23Q2_BROADREACH_nonTIER_V2_20230504 - Ayisolwainkosi Ncube.xlsx") %>%
  read_excel(sheet="import") %>%
  mutate(primepartner = "BROADREACH HEALTHCARE (PTY) LTD")

RTC<-folderpath %>%
  return_latest("RighttoCare_FY23Q2_nonTIER_v2_20230503 - Given Malete.xlsx") %>%
  read_excel(sheet="import") %>%
  mutate(primepartner = "RIGHT TO CARE",
         value = as.numeric(value))

#CORRECT OrgUnitNames fs Mohau Hospital and fs Tshwaraganang (Dealesville) Clinic
WRHI_70301<-folderpath %>%
  return_latest("FY23Q2_WRHI_70301_NONTIER_v2_20230504 - Kesebone Makhala.xlsx") %>%
  read_excel(sheet="import")%>%
  mutate(primepartner = "WITS HEALTH CONSORTIUM (PTY) LTD")

WRHI_80007<-folderpath %>%
  return_latest("WRHI_80007_FY23Q2_nonTIER_v1_20230425 - Lucky S Khoza.xlsx") %>%
  read_excel(sheet="import") %>%
  dplyr::mutate(primepartner = "WITS HEALTH CONSORTIUM (PTY) LTD") %>%
  select(1:15)

FY23Q2_Master_NonTier_DRAFT<- bind_rows(ANOVA,
                                        MATCH,
                                        BRCH, RTC, WRHI_70301
                                        , WRHI_80007
                                        ) %>%
  mutate(`period`="2023Q1") %>%
  select (`mech_code`: `period`, primepartner)

FY23Q2_Master_NonTier_DRAFT <- FY23Q2_Master_NonTier_DRAFT %>%
  select(mech_uid, orgUnit_uid, dataElement_uid, categoryOptionCombo_uid, value, period) %>%
  #select(-c(Column1, Column2)) %>%
  drop_na() %>%
  distinct()

# FY23Q1_Master_NonTier_DRAFT %>%
#   group_by(mech_uid, orgUnit_uid, dataElement_uid, categoryOptionCombo_uid, period) %>%
#   summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop")


# aggregate up!

  # add match

# FY22Q4_Master_NonTier_Final <- FY22Q4_Master_NonTier_DRAFT %>%
#   left_join(df_fac_master, by = c("orgUnitName" = "OU5name")) %>%
#   rename("dataElement"= "dataElementName") %>%
#   rename("SubDistrict"= "OU4name") %>%
#   rename("District"= "OU3name") %>%
#   select(mech_code, mech_uid, orgUnitName, `DATIM UID`, dataElement, dataElement_uid, categoryOptionComboName,categoryOptionCombo_uid, value) %>%
#   rename("orgUnit_uid"= "DATIM UID") %>%
#   distinct() %>%
#   drop_na()
#
# non_tier <- unique(FY22Q4_Master_NonTier_DRAFT$orgUnit_uid)
# facility <- unique(df_fac_master$`DATIM UID`)
#
# setdiff(non_tier, facility)
#
#
# janitor::get_dupes(FY22Q4_Master_NonTier_Final)
#
# df_msd %>%
#   filter(fiscal_year == 2022,
#          sitename == "mp Bhubezi CHC")



#Master NONTIER Import File
write_csv(FY23Q2_Master_NonTier_DRAFT, glue("{dataout}/FY23Q2_USAID_SA_Import_NonTier_consolidated_v1_{today}.csv"))



# TIER / NON-TIER CONSOLIDATION ----------------------------------------------

tier_final <- dataout %>%
  return_latest("FY23Q2_TIER_Import_File_v4_2023-05-04") %>%
  read_csv()

non_tier_final <- dataout %>%
  return_latest("FY23Q2_USAID_SA_Import_NonTier_consolidated_v1") %>%
  read_csv()


arv_final <- dataout %>%
  return_latest("FY23Q2_ARVDISP_Import_File_v2_2023-05-05") %>%
  read_csv()

#address ARVDISP

tier_minus_arvdisp <- tier_final %>%
  filter(dataElement_uid != "jjXWGplLXqF")


fy23q2_final <- bind_rows(tier_minus_arvdisp, non_tier_final, arv_final)

fy23q2_nozero <- fy23q2_final %>% filter(value != 0)

#write master consolidated
write_csv(fy23q2_final, glue("{dataout}/FY23Q2_USAID_SA_Import_FINALv2_{today}.csv"))
write_csv(fy23q2_nozero, glue("{dataout}/FY23Q2_USAID_SA_Import_FINALv2_{today}_NOZERO.csv"))


