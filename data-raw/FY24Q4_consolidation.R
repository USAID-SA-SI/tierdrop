# PROJECT:  SA-SI-MER- NONTIER [IN PROGRESS]
# AUTHOR:   Vanessa Da Costa| USAID
# PURPOSE:  Q3 MER Processing- NONTIER Indicators
# LICENSE:  MIT
# DATE:   2022-07-27
# UPDATED: 2022-08-01
# NOTE: Adapted from SA-SI-MER Script by Karishma Srikanth


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glamr)
library(gophr)
library(glue)
library(readxl)
library(googlesheets4)

# IMPORT ------------------------------------------------------------------

# #store some locals (change this to a grab metadata later as well)
# fiscal_quarter <- "FY23Q3"
# import_period_style <- "2023Q2"
# curr_qtr <- "Q3"
# today <- lubridate::today()

folderpath <- "data-raw/Import Files/Consolidation"
dataout <- "data-raw/Import Files"

list.files(folderpath)

# 1) pull in tier files -----------------------------------------------------------

fy25q1_tier <- dataout %>%
  return_latest("FY25Q1_TIER_Import_File_v4_2025-04-08") %>%
  read_csv()


# first, lets pull non TIER -----------------------------------------------------
fy25q1_nontier <- folderpath %>%
  return_latest("Appended Non-TiER") %>% #change to match the extract filepath
  read_csv() %>%
  select(mech_uid,orgUnit_uid, dataElement_uid, categoryOptionCombo_uid, value, period) %>%
  mutate(period == "2024Q3")

#no dupes
fy24q4_nontier %>%
  janitor::get_dupes(mech_uid ,orgUnit_uid,dataElement_uid,categoryOptionCombo_uid, period)

#no missing UID
fy24q4_nontier %>%
  filter(is.na(dataElement_uid))

# rbind - tons of dupes
fy24q4_tier_nontier <- bind_rows(fy24q4_tier, fy24q4_nontier) %>%
  distinct() %>%
  filter(value != 0) %>%
  group_by(mech_uid ,orgUnit_uid,dataElement_uid,categoryOptionCombo_uid, period) %>%
  summarise(value=sum(value,na.rm = TRUE)) %>%
  ungroup()

# --------------------------------------------------------------------------------------
# AGYW -----------------------------------------------------
fy24q4_agyw <- folderpath %>%
  return_latest("AGYW_PREV_FY24Q4") %>% #change to match the extract filepath
  read_csv() %>%
  rename(dataElement_uid = dataElement,
         orgUnit_uid = Orgunit,
         categoryOptionCombo_uid = categoryOptionCombo,
         mech_uid = attributeOptionCombo,
         value= Value) %>%
  select(mech_uid,orgUnit_uid, dataElement_uid, categoryOptionCombo_uid, value, period)

# CDC -----------------------------------------------------
fy24q4_CDC <- folderpath %>%
  return_latest("CDC_AND_CS_DIRECT_IMPORT") %>% #change to match the extract filepath
  read_csv() %>%
  rename(dataElement_uid = dataelement_uid,
         orgUnit_uid = orgunit_uid,
         categoryOptionCombo_uid = categoryoptioncombo_uid) %>%
  select(mech_uid,orgUnit_uid, dataElement_uid, categoryOptionCombo_uid, value, period)

# HRH -----------------------------------------------------
fy24q4_HRH <- folderpath %>%
  return_latest("Final_HRAH_NAT_PEPFAR_USAID_CDC") %>% #change to match the extract filepath
  read_csv() %>%
  rename(dataElement_uid = Dataelement,
         orgUnit_uid = OrgUnit,
         period = Period,
         categoryOptionCombo_uid = CategoryOptionCombo,
         mech_uid = AttributeOptionCombo,
         value= Value) %>%
  select(mech_uid,orgUnit_uid, dataElement_uid, categoryOptionCombo_uid, value, period)


# LAB -----------------------------------------------------
fy24q4_LAB <- folderpath %>%
  return_latest("FY24Q4Final_USAID_CDC_LAB_PTCQI") %>% #change to match the extract filepath
  read_csv() %>%
  rename(dataElement_uid = Dataelement,
         orgUnit_uid = OrgUnit,
         period = Period,
         categoryOptionCombo_uid = CategoryOptionCombo,
         mech_uid = AttributeOptionCombo,
         value= Value) %>%
  select(mech_uid,orgUnit_uid, dataElement_uid, categoryOptionCombo_uid, value, period)

#check dupes
bind_rows(fy24q4_agyw,
         # fy24q4_CDC,
          fy24q4_HRH, fy24q4_LAB) %>%
  filter(is.na(dataElement_uid)) %>%
  janitor::get_dupes(mech_uid ,orgUnit_uid,dataElement_uid,categoryOptionCombo_uid, period)

# BIND ALL --------------------------------------------------------------------------------------
# fy24q4_tier_nontier <- bind_rows(fy24q4_tier, fy24q4_nontier) %>%
#   distinct()

fy24q4_all_others <- bind_rows(fy24q4_agyw,
                              # fy24q4_CDC,
                               fy24q4_HRH, fy24q4_LAB)
#Aggregate final NON-TIER import files for all DSP partners

df_final_consolidated <- bind_rows(fy24q4_tier_nontier, fy24q4_all_others) %>%
select(dataElement_uid, period, orgUnit_uid, categoryOptionCombo_uid, mech_uid, value) %>%
mutate(period = "2024Q3") %>%
  #select(-c(Column1, Column2)) %>%
  drop_na() %>%
  distinct()

#ALL THE DUPLICATES - 29k
df_final_consolidated %>%
  janitor::get_dupes(mech_uid ,orgUnit_uid,dataElement_uid,categoryOptionCombo_uid, period)


#EXPORT
today <- lubridate::today()
write_csv(df_final_consolidated, glue("{dataout}/{fiscal_quarter}_FINAL_consolidated_v4_NO_CDC_{today}.csv"))



# TIER / NON-TIER CONSOLIDATION ----------------------------------------------

tier_final <- dataout %>%
  return_latest("FY23Q3_TIER_Import_File_2023-07-27_v2") %>%
  read_csv()

non_tier_final <- dataout %>%
  return_latest("FY23Q3_NonTIER_consolidated_v1_2023-07-27_v1.csv") %>% #change to match filepath
  read_csv()


fy23q3_final <- bind_rows(tier_final, non_tier_final)

fy23q3_nozero <- fy23q3_final %>% filter(value != 0)

#write master consolidated
write_csv(fy23q3_final, glue("{dataout}/{fiscal_quarter}_USAID_SA_Import_FINALv1_{today}.csv"))
write_csv(fy23q3_nozero, glue("{dataout}/{fiscal_quarter}_USAID_SA_Import_FINALv1_{today}_NOZERO.csv"))
