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

#store some locals (change this to a grab metadata later as well)
fiscal_quarter <- "FY23Q3"
import_period_style <- "2023Q2"
curr_qtr <- "Q3"
today <- lubridate::today()

folderpath <- "data-raw/Import Files/NON_TIER"
dataout <- "data-raw/Import Files"

list.files(folderpath)


#Aggregate final NON-TIER import files for all DSP partners

ANOVA <- folderpath %>%
  return_latest("ANOVA") %>% #change to match the extract filepath
  read_excel(sheet="import") %>%
  mutate(primepartner = "ANOVA HEALTH INSTITUTE",
         mech_code = as.numeric(mech_code))

MATCH<- folderpath %>%
  return_latest("MATCH") %>% #change to match the extract filepath
  read_excel(sheet="import") %>%
  mutate(primepartner = "MATERNAL ADOLESCENT AND CHILD HEALTH INSTITUTE NPC")

BRCH<- folderpath %>%
  return_latest("BROADREACH") %>% #change to match the extract filepath
  read_excel(sheet="import") %>%
  mutate(primepartner = "BROADREACH HEALTHCARE (PTY) LTD")

RTC<-folderpath %>%
  return_latest("RTC") %>% #change to match the extract filepath
  read_excel(sheet="import") %>%
  mutate(primepartner = "RIGHT TO CARE",
         value = as.numeric(value))

WRHI_70301<-folderpath %>%
  return_latest("WRHI_70301") %>% #change to match the extract filepath
  read_excel(sheet="import")%>%
  mutate(primepartner = "WITS HEALTH CONSORTIUM (PTY) LTD")

WRHI_80007<-folderpath %>%
  return_latest("WRHI_80007") %>% #change to match the extract filepath
  read_excel(sheet="import") %>%
  dplyr::mutate(primepartner = "WITS HEALTH CONSORTIUM (PTY) LTD") %>%
  select(1:15)

df_nontier <- bind_rows(ANOVA,
                        MATCH,
                        BRCH,
                        RTC,
                        WRHI_70301,
                        WRHI_80007) %>%
  mutate(`period`= import_period_style) %>%
  select (`mech_code`: `period`, primepartner)

df_nontier_final <- df_nontier %>%
  select(mech_uid, orgUnit_uid, dataElement_uid, categoryOptionCombo_uid, value, period) %>%
  #select(-c(Column1, Column2)) %>%
  drop_na() %>%
  distinct()

#export
write_csv(df_nontier_final, glue("{dataout}/{fiscal_quarter}_NonTIER_consolidated_v1_{today}.csv"))



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
