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
resource_folder <- "data-raw/q4_prep"

files <- folderpath %>%
  list.files(full.names = TRUE)

# df <- files %>% map_dfr(~read_excel(.x, sheet ="import", col_types = "text"))
#
# FY22Q4_Master_NonTier_DRAFT <- df %>%
#   select(1:14) %>%
#   mutate(primepartner = case_when(mech_code==70301 ~ "WITS HEALTH CONSORTIUM (PTY) LTD",
#                                   mech_code ==70287 ~ "BROADREACH HEALTHCARE (PTY) LTD",
#                                   mech_code ==80007 ~ "WITS HEALTH CONSORTIUM (PTY) LTD",
#                                   mech_code ==81902 ~ "MATERNAL ADOLESCENT AND CHILD HEALTH INSTITUTE NPC",
#                                   mech_code ==70290 ~ "RIGHT TO CARE",
#                                   mech_code ==70310 ~ "ANOVA HEALTH INSTITUTE")) %>%
#   mutate(`period`="2022Q4") %>%
#   select (`mech_code`: `period`) %>%
#   drop_na()

df_msd <- si_path() %>%
  return_latest('MER_Structured_Datasets_Site_IM_FY20-23_20220916_v2_2_South Africa') %>%
  read_msd()

  #df_msd<- read.delim("Q3 Datasets/MER_Structured_Datasets_Site_IM_FY20-23_20220617_v2_1_South Africa.txt")

q4_id <- "1G2u0Rw0EgsExfPDjKIXzzB31OLzAVhkdPb5yz1lOauU"

#Load MFL as is
mfl_new_df <- googlesheets4::read_sheet(q4_id, sheet = "MFL_FY22Q4")

# #facility list for MFL checks adding district and sub-district
# df_fac<-read_excel("Q3 Datasets/USAID_MASTER_FACILITY_LIST_FY22Q3_draft v3.xlsx", sheet="MFL")
#
#
#so we have subdistricts as orgunits
df_fac_2<-resource_folder %>%
  return_latest("Additive Facility List_missingNONTIER") %>%
  read_excel()

df_fac<- mfl_new_df %>%
  select (`OU5name`, `OU4name`, `OU3name`, `DATIM UID`)  %>%
  distinct()

df_fac_master<- bind_rows(df_fac, df_fac_2) %>%
  select (`OU5name`, `OU4name`, `OU3name`, `DATIM UID`)  %>%
  distinct()


#Aggregate final NON-TIER import files for all DSP partners
ANOVA <- folderpath %>%
  return_latest("70310 v2_Anova_Non_TIER_20221103_") %>%
  read_excel(sheet="import") %>%
  mutate(primepartner = "ANOVA HEALTH INSTITUTE",
         mech_code = as.numeric(mech_code))

MATCH<- folderpath %>%
  return_latest("MATCH") %>%
  read_excel(sheet="import") %>%
  mutate(primepartner = "MATERNAL ADOLESCENT AND CHILD HEALTH INSTITUTE NPC")

#REMOVE TB_STAT BEFORE READING IN FILE
BRCH<- folderpath %>%
  return_latest("70287 v2 BROADREACH_NONTIER_20221103") %>%
  read_excel(sheet="import") %>%
  mutate(primepartner = "BROADREACH HEALTHCARE (PTY) LTD")

RTC<-folderpath %>%
  return_latest("70290 v2 RTC-Non Tier Data Import Template - FY22Q4V2-03112022-1400pm") %>%
  read_excel(sheet="import") %>%
  mutate(primepartner = "RIGHT TO CARE",
         value = as.numeric(value))

#CORRECT OrgUnitNames fs Mohau Hospital and fs Tshwaraganang (Dealesville) Clinic
WRHI_70301<-folderpath %>%
  return_latest("70301 WRHI_NONTIER_v3_20221102") %>%
  read_excel(sheet="import")%>%
  mutate(primepartner = "WITS HEALTH CONSORTIUM (PTY) LTD")

WRHI_80007<-folderpath %>%
  return_latest("80007 WRHI Non TIER") %>%
  read_excel(sheet="import") %>%
  dplyr::mutate(primepartner = "WITS HEALTH CONSORTIUM (PTY) LTD") %>%
  select(1:15)

FY22Q4_Master_NonTier_DRAFT<- bind_rows(ANOVA, MATCH, BRCH, RTC, WRHI_70301, WRHI_80007) %>%
  mutate(`period`="2022Q4") %>%
  select (`mech_code`: `period`) %>% select(-c(Column1, Column2)) %>%
  drop_na()

FY22Q4_Master_NonTier_Final <- FY22Q4_Master_NonTier_DRAFT %>%
  left_join(df_fac_master, by = c("orgUnitName" = "OU5name")) %>%
  rename("dataElement"= "dataElementName") %>%
  rename("SubDistrict"= "OU4name") %>%
  rename("District"= "OU3name") %>%
  select(mech_code, mech_uid, orgUnitName, `DATIM UID`, dataElement, dataElement_uid, categoryOptionComboName,categoryOptionCombo_uid, value) %>%
  rename("orgUnit_uid"= "DATIM UID") %>%
  distinct() %>%
  drop_na()

non_tier <- unique(FY22Q4_Master_NonTier_DRAFT$orgUnit_uid)
facility <- unique(df_fac_master$`DATIM UID`)

setdiff(non_tier, facility)


janitor::get_dupes(FY22Q4_Master_NonTier_Final)

df_msd %>%
  filter(fiscal_year == 2022,
         sitename == "mp Bhubezi CHC")



#Master NONTIER Import File
write_csv(FY22Q4_Master_NonTier_Final, glue("{folderpath}/Q4_USAID_SA_Import_NonTier_revised_{today}.csv"))

#File to be used for NON-TIER  validation checks
FY22Q4_Master_NonTier_Validations<- FY22Q4_Master_NonTier_DRAFT %>%
  #add a code that changes dataelement name to indicator
  mutate(indicator = str_extract(`dataElementName`, "[^ (]+"),
         numeratordenom = str_extract(`dataElementName`, "[^,]+"),
         numeratordenom = str_extract(`numeratordenom`, "(?<=())[^()]*$"),
         DSD_TA = str_split(`dataElementName`, ", ") %>% unlist() %>% nth(2),
         standardizeddisaggregate = str_extract(`dataElementName`, "(?<=,)[^,]*$"))
#File to be combined with TIER indicators
# LEVEL 1 ------------------------------------------------------------------

genie_validation <- df_msd %>%
  filter(funding_agency == "USAID",
         fiscal_year == 2022,
         standardizeddisaggregate == "Total Numerator") %>%
  group_by(prime_partner_name,mech_code, sitename, facilityuid, funding_agency, indicator, fiscal_year, standardizeddisaggregate) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
  #select only non-tier indicators
  filter(indicator %in% c("HTS_INDEX,","HTS_SELF", "HTS_TST", "PMTCT_ART", "PMTCT_EID", "PMTCT_HEI_POS", "PMTCT_HEI_POS_ART","PMTCT_STAT", "HTS_RECENT"))


#aggregate results up to facility by indicator
validation_agg_result <- FY22Q4_Master_NonTier_Validations %>%
  #exclude dataelements that are duplicates within the indicator
  filter(!dataElementName %in% c("HTS_SELF (N, TA, KeyPop/HIVSelfTest): HIV self test kits distributed",
                                 "PMTCT_HEI_POS (N, TA, Age/HIVStatus/ARTStatus): Infant Testing","PMTCT_HEI_POS (N, DSD, Age/HIVStatus/ARTStatus): Infant Testing")) %>%
  group_by(primepartner,mech_code, mech_uid, orgUnitName,orgUnit_uid, indicator, period) %>%
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop")

#check for results in previous quarter and missing in current quarter
check_1_1 <- genie_validation %>%
  mutate(mech_code = as.numeric(mech_code)) %>%
  left_join(validation_agg_result, by = c("sitename" = "orgUnitName", "indicator", "mech_code")) %>% view()
  mutate(check = ifelse(!is.na(qtr3) & is.na(value), "Value reported in previous quarter but missing in this quarter", NA),
         level = "Level 1",
         today_date = lubridate::today(),
         type_check = "Result in previous quarter") %>%
  filter(!is.na(check)) %>% view()
  select(prime_partner_name, mech_code, level, today_date, type_check,check,indicator, sitename)


#bind all relevant checks
level1_checks <- check_1_1

#ANOVA
anova_level_1 <- level1_checks %>%
  filter(prime_partner_name == "ANOVA HEALTH INSTITUTE")

#BROADREACH
BR_level_1 <- level1_checks %>%
  filter(prime_partner_name == "BROADREACH HEALTHCARE (PTY) LTD")

#MATCH
MATCH_level_1 <- level1_checks %>%
  filter(prime_partner_name == "MATERNAL ADOLESCENT AND CHILD HEALTH INSTITUTE NPC")

#RIGHT TO CARE
RTC_level_1 <- level1_checks %>%
  filter(prime_partner_name == "RIGHT TO CARE")

#WRHI
WRHI_70301_level_1 <- level1_checks %>%
  filter(prime_partner_name == 'WITS HEALTH CONSORTIUM (PTY) LTD',
    mech_code == 70301)

#WRHI
WRHI_80007_level_1 <- level1_checks %>%
  filter(prime_partner_name == 'WITS HEALTH CONSORTIUM (PTY) LTD',
         mech_code == 80007)

write_excel_csv(level1_checks, paste0("MASTER_Level1_NONTIERchecks_FY22Q3_", format(Sys.time(), "%d-%b-%Y"), ".csv"))
write_csv(anova_level_1, "ANOVA_Level1_NONTIERchecks_FY22Q3.csv")
write_csv(anova_level_1, "ANOVA_Level1_NONTIERchecks_FY22Q3.csv")
write_csv(BR_level_1, "BroadReach_Level1_NONTIERchecks_FY22Q3.csv")
write_csv(MATCH_level_1, "MATCH_Level1_NONTIERchecks_FY22Q3.csv")
write_csv(RTC_level_1, "RTC_Level1_NONTIERchecks_FY22Q3.csv")
write_csv(WRHI_70301_level_1, "WRHI_70301_Level1_NONTIERchecks_FY22Q3.csv")
write_csv(WRHI_80007_level_1, "WRHI_80007_Level1_NONTIERchecks_FY22Q3.csv")


# LEVEL 2 ---------------------------------------------------------------

#2.9 - Index_offered<Index_accepted (facility) [NON-TIER]

check_2_9 <- FY22Q4_Master_NonTier_Validations %>%
  filter(indicator %in% c("HTS_INDEX")) %>%
  filter(standardizeddisaggregate %in% c(" Index/Age/Sex/CasesOffered): Number of index cases offered"," Index/Age/Sex/CasesAccepted): Number of index cases accepted")) %>%
  group_by(primepartner,mech_code, mech_uid, orgUnitName,orgUnit_uid, indicator, standardizeddisaggregate, period) %>%
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = "standardizeddisaggregate", values_from = "value") %>%
  mutate(check = ifelse(` Index/Age/Sex/CasesOffered): Number of index cases offered` < ` Index/Age/Sex/CasesAccepted): Number of index cases accepted`, "Index_offered<Index_accepted", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(primepartner,mech_code,level,today_date,check, period, orgUnitName)

#2.10 - Index_contacts <contacts_ new_pos + contacts_ new_neg + contacts_ known_pos (facility)
check_2_10 <- FY22Q4_Master_NonTier_Validations %>%
  filter(indicator %in% c("HTS_INDEX")) %>%
  filter(standardizeddisaggregate %in% c(" Index/Age Aggregated/Sex/Contacts): Number of contacts", " Index/Age/Sex/Result): HTS Result")) %>%
  group_by(mech_code, mech_uid, orgUnitName,orgUnit_uid, indicator, standardizeddisaggregate, period, primepartner) %>%
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = "standardizeddisaggregate", values_from = "value") %>%
  mutate(check = ifelse(` Index/Age Aggregated/Sex/Contacts): Number of contacts` < ` Index/Age/Sex/Result): HTS Result`, "Index_contacts <contacts_ new_pos + contacts_ new_neg + contacts_ known_pos", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(primepartner,mech_code,level,today_date,check, period, orgUnitName)
#select(primepartner,level,today_date,check, period, orgUnitName,` Index/Age Aggregated/Sex/Contacts): Number of contacts`,` Index/Age/Sex/Result): HTS Result`)


#2.11 - HTS_TST_POS (>=15) >=HTS_RECENT #note, no partners reported HTS_RECENT as of 7.29
HTS_TST_POS_15plus <- FY22Q4_Master_NonTier_Validations %>%
  filter((`indicator`== "HTS_TST" & (categoryOptionComboName== "15-19, Male, Positive" | categoryOptionComboName==  "15-19, Female, Positive"
                                     | categoryOptionComboName== "20-24, Male, Positive" | categoryOptionComboName==  "20-24, Female, Positive"
                                     | categoryOptionComboName== "25-29, Male, Positive" | categoryOptionComboName==  "25-29, Female, Positive"
                                     | categoryOptionComboName== "30-34, Male, Positive" | categoryOptionComboName==  "30-34, Female, Positive"
                                     | categoryOptionComboName== "35-39, Male, Positive" | categoryOptionComboName==  "35-39, Female, Positive"
                                     | categoryOptionComboName== "40-44, Male, Positive" | categoryOptionComboName==  "40-44, Female, Positive"
                                     | categoryOptionComboName== "45-49, Male, Positive" | categoryOptionComboName==  "45-49, Female, Positive"
                                     | categoryOptionComboName== "50+, Male, Positive" | categoryOptionComboName==  "50+, Female, Positive")))

HTS_RECENT <- FY22Q4_Master_NonTier_Validations %>%
  filter(indicator %in% c("HTS_RECENT"))

check_2_11<-bind_rows(HTS_TST_POS_15plus, HTS_RECENT) %>%
  group_by(mech_code, mech_uid, orgUnitName,orgUnit_uid, indicator, period, primepartner) %>%
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = "indicator", values_from = "value") %>%
  mutate(check = ifelse(`HTS_TST` >= `HTS_RECENT`, "HTS_TST_POS (>=15) >=HTS_RECENT", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(primepartner,mech_code,level,today_date,check, period, orgUnitName)


#2.12 - HTS_RECENT >= Key Populations Sub-total

#2.13 - HTS_TST<HTS_TST_POS
check_2_13 <-  FY22Q4_Master_NonTier_Validations %>%
  filter(indicator %in% c("HTS_TST")) %>%
  group_by(mech_code, mech_uid, orgUnitName,orgUnit_uid, indicator, period, primepartner,categoryOptionComboName) %>%
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = "categoryOptionComboName", values_from = "value") %>%
  mutate(HTS_TST_POS = `Unknown Age, Female, Positive` +`Unknown Age, Male, Positive` + `<1, Female, Positive`+
           `<1, Male, Positive`+ `10-14, Female, Positive` +
           `10-14, Male, Positive`+ `1-4, Female, Positive`+ `1-4, Male, Positive`+ `15-19, Female, Positive`+
           `15-19, Male, Positive`+ `20-24, Female, Positive`+ `20-24, Male, Positive`+ `25-29, Female, Positive`+
           `25-29, Male, Positive`+ `30-34, Female, Positive`+ `30-34, Male, Positive`+ `35-39, Female, Positive`+
           `35-39, Male, Positive`+ `40-44, Female, Positive`+ `40-44, Male, Positive`+ `45-49, Female, Positive`+
           `45-49, Male, Positive`+ `50+, Female, Positive`+ `50+, Male, Positive`+`5-9, Female, Positive`+ `5-9, Male, Positive`,
         HTS_TST= HTS_TST_POS + `Unknown Age, Female, Negative` +`Unknown Age, Male, Negative` + `<1, Female, Negative`+
           `<1, Male, Negative`+ `10-14, Female, Negative` +
           `10-14, Male, Negative`+ `1-4, Female, Negative`+ `1-4, Male, Negative`+ `15-19, Female, Negative`+
           `15-19, Male, Negative`+ `20-24, Female, Negative`+ `20-24, Male, Negative`+ `25-29, Female, Negative`+
           `25-29, Male, Negative`+ `30-34, Female, Negative`+ `30-34, Male, Negative`+ `35-39, Female, Negative`+
           `35-39, Male, Negative`+ `40-44, Female, Negative`+ `40-44, Male, Negative`+ `45-49, Female, Negative`+
           `45-49, Male, Negative`+ `50+, Female, Negative`+ `50+, Male, Negative`+`5-9, Female, Negative`+ `5-9, Male, Negative`,
         check = ifelse(`HTS_TST` < `HTS_TST_POS`, "HTS_TST < HTS_TST_POS", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(primepartner,mech_code,level,today_date,check, period, orgUnitName)


#2.14 - Unknown /OVC_HIV_STAT not greater than 10% by APR less than 5%
##TO ADD FOR FY22Q4

#2.15 - PMTCT_EID 0-2months > PMTCT_STAT_POS+HTS_TST_POS_PMTCT PostANC1/Age/Sex/Result
PMTCT_EID_0_2months <- FY22Q4_Master_NonTier_Validations %>%
  filter((`indicator`== "PMTCT_EID" & categoryOptionComboName== "<= 2 months"))

PMTCT_STAT_POS <- FY22Q4_Master_NonTier_Validations %>%
  filter(`indicator`== "PMTCT_STAT") %>%
  filter(numeratordenom %in% c("N")) %>%
  filter(str_detect(categoryOptionComboName, "Positive"))

HTS_TST_POS_PMTCT <-FY22Q4_Master_NonTier_Validations %>%
  filter(dataElementName %in% c("HTS_TST (N, DSD, PMTCT PostANC/Age/Sex/Result): HTS received results", "HTS_TST (N, TA, PMTCT PostANC/Age/Sex/Result): HTS received results")) %>%
  filter(str_detect(categoryOptionComboName, "Positive")) #INDICATOR IS HTS_TCT

check_2_15<-bind_rows(PMTCT_EID_0_2months, PMTCT_STAT_POS, HTS_TST_POS_PMTCT) %>%
  group_by(mech_code, mech_uid, orgUnitName,orgUnit_uid, indicator, period, primepartner) %>%
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = "indicator", values_from = "value") %>%
  mutate(PMTCT_POS = PMTCT_STAT + HTS_TST,
         check = ifelse(PMTCT_EID > PMTCT_POS, "PMTCT_EID 0-2months > PMTCT_STAT_POS+HTS_TST_POS_PMTCT", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(primepartner,mech_code,level,today_date,check, period, orgUnitName)

#2.16 - PMTCT_HEI_POS > PMTCT_EID
PMTCT_EID <- FY22Q4_Master_NonTier_Validations %>%
  filter(`indicator`== "PMTCT_EID")

PMTCT_HEI_POS <- FY22Q4_Master_NonTier_Validations %>%
  filter(`indicator`== "PMTCT_HEI_POS") %>%
  filter(categoryOptionComboName %in% c("<= 2 months, Positive","2 - 12 months , Positive"))

check_2_16 <-  bind_rows(PMTCT_EID,PMTCT_HEI_POS ) %>%
  group_by(mech_code, mech_uid, orgUnitName,orgUnit_uid, indicator, period, primepartner) %>%
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = "indicator", values_from = "value") %>%
  mutate(check = ifelse(`PMTCT_HEI_POS` > `PMTCT_EID`, "PMTCT_HEI_POS > PMTCT_EID", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(primepartner,mech_code,level,today_date,check, period, orgUnitName)

# 2.17 - PMTCT_STAT numerator > PMTCT_STAT denominator
check_2_17 <-  FY22Q4_Master_NonTier_Validations %>%
  filter(indicator %in% c("PMTCT_STAT")) %>%
  group_by(mech_code, mech_uid, orgUnitName,orgUnit_uid, indicator, period, numeratordenom,primepartner) %>%
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = "numeratordenom", values_from = "value") %>%
  mutate(check = ifelse(`N` > `D`, "PMTCT_STAT_N >  PMTCT_STAT_D", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(primepartner,mech_code, level,today_date,check, period, orgUnitName)

# 2.19 - PMTCT_STAT_POS < PMTCT_ART
PMTCT_ART <- FY22Q4_Master_NonTier_Validations %>%
  filter(`indicator`== "PMTCT_ART")

check_2_19 <-  bind_rows(PMTCT_STAT_POS, PMTCT_ART) %>%
  group_by(mech_code, mech_uid, orgUnitName,orgUnit_uid, indicator, period, primepartner) %>%
  summarise(across(starts_with("value"), sum, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = "indicator", values_from = "value") %>%
  mutate(check = ifelse(`PMTCT_STAT` < `PMTCT_ART`, "PMTCT_STAT_POS < PMTCT_ART", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(primepartner,mech_code,level,today_date,check, period, orgUnitName)


#without HTS_RECENT

#without HTS_RECENT
level2_checks <- bind_rows( check_2_9,  check_2_10, check_2_13, check_2_15, check_2_16, check_2_17, check_2_19)

#ALL CHECK
level2_checks <- bind_rows( check_2_9,  check_2_10, check_2_11, check_2_13, check_2_15, check_2_16, check_2_17, check_2_19)
#ANOVA
anova_level_2 <- level2_checks %>%
  filter(primepartner == "ANOVA HEALTH INSTITUTE")

#BROADREACH
BR_level_2 <- level2_checks %>%
  filter(primepartner == "BROADREACH HEALTHCARE (PTY) LTD")

#MATCH
MATCH_level_2 <- level2_checks %>%
  filter(primepartner == "MATERNAL ADOLESCENT AND CHILD HEALTH INSTITUTE NPC")

#RIGHT TO CARE
RTC_level_2 <- level2_checks %>%
  filter(primepartner == "RIGHT TO CARE")

#WRHI 80007
WRHI_80007_level_2 <- level2_checks %>%
  filter(mech_code == 80007)

#WRHI 70301
WRHI_70301_level_2 <- level2_checks %>%
  filter(mech_code == 70301)


write_csv(anova_level_2, "ANOVA_Level2_NONTIERchecks_FY22Q3.csv")
write_csv(BR_level_2, "BroadReach_Level2_NONTIERchecks_FY22Q3.csv")
write_csv(MATCH_level_2, "MATCH_Level2_NONTIERchecks_FY22Q3.csv")
write_csv(RTC_level_2, "RTC_Level2_NONTIERchecks_FY22Q3.csv")
write_csv(WRHI_70301_level_2, "WRHI_70301_Level2_NONTIERchecks_FY22Q3.csv")

write_csv(level2_checks, "Level2_NONTIERchecks_readyforqc_FY22Q3.csv")

#Save all checks in master Excel workbook
Master_NONTIER_wb<- openxlsx::loadWorkbook("Dataout/MASTER MER NONTIER DQRT Tracker.xlsx")

openxlsx::writeData(Master_NONTIER_wb, sheet = "Level1", x = level1_checks,
                    colNames=T, withFilter=T)
openxlsx::writeData(Master_NONTIER_wb, sheet = "Level2", x = level2_checks,
                    colNames=T, withFilter=T)
openxlsx::saveWorkbook(Master_NONTIER_wb, "Dataout/MASTER MER NONTIER DQRT Tracker.xlsx", overwrite = TRUE)


