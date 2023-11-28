# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  non-TIER DQRT
# REF ID:   df2e3b9f
# LICENSE:  MIT
# DATE:     2023-08-29
# UPDATED: 2023-11-27 (FY23Q4)

# DEPENDENCIES ------------------------------------------------------------

library(glamr)
library(tidyverse)
library(gophr)
library(glue)
library(openxlsx)

# GLOBAL VARIABLES --------------------------------------------------------

genie_folderpath <- "data-raw/MSD-Genie"

#change filename as needed
  #genie filtered for USAID + FY23
file_path <- genie_folderpath %>%
  return_latest("Genie-SiteByIMs-South-Africa-Daily-2023-11-27")

# IMPORT ------------------------------------------------------------------

df_msd <- read_psd(file_path)

tierdrop::get_meta("FY23Q4")

curr_fy_lab <- "FY23"
prev_pd <- glue("{curr_fy_lab}{prev_qtr}")



#print to check
fiscal_quarter
curr_fy
prev_pd
prev_fy

# MUNGE -------------------------------------------------------------------

#will take a WHILE to run
df_validation <- df_msd %>%
  filter(funding_agency == "USAID",
         fiscal_year %in% c(curr_fy),
         #source_name == "DATIM",
         standardizeddisaggregate == "Total Numerator") %>%
  group_by(snu1, psnu, sitename, facilityuid, prime_partner_name, mech_code, funding_agency,
           indicator,fiscal_year, standardizeddisaggregate) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop")

#will take a WHILE to run (check on reporting pd to ensure check is looking where you need)
df_target_result <- df_msd %>%
  filter(funding_agency == "USAID",
         fiscal_year %in% c(curr_fy),
         standardizeddisaggregate == "Total Numerator") %>%
  group_by(snu1, psnu, sitename, facilityuid, prime_partner_name, mech_code, funding_agency,
           indicator,fiscal_year, standardizeddisaggregate) %>%
  summarise(across(c(targets, cumulative), sum, na.rm = TRUE), .groups = "drop")

df_itt <- df_msd %>%
  filter(funding_agency == "USAID",
         fiscal_year %in% c(curr_fy),
         indicator == "TX_ML",
         standardizeddisaggregate == "Age/Sex/ARTNoContactReason/HIVStatus") %>%
  group_by(snu1, psnu, sitename, facilityuid, prime_partner_name, mech_code,
           funding_agency, indicator, fiscal_year, standardizeddisaggregate, otherdisaggregate) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop")

df_validation_denom <- df_msd %>%
  filter(funding_agency == "USAID",
         fiscal_year %in% c(curr_fy),
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
  group_by(snu1, psnu, sitename, facilityuid, prime_partner_name, mech_code, funding_agency,
           numeratordenom, indicator,fiscal_year, standardizeddisaggregate) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop")


# LEVEL 1 CHECKS -------------------------------------------------------

#just net-new: OKAY
check_1_1 <- df_validation %>%
  reshape_msd() %>%
  pivot_wider(names_from = "period") %>%
  mutate(check = ifelse(`FY23Q3` > 0 & `FY23Q4` == 0, "Value reported in previous quarter but missing in this quarter", NA),
         level = "Level 1",
         today_date = lubridate::today(),
         type_check = "Result in previous quarter") %>%
  filter(!is.na(check)) %>%
  mutate(period = fiscal_quarter) %>%
  select(mech_code, prime_partner_name, level, today_date, period, type_check, check, `FY23Q3`, `FY23Q4`, indicator, snu1, psnu, sitename)

check_1_1 <- check_1_1 %>%
  filter(indicator %ni% c("GEND_GBV", "GEND_GBV_PhysicalEmotionalViolence", "GEND_GBV_SexualViolence",
                          "TX_TB", "TB_PREV", "KP_PREV", "PP_PREV")) %>%
  filter(!str_detect(indicator, "OVC"))

check_1_1 %>%
  filter(mech_code == "70287") %>%
  write_csv(glue("data-raw/DATIM Genie checks/GENIE-BROADREACH_Level1_ALLCHECKS_{fiscal_quarter}.csv"))

check_1_1 %>%
  filter(mech_code == "81902") %>%
  write_csv(glue("data-raw/DATIM Genie checks/GENIE-MATCH_Level1_ALLCHECKS_{fiscal_quarter}.csv"))

check_1_1 %>%
  filter(mech_code == "70290") %>%
  write_csv(glue("data-raw/DATIM Genie checks/GENIE-RTC_Level1_ALLCHECKS_{fiscal_quarter}.csv"))

check_1_1 %>%
  filter(mech_code == "70310") %>%
  write_csv(glue("data-raw/DATIM Genie checks/GENIE-ANOVA_Level1_ALLCHECKS_{fiscal_quarter}.csv"))

check_1_1 %>%
  filter(mech_code == "80007") %>%
  write_csv(glue("data-raw/DATIM Genie checks/GENIE-WRHI_80007_Level1_ALLCHECKS_{fiscal_quarter}.csv"))

check_1_1 %>%
  filter(mech_code == "70301") %>%
  write_csv(glue("data-raw/DATIM Genie checks/GENIE-WRHI_70301_Level1_ALLCHECKS_{fiscal_quarter}.csv"))


# LEVEL 2 ---------------------------------------------------------------

#2.1 - PrEP_CT  Q4 < PrEP_CT Q3+ PREP_NEW Q4 (facility)
check_2_1 <- df_validation %>%
  filter(indicator %in% c("PrEP_NEW", "PrEP_CT")) %>%
  group_by(prime_partner_name,mech_code, snu1, psnu, sitename,facilityuid, indicator, standardizeddisaggregate, fiscal_year) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
  reshape_msd() %>%
  filter(period %in% c(fiscal_quarter ,prev_pd)) %>%
  mutate(indic_name = glue("{indicator}_{period}")) %>%
  #select(-c(indicator, period)) %>%
  # mutate(dataElement = str_extract(dataElement, "[^ (]+")) %>%
  #utate(dataElement_uid = indicator) %>%
  pivot_wider(names_from = "indic_name",values_from = "value") %>%
  # rename()
  mutate(check = ifelse(`PrEP_CT_FY23Q4` < `PrEP_CT_FY23Q3` + `PrEP_NEW_FY23Q4`,
                        "PrEP_CT  Q4 < PrEP_CT FY23Q3 + PREP_NEW Q4", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(mech_code, prime_partner_name, level, today_date, period, type_check, check, snu1, psnu, sitename)

#2.2 - PREP_CT<=Test result disagg

# df_msd %>%
#   filter(funding_agency == "USAID",
#          indicator== "PrEP_CT",
#          fiscal_year %in% c(2023),
#          standardizeddisaggregate == "Total Numerator") %>%
#   group_by(snu1, psnu, sitename, facilityuid, prime_partner_name, mech_code, funding_agency,
#            indicator,fiscal_year, standardizeddisaggregate) %>%
#   summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop")

#2.3 - PREP_CT<= KP Disagg

#2.4 - TB_PREV_D < TB_PREV_N (facility) - SEMI ANNUAL

check_2_4 <- df_validation_denom %>%
  filter(indicator %in% c("TB_PREV")) %>%
  group_by(prime_partner_name,mech_code, snu1, psnu, sitename,facilityuid, indicator, numeratordenom, fiscal_year) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
  reshape_msd() %>%
  filter(period == fiscal_quarter) %>%
  # mutate(dataElement = str_extract(dataElement, "[^ (]+")) %>%
  #utate(dataElement_uid = indicator) %>%
  pivot_wider(names_from = "numeratordenom", values_from = "value") %>%
  # rename()
  mutate(check = ifelse(`D` < `N`, "TB_PREV_D < TB_PREV_N", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(mech_code, prime_partner_name, level, today_date, type_check, period, check,snu1, psnu, sitename)


# 2.5 - TB_STAT numerator >  TB_STAT denominator

check_2_5 <- df_validation_denom %>%
  filter(indicator %in% c("TB_STAT")) %>%
  group_by(prime_partner_name,mech_code, snu1, psnu, sitename,facilityuid, indicator, numeratordenom, fiscal_year) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
  reshape_msd() %>%
  filter(period == fiscal_quarter) %>%
  pivot_wider(names_from = "numeratordenom", values_from = "value") %>%
  # rename()
  mutate(check = ifelse(`D` < `N`, "TB_STAT_N >  TB_STAT_D", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(mech_code, prime_partner_name, level, today_date, type_check, period, check,snu1, psnu, sitename)

#2.6 - TX_NEW > TX_CURR

check_2_6 <-  df_validation %>%
  filter(indicator %in% c("TX_NEW", "TX_CURR")) %>%
  group_by(prime_partner_name,mech_code, snu1, psnu, sitename,facilityuid, indicator, standardizeddisaggregate, fiscal_year) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
  reshape_msd() %>%
  filter(period == fiscal_quarter) %>%
  # mutate(dataElement = str_extract(dataElement, "[^ (]+")) %>%
  #utate(dataElement_uid = indicator) %>%
  pivot_wider(names_from = "indicator", values_from = "value") %>%
  # rename()
  mutate(check = ifelse(`TX_NEW` > `TX_CURR`, "TX_NEW > TX_CURR", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(mech_code, prime_partner_name, level, today_date, type_check, period, check, snu1, psnu, sitename)

# CHECK 2.7- - TX_ML
check_2_7 <- df_itt %>%
  filter(indicator %in% c("TX_ML")) %>%
  mutate(IIT = recode(otherdisaggregate, "No Contact Outcome - Interruption in Treatment <3 Months Treatment" = "IIT <3",
                      "No Contact Outcome - Interruption In Treatment 6+ Months Treatment" = "IIT 3+",
                      "No Contact Outcome - Interruption in Treatment 3-5 Months Treatment" = "IIT 3+")) %>%
  group_by(prime_partner_name,mech_code, snu1, psnu, sitename,facilityuid, indicator, IIT, fiscal_year) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
  reshape_msd() %>%
  filter(period == fiscal_quarter,
         !str_detect(IIT, "No Contact Outcome")) %>%
  #  count(otherdisaggregate, IIT)
  # mutate(dataElement = str_extract(dataElement, "[^ (]+")) %>%
  #utate(dataElement_uid = indicator) %>%
  pivot_wider(names_from = "IIT", values_from = "value") %>%
  # rename()
  mutate(check = ifelse(`IIT <3` > `IIT 3+`, "TX ML IIT <3 > IIT 3+", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(mech_code, prime_partner_name, level, today_date, type_check, period, check, snu1, psnu, sitename)

#2.8 - TX_RTT > TX_CURR

check_2_8 <- df_validation %>%
  filter(indicator %in% c("TX_RTT", "TX_CURR")) %>%
  group_by(prime_partner_name,mech_code, snu1, psnu, sitename,facilityuid, indicator, standardizeddisaggregate, fiscal_year) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
  reshape_msd() %>%
  filter(period == fiscal_quarter) %>%
  # mutate(dataElement = str_extract(dataElement, "[^ (]+")) %>%
  #utate(dataElement_uid = indicator) %>%
  pivot_wider(names_from = "indicator", values_from = "value") %>%
  # rename()
  mutate(check = ifelse(`TX_RTT` > `TX_CURR`, "TX_RTT > TX_CURR", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(mech_code, prime_partner_name, level, today_date, type_check, period, check, snu1, psnu, sitename)

#2.9 - TX_PVLS numerator > TX_PVLS denominator

check_2_9 <- df_validation_denom %>%
  filter(indicator %in% c("TX_PVLS")) %>%
  group_by(prime_partner_name,mech_code, snu1, psnu, sitename,facilityuid, indicator, numeratordenom, fiscal_year) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
  reshape_msd() %>%
  filter(period == fiscal_quarter) %>%
  # mutate(dataElement = str_extract(dataElement, "[^ (]+")) %>%
  #utate(dataElement_uid = indicator) %>%
  pivot_wider(names_from = "numeratordenom", values_from = "value") %>%
  # rename()
  mutate(check = ifelse(`D` < `N`, "TV_PVLS_N >  TX_PVLS_D", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(mech_code, prime_partner_name, level, today_date, type_check, period, check,snu1, psnu, sitename)


#2.10 - Index_offered<Index_accepted (facility) [NON-TIER]
check_2_10 <- df_msd %>%
  filter(indicator %in% c("HTS_INDEX"),
         funding_agency == "USAID") %>%
  filter(standardizeddisaggregate %in% c("1:Age/Sex/IndexCasesOffered","2:Age/Sex/IndexCasesAccepted")) %>%
  group_by(prime_partner_name,mech_code , snu1, psnu, sitename, facilityuid, indicator, standardizeddisaggregate, fiscal_year) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
  reshape_msd() %>%
  filter(period == fiscal_quarter) %>%
  pivot_wider(names_from = "standardizeddisaggregate", values_from = "value") %>%
  mutate(check = ifelse(`1:Age/Sex/IndexCasesOffered` < `2:Age/Sex/IndexCasesAccepted`, "Index_offered<Index_accepted", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(mech_code, prime_partner_name,level,today_date, type_check, check, period, snu1, psnu, sitename)

#2.11 - Index_contacts <contacts_ new_pos + contacts_ new_neg + contacts_ known_pos (facility)
check_2_11 <- df_msd %>%
  filter(indicator %in% c("HTS_INDEX"),
         funding_agency == "USAID") %>%
  filter(standardizeddisaggregate %in% c("3:Age Aggregated/Sex/Contacts", "4:Age/Sex/Result")) %>%
  group_by(prime_partner_name,mech_code , snu1, psnu, sitename, facilityuid, indicator,
           standardizeddisaggregate, otherdisaggregate, statushiv, fiscal_year) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
  reshape_msd() %>%
  filter(period == fiscal_quarter) %>%
  mutate(disagg_name = glue("{otherdisaggregate}_{statushiv}")) %>%
  mutate(disagg_name = recode(disagg_name, "NA_NA" = "Total Contacts",
                              "NA_Negative" = "Negatives")) %>%
  select(-c(standardizeddisaggregate:statushiv)) %>%
  # filter(!str_detect(disagg_name, "NA")) %>%
  #count(disagg_name, standardizeddisaggregate, otherdisaggregate, statushiv
  pivot_wider(names_from = "disagg_name", values_from = "value") %>%
  mutate(check = ifelse(`Total Contacts` <
                          `Known at Entry_Positive` + `Newly Identified_Negative` + `Newly Identified_Positive`,
                        "Index_contacts <contacts_ new_pos + contacts_ new_neg + contacts_ known_pos", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(mech_code, prime_partner_name,level,today_date, type_check, check, period, snu1, psnu, sitename)


#2.12 - HTS_TST_POS (>=15) >=HTS_RECENT #note, no partners reported HTS_RECENT as of 7.29
HTS_TST_POS_15plus <- df_msd %>%
  filter(funding_agency == "USAID") %>%
  filter((`indicator`== "HTS_TST" & (categoryoptioncomboname == "15-19, Male, Positive" | categoryoptioncomboname ==  "15-19, Female, Positive"
                                     | categoryoptioncomboname == "20-24, Male, Positive" | categoryoptioncomboname ==  "20-24, Female, Positive"
                                     | categoryoptioncomboname == "25-29, Male, Positive" | categoryoptioncomboname ==  "25-29, Female, Positive"
                                     | categoryoptioncomboname == "30-34, Male, Positive" | categoryoptioncomboname ==  "30-34, Female, Positive"
                                     | categoryoptioncomboname == "35-39, Male, Positive" | categoryoptioncomboname ==  "35-39, Female, Positive"
                                     | categoryoptioncomboname == "40-44, Male, Positive" | categoryoptioncomboname ==  "40-44, Female, Positive"
                                     | categoryoptioncomboname == "45-49, Male, Positive" | categoryoptioncomboname ==  "45-49, Female, Positive"
                                     | categoryoptioncomboname == "50+, Male, Positive" | categoryoptioncomboname ==  "50+, Female, Positive")))


HTS_RECENT <- df_msd %>%
  filter(funding_agency == "USAID") %>%
  filter(indicator %in% c("HTS_RECENT"),
         fiscal_year %in% c(curr_fy)) %>%
  group_by(prime_partner_name,mech_code , snu1, psnu, sitename, facilityuid, indicator, fiscal_year) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop")


check_2_12 <- bind_rows(HTS_TST_POS_15plus, HTS_RECENT) %>%
  group_by(prime_partner_name,mech_code , snu1, psnu, sitename, facilityuid, indicator, fiscal_year) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
  reshape_msd() %>%
  filter(period == fiscal_quarter) %>%
  pivot_wider(names_from = "indicator", values_from = "value") %>%
  mutate(check = ifelse(`HTS_TST` >= `HTS_RECENT`, "HTS_TST_POS (>=15) >=HTS_RECENT", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(mech_code, prime_partner_name,level,today_date, type_check, check, period, snu1, psnu, sitename)

#2.13 - HTS_TST<HTS_TST_POS
check_2_13 <-  df_msd %>%
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS")) %>%
  # count(indicator, standardizeddisaggregate, otherdisaggregate, statushiv)
  group_by(prime_partner_name, mech_code , snu1, psnu, sitename, facilityuid, indicator, fiscal_year) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
  reshape_msd() %>%
  filter(period == fiscal_quarter) %>%
  pivot_wider(names_from = "indicator", values_from = "value") %>%
  mutate(check = ifelse(`HTS_TST` < `HTS_TST_POS`, "HTS_TST < HTS_TST_POS", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(mech_code, prime_partner_name,level,today_date, type_check, check, period, snu1, psnu, sitename)

#2.14 - PMTCT_EID 0-2months > PMTCT_STAT_POS+HTS_TST_POS_PMTCT PostANC1/Age/Sex/Result
PMTCT_EID_0_2months <- df_msd %>%
  filter(funding_agency == "USAID") %>%
  filter((`indicator`== "PMTCT_EID"
          & categoryoptioncomboname== "<= 2 months"
  ))


PMTCT_STAT_POS <- df_msd %>%
  filter(funding_agency == "USAID") %>%
  filter(indicator== "PMTCT_STAT_POS",
         numeratordenom == "N",
         standardizeddisaggregate == "Age/Sex/KnownNewResult"
  )

HTS_TST_POS_PMTCT <-df_msd %>%
  filter(funding_agency == "USAID") %>%
  filter(indicator == "HTS_TST",
         standardizeddisaggregate == "Modality/Age/Sex/Result",
         modality == "PMTCT ANC",
         statushiv == "Positive")

check_2_14 <- bind_rows(PMTCT_EID_0_2months, PMTCT_STAT_POS, HTS_TST_POS_PMTCT) %>%
  group_by(prime_partner_name, mech_code , snu1, psnu, sitename, facilityuid, indicator, fiscal_year) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
  reshape_msd() %>%
  filter(period == fiscal_quarter) %>%
  pivot_wider(names_from = "indicator", values_from = "value") %>%
  mutate(PMTCT_POS = PMTCT_STAT_POS + HTS_TST,
         check = ifelse(PMTCT_EID > PMTCT_POS, "PMTCT_EID 0-2months > PMTCT_STAT_POS+HTS_TST_POS_PMTCT", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(mech_code, prime_partner_name,level,today_date,type_check, check, period, snu1, psnu, sitename)


#2.15 - PMTCT_HEI_POS > PMTCT_EID
PMTCT_EID <- df_msd %>%
  filter(funding_agency == "USAID") %>%
  filter(`indicator`== "PMTCT_EID",
         standardizeddisaggregate == "Total Numerator")


PMTCT_HEI_POS <- df_msd %>%
  filter(funding_agency == "USAID") %>%
  filter(`indicator`== "PMTCT_HEI_POS",
         standardizeddisaggregate == "Age/HIVStatus")

check_2_15 <-  bind_rows(PMTCT_EID,PMTCT_HEI_POS) %>%
  group_by(prime_partner_name, mech_code , snu1, psnu, sitename, facilityuid, indicator, fiscal_year) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
  reshape_msd() %>%
  filter(period == fiscal_quarter) %>%
  pivot_wider(names_from = "indicator", values_from = "value") %>%
  mutate(check = ifelse(`PMTCT_HEI_POS` > `PMTCT_EID`, "PMTCT_HEI_POS > PMTCT_EID", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(mech_code, prime_partner_name,level,today_date,type_check, check, period, snu1, psnu, sitename)


#2.16 - PMTCT_STAT numerator > PMTCT_STAT denominator
check_2_16 <-  df_msd %>%
  filter(funding_agency == "USAID") %>%
  filter(indicator %in% c("PMTCT_STAT"),
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
  group_by(prime_partner_name, mech_code , snu1, psnu, sitename, facilityuid, indicator, standardizeddisaggregate, fiscal_year) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
  reshape_msd() %>%
  filter(period == fiscal_quarter) %>%
  pivot_wider(names_from = "standardizeddisaggregate", values_from = "value") %>%
  mutate(check = ifelse(`Total Numerator` > `Total Denominator`, "PMTCT_STAT_N >  PMTCT_STAT_D", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(mech_code, prime_partner_name,level,today_date, type_check, check, period, snu1, psnu, sitename)


#2.17 - PMTCT_STAT_POS < PMTCT_ART
PMTCT_ART <- df_msd %>%
  filter(funding_agency == "USAID") %>%
  filter(`indicator`== "PMTCT_ART",
         standardizeddisaggregate == "Age/NewExistingArt/Sex/HIVStatus")


check_2_17 <-  bind_rows(PMTCT_STAT_POS, PMTCT_ART) %>%
  group_by(prime_partner_name, mech_code , snu1, psnu, sitename, facilityuid, indicator, fiscal_year) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
  reshape_msd() %>%
  filter(period == fiscal_quarter) %>%
  pivot_wider(names_from = "indicator", values_from = "value") %>%
  mutate(check = ifelse(`PMTCT_STAT_POS` < `PMTCT_ART`, "PMTCT_STAT_POS < PMTCT_ART", NA),
         level = "Level 2",
         today_date = lubridate::today(),
         type_check = "IntraIndicator") %>%
  filter(!is.na(check)) %>%
  select(mech_code, prime_partner_name,level,today_date, type_check, check, period, snu1, psnu, sitename)



#BIND

#without HTS_RECENT
  # comment out the empty ones
level2_checks_nonTIER <- bind_rows(
#  check_2_10,
  check_2_11,
  check_2_12,
  check_2_13,
  check_2_14,
  check_2_15,
  check_2_16,
  check_2_17,
  check_2_6,
  check_2_7) %>%
  select(mech_code, prime_partner_name, level, today_date, type_check, check,
         period, snu1, psnu, sitename)

# BIND CHECKS ---------------------------------------------------------------------



#ALL CHECK - comment out the empty ones
level2_checks_all <- bind_rows(
  # check_2_1,
  check_2_4,
  check_2_5,
  check_2_6,
  check_2_7,
  # check_2_8,
  # check_2_9,
  #  check_2_10,
  check_2_11,
  check_2_12,
  check_2_13,
  check_2_14,
  check_2_15,
  check_2_16,
  check_2_17)

#ALL CHECK
level2_checks_all_no_recency <- bind_rows(
  # check_2_1,
  check_2_4,
  check_2_5,
  check_2_6,
  check_2_7,
  # check_2_8,
  # check_2_9,
  #  check_2_10,
  check_2_11,
  #  check_2_12,
  check_2_13,
  check_2_14,
  check_2_15,
  check_2_16,
  check_2_17)



#ANOVA
anova_level_2_all <- level2_checks_nonTIER %>%
  select(mech_code, prime_partner_name, level, today_date, type_check, period,
         check, snu1, psnu, sitename) %>%
  filter(mech_code == "70310")

#BROADREACH
BR_level_2_all <- level2_checks_nonTIER %>%
  select(mech_code, prime_partner_name, level, today_date, type_check, period,
         check, snu1, psnu, sitename) %>%
  filter(mech_code == "70287")


#MATCH
MATCH_level_2_all <- level2_checks_nonTIER %>%
  select(mech_code, prime_partner_name, level, today_date, type_check, period,
         check, snu1, psnu, sitename) %>%
  filter(mech_code == "81902")


#RIGHT TO CARE
RTC_level_2_all <-level2_checks_nonTIER %>%
  select(mech_code, prime_partner_name, level, today_date, type_check, period,
         check, snu1, psnu, sitename) %>%
  filter(mech_code == "70290")

#WRHI 70301
WRHI_level_2_all_70301 <- level2_checks_nonTIER %>%
  select(mech_code, prime_partner_name, level, today_date, type_check, period,
         check, snu1, psnu, sitename) %>%
  filter(prime_partner_name == "WITS HEALTH CONSORTIUM (PTY) LTD",
         mech_code == "70301")

#WRHI 80007
WRHI_level_2_all_80007 <- level2_checks_nonTIER%>%
  select(mech_code, prime_partner_name, level, today_date, type_check, period,
         check, snu1, psnu, sitename) %>%
  filter(prime_partner_name == "WITS HEALTH CONSORTIUM (PTY) LTD",
         mech_code == "80007")


write_csv(anova_level_2_all, glue("data-raw/DATIM Genie checks/GENIE-ANOVA_Level2_ALLCHECKS_{fiscal_quarter}.csv"))
write_csv(BR_level_2_all, glue("data-raw/DATIM Genie checks/GENIE-BroadReach_Level2_ALLCHECKS_{fiscal_quarter}.csv"))
write_csv(MATCH_level_2_all, glue("data-raw/DATIM Genie checks/GENIE-MATCH_Level2_ALLCHECKS_{fiscal_quarter}.csv"))
write_csv(RTC_level_2_all, glue("data-raw/DATIM Genie checks/GENIE-RTC_Level2_ALLCHECKS_{fiscal_quarter}.csv"))
write_csv(WRHI_level_2_all_70301, glue("data-raw/DATIM Genie checks/GENIE-WRHI_70301_Level2_ALLCHECKS_{fiscal_quarter}.csv"))
write_csv(WRHI_level_2_all_80007, glue("data-raw/DATIM Genie checks/GENIE-WRHI_80007_Level2_ALLCHECKS_{fiscal_quarter}.csv"))

