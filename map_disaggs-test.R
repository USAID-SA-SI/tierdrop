#Read in mapping file from google drive
df_map_distinct <- googlesheets4::read_sheet(disagg_map_id) %>%
  dplyr::rename("Test Result/Outcome/Duration" = "Test Resuts/Outcome/Duration",
                "DSD_TA" = "Support Type")


ndoh_mapped <- ndoh_clean %>%
  dplyr::filter(indicator %ni% c("PrEP_CT", "TX_RTT")) %>%
  dplyr::left_join(df_map_distinct, by = c("Test Result/Outcome/Duration" = "Test Resuts/Outcome/Duration",
                                    "Sex", "CoarseAgeGroup", "Result", "indicator", "numeratordenom", "DSD_TA" = "Support Type"))

#join mech metadata
ndoh_mapped <- ndoh_mapped %>%
  dplyr::left_join(mech_df, by = c("datim_uid" = "facilityuid"))
# %>%
#   left_join(mech_xwalk, by = c('mech_code'))

# ndoh_mapped <- ndoh_mapped %>%
#   select(-c(mech_name.y)) %>%
#   rename(mech_name = mech_name.x)


#check for missing disagg maps
missing_disagg2 <-  ndoh_mapped %>%
  dplyr::filter(is.na(dataElement),
         !is.na(District)) %>%
  dplyr::select(indicator, DSD_TA, District, Facility, Sex, CoarseAgeGroup, `Test Result/Outcome/Duration`,
         Result, numeratordenom, dataElement)


#address PrEP_CT TEST RESULT AND AGE/SEX DISAGG -------------------------------

#first, map to the age groups

ndoh_prep <- ndoh_clean %>%
  dplyr::filter(indicator == "PrEP_CT") %>%
  dplyr::group_by(usaid_facility, ou5uid, datim_uid, old_ou5code, period, DSD_TA,
           Province, District, SubDistrict, Facility, `Test Result/Outcome/Duration`,
           Sex, CoarseAgeGroup, indicator, numeratordenom) %>%
  dplyr::summarise(dplyr::across(starts_with("Total"), sum, na.rm = TRUE), .groups = "drop")


ndoh_prep_age <- ndoh_prep %>%
  dplyr::left_join(df_map_distinct %>% dplyr::select(-c(Result)), by = c("Test Result/Outcome/Duration" = "Test Resuts/Outcome/Duration",
                                                           "Sex", "CoarseAgeGroup", "indicator", "numeratordenom", "DSD_TA" = "Support Type")) %>%
  dplyr::distinct() %>%
  dplyr::left_join(mech_df, by = c("datim_uid" = "facilityuid"))
# %>%
#   dplyr::left_join(mech_xwalk, by = c('mech_code'))
# %>%
#   select(-c(mech_name.y)) %>%
#   rename(mech_name = mech_name.x)

ndoh_prep_age <- ndoh_prep_age %>%
  dplyr::select(mech_code, mech_uid, Facility, datim_uid, dataElement, dataElement_uid,categoryOptionComboName, categoryOptionCombo_uid,
         Total, SubDistrict, District) %>%
  dplyr::rename(value = Total,
         orgUnit_uid = datim_uid,
         orgUnitName = Facility)

#address PrEP_CT - now, map test result
ndoh_prep_test <- ndoh_clean %>%
  dplyr::filter(indicator == "PrEP_CT") %>%
  dplyr::group_by(usaid_facility, ou5uid, datim_uid, old_ou5code, period, DSD_TA,
           Province, District, SubDistrict, Facility, `Test Result/Outcome/Duration`,
           Result, indicator, numeratordenom) %>%
  dplyr::summarise(dplyr::across(starts_with("Total"), sum, na.rm = TRUE), .groups = "drop")

ndoh_prep_test <- ndoh_prep_test %>%
  dplyr::left_join(df_map_distinct %>%
                     dplyr::select(-c(Sex, CoarseAgeGroup)) %>%
                     dplyr::filter(stringr::str_detect(dataElement, "TestResult")),
            by = c("Test Result/Outcome/Duration" = "Test Resuts/Outcome/Duration",
                   "Result","indicator", "numeratordenom", "DSD_TA" = "Support Type")) %>%
  dplyr::distinct() %>%
  dplyr::left_join(mech_df, by = c("datim_uid" = "facilityuid"))
# %>%
#   left_join(mech_xwalk, by = c('mech_code'))
# %>%
#   select(-c(mech_name.y)) %>%
#   rename(mech_name = mech_name.x)

ndoh_prep_test <- ndoh_prep_test %>%
  dplyr::select(mech_code, mech_uid, Facility, datim_uid, dataElement, dataElement_uid,categoryOptionComboName, categoryOptionCombo_uid,
         Total, SubDistrict, District) %>%
  dplyr::rename(value = Total,
         orgUnit_uid = datim_uid,
         orgUnitName = Facility)

ndoh_prep_final <- dplyr::bind_rows(ndoh_prep_age, ndoh_prep_test)


#janitor::get_dupes(ndoh_prep_final) %>% view()


#TX RTT --------------------------------------------------------------------------

#first, map to the age groups

ndoh_rtt_age_sex <- ndoh_clean %>%
  dplyr::filter(indicator == "TX_RTT") %>%
  dplyr::group_by(usaid_facility, ou5uid, datim_uid, old_ou5code, period, DSD_TA,
           Province, District, SubDistrict, Facility,
           Sex, CoarseAgeGroup, Result, indicator, numeratordenom) %>%
  dplyr::summarise(dplyr::across(starts_with("Total"), sum, na.rm = TRUE), .groups = "drop")

ndoh_rtt_age_sex <- ndoh_rtt_age_sex %>%
  dplyr::left_join(df_map_distinct %>%
                     dplyr::select(-c(`Test Resuts/Outcome/Duration`)) %>%
                     dplyr::filter(stringr::str_detect(dataElement, "Age/Sex/HIVStatus")), by = c("Sex", "CoarseAgeGroup", "Result","indicator",
                                                                           "numeratordenom", "DSD_TA" = "Support Type")) %>%
  dplyr::distinct() %>%
  dplyr::left_join(mech_df, by = c("datim_uid" = "facilityuid"))
# %>%
#   left_join(mech_xwalk, by = c('mech_code'))
# %>%
#   select(-c(mech_name.y)) %>%
#   rename(mech_name = mech_name.x)

ndoh_rtt_age_sex <- ndoh_rtt_age_sex %>%
  dplyr::select(mech_code, mech_uid, Facility, datim_uid, dataElement, dataElement_uid,categoryOptionComboName, categoryOptionCombo_uid,
         Total, SubDistrict, District) %>%
  dplyr::rename(value = Total,
         orgUnit_uid = datim_uid,
         orgUnitName = Facility)

#address TX_RTT = no contact reason
ndoh_rtt_nocontact <- ndoh_clean %>%
  dplyr::filter(indicator == "TX_RTT") %>%
  dplyr::group_by(usaid_facility, ou5uid, datim_uid, old_ou5code, period, DSD_TA,
           Province, District, SubDistrict, Facility,
           `Test Result/Outcome/Duration`, indicator, numeratordenom) %>%
  dplyr::summarise(dplyr::across(starts_with("Total"), sum, na.rm = TRUE), .groups = "drop")

ndoh_rtt_nocontact <- ndoh_rtt_nocontact %>%
  dplyr::left_join(df_map_distinct %>%
                     dplyr::select(-c(Sex, CoarseAgeGroup, Result)) %>%
                     dplyr::filter(stringr::str_detect(dataElement, "ARTNoContactReasonIIT")),
            by = c("Test Result/Outcome/Duration" = "Test Resuts/Outcome/Duration",
                   "indicator", "numeratordenom", "DSD_TA" = "Support Type")) %>%
  dplyr::distinct() %>%
  dplyr::left_join(mech_df, by = c("datim_uid" = "facilityuid"))
# %>%
#   left_join(mech_xwalk, by = c('mech_code'))
# %>%
#   select(-c(mech_name.y)) %>%
#   rename(mech_name = mech_name.x)

ndoh_rtt_nocontact <- ndoh_rtt_nocontact %>%
  dplyr::select(mech_code, mech_uid, Facility, datim_uid, dataElement, dataElement_uid,categoryOptionComboName, categoryOptionCombo_uid,
         Total, SubDistrict, District) %>%
  dplyr::rename(value = Total,
         orgUnit_uid = datim_uid,
         orgUnitName = Facility)

ndoh_rtt_all <- dplyr::bind_rows(ndoh_rtt_age_sex, ndoh_rtt_nocontact)
