#MER mapping file
mapping_df <- reference_folder %>%
  glamr::return_latest("RTC_DATIM MER TIER Results Consolidated_Workfilev2.xlsx") %>%
  readxl::read_xlsx()

#create indicator and N/D variable in mapping file
df_map_clean <- mapping_df %>%
  # count(`Datim UID`) %>%
  dplyr::mutate(indicator = stringr::str_extract(`Datim UID`, "[^ (]+"),
         numeratordenom = stringr::str_extract(`Datim UID`, "[^,]+"),
         numeratordenom = stringr::str_sub(numeratordenom, start= -1),
         CoarseAgeGroup = ifelse(stringr::str_detect(dataElement, "TX_RTT") & CoarseAgeGroup %in% c("50-54", "55-59", "60-64", "65+"),
                                 "50+", CoarseAgeGroup),
         DSD_TA = stringr::str_split(`Datim UID`, ", ") %>% unlist() %>% dplyr::nth(2),
         indicator = ifelse(indicator == "PMTCT_HEI_POS" & dataElement == "PMTCT_HEI_POS (N, DSD, Age/HIVStatus/ARTStatus): Infant Testing",
                            "PMTCT_HEI_POS_ART", indicator)) %>%
  dplyr::select(-c(Total))

#get distinct
df_map_distinct <- df_map_clean %>% dplyr::distinct(`Test Resuts/Outcome/Duration`, Sex, CoarseAgeGroup, Result, indicator, numeratordenom,
                                             dataElement, dataElement_uid, categoryOptionComboName, categoryOptionCombo_uid, `Support Type`)

#because mapping file is incomplete, read in additional mapping file
# additional_map <- reference_folder %>%
#   glamr::return_latest("additional-disagg-mapping-fy22q3") %>%
#   readxl::read_xlsx()
#
# additional_map <- additional_map %>%
#   dplyr::select(`Test Resuts/Outcome/Duration`, Sex, CoarseAgeGroup, Result, dataElement, dataElement_uid,
#          categoryOptionComboName, categoryOptionCombo_uid, DSD_TA, indicator, numeratordenom) %>%
#   dplyr::distinct() %>%
#   dplyr::rename(`Test Resuts/Outcome/Duration` = `Test Resuts/Outcome/Duration`,
#          `Support Type` = DSD_TA)

#bind mapping files
df_map_distinct <- df_map_distinct
#%>%
  #dplyr::bind_rows(additional_map)

# send this up to drive for now

#add file to drive

map_id <- "1-fZqjcK0F5iprJIg1lpITTjw4rYtQbCmnBs9-buxhUY"
map_id <- googledrive::drive_create(name = "USAID disaggregate mapping file", path = "Partner Reporting/Panagora/USAID SI Data Management & Reporting/MER/DATIM Import Files/Mapping File", type = "spreadsheet")

#read to google sheets
googlesheets4::sheet_write(data = df_map_distinct, ss = map_id)

# now, let's identify what disagg combos we still need: ------------------------

#MER mapping file
full_map <- reference_folder %>%
  glamr::return_latest("RTC_DATIM MER TIER Results Consolidated_Workfile.xlsx") %>%
  readxl::read_xlsx(sheet = "Sheet2")

df_map_test <- df_map_distinct %>%
  dplyr::distinct(dataElement, dataElement_uid, categoryOptionComboName, categoryOptionCombo_uid)

full_map <- full_map %>%
  dplyr::distinct(dataelementname, dataelement_uid, categoryoptioncomboname, categoryoptioncombo_uid)


test <- dplyr::anti_join(full_map, df_map_test, by = c("dataelementname" = "dataElement",
                                               'dataelement_uid' = 'dataElement_uid',
                                               'categoryoptioncomboname' = 'categoryOptionComboName',
                                               'categoryoptioncombo_uid' = 'categoryOptionCombo_uid'))

today <- lubridate::today()

readr::write_csv(test, glue::glue('data-raw/missing-disaggs-{today}.csv'))
