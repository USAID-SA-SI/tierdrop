#' Mapping DATIM UIDS to NDOH tab structure
#'
#' @param df NDOH dataset to map DATIM hierachy into
#' @param ind_sel Selected indicators to map - default is "All" indicators,
#' which removes PrEP_CT and TX_RTT as they must be mapped separately ("All", "PrEP_CT", "TX_RTT")
#' @param disaggregate Selected disaggregate group to map - default is for all indicators (minus PrEP_CT & TX_RTT)
#' i.e. PrEP_CT Disaggs: (1) "Age/Sex" (2) "TestResult", TX_RTT Disaggs: (1) "Age/Sex/HIVStatus" (2) "ARTNoContactReasonIIT
#' @param all_indic TRUE if data for all indicators (minus PrEP_CT and TX_RTT) are being mapped, FALSE if not
#'
#' @return
#' @export
#'
#' @examples
#'  \dontrun{
#'    map_disaggs(df = ndoh_clean, ind_sel = "PrEP_CT", disaggregate ="Age/Sex", all_indic = FALSE)
#'    map_disaggs(df = ndoh_clean, all_indic = TRUE)
#' }
map_disaggs <- function(df, ind_sel = "All", disaggregate = "All", all_indic = TRUE) {

  #Read in mapping file from google drive
  df_map_distinct <- googlesheets4::read_sheet(disagg_map_id) %>%
    dplyr::rename("Test Result/Outcome/Duration" = "Test Resuts/Outcome/Duration",
                  "DSD_TA" = "Support Type")

  #grab column names for NDOH
  col_names <- df %>%
    names()

  if (all_indic == TRUE) {


    group_vars <- col_names[col_names %ni% c("usaid_facility", "ou5uid", "datim_uid",
                                                 'old_ou5code', 'period', 'Province', 'District',
                                                 'SubDistrict', 'Facility', "Total")]

    ndoh_disagg <- df %>%
      dplyr::filter(indicator %ni% c("PrEP_CT", "TX_RTT")) %>%
      dplyr::left_join(df_map_distinct, by = c(group_vars)) %>%
      dplyr::distinct() %>%
      dplyr::left_join(mech_df, by = c("datim_uid" = "facilityuid"))

  } else {

    col_names <- col_names[col_names %ni% c("Total")]

    if (ind_sel == "PrEP_CT" & disaggregate == "Age/Sex") {
      unselect_vars <- c("Result")
    } else if (ind_sel == "PrEP_CT" & disaggregate == "TestResult") {
      unselect_vars <- c("Sex", "CoarseAgeGroup")
    } else if (ind_sel == "TX_RTT" & disaggregate == "Age/Sex/HIVStatus") {
      unselect_vars <- c("Test Result/Outcome/Duration")
    } else if (ind_sel == "TX_RTT" & disaggregate == "ARTNoContactReasonIIT") {
      unselect_vars <- c("Sex", "CoarseAgeGroup", "Result")
    }

    select_vars <- col_names[col_names %ni% c(unselect_vars)]
    group_vars <- select_vars[select_vars %ni% c("usaid_facility", "ou5uid", "datim_uid",
                                                 'old_ou5code', 'period', 'Province', 'District',
                                                 'SubDistrict', 'Facility')]

    ndoh_disagg <- df %>%
      dplyr::filter(indicator %in% ind_sel) %>%
      dplyr::group_by(.[,select_vars]) %>%
      dplyr::summarise(dplyr::across(tidyselect::starts_with("Total"), sum, na.rm = TRUE), .groups = "drop") %>%
      dplyr::left_join(df_map_distinct %>%
                         dplyr::select(-c(unselect_vars)) %>%
                         dplyr::filter(stringr::str_detect(dataElement, disaggregate)), by = c(group_vars)) %>%
      dplyr::distinct() %>%
      dplyr::left_join(mech_df, by = c("datim_uid" = "facilityuid"))


  }

  # #this is the format for the DATIM import file
  # ndoh_disagg <- ndoh_disagg %>%
  #   dplyr::select(mech_code, mech_uid, Facility, datim_uid, dataElement, dataElement_uid,categoryOptionComboName, categoryOptionCombo_uid,
  #                 Total, SubDistrict, District) %>%
  #   dplyr::rename(value = Total,
  #                 orgUnit_uid = datim_uid,
  #                 orgUnitName = Facility)

  return(ndoh_disagg)

}


#' NDOH Post Processing Function
#'
#' @description This function maps in DATIM elements using `map_disaggs()`, binds all the indicator levels together &
#'  exports processed data in import file or validation file format
#' @param df dataframe of interest
#' @param kp parameter to map and process KP tabs if KP = TRUE (default KP = FALSE)
#' @param export_type parameter to extract data in specific format (1) DATIM import file (2) Validation file for internal checks
#'
#' @return
#' @export
#'
#' @examples
#'  \dontrun{
#'    ndoh_post_processing(kp = FALSE, export_type = "Import")
#' }
ndoh_post_processing <- function(df, kp = FALSE, export_type) {
#
# if (kp == FALSE) {
#   df <- ndoh_clean
# } else {
#   df <- ndoh_clean_kp
# }

df_all <- map_disaggs(df, all_indic = TRUE)
df_prep_test <- map_disaggs(df, "PrEP_CT", "TestResult", FALSE)
df_prep_age <- map_disaggs(df, "PrEP_CT", "Age/Sex", FALSE)
df_rtt_age <- map_disaggs(df, "TX_RTT", "Age/Sex/HIVStatus", FALSE)
df_rtt_nocontact <- map_disaggs(df, "TX_RTT", "ARTNoContactReasonIIT", FALSE)

df_final <- dplyr::bind_rows(df_all,
                             df_prep_test,
                             df_prep_age,
                             df_rtt_age,
                             df_rtt_nocontact)


if (export_type == "Import") {
  keep_vars <- c("mech_uid","datim_uid",
                 "dataElement_uid","categoryOptionCombo_uid",
                 "Total")
} else if (export_type == "Validation") {
  keep_vars <- c("period","Province", "District","SubDistrict", "Facility",
                 "datim_uid", "mech_code", "mech_uid", 'prime_partner_name',
                 "indicator", "numeratordenom","Test Result/Outcome/Duration", "Sex",
                 "CoarseAgeGroup", "Result","dataElement", "dataElement_uid", "categoryOptionComboName",
                 "categoryOptionCombo_uid", "Total")
}

df_final <- df_final %>%
  dplyr::select(keep_vars) %>%
  dplyr::rename(value = Total,
                  orgUnit_uid = datim_uid)
# %>%
#   dplyr::mutate(period = glue::glue(stringr::str_sub(fiscal_quarter, 1, 4), quarters[qtr_num-1]))


return(df_final)



}



