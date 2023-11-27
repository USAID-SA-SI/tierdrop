#' Tidy / munge NDOH import
#'
#' @param df imported dataframe to pass in
#' @param kp parameter to tidy KP tabs if KP = TRUE (default KP = FALSE)
#'
#' @return
#' @export
#'
#' @examples
#'  \dontrun{
#'    tidy_ndoh(ndoh_all, kp = FALSE)
#' }
#'
tidy_ndoh <- function(df, kp = FALSE) {

  #join NDOH to MFL
  ndoh_join <- df_fac %>%
    dplyr::left_join(df,  by = c("ou5uid" = "UID"))

  if (kp == TRUE) {
    ndoh_join <- ndoh_join %>%
      dplyr::mutate(indicator = stringr::str_replace(indicator, "_KP", ""))
  }


  #Munge and clean up NDOH names
  ndoh_clean <- ndoh_join %>%
    dplyr::mutate(indicator = dplyr::recode(indicator, "PrEP_New" = "PrEP_NEW",
                                            "TB_PREV_Denom" = "TB_PREV_D",
                                            "TB_PREV_Numer" = "TB_PREV_N",
                                            "TB_STAT_Denom" = "TB_STAT_D",
                                            "TB_STAT_Numer" = "TB_STAT_N",
                                            "TX TB_D" = "TX_TB_D",
                                            "TX TB_N" = "TX_TB_N",
                                            "TX_PVLS_Denom" = "TX_PVLS_D",
                                            "TX_PVLS_Numer" = "TX_PVLS_N"),
                  numeratordenom = ifelse(stringr::str_detect(indicator, "_D"), "D", "N"),
                  CoarseAgeGroup = ifelse(indicator %ni% c("TX_CURR", "TX_PVLS_D", "TX_PVLS_N", "TX_NEW", "TX_ML", "TX_RTT") & CoarseAgeGroup %in% c("50-54", "55-59", "60-64", "65+"),
                                          "50+", CoarseAgeGroup),
                  indicator = dplyr::recode(indicator, "TB_PREV_D" = "TB_PREV",
                                            "TB_PREV_N" = "TB_PREV",
                                            "TB_STAT_N" = "TB_STAT",
                                            "TB_STAT_D" = "TB_STAT",
                                            "TX_PVLS_D" = "TX_PVLS",
                                            "TX_PVLS_N" = "TX_PVLS",
                                            "TX_TB_N" = "TX_TB",
                                            "TX_TB_D" = "TX_TB"))

  #address over50 disagg issue (need to reclassify age groups above 50 as "50+")
  ndoh_over50 <- ndoh_clean %>%
    dplyr::filter(CoarseAgeGroup == "50+") %>%
    dplyr::group_by(usaid_facility, ou5uid, datim_uid, new_ou5_code, period, DSD_TA,
                    Province, District, SubDistrict, Facility, `Test Result/Outcome/Duration`,
                    Sex, CoarseAgeGroup, Result, indicator, numeratordenom) %>%
    dplyr::summarise(dplyr::across(tidyselect::starts_with("Total"), sum, na.rm = TRUE), .groups = "drop")

  ndoh_clean <- ndoh_clean %>%
    dplyr::filter(CoarseAgeGroup != "50+") %>%
    dplyr::bind_rows(ndoh_over50)

  return(ndoh_clean)
}

#' Tidy and map disaggs for ARVDISP
#'
#' @param df dataframe from import_arvdisp()
#'
#' @return
#' @export
#'
#' @examples
#'  \dontrun{
#'    import_arvdisp(ndoh_filepath) %>% tidy_map_arvdisp
#' }
#'

tidy_map_arvdisp <- function(df) {

  #join NDOH to MFL
  df_arv_clean <- df_fac %>%
    dplyr::left_join(df,  by = c("ou5uid" = "UID")) %>%
    dplyr::mutate(indicator = "SC_ARVDISP",
           RegimenCode = dplyr::recode(RegimenCode, "13.0" = "13"))

  #map disaggs + mechs
  df_arv_mapped <- df_arv_clean %>%
    dplyr::filter(!is.na(Facility)) %>%
    dplyr::left_join(arv_map, by = c("CoarseAgeGroup", "RegimenCode", "indicator")) %>%
    dplyr::mutate(dataElement = ifelse(is.na(dataElement), "SC_ARVDISP (N, NoApp, DispensedARVBottles): Dispensed ARV bottles", dataElement),
           dataElement_uid = ifelse(is.na(dataElement_uid), "jjXWGplLXqF", dataElement_uid)) %>%
    dplyr::mutate(categoryOptionComboName = dplyr::case_when(CoarseAgeGroup %in% c(">=15") & is.na(categoryOptionComboName) ~ "ARV Bottles - Other (Adult)",
                                               CoarseAgeGroup %in% c("<15") & is.na(categoryOptionComboName) ~ "ARV Bottles - Other (Pediatric)",
                                               TRUE ~ categoryOptionComboName)) %>%
    dplyr::left_join(arv_map %>% dplyr::select(categoryOptionComboName, categoryOptionCombo_uid) %>% dplyr::distinct(), by = c("categoryOptionComboName")) %>%
    dplyr::select(-c(categoryOptionCombo_uid.x)) %>%
    dplyr::rename(categoryOptionCombo_uid =categoryOptionCombo_uid.y) %>%
    dplyr::left_join(mech_mfl, by = c("datim_uid" = "facilityuid"))

  #collapse to just dataElement and catoptioncombo
  df_arv_collapse <- df_arv_mapped %>%
    dplyr::group_by(usaid_facility, datim_uid, period, Province, District, SubDistrict,
             Facility, indicator, dataElement, dataElement_uid, categoryOptionComboName,
             categoryOptionCombo_uid, sitename, mech_code, prime_partner_name,
             mech_uid) %>%
    dplyr::summarise(dplyr::across(c(tidyselect::starts_with("Packs")), sum, na.rm = TRUE), .groups = "drop")

  return(df_arv_collapse)

}
