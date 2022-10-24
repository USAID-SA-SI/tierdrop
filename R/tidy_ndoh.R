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
    dplyr::left_join(df,  by = c("new_ou5_code" = "Code"))

  if (kp == TRUE) {
    ndoh_join <- ndoh_join %>%
      dplyr::mutate(indicator = stringr::str_replace(indicator, "_KP", ""))
  }


  #Munge and clean up NDOH names
  ndoh_clean <- ndoh_join %>%
    dplyr::mutate(indicator = dplyr::recode(indicator, "PrEP_New" = "PrEP_NEW",
                              "TB PREV_D" = "TB_PREV_D",
                              "TB PREV_N" = "TB_PREV_N",
                              "TB_STAT_Denom" = "TB_STAT_D",
                              "TB_STAT_Numer" = "TB_STAT_N",
                              "TX TB_D" = "TX_TB_D",
                              "TX TB_N" = "TX_TB_N",
                              "TX_PVLS_Denom" = "TX_PVLS_D",
                              "TX_PVLS_Numer" = "TX_PVLS_N"),
           numeratordenom = ifelse(stringr::str_detect(indicator, "_D"), "D", "N"),
           CoarseAgeGroup = ifelse(indicator != "TX_CURR" & CoarseAgeGroup %in% c("50-54", "55-59", "60-64", "65+"),
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
