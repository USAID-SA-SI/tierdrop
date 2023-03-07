#' Import NDOH tabs
#'
#' @param filepath provide the full file path for the NDOH import file
#' @param qtr return the quarter of the NDOH reporting file
#' @param kp processes KP tabs if TRUE and non-KP tabs if FALSE (default kp = FALSE)
#'
#' @return
#' @export
#'
#' @examples
#'  \dontrun{
#'    ndoh_all <- import_ndoh(qtr = "Q3", kp = FALSE)
#'    ndoh_kp <- import_ndoh(qtr = "Q3", kp = TRUE)
#' }
#'

import_ndoh <- function(filepath, qtr, kp = FALSE) {

  if (qtr %in% c("Q1", "Q3") & kp == FALSE) {
    indic_list <- tier_qtr
  } else if (qtr == "Q2" & kp == FALSE) {
    indic_list <- tier_semi
  } else if (qtr == "Q4" & kp == FALSE) {
    indic_list <- tier_ann
  } else if (kp == TRUE) {
    indic_list <- tier_kp_indic
  }

  #Read in TIER with new function
  df <- indic_list %>%
    purrr::map_dfr(.f = ~ read_all_the_things(filepath, sheet = .x)) %>%
    dplyr::relocate(`Test Result/Outcome/Duration`, .after = Code) %>%
    dplyr::relocate(Result, .after = CoarseAgeGroup)


  df_final <- df %>%
    dplyr::mutate(District = dplyr::recode(District,
                                           "fs Thabo Mofutsanyana District Municipality" = "fs Thabo Mofutsanyane District Municipality")) %>%
    dplyr::filter(District %in% usaid_dsp_district) %>%
    dplyr::mutate(code_num = stringr::str_length(Code)) %>%
    dplyr::group_by(Province, District, SubDistrict, Facility) %>%
    dplyr::arrange(dplyr::desc(code_num)) %>%
    dplyr::left_join(df_fac %>%
                       dplyr::mutate(usaid_facility = dplyr::recode(usaid_facility, "lp Matsotsosela Clinic" = "lp Matsotsosela clinic")) %>%
                       dplyr::select(usaid_facility, new_ou5_code), by = c("Facility" = "usaid_facility")) %>%
    dplyr::mutate(new_ou5_code = as.character(new_ou5_code),
                   Code = ifelse(code_num < 7, new_ou5_code, Code)
                  # ,
                  # Code = ifelse(Facility %in% misaligned_sites, new_ou5_code, Code)
                  ) %>%
    tidyr::fill(Code) %>%
    dplyr::ungroup() %>%
    #  dplyr::count(Facility, Code) %>%
    dplyr::select(-c(code_num, new_ou5_code))

  if (kp == TRUE) {
    #Aggregate across KP groups
    df_final <- df_final %>%
      dplyr::group_by(Province, District, SubDistrict, Facility, Code, `Test Result/Outcome/Duration`,
               Sex, CoarseAgeGroup, Result, indicator) %>%
      dplyr::summarise(dplyr::across(tidyselect::starts_with("Total"), sum, na.rm = TRUE), .groups = "drop")


  }

return(df_final)
}


