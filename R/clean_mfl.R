
#' Reshape MFL
#'
#' @param mfl_period version of MFL
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   mfl <- clean_mfl()
#' }
clean_mfl <- function(mfl_period = "FY23") {

  tab_name <- ifelse(mfl_period == "FY23", "MFL_FY23", "MFL_FY22Q4")

  #Read in MFL from google drive (need to parameterize this for the sheet name)
  mfl_new_df <- googlesheets4::read_sheet(mfl_fy23_id, sheet = tab_name)

  #Reshape facility list - for Fy22Q3 list, change to account for DSD/Roving TA
  df_fac <- mfl_new_df %>%
    dplyr::filter(!is.na(OU2name)) %>%
    janitor::clean_names() %>%
    dplyr::select(ou5name, ou5uid, datim_uid, old_ou5code, tidyselect::contains("dsd")) %>%
    # tidyr::pivot_longer(cols = tidyselect::contains("dsd"), names_to = "period", values_to = "DSD_TA") %>%
    dplyr::mutate(period = fiscal_quarter,
                  old_ou5code = as.character(old_ou5code),
                  dsd_ta = ifelse(dsd_ta == "DSD+Roving TA", "DSD", dsd_ta)) %>%
    dplyr::rename(usaid_facility = ou5name)

  return(df_fac)
}

# mfl_new_df %>%
#   dplyr::filter(!is.na(OU2name)) %>%
#   janitor::clean_names() %>%
#   dplyr::select(ou5name, ou5uid, datim_uid, mechanism_i_d, mechanism_uid, old_ou5code, tidyselect::starts_with("fy")) %>%
#   tidyr::pivot_longer(cols = tidyselect::starts_with("fy22"), names_to = "period", values_to = "DSD_TA") %>%
#   dplyr::mutate(period = stringr::str_sub(period, start = 1, end = 6) %>% toupper(),
#                 old_ou5code = as.character(old_ou5code),
#                 DSD_TA = ifelse(DSD_TA == "DSD+Roving TA", "DSD", DSD_TA)) %>%
#   dplyr::rename(usaid_facility = ou5name)

