
#' Reshape MFL
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   mfl <- clean_mfl()
#' }
clean_mfl <- function() {

  #Read in MFL from google drive
  mfl_new_df <- googlesheets4::read_sheet(mfl_new_id, sheet = "MFL")

  #Reshape facility list - for Fy22Q3 list, change to account for DSD/Roving TA
  df_fac <- mfl_new_df %>%
    dplyr::filter(!is.na(OU2name)) %>%
    janitor::clean_names() %>%
    dplyr::select(ou5name, ou5uid, datim_uid, old_ou5code, tidyselect::starts_with("fy22")) %>%
    tidyr::pivot_longer(cols = tidyselect::starts_with("fy22"), names_to = "period", values_to = "DSD_TA") %>%
    dplyr::mutate(period = stringr::str_sub(period, start = 1, end = 6) %>% toupper(),
                  old_ou5code = as.character(old_ou5code),
                  DSD_TA = ifelse(DSD_TA == "DSD+Roving TA", "DSD", DSD_TA)) %>%
    dplyr::rename(usaid_facility = ou5name)

  return(df_fac)
}



