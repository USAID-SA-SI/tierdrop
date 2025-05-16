
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
clean_mfl <- function(mfl_period = period, type = "main") {

  #use metadata from get_meta()
  tab_name <- str_c("MFL_", fy, "_", curr_qtr)

  #Read in MFL from google drive (need to parameterize this for the sheet name)
  mfl_new_df <- googlesheets4::read_sheet(mfl_fy25_id, sheet = tab_name)

  if (type == "main") {

    df_fac <- mfl_new_df %>%
      dplyr::filter(!is.na(OU2name)) %>%
      janitor::clean_names() %>%
      dplyr::select(ou5name, ou5uid, datim_uid, new_ou5_code, tidyselect::contains("dsd")) %>%
      dplyr::rename("dsd_ta" = glue::glue("{fiscal_quarter %>% tolower()}_dsd_ta")) %>%
      dplyr::mutate(period = fiscal_quarter,
                    new_ou5_code = as.character(new_ou5_code),
                    dsd_ta = ifelse(dsd_ta == "DSD+Roving TA", "DSD", dsd_ta)) %>%
      dplyr::rename(usaid_facility = ou5name)

  } else if (type == "mech") {
    #get mech info from MFL
    df_fac <- mfl_new_df %>%
      dplyr::filter(!is.na(OU2name)) %>%
      janitor::clean_names() %>%
      dplyr::select(ou5name, datim_uid,partner, mechanism_i_d, mechanism_uid) %>%
      dplyr::rename(sitename = ou5name,
                    facilityuid = datim_uid,
                    prime_partner_name = partner,
                    mech_code = mechanism_i_d,
                    mech_uid = mechanism_uid)
  }

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

