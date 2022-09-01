#' Grab site-level mechanism mechdata from DATIM/recent MSD
#'
#' @param df Calls on mechanism meta-data that is pulled by `pull_mech_uid()`
#' @param extra_mechs If TRUE, function will read in extra mechanism data saved in reference file folder
#' (default = FALSE)
#' @return
#' @export
#'
#' @examples
#'  \dontrun{
#'    grab_mech_data(df = mechs, extra_mechs = TRUE)
#' }
#'
grab_mech_data <- function(df, extra_mechs = FALSE) {

  if (extra_mechs == TRUE) {

    vars <- c("sitename", "facilityuid", "mech_code","prime_partner_name")

    print(glue::glue_col("{yellow Before continuing, please ensure that your additional mechanism data is saved
                         in the `data-raw/Reference Files` folder with the following filepath:}"))
  print(glue::glue_col("{blue additional-mechdata-{fiscal_quarter}.xlsx} {white (amend fiscal quarter as needed)}"))
    print(glue::glue_col("{yellow
                         Please include the following 4 variables:}"))
    print(glue::glue_col("{white {vars}}"))

    cont <- readline(prompt = "When done, enter 1: ")

  }


  #grab mech codes and mech UIDs for SOuth AFrica - filter to relevant DSPs
  mech_xwalk <- df %>%
    dplyr::filter(operatingunit == "South Africa",
                  mech_code %in% c(70310, 70287, 81902, 70290, 70301)) %>%
    dplyr::select(mech_code, mech_uid)

  #pull in additional mechs manually if true
  if (extra_mechs == TRUE) {
    match_mechs <- reference_folder %>%
      glamr::return_latest(glue::glue("additional-mechdata-{fiscal_quarter}.xlsx")) %>%
      readxl::read_xlsx()
  }


  #read the most recent MSD from the Genie folder
  df_genie <- msd_folder %>%
    glamr::return_latest() %>%
    gophr::read_msd()

  #grab mech info by sites reported in most recent MSD
  msd_mechs2 <- df_genie %>%
    dplyr::filter(funding_agency == "USAID",
                  fiscal_year == 2022,
                  mech_code %in% c(70310, 70287, 81902, 70290, 70301)) %>%
    dplyr::count(sitename, facilityuid, mech_code, prime_partner_name)

  #bind extra mechs if true
  if (extra_mechs == TRUE) {
    msd_mechs2 <- msd_mechs2 %>%
      rbind(match_mechs)
  }

  msd_mechs_final <- msd_mechs2 %>%
    dplyr::left_join(mech_xwalk, by = c('mech_code')) %>%
    # dplyr::filter(facilityuid != "~") %>%
    dplyr::select(-c(n))

  return(msd_mechs_final)

}



