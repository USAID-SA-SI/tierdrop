#' Set up processing folders
#'
#' @return creates data and dataout folder
#' @export
#'
#' @examples
#' \dontrun{
#' #standard
#'   dir_setup()
#' }

dir_setup <- function() {

  folders <- list("data-raw", "data-raw/NDOH", "data-raw/Reference Files", "data-raw/MSD-Genie")
  data_files <- c("NDOH File", "Disaggregate Mapping File")

  #if (!base::dir.exists(file.path(".", folders)))
    glamr::folder_setup(folders)
   glamr::setup_gitignore()

    print(glue::glue_col("{yellow Please save the following files to the data-raw/NDOH folder:
                         } Latest NDOH File
                         {yellow Please save the following files to the data-raw/Reference Files folder:
                         } Completed Disaggregate Mapping Reference File
                         {yellow Please save the following files to the data-raw/MSD-Genie folder:
                         } Most recent site-level MSD or data-pull from Genie"))



}

#' Read tabs from NDOH for each indicator and adjust column names
#'
#' @param path file path for NDOH
#' @param sheet tab name
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   df <- read_all_the_things(filepath, "TX_CURR")
#' }
read_all_the_things <- function(path, sheet){

  col_renamed <- switch(sheet,
                        "PrEP_New" = names_prep_new,
                        "PrEP_CT" = names_prep_ct,
                        "HTS_TST" = names_hts_tst,
                        "PMTCT_EID" = names_pmtct_eid,
                        "TX_NEW" = names_tx_new,
                        "PMTCT_HEI_POS" = names_pmtct_hei_pos,
                        "PMTCT_HEI_POS_ART" = names_pmtct_hei_pos,
                        "TX_CURR" = names_tx_curr,
                        "TX_RTT" = names_tx_rtt,
                        "TX_ML" = names_tx_ml,
                        "PMTCT_ART" = names_pmtct_art,
                        "TB_ART" = names_tb_art,
                        "TX_PVLS_Denom" = names_tx_pvls,
                        "TX_PVLS_Numer" = names_tx_pvls,
                        "TB_STAT_Denom" = names_tb_stat_d,
                        "TB_STAT_Numer" = names_tb_stat_n,
                        "TX TB_D" = names_tx_tb_d,
                        "TX TB_N" = names_tx_tb_n,
                        "TB PREV_D" = names_tb_prev,
                        "TB PREV_N" = names_tb_prev,
                        "PrEP_New_KP" = names_prep_new_kp,
                        "PrEP_CT_KP" = names_prep_ct_kp,
                        "HTS_TST_KP" = names_hts_tst_kp,
                        "TX_NEW_KP" = names_tx_new_kp,
                        "TX_CURR_KP" = names_tx_curr_kp,
                        "TX_ML_KP" = names_tx_ml_kp,
                        "TX_PVLS_Denom_KP" = names_tx_pvls_kp,
                        "TX_PVLS_Numer_KP" = names_tx_pvls_kp,
                        "ARVDISP" = names_arvdisp)

  df <- readxl::read_excel(path, sheet, col_names = col_renamed,  col_types = "text", skip =1)

  df <-  df %>%
    dplyr::mutate(indicator = sheet,
           Total = as.numeric(Total),
           Code = stringr::str_replace(Code, ".0$", ""))

  return(df)
}


#' Paint console text in yellow
#'
#' @param txt text to be printed
#' @export
#'
paint_yellow <- function(txt) {
  msg <- crayon::yellow(txt)
  return(msg)
}

#' Paint if true
#'
#' @param value text to be painted and printed
#' @param true_paint crayon function to execute
#' @param false_paint crayon function to execute
#' @export
#'
paint_iftrue <- function(value,
                         true_paint = crayon::green,
                         false_paint = crayon::red) {

  ifelse(base::isTRUE(value), true_paint(value), false_paint(value))
}

#' Paint console text in green
#'
#' @param txt text to be printed
#' @export
#'
paint_green <- function(txt) {
  msg <- crayon::green(txt)
  return(msg)
}

#' @title Negate in
#' @description negate `%in%`
#' @export
`%ni%` <- Negate(`%in%`)


