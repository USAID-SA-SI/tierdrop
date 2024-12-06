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

  folders <- list("data-raw", "data-raw/NDOH", "data-raw/Reference Files", "data-raw/MSD-Genie",
                  "data-raw/Import Files", "data-raw/Validation Files")
  #data_files <- c("NDOH File", "Disaggregate Mapping File")

  #if (!base::dir.exists(file.path(".", folders)))
  glamr::folder_setup(folders)
  # glamr::setup_gitignore()

  print(glue::glue_col("{yellow Please save the following files to the data-raw/NDOH folder:
                         } Latest NDOH File
                         {yellow Please save the following files to the data-raw/Reference Files folder:
                         } Additional mechanism data for specific sites (if needed)
                         {yellow Please save the following files to the data-raw/MSD-Genie folder:
                         } Most recent site-level MSD or data-pull from Genie"))

  cont <- readline(prompt = "When done, enter 1: ")


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
                        "PrEP_NEW" = names_prep_new,
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
                        "TX_PVLS_Denom" = names_tx_pvls_d,
                        "TX_PVLS_Numer" = names_tx_pvls_n,
                        "TB_STAT_Denom" = names_tb_stat_d,
                        "TB_STAT_Numer" = names_tb_stat_n,
                        "TX_TB_Denom" = names_tx_tb_d,
                        "TX_TB_Denom_Pos" = names_tx_tb_d_pos,
                        "TX_TB_Denom_TestType" = names_tx_tb_d_testtype,
                        "TX_TB_Numer" = names_tx_tb_n,
                        "TB_PREV_Denom" = names_tb_prev,
                        "TB_PREV_Numer" = names_tb_prev,
                        "PrEP_NEW_KP" = names_prep_new_kp,
                        "PrEP_CT_KP" = names_prep_ct_kp,
                        "HTS_TST_KP" = names_hts_tst_kp,
                        "TX_NEW_KP" = names_tx_new_kp,
                        "TX_CURR_KP" = names_tx_curr_kp,
                        "TX_ML_KP" = names_tx_ml_kp,
                        "TX_PVLS_Denom_KP" = names_tx_pvls_d_kp,
                        "TX_PVLS_Numer_KP" = names_tx_pvls_n_kp,
                        "ARVDISP" = names_arvdisp)

  df <- readxl::read_excel(path, sheet, col_names = col_renamed,  col_types = "text", skip =1)

  df <-  df %>%
    dplyr::mutate(indicator = sheet,
                  Total = as.numeric(Total)
                  # ,
                  # Code = stringr::str_replace(Code, ".0$", "")
    )

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



#' Split into partner-level import files
#'
#' @param df final import file dataframe
#' @param mech_code partner mech code
#' @param save if TRUE, the function will save dataframe to appropriate export folder (default = TRUE)

#'
#' @return
#' @export
#'
#' @examples
#'
partner_import <- function(df, mech_code, save = TRUE) {

  if (mech_code == "70287") {
    uid <- "koVrJ0HjBxy"
  } else if (mech_code == "70310") {
    uid <- "LbZtY0khSQw"
  } else if (mech_code == "87576") {
    uid <- "OqUUS4Qs62C"
  } else if (mech_code == "70290") {
    uid <- "R6zwVobwi58"
  } else if (mech_code == "70301") {
    uid <- "Rv3LaFFxBCY"
    } else if (mech_code == "87577") {
  uid <- "Kk2uIim6u4A"
    } else if (mech_code == "87575") {
  uid <- "q2gSoCyysic"
}


  df_partner <- df %>%
    dplyr::filter(!is.na(dataElement_uid)) %>%
    dplyr::filter(mech_uid == uid)

  if (save ==  TRUE) {
    readr::write_csv(df_partner, glue::glue("{import_folder}/{mech_code}_{fiscal_quarter}_TIER_Import_File_{today}.csv"))
  }



  return(df_partner)
}


# UPDATES IN Q4 TO IMPROVE WORKFLOW

import_ndoh2 <- function(filepath, qtr, kp = FALSE, skip_tabs = NULL) {
  if (qtr %in% c("Q1", "Q3") & kp == FALSE) {
    indic_list <- tier_qtr
  } else if (qtr == "Q2" & kp == FALSE) {
    indic_list <- tier_semi
  } else if (qtr == "Q4" & kp == FALSE) {
    indic_list <- tier_ann
  } else if (kp == TRUE) {
    indic_list <- tier_kp_indic
  }

  # Remove tabs specified in skip_tabs
  indic_list <- setdiff(indic_list, skip_tabs)

  # Initialize lists to track successes and issues
  successful_tabs <- list()
  skipped_tabs <- list()
  failed_tabs <- list()

  # Process each tab
  df <- purrr::map_dfr(indic_list, ~ {
    tab_name <- .x
    result <- purrr::safely(read_all_the_things2)(filepath, tab_name)



    if (is.null(result$result)) {
      warning(glue::glue("Failed to read tab '{tab_name}' due to errors: {result$error}"))
      failed_tabs <<- c(failed_tabs, tab_name)
      return(NULL)
    }

    # Validate and allow user intervention
    tab_data <- result$result
    validated_data <- validate_columns(tab_data, sheet_name = tab_name, expected_cols = get_expected_columns(tab_name))

    if (nrow(validated_data) == 0) {
      warning(glue::glue("Skipping tab '{tab_name}' due to validation issues."))
      skipped_tabs <<- c(skipped_tabs, tab_name)
      return(NULL)
    }

    successful_tabs <<- c(successful_tabs, tab_name)
    return(validated_data)
  })

  df <- df %>%
    dplyr::select(dplyr::all_of(unique(unlist(lapply(indic_list, get_expected_columns)))), tidyselect::everything())


  # Final processing
  if (nrow(df) == 0) {
    warning("No data was successfully imported from any tabs.")
    return(NULL)
  }

  # Print summary of processing
  cat(glue::glue("Import Summary:\n",
                 "  Successfully processed tabs: {paste(successful_tabs, collapse = ', ')}\n",
                 "  Skipped tabs: {paste(skipped_tabs, collapse = ', ')}\n",
                 "  Failed tabs: {paste(failed_tabs, collapse = ', ')}\n"))


  df_final <- df %>%
    dplyr::mutate(District = dplyr::recode(District,
                                           "fs Thabo Mofutsanyana District Municipality" = "fs Thabo Mofutsanyane District Municipality")) %>%
    dplyr::filter(District %in% usaid_dsp_district) %>%
    dplyr::mutate(code_num = stringr::str_length(Code)) %>%
    dplyr::group_by(Province, District, `Sub district`, Facility) %>%
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


  return(df_final)



}

# Helper function to get expected columns for a specific tab
get_expected_columns <- function(tab_name) {
  # Map tab names to expected column sets
  switch(tab_name,
         "PrEP_NEW" = names_prep_new,
         "PrEP_CT" = names_prep_ct,
         "TX_NEW" = names_tx_new,
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
         "TX_PVLS_Denom" = names_tx_pvls_d,
         "TX_PVLS_Numer" = names_tx_pvls_n,
         "TB_STAT_Denom" = names_tb_stat_d,
         "TB_STAT_Numer" = names_tb_stat_n,
         "TX_TB_Denom" = names_tx_tb_d,
         "TX_TB_Denom_Pos" = names_tx_tb_d_pos,
         "TX_TB_Denom_TestType" = names_tx_tb_d_testtype,
         "TX_TB_Numer" = names_tx_tb_n,
         "TB_PREV_Denom" = names_tb_prev,
         "TB_PREV_Numer" = names_tb_prev,
         "PrEP_NEW_KP" = names_prep_new_kp,
         "PrEP_CT_KP" = names_prep_ct_kp,
         "HTS_TST_KP" = names_hts_tst_kp,
         "TX_NEW_KP" = names_tx_new_kp,
         "TX_CURR_KP" = names_tx_curr_kp,
         "TX_ML_KP" = names_tx_ml_kp,
         "TX_PVLS_Denom_KP" = names_tx_pvls_d_kp,
         "TX_PVLS_Numer_KP" = names_tx_pvls_n_kp,
         "ARVDISP" = names_arvdisp,
         stop(glue::glue("No expected columns defined for tab '{tab_name}'.")))
}

read_all_the_things2 <- function(path, sheet) {

  # Define expected column mappings for specific tabs
  rename_columns_by_tab <- function(df, tab_name) {

    rename_mappings <- list(
      "TX_RTT" = c("IIT Duration" = "Test Result/Outcome/Duration"),
      "TB_ART" = c("ART status" = "Test Result/Outcome/Duration"),
      "TX_TB_Numer" = c("ART status" = "Test Result/Outcome/Duration"),
      "TB_STAT_Numer" = c("HIVstatus" = "Test Result/Outcome/Duration"),
      "TX_TB_Denom" = c("ART status" = "Test Result/Outcome/Duration",
                        "Screeningresult" = "Result"),
      "TB_PREV_Denom" = c("ART Status" = "Test Result/Outcome/Duration",
                          "Outcome" = "Result"),
      "TB_PREV_Numer" = c("ART Status" = "Test Result/Outcome/Duration")

      # Add other tab-specific mappings here
    )

    if (tab_name %in% names(rename_mappings)) {
      mapping <- rename_mappings[[tab_name]]
      df <- df %>%
        dplyr::rename_with(~ mapping[.x], .cols = intersect(names(df), names(mapping)))
      cat(glue::glue("Renamed columns in tab '{tab_name}' as per the mapping.\n"))
    }
    return(df)
  }

  # Determine expected columns for the sheet
  col_renamed <- switch(sheet,
                        "PrEP_NEW" = names_prep_new,
                        "PrEP_NEW" = names_prep_new,
                        "PrEP_CT" = names_prep_ct,
                        "TX_NEW" = names_tx_new,
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
                        "TX_PVLS_Denom" = names_tx_pvls_d,
                        "TX_PVLS_Numer" = names_tx_pvls_n,
                        "TB_STAT_Denom" = names_tb_stat_d,
                        "TB_STAT_Numer" = names_tb_stat_n,
                        "TX_TB_Denom" = names_tx_tb_d,
                        "TX_TB_Denom_Pos" = names_tx_tb_d_pos,
                        "TX_TB_Denom_TestType" = names_tx_tb_d_testtype,
                        "TX_TB_Numer" = names_tx_tb_n,
                        "TB_PREV_Denom" = names_tb_prev,
                        "TB_PREV_Numer" = names_tb_prev,
                        "PrEP_NEW_KP" = names_prep_new_kp,
                        "PrEP_CT_KP" = names_prep_ct_kp,
                        "HTS_TST_KP" = names_hts_tst_kp,
                        "TX_NEW_KP" = names_tx_new_kp,
                        "TX_CURR_KP" = names_tx_curr_kp,
                        "TX_ML_KP" = names_tx_ml_kp,
                        "TX_PVLS_Denom_KP" = names_tx_pvls_d_kp,
                        "TX_PVLS_Numer_KP" = names_tx_pvls_n_kp,
                        "ARVDISP" = names_arvdisp,
                        stop(glue::glue("Sheet '{sheet}' not recognized."))
  )

  # Read the Excel sheet
  df <- readxl::read_excel(path, sheet, col_names = TRUE, col_types = "text")

  # Rename columns based on tab-specific mappings
  df <- rename_columns_by_tab(df, sheet)

  # Validate columns and re-order
  df <- validate_columns(df, sheet, expected_cols = col_renamed)

  # Add indicator column and handle data types
  df <- df %>%
    dplyr::mutate(indicator = sheet,
                  Total = as.numeric(Total))

  # Retain only essential columns and remove extras
  retained_columns <- c(col_renamed, "indicator") # Ensure 'indicator' is included
  extra_columns <- setdiff(names(df), retained_columns)

  if (length(extra_columns) > 0) {
    cat(glue::glue("Removing extra columns from tab '{sheet}': {paste(extra_columns, collapse = ', ')}\n"))
  }

  df <- df %>%
    dplyr::select(dplyr::all_of(retained_columns))


  return(df)
}


