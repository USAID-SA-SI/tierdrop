#' NDOH Processing Function
#'
#' @description This function calls on the wrapper processing function `ndoh_wrapper()` to process
#' both the non-KP and KP tabs in the NDOH file, and bind them all together
#'
#' @param filepath NDOH filepath
#' @param qtr current quarter
#' @param export_type parameter to extract data in specific format (1) DATIM import file (2) Validation file for internal checks
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   df <- ndoh_processing(ndoh_filepath, qtr = "Q3", export_type = "Import")
#' }

ndoh_processing <- function(filepath = ndoh_filepath, qtr = curr_qtr, export_type = "Import") {

  #kp <- TRUE
  ndoh_processed_kp <- ndoh_wrapper(ndoh_filepath, qtr = curr_qtr, kp = TRUE, export_type = export_type)

 # kp <- FALSE
  ndoh_processed_all <- ndoh_wrapper(ndoh_filepath, qtr = curr_qtr, kp = FALSE, export_type = export_type)

  df_final <- dplyr::bind_rows(ndoh_processed_kp,
                               ndoh_processed_all)

  if (export_type == "Import") {
    readr::write_csv(df_final, glue::glue("{import_folder}/{fiscal_quarter}_TIER_Import_File.csv"))
  } else if (export_type == "Validation") {
    readr::write_csv(df_final, glue::glue("{validation_folder}/{fiscal_quarter}_TIER_Import_File.csv"))
  }

  return(df_final)

}


#' Wrapper function to process NDOH data
#'
#' @description This function wraps together functions to import the NDOH file and append all indicator tabs
#' (`import_ndoh()`), checks for facility alignment between NDOH and the MFL (`validate_ndoh()`), tidies the
#' dataframe and cleans up indicator names (`tidy_ndoh()`), and finally maps in the dataElement and categoryOptionCombo
#' information using `map_disaggs()` and `ndoh_post_processing()`
#'
#' @param filepath NDOH file path
#' @param qtr current quarter for NDOH processing
#' @param kp processes KP tabs in NDOH file if TRUE (default is FALSE) - need to run both and bind together
#' @param export_type parameter to extract data in specific format (1) DATIM import file (2) Validation file for internal checks
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   df <- ndoh_wrapper(ndoh_filepath, qtr = "Q3", kp = TRUE, export_type = "Validation")
#' }

ndoh_wrapper <- function(filepath = ndoh_filepath, qtr = curr_qtr, kp = FALSE, export_type = "Import") {

  #Import NDOH file and append all the tabs to indicator names - merge into one dataset
  ndoh_all <- import_ndoh(filepath = ndoh_filepath, qtr = curr_qtr, kp = kp)

  #check for facility alignment between MFL and NDOH - okay if they are all Correctional
  validate_ndoh(ndoh_all)

  #clean up the NDOH variable names and tidy df
  ndoh_clean <- tidy_ndoh(ndoh_all, kp = kp)

  #Map in dataElements
  ndoh_mapped <- ndoh_post_processing(ndoh_clean, kp = kp, export_type = export_type)

  return(ndoh_mapped)
}

#' Split import file into partner import files for DQRT
#'
#' @param df TIER import file from `ndoh_processing()`
#' @param mech_code partner mech code (numeric)
#'
#' @return
#' @export
#'
#' \dontrun{
#'
#'   df_partner <- partner_import(df = df_import, 70287)
#' }



partner_import <- function(df, mech_code) {
  df_partner <- df %>%
    dplyr::filter(!is.na(dataElement)) %>%
    dplyr::filter(mech_code == mech_code)

  readr::write_csv(df_partner, glue::glue("{import_folder}/{mech_code}_{fiscal_quarter}_TIER_Import_File.csv"))


  return(df_partner)
}

