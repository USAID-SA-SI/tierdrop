#' Validate facilities reported in NDOH by MFL
#'
#' @param df NDOH dataframe to validate

#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   validate_ndoh(ndoh_all)
#' }
validate_ndoh <- function(df) {

  #CHECK******
  #what facilities are in NDOH but not in MFL? ADdress MFL qc as needed
  ndoh_code <- unique(df$UID)
  mfl_code <- unique(df_fac$ou5uid)
  code_list <- setdiff(ndoh_code, mfl_code)


  # all correctional sites after manual mutates
  missing_sites <- df %>%
    dplyr::filter(UID %in% code_list) %>%
    dplyr::distinct(Facility) %>%
    dplyr::pull()

  cat("\n---- IMPORT VALIDATION ----",
      "\nAre there facilities reported in NDOH that are not in the USAID MFL?",
      paint_yellow(missing_sites), sep='\n')

}



