#' Validate facilities reported in NDOH by MFL
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
  #what facilities are in NDOH but not in MFL? All correctional facilities
  ndoh_code <- unique(df$Code)
  mfl_code <- unique(df_fac$old_ou5code)
  code_list <- setdiff(ndoh_code, mfl_code)


  NDOH_MFL_triangulation <- df %>% dplyr::filter(Code %in% code_list) %>%
    dplyr::distinct(Facility) %>%
    dplyr::pull(Facility)

  cat("\n---- IMPORT VALIDATION ----",
      "\nAre there facilities reported in NDOH that are not in the USAID MFL?",
      paint_yellow(NDOH_MFL_triangulation), sep='\n')

}



