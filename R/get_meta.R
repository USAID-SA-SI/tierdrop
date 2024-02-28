#' Get period metadata
#'
#' @description Returns current fiscal year, qtr, and import file period to store as locals in env
#' @param period Input reporting period
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' get_meta("FY23Q4")
#' }


get_meta <- function(period) {

  #check to make sure input is 6 characters
  if(stringr::str_length(period) != 6)
    stop("Period not in correct 6 character format (eg: FY23Q4)")

  #build check to make sure period is entered correctly
  if(glue::glue("{stringr::str_sub(period, start = 1, end = 2)}") != "FY")
    stop("Period not starting with FY (eg: FY23Q4)")

  #extract current FY and qtr
  fiscal_quarter <<- period
  curr_fy <<- glue::glue("20{stringr::str_sub(period, start = 3, end = 4)}")
  curr_qtr <<- glue::glue("{stringr::str_sub(period,start = 5)}")
  today <- lubridate::today()

  q <- glue::glue("{stringr::str_sub(curr_qtr,end = 1)}")
  fy <- glue::glue("{stringr::str_sub(period, start = 1, end = 2)}")

  #check to make sure input is 6 characters
  if(q != "Q")
    stop("Q for quarter not capitalized - please capitalize Q (i.e. FY23Q1)")

  #check to make sure input is 6 characters
  if(fy != "FY")
    stop("FY for fiscal year not capitalized - please capitalize FY (i.e. FY23Q1)")

  #specific prev qtr
  prev_qtr <<- dplyr::case_when(curr_qtr == "Q1" ~ "Q4",
                        curr_qtr == "Q2" ~ "Q1",
                        curr_qtr == "Q3" ~ "Q2",
                        curr_qtr == "Q4" ~ "Q3")

  #create import file period type (FY23Q3 --> 2023Q2)
  fy_num <<- as.numeric(curr_fy)
  prev_fy <<- as.character(fy_num - 1)

  import_period_style <<-
    if(curr_qtr == "Q1") {
      glue::glue("{prev_fy}{prev_qtr}")
    } else {
      glue::glue("{curr_fy}{prev_qtr}")
    }


  #store folder paths
  ndoh_folderpath <<- 'data-raw/NDOH'
  reference_folder <<- "data-raw/Reference Files"
  msd_folder <<- "data-raw/MSD-Genie"
  import_folder <<- "data-raw/Import Files"

  #vars
  import_vars <<- c("mech_uid", "orgUnit_uid",	"dataElement_uid",
                   "categoryOptionCombo_uid",	"value",	"period")

  partner_vars <<- c("mech_code","mech_uid", "orgUnit_uid", "Facility","dataElement",	"dataElement_uid", "categoryOptionComboName",
                    "categoryOptionCombo_uid",	"value",	"period")


  validation_vars <<- c("period","Province", "District","SubDistrict", "Facility",
                       "orgUnit_uid", "mech_code", "mech_uid",
                       "indicator", "numeratordenom", "Sex",
                       "CoarseAgeGroup", "Result","dataElement", "dataElement_uid", "categoryOptionComboName",
                       "categoryOptionCombo_uid", "value")

  #print
  cat("\n---- METADATA STORED LOCALLY ----",
    "\nReporting Period:", paint_yellow(period),
      "\nCurrent Quarter:", paint_yellow(curr_qtr),
    "\nImport File Quarter:", paint_yellow(import_period_style),
    "\nToday's date:", paint_yellow(print(today)),
      "\n")
}





