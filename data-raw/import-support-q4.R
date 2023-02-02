if (qtr %in% c("Q1", "Q3") & kp == FALSE) {
  indic_list <- tier_qtr
} else if (qtr == "Q2" & kp == FALSE) {
  indic_list <- tier_semi
} else if (qtr == "Q4" & kp == FALSE) {
  indic_list <- tier_ann
} else if (kp == TRUE) {
  indic_list <- tier_kp_indic
}

misaligned_sites <-  c("mp Naas CHC",
"mp Rockdale CHC",
"ec Matatiele Community Clinic",
"kz Empangeni Clinic",
"kz Khandisa Clinic",
"kz Luwamba Clinic",
"kz Port Shepstone Mobile 3")



#Read in TIER with new function
df <- indic_list %>%
  purrr::map_dfr(.f = ~ read_all_the_things(ndoh_filepath, sheet = .x)) %>%
  dplyr::relocate(`Test Result/Outcome/Duration`, .after = Code) %>%
  dplyr::relocate(Result, .after = CoarseAgeGroup)


df_final <- df %>%
  dplyr::mutate(District = dplyr::recode(District,
                                         "fs Thabo Mofutsanyana District Municipality" = "fs Thabo Mofutsanyane District Municipality")) %>%
  dplyr::filter(District %in% usaid_dsp_district) %>%
  dplyr::mutate(code_num = stringr::str_length(Code)) %>%
  dplyr::group_by(Province, District, SubDistrict, Facility) %>%
  dplyr::arrange(dplyr::desc(code_num)) %>%
  dplyr::left_join(mfl_new_df %>%
                     dplyr::mutate(OU5name = recode(OU5name, "lp Matsotsosela Clinic" = "lp Matsotsosela clinic")) %>%
                     dplyr::select(OU5name, `New_OU5 Code`), by = c("Facility" = "OU5name")) %>%
  dplyr::mutate(`New_OU5 Code` = as.character(`New_OU5 Code`),
                Code = ifelse(code_num < 7, `New_OU5 Code`, Code),
                Code = ifelse(Facility %in% misaligned_sites, `New_OU5 Code`, Code)) %>%
  tidyr::fill(Code) %>%
  dplyr::ungroup() %>%
  #  dplyr::count(Facility, Code) %>%
  dplyr::select(-c(code_num, `New_OU5 Code`))

if (kp == TRUE) {
  #Aggregate across KP groups
  df_final <- df_final %>%
    dplyr::group_by(Province, District, SubDistrict, Facility, Code, `Test Result/Outcome/Duration`,
                    Sex, CoarseAgeGroup, Result, indicator) %>%
    dplyr::summarise(dplyr::across(tidyselect::starts_with("Total"), sum, na.rm = TRUE), .groups = "drop")


}

return(df_final)
