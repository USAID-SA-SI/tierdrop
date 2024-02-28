#SAVE STANDARD COLUMN NAMES FOR READING IN NDOH


#STANDARD NAMES
standard_names <- c("Province", "District", "SubDistrict", "Facility", "UID",
                    "Code")
usethis::use_data(standard_names, overwrite = TRUE)

#KP NAMES
kp_names <- c("KP_Location", "KP_Type")
usethis::use_data(kp_names, overwrite = TRUE)

#INDICATOR SPECIFIC NAMES
names_prep_new <- c(standard_names, "Sex", "CoarseAgeGroup", "Total")
names_prep_ct <- c(standard_names, "Sex", "CoarseAgeGroup", "Result", "Total")
names_hts_tst <- c(standard_names, "Test Result/Outcome/Duration" ,"Sex", "CoarseAgeGroup", "HIVTestOfferedIn", "Total")
names_pmtct_eid <- c(standard_names, "CoarseAgeGroup", "Total")
names_tx_new <- c(standard_names,  "Sex", "CoarseAgeGroup", "CD4", "Total")
names_pmtct_hei_pos <- c(standard_names, "CoarseAgeGroup", "Total")
names_tx_curr <- c(standard_names, "Sex", "CoarseAgeGroup", "Total")
names_tx_rtt <- c(standard_names, "Sex", "CoarseAgeGroup","Test Result/Outcome/Duration", "CD4","Total")
names_tx_ml <- c(standard_names, "Test Result/Outcome/Duration", "CoarseAgeGroup", "Sex", "Total")
names_pmtct_art <- c(standard_names, "CoarseAgeGroup", "Test Result/Outcome/Duration", "Total")
names_tb_art <- c(standard_names, "CoarseAgeGroup", "Sex","Test Result/Outcome/Duration", "Total")
names_tx_pvls_n <- c(standard_names, "Sex", "CoarseAgeGroup","VL_BIN","Total")
names_tx_pvls_d <- c(standard_names, "Sex", "CoarseAgeGroup", "Total")
names_tb_stat_d <- c(standard_names, "Sex", "CoarseAgeGroup", "Total")
names_tb_stat_n <- c(standard_names, "Sex", "Test Result/Outcome/Duration", "CoarseAgeGroup", "Total")
names_tx_tb_d <- c(standard_names, "Test Result/Outcome/Duration", "CoarseAgeGroup",  "Sex", "Result","Total")
names_tx_tb_d_pos <- c(standard_names, "Total")
names_tx_tb_d_testtype <- c(standard_names, "Test Result/Outcome/Duration","Total")
names_tx_tb_n <- c(standard_names, "Test Result/Outcome/Duration", "CoarseAgeGroup",  "Sex","Total")
names_tb_prev <- c(standard_names, "Test Result/Outcome/Duration",  "Sex", "CoarseAgeGroup", "Total")

usethis::use_data(names_prep_new, overwrite = TRUE)
usethis::use_data(names_prep_ct, overwrite = TRUE)
usethis::use_data(names_hts_tst, overwrite = TRUE)
usethis::use_data(names_pmtct_eid, overwrite = TRUE)
usethis::use_data(names_tx_new, overwrite = TRUE)
usethis::use_data(names_pmtct_hei_pos, overwrite = TRUE)
usethis::use_data(names_tx_curr, overwrite = TRUE)
usethis::use_data(names_tx_rtt, overwrite = TRUE)
usethis::use_data(names_tx_ml, overwrite = TRUE)
usethis::use_data(names_pmtct_art, overwrite = TRUE)
usethis::use_data(names_tb_art, overwrite = TRUE)
usethis::use_data(names_tx_pvls_n, overwrite = TRUE)
usethis::use_data(names_tx_pvls_d, overwrite = TRUE)
usethis::use_data(names_tb_stat_d, overwrite = TRUE)
usethis::use_data(names_tb_stat_n, overwrite = TRUE)
usethis::use_data(names_tx_tb_d, overwrite = TRUE)
usethis::use_data(names_tx_tb_d_pos, overwrite = TRUE)
usethis::use_data(names_tx_tb_d_testtype, overwrite = TRUE)
usethis::use_data(names_tx_tb_n, overwrite = TRUE)
usethis::use_data(names_tb_prev, overwrite = TRUE)


#KP INDIC specific names ----

names_prep_new_kp <- c(names_prep_new, kp_names, "Total")[-9]
names_prep_ct_kp <- c(names_prep_ct,kp_names, "Total")[-10]
names_hts_tst_kp <- c(names_hts_tst,kp_names, "Total")[-11]
names_tx_new_kp <- c(names_tx_new, kp_names, "Total")[-10]
names_tx_curr_kp <- c(names_tx_curr, kp_names, "Total")[-9]
names_tx_ml_kp <- c(names_tx_ml,kp_names, "Total")[-10]
names_tx_pvls_d_kp <- c(names_tx_pvls_d, kp_names, "Total")[-9]
names_tx_pvls_n_kp <- c(names_tx_pvls_n, kp_names, "Total")[-10]


usethis::use_data(names_prep_new_kp, overwrite = TRUE)
usethis::use_data(names_prep_ct_kp, overwrite = TRUE)
usethis::use_data(names_hts_tst_kp, overwrite = TRUE)
usethis::use_data(names_tx_new_kp, overwrite = TRUE)
usethis::use_data(names_tx_curr_kp, overwrite = TRUE)
usethis::use_data(names_tx_ml_kp, overwrite = TRUE)
usethis::use_data(names_tx_pvls_d_kp, overwrite = TRUE)
usethis::use_data(names_tx_pvls_n_kp, overwrite = TRUE)


#ARV DISP----

names_arvdisp <- c(standard_names, "CoarseAgeGroup", "RegimenCode", "Packs")
usethis::use_data(names_arvdisp, overwrite = TRUE)




