#' Quarterly TIER indicators (use Q1 and Q3)
#' @description
#' This is the list of indicators to process from NDOH in Q1 and Q3
#'
#' @export
#'

#quarterly (all qtrs) TIER indicators (not ARVDISP)
tier_qtr <- c("PrEP_New","PrEP_CT","TX_NEW",
              "TX_CURR", "TX_RTT", "TX_ML",
              "TX_PVLS_Denom", "TX_PVLS_Numer",
              "TB_STAT_Denom", "TB_STAT_Numer"
)

#' Semi-Annual TIER indicators (use in Q2 only)
#' @description
#' This is the list of indicators to process from NDOH in Q2 (note: process SC_ARVDISP separately)
#'
#' @export
#'
#'
#semi-annual (Q2 and Q4) TIER indicators (not ARVDISP)
tier_semi <- c("PrEP_New","PrEP_CT","TX_NEW",
               "TX_CURR", "TX_RTT", "TX_ML",
               "TX_PVLS_Denom", "TX_PVLS_Numer",
               "TB_STAT_Denom", "TB_STAT_Numer",
               "TX TB_Denom", "TX TB_Denom_Pos", "TX TB_Denom_TestType",
               "TX TB_Numer", "TB PREV_N", "TB PREV_D")

#' Annual TIER indicators (use in Q4 only)
#' @description
#' This is the list of indicators to process from NDOH in Q4 (note: process SC_ARVDISP separately)
#'
#' @export
#'

#Annual (Q4) TIER indicators (not ARVDISP)
tier_ann <- c("PrEP_New","PrEP_CT","TX_NEW",
              "TX_CURR", "TX_RTT", "TX_ML",
              "TB_ART",
              "TX_PVLS_Denom", "TX_PVLS_Numer",
              "TB_STAT_Denom", "TB_STAT_Numer",
              "TX TB_Denom", "TX TB_Denom_Pos", "TX TB_Denom_TestType",
              "TX TB_Numer", "TB PREV_N", "TB PREV_D")

#' Quarterly TIER indicators for KP tabs (use in each quarter)
#' @description
#' This is the list of KP tab indicators to process from NDOH in all quarters
#'
#' @export
#'

#KP TIER indicators
tier_kp_indic <- c("PrEP_New_KP", "PrEP_CT_KP",
                   "TX_NEW_KP", 'TX_CURR_KP', "TX_ML_KP",
                   "TX_PVLS_Denom_KP", "TX_PVLS_Numer_KP")

#' Quarterly non-TIER indicators reported in NDOH file
#' @description
#' This is the list of non-TIER indicators that are reported in NDOH
#'
#' @export
#'

#non-TIER indicators in NDOH
non_tier_qtr <- c("HTS_TST", "PMTCT_EID","PMTCT_HEI_POS","PMTCT_HEI_POS_ART", "PMTCT_ART")
