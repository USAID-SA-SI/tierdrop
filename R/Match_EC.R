

library(tidyverse)
getwd()
setwd("C://Users//ctrapence//Downloads")
Match_ec <- openxlsx2::read_xlsx("Non Tier Data Import Template Updated - FY25v1 (download in excel).xlsx",sheet = "import") %>%
  dplyr::select(dataElement_uid,orgUnit_uid,categoryOptionCombo_uid,mech_uid,value) %>%
  dplyr::mutate(Period="2024Q4") %>%
  dplyr::select(dataElement_uid,Period,orgUnit_uid,categoryOptionCombo_uid,mech_uid,value) %>%
  group_by_if(is.character) %>%
  summarise(Value=sum(value)) %>%
  rename(dataElement=dataElement_uid,	Period=Period,	Orgunit=orgUnit_uid,	categoryOptionCombo=categoryOptionCombo_uid,
         attributeOptionCombo=mech_uid,	Value=Value)



write.csv(Match_ec,"Nono-Tier-EC_14052025.csv",)


#KZN Import file
Match_kz <- openxlsx2::read_xlsx("Non Tier Data Import Template_MatCH_FY25Q1_V2.xlsx",sheet = "import") %>%
  dplyr::select(dataElement_uid,orgUnit_uid,categoryOptionCombo_uid,mech_uid,value) %>%
  dplyr::mutate(Period="2024Q4") %>%
  dplyr::select(dataElement_uid,Period,orgUnit_uid,categoryOptionCombo_uid,mech_uid,value) %>%
  group_by_if(is.character) %>%
  summarise(Value=sum(value)) %>%
  rename(dataElement=dataElement_uid,	Period=Period,	Orgunit=orgUnit_uid,	categoryOptionCombo=categoryOptionCombo_uid,
         attributeOptionCombo=mech_uid,	Value=Value)



  write.csv(Match_kz,"Nono-Tier-KZ_14052025v2.csv",row.names =FALSE)
