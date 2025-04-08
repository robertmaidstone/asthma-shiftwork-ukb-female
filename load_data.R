
# load packages -----------------------------------------------------------


library(tidyverse)


# load data ---------------------------------------------------------------

load("data/ukb_merged_diet.RData")
# ukb_data_processed <- ukb_merged 
# 
# rm(ukb_merged)

read.csv(file = "data/shrinemedicationlist.csv")  -> medlist
read.csv(file = "data/occupation_asthmarisk_v2.csv")  -> occ_asthma
read.csv(file = "data/sleepmedication.csv")  -> sleep_medlist

medlist %>% dplyr::select(Code_description) %>%
  unlist %>%
  lapply(function(x){strsplit(as.character(x),split = " ")[[1]][1]}) %>%
  unlist %>%
  paste0(",",.,",",collapse="|") -> patterns_all

medlist %>% 
  filter(Relevance=="mod/severe") %>%
  dplyr::select(Code_description) %>%
  unlist %>%
  lapply(function(x){strsplit(as.character(x),split = " ")[[1]][1]}) %>%
  unlist %>%
  paste0(",",.,",",collapse="|") -> patterns_ms

sleep_medlist %>% dplyr::select(Coding) %>%
  unlist %>%
  paste0(",",.,",",collapse="|") -> patterns_sleep

# filtered jobs -----------------------------------------------------------

occ_asthma %>% as_tibble() %>%
  filter(Risk.for.Asthma=="Y" | Risk.for.Asthma=="M") %>%
  dplyr::select(Number) %>% unlist -> asthmarisk_occs

occ_asthma %>% as_tibble() %>%
  filter(Medical.Required=="Y") %>%
  dplyr::select(Number) %>% unlist -> medrequired_occs

# exclusion criteria ------------------------------------------------------

nCI_code_cols <- paste("X20002.0.",0:32,sep="")
#CI_code_cols <- paste("X20001.0.",0:5,sep="")
Job_SOC_cols <- paste("X22617.0.",0:39,sep="")
ICD10_code_cols <- paste("X41270.0.",0:212,sep="")
ICD9_code_cols <- paste("X41271.0.",0:46,sep="")

ukb_data_processed %>% 
  as_tibble %>%
  unite(col=nCI_code,all_of(nCI_code_cols),sep=",",remove=TRUE) %>%
  mutate(nCI_code=paste0(",",nCI_code,",")) %>%
  #unite(col=CI_code,all_of(CI_code_cols),sep=",",remove=TRUE) %>%
  #mutate(CI_code=paste0(",",CI_code,",")) %>%
  unite(col=ICD10_code,all_of(ICD10_code_cols),sep=",",remove=FALSE) %>%
  mutate(ICD10_code=paste0(",",ICD10_code,",")) %>%
  unite(col=ICD9_code,all_of(ICD9_code_cols),sep=",",remove=TRUE) %>%
  mutate(ICD9_code=paste0(",",ICD9_code,",")) %>%
  unite(col=Job_SOC,all_of(Job_SOC_cols),sep=",",remove=TRUE) %>%
  mutate(Job_SOC=paste0(",",Job_SOC,",")) %>%
  mutate(Job_AsthmaRisk=grepl(paste0(",",paste0(asthmarisk_occs,collapse = ",|,"),","),Job_SOC)) %>%
  mutate(Job_MedRequired=grepl(paste0(",",paste0(medrequired_occs,collapse = ",|,"),","),Job_SOC)) %>%
  as_tibble() -> ukb_data_processed

