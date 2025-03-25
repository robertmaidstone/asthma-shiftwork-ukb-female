# AsthmaInFemaleNightShift
 Code to recreate analysis and plots/tables from paper "Increased risk of asthma in female night shift workers"

## load_data.R

Loads in UKB data from the RData object "data/ukb_merged.RData".

Combines with "data/shrinemedicationlist.csv" (list of medication used to treat/manage asthma/moderate-severe asthma) "data/occupation_asthmarisk_v2.csv" (list of occupations considered an asthma risk). 

Unites groups of columns (e.g. ICD 9, ICD 10, non-cancer illness codes) into singular variables containing all codes as a string comma separated.

## data_wrangling.R