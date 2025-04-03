# AsthmaInFemaleNightShift
 Code to recreate analysis and plots/tables from paper "Increased risk of asthma in female night shift workers"

## load_data.R

Loads in UKB data from the RData object "data/ukb_merged.RData".

Combines with "data/shrinemedicationlist.csv" (list of medication used to treat/manage asthma/moderate-severe asthma) "data/occupation_asthmarisk_v2.csv" (list of occupations considered an asthma risk). 

Unites groups of columns (e.g. ICD 9, ICD 10, non-cancer illness codes) into singular variables containing all codes as a string comma separated.

## data_wrangling.R

Takes data from load_data.R. Adds in some variable names to aid readability.

Manipulation of variables including creating variables for specific diseases (from various fields including non-cancer illness codes, ICD, medication etc.), defining shift work exposures and calculating pack years.

## CharacteristicTables.R

Creates social-demographic characteristic table (Table 1) and health characteristic table (Table 2) of key variables.

## OR_asthmashiftsex

Figure 1 (maybe 2 in medRxiv/CHEST versions) plus p-values

## frequency_shiftwork

Frequency of shift work using historical data. Maybe going in supplemental