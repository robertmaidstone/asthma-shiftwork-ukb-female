# Asthma, Shift Work, and Genetic Sex in UK Biobank

![R](https://img.shields.io/badge/R-%3E%3D4.2-blue)
![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)
[![DOI](https://zenodo.org/badge/DOI/10.1183/23120541.00137-2025.svg)](https://doi.org/10.1183/23120541.00137-2025)
![Last Commit](https://img.shields.io/github/last-commit/robertmaidstone/asthma-shiftwork-ukb-female)

This repository contains the full analysis pipeline for a cross-sectional UK Biobank study investigating whether genetic sex modifies the association between night shift work and asthma. The project contains end-to-end epidemiological analysis using large-scale biomedical data; including data processing, cohort construction, variable derivation, logistic regression modelling, mediation analysis, and outputting of results into publication-ready figures and tables.

The study findings are published in:  
**Maidstone et al., *ERJ Open Research*, 2025.** ([link](https://doi.org/10.1183/23120541.00137-2025))

---

## Repository Structure
```
asthma-shiftwork-ukb-female/
├── data/                     # External reference files (not included)
├── load_data.R               # Data ingestion and merging
├── data_wrangling.R          # Variable derivation and cleaning
├── CharacteristicTables.R    # Table 1 and Table 2 generation
├── OR_asthmashiftsex.R       # Main regression models and figures
├── mediation_analysis.R      # Mediation models
├── frequency_shiftwork.R     # Historical shift work frequency analyses
├── lifetime_shiftwork.R      # Lifetime shift work exposure analyses
└── README.md
```


---

## Overview of the Analysis Pipeline

### **1. Data Loading (`load_data.R`)**
- Loads UK Biobank data from `data/ukb_merged.RData`.
- Merges with:
  - `shrinemedicationlist.csv` — asthma medication list (Shrine et al., 2019)
  - `occupation_asthmarisk_v2.csv` — occupations with elevated asthma risk
  - `sleepmedication.csv` — sleep/mental health medication list (He et al., 2022)
- Collapses ICD‑9, ICD‑10, and non‑cancer illness codes into unified variables.

---

### **2. Data Wrangling (`data_wrangling.R`)**
- Adds readable variable names.
- Constructs disease indicators from ICD codes, medication, and self‑report.
- Defines shift‑work exposure variables.
- Calculates pack‑years and other covariates.

---

### **3. Characteristic Tables (`CharacteristicTables.R`)**
Generates:

- **Table 1:** Sociodemographic characteristics  
- **Table 2:** Health characteristics  

**📌 _Placeholder — insert example table screenshot here_**  
*(e.g., Table 1 or Table 2 as PNG)*

---

### **4. Main Regression Models (`OR_asthmashiftsex.R`)**
- Logistic regression models for asthma vs. shift‑work frequency.
- Sex‑stratified and interaction models.
- Odds ratios and confidence intervals.
- Generates Figure 1 (and Figure 2 in some versions).

**Figure 1**  
<img src="/examples/fig1.png" alt="Figure 1, from manuscript. Adjusted odds ratios (95% confidence intervals) of asthma and asthma symptoms by current shift work exposure, stratified by sex." width="700" height="700">

---

### **5. Historical and Lifetime Shift Work**
- `frequency_shiftwork.R` — historical frequency of shift work.
- `lifetime_shiftwork.R` — lifetime exposure metrics.
- Intended for supplementary analyses.

---

### **6. Mediation Analysis (`mediation_analysis.R`)**
- Explores potential mediators in Model 3.
- Uses established mediation frameworks in R.

---

## Requirements

### **Software**
- **R ≥ 4.2**

### **Key Packages**
`tidyverse`, `data.table`, `broom`, `tableone`,  
`ggplot2`, `mediation`, `forcats`, `stringr`, etc.

*(Optional: add a `sessionInfo()` block here.)*

---

## Running the Pipeline

**Data access:** This project uses UK Biobank data, which cannot be shared publicly. To reproduce the analysis, you must have an approved UKB project and access to the relevant fields. The code was originally run on local UKB extracts but can be adapted for the Research Access Platform (RAP) with minor path adjustments.

Assuming you have access to UK Biobank data and have placed the required files in `data/`:

```r
source("load_data.R")
source("data_wrangling.R")
source("CharacteristicTables.R")
source("OR_asthmashiftsex.R")
source("mediation_analysis.R")
```
Each script is modular and can be run independently if you only need specific outputs.

## Citation
If using this code please cite the published paper. Published paper also contains full methodological details, results, and interpretation.

*Increased risk of asthma in female night shift workers.* Robert J. Maidstone, David W. Ray, Junxi Liu, Jack Bowden, Martin K. Rutter and Hannah J. Durrington - ERJ Open Research, 2025.
([DOI: 10.1183/23120541.00137-2025](https://doi.org/10.1183/23120541.00137-2025))

## Contributing
Contributions, suggestions, and extensions are welcome.
Please open an issue or submit a pull request.

 ## References
- Shrine N., Portelli M.A., John C., et al. Moderate-To-Severe asthma in individuals of European ancestry: a genome-wide association study. Lancet Respir Med, 2019.
- He L., Ma T., Li J., Luo Y., Zhang G., Cheng X., Bai Y. Adherence to a healthy sleep pattern and incidence of cardiometabolic multimorbidity among hypertensive patients: a prospective study of UK Biobank. Sleep 45(10), 2022.
