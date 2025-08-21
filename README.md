# Diabetes Prescribing Inequalities and Integrated Care System Impact in West Yorkshire

## Project Overview

This repository contains the complete analytical pipeline for examining diabetes medication inequalities and the impact of Integrated Care System (ICS) implementation in West Yorkshire, England. The study uses interrupted time series methodology to evaluate prescribing patterns across socioeconomic gradients from June 2020 to May 2025.

## Repository Structure

```
├── data/
│   ├── raw/                              # Raw data files (not included in repo)
│   │   ├── LSOA_WY.xls                  # West Yorkshire LSOA boundaries and names
│   │   ├── LSOA_decile.csv              # Index of Multiple Deprivation rankings
│   │   └── gp-reg-pat-prac-lsoa-all-*.csv # Annual GP patient registration data
│   ├── processed/
│   │   ├── WestYorkshire_GPDrugData_by_Practice.csv
│   │   ├── WestYorkshire_Diabetes_Prescriptions_by_LSOA_Monthly.csv
│   │   └── diabetes_its_analysis_data_lsoa.csv
│   └── results/
│       └── its_model_summary.csv
├── code/
│   ├── 01_data_collection.ipynb         # OpenPrescribing API data collection
│   ├── 02_spatial_allocation.ipynb      # GP practice to LSOA allocation
│   ├── 03_data_preprocessing.ipynb      # ITS analysis data preparation
│   ├── 04_imd_analysis.Rmd             # Socioeconomic inequality analysis
│   └── 05_its_analysis.Rmd             # Interrupted time series analysis
├── plots/
│   ├── plots_diabetes/                  # IMD inequality visualizations
│   └── plots_its/                      # ITS analysis results
├── docs/
│   ├── data_dictionary.txt
│   └── methodology_notes.md
└── README.md
```

## Methodology Pipeline

### 1. Data Collection (`01_data_collection.ipynb`)

**Automated OpenPrescribing API Data Retrieval**

- **Scope**: 7 diabetes medication categories across 5 West Yorkshire sub-ICB areas
- **Time Period**: June 2020 - May 2025 (60 months)
- **Geographic Coverage**: 294 GP practices serving West Yorkshire residents
- **BNF Categories**:
  - 6.1.1.1: Short-acting insulins
  - 6.1.1.2: Intermediate and long-acting insulins  
  - 6.1.2.1: Sulfonylureas
  - 6.1.2.2: Biguanides (primarily metformin)
  - 6.1.2.3: Other antidiabetic drugs
  - 6.1.4: Treatment of hypoglycaemia
  - 6.1.6: Diabetic diagnostic and monitoring agents

**Key Features**:
- Memory-optimized chunked processing for large datasets
- Rate-limiting compliance with API restrictions
- Automatic deduplication and data quality validation
- Wide-format transformation for subsequent analysis

**Output**: `WestYorkshire_GPDrugData_by_Practice.csv` (16,450 rows × 25 columns)

### 2. Spatial Allocation (`02_spatial_allocation.ipynb`)

**GP Practice to LSOA Prescription Allocation**

- **Method**: Proportional allocation based on patient registration distributions
- **Temporal Accuracy**: Year-specific patient registration data (2020-2025)
- **Geographic Scope**: 1,404 LSOAs across West Yorkshire
- **Validation**: 99.77% successful data linkage with conservation checks

**Allocation Process**:
1. Load annual GP patient registration data by LSOA residence
2. Calculate proportional patient distribution for each GP practice
3. Allocate prescription quantities proportionally to served LSOAs
4. Aggregate to monthly LSOA-level summaries
5. Validate allocation accuracy through conservation testing

**Output**: `WestYorkshire_Diabetes_Prescriptions_by_LSOA_Monthly.csv` (82,205 rows × 21 columns)

### 3. Data Preprocessing (`03_data_preprocessing.ipynb`)

**Interrupted Time Series Analysis Preparation**

- **Intervention Point**: July 2022 (ICS implementation)
- **Time Variables**: Continuous time points and intervention indicators
- **Core Indicators**:
  - **IDR (Insulin Dependency Ratio)**: (Short-acting insulin + Intermediate/long-acting insulin prescriptions) / Total diabetes prescriptions × 100
  - **ATU (Advanced Therapy Utilization)**: Other antidiabetic drugs / (Sulfonylureas + Biguanides + Other antidiabetic drugs) × 100
  - **HMR (Hypoglycemia Risk Management Ratio)**: Hypoglycemia treatment prescriptions / Total diabetes prescriptions × 100

**Quality Assurance**:
- Missing value assessment and time series completeness verification
- Pre-post intervention comparative analysis
- Data type optimization and standardization

**Output**: `diabetes_its_analysis_data_lsoa.csv` (82,205 rows × 25 columns)

### 4. Socioeconomic Analysis (`04_imd_analysis.Rmd`)

**Index of Multiple Deprivation (IMD) Inequality Assessment**

- **Deprivation Measure**: IMD 2019 national decile rankings
- **Analysis Focus**: Prescribing pattern variations across socioeconomic gradients
- **Comparison Groups**: Most deprived (decile 1) vs. least deprived (decile 10)

**Visualizations**:
- Temporal trends in prescription volumes by drug category
- Ribbon plots showing prescribing inequalities over time
- Pre/post intervention changes by deprivation level

**Output**: Figure series documenting baseline inequalities and intervention impacts

### 5. Interrupted Time Series Analysis (`05_its_analysis.Rmd`)

**ICS Implementation Impact Evaluation**

- **Statistical Method**: Segmented linear regression with control variables
- **Model Specification**: `Outcome ~ time + intervention + post_intervention + control`
- **Control Variable**: Total diabetes prescription volume
- **Effect Estimation**: Immediate effects and slope changes

**Key Outputs**:
- Observed vs. counterfactual trend comparisons
- Pointwise and cumulative intervention effects
- Statistical significance testing for policy impacts

## Key Findings

### Data Coverage
- **Geographic**: 1,404 LSOAs across West Yorkshire
- **Temporal**: 60 months (June 2020 - May 2025)
- **Prescriptions**: 13.7 million diabetes prescription items
- **Completeness**: 99.77% successful data linkage

### Prescribing Patterns
- **Total Diabetes Prescriptions**: 13,682,371 items over study period
- **Most Common**: Biguanides (42.5%), Other antidiabetic drugs (30.9%)
- **Insulin Usage**: 12.89% average dependency ratio across LSOAs

### ICS Implementation Effects
- **IDR**: Significant reduction (-1.22 percentage points average change)
- **ATU**: Substantial increase (+8.04 percentage points average change)  
- **HMR**: Modest increase (+0.06 percentage points average change)

### Socioeconomic Inequalities
- Persistent disparities across deprivation levels
- Differential intervention impacts by socioeconomic status
- Evidence of widening gaps in advanced therapy access

## Data Requirements

### Input Files (Not Included in Repository)
```
data/raw/
├── LSOA_WY.xls                     # West Yorkshire LSOA boundaries
├── LSOA_decile.csv                 # IMD 2019 decile rankings  
├── gp-reg-pat-prac-lsoa-all-20.csv # 2020 patient registrations
├── gp-reg-pat-prac-lsoa-all-21.csv # 2021 patient registrations
├── gp-reg-pat-prac-lsoa-all-22.csv # 2022 patient registrations
├── gp-reg-pat-prac-lsoa-all-23.csv # 2023 patient registrations
├── gp-reg-pat-prac-lsoa-all-24.csv # 2024 patient registrations
└── gp-reg-pat-prac-lsoa-all-25.csv # 2025 patient registrations
```

**Data Sources**:
- Prescription data: [OpenPrescribing.net](https://openprescribing.net/) via API
- Patient registrations: [NHS Digital](https://digital.nhs.uk/)
- Geographic boundaries: [ONS Geography Portal](https://geoportal.statistics.gov.uk/)
- Deprivation indices: [MHCLG Indices of Deprivation](https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019)

## Software Dependencies

### Python (Google Colab)
```python
pandas>=1.3.0
numpy>=1.21.0
requests>=2.25.0
datetime
tqdm>=4.62.0
```

### R 
```r
tidyverse>=1.3.0
ggpubr>=0.4.0
gridExtra>=2.3
tidybayes>=3.0.0
patchwork>=1.1.0
```

## Usage Instructions

### 1. Data Collection
```bash
# Run in Google Colab
01_data_collection.ipynb
```
- Requires internet connection for OpenPrescribing API access
- Runtime: ~20 minutes
- Memory requirements: <500MB

### 2. Spatial Processing
```bash
# Ensure raw data files are in working directory
02_spatial_allocation.ipynb
```
- Requires patient registration files for all years
- Runtime: ~6 minutes
- Memory requirements: ~200MB

### 3. Analysis Preparation
```bash
03_data_preprocessing.ipynb
```
- Generates final analysis-ready dataset
- Runtime: <2 minutes

### 4. Statistical Analysis
```r
# Run R Markdown files
04_imd_analysis.Rmd
05_its_analysis.Rmd
```
- Generates publication-ready figures
- Exports statistical summaries

## Output Files

### Processed Datasets
- **WestYorkshire_GPDrugData_by_Practice.csv**: Practice-level monthly prescriptions
- **WestYorkshire_Diabetes_Prescriptions_by_LSOA_Monthly.csv**: LSOA-level allocated prescriptions  
- **diabetes_its_analysis_data_lsoa.csv**: ITS analysis-ready dataset with calculated indicators

### Analysis Results
- **its_model_summary.csv**: Statistical model coefficients and significance tests
- **monthly_summary_for_its.csv**: Aggregated temporal trends

### Visualizations
- **plots_diabetes/**: IMD inequality analysis figures
- **plots_its/**: Interrupted time series analysis plots


## Author

Zihao Li - 201896942 - MSc Geographical Information Systems

---

**Note**: Raw data files are not included in this repository due to size and licensing restrictions. Users must obtain these from the respective data providers as listed above.
