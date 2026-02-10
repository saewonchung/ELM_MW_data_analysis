# ELM: Eyetracking Loneliness and Mind-wandering

fNIRS-based mind wandering study: data preprocessing and analysis pipeline

## 📁 Project Structure

```
ELM_MW_data_analysis/
├── NIRx_raw_data/          # Raw fNIRS data
├── ELM_filtered_data/      # Filtered data
├── ELM_preprocessed/       # Preprocessed fNIRS data
├── SART_data/              # SART task data
└── Qualtrics_data/         # Survey data
```

## 🔬 Analysis Pipeline

### 1. Data Preprocessing
- `elm_preprocessing.ipynb` - Main preprocessing notebook

### 2. ISC (Inter-Subject Correlation) Analysis
- `compute-dyad-isc-ELM.R` - Dyad-level ISC computation

### 3. Behavioral Data Analysis
- `ELM_Qualtrics.R` - Qualtrics survey data cleaning
- `prepare_survey2_data.R` - Incorporating Qualtrics survey 2
- `ELM_SART_Probe.R`- SART performance & probe analysis
- `ELM_final_analysis_with_ISC_refined.R` - ISC, Behavioral data analysis
- `ELM_descriptives_and_correlations.R` - Descriptive statistics and correlations
- `ELM_dyad_level_analysis.R` - Dyad-level analysis

## 📊 Key Results

### Tables

### Figures

## Tools & Technologies

- **Python** - Data preprocessing (Jupyter Notebook)
- **R** - Statistical analysis and visualization
- **fNIRS** - NIRx system

## 📧 Contact

Saewon Chung
Email: saewonch@usc.edu
GitHub: [@saewonchung](https://github.com/saewonchung)

---
*Last updated: 2026-02-09*
