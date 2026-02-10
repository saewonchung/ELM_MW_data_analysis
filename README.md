# Eyetracking Loneliness and Mind-wandering (ELM)

fNIRS-based mind wandering study: data preprocessing and analysis pipeline

## 📁 Project Structure

```
ELM_MW_data_analysis/
│
├── 📊 Data Directories
│   ├── NIRx_raw_data/           # Raw fNIRS data 
│   ├── ELM_filtered_data/       # ELM-sorted data
│   ├── ELM_preprocessed/        # Preprocessed fNIRS data 
│   ├── Qualtrics_data/          # Survey data
│   └── SART_data/               # SART task data
│
├── 🔬 Analysis Scripts
│   │
│   ├── Data Preprocessing
│   │   └── elm_preprocessing.ipynb           # Main fNIRS preprocessing pipeline
│   │
│   ├── Survey Data Processing
│   │   └── Qualtrics_data/
│   │       └── Qualtrics_data_cleaning.R     # Survey 4 & 2 cleaning and merging
│   │
│   ├── SART Analysis
│   │   └── SART_data/
│   │       └── ELM_SART_Probe.R              # SART performance & probe analysis
│   │
│   ├── ISC Analysis
│   │   ├── compute-dyad-isc-ELM.R            # Dyad-level ISC computation
│   │   └── compute-dyad-isc-ELM.Rmd          # ISC computation (R Markdown)
│   │
│   └── Integrated Analysis
│       ├── ELM_final_analysis_with_ISC_refined.R    # Main ISC + behavioral analysis
│       └── ELM_descriptives_and_correlations.R      # Descriptive stats & correlations
│
├── 📦 Output Files
│   ├── Qualtrics_all_merged.csv             # Final merged survey data
│   ├── ISC_ROI_level_ELM.csv                # ROI-level ISC results
│   ├── Table*.csv/html                       # Analysis result tables
│   └── Figure*.png                           # Visualization outputs
│
└── 🗄️ archive/                              # Deprecated/old scripts
```

## 🔬 Analysis Pipeline

### 1. Data Preprocessing
**fNIRS Data:**
- [elm_preprocessing.ipynb](elm_preprocessing.ipynb) - Raw NIRx data → preprocessed hemodynamic signals

**Survey Data:**
- [Qualtrics_data/Qualtrics_data_cleaning.R](Qualtrics_data/Qualtrics_data_cleaning.R) - Survey 4 + Survey 2 → merged dataset

**SART Data:**
- [SART_data/ELM_SART_Probe.R](SART_data/ELM_SART_Probe.R) - SART performance metrics & mind-wandering probes

### 2. ISC (Inter-Subject Correlation) Analysis
- [compute-dyad-isc-ELM.R](compute-dyad-isc-ELM.R) - Compute dyad-level ISC from preprocessed fNIRS data
- [compute-dyad-isc-ELM.Rmd](compute-dyad-isc-ELM.Rmd) - ISC computation with documentation

### 3. Integrated Behavioral & Neural Analysis
- [ELM_final_analysis_with_ISC_refined.R](ELM_final_analysis_with_ISC_refined.R) - Main analysis: ISC + behavioral correlations
- [ELM_descriptives_and_correlations.R](ELM_descriptives_and_correlations.R) - Descriptive statistics & correlation matrices

### Analysis Workflow
```
NIRx_raw_data → ELM_filtered_data → elm_preprocessing.ipynb → ELM_preprocessed
                                                                      ↓
                                                            compute-dyad-isc-ELM.R
                                                                      ↓
                                                                ISC_ROI_level_ELM.csv
                                                                      
Qualtrics surveys → Qualtrics_data_cleaning.R → Qualtrics_all_merged.csv
                                                                      
SART_data → ELM_SART_Probe.R → SART_results.csv                      
                                              ↓                       
                                ELM_final_analysis_with_ISC_refined.R
                                              ↓
                                    📊 Final Results & Figures
```

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
