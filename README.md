# ELM Mind Wandering Data Analysis

fNIRS-based mind wandering study: data preprocessing and analysis pipeline

## 📁 Project Structure

```
ELM_MW_data_analysis/
├── NIRx_raw_data/          # Raw fNIRS data
├── ELM_preprocessed/       # Preprocessed fNIRS data
├── SART_data/              # SART task data
├── Qualtrics_data/         # Survey data
└── ELM_filtered_data/      # Filtered data
```

## 🔬 Analysis Pipeline

### 1. Data Preprocessing
- `elm_preprocessing.ipynb` - Main preprocessing notebook
- `nirxprep.ipynb` - NIRx data preprocessing
- `relabel_education_sart.ipynb` - SART data relabeling

### 2. ISC (Inter-Subject Correlation) Analysis
- `compute-dyad-isc-ELM.R` - Dyad-level ISC computation
- `ISC_Simple_Analysis.R` - Basic ISC analysis
- `ISC_Brain_Exploratory_Analysis.R` - Brain region exploratory analysis
- `ISC_Moderation_Analysis.R` - Moderation analysis
- `fNIRS_ISC Analysis_v02_ROI (1).R` - ROI-level analysis

### 3. Behavioral Data Analysis
- `ELM_descriptives_and_correlations.R` - Descriptive statistics and correlations
- `ELM_dyad_level_analysis.R` - Dyad-level analysis
- `Survey_Accuracy_Correlation_Analysis.R` - Survey-accuracy correlation
- `Exploratory_Survey_Correlations.R` - Exploratory correlation analysis
- `Exploratory_Pearson_Regression_Analysis.R` - Regression analysis

## 📊 Key Results

### Tables
- `Table1A_Descriptives_Primary.csv/html` - Descriptive statistics
- `Table2_Correlations_Flat.csv` - Correlation matrix
- `ISC_Behavioral_Correlations_FDR.csv` - ISC-behavioral correlations (FDR corrected)

### Figures
- `Figure1_Correlation_Matrix.png` - Correlation heatmap
- `Survey_Accuracy_Correlation_Matrix.png` - Survey-accuracy correlations
- Various visualization outputs (Rplot*.png)

## 🛠️ Tools & Technologies

- **R** - Statistical analysis and visualization
- **Python** - Data preprocessing (Jupyter Notebook)
- **fNIRS** - NIRx system

## 📝 Notes

See comments in individual script files for detailed methodology and results.

## 📧 Contact

Saewon Chung
Email: saewonch@usc.edu
GitHub: [@saewonchung](https://github.com/saewonchung)

---
*Last updated: 2026-02-09*
