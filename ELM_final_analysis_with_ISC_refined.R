# ISC-Behavioral Analysis for ELM Data
#
# Sample size notes:
#   - Total Qualtrics: 113 subjects
#   - ISC data available: 107 subjects (after fNIRS quality control)
#   - Excluded from analysis: subjects 38, 39, 40 (data quality issues)
#   - Final sample varies by analysis depending on available data for each
#     ROI/signal/stimulus combination

library(tidyverse)
library(broom)
library(gt)
library(ggpubr)
library(lme4)
library(lmerTest)

# 0. Load data ----
isc_df <- read_csv("/Users/saewonchung/Desktop/ELM_MW_data_analysis/ISC_ROI_level_ELM.csv")
qualtrics_df <- read_csv("/Users/saewonchung/Desktop/ELM_MW_data_analysis/Qualtrics_data/Qualtrics_all_merged.csv")
sart_df <- read_csv("/Users/saewonchung/Desktop/ELM_MW_data_analysis/SART_data/SART_results.csv")

# 1. Prepare behavioral data ----
# Combine Qualtrics and SART data
# Note: qualtrics_df$pid is numeric, sart_df$pid is character (filenames) - extract numeric portion
# Note: SART has duplicate pids (38, 101) - keep first occurrence after filtering invalid data
# Note: Second PID 38 entry should actually be PID 39 (to be corrected in source data later)
sart_df <- sart_df %>%
  # Extract numeric PID from filename (e.g., "01_sart..." -> 1, "048_sart..." -> 48)
  mutate(pid = as.numeric(str_extract(pid, "^\\d+"))) %>%
  # Remove rows where pid extraction failed (resulted in NA)
  filter(!is.na(pid)) %>%
  # Remove rows with missing Accuracy data
  filter(!is.na(Accuracy)) %>%
  # Handle duplicates: keep first occurrence for each pid
  distinct(pid, .keep_all = TRUE)

behavioral_df <- left_join(qualtrics_df, sart_df, by = "pid") %>%
  rename(subject = pid) %>%
  # Exclude problematic subjects (38, 39, 40)
  filter(!subject %in% c(38, 39, 40))

cat(sprintf("Behavioral data: %d subjects\n", n_distinct(behavioral_df$subject)))

# 2. Merge ISC with behavioral data ----
merged_df <- isc_df %>%
  left_join(behavioral_df, by = "subject") %>%
  filter(!is.na(mean_isc_z))

cat(sprintf("Merged data: %d subjects with ISC data\n", n_distinct(merged_df$subject)))
cat(sprintf("Total rows: %d\n", nrow(merged_df)))

# Save merged dataset
write_csv(merged_df, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/ISC_behavioral_merged.csv")

# 3. Define variables of interest ----
# Trait-level variables (apply to all stimuli)
behavioral_vars <- c(
  "MW_state_SART",
  "MW_trait_total",
  "Accuracy",
  "MW_Ratio",
  "Loneliness",
  "AQ10_scored",
  "PHQ9_total",
  "GAD_total",
  "SIAS_total",
  "WHO_5_total",
  "ASRS_total"
)

# Stimulus-specific variables (must match ISC stimulus)
# e.g., Mindwandering_Zima only correlates with Zima ISC
stimulus_specific_vars <- c("Mindwandering")

# 4. Spearman correlation analysis: ISC vs Behavioral ----
cat("\nComputing Spearman correlations...\n")

roi_signal_combos <- merged_df %>%
  distinct(channel, signal_type)

correlation_results <- data.frame()

for (i in 1:nrow(roi_signal_combos)) {
  roi <- roi_signal_combos$channel[i]
  signal <- roi_signal_combos$signal_type[i]

  # Loop over Zima vs Splitscreen
  for (stim in c("Zima", "Splitscreen")) {

    subset_df <- merged_df %>%
      filter(channel == roi, signal_type == signal, stimulus == stim)

    # Build list of variables to test for this stimulus
    # Include trait-level vars + stimulus-matched specific vars
    vars_to_test <- behavioral_vars
    for (ss_var in stimulus_specific_vars) {
      vars_to_test <- c(vars_to_test, paste0(ss_var, "_", stim))
    }

    for (var in vars_to_test) {
      # Check if variable exists in data
      if (!var %in% colnames(subset_df)) {
        next
      }

      temp <- subset_df %>%
        select(mean_isc_z, all_of(var)) %>%
        filter(is.finite(mean_isc_z), is.finite(.data[[var]]))

      if (nrow(temp) > 2) {
        test_result <- suppressWarnings(cor.test(temp$mean_isc_z, temp[[var]], method = "spearman"))

        correlation_results <- rbind(correlation_results, data.frame(
          ROI = roi,
          Signal_Type = signal,
          Stimulus = stim,
          Behavioral_Variable = var,
          Spearman_rho = test_result$estimate,
          p_value = test_result$p.value,
          n = nrow(temp)
        ))
      }
    }
  }
}

# 5. FDR correction ----
cat("\nApplying FDR correction...\n")

correlation_results_fdr <- correlation_results %>%
  group_by(Stimulus, Signal_Type, Behavioral_Variable) %>%
  mutate(
    p_fdr = p.adjust(p_value, method = "fdr"),
    sig_uncorrected = p_value < 0.05,
    sig_fdr = p_fdr < 0.05
  ) %>%
  ungroup()

# Save full results
write_csv(correlation_results_fdr, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/ISC_Behavioral_Correlations_FDR.csv")

# Print significant results
cat("\n=== Significant results (FDR < 0.05) ===\n")
sig_results <- correlation_results_fdr %>%
  filter(sig_fdr) %>%
  arrange(Behavioral_Variable, Stimulus, Signal_Type, p_fdr)

print(sig_results)

# Create publication-ready summary table for significant results
if (nrow(sig_results) > 0) {
  sig_results_formatted <- sig_results %>%
    mutate(
      Spearman_rho = round(Spearman_rho, 3),
      p_value = round(p_value, 4),
      p_fdr = round(p_fdr, 4)
    ) %>%
    select(ROI, Signal_Type, Stimulus, Behavioral_Variable, Spearman_rho, p_value, p_fdr, n)

  sig_results_gt <- sig_results_formatted %>%
    gt() %>%
    tab_header(
      title = "Significant ISC-Behavioral Correlations",
      subtitle = "FDR-corrected p < .05"
    ) %>%
    cols_label(
      ROI = "ROI",
      Signal_Type = "Signal",
      Stimulus = "Video",
      Behavioral_Variable = "Behavioral Measure",
      Spearman_rho = "ρ",
      p_value = "p (uncorr)",
      p_fdr = "p (FDR)",
      n = "N"
    ) %>%
    cols_align(align = "center", columns = c(Signal_Type, Stimulus, Spearman_rho, p_value, p_fdr, n)) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = cell_fill(color = "#ffe6e6"),
      locations = cells_body(rows = p_fdr < 0.001)
    ) %>%
    tab_style(
      style = cell_fill(color = "#fff0e6"),
      locations = cells_body(rows = p_fdr >= 0.001 & p_fdr < 0.01)
    ) %>%
    tab_style(
      style = cell_fill(color = "#ffffe6"),
      locations = cells_body(rows = p_fdr >= 0.01 & p_fdr < 0.05)
    ) %>%
    tab_options(
      table.font.size = 11,
      heading.title.font.size = 16,
      heading.subtitle.font.size = 12
    )

  print(sig_results_gt)
  gtsave(sig_results_gt, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/Table_ISC_Significant_Results.html")
}

# 6. Create summary tables for key variables ----
create_correlation_table <- function(var_name, df) {
  df %>%
    filter(Behavioral_Variable == var_name) %>%
    select(ROI, Signal_Type, Stimulus, Spearman_rho, p_value, p_fdr, n, sig_fdr) %>%
    arrange(p_fdr) %>%
    mutate(
      Spearman_rho = round(Spearman_rho, 3),
      p_value = round(p_value, 3),
      p_fdr = round(p_fdr, 3)
    ) %>%
    gt() %>%
    tab_header(title = paste0("ISC – ", var_name, " Correlations")) %>%
    cols_align(align = "center") %>%
    tab_style(
      style = cell_fill(color = "lightblue"),
      locations = cells_body(rows = sig_fdr == TRUE)
    )
}

# Generate tables for key variables
table_mw_state <- create_correlation_table("MW_state_SART", correlation_results_fdr)
table_mw_trait <- create_correlation_table("MW_trait_total", correlation_results_fdr)
table_loneliness <- create_correlation_table("Loneliness", correlation_results_fdr)
table_accuracy <- create_correlation_table("Accuracy", correlation_results_fdr)
table_mw_ratio <- create_correlation_table("MW_Ratio", correlation_results_fdr)
table_mw_zima <- create_correlation_table("Mindwandering_Zima", correlation_results_fdr)
table_mw_splitscreen <- create_correlation_table("Mindwandering_Splitscreen", correlation_results_fdr)

# 7. Heatmap visualization ----
cat("\nGenerating heatmap...\n")

p_heatmap <- correlation_results_fdr %>%
  filter(Behavioral_Variable %in% c("MW_state_SART", "MW_trait_total", "Loneliness", "Accuracy")) %>%
  ggplot(aes(x = Behavioral_Variable, y = ROI, fill = Spearman_rho)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(sig_fdr, "*", "")), size = 6, vjust = 0.75) +
  facet_grid(Signal_Type ~ Stimulus) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "ROI-wise Spearman Correlations (ISC vs Behavioral)",
       fill = "ρ",
       caption = "* = FDR < 0.05") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p_heatmap)

# 8. Mixed-effects models ----
cat("\nFitting mixed-effects models...\n")

# Prepare data for LMM (Zima + HbO)
df_zima_hbo <- merged_df %>%
  filter(stimulus == "Zima", signal_type == "HbO") %>%
  filter(is.finite(mean_isc_z), is.finite(MW_state_SART), is.finite(Loneliness)) %>%
  mutate(
    subject = factor(subject),
    channel = factor(channel)
  )

# Model 1: MW_state only
m1 <- lmer(mean_isc_z ~ MW_state_SART + (1 | subject) + (1 | channel),
           data = df_zima_hbo)

# Model 2: Loneliness only
m2 <- lmer(mean_isc_z ~ Loneliness + (1 | subject) + (1 | channel),
           data = df_zima_hbo)

# Model 3: Both predictors + MW_trait
m3 <- lmer(mean_isc_z ~ MW_state_SART + Loneliness + MW_trait_total +
             (1 | subject) + (1 | channel),
           data = df_zima_hbo)

cat("\n=== Model 1: MW_state_SART only ===\n")
print(summary(m1))

cat("\n=== Model 2: Loneliness only ===\n")
print(summary(m2))

cat("\n=== Model 3: MW_state + Loneliness + MW_trait ===\n")
print(summary(m3))
