# Exploratory ISC Brain Data Analysis
# Multiple analyses on ISC data

library(tidyverse)
library(lme4)
library(lmerTest)
library(broom)
library(ggpubr)
library(corrplot)

# Load data ----
isc_df <- read_csv("/Users/saewonchung/Desktop/ELM_MW_data_analysis/ISC_behavioral_merged.csv")

cat(sprintf("Total observations: %d\n", nrow(isc_df)))
cat(sprintf("Unique subjects: %d\n", n_distinct(isc_df$subject)))
cat(sprintf("ROIs: %s\n", paste(unique(isc_df$channel), collapse = ", ")))
cat("\n")

# PART 1: ROI COMPARISONS ----
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
cat("PART 1: ROI COMPARISONS (Which ROI has highest/lowest ISC?)\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n\n")

roi_summary <- isc_df %>%
  filter(is.finite(mean_isc_z)) %>%
  group_by(channel, signal_type, stimulus) %>%
  summarise(
    mean_ISC = mean(mean_isc_z, na.rm = TRUE),
    sd_ISC = sd(mean_isc_z, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_ISC))

print(roi_summary)
write_csv(roi_summary, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/ISC_ROI_Summary.csv")

# PART 2: STIMULUS EFFECTS (Zima vs Splitscreen) ----
cat("\n\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
cat("PART 2: STIMULUS EFFECTS (유의한 차이만 출력)\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n\n")

stimulus_results <- list()

for (roi in unique(isc_df$channel)) {
  for (signal in unique(isc_df$signal_type)) {

    # Prepare wide format for paired t-test
    wide_df <- isc_df %>%
      filter(channel == roi, signal_type == signal, is.finite(mean_isc_z)) %>%
      select(subject, stimulus, mean_isc_z) %>%
      pivot_wider(names_from = stimulus, values_from = mean_isc_z) %>%
      filter(!is.na(Zima), !is.na(Splitscreen), is.finite(Zima), is.finite(Splitscreen))

    if (nrow(wide_df) < 10) next

    # Check if data has variance
    if (sd(wide_df$Zima, na.rm = TRUE) < 1e-10 || sd(wide_df$Splitscreen, na.rm = TRUE) < 1e-10) next

    # Paired t-test
    t_result <- tryCatch(
      t.test(wide_df$Zima, wide_df$Splitscreen, paired = TRUE),
      error = function(e) NULL
    )

    if (is.null(t_result)) next

    if (t_result$p.value < 0.05) {
      cat(sprintf("\n%s (%s): Zima vs Splitscreen\n", roi, signal))
      cat(sprintf("  Zima: M = %.4f, Splitscreen: M = %.4f\n",
                  mean(wide_df$Zima), mean(wide_df$Splitscreen)))
      cat(sprintf("  t(%d) = %.3f, p = %.4f, d = %.3f\n",
                  t_result$parameter,
                  t_result$statistic,
                  t_result$p.value,
                  t_result$statistic / sqrt(nrow(wide_df))))

      stimulus_results[[length(stimulus_results) + 1]] <- data.frame(
        ROI = roi,
        Signal = signal,
        Mean_Zima = mean(wide_df$Zima),
        Mean_Splitscreen = mean(wide_df$Splitscreen),
        t_value = t_result$statistic,
        df = t_result$parameter,
        p_value = t_result$p.value,
        n = nrow(wide_df)
      )
    }
  }
}

if (length(stimulus_results) > 0) {
  stim_df <- bind_rows(stimulus_results)
  write_csv(stim_df, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/ISC_Stimulus_Effects.csv")
} else {
  cat("  (No significant stimulus effects found)\n")
}

# PART 3: SIGNAL TYPE EFFECTS (HbO vs HbR) ----
cat("\n\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
cat("PART 3: SIGNAL TYPE EFFECTS (HbO vs HbR, 유의한 차이만)\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n\n")

signal_results <- list()

for (roi in unique(isc_df$channel)) {
  for (stim in unique(isc_df$stimulus)) {

    wide_df <- isc_df %>%
      filter(channel == roi, stimulus == stim, is.finite(mean_isc_z)) %>%
      select(subject, signal_type, mean_isc_z) %>%
      pivot_wider(names_from = signal_type, values_from = mean_isc_z) %>%
      filter(!is.na(HbO), !is.na(HbR), is.finite(HbO), is.finite(HbR))

    if (nrow(wide_df) < 10) next

    # Check variance
    if (sd(wide_df$HbO, na.rm = TRUE) < 1e-10 || sd(wide_df$HbR, na.rm = TRUE) < 1e-10) next

    t_result <- tryCatch(
      t.test(wide_df$HbO, wide_df$HbR, paired = TRUE),
      error = function(e) NULL
    )

    if (is.null(t_result)) next

    if (t_result$p.value < 0.05) {
      cat(sprintf("\n%s (%s): HbO vs HbR\n", roi, stim))
      cat(sprintf("  HbO: M = %.4f, HbR: M = %.4f\n",
                  mean(wide_df$HbO), mean(wide_df$HbR)))
      cat(sprintf("  t(%d) = %.3f, p = %.4f\n",
                  t_result$parameter,
                  t_result$statistic,
                  t_result$p.value))

      signal_results[[length(signal_results) + 1]] <- data.frame(
        ROI = roi,
        Stimulus = stim,
        Mean_HbO = mean(wide_df$HbO),
        Mean_HbR = mean(wide_df$HbR),
        t_value = t_result$statistic,
        p_value = t_result$p.value,
        n = nrow(wide_df)
      )
    }
  }
}

if (length(signal_results) > 0) {
  signal_df <- bind_rows(signal_results)
  write_csv(signal_df, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/ISC_Signal_Effects.csv")
} else {
  cat("  (No significant signal type effects found)\n")
}

# PART 4: ROI-ROI CORRELATIONS (같은 stimulus, signal에서) ----
cat("\n\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
cat("PART 4: ROI-ROI CORRELATIONS (유의한 상관관계만)\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n\n")

roi_cor_results <- list()

for (stim in unique(isc_df$stimulus)) {
  for (signal in unique(isc_df$signal_type)) {

    cat(sprintf("\n### %s - %s ###\n", stim, signal))

    # Create wide format with ROIs as columns
    wide_roi <- isc_df %>%
      filter(stimulus == stim, signal_type == signal) %>%
      select(subject, channel, mean_isc_z) %>%
      pivot_wider(names_from = channel, values_from = mean_isc_z) %>%
      select(-subject) %>%
      drop_na()

    if (nrow(wide_roi) < 20) {
      cat("  (Insufficient data)\n")
      next
    }

    # Correlation matrix
    cor_mat <- cor(wide_roi, use = "complete.obs")

    # Extract significant correlations
    rois <- colnames(wide_roi)
    sig_found <- FALSE

    for (i in 1:(length(rois)-1)) {
      for (j in (i+1):length(rois)) {

        cor_test <- tryCatch(
          cor.test(wide_roi[[rois[i]]], wide_roi[[rois[j]]]),
          error = function(e) NULL
        )

        if (!is.null(cor_test) && !is.na(cor_test$p.value) && cor_test$p.value < 0.05) {
          sig_found <- TRUE
          cat(sprintf("  ✓ %s - %s: r = %.3f, p = %.4f\n",
                      rois[i], rois[j], cor_test$estimate, cor_test$p.value))

          roi_cor_results[[length(roi_cor_results) + 1]] <- data.frame(
            Stimulus = stim,
            Signal = signal,
            ROI_1 = rois[i],
            ROI_2 = rois[j],
            r = cor_test$estimate,
            p_value = cor_test$p.value,
            n = nrow(wide_roi)
          )
        }
      }
    }

    if (!sig_found) {
      cat("  (No significant ROI-ROI correlations)\n")
    }
  }
}

if (length(roi_cor_results) > 0) {
  roi_cor_df <- bind_rows(roi_cor_results) %>% arrange(p_value)
  write_csv(roi_cor_df, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/ISC_ROI_Correlations.csv")
}

# PART 5: ISC PREDICTING BEHAVIORAL OUTCOMES ----
cat("\n\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
cat("PART 5: ISC → BEHAVIORAL OUTCOMES (유의한 predictor만)\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n\n")

outcomes <- c("Accuracy", "MW_Ratio", "Mindwandering_Zima", "Mindwandering_Splitscreen")

isc_predict_results <- list()

for (outcome in outcomes) {

  cat(sprintf("\n### DV: %s ###\n", outcome))

  sig_found <- FALSE

  for (roi in unique(isc_df$channel)) {
    for (signal in unique(isc_df$signal_type)) {
      for (stim in unique(isc_df$stimulus)) {

        temp_df <- isc_df %>%
          filter(channel == roi, signal_type == signal, stimulus == stim) %>%
          select(subject, mean_isc_z, all_of(outcome)) %>%
          filter(is.finite(mean_isc_z), is.finite(.data[[outcome]])) %>%
          distinct(subject, .keep_all = TRUE)

        if (nrow(temp_df) < 20) next

        # Simple regression
        frm <- formula(paste0(outcome, " ~ mean_isc_z"))
        model <- lm(frm, data = temp_df)
        model_summary <- summary(model)

        predictor_p <- model_summary$coefficients[2, 4]

        if (!is.na(predictor_p) && predictor_p < 0.05) {
          sig_found <- TRUE

          cat(sprintf("  %s (%s, %s): β = %.4f, p = %.4f, R² = %.3f, n = %d\n",
                      roi, signal, stim,
                      model_summary$coefficients[2, 1],
                      predictor_p,
                      model_summary$r.squared,
                      nrow(temp_df)))

          isc_predict_results[[length(isc_predict_results) + 1]] <- data.frame(
            Outcome = outcome,
            ROI = roi,
            Signal = signal,
            Stimulus = stim,
            Beta = model_summary$coefficients[2, 1],
            p_value = predictor_p,
            R_squared = model_summary$r.squared,
            n = nrow(temp_df)
          )
        }
      }
    }
  }

  if (!sig_found) {
    cat("  (No significant ISC predictors)\n")
  }
}

if (length(isc_predict_results) > 0) {
  isc_pred_df <- bind_rows(isc_predict_results) %>% arrange(p_value)
  write_csv(isc_pred_df, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/ISC_Predicting_Behavior.csv")
}

# PART 6: HIGH vs LOW ISC GROUPS (median split) ----
cat("\n\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
cat("PART 6: HIGH vs LOW ISC GROUPS (Survey 특성 비교)\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n\n")

survey_vars <- c("Loneliness", "PHQ9_total", "GAD_total", "SIAS_total",
                 "MW_trait_total", "MW_state_SART", "Neuroticism", "Extraversion")

group_comp_results <- list()

# Use vmPFC HbO Zima as example (most theory-driven ROI)
vmPFC_data <- isc_df %>%
  filter(channel == "vmPFC", signal_type == "HbO", stimulus == "Zima") %>%
  select(subject, mean_isc_z, all_of(survey_vars)) %>%
  distinct(subject, .keep_all = TRUE) %>%
  filter(is.finite(mean_isc_z))

if (nrow(vmPFC_data) > 20) {

  cat("Using vmPFC (HbO, Zima) for group comparison\n\n")

  # Median split
  median_isc <- median(vmPFC_data$mean_isc_z, na.rm = TRUE)
  vmPFC_data <- vmPFC_data %>%
    mutate(ISC_group = ifelse(mean_isc_z >= median_isc, "High", "Low"))

  for (survey_var in survey_vars) {

    if (!survey_var %in% colnames(vmPFC_data)) next

    temp_df <- vmPFC_data %>%
      select(ISC_group, all_of(survey_var)) %>%
      filter(is.finite(.data[[survey_var]]))

    if (nrow(temp_df) < 10) next

    # t-test
    t_result <- t.test(as.formula(paste0(survey_var, " ~ ISC_group")), data = temp_df)

    if (t_result$p.value < 0.05) {

      means <- temp_df %>%
        group_by(ISC_group) %>%
        summarise(M = mean(.data[[survey_var]], na.rm = TRUE), .groups = "drop")

      cat(sprintf("  ✓ %s: High M = %.3f, Low M = %.3f, p = %.4f\n",
                  survey_var,
                  means$M[means$ISC_group == "High"],
                  means$M[means$ISC_group == "Low"],
                  t_result$p.value))

      group_comp_results[[length(group_comp_results) + 1]] <- data.frame(
        Survey_Variable = survey_var,
        High_ISC_Mean = means$M[means$ISC_group == "High"],
        Low_ISC_Mean = means$M[means$ISC_group == "Low"],
        t_value = t_result$statistic,
        p_value = t_result$p.value
      )
    }
  }
}

if (length(group_comp_results) > 0) {
  group_comp_df <- bind_rows(group_comp_results)
  write_csv(group_comp_df, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/ISC_Group_Comparison.csv")
} else {
  cat("  (No significant group differences)\n")
}

# SUMMARY ----
cat("\n\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
cat("✅ ISC EXPLORATORY ANALYSIS COMPLETE!\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
cat("Output files:\n")
cat("  - ISC_ROI_Summary.csv\n")
cat("  - ISC_Stimulus_Effects.csv (if significant)\n")
cat("  - ISC_Signal_Effects.csv (if significant)\n")
cat("  - ISC_ROI_Correlations.csv (if significant)\n")
cat("  - ISC_Predicting_Behavior.csv (if significant)\n")
cat("  - ISC_Group_Comparison.csv (if significant)\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
