# ISC Simple Analysis: Prediction & Group Comparison
library(tidyverse)

# Load data ----
isc_df <- read_csv("/Users/saewonchung/Desktop/ELM_MW_data_analysis/ISC_behavioral_merged.csv")

cat(sprintf("Total observations: %d\n", nrow(isc_df)))
cat(sprintf("Unique subjects: %d\n\n", n_distinct(isc_df$subject)))

outcomes <- c("Accuracy", "MW_Ratio", "Mindwandering_Zima", "Mindwandering_Splitscreen")
survey_vars <- c("Loneliness", "PHQ9_total", "GAD_total", "SIAS_total",
                 "MW_trait_total", "MW_state_SART", "Neuroticism", "Extraversion",
                 "AQ10_scored", "ASRS_total", "WHO_5_total")

# PART 1: ISC PREDICTING BEHAVIORAL OUTCOMES ----
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
cat("PART 1: ISC → BEHAVIORAL OUTCOMES (FDR-corrected)\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n\n")

# First, collect ALL results (not filtering by p-value yet)
isc_predict_results <- list()

for (outcome in outcomes) {
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

        if (!is.na(predictor_p)) {
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
}

# Apply FDR correction
if (length(isc_predict_results) > 0) {
  isc_pred_df <- bind_rows(isc_predict_results)
  isc_pred_df$p_adjusted <- p.adjust(isc_pred_df$p_value, method = "fdr")
  isc_pred_df <- isc_pred_df %>% arrange(p_adjusted)

  # Report significant results after FDR correction
  cat(sprintf("Total tests performed: %d\n", nrow(isc_pred_df)))
  sig_results <- isc_pred_df %>% filter(p_adjusted < 0.05)
  cat(sprintf("Significant after FDR correction (q < 0.05): %d\n\n", nrow(sig_results)))

  if (nrow(sig_results) > 0) {
    for (i in 1:nrow(sig_results)) {
      row <- sig_results[i, ]
      cat(sprintf("  %s: %s (%s, %s)\n    β = %.4f, p = %.4f, q = %.4f, R² = %.3f, n = %d\n",
                  row$Outcome, row$ROI, row$Signal, row$Stimulus,
                  row$Beta, row$p_value, row$p_adjusted, row$R_squared, row$n))
    }
  } else {
    cat("  (No significant ISC predictors after FDR correction)\n")
  }

  write_csv(isc_pred_df, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/ISC_Predicting_Behavior.csv")
}

# PART 2: HIGH vs LOW ISC GROUPS ----
cat("\n\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
cat("PART 2: HIGH vs LOW ISC GROUPS (FDR-corrected)\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n\n")

# First, collect ALL results (not filtering by p-value yet)
group_comp_results <- list()

for (roi in unique(isc_df$channel)) {
  for (signal in unique(isc_df$signal_type)) {
    for (stim in unique(isc_df$stimulus)) {

      roi_data <- isc_df %>%
        filter(channel == roi, signal_type == signal, stimulus == stim) %>%
        select(subject, mean_isc_z, all_of(survey_vars)) %>%
        distinct(subject, .keep_all = TRUE) %>%
        filter(is.finite(mean_isc_z))

      if (nrow(roi_data) < 30) next

      # Median split
      median_isc <- median(roi_data$mean_isc_z, na.rm = TRUE)
      roi_data <- roi_data %>%
        mutate(ISC_group = ifelse(mean_isc_z >= median_isc, "High", "Low"))

      for (survey_var in survey_vars) {

        if (!survey_var %in% colnames(roi_data)) next

        temp_df <- roi_data %>%
          select(ISC_group, all_of(survey_var)) %>%
          filter(is.finite(.data[[survey_var]]))

        if (nrow(temp_df) < 20) next

        # t-test
        t_result <- tryCatch(
          t.test(as.formula(paste0(survey_var, " ~ ISC_group")), data = temp_df),
          error = function(e) NULL
        )

        if (is.null(t_result)) next

        means <- temp_df %>%
          group_by(ISC_group) %>%
          summarise(M = mean(.data[[survey_var]], na.rm = TRUE), .groups = "drop")

        high_mean <- means$M[means$ISC_group == "High"]
        low_mean <- means$M[means$ISC_group == "Low"]

        group_comp_results[[length(group_comp_results) + 1]] <- data.frame(
          ROI = roi,
          Signal = signal,
          Stimulus = stim,
          Survey_Variable = survey_var,
          High_ISC_Mean = high_mean,
          Low_ISC_Mean = low_mean,
          t_value = t_result$statistic,
          p_value = t_result$p.value,
          n = nrow(temp_df)
        )
      }
    }
  }
}

# Apply FDR correction
if (length(group_comp_results) > 0) {
  group_comp_df <- bind_rows(group_comp_results)
  group_comp_df$p_adjusted <- p.adjust(group_comp_df$p_value, method = "fdr")
  group_comp_df <- group_comp_df %>% arrange(p_adjusted)

  # Report significant results after FDR correction
  cat(sprintf("Total tests performed: %d\n", nrow(group_comp_df)))
  sig_results <- group_comp_df %>% filter(p_adjusted < 0.05)
  cat(sprintf("Significant after FDR correction (q < 0.05): %d\n\n", nrow(sig_results)))

  if (nrow(sig_results) > 0) {
    current_roi <- ""
    for (i in 1:nrow(sig_results)) {
      row <- sig_results[i, ]
      roi_label <- sprintf("%s (%s, %s)", row$ROI, row$Signal, row$Stimulus)

      if (roi_label != current_roi) {
        cat(sprintf("\n### %s ###\n", roi_label))
        current_roi <- roi_label
      }

      cat(sprintf("  ✓ %s: High M = %.3f, Low M = %.3f\n    p = %.4f, q = %.4f\n",
                  row$Survey_Variable, row$High_ISC_Mean, row$Low_ISC_Mean,
                  row$p_value, row$p_adjusted))
    }
  } else {
    cat("  (No significant group differences after FDR correction)\n")
  }

  write_csv(group_comp_df, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/ISC_Group_Comparison.csv")

  cat("\n\n")
  cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
  cat(sprintf("Total significant group differences (FDR q < 0.05): %d\n", nrow(sig_results)))
  cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
} else {
  cat("\n  (No tests performed)\n")
}

cat("\n✅ Analysis complete!\n")
cat("Output files:\n")
cat("  - ISC_Predicting_Behavior.csv\n")
cat("  - ISC_Group_Comparison.csv\n")
