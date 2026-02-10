# Exploratory Survey-Performance Correlation Analysis
# Quick exploration to find significant correlations only

library(tidyverse)

# Load data ----
qualtrics_df <- read_csv("/Users/saewonchung/Desktop/ELM_MW_data_analysis/Qualtrics_data/Qualtrics_all_merged.csv")
sart_df <- read_csv("/Users/saewonchung/Desktop/ELM_MW_data_analysis/SART_data/SART_results.csv")

# Prepare SART data
sart_df <- sart_df %>%
  mutate(pid = as.numeric(str_extract(pid, "^\\d+"))) %>%
  filter(!is.na(pid), !is.na(Accuracy)) %>%
  distinct(pid, .keep_all = TRUE)

# Merge data
merged_df <- left_join(qualtrics_df, sart_df, by = "pid") %>%
  filter(!pid %in% c(38, 39, 40))

cat(sprintf("Total subjects: %d\n\n", nrow(merged_df)))

# Define survey variables to explore ----
survey_vars <- c(
  # Mental health
  "Loneliness",
  "AQ10_scored",
  "PHQ9_total",
  "GAD_total",
  "SIAS_total",
  "WHO_5_total",
  "ASRS_total",

  # Mind wandering
  "MW_trait_total",
  "MW_state_SART",

  # Personality (Big Five)
  "Extraversion",
  "Agreeableness",
  "Conscientiousness",
  "Neuroticism",
  "Openness"
)

# Define performance/outcome variables ----
performance_vars <- c(
  # SART performance
  "Accuracy",
  "MW_Ratio",

  # Video-specific mind wandering
  "Mindwandering_Zima",
  "Mindwandering_Splitscreen",

  # Video enjoyment/appreciation
  "Enjoy_1_Zima",
  "Enjoy_1_Splitscreen",
  "Enjoy_2_Zima",
  "Enjoy_2_Splitscreen",
  "Enjoy_3_Zima",
  "Enjoy_3_Splitscreen",
  "Appreciate_1_Zima",
  "Appreciate_1_Splitscreen",
  "Appreciate_2_Zima",
  "Appreciate_2_Splitscreen",
  "Appreciate_3_Zima",
  "Appreciate_3_Splitscreen",
  "Interest_Zima",
  "Interest_Splitscreen",
  "Familiarity_Zima",
  "Familiarity_Splitscreen"
)

# Function to test correlation and print if significant ----
test_correlation <- function(df, var1, var2, alpha = 0.05) {
  # Check if both variables exist
  if (!var1 %in% colnames(df) || !var2 %in% colnames(df)) {
    return(NULL)
  }

  # Filter valid data
  temp_df <- df %>%
    select(all_of(c(var1, var2))) %>%
    filter(is.finite(.data[[var1]]), is.finite(.data[[var2]]))

  if (nrow(temp_df) < 3) {
    return(NULL)
  }

  # Spearman correlation
  cor_test <- suppressWarnings(cor.test(temp_df[[var1]], temp_df[[var2]], method = "spearman"))

  # Return result if significant
  if (cor_test$p.value < alpha) {
    return(data.frame(
      Survey_Variable = var1,
      Performance_Variable = var2,
      rho = cor_test$estimate,
      p_value = cor_test$p.value,
      n = nrow(temp_df)
    ))
  }

  return(NULL)
}

# EXPLORATORY ANALYSIS: Find all significant correlations ----
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
cat("EXPLORATORY CORRELATION ANALYSIS (p < 0.05)\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n\n")

all_results <- list()

for (survey_var in survey_vars) {
  cat(sprintf("\n### %s ###\n", survey_var))

  sig_found <- FALSE

  for (perf_var in performance_vars) {
    result <- test_correlation(merged_df, survey_var, perf_var, alpha = 0.05)

    if (!is.null(result)) {
      sig_found <- TRUE
      all_results[[length(all_results) + 1]] <- result

      cat(sprintf("  ✓ %s: ρ = %.3f, p = %.4f, n = %d\n",
                  perf_var,
                  result$rho,
                  result$p_value,
                  result$n))
    }
  }

  if (!sig_found) {
    cat("  (No significant correlations found)\n")
  }
}

# Combine all results and save ----
if (length(all_results) > 0) {
  combined_results <- bind_rows(all_results) %>%
    arrange(p_value)

  write_csv(combined_results, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/Exploratory_Significant_Correlations.csv")

  cat("\n\n")
  cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
  cat("SUMMARY: Top 20 Strongest Significant Correlations\n")
  cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n\n")

  top_20 <- combined_results %>%
    arrange(desc(abs(rho))) %>%
    head(20)

  print(as.data.frame(top_20))

  cat("\n\n")
  cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
  cat(sprintf("Total significant correlations found: %d\n", nrow(combined_results)))
  cat(sprintf("Results saved to: Exploratory_Significant_Correlations.csv\n"))
  cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")

} else {
  cat("\n\nNo significant correlations found.\n")
}

# BONUS: Test for interaction effects (moderation) ----
cat("\n\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
cat("BONUS: Testing Interaction Effects (p < 0.05)\n")
cat("Moderator: Loneliness\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n\n")

# Center the moderator
merged_df_centered <- merged_df %>%
  mutate(Loneliness_c = Loneliness - mean(Loneliness, na.rm = TRUE))

# Test survey vars predicting performance with Loneliness as moderator
outcome_vars <- c("Accuracy", "Mindwandering_Zima", "Mindwandering_Splitscreen", "MW_Ratio")
predictor_vars <- c("PHQ9_total", "GAD_total", "SIAS_total", "MW_state_SART", "Neuroticism")

for (outcome in outcome_vars) {
  if (!outcome %in% colnames(merged_df_centered)) next

  for (predictor in predictor_vars) {
    if (!predictor %in% colnames(merged_df_centered)) next

    # Center predictor
    predictor_c <- paste0(predictor, "_c")
    merged_df_centered[[predictor_c]] <- merged_df_centered[[predictor]] -
      mean(merged_df_centered[[predictor]], na.rm = TRUE)

    # Interaction model
    formula_str <- paste0(outcome, " ~ ", predictor_c, " * Loneliness_c")
    model <- lm(as.formula(formula_str), data = merged_df_centered)
    model_summary <- summary(model)

    # Check if interaction is significant
    interaction_p <- model_summary$coefficients[4, 4]

    if (!is.na(interaction_p) && interaction_p < 0.05) {
      cat(sprintf("\n### %s ~ %s × Loneliness ###\n", outcome, predictor))
      print(model_summary)
    }
  }
}

cat("\n✅ Exploratory analysis complete!\n")
