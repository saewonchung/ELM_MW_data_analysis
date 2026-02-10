# Exploratory Analysis: Pearson Correlations + Regression
# Quick exploration with Pearson correlation and regression models

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

# Define variables ----
survey_vars <- c(
  "Loneliness",
  "AQ10_scored",
  "PHQ9_total",
  "GAD_total",
  "SIAS_total",
  "WHO_5_total",
  "ASRS_total",
  "MW_trait_total",
  "MW_state_SART",
  "Extraversion",
  "Agreeableness",
  "Conscientiousness",
  "Neuroticism",
  "Openness"
)

outcome_vars <- c(
  "Accuracy",
  "MW_Ratio",
  "Mindwandering_Zima",
  "Mindwandering_Splitscreen"
)

# PART 1: PEARSON CORRELATIONS ----
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
cat("PART 1: PEARSON CORRELATION ANALYSIS (p < 0.05)\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n\n")

all_cors <- list()

for (outcome in outcome_vars) {
  cat(sprintf("\n### %s ###\n", outcome))

  sig_found <- FALSE

  for (survey_var in survey_vars) {
    if (!survey_var %in% colnames(merged_df) || !outcome %in% colnames(merged_df)) {
      next
    }

    temp_df <- merged_df %>%
      select(all_of(c(survey_var, outcome))) %>%
      filter(is.finite(.data[[survey_var]]), is.finite(.data[[outcome]]))

    if (nrow(temp_df) < 3) next

    cor_test <- suppressWarnings(cor.test(temp_df[[survey_var]], temp_df[[outcome]],
                                          method = "pearson"))

    if (cor_test$p.value < 0.05) {
      sig_found <- TRUE
      all_cors[[length(all_cors) + 1]] <- data.frame(
        Outcome = outcome,
        Predictor = survey_var,
        r = cor_test$estimate,
        p_value = cor_test$p.value,
        n = nrow(temp_df)
      )

      cat(sprintf("  ✓ %s: r = %.3f, p = %.4f, n = %d\n",
                  survey_var, cor_test$estimate, cor_test$p.value, nrow(temp_df)))
    }
  }

  if (!sig_found) {
    cat("  (No significant correlations found)\n")
  }
}

# Save correlation results
if (length(all_cors) > 0) {
  cor_results <- bind_rows(all_cors) %>% arrange(p_value)
  write_csv(cor_results, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/Pearson_Correlations.csv")

  cat("\n\n")
  cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
  cat(sprintf("Total significant correlations: %d\n", nrow(cor_results)))
  cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
}

# PART 2: SIMPLE REGRESSION (유의한 predictor만 출력) ----
cat("\n\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
cat("PART 2: SIMPLE REGRESSION ANALYSIS (p < 0.05)\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n\n")

simple_reg_results <- list()

for (outcome in outcome_vars) {
  cat(sprintf("\n### DV: %s ###\n", outcome))

  sig_found <- FALSE

  for (survey_var in survey_vars) {
    if (!survey_var %in% colnames(merged_df) || !outcome %in% colnames(merged_df)) {
      next
    }

    # Filter complete cases
    temp_df <- merged_df %>%
      select(all_of(c(survey_var, outcome))) %>%
      filter(is.finite(.data[[survey_var]]), is.finite(.data[[outcome]]))

    if (nrow(temp_df) < 10) next

    # Simple regression
    frm <- formula(paste0(outcome, " ~ ", survey_var))
    model <- lm(frm, data = temp_df)
    model_summary <- summary(model)

    # Check if predictor is significant
    predictor_p <- model_summary$coefficients[2, 4]

    if (!is.na(predictor_p) && predictor_p < 0.05) {
      sig_found <- TRUE

      cat(sprintf("\n  %s ~ %s\n", outcome, survey_var))
      cat(sprintf("    β = %.4f, SE = %.4f, t = %.3f, p = %.4f\n",
                  model_summary$coefficients[2, 1],
                  model_summary$coefficients[2, 2],
                  model_summary$coefficients[2, 3],
                  model_summary$coefficients[2, 4]))
      cat(sprintf("    R² = %.4f, Adjusted R² = %.4f, n = %d\n",
                  model_summary$r.squared,
                  model_summary$adj.r.squared,
                  nrow(temp_df)))

      simple_reg_results[[length(simple_reg_results) + 1]] <- data.frame(
        Outcome = outcome,
        Predictor = survey_var,
        Beta = model_summary$coefficients[2, 1],
        SE = model_summary$coefficients[2, 2],
        t_value = model_summary$coefficients[2, 3],
        p_value = model_summary$coefficients[2, 4],
        R_squared = model_summary$r.squared,
        Adj_R_squared = model_summary$adj.r.squared,
        n = nrow(temp_df)
      )
    }
  }

  if (!sig_found) {
    cat("  (No significant predictors found)\n")
  }
}

# Save simple regression results
if (length(simple_reg_results) > 0) {
  simple_reg_df <- bind_rows(simple_reg_results) %>% arrange(p_value)
  write_csv(simple_reg_df, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/Simple_Regression_Results.csv")
}

# PART 3: MULTIPLE REGRESSION (outcome별로 모든 유의한 predictor 포함) ----
cat("\n\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
cat("PART 3: MULTIPLE REGRESSION ANALYSIS\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n\n")

multiple_reg_results <- list()

for (outcome in outcome_vars) {
  # Get significant predictors from simple regression
  sig_predictors <- simple_reg_df %>%
    filter(Outcome == outcome, p_value < 0.05) %>%
    pull(Predictor)

  if (length(sig_predictors) == 0) {
    cat(sprintf("\n### %s: No significant predictors for multiple regression ###\n", outcome))
    next
  }

  # Prepare data with complete cases
  vars_to_select <- c(outcome, sig_predictors)
  temp_df <- merged_df %>%
    select(all_of(vars_to_select)) %>%
    drop_na()

  if (nrow(temp_df) < 20) {
    cat(sprintf("\n### %s: Insufficient complete cases (n = %d) ###\n", outcome, nrow(temp_df)))
    next
  }

  # Multiple regression
  predictor_string <- paste(sig_predictors, collapse = " + ")
  frm <- formula(paste0(outcome, " ~ ", predictor_string))
  model <- lm(frm, data = temp_df)
  model_summary <- summary(model)

  cat(sprintf("\n### %s ~ %s ###\n", outcome, predictor_string))
  cat(sprintf("R² = %.4f, Adjusted R² = %.4f, F = %.3f, p = %.4f, n = %d\n\n",
              model_summary$r.squared,
              model_summary$adj.r.squared,
              model_summary$fstatistic[1],
              pf(model_summary$fstatistic[1],
                 model_summary$fstatistic[2],
                 model_summary$fstatistic[3], lower.tail = FALSE),
              nrow(temp_df)))

  print(model_summary$coefficients)

  # Save each predictor's results
  for (i in 2:nrow(model_summary$coefficients)) {
    multiple_reg_results[[length(multiple_reg_results) + 1]] <- data.frame(
      Outcome = outcome,
      Predictor = rownames(model_summary$coefficients)[i],
      Beta = model_summary$coefficients[i, 1],
      SE = model_summary$coefficients[i, 2],
      t_value = model_summary$coefficients[i, 3],
      p_value = model_summary$coefficients[i, 4],
      Model_R_squared = model_summary$r.squared,
      Model_Adj_R_squared = model_summary$adj.r.squared,
      n = nrow(temp_df)
    )
  }
}

# Save multiple regression results
if (length(multiple_reg_results) > 0) {
  multiple_reg_df <- bind_rows(multiple_reg_results)
  write_csv(multiple_reg_df, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/Multiple_Regression_Results.csv")
}

# PART 4: STEPWISE REGRESSION (exploratory) ----
cat("\n\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
cat("PART 4: STEPWISE REGRESSION (Backward Selection)\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n\n")

for (outcome in outcome_vars) {
  # Prepare complete cases with all survey vars
  vars_to_select <- c(outcome, survey_vars)
  temp_df <- merged_df %>%
    select(all_of(vars_to_select)) %>%
    drop_na()

  if (nrow(temp_df) < 30) {
    cat(sprintf("\n### %s: Insufficient complete cases (n = %d) ###\n", outcome, nrow(temp_df)))
    next
  }

  # Full model
  predictor_string <- paste(survey_vars, collapse = " + ")
  frm_full <- formula(paste0(outcome, " ~ ", predictor_string))
  model_full <- lm(frm_full, data = temp_df)

  # Backward stepwise
  model_step <- step(model_full, direction = "backward", trace = 0)
  model_summary <- summary(model_step)

  # Only show if final model has significant predictors
  if (nrow(model_summary$coefficients) > 1) {
    cat(sprintf("\n### %s: Final Model (n = %d) ###\n", outcome, nrow(temp_df)))
    cat(sprintf("R² = %.4f, Adjusted R² = %.4f\n\n",
                model_summary$r.squared,
                model_summary$adj.r.squared))
    print(model_summary$coefficients)
    cat("\n")
  }
}

cat("\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
cat("✅ Analysis complete!\n")
cat("Output files:\n")
cat("  - Pearson_Correlations.csv\n")
cat("  - Simple_Regression_Results.csv\n")
cat("  - Multiple_Regression_Results.csv\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
