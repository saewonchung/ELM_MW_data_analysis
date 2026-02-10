# Dyad-Level ISC Analysis for ELM Data
# Following Baek et al. (2023) 
# "Lonely individuals process the world in idiosyncratic ways"
#
# Key method:
#   1. Doubled dataframe for fully-crossed random effects
#   2. Mixed model: lmer(isc_z ~ predictor + (1|sub1) + (1|sub2))
#   3. Penalized p-value: df/2 to correct for doubled data

library(tidyverse)
library(lme4)
library(lmerTest)
library(broom)
library(gt)

# 0. Load data ----
base_dir <- "/Users/saewonchung/Desktop/ELM_MW_data_analysis"

dyad_isc <- read_csv(file.path(base_dir, "ISC_dyad_level_ELM.csv"), show_col_types = FALSE)
qualtrics_df <- read_csv(file.path(base_dir, "Qualtrics_data/Qualtrics_all_merged.csv"), show_col_types = FALSE)
sart_df <- read_csv(file.path(base_dir, "SART_data/SART_results.csv"), show_col_types = FALSE)

cat(sprintf("Loaded dyad ISC: %d observations\n", nrow(dyad_isc)))
cat(sprintf("Unique dyads per ROI/signal/stimulus: %.0f\n",
            nrow(dyad_isc) / length(unique(dyad_isc$channel)) /
            length(unique(dyad_isc$signal_type)) / length(unique(dyad_isc$stimulus))))

# 1. Prepare behavioral data ----
sart_df <- sart_df %>%
  mutate(pid = as.numeric(pid)) %>%
  distinct(pid, .keep_all = TRUE)

behavioral_df <- left_join(qualtrics_df, sart_df, by = "pid") %>%
  rename(subject = pid) %>%
  filter(!subject %in% c(38, 39, 40))  # Exclude problematic subjects

cat(sprintf("Behavioral data: %d subjects\n", n_distinct(behavioral_df$subject)))

# 2. Merge behavioral data with dyad ISC ----
# Join behavioral data for both sub1 and sub2
dyad_behavioral <- dyad_isc %>%
  mutate(sub1 = as.numeric(sub1), sub2 = as.numeric(sub2)) %>%
  left_join(behavioral_df, by = c("sub1" = "subject"), suffix = c("", "_1")) %>%
  left_join(behavioral_df, by = c("sub2" = "subject"), suffix = c("_1", "_2")) %>%
  filter(!is.na(isc_z))

cat(sprintf("Dyad-behavioral merged: %d observations\n", nrow(dyad_behavioral)))

# 3. Create doubled dataframe (Baek et al. method) ----
# This allows fully-crossed random effects
dyad_doubled <- dyad_behavioral
temp_sub1 <- dyad_doubled$sub1
dyad_doubled$sub1 <- dyad_doubled$sub2
dyad_doubled$sub2 <- temp_sub1
dyad_doubled <- bind_rows(dyad_behavioral, dyad_doubled)

cat(sprintf("Doubled dataframe: %d observations\n", nrow(dyad_doubled)))

# Calculate number of unique dyads for penalized p-value
n_unique_dyad <- n_distinct(paste(pmin(dyad_isc$sub1, dyad_isc$sub2),
                                   pmax(dyad_isc$sub1, dyad_isc$sub2)))
cat(sprintf("Unique dyads: %d\n", n_unique_dyad))

# 4. Compute dyad-level predictors ----
dyad_doubled <- dyad_doubled %>%
  mutate(
    # Max value in dyad (Baek et al. approach)
    Loneliness_max = pmax(Loneliness_1, Loneliness_2, na.rm = TRUE),
    MW_state_max = pmax(MW_state_SART_1, MW_state_SART_2, na.rm = TRUE),
    MW_trait_max = pmax(MW_trait_total_1, MW_trait_total_2, na.rm = TRUE),
    PHQ9_max = pmax(PHQ9_total_1, PHQ9_total_2, na.rm = TRUE),
    GAD_max = pmax(GAD_total_1, GAD_total_2, na.rm = TRUE),
    SIAS_max = pmax(SIAS_total_1, SIAS_total_2, na.rm = TRUE),
    AQ10_max = pmax(AQ10_scored_1, AQ10_scored_2, na.rm = TRUE),
    ASRS_max = pmax(ASRS_total_1, ASRS_total_2, na.rm = TRUE),
    WHO5_max = pmax(WHO_5_total_1, WHO_5_total_2, na.rm = TRUE),

    # Stimulus-specific mindwandering (max in dyad)
    Mindwandering_Zima_max = pmax(Mindwandering_Zima_1, Mindwandering_Zima_2, na.rm = TRUE),
    Mindwandering_Splitscreen_max = pmax(Mindwandering_Splitscreen_1, Mindwandering_Splitscreen_2, na.rm = TRUE),

    # Binary classification for loneliness
    Loneliness_bin_1 = ifelse(Loneliness_1 > median(Loneliness_1, na.rm = TRUE), "high", "low"),
    Loneliness_bin_2 = ifelse(Loneliness_2 > median(Loneliness_2, na.rm = TRUE), "high", "low"),
    dyad_loneliness_binary = case_when(
      Loneliness_bin_1 == "low" & Loneliness_bin_2 == "low" ~ "low_low",
      Loneliness_bin_1 == "high" & Loneliness_bin_2 == "high" ~ "high_high",
      TRUE ~ "low_high"
    ),

    # Binary classification for MW_state
    MW_state_bin_1 = ifelse(MW_state_SART_1 > median(MW_state_SART_1, na.rm = TRUE), "high", "low"),
    MW_state_bin_2 = ifelse(MW_state_SART_2 > median(MW_state_SART_2, na.rm = TRUE), "high", "low"),
    dyad_MW_state_binary = case_when(
      MW_state_bin_1 == "low" & MW_state_bin_2 == "low" ~ "low_low",
      MW_state_bin_1 == "high" & MW_state_bin_2 == "high" ~ "high_high",
      TRUE ~ "low_high"
    )
  )

# 5. Define analysis function (continuous predictor) ----
run_dyad_analysis_continuous <- function(data, predictor_name) {
  # Filter out NA values for the predictor
  predictor_col <- paste0(predictor_name, "_max")

  if (!predictor_col %in% colnames(data)) {
    return(NULL)
  }

  data_clean <- data %>%
    filter(!is.na(.data[[predictor_col]]), !is.na(isc_z))

  if (nrow(data_clean) < 100) {
    return(NULL)
  }

  # Calculate unique dyads for this subset (for penalized df, Baek et al. method)
  n_unique_dyad_subset <- n_distinct(paste(pmin(data_clean$sub1, data_clean$sub2),
                                            pmax(data_clean$sub1, data_clean$sub2)))

  tryCatch({
    # Fit mixed model
    formula_str <- paste0("scale(isc_z) ~ scale(", predictor_col, ") + (1|sub1) + (1|sub2)")
    md <- lmer(as.formula(formula_str), data = data_clean)
    md_summary <- summary(md)

    # Extract coefficients
    B <- md_summary$coefficients[2, 1]
    se <- md_summary$coefficients[2, 2]
    t_val <- md_summary$coefficients[2, 4]

    # Use n_unique_dyad - 2 as df (Baek et al. method)
    df_penalized <- n_unique_dyad_subset - 2

    # Penalized p-value (df/2 for doubled data)
    p_penalized <- 2 * pt(-abs(t_val), df_penalized / 2)

    return(data.frame(
      predictor = predictor_name,
      B = B,
      SE = se,
      df = df_penalized,
      t = t_val,
      p_penalized = p_penalized,
      n_obs = nrow(data_clean),
      n_unique_dyad = n_unique_dyad_subset
    ))
  }, error = function(e) {
    return(NULL)
  })
}

# 6. Run analysis for each ROI × Signal × Stimulus × Predictor ----
cat("\n=== Running dyad-level analysis (Baek et al. method) ===\n")

# Trait-level predictors (apply to all stimuli)
trait_predictors <- c(
  "Loneliness", "MW_state", "MW_trait",
  "PHQ9", "GAD", "SIAS", "AQ10", "ASRS", "WHO5"
)

# Stimulus-specific predictors (only match with corresponding stimulus)
stimulus_specific_predictors <- c("Mindwandering_Zima", "Mindwandering_Splitscreen")

results_list <- list()

for (roi in unique(dyad_doubled$channel)) {
  for (signal in unique(dyad_doubled$signal_type)) {
    for (stim in unique(dyad_doubled$stimulus)) {

      subset_data <- dyad_doubled %>%
        filter(channel == roi, signal_type == signal, stimulus == stim)

      # Trait-level predictors
      for (pred in trait_predictors) {
        result <- run_dyad_analysis_continuous(subset_data, pred)

        if (!is.null(result)) {
          result$ROI <- roi
          result$signal_type <- signal
          result$stimulus <- stim
          results_list[[length(results_list) + 1]] <- result
        }
      }

      # Stimulus-specific predictor (match stimulus)
      stim_pred <- paste0("Mindwandering_", stim)
      if (stim_pred %in% stimulus_specific_predictors) {
        result <- run_dyad_analysis_continuous(subset_data, stim_pred)

        if (!is.null(result)) {
          result$ROI <- roi
          result$signal_type <- signal
          result$stimulus <- stim
          results_list[[length(results_list) + 1]] <- result
        }
      }
    }
  }
}

results_df <- bind_rows(results_list)

# 7. Apply FDR correction ----
cat("\nApplying FDR correction...\n")

results_fdr <- results_df %>%
  group_by(predictor) %>%
  mutate(
    p_fdr = p.adjust(p_penalized, method = "fdr"),
    sig_uncorrected = p_penalized < 0.05,
    sig_fdr = p_fdr < 0.05
  ) %>%
  ungroup()

# 8. Save results ----
output_file <- file.path(base_dir, "Dyad_ISC_Behavioral_Results.csv")
write_csv(results_fdr, output_file)
cat(sprintf("\nResults saved to: %s\n", output_file))

# 9. Print significant results ----
cat("\n=== Significant results (FDR < 0.05) ===\n")
sig_results <- results_fdr %>%
  filter(sig_fdr) %>%
  arrange(predictor, p_fdr)

if (nrow(sig_results) > 0) {
  print(sig_results %>% select(ROI, signal_type, stimulus, predictor, B, t, p_penalized, p_fdr))
} else {
  cat("No significant results after FDR correction.\n")
}

cat("\n=== Significant results (uncorrected p < 0.05) ===\n")
sig_uncorr <- results_fdr %>%
  filter(sig_uncorrected) %>%
  arrange(predictor, p_penalized)

print(sig_uncorr %>% select(ROI, signal_type, stimulus, predictor, B, t, p_penalized, p_fdr))

# 10. Binary analysis (Anna Karenina model) ----
cat("\n=== Binary Analysis (Anna Karenina model) ===\n")

library(emmeans)

run_binary_analysis <- function(data, binary_var, debug = FALSE) {
  data_clean <- data %>%
    filter(!is.na(.data[[binary_var]]), !is.na(isc_z))

  n_levels <- length(unique(data_clean[[binary_var]]))

  if (debug) {
    cat(sprintf("  Data rows: %d, levels: %d\n", nrow(data_clean), n_levels))
  }

  if (nrow(data_clean) < 100 || n_levels < 3) {
    if (debug) cat(sprintf("  Skipping: rows=%d, levels=%d\n", nrow(data_clean), n_levels))
    return(NULL)
  }

  # Set factor levels explicitly (alphabetical: high_high, low_high, low_low)
  data_clean[[binary_var]] <- factor(data_clean[[binary_var]],
                                      levels = c("high_high", "low_high", "low_low"))

  # Calculate unique dyads for this subset (for penalized df)
  n_unique_dyad_subset <- n_distinct(paste(pmin(data_clean$sub1, data_clean$sub2),
                                            pmax(data_clean$sub1, data_clean$sub2)))

  tryCatch({
    formula_str <- paste0("scale(isc_z) ~ ", binary_var, " + (1|sub1) + (1|sub2)")
    md <- lmer(as.formula(formula_str), data = data_clean)

    # Get estimated marginal means with lmerTest.limit disabled (we use our own df)
    # Factor order: high_high (1), low_high (2), low_low (3)
    Contrasts <- list(
      HHvsLL = c(1, 0, -1),   # high_high vs low_low
      HHvsLH = c(1, -1, 0),   # high_high vs low_high
      LHvsLL = c(0, 1, -1)    # low_high vs low_low
    )

    emm_formula <- as.formula(paste0("~ ", binary_var))
    emm_md <- emmeans(md, emm_formula, contr = Contrasts, adjust = "none",
                      lmerTest.limit = Inf, pbkrtest.limit = Inf)
    contrast_summary <- as.data.frame(summary(emm_md$contrasts))

    # Extract with penalized p-values
    # Use n_unique_dyad - 2 as df (Baek et al. method), not emmeans df
    df_penalized <- n_unique_dyad_subset - 2

    results <- data.frame(
      contrast = contrast_summary$contrast,
      B = contrast_summary$estimate,
      SE = contrast_summary$SE,
      t = contrast_summary$t.ratio,
      df = df_penalized,  # Use unique dyad count for df
      n_unique_dyad = n_unique_dyad_subset
    )

    # Penalized p-value (df/2 for doubled data)
    results$p_penalized <- 2 * pt(-abs(results$t), df_penalized / 2)

    return(results)
  }, error = function(e) {
    cat(sprintf("  Error in %s: %s\n", binary_var, e$message))
    return(NULL)
  })
}

# Run binary analysis for all ROIs × Signals × Stimuli
binary_vars <- c("dyad_loneliness_binary", "dyad_MW_state_binary")
binary_results_all <- list()

for (binary_var in binary_vars) {
  var_name <- gsub("dyad_|_binary", "", binary_var)

  for (roi in unique(dyad_doubled$channel)) {
    for (signal in unique(dyad_doubled$signal_type)) {
      for (stim in unique(dyad_doubled$stimulus)) {

        subset_data <- dyad_doubled %>%
          filter(channel == roi, signal_type == signal, stimulus == stim)

        result <- run_binary_analysis(subset_data, binary_var, debug = TRUE)

        if (!is.null(result)) {
          result$ROI <- roi
          result$signal_type <- signal
          result$stimulus <- stim
          result$variable <- var_name
          binary_results_all[[length(binary_results_all) + 1]] <- result
        }
      }
    }
  }
}

if (length(binary_results_all) > 0) {
  binary_df <- bind_rows(binary_results_all) %>%
    group_by(variable, contrast) %>%
    mutate(p_fdr = p.adjust(p_penalized, method = "fdr")) %>%
    ungroup()

  cat("\n=== Binary Analysis Results (FDR < 0.05) ===\n")
  sig_binary <- binary_df %>% filter(p_fdr < 0.05)
  if (nrow(sig_binary) > 0) {
    print(sig_binary %>%
            select(variable, ROI, signal_type, stimulus, contrast, B, t, p_penalized, p_fdr) %>%
            arrange(variable, p_fdr))
  } else {
    cat("No significant results after FDR correction.\n")
  }

  cat("\n=== Binary Analysis Results (uncorrected p < 0.05) ===\n")
  sig_binary_uncorr <- binary_df %>% filter(p_penalized < 0.05)
  print(sig_binary_uncorr %>%
          select(variable, ROI, signal_type, stimulus, contrast, B, t, p_penalized, p_fdr) %>%
          arrange(variable, p_penalized))

  write_csv(binary_df, file.path(base_dir, "Dyad_ISC_Binary_Results.csv"))
}

# 11. Summary statistics ----
cat("\n=== Summary ===\n")
cat(sprintf("Total tests: %d\n", nrow(results_fdr)))
cat(sprintf("Significant (uncorrected p < 0.05): %d\n", sum(results_fdr$sig_uncorrected)))
cat(sprintf("Significant (FDR < 0.05): %d\n", sum(results_fdr$sig_fdr)))

cat("\n✅ Dyad-level analysis complete!\n")



