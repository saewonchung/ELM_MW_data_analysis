# ISC Moderation Analysis
# Testing if survey variables moderate the relationship between ISC and behavior

library(tidyverse)

# Load data ----
isc_df <- read_csv("/Users/saewonchung/Desktop/ELM_MW_data_analysis/ISC_behavioral_merged.csv")

cat(sprintf("Total observations: %d\n", nrow(isc_df)))
cat(sprintf("Unique subjects: %d\n\n", n_distinct(isc_df$subject)))

# Define variables ----
outcomes <- c("Accuracy", "MW_Ratio", "Mindwandering_Zima", "Mindwandering_Splitscreen")

moderators <- c(
  "Loneliness",
  "PHQ9_total",
  "GAD_total",
  "SIAS_total",
  "MW_trait_total",
  "MW_state_SART",
  "Neuroticism",
  "Extraversion"
)

# MODERATION ANALYSIS ----
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
cat("MODERATION ANALYSIS: Does Survey Variable Moderate ISC → Behavior?\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
cat("Model: Outcome ~ ISC * Moderator\n")
cat("유의한 상호작용(interaction) 효과만 출력\n")
cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n\n")

moderation_results <- list()

for (outcome in outcomes) {

  cat(sprintf("\n### OUTCOME: %s ###\n", outcome))

  sig_found <- FALSE

  for (roi in unique(isc_df$channel)) {
    for (signal in unique(isc_df$signal_type)) {
      for (stim in unique(isc_df$stimulus)) {

        for (moderator in moderators) {

          # Prepare data
          temp_df <- isc_df %>%
            filter(channel == roi, signal_type == signal, stimulus == stim) %>%
            select(subject, mean_isc_z, all_of(outcome), all_of(moderator)) %>%
            filter(
              is.finite(mean_isc_z),
              is.finite(.data[[outcome]]),
              is.finite(.data[[moderator]])
            ) %>%
            distinct(subject, .keep_all = TRUE)

          if (nrow(temp_df) < 30) next

          # Center variables for interpretation
          temp_df <- temp_df %>%
            mutate(
              ISC_c = mean_isc_z - mean(mean_isc_z, na.rm = TRUE),
              Moderator_c = .data[[moderator]] - mean(.data[[moderator]], na.rm = TRUE)
            )

          # Moderation model: Outcome ~ ISC * Moderator
          frm <- formula(paste0(outcome, " ~ ISC_c * Moderator_c"))
          model <- lm(frm, data = temp_df)
          model_summary <- summary(model)

          # Check if interaction is significant
          interaction_p <- model_summary$coefficients[4, 4]  # 4th row = interaction term

          if (!is.na(interaction_p) && interaction_p < 0.05) {

            sig_found <- TRUE

            cat(sprintf("\n  ✓ %s (%s, %s) × %s\n", roi, signal, stim, moderator))
            cat(sprintf("    Interaction β = %.4f, p = %.4f\n",
                        model_summary$coefficients[4, 1],
                        interaction_p))
            cat(sprintf("    Model R² = %.3f, Adjusted R² = %.3f\n",
                        model_summary$r.squared,
                        model_summary$adj.r.squared))
            cat(sprintf("    n = %d\n", nrow(temp_df)))

            # Store results
            moderation_results[[length(moderation_results) + 1]] <- data.frame(
              Outcome = outcome,
              ROI = roi,
              Signal = signal,
              Stimulus = stim,
              Moderator = moderator,
              ISC_main = model_summary$coefficients[2, 1],
              ISC_main_p = model_summary$coefficients[2, 4],
              Moderator_main = model_summary$coefficients[3, 1],
              Moderator_main_p = model_summary$coefficients[3, 4],
              Interaction_beta = model_summary$coefficients[4, 1],
              Interaction_p = interaction_p,
              R_squared = model_summary$r.squared,
              Adj_R_squared = model_summary$adj.r.squared,
              n = nrow(temp_df)
            )
          }
        }
      }
    }
  }

  if (!sig_found) {
    cat("  (No significant moderation effects found)\n")
  }
}

# Save results ----
if (length(moderation_results) > 0) {
  mod_df <- bind_rows(moderation_results) %>%
    arrange(Interaction_p)

  write_csv(mod_df, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/ISC_Moderation_Results.csv")

  cat("\n\n")
  cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
  cat(sprintf("Total significant moderation effects: %d\n", nrow(mod_df)))
  cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n\n")

  # Show top 10 strongest moderation effects
  cat("Top 10 Strongest Moderation Effects:\n\n")
  top_10 <- mod_df %>%
    arrange(Interaction_p) %>%
    head(10) %>%
    select(Outcome, ROI, Signal, Stimulus, Moderator, Interaction_beta, Interaction_p, R_squared, n)

  print(as.data.frame(top_10))

  # DETAILED OUTPUT for strongest effects ----
  cat("\n\n")
  cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n")
  cat("DETAILED BREAKDOWN: Top 3 Moderation Effects\n")
  cat("=" %>% rep(80) %>% paste0(collapse = ""), "\n\n")

  top_3 <- mod_df %>%
    arrange(Interaction_p) %>%
    head(3)

  for (i in 1:nrow(top_3)) {

    row <- top_3[i, ]

    cat(sprintf("\n%d. %s ~ %s (%s, %s) × %s\n",
                i,
                row$Outcome,
                row$ROI,
                row$Signal,
                row$Stimulus,
                row$Moderator))
    cat(sprintf("   Interaction: β = %.4f, p = %.4f ***\n",
                row$Interaction_beta,
                row$Interaction_p))
    cat(sprintf("   ISC main effect: β = %.4f, p = %.4f\n",
                row$ISC_main,
                row$ISC_main_p))
    cat(sprintf("   %s main effect: β = %.4f, p = %.4f\n",
                row$Moderator,
                row$Moderator_main,
                row$Moderator_main_p))
    cat(sprintf("   Model R² = %.3f (n = %d)\n",
                row$R_squared,
                row$n))

    # Interpretation
    cat("\n   Interpretation:\n")
    if (row$Interaction_beta > 0) {
      cat(sprintf("   - ISC → %s 관계가 %s가 높을수록 강해집니다 (양의 상호작용)\n",
                  row$Outcome, row$Moderator))
    } else {
      cat(sprintf("   - ISC → %s 관계가 %s가 높을수록 약해집니다 (음의 상호작용)\n",
                  row$Outcome, row$Moderator))
    }
    cat("\n")
  }

} else {
  cat("\n\nNo significant moderation effects found.\n")
}

cat("\n✅ Moderation analysis complete!\n")
cat("Output file: ISC_Moderation_Results.csv\n")
