# Survey Data vs. SART Accuracy Correlation Analysis
# Exploring relationships between psychological measures and task performance

library(tidyverse)
library(corrplot)
library(Hmisc)
library(ggpubr)
library(gt)

# Load data ----
qualtrics_df <- read_csv("/Users/saewonchung/Desktop/ELM_MW_data_analysis/Qualtrics_data/Qualtrics_all_merged.csv")
sart_df <- read_csv("/Users/saewonchung/Desktop/ELM_MW_data_analysis/SART_data/SART_results.csv")

# Prepare SART data ----
sart_df <- sart_df %>%
  mutate(pid = as.numeric(str_extract(pid, "^\\d+"))) %>%
  filter(!is.na(pid), !is.na(Accuracy)) %>%
  distinct(pid, .keep_all = TRUE)

# Merge data ----
merged_df <- left_join(qualtrics_df, sart_df, by = "pid") %>%
  filter(!pid %in% c(38, 39, 40))  # Exclude problematic subjects

cat(sprintf("Total subjects: %d\n", nrow(merged_df)))
cat(sprintf("Subjects with valid Accuracy: %d\n", sum(!is.na(merged_df$Accuracy))))

# Define variables of interest ----
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

# Performance variables
performance_vars <- c(
  "Accuracy",
  "MW_Ratio",
  "Mindwandering_Zima",
  "Mindwandering_Splitscreen"
)

# 1. Correlation analysis: Survey vs Accuracy ----
cat("\n=== Correlating Survey Variables with SART Accuracy ===\n")

correlation_results <- data.frame()

for (survey_var in survey_vars) {
  # Check if variable exists
  if (!survey_var %in% colnames(merged_df)) {
    next
  }

  # Filter valid data
  temp_df <- merged_df %>%
    select(all_of(survey_var), Accuracy) %>%
    filter(is.finite(.data[[survey_var]]), is.finite(Accuracy))

  if (nrow(temp_df) > 2) {
    # Spearman correlation
    cor_test <- cor.test(temp_df[[survey_var]], temp_df$Accuracy, method = "spearman")

    correlation_results <- rbind(correlation_results, data.frame(
      Survey_Variable = survey_var,
      Performance_Variable = "Accuracy",
      Spearman_rho = cor_test$estimate,
      p_value = cor_test$p.value,
      n = nrow(temp_df)
    ))
  }
}

# Sort by p-value
correlation_results <- correlation_results %>%
  mutate(
    sig = p_value < 0.05
  ) %>%
  arrange(p_value)

print(correlation_results)

# Save results
write_csv(correlation_results, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/Survey_Accuracy_Correlations.csv")

# Print significant results
cat("\n=== Significant correlations (p < 0.05) ===\n")
print(correlation_results %>% filter(sig))

# 2. Extended correlation: Survey vs all performance measures ----
cat("\n=== Extended Analysis: Survey vs All Performance Measures ===\n")

extended_results <- data.frame()

for (survey_var in survey_vars) {
  if (!survey_var %in% colnames(merged_df)) next

  for (perf_var in performance_vars) {
    if (!perf_var %in% colnames(merged_df)) next

    temp_df <- merged_df %>%
      select(all_of(survey_var), all_of(perf_var)) %>%
      filter(is.finite(.data[[survey_var]]), is.finite(.data[[perf_var]]))

    if (nrow(temp_df) > 2) {
      cor_test <- cor.test(temp_df[[survey_var]], temp_df[[perf_var]], method = "spearman")

      extended_results <- rbind(extended_results, data.frame(
        Survey_Variable = survey_var,
        Performance_Variable = perf_var,
        Spearman_rho = cor_test$estimate,
        p_value = cor_test$p.value,
        n = nrow(temp_df)
      ))
    }
  }
}

# Sort by p-value
extended_results <- extended_results %>%
  mutate(
    sig = p_value < 0.05
  ) %>%
  arrange(p_value)

write_csv(extended_results, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/Survey_Performance_Extended_Correlations.csv")

# Print significant findings
cat("\n=== Significant Extended Correlations (p < 0.05) ===\n")
print(extended_results %>% filter(sig))

# 3. Correlation matrix visualization ----
cat("\n=== Creating correlation matrix ===\n")

# Select complete cases for visualization
vars_for_matrix <- c("Accuracy", survey_vars)
matrix_df <- merged_df %>%
  select(all_of(vars_for_matrix)) %>%
  drop_na()

cat(sprintf("Complete cases for matrix: %d\n", nrow(matrix_df)))

# Compute correlation matrix
cor_matrix <- cor(matrix_df, method = "spearman")

# Save correlation matrix
write_csv(as.data.frame(cor_matrix), "/Users/saewonchung/Desktop/ELM_MW_data_analysis/Correlation_Matrix.csv")

# Plot correlation matrix
png("/Users/saewonchung/Desktop/ELM_MW_data_analysis/Survey_Accuracy_Correlation_Matrix.png",
    width = 1200, height = 1000, res = 120)
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         title = "Spearman Correlations: Survey Variables & SART Accuracy",
         mar = c(0,0,2,0))
dev.off()

# 4. Scatterplots for top correlations ----
cat("\n=== Generating scatterplots for top correlations ===\n")

# Get top 4 strongest correlations with Accuracy
top_cors <- correlation_results %>%
  arrange(desc(abs(Spearman_rho))) %>%
  head(4)

plot_list <- list()

for (i in 1:nrow(top_cors)) {
  var_name <- top_cors$Survey_Variable[i]

  plot_df <- merged_df %>%
    select(all_of(var_name), Accuracy) %>%
    filter(is.finite(.data[[var_name]]), is.finite(Accuracy))

  # Calculate correlation for annotation
  cor_result <- cor.test(plot_df[[var_name]], plot_df$Accuracy, method = "spearman")
  cor_label <- sprintf("ρ = %.3f, p = %.3f", cor_result$estimate, cor_result$p.value)

  p <- ggplot(plot_df, aes(x = .data[[var_name]], y = Accuracy)) +
    geom_point(alpha = 0.6, size = 2.5, color = "#1f78b4") +
    geom_smooth(method = "lm", se = TRUE, color = "#e31a1c", fill = "#fb9a99") +
    annotate("text", x = Inf, y = Inf, label = cor_label,
             hjust = 1.1, vjust = 1.5, size = 4) +
    labs(
      title = paste0(var_name, " vs. SART Accuracy"),
      x = var_name,
      y = "SART Accuracy"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold")
    )

  plot_list[[i]] <- p
}

# Combine plots
combined_plot <- ggarrange(plotlist = plot_list, ncol = 2, nrow = 2)
ggsave("/Users/saewonchung/Desktop/ELM_MW_data_analysis/Top_Survey_Accuracy_Scatterplots.png",
       combined_plot, width = 12, height = 10)

# 5. Summary table for publication ----
summary_table <- correlation_results %>%
  mutate(
    Spearman_rho = round(Spearman_rho, 3),
    p_value = round(p_value, 4),
    Significance = case_when(
      sig ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(Survey_Variable, Spearman_rho, p_value, Significance, n, sig)

summary_gt <- summary_table %>%
  gt() %>%
  tab_header(
    title = "Survey Variables × SART Accuracy",
    subtitle = "Spearman Correlations"
  ) %>%
  cols_label(
    Survey_Variable = "Survey Variable",
    Spearman_rho = "ρ",
    p_value = "p",
    Significance = "Sig.",
    n = "N"
  ) %>%
  cols_align(align = "center", columns = c(Spearman_rho, p_value, Significance, n)) %>%
  tab_style(
    style = cell_fill(color = "#ffe6e6"),
    locations = cells_body(rows = sig == TRUE)
  ) %>%
  tab_footnote(
    footnote = "* = p < 0.05",
    locations = cells_column_labels(columns = Significance)
  )

print(summary_gt)
gtsave(summary_gt, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/Survey_Accuracy_Summary_Table.html")

cat("\n✅ Analysis complete!\n")
cat("Output files:\n")
cat("  - Survey_Accuracy_Correlations.csv\n")
cat("  - Survey_Performance_Extended_Correlations.csv\n")
cat("  - Correlation_Matrix.csv\n")
cat("  - Survey_Accuracy_Correlation_Matrix.png\n")
cat("  - Top_Survey_Accuracy_Scatterplots.png\n")
cat("  - Survey_Accuracy_Summary_Table.html\n")
