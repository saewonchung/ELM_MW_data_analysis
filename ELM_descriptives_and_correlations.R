# Descriptive Statistics and Correlation Matrix for ELM Study
# Publication-ready tables for sharing

library(tidyverse)
library(gt)
library(Hmisc)
library(corrplot)
library(psych)

# Load data ----
qualtrics_df <- read_csv("/Users/saewonchung/Desktop/ELM_MW_data_analysis/Qualtrics_data/Qualtrics_raw_data_cleaned.csv")
sart_df <- read_csv("/Users/saewonchung/Desktop/ELM_MW_data_analysis/SART_data/SART_results.csv")

# Merge and clean ----
combined_df <- left_join(qualtrics_df, sart_df, by = "pid") %>%
  rename(subject = pid) %>%
  mutate(subject = as.numeric(subject)) %>%
  filter(!subject %in% c(38, 39, 40))

cat(sprintf("Sample size: N = %d\n\n", n_distinct(combined_df$subject)))

# Variables of interest ----
# Primary measures
vars_primary <- c(
  "Loneliness",
  "MW_state_SART",
  "MW_trait_total",
  "Accuracy",
  "MW_Ratio",
  "PHQ9_total",
  "GAD_total",
  "ASRS_total",
  "SIAS_total",
  "WHO_5_total",
  "AQ10_scored"
)

# Big Five personality traits (separate table)
vars_bigfive <- c(
  "Extraversion",
  "Agreeableness",
  "Conscientiousness",
  "Neuroticism",
  "Openness"
)

# Combined for correlations
vars_of_interest <- c(vars_primary, vars_bigfive)

# Variable labels for cleaner display
var_labels <- c(
  "Loneliness" = "Loneliness (UCLA)",
  "MW_state_SART" = "Mind Wandering (State)",
  "MW_trait_total" = "Mind Wandering (Trait)",
  "Accuracy" = "SART Accuracy (%)",
  "MW_Ratio" = "SART MW Ratio",
  "PHQ9_total" = "Depression (PHQ-9)",
  "GAD_total" = "Anxiety (GAD-7)",
  "ASRS_total" = "ADHD (ASRS)",
  "SIAS_total" = "Social Anxiety (SIAS)",
  "WHO_5_total" = "Well-being (WHO-5)",
  "AQ10_scored" = "Autism (AQ-10)",
  "Extraversion" = "Extraversion (BFI)",
  "Agreeableness" = "Agreeableness (BFI)",
  "Conscientiousness" = "Conscientiousness (BFI)",
  "Neuroticism" = "Neuroticism (BFI)",
  "Openness" = "Openness (BFI)"
)

# 1. Descriptive Statistics Table ----
cat("=== Generating Descriptive Statistics Tables ===\n")

# Function to create descriptive table
create_descriptives <- function(vars, title_text) {
  combined_df %>%
    select(all_of(vars)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Variable",
      values_to = "Value"
    ) %>%
    group_by(Variable) %>%
    summarise(
      N = sum(!is.na(Value)),
      Mean = mean(Value, na.rm = TRUE),
      SD = sd(Value, na.rm = TRUE),
      Median = median(Value, na.rm = TRUE),
      Min = min(Value, na.rm = TRUE),
      Max = max(Value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Variable_Label = var_labels[Variable],
      Mean = round(Mean, 2),
      SD = round(SD, 2),
      Median = round(Median, 2),
      Min = round(Min, 2),
      Max = round(Max, 2)
    ) %>%
    select(Variable_Label, N, Mean, SD, Median, Min, Max)
}

# Table 1A: Primary measures
descriptives_primary <- create_descriptives(vars_primary, "Primary Measures")

descriptives_primary_gt <- descriptives_primary %>%
  gt() %>%
  tab_header(
    title = "Table 1A: Descriptive Statistics - Primary Measures",
    subtitle = sprintf("ELM Study (N = %d)", n_distinct(combined_df$subject))
  ) %>%
  cols_align(
    align = "left",
    columns = Variable_Label
  ) %>%
  cols_align(
    align = "center",
    columns = c(N, Mean, SD, Median, Min, Max)
  ) %>%
  cols_label(
    Variable_Label = "Measure",
    N = "N",
    Mean = "M",
    SD = "SD",
    Median = "Mdn",
    Min = "Min",
    Max = "Max"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_fill(color = "#f0f0f0"),
    locations = cells_body(rows = seq(2, nrow(descriptives_primary), 2))
  ) %>%
  opt_row_striping() %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 16,
    heading.subtitle.font.size = 12
  )

print(descriptives_primary_gt)
cat("\n")

# Table 1B: Big Five personality traits
descriptives_bigfive <- create_descriptives(vars_bigfive, "Big Five Personality")

descriptives_bigfive_gt <- descriptives_bigfive %>%
  gt() %>%
  tab_header(
    title = "Table 1B: Descriptive Statistics - Big Five Personality",
    subtitle = sprintf("ELM Study (N = %d)", n_distinct(combined_df$subject))
  ) %>%
  cols_align(
    align = "left",
    columns = Variable_Label
  ) %>%
  cols_align(
    align = "center",
    columns = c(N, Mean, SD, Median, Min, Max)
  ) %>%
  cols_label(
    Variable_Label = "Trait",
    N = "N",
    Mean = "M",
    SD = "SD",
    Median = "Mdn",
    Min = "Min",
    Max = "Max"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_fill(color = "white"),
    locations = cells_body(rows = seq(2, nrow(descriptives_bigfive), 2))
  ) %>%
  opt_row_striping() %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 16,
    heading.subtitle.font.size = 12
  )

print(descriptives_bigfive_gt)

# Save as HTML
gtsave(descriptives_primary_gt, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/Table1A_Descriptives_Primary.html")
gtsave(descriptives_bigfive_gt, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/Table1B_Descriptives_BigFive.html")

# Save as CSV
write_csv(descriptives_primary, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/Table1A_Descriptives_Primary.csv")
write_csv(descriptives_bigfive, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/Table1B_Descriptives_BigFive.csv")

# 2. Correlation Matrix ----
cat("\n=== Generating Correlation Matrix ===\n")

# Compute correlations with p-values
corr_data <- combined_df %>%
  select(all_of(vars_of_interest))

# Use Hmisc::rcorr for correlations with p-values
corr_results <- rcorr(as.matrix(corr_data), type = "pearson")

# Flatten correlation matrix with p-values
flatten_corr_matrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    Variable1 = rownames(cormat)[row(cormat)[ut]],
    Variable2 = rownames(cormat)[col(cormat)[ut]],
    r = cormat[ut],
    p = pmat[ut]
  )
}

corr_flat <- flatten_corr_matrix(corr_results$r, corr_results$P) %>%
  mutate(
    r = round(r, 3),
    p = round(p, 4),
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01 ~ "**",
      p < 0.05 ~ "*",
      TRUE ~ ""
    ),
    r_display = sprintf("%.3f%s", r, sig)
  ) %>%
  arrange(p)

# Save flattened correlation table
write_csv(corr_flat, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/Table2_Correlations_Flat.csv")

# Create correlation matrix for display (lower triangle with significance stars)
corr_matrix_display <- corr_results$r
p_matrix <- corr_results$P

# Add significance stars to correlation values
corr_with_sig <- matrix("", nrow = nrow(corr_matrix_display), ncol = ncol(corr_matrix_display))
for (i in 1:nrow(corr_matrix_display)) {
  for (j in 1:ncol(corr_matrix_display)) {
    if (i == j) {
      corr_with_sig[i, j] <- "—"
    } else if (i > j) {
      r_val <- corr_matrix_display[i, j]
      p_val <- p_matrix[i, j]
      sig_star <- if (p_val < 0.001) "***" else if (p_val < 0.01) "**" else if (p_val < 0.05) "*" else ""
      corr_with_sig[i, j] <- sprintf("%.2f%s", r_val, sig_star)
    }
  }
}

# Convert to data frame with variable names
corr_df_display <- as.data.frame(corr_with_sig)
colnames(corr_df_display) <- 1:length(vars_of_interest)
corr_df_display <- cbind(Variable = vars_of_interest, corr_df_display)

# Create GT table for correlation matrix
corr_gt <- corr_df_display %>%
  mutate(Variable = var_labels[Variable]) %>%
  gt() %>%
  tab_header(
    title = "Correlation Matrix (Pearson's r)",
    subtitle = "Lower triangle shown; * p < .05, ** p < .01, *** p < .001"
  ) %>%
  cols_align(
    align = "left",
    columns = Variable
  ) %>%
  cols_align(
    align = "center",
    columns = -Variable
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_options(
    table.font.size = 10,
    heading.title.font.size = 16,
    heading.subtitle.font.size = 11
  )

print(corr_gt)

# Save correlation matrix
gtsave(corr_gt, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/Table2_Correlations.html")


# Correlation table with exact p-values for reporting in Notion
vars_exclude_table <- c(
  "MW_state_SART",
  "Accuracy",
  "MW_Ratio"
)

vars_for_table <- setdiff(vars_of_interest, vars_exclude_table)

var_labels_table <- c(
  "PHQ9_total"    = "depression",
  "Loneliness"     = "loneliness",
  "GAD_total"      = "anxiety",
  "ASRS_total"     = "ADHD",
  "SIAS_total"     = "social_anxiety",
  "WHO_5_total"    = "well-being",
  "AQ10_scored"    = "autism",
  "Extraversion"   = "extraversion",
  "Agreeableness"  = "agreeableness",
  "Conscientiousness" = "conscientiousness",
  "Neuroticism"    = "neuroticism",
  "Openness"       = "openness",
  "MW_trait_total" = "mindwandering"
)

corr_flat_ordered <- corr_flat %>%
  # Variable1, Variable2 모두에서 제외
  filter(
    Variable1 %in% vars_for_table,
    Variable2 %in% vars_for_table
  ) %>%
  # 순서 고정
  mutate(
    Variable1 = factor(Variable1, levels = vars_for_table),
    Variable2 = factor(Variable2, levels = vars_for_table)
  ) %>%
  arrange(Variable1, Variable2) %>%
  # 라벨 적용
  mutate(
    Variable1_label = ifelse(
      is.na(var_labels_table[as.character(Variable1)]),
      as.character(Variable1),
      var_labels_table[as.character(Variable1)]
    ),
    Variable2_label = ifelse(
      is.na(var_labels_table[as.character(Variable2)]),
      as.character(Variable2),
      var_labels_table[as.character(Variable2)]
    )
  )

corr_p_gt_ordered <- corr_flat_ordered %>%
  select(Variable1_label, Variable2_label, r, p) %>%
  gt(groupname_col = "Variable1_label") %>%
  tab_header(
    title = "Table S1. Pairwise Correlations with Exact p-values",
    subtitle = "Pearson correlations; variables shown in predefined order"
  ) %>%
  cols_label(
    Variable2_label = "Variable 2",
    r = "r",
    p = "p-value"
  ) %>%
  cols_align(align = "left", columns = Variable2_label) %>%
  cols_align(align = "center", columns = c(r, p)) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  tab_options(
    table.font.size = 11,
    row_group.as_column = FALSE
  )

print(corr_p_gt_ordered)


# 3. Correlation Plot (Visual) ----
cat("\n=== Generating Correlation Plot ===\n")

# Shorten variable names for plot
var_names_short <- c(
  "Lonely", "MW_State", "MW_Trait", "Accuracy", "MW_Ratio",
  "PHQ9", "GAD", "ASRS", "SIAS", "WHO5", "AQ10",
  "Extra", "Agree", "Consc", "Neuro", "Open"
)

corr_matrix_plot <- corr_results$r
colnames(corr_matrix_plot) <- var_names_short
rownames(corr_matrix_plot) <- var_names_short

# Save high-resolution plot
png("/Users/saewonchung/Desktop/ELM_MW_data_analysis/Figure1_Correlation_Matrix.png",
    width = 3000, height = 3000, res = 300)

p_mat_plot <- corr_results$P[rownames(corr_matrix_plot), colnames(corr_matrix_plot)] # need to refine

corrplot( # need to refine due to name change in labels
  corr_matrix_plot,
  method = "color",
  type = "lower",
  order = "original",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 1.2,
  p.mat = p_mat_plot,
  sig.level = 0.05,
  insig = "blank",
  addCoef.col = "black",
  number.cex = 0.7,
  col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200),
  diag = FALSE,
  cl.cex = 1.0,
  title = "Correlation Matrix (Pearson's r)",
  mar = c(0, 0, 2, 0)
)

dev.off()

# Correlation Plot for Notion reporting ----
# 1) 사용할 변수 순서 (기존 그대로)
vars_plot <- vars_for_table

# 2) correlation / p-value matrix (같은 변수, 같은 순서)
corr_mat <- corr_results$r[vars_plot, vars_plot]
p_mat    <- corr_results$P[vars_plot, vars_plot]

# 3) long format으로 변환
heatmap_df <- expand.grid(
  Variable1 = vars_plot,
  Variable2 = vars_plot
) %>%
  mutate(
    r = corr_mat[cbind(match(Variable1, vars_plot),
                       match(Variable2, vars_plot))],
    p = p_mat[cbind(match(Variable1, vars_plot),
                    match(Variable2, vars_plot))]
  ) %>%
  filter(Variable1 != Variable2) %>%   # 대각선 제거
  mutate(
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE ~ ""
    ),
    label = sprintf("%.2f%s", r, sig),
    Variable1 = factor(Variable1, levels = vars_plot),
    Variable2 = factor(Variable2, levels = vars_plot)
  )

# 4) Heatmap plot (Notion-friendly)
ggplot(heatmap_df, aes(x = Variable1, y = Variable2, fill = r)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = label), size = 4.5, fontface = "bold") +
  scale_fill_gradient2(
    low = "#6D9EC1",
    mid = "white",
    high = "#E46726",
    midpoint = 0,
    name = "Correlation"
  ) +
  scale_x_discrete(labels = var_labels_table) +
  scale_y_discrete(labels = var_labels_table) +
  coord_fixed() +
  labs(
    title = "Correlation Heatmap",
    x = "Variable X",
    y = "Variable Y"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold")
  )


cat("\n✅ Descriptive statistics and correlation analysis complete!\n\n")
cat("Output files:\n")
cat("  - Table1A_Descriptives_Primary.html/.csv (primary measures)\n")
cat("  - Table1B_Descriptives_BigFive.html/.csv (personality traits)\n")
cat("  - Table2_Correlations.html (formatted correlation matrix)\n")
cat("  - Table2_Correlations_Flat.csv (pairwise correlations)\n")
cat("  - Table3_Significant_Correlations.html (sig correlations only)\n")
cat("  - Figure1_Correlation_Matrix.png (visual plot)\n\n")

# 4. Additional: Significant correlations summary ----
cat("=== Significant Correlations (p < 0.05) ===\n\n")

sig_correlations <- corr_flat %>%
  filter(p < 0.05) %>%
  mutate(
    Variable1 = var_labels[Variable1],
    Variable2 = var_labels[Variable2]
  ) %>%
  select(Variable1, Variable2, r, p, sig)

print(sig_correlations)

# Create formatted table for significant correlations
sig_corr_gt <- sig_correlations %>%
  gt() %>%
  tab_header(
    title = "Significant Correlations",
    subtitle = "Pearson's r, p < .05"
  ) %>%
  cols_label(
    Variable1 = "Variable 1",
    Variable2 = "Variable 2",
    r = "r",
    p = "p-value",
    sig = "Sig"
  ) %>%
  cols_align(align = "center", columns = c(r, p, sig)) %>%
  tab_style(
    style = cell_fill(color = "#ffe6e6"),
    locations = cells_body(rows = p < 0.001)
  ) %>%
  tab_style(
    style = cell_fill(color = "#fff0e6"),
    locations = cells_body(rows = p >= 0.001 & p < 0.01)
  ) %>%
  tab_style(
    style = cell_fill(color = "#ffffe6"),
    locations = cells_body(rows = p >= 0.01 & p < 0.05)
  ) %>%
  tab_options(table.font.size = 12)

print(sig_corr_gt)
gtsave(sig_corr_gt, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/Table3_Significant_Correlations.html")

cat("\nDone! All tables and figures saved.\n")
