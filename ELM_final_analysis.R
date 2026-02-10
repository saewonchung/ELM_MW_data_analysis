library(tidyverse)
library(dplyr)
library(stringr)
library(dplyr)
library(readr)
library(readxl)
library(lubridate)
library(corrplot)
library(Hmisc)
library(broom)

# 0. Importing all data (Qualtrics survey results, SART results) at one dataframe 

# Qualtrics_data <- read_csv("/Users/saewonchung/Desktop/ELM_MW_data_analysis/Qualtrics_data/Qualtrics_raw_data_cleaned.csv")
Qualtrics_data <- read_csv("/Users/saewonchung/Desktop/ELM_MW_data_analysis/Qualtrics_data/Qualtrics_all_merged.csv") # including survey 2
SART_data <- read_csv("/Users/saewonchung/Desktop/ELM_MW_data_analysis/SART_data/SART_results.csv")

# change pid into numerical values (e.g., change 015 into 15)
# change pid=38b from Qualtrics_data into pid = 115
Qualtrics_data <- Qualtrics_data %>%
  # mutate(pid = recode(pid, "38b" = "115")) %>%
  mutate(pid = as.numeric(pid)) 

# change one of the pid=38 from SART_data into pid = 115
SART_data <- SART_data %>%
  # mutate(pid = recode(pid, "38" = "39")) %>%
  mutate(pid = as.numeric(pid))

# need to come back and figure out how to sort out pid 38, 39, 40; for now we exclude all of them to avoid confusion
combined_df <- left_join(Qualtrics_data,SART_data, by = "pid")

combined_df <- combined_df %>%
  filter(!pid %in% c(38, 39, 40))

# write_csv(combined_df, "combined_temp.csv")

# 1. Linking Loneliness ratings with SART and Probe results 
names(combined_df)

var_name <- c("Accuracy", "MW_Ratio", "MW_trait_total", "MW_state_SART",
              "PHQ9_total", "ASRS_total", "SIAS_total", "WHO_5_total", "GAD_total", "AQ10_scored")

# Regression coefficient

model_results <- lapply(var_name, function(var) {
  form <- as.formula(paste("Loneliness ~", var))
  model <- lm(form, data = combined_df)
  tidy(model) %>%
    mutate(predictor = var)
})

summary_table <- do.call(rbind, model_results)
summary_table <- summary_table[summary_table$term != "(Intercept)", ]
print(summary_table[, c("predictor", "estimate", "std.error", "statistic", "p.value")])

# Descriptives
cor_results <- corr.test(vars, use = "pairwise.complete.obs"); 
cor_results$r
res <- rcorr(as.matrix(vars))

for (i in 1:nrow(flattened)) {
  row <- flattened[i, ]
  corr_val <- sprintf("%.2f", row$correlation)
  
  if (row$p_value < 0.05) {
    corr_val <- paste0("\033[31m", corr_val, "\033[0m")  # red text
  }
  
  cat(sprintf("%-15s %-15s %-10s p = %.3f\n", 
              row$var1, row$var2, corr_val, row$p_value))
}

flatt.cor.matrix <- flattenCorrMatrix(res$r, res$P)


library(gt)

vars_of_interest <- c(
  "Loneliness",
  "Accuracy",
  "MW_Ratio",
  "MW_trait_total",
  "MW_state_SART",
  "PHQ9_total",
  "ASRS_total",
  "SIAS_total",
  "WHO_5_total",
  "GAD_total",
  "AQ10_scored"
)

descriptives <- combined_df %>%
  select(all_of(vars_of_interest)) %>%
  summarise(
    across(
      everything(),
      list(
        n = ~ sum(!is.na(.)),
        mean = ~ mean(., na.rm = TRUE),
        sd = ~ sd(., na.rm = TRUE),
        min = ~ min(., na.rm = TRUE),
        max = ~ max(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

descriptives

descriptives_long %>%
  mutate(
    Mean = round(Mean, 2),
    SD = round(SD, 2),
    Min = round(Min, 2),
    Max = round(Max, 2)
  ) %>%
  gt() %>%
  tab_header(title = "Descriptive Statistics") %>%
  cols_align(align = "center") %>%
  cols_label(
    Variable = "Variable",
    n = "N",
    Mean = "Mean",
    SD = "SD",
    Min = "Min",
    Max = "Max"
  )

descriptives_long <- combined_df %>%
  select(all_of(vars_of_interest)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  group_by(Variable) %>%
  summarise(
    n = sum(!is.na(Value)),
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    .groups = "drop"
  )

descriptives_long

library(gt)

descriptives_long %>%
  mutate(
    Mean = round(Mean, 2),
    SD = round(SD, 2),
    Min = round(Min, 2),
    Max = round(Max, 2)
  ) %>%
  gt() %>%
  tab_header(title = "Descriptive Statistics") %>%
  cols_align(align = "center") %>%
  cols_label(
    Variable = "Variable",
    n = "N",
    Mean = "Mean",
    SD = "SD",
    Min = "Min",
    Max = "Max"
  )

corr_df <- combined_df %>%
  select(all_of(vars_of_interest))

library(tidyverse)

corr_table <- corr_df %>%
  cor(use = "pairwise.complete.obs", method = "pearson") %>%
  as.data.frame() %>%
  rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "r") %>%
  filter(Var1 != Var2)

library(Hmisc)

corr_results <- rcorr(as.matrix(corr_df), type = "pearson")

corr_results$r   # correlation coefficients
corr_results$P   # p-values
corr_results$n   # pairwise N

p_table <- corr_results$P %>%
  as.data.frame() %>%
  rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "p")

corr_long <- left_join(corr_table, p_table, by = c("Var1", "Var2"))

library(corrplot)

corrplot(
  corr_results$r,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  p.mat = corr_results$P,
  sig.level = 0.05,
  insig = "blank"
)

