library(tidyverse)

isc_df <- read_csv("/Users/saewonchung/Desktop/ELM_MW_data_analysis/ISC_ROI_level_ELM.csv")
mw_df <- read_csv("/Users/saewonchung/Desktop/ELM_MW_data_analysis/combined_temp.csv")

mw_df <- mw_df %>%
  mutate(rename(mw_df, subject = pid))

merged_df <- isc_df %>%
  left_join(mw_df, by = "subject")

# write_csv(merged_df, "/Users/saewonchung/Desktop/fNIRS\ pilot\ analysis/ISC_ROI_Qualtrics_SART.csv")

# Ensure all behavioral variables are numeric
behavioral_vars <- setdiff(colnames(mw_df), "subject")
mw_df[behavioral_vars] <- lapply(mw_df[behavioral_vars], function(x) as.numeric(as.character(x)))

# Loop through each ROI and signal type
roi_signal_combos <- merged_df %>%
  distinct(channel, signal_type)

results <- data.frame() 

for (i in 1:nrow(roi_signal_combos)) {
  roi <- roi_signal_combos$channel[i]
  signal <- roi_signal_combos$signal_type[i]
  
  # loop over Zima vs Splitscreen
  for (stim in c("Zima", "Splitscreen")) {
    
    subset_df <- merged_df %>%
      filter(channel == roi, signal_type == signal, stimulus == stim)
    
    for (var in behavioral_vars) {
      temp <- subset_df %>%
        dplyr::select(mean_isc_z, all_of(var)) %>%
        filter(is.finite(mean_isc_z), is.finite(.data[[var]]))
      
      if (nrow(temp) > 2) {
        test_result <- suppressWarnings(cor.test(temp$mean_isc_z, temp[[var]], method = "spearman"))
        
        results <- rbind(results, data.frame(
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

print(results)
# write_csv(results, "/Users/saewonchung/Desktop/fNIRS\ pilot\ analysis/ISC_Behavior_Correlations.csv")

significant_results <- results %>% filter(p_value < 0.05)
print(significant_results)


# Multiple comparison correction: filtering by adjusted p < 0.05

results_mw_state_zima_hbo <- results[which(results$Behavioral_Variable=="MW_state_SART" & results$Stimulus=="Zima"
                                           & results$Signal_Type=="HbO"),]
results_mw_state_zima_hbo$p_adj <- p.adjust(results_mw_state_zima_hbo$p_value, method = "fdr")
results_mw_state_splitscreen_hbo <- results[which(results$Behavioral_Variable=="MW_state_SART" & results$Stimulus=="Splitscreen"
                                           & results$Signal_Type=="HbO"),]
results_mw_state_splitscreen_hbo$p_adj <- p.adjust(results_mw_state_zima_hbo$p_value, method = "fdr")
results_mw_trait_zima_hbo <- results[which(results$Behavioral_Variable=="MW_trait_total" & results$Stimulus=="Splitscreen"
                                           & results$Signal_Type=="HbO"),]
results_mw_trait_zima_hbo$p_adj <- p.adjust(results_mw_trait_zima_hbo$p_value, method = "fdr")
results_loneliness_zima_hbo <- results[which(results$Behavioral_Variable=="Loneliness" & results$Stimulus=="Zima"
                                           & results$Signal_Type=="HbO"),]
results_loneliness_zima_hbo$p_adj <- p.adjust(results_loneliness_zima_hbo$p_value, method = "fdr")

# write_csv(results_mw_state_zima_hbo, "/Users/saewonchung/Desktop/fNIRS\ pilot\ analysis/ISC_mw_zima_hbo.csv")
# write_csv(results_loneliness_zima_hbo, "/Users/saewonchung/Desktop/fNIRS\ pilot\ analysis/ISC_loneliness_zima_hbo.csv")


# plotting 
library(ggplot2)

results %>%
  ggplot(aes(x = Behavioral_Variable, y = ROI, fill = Spearman_rho)) +
  geom_tile(color = "white") +
  facet_wrap(~Signal_Type + Stimulus) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "ROI-wise Spearman correlations", fill = "ρ") +
  theme_minimal(base_size = 14)


library(gt)

results %>%
  arrange(p_value) %>%
  filter(p_value < 0.05) %>%
  gt() %>%
  tab_header(title = "Significant ISC-Behavior Correlations (p-value < 0.05)")

# Table for ISC-MW state Correlation
results_mw_state_zima_hbo %>%
  dplyr::select(-Signal_Type, -Stimulus, -Behavioral_Variable, -n) %>%
  dplyr::mutate(
    p_value = round(p_value, 3),
    p_adj = round(p_adj, 3),
    Spearman_rho = round(Spearman_rho, 3)
  ) %>%
  dplyr::arrange(p_value) %>%
  dplyr::rename(
    "Spearman's rho" = "Spearman_rho",
    "uncorrected p" = p_value,
    "corrected p" = p_adj
  ) %>%
  gt() %>%
  tab_header(title = "ISC-MW state Correlations") %>%
  cols_align(
    align = "center",
    columns = c("Spearman's rho","uncorrected p","corrected p")
  )

results_mw_state_splitscreen_hbo %>%
  dplyr::select(-Signal_Type, -Stimulus, -Behavioral_Variable, -n) %>%
  dplyr::mutate(
    p_value = round(p_value, 3),
    p_adj = round(p_adj, 3),
    Spearman_rho = round(Spearman_rho, 3)
  ) %>%
  dplyr::arrange(p_value) %>%
  dplyr::rename(
    "Spearman's rho" = Spearman_rho,
    "uncorrected p" = p_value,
    "corrected p" = p_adj
  ) %>%
  gt() %>%
  tab_header(title = "ISC-MW state Correlations") %>%
  cols_align(
    align = "center",
    columns = c("Spearman's rho","uncorrected p","corrected p")
  )

# Table for ISC-MW trait Correlation
results_mw_trait_zima_hbo %>%
  dplyr::select(-Signal_Type, -Stimulus, -Behavioral_Variable, -n) %>%
  dplyr::mutate(
    p_value = round(p_value, 3),
    p_adj = round(p_adj, 3),
    Spearman_rho = round(Spearman_rho, 3)
  ) %>%
  dplyr::arrange(p_value) %>%
  dplyr::rename(
    "Spearman's rho" = "Spearman_rho",
    "uncorrected p" = p_value,
    "corrected p" = p_adj
  ) %>%
  gt() %>%
  tab_header(title = "ISC-MW trait Correlations") %>%
  cols_align(
    align = "center",
    columns = c("Spearman's rho","uncorrected p","corrected p")
  )

# Table for ISC-Loneliness Correlation
results_loneliness_zima_hbo %>%
  dplyr::select(-Signal_Type, -Stimulus, -Behavioral_Variable, -n) %>%
  dplyr::mutate(
    p_value = round(p_value, 3),
    p_adj = round(p_adj, 3),
    Spearman_rho = round(Spearman_rho, 3)
  ) %>%
  dplyr::arrange(p_value) %>%
  dplyr::rename(
    "Spearman's rho" = Spearman_rho,
    "uncorrected p" = p_value,
    "corrected p" = p_adj
  ) %>%
  gt() %>%
  tab_header(title = "ISC-Loneliness Correlations") %>%
  cols_align(
    align = "center",
    columns = c("Spearman's rho","uncorrected p","corrected p")
  )

results_mw_state_splitscreen_hbo %>%
  dplyr::select(-Signal_Type, -Stimulus, -Behavioral_Variable, -n) %>%
  dplyr::mutate(
    p_value = round(p_value, 3),
    p_adj = round(p_adj, 3),
    Spearman_rho = round(Spearman_rho, 3)
  ) %>%
  dplyr::arrange(p_value) %>%
  dplyr::rename(
    "Spearman's rho" = Spearman_rho,
    "uncorrected p" = p_value,
    "corrected p" = p_adj
  ) %>%
  gt() %>%
  tab_header(title = "ISC-MW state Correlations") %>%
  cols_align(
    align = "center",
    columns = c("Spearman's rho","uncorrected p","corrected p")
  )

merged_df %>%
  filter(channel == "vmPFC", signal_type == "HbO", stimulus == "Zima") %>%
  ggplot(aes(x = Loneliness, y = mean_isc_z)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(title = "vmPFC ISC vs. Loneliness (Zima - HbO)",
       x = "Loneliness Score", y = "Mean ISC (z)") +
  theme_classic(base_size = 14)

merged_df %>%
  filter(channel == "vmPFC", signal_type == "HbO", stimulus == "Zima") %>%
  ggplot(aes(x = MW_state_SART, y = mean_isc_z)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(title = "vmPFC ISC vs. Mind wandering (Zima - HbO)",
       x = "MW_state_SART", y = "Mean ISC (z)") +
  theme_classic(base_size = 14)

library(ggplot2)
library(ggthemes)

merged_df %>%
  filter(channel == "vmPFC", signal_type == "HbO", stimulus == "Zima") %>%
  ggplot(aes(x = MW_state_SART, y = mean_isc_z)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.6, size = 3, color = "#377eb8") +  # prettier points with slight jitter
  geom_smooth(method = "lm", se = TRUE, color = "#e41a1c", size = 1.2) +            # bold regression line
  labs(
    title = "vmPFC ISC vs. Mind Wandering",
    x = "Mind-Wandering State",
    y = "Mean ISC (z-score)"
  ) +
  theme_minimal(base_size = 16) +  # cleaner, modern look
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )



library(ggplot2)
library(ggpubr)

# Filter data
filtered_df <- merged_df %>%
  filter(channel == "vmPFC", signal_type == "HbO", stimulus == "Zima")

# Set bounds for correlation label position
x_pos <- min(filtered_df$MW_state_SART) + 0.1 * diff(range(filtered_df$MW_state_SART))
y_pos <- max(filtered_df$mean_isc_z) - 0.05 * diff(range(filtered_df$mean_isc_z))

# Plot
ggplot(filtered_df, aes(x = MW_state_SART, y = mean_isc_z)) +
  geom_point(color = "#a6cee3", size = 1.8, alpha = 0.7) +  # lighter blue points
  geom_smooth(method = "lm", se = TRUE, fill = "#a6cee3", color = "#1f78b4", size = 1.2) +  # light blue fill, medium blue line
  stat_cor(method = "pearson",
           aes(label = paste(..rr.label.., ..p.label.., sep = ", ")),
           label.x = x_pos, label.y = y_pos,
           size = 4, color = "black") +
  labs(
    title = "Effect of Mind Wandering on ISC (vmPFC)",
    x = "Mind Wandering State (SART)",
    y = "Mean ISC (z-score)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(color = "black", face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )





#============================= Exploratory =====================================#

# regression model

library(lm.beta)

model <- lm(mean_isc_z ~ Loneliness + MW_trait + MW_state_SART, data = filter(merged_df, stimulus == "Zima", signal_type == "HbR"))
model_std <- lm.beta(model)
summary(model_std)
broom::tidy(model_std)

summary(lm(scale(mean_isc_z) ~ scale(Loneliness) + scale(MW_state_SART) + scale(Loneliness)*scale(MW_state_SART), data = filter(merged_df, stimulus == "Zima", signal_type == "HbO")))
summary(lm(mean_isc_z ~ Loneliness + MW_trait + MW_state_SART, data = filter(merged_df, stimulus == "Zima", signal_type == "HbO")))
summary(lm(mean_isc_z ~ Loneliness + MW_trait + MW_state_SART, data = filter(merged_df, stimulus == "Zima", signal_type == "HbR")))

# Regression Loop for ROI × Signal × Stimulus
# Standardize your predictors and outcome
merged_df_std <- merged_df %>%
  mutate(across(c(mean_isc_z, Loneliness, MW_trait), ~ scale(.x)[,1]))  # [,1] drops attributes

# Create all combinations of ROI, signal, and stimulus
roi_signal_stim_combos <- merged_df_std %>%
  distinct(channel, signal_type, stimulus)

# Initialize results data frame
regression_results <- data.frame()

# Loop across combinations
for (i in 1:nrow(roi_signal_stim_combos)) {
  roi <- roi_signal_stim_combos$channel[i]
  signal <- roi_signal_stim_combos$signal_type[i]
  stim <- roi_signal_stim_combos$stimulus[i]
  
  # Subset the data
  df_sub <- merged_df_std %>%
    filter(channel == roi,
           signal_type == signal,
           stimulus == stim)
  
  # Run regression only if there are enough data points
  if (nrow(df_sub) >= 10) {  # adjust threshold as needed
    model <- lm(mean_isc_z ~ Loneliness + MW_trait + MW_state_SART, data = df_sub)
    
    # Tidy and add meta info
    tidy_model <- tidy(model) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        ROI = roi,
        Signal_Type = signal,
        Stimulus = stim,
        n = nrow(df_sub)
      )
    
    regression_results <- bind_rows(regression_results, tidy_model)
  }
}

print(regression_results)
significant_regression_results <- regression_results %>% filter(p.value < 0.05)


# summary table
library(broom)

tidy(model) %>%
  dplyr::select(term, estimate, std.error, statistic, p.value) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

library(gt)

results %>%
  arrange(p_value) %>%
  filter(p_value < 0.05) %>%
  gt() %>%
  tab_header(title = "Significant ISC-Behavior Correlations (p-value < 0.05)")

results %>%
  arrange(p_adj) %>%
  filter(p_adj < 0.05) %>%
  gt() %>%
  tab_header(title = "Significant ISC-Behavior Correlations (FDR < 0.05)")


summary(lm(Loneliness~Accuracy+MW_state_SART+MW_Ratio, data = merged_df))


##### Exploratory
vars_of_interest <- c(
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

results_fdr_all <- results %>%
  filter(Behavioral_Variable %in% vars_of_interest) %>%
  group_by(Stimulus, Signal_Type, Behavioral_Variable) %>%
  mutate(
    p_fdr = p.adjust(p_value, method = "fdr"),
    sig_fdr = p_fdr < 0.05
  ) %>%
  ungroup()

results_fdr_all %>%
  filter(sig_fdr) %>%
  arrange(Behavioral_Variable, Stimulus, Signal_Type, p_fdr)

gt_tables <- lapply(vars_of_interest, function(v) {
  results_fdr_all %>%
    filter(Behavioral_Variable == v) %>%
    select(ROI, Signal_Type, Stimulus, Spearman_rho, p_value, p_fdr, n) %>%
    arrange(p_fdr) %>%
    gt() %>%
    tab_header(title = paste0("ISC – ", v, " Correlations (FDR-corrected)")) %>%
    fmt_number(columns = c(Spearman_rho, p_value, p_fdr), decimals = 3)
})

names(gt_tables) <- vars_of_interest
gt_tables[["MW_state_SART"]]
gt_tables[["MW_trait_total"]]
gt_tables[["MW_Ratio"]]
gt_tables[["Accuracy"]]
gt_tables[["Loneliness"]]
gt_tables[["ASRS_total"]]

library(lme4)
library(lmerTest) 

library(lme4)
library(lmerTest)
library(dplyr)

df_zima_hbo <- merged_df %>%
  filter(stimulus == "Zima", signal_type == "HbO") %>%
  filter(is.finite(mean_isc_z), is.finite(MW_state_SART)) %>%
  mutate(
    subject = factor(subject),
    channel = factor(channel)
  )

model_zima_hbo <- lmer(
  mean_isc_z ~ MW_state_SART + (1 | subject) + (1 | channel),
  data = df_zima_hbo
)

summary(model_zima_hbo)

model_lonely_zima_hbo <- lmer(
  mean_isc_z ~ Loneliness + (1 | subject) + (1 | channel),
  data = df_zima_hbo
)

summary(model_lonely_zima_hbo)


library(lme4)
library(lmerTest)
library(dplyr)

df_zima_hbo <- merged_df %>%
  filter(stimulus == "Zima", signal_type == "HbO") %>%
  filter(is.finite(mean_isc_z), is.finite(MW_state_SART), is.finite(Loneliness)) %>%
  mutate(
    subject = factor(subject),
    channel = factor(channel)
  )

m1 <- lmer(mean_isc_z ~ MW_state_SART + (1 | subject) + (1 | channel), data = df_zima_hbo)
m2 <- lmer(mean_isc_z ~ Loneliness   + (1 | subject) + (1 | channel), data = df_zima_hbo)

# 둘 다 같이 (unique effects)
m3 <- lmer(mean_isc_z ~ MW_state_SART + Loneliness + MW_trait_total +
             (1 | subject) + (1 | channel), data = df_zima_hbo)

summary(m1); summary(m2); summary(m3)
anova(m1, m3)   # MW-only vs MW+Loneliness+Trait
