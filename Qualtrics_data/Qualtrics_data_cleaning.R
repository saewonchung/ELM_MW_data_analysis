# ============================================================================
# Qualtrics Data Cleaning and Merging Pipeline
# ============================================================================
# This script combines Survey 4 (individual measures) and Survey 2 (video-specific data)
#
# Input files:
#   - Qualtrics_raw_data_full_250105.xlsx (Survey 4 - individual measures)
#   - Qualtrics_Survey_2_raw_data_250109.csv (Survey 2 - video responses)
#
# Output files:
#   - Qualtrics_raw_data_cleaned.csv (cleaned Survey 4 data)
#   - Qualtrics_all_merged.csv (final merged data)
# ============================================================================

library(dplyr)
library(readr)
library(readxl)
library(tidyverse)
library(lubridate)
library(corrplot)
library(Hmisc)
library(broom)

# Setup paths
base_dir <- "/Users/saewonchung/Desktop/ELM_MW_data_analysis"

cat("============================================================\n")
cat("PART 1: Survey 4 (Individual Measures) Processing\n")
cat("============================================================\n\n")

# ============================================================================
# PART 1: SURVEY 4 PROCESSING (Individual Measures)
# ============================================================================

# 0. Download data from Qualtrics and roughly clean data from excel
Qualtrics_df <- read_excel(file.path(base_dir, "Qualtrics_data/Qualtrics_raw_data_full_250105.xlsx"), # N = 114
  sheet = "raw_data_cleaned") # removed data from test runs, blank rows, duplicates, cleaning unnecessary columns

cat(sprintf("Survey 1 loaded: %d subjects\n", nrow(Qualtrics_df)))

# 1. Additional data cleaning in R
Qualtrics_df %>%
  select(4:ncol(Qualtrics_df)) %>%
  select(where(~ any(is.na(.x)))) # check any additional missing values

missing_pids <- Qualtrics_df %>%
  filter(if_any(4:ncol(Qualtrics_df), is.na)) %>%
  pull(pid) # check pid with missing values

if (length(missing_pids) > 0) {
  cat("PIDs with missing values:", paste(missing_pids, collapse = ", "), "\n")
}

# 14; missing Q3_1
# 21; missing Bigfive_9
# 41; no missing values
# 76; missing UCLA_Loneliness_4

names(Qualtrics_df)
colnames(Qualtrics_df)[3] <- "MW_free_response"


# 2. Creating variables (recode, sum/average)

# Loneliness
Qualtrics_df <- Qualtrics_df %>%
  mutate(across(starts_with("UCLA Loneliness_"), as.numeric)) %>%
  rowwise() %>%
  mutate(Loneliness = sum(c_across(starts_with("UCLA Loneliness_")), na.rm = TRUE)) %>%
  ungroup()

# Autism
AQ10_agree_items <- paste0("AQ10_", c(1, 7, 8, 10)) # which items are "agree = 1"
AQ10_disagree_items <- paste0("AQ10_", c(2, 3, 4, 5, 6, 9)) # which items are "disagree = 1"

Qualtrics_df <- Qualtrics_df %>%
  mutate(
    # Items where Agree (1,2) = 1 point
    across(
      all_of(AQ10_agree_items),
      ~ ifelse(. %in% c(1, 2), 1,
               ifelse(. %in% c(3, 4), 0, NA_real_)
      ),
      .names = "{.col}_score"
    ),
    # Items where Disagree (3,4) = 1 point
    across(
      all_of(AQ10_disagree_items),
      ~ ifelse(. %in% c(3, 4), 1,
               ifelse(. %in% c(1, 2), 0, NA_real_)
      ),
      .names = "{.col}_score"
    )
  ) %>%
  rowwise() %>%
  mutate(
    AQ10_scored = sum(c_across(matches("^AQ10_\\d+_score$")), na.rm = FALSE)
  ) %>%
  ungroup()

# Depression (PHQ-9)
Qualtrics_df <- Qualtrics_df %>%
  rowwise() %>%
  mutate(
    PHQ9_total = sum(as.numeric(unlist(c_across(starts_with("PHQ9-minus9_")))), na.rm = TRUE)
  ) %>%
  ungroup()

# Anxiety (Generalized Anxiety Disorder; GAD)
Qualtrics_df <- Qualtrics_df %>% # total score can be further categorized into 4 levels of anxiety
  mutate(across(starts_with("GAD-1_"), ~ as.numeric(.))) %>%
  rowwise() %>%
  mutate(
    GAD_total = sum(as.numeric(unlist(c_across(starts_with("GAD-1_")))), na.rm = TRUE)
  ) %>%
  ungroup()

# Social Interaction Anxiety (SIAS)
Qualtrics_df <- Qualtrics_df %>%
  mutate(across(starts_with("SIAS_"), as.numeric)) %>%
  mutate(
    SIAS_5  = 4 - SIAS_5,
    SIAS_9  = 4 - SIAS_9,
    SIAS_11 = 4 - SIAS_11
  ) %>%
  rowwise() %>%
  mutate(
    SIAS_total = sum(c_across(starts_with("SIAS_")), na.rm = TRUE)
  ) %>%
  ungroup()

# Bigfive
Qualtrics_df <- Qualtrics_df %>%
  mutate(across(starts_with("Bigfive_"), ~ as.numeric(.))) %>%
  mutate(
  Bigfive_1 = 6 - Bigfive_1,
  Bigfive_3 = 6 - Bigfive_3,
  Bigfive_4 = 6 - Bigfive_4,
  Bigfive_5 = 6 - Bigfive_5,
  Bigfive_7 = 6 - Bigfive_7
  ) %>%
  rowwise() %>%
  mutate(
    Extraversion      = mean(c(Bigfive_1,  Bigfive_6),  na.rm = TRUE),
    Agreeableness     = mean(c(Bigfive_2,  Bigfive_7),  na.rm = TRUE),
    Conscientiousness = mean(c(Bigfive_3,  Bigfive_8),  na.rm = TRUE),
    Neuroticism       = mean(c(Bigfive_4,  Bigfive_9),  na.rm = TRUE),
    Openness          = mean(c(Bigfive_5,  Bigfive_10), na.rm = TRUE)
  ) %>%
  ungroup()


# Well-being (WHO-5)
Qualtrics_df <- Qualtrics_df %>%
  mutate(across(starts_with("WHO-5_"), ~ as.numeric(.))) %>%
  rowwise() %>%
  mutate(
    WHO_5_total = sum(as.numeric(unlist(c_across(starts_with("WHO-5_")))), na.rm = TRUE)
  ) %>%
  ungroup()

# ADHD (ASRS)
Qualtrics_df <- Qualtrics_df %>%
  mutate(across(starts_with("ASRS_"), ~ as.numeric(.))) %>%
  mutate(across(starts_with("ASRS_"), ~ .x - 1)) %>% # scoring should be 0-4 instead of 1-5
  rowwise() %>%
  mutate(
    ASRS_total = sum(as.numeric(unlist(c_across(starts_with("ASRS_")))), na.rm = TRUE)
  ) %>%
  ungroup()

# Trait Mindwandering
Qualtrics_df <- Qualtrics_df %>%
  mutate(across(starts_with("MW trait_"), ~ as.numeric(.))) %>%
  rowwise() %>%
  mutate(
    MW_trait_total = sum(as.numeric(unlist(c_across(starts_with("MW trait_")))), na.rm = TRUE)
  ) %>%
  ungroup()

# State Mindwandering
Qualtrics_df <- Qualtrics_df %>%
  rename(MW_state_SART = `MW state (SART)`) %>%
  mutate(MW_state_SART = as.numeric(MW_state_SART))

# Perceived Idiosyncrasies
Qualtrics_df <- Qualtrics_df %>%
  mutate(across(starts_with("Q"), ~ as.numeric(.)))

# 3. Export Survey 1 cleaned data (INTERMEDIATE OUTPUT)
output_file1 <- file.path(base_dir, "Qualtrics_data/Qualtrics_raw_data_cleaned.csv")
write_csv(Qualtrics_df, output_file1)
cat(sprintf("\n✓ Survey 1 cleaned data saved: %s\n", output_file1))
cat(sprintf("  Total subjects: %d, Total columns: %d\n\n", nrow(Qualtrics_df), ncol(Qualtrics_df)))


# ============================================================================
# PART 2: SURVEY 2 PROCESSING (Video-Specific Data)
# ============================================================================

cat("============================================================\n")
cat("PART 2: Survey 2 (Video-Specific Data) Processing\n")
cat("============================================================\n\n")

# 1. Load Survey 2 (long format - 2 rows per subject)
survey2 <- read_csv(file.path(base_dir, "Qualtrics_data/Qualtrics_Survey_2_raw_data_250109.csv"),
                    show_col_types = FALSE)

cat(sprintf("Survey 2 loaded: %d rows, %d subjects\n",
            nrow(survey2), n_distinct(survey2$pid)))

# 2. Clean vid column and select relevant columns
# Note: Duplicate column names get auto-renamed:
#   - First Q2, Q3 -> drop (not needed)
#   - Q6 -> Mindwandering
#   - Q4 -> Interest
#   - Second Q3 (Q3...33) -> Familiarity
survey2_clean <- survey2 %>%
  mutate(
    # Convert pid to numeric (removes leading zeros like "051" -> 51)
    pid = as.numeric(pid),
    video = case_when(
      str_detect(vid, "Zima") ~ "Zima",
      str_detect(vid, "Splitscreen") ~ "Splitscreen",
      TRUE ~ NA_character_
    )
  ) %>%
  select(pid, video,
         Mindwandering = Q6,
         MW_free_response = `?MW free response`,
         Enjoy_1, Enjoy_2, Enjoy_3,
         Appreciate_1, Appreciate_2, Appreciate_3,
         Interest = Q4,
         Familiarity = `Q3...33`)

# Check for any missing video assignments
if (any(is.na(survey2_clean$video))) {
  warning("Some rows have unrecognized video types!")
  print(survey2 %>% filter(!str_detect(vid, "Zima|Splitscreen")) %>% select(pid, vid))
}

cat(sprintf("Cleaned data: %d rows\n", nrow(survey2_clean)))

# 3. Pivot to wide format (1 row per subject)
# Note: Some subjects have duplicate responses per video:
#   - pid 10 (Splitscreen): 1st NA, 2nd valid → uses valid value
#   - pid 38 (Zima): both valid, same Mindwandering (1), different Interest (4 vs 3) → mean = 3.5
#   - pid 87 (Splitscreen): 1st valid, 2nd NA → uses valid value
#   - pid 91 (Splitscreen): 1st valid, 2nd NA → uses valid value
#   - pid 101 (Splitscreen): both valid, same Mindwandering (2), different Interest (4 vs 3) → mean = 3.5
# We use values_fn to handle duplicates: mean for numeric (ignores NA), first for character
survey2_wide <- survey2_clean %>%
  pivot_wider(
    id_cols = pid,
    names_from = video,
    values_from = c(Mindwandering, MW_free_response,
                    Enjoy_1, Enjoy_2, Enjoy_3,
                    Appreciate_1, Appreciate_2, Appreciate_3,
                    Interest, Familiarity),
    names_glue = "{.value}_{video}",
    values_fn = list(
      Mindwandering = mean,
      MW_free_response = first,
      Enjoy_1 = mean, Enjoy_2 = mean, Enjoy_3 = mean,
      Appreciate_1 = mean, Appreciate_2 = mean, Appreciate_3 = mean,
      Interest = mean,
      Familiarity = mean
    )
  )

cat(sprintf("Wide format: %d subjects\n", nrow(survey2_wide)))


# ============================================================================
# PART 3: MERGE SURVEY 1 AND SURVEY 2
# ============================================================================

cat("\n============================================================\n")
cat("PART 3: Merging Survey 1 and Survey 2\n")
cat("============================================================\n\n")

# Load trait data (already cleaned in Part 1)
trait_data <- Qualtrics_df %>%
  mutate(pid = as.numeric(pid))  # Convert pid to numeric (removes "38b" -> NA, "51" -> 51)

# Check for any NA pids after conversion (like "38b")
na_pids <- trait_data %>% filter(is.na(pid))
if (nrow(na_pids) > 0) {
  cat(sprintf("Note: %d subjects with non-numeric pid removed (e.g., '38b')\n", nrow(na_pids)))
  trait_data <- trait_data %>% filter(!is.na(pid))
}

cat(sprintf("Trait data: %d subjects\n", nrow(trait_data)))

# Merge by pid
merged <- trait_data %>%
  left_join(survey2_wide, by = "pid")

cat(sprintf("Merged data: %d subjects\n", nrow(merged)))

# Check for any unmatched subjects
unmatched_trait <- trait_data %>%
  anti_join(survey2_wide, by = "pid")
unmatched_survey2 <- survey2_wide %>%
  anti_join(trait_data, by = "pid")

if (nrow(unmatched_trait) > 0) {
  cat(sprintf("Warning: %d subjects in trait data not found in Survey 2\n",
              nrow(unmatched_trait)))
  print(unmatched_trait$pid)
}

if (nrow(unmatched_survey2) > 0) {
  cat(sprintf("Warning: %d subjects in Survey 2 not found in trait data\n",
              nrow(unmatched_survey2)))
  print(unmatched_survey2$pid)
}

# Save merged data (FINAL OUTPUT)
output_file2 <- file.path(base_dir, "Qualtrics_data/Qualtrics_all_merged.csv")
write_csv(merged, output_file2)

cat(sprintf("\n✓ Merged data saved: %s\n", output_file2))
cat(sprintf("  Total columns: %d\n", ncol(merged)))
cat(sprintf("  New video-specific columns added: %d\n", ncol(survey2_wide) - 1))

cat("\n============================================================\n")
cat("PROCESSING COMPLETE!\n")
cat("============================================================\n")
cat("Output files:\n")
cat(sprintf("  1. %s\n", output_file1))
cat(sprintf("  2. %s\n", output_file2))
cat("============================================================\n")
