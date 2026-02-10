# Prepare Survey 2 Data: Long to Wide Format + Merge with Trait Data
# This script converts video-specific survey data from long format (2 rows per subject)
# to wide format (1 row per subject) and merges with existing trait data.

library(tidyverse)

# Setup paths
base_dir <- "/Users/saewonchung/Desktop/ELM_MW_data_analysis"

# 1. Load Survey 2 (long format - 2 rows per subject)
survey2 <- read_csv(file.path(base_dir, "Qualtrics_data/Qualtrics_Survey_2_raw_data_250109.csv"),
                    show_col_types = FALSE)

cat(sprintf("Survey 2 loaded: %d rows, %d subjects\n",
            nrow(survey2), n_distinct(survey2$pid)))

# Check column names (Qualtrics export has duplicate names)
cat("Column names in Survey 2:\n")
print(names(survey2))

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

# 4. Load trait data
trait_data <- read_csv(file.path(base_dir, "Qualtrics_data/Qualtrics_raw_data_cleaned.csv"),
                       show_col_types = FALSE) %>%
  mutate(pid = as.numeric(pid))  # Convert pid to numeric (removes "38b" -> NA, "51" -> 51)

# Check for any NA pids after conversion (like "38b")
na_pids <- trait_data %>% filter(is.na(pid))
if (nrow(na_pids) > 0) {
  cat(sprintf("Note: %d subjects with non-numeric pid removed (e.g., '38b')\n", nrow(na_pids)))
  trait_data <- trait_data %>% filter(!is.na(pid))
}

cat(sprintf("Trait data loaded: %d subjects\n", nrow(trait_data)))

# 5. Merge by pid
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

# 6. Save merged data
output_file <- file.path(base_dir, "Qualtrics_data/Qualtrics_all_merged.csv")
write_csv(merged, output_file)

cat(sprintf("\nDone! Merged data saved to: %s\n", output_file))
cat(sprintf("Total columns: %d\n", ncol(merged)))
cat(sprintf("New video-specific columns added: %d\n", ncol(survey2_wide) - 1))
