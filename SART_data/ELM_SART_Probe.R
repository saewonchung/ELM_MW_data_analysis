library(tidyverse)
library(dplyr)
library(stringr)

directory_path <- "/Users/saewonchung/Desktop/ELM_MW_data_analysis/SART_data"
file_list <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)

# SART Performance Accuracy 
SART_perf_accuracy <- data.frame(File = character(), Accuracy = numeric(), stringsAsFactors = FALSE)

for (file in file_list) {
  data <- read_csv(file, show_col_types = FALSE)

  # Remove practice trials (first 20 rows: blank row + 19 practice trials)
  if (nrow(data) > 20) {
    data <- data %>% slice(-(1:20))
  }

  if (all(c("key_resp.corr", "trialType") %in% names(data))) {
    filtered <- data %>% filter(trialType != "Probe 1")
    
    correct <- sum(filtered$key_resp.corr == 1, na.rm = TRUE)
    total <- sum(!is.na(filtered$key_resp.corr))
    
    acc <- if (total > 0) round(correct / total * 100, 2) else NA
    
    SART_perf_accuracy <- rbind(SART_perf_accuracy, data.frame(
      File = basename(file),
      Accuracy = acc
    ))
  }
}

# Probe results
SART_probe_results <- data.frame(File = character(), Right_Count = integer(), Total_Valid = integer(), Right_Ratio = numeric(), stringsAsFactors = FALSE)

for (file in file_list) {
  data <- read_csv(file, show_col_types = FALSE)

  # Remove practice trials (first 20 rows: blank row + 19 practice trials)
  if (nrow(data) > 20) {
    data <- data %>% slice(-(1:20))
  }

  if (all(c("trialType", "probe1_resp.keys") %in% names(data))) {
    probe_data <- data %>%
      filter(trialType == "Probe 1") %>%
      filter(probe1_resp.keys %in% c("left", "right"))
    
    right_count <- sum(probe_data$probe1_resp.keys == "right", na.rm = TRUE)
    total_valid <- nrow(probe_data)
    right_ratio <- if (total_valid > 0) round(right_count / total_valid, 3) else NA
    
    SART_probe_results <- rbind(SART_probe_results, data.frame(
      File = basename(file),
      Right_Count = right_count,
      Total_Valid = total_valid,
      MW_Ratio = right_ratio
    ))
  }
}

SART_combined_results <- left_join(SART_perf_accuracy, SART_probe_results, by = "File")

SART_combined_results_clean <- SART_combined_results %>%
  # mutate(File = str_extract(File, "^\\d+")) %>%
  # dplyr::select(-Right_Count, -Total_Valid) %>%
  rename(pid = File) %>%
  # Remove rows with Accuracy below 70
  filter(Accuracy >= 70 | is.na(Accuracy))
  
write_csv(SART_combined_results_clean, "/Users/saewonchung/Desktop/ELM_MW_data_analysis/SART_data/SART_results.csv")
