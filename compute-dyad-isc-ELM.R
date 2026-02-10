# Compute ROI-level ISC from ELM Preprocessed Data
# Based on refined analysis combining best practices from reference scripts

library(tidyverse)

# Setup and define parameters ----
base_dir <- "/Users/saewonchung/Desktop/ELM_MW_data_analysis/ELM_preprocessed"

# File patterns for both video types
file_pattern_zima <- "sub-\\d+_ses-.+_task-Video_label-Zima_haemo\\.csv$"
file_pattern_splitscreen <- "sub-\\d+_ses-.+_task-Video_label-Splitscreen_haemo\\.csv$"

# Map ROI to list of channels (fixed typo: S10_11 → S10_D11)
roi_map <- list(
    vmPFC = c("S1_D1", "S1_D9", "S1_D8", "S2_D9", "S10_D9"),
    dmPFC = c("S9_D9", "S9_D3", "S9_D11", "S9_D4", "S5_D3", "S5_D4", "S13_D4", "S13_D11"),
    ldlPFC = c("S5_D5", "S4_D5", "S4_D3", "S2_D3", "S2_D2", "S3_D2"),
    rdlPFC = c("S13_D12", "S12_D12", "S12_D11", "S10_D11", "S10_D10", "S11_D10"),
    lTPJ = c("S8_D7", "S8_D6", "S7_D7", "S7_D6"),
    rTPJ = c("S15_D14", "S15_D13", "S16_D14", "S16_D13")
)

# Load and prepare data for both videos ----
load_video_data <- function(pattern, video_name) {
  file_paths <- list.files(path = base_dir,
                           pattern = pattern,
                           recursive = TRUE,
                           full.names = TRUE)

  subject_ids <- str_extract(file_paths, "(?<=sub-)\\d+")
  ordering <- order(as.numeric(subject_ids))
  file_paths <- file_paths[ordering]
  subject_ids <- subject_ids[ordering]

  subject_data <- lapply(file_paths, function(f) {
    read_csv(f, show_col_types = FALSE) %>% select(-time)
  })

  names(subject_data) <- subject_ids

  cat(sprintf("Loaded %s: %d subjects\n", video_name, length(subject_data)))

  return(subject_data)
}

subject_data_zima <- load_video_data(file_pattern_zima, "Zima")
subject_data_splitscreen <- load_video_data(file_pattern_splitscreen, "Splitscreen")

# Preprocessing function: trim, z-score, and average by ROI ----
preprocess_and_roi <- function(subject_data, video_name) {
  # Trim to shortest length
  lengths_check <- sapply(subject_data, nrow)
  cat(sprintf("%s lengths: min=%d, max=%d\n", video_name, min(lengths_check), max(lengths_check)))
  min_len <- min(lengths_check)
  subject_data <- lapply(subject_data, function(df) df[1:min_len, ])

  # Z score within participant within channel
  subject_data <- lapply(subject_data, function(df) {
    df %>% mutate(across(everything(), scale))
  })

  # Average by ROI
  average_by_roi <- function(subject_data, signal_type = c("hbo", "hbr")) {
    signal_type <- match.arg(signal_type)
    roi_map_hbx <- lapply(roi_map, function(chs) paste(chs, signal_type))

    lapply(subject_data, function(df) {
      df_filtered <- df %>% select(matches(signal_type))

      roi_data <- map_dfc(roi_map_hbx, function(chs) {
        valid_chs <- intersect(chs, colnames(df_filtered))
        if (length(valid_chs) > 0) {
          rowMeans(df_filtered[, valid_chs, drop = FALSE], na.rm = TRUE)
        } else {
          rep(NA, nrow(df_filtered))
        }
      })

      colnames(roi_data) <- names(roi_map_hbx)
      as_tibble(roi_data)
    })
  }

  list(
    hbo = average_by_roi(subject_data, "hbo"),
    hbr = average_by_roi(subject_data, "hbr")
  )
}

zima_roi <- preprocess_and_roi(subject_data_zima, "Zima")
splitscreen_roi <- preprocess_and_roi(subject_data_splitscreen, "Splitscreen")

# Compute Dyad-Level ISC per Channel (with Fisher's z-transformation) ----
fisher_z <- function(r) 0.5 * log((1 + r) / (1 - r), base=exp(1)) # equivalent to atanh(r)
inverse_fisher_z <- function(z) (exp(2 * z) - 1) / (exp(2 * z) + 1) # equivalent to tanh(z)

compute_dyad_isc <- function(subject_data_roi, channel_names) {
  dyad_isc_list <- list()

  for (ch_name in channel_names) {
    ts_all <- lapply(subject_data_roi, function(x) x[[ch_name]])
    available <- which(sapply(ts_all, function(x) is.numeric(x) && length(x) == length(ts_all[[1]])))

    if (length(available) < 2) next

    for (i in 1:(length(available) - 1)) {
      for (j in (i + 1):length(available)) {
        sub1 <- names(subject_data_roi)[available[i]]
        sub2 <- names(subject_data_roi)[available[j]]
        r <- cor(ts_all[[sub1]], ts_all[[sub2]], use = "complete.obs")
        z <- fisher_z(r)

        dyad_isc_list[[length(dyad_isc_list) + 1]] <- tibble(
          channel = ch_name,
          sub1 = sub1,
          sub2 = sub2,
          isc_z = z
        )
      }
    }
  }

  bind_rows(dyad_isc_list)  # Fixed: added return statement
}

# Compute ISC for all combinations
channel_names_zima_hbo <- unique(unlist(lapply(zima_roi$hbo, colnames)))
channel_names_zima_hbr <- unique(unlist(lapply(zima_roi$hbr, colnames)))
channel_names_split_hbo <- unique(unlist(lapply(splitscreen_roi$hbo, colnames)))
channel_names_split_hbr <- unique(unlist(lapply(splitscreen_roi$hbr, colnames)))

cat("\nComputing dyad-level ISC...\n")
dyad_isc_zima_hbo <- compute_dyad_isc(zima_roi$hbo, channel_names_zima_hbo)
dyad_isc_zima_hbr <- compute_dyad_isc(zima_roi$hbr, channel_names_zima_hbr)
dyad_isc_split_hbo <- compute_dyad_isc(splitscreen_roi$hbo, channel_names_split_hbo)
dyad_isc_split_hbr <- compute_dyad_isc(splitscreen_roi$hbr, channel_names_split_hbr)

# Save Dyad-Level ISC to CSV ----
# For dyad-level analysis following Baek et al. (2023) Psychological Science
dyad_isc_all <- bind_rows(
  dyad_isc_zima_hbo %>% mutate(signal_type = "HbO", stimulus = "Zima"),
  dyad_isc_zima_hbr %>% mutate(signal_type = "HbR", stimulus = "Zima"),
  dyad_isc_split_hbo %>% mutate(signal_type = "HbO", stimulus = "Splitscreen"),
  dyad_isc_split_hbr %>% mutate(signal_type = "HbR", stimulus = "Splitscreen")
)

dyad_output_file <- file.path(dirname(base_dir), "ISC_dyad_level_ELM.csv")
write_csv(dyad_isc_all, dyad_output_file)
cat(sprintf("Dyad-level ISC saved to: %s\n", dyad_output_file))
cat(sprintf("Total dyad observations: %d\n", nrow(dyad_isc_all)))

# Compute Individual-Level ISC per Channel ----
to_individual_isc <- function(dyad_df, signal_type, stimulus) {
  dyad_df %>%
    pivot_longer(cols = c(sub1, sub2), names_to = "role", values_to = "subject") %>%
    group_by(subject, channel) %>%
    dplyr::summarize(mean_isc_z = mean(isc_z, na.rm = TRUE), .groups = "drop") %>%
    mutate(signal_type = signal_type, stimulus = stimulus)
}

cat("\nComputing individual-level ISC...\n")
roi_isc_zima_hbo <- to_individual_isc(dyad_isc_zima_hbo, "HbO", "Zima")
roi_isc_zima_hbr <- to_individual_isc(dyad_isc_zima_hbr, "HbR", "Zima")
roi_isc_split_hbo <- to_individual_isc(dyad_isc_split_hbo, "HbO", "Splitscreen")
roi_isc_split_hbr <- to_individual_isc(dyad_isc_split_hbr, "HbR", "Splitscreen")

# Save results to CSV ----
roi_isc <- bind_rows(roi_isc_zima_hbo, roi_isc_zima_hbr,
                     roi_isc_split_hbo, roi_isc_split_hbr) %>%
  mutate(subject = as.numeric(subject))

output_file <- file.path(dirname(base_dir), "ISC_ROI_level_ELM.csv")
write_csv(roi_isc, output_file)

cat(sprintf("\n✅ ISC analysis complete!\n"))
cat(sprintf("Output saved to: %s\n", output_file))
cat(sprintf("Total rows: %d\n", nrow(roi_isc)))
cat(sprintf("Subjects: %d\n", length(unique(roi_isc$subject))))
cat(sprintf("ROIs: %s\n", paste(unique(roi_isc$channel), collapse=", ")))

# Visualization (optional) ----
# Plot HbO ISC by stimulus and ROI
p1 <- ggplot(roi_isc %>% filter(signal_type == "HbO"),
       aes(x = stimulus, y = mean_isc_z, fill = stimulus)) +
  geom_boxplot() +
  facet_wrap(~channel) +
  labs(title = "ROI-wise ISC by Stimulus (HbO)",
       y = "Mean ISC (Fisher z)",
       x = "Stimulus") +
  theme_minimal()

print(p1)

# Plot HbR ISC by stimulus and ROI
p2 <- ggplot(roi_isc %>% filter(signal_type == "HbR"),
       aes(x = stimulus, y = mean_isc_z, fill = stimulus)) +
  geom_boxplot() +
  facet_wrap(~channel) +
  labs(title = "ROI-wise ISC by Stimulus (HbR)",
       y = "Mean ISC (Fisher z)",
       x = "Stimulus") +
  theme_minimal()

print(p2)

cat("\nDone! You can now view the plots and check the output CSV.\n")
