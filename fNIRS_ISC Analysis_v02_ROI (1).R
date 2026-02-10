library(tidyverse)
library(stringr)
library(readr)
library(dplyr)
library(tidyr)

# Loading data (n = 22, sub2~sub23)
base_dir <- "/Users/saewonchung/Desktop/fNIRS\ pilot\ analysis/Data_preprocessed"

file_paths_z <- list.files(path = base_dir,
                           pattern = "sub-\\d+_task-Video_label-Zima_haemo\\.csv$",
                           recursive = TRUE,
                           full.names = TRUE)

file_paths_sc <- list.files(path = base_dir,
                            pattern = "sub-\\d+_task-Video_label-Splitscreen_haemo\\.csv$",
                            recursive = TRUE,
                            full.names = TRUE)


subject_ids_z <- str_extract(file_paths_z, "(?<=sub-)\\d+")
ordering_z <- order(as.numeric(subject_ids_z))
file_paths_z <- file_paths_z[ordering_z]
subject_ids_z <- subject_ids_z[ordering_z]

subject_ids_sc <- str_extract(file_paths_sc, "(?<=sub-)\\d+")
ordering_sc <- order(as.numeric(subject_ids_sc))
file_paths_sc <- file_paths_sc[ordering_sc]
subject_ids_sc <- subject_ids_sc[ordering_sc]

subject_data_z <- lapply(file_paths_z, function(f) {
  read_csv(f, show_col_types = FALSE) %>%
    dplyr::select(-time)
})

subject_data_sc <- lapply(file_paths_sc, function(f) {
  read_csv(f, show_col_types = FALSE) %>%
    dplyr::select(-time)
})


# ROI mapping
roi_map_hbo <- list(
  vmPFC = c("S1_D1 hbo", "S1_D9 hbo", "S1_D8 hbo", "S2_D9 hbo", "S10_D9 hbo"),
  dmPFC = c("S9_D9 hbo", "S9_D3 hbo", "S9_D11 hbo", "S9_D4 hbo", "S5_D3 hbo", "S5_D4 hbo", "S13_D4 hbo", "S13_D11 hbo"),
  ldlPFC = c("S5_D5 hbo", "S4_D5 hbo", "S4_D3 hbo", "S2_D3 hbo", "S2_D2 hbo", "S3_D2 hbo"),
  rdlPFC = c("S13_D12 hbo", "S12_D12 hbo", "S12_D11 hbo", "S10_11 hbo", "S10_D10 hbo", "S11_D10 hbo"),
  lTPJ = c("S8_D7 hbo", "S8_D6 hbo", "S7_D7 hbo", "S7_D6 hbo"),
  rTPJ = c("S15_D14 hbo", "S15_D13 hbo", "S16_D14 hbo", "S16_D13 hbo")
)

roi_map_hbr <- list(
  vmPFC = c("S1_D1 hbo", "S1_D9 hbo", "S1_D8 hbo", "S2_D9 hbo", "S10_D9 hbo"),
  dmPFC = c("S9_D9 hbo", "S9_D3 hbo", "S9_D11 hbo", "S9_D4 hbo", "S5_D3 hbo", "S5_D4 hbo", "S13_D4 hbo", "S13_D11 hbo"),
  ldlPFC = c("S5_D5 hbo", "S4_D5 hbo", "S4_D3 hbo", "S2_D3 hbo", "S2_D2 hbo", "S3_D2 hbo"),
  rdlPFC = c("S13_D12 hbo", "S12_D12 hbo", "S12_D11 hbo", "S10_11 hbo", "S10_D10 hbo", "S11_D10 hbo"),
  lTPJ = c("S8_D7 hbo", "S8_D6 hbo", "S7_D7 hbo", "S7_D6 hbo"),
  rTPJ = c("S15_D14 hbo", "S15_D13 hbo", "S16_D14 hbo", "S16_D13 hbo")
)

roi_map_hbr <- lapply(roi_map_hbo, function(chs) {
  str_replace_all(chs, "hbo", "hbr")
})

names(subject_data_z) <- subject_ids_z
names(subject_data_sc) <- subject_ids_sc

n_subjects_z <- length(subject_data_z)
channel_names_z <- unique(unlist(lapply(subject_data_z, colnames)))
n_channels_z <- length(channel_names_z)
mean_isc_z <- numeric(n_channels_z)

n_subjects_sc <- length(subject_data_sc)
channel_names_sc <- unique(unlist(lapply(subject_data_sc, colnames)))
n_channels_sc <- length(channel_names_sc)
mean_isc_sc <- numeric(n_channels_sc)

# Trim to shortest length
lengths_check_z <- sapply(subject_data_z, nrow) # length check
print(lengths_check_z)
min_len_z <- min(sapply(subject_data_z, nrow)) # if length different,
subject_data_z <- lapply(subject_data_z, function(df) df[1:min_len_z, ]) # trim to min length

lengths_check_sc <- sapply(subject_data_sc, nrow) # length check
print(lengths_check_sc)
min_len_sc <- min(sapply(subject_data_sc, nrow)) # if length different,
subject_data_sc <- lapply(subject_data_sc, function(df) df[1:min_len_sc, ]) # trim to min length

# Z score within participant within channel
subject_data_z <- lapply(subject_data_z, function(df) {
  df %>% mutate(across(everything(), scale))
}) 

subject_data_sc <- lapply(subject_data_sc, function(df) {
  df %>% mutate(across(everything(), scale))
})

# Average by ROI
average_by_roi <- function(subject_data, signal_type = c("hbo", "hbr")) {
  signal_type <- match.arg(signal_type)
  roi_map <- if (signal_type == "hbo") roi_map_hbo else roi_map_hbr
  
  lapply(subject_data, function(df) {
    df_filtered <- df %>% dplyr::select(matches(signal_type))
    
    roi_data <- map_dfc(roi_map, function(chs) {
      valid_chs <- intersect(chs, colnames(df_filtered))
      if (length(valid_chs) > 0) {
        rowMeans(df_filtered[, valid_chs, drop = FALSE], na.rm = TRUE)
      } else {
        rep(NA, nrow(df_filtered))
      }
    })
    
    colnames(roi_data) <- names(roi_map)
    as_tibble(roi_data)
  })
}

subject_data_z_hbo_roi <- average_by_roi(subject_data_z, "hbo")
subject_data_z_hbr_roi <- average_by_roi(subject_data_z, "hbr")
subject_data_sc_hbo_roi <- average_by_roi(subject_data_sc, "hbo")
subject_data_sc_hbr_roi <- average_by_roi(subject_data_sc, "hbr")


# Fisher Z score 
fisher_z <- function(r) 0.5 * log((1 + r) / (1 - r))
inverse_fisher_z <- function(z) (exp(2 * z) - 1) / (exp(2 * z) + 1)

# Compute Dyad-Level ISC per Channel (with Fisher Z)

channel_names_z_hbo <- unique(unlist(lapply(subject_data_z_hbo_roi, colnames)))
channel_names_z_hbr <- unique(unlist(lapply(subject_data_z_hbr_roi, colnames)))
channel_names_sc_hbo <- unique(unlist(lapply(subject_data_sc_hbo_roi, colnames)))
channel_names_sc_hbr <- unique(unlist(lapply(subject_data_sc_hbr_roi, colnames)))
n_subjects <- length(subject_data_z_hbo_roi)


dyad_isc_list <- list()

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
  
  bind_rows(dyad_isc_list)
}

dyad_isc_z_hbo <- compute_dyad_isc(subject_data_z_hbo_roi, channel_names_z_hbo)
dyad_isc_z_hbr <- compute_dyad_isc(subject_data_z_hbr_roi, channel_names_z_hbr)
dyad_isc_sc_hbo <- compute_dyad_isc(subject_data_sc_hbo_roi, channel_names_sc_hbo)
dyad_isc_sc_hbr <- compute_dyad_isc(subject_data_sc_hbr_roi, channel_names_sc_hbr)

# Individual-Level ISC per Channel
to_individual_isc <- function(dyad_df, label) {
  dyad_df %>%
    pivot_longer(cols = c(sub1, sub2), names_to = "role", values_to = "subject") %>%
    group_by(subject, channel) %>%
    dplyr::summarize(mean_isc_z = mean(isc_z, na.rm = TRUE), .groups = "drop") %>%
    mutate(signal_type = label)
}

roi_isc_z_hbo <- to_individual_isc(dyad_isc_z_hbo, "HbO") %>% mutate(stimulus = "Zima")
roi_isc_z_hbr <- to_individual_isc(dyad_isc_z_hbr, "HbR") %>% mutate(stimulus = "Zima")
roi_isc_sc_hbo <- to_individual_isc(dyad_isc_sc_hbo, "HbO") %>% mutate(stimulus = "Splitscreen")
roi_isc_sc_hbr <- to_individual_isc(dyad_isc_sc_hbr, "HbR") %>% mutate(stimulus = "Splitscreen")

roi_isc <- bind_rows(roi_isc_z_hbo, roi_isc_z_hbr, roi_isc_sc_hbo, roi_isc_sc_hbr)
roi_isc <- roi_isc %>% 
  mutate(subject = as.numeric(subject))

write_csv(roi_isc, "/Users/saewonchung/Desktop/fNIRS pilot analysis/ISC_ROI_level.csv")


# plotting 

ggplot(roi_isc %>% filter(signal_type == "HbO"), aes(x = stimulus, y = mean_isc_z, fill = stimulus)) +
  geom_boxplot() +
  facet_wrap(~channel) +
  labs(title = "ROI-wise ISC by Stimulus (HbO)", y = "Mean ISC (Fisher z)", x = "Stimulus")

