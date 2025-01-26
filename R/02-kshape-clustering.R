### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HEADER ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Author:       Daniel Leiria
# Copyright     Copyright 2025 - Daniel Leiria
# Email:        daniel.h.leiria@gmail.com

# Date:         2025-01-19

# Script name:  02-kshape-clustering.R
# R version:    4.4.1
 
# Script Description:
# Apply k-Shape clustering of the monthly time series measurements
# Store the clusters group associated per geocode in the folder data/03-output-clusters


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SETUP ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Description: 
# Clears the console
# Remove all variables of the work space
cat("\014")
rm(list = ls())

# Call library and settings
source("R/00-library.R")
source("R/00-settings.R")

# Call functions
source("R/func01-silhouette-score.R")

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD PREPROCESSED DATA ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description:
# Load the different files with their specifications

df_electricity <- read.csv("data/02-preprocessed/electricity-preprocessed.csv")


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PREPARE DATAFRAME FOR THE CLUSTERING ALGORITHM ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description: 
# The dataframe must be converted into a matrix where the rownames is the postcodes
# The postcodes cannot be repeated
# The columns must be the month numbers (1-12)
# The values inside the matrix must be z-score standardized energy measurements

matrix_elec <- reshape2::dcast(df_electricity, # Convert the dataframe into a matrix
                               postcodes ~ month,
                               value.var = "energy_standardized")

# Set row names and remove the postcodes column
rownames(matrix_elec) <- matrix_elec$postcodes
matrix_elec$postcodes <- NULL


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SILHOUETTE PLOTS ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description: 
# Applying cvi methods into matrix, in order to find optimal number of clusters.
# In this work, I selected to use silhouette score
# Plot and save the results

ggplot_silh_initial <- silh_score(matrix_input = matrix_elec,
                                  clusters_interval = clusters_interval,
                                  seed_value = seed_value,
                                  figure_name = "ggplot_silh_initial.png")

ggplot_silh_initial

### ============================================================================
# APPLY INITIAL K-SHAPE ALGORITHM IN THE DATA ----
### ============================================================================

# Description:
# Apply k-Shape clustering technique using 4 clusters.
# Initial clustering (before outlier filtering).

ks_dataset_initial <- nrow(matrix_elec)
print(paste("Total number of buildings initially clustered:", ks_dataset_initial))

# Optimal number of clusters
clust_initial <- 4L

### Apply k-Shape
ks_initial <- tsclust(matrix_elec,
                      seed = seed_value,
                      k = clust_initial,
                      type = "p",
                      distance = "sbd",
                      centroid = "shape"
)

plot(ks_initial, type = "series")
plot(ks_initial, type = "centroids")


### Extract data
df_ks_initial <- data.frame(data = ks_initial@datalist) %>%
  mutate(month = row_number()) %>% # Add time_months column
  pivot_longer(
    cols = starts_with("data"), # Pivot columns
    names_to = "postcodes",
    values_to = "energy_standardized"
  ) %>%
  mutate(postcodes = as.numeric(str_extract(postcodes, "\\d+"))) %>%
  arrange(postcodes, month) %>%
  mutate(centroid = rep(ks_initial@cluster, each = 12))


df_sbd <- data.frame(
  sbd_distance = ks_initial@cldist,
  postcodes = unique(df_electricity$postcodes)
)


### Extract centroids
df_ks_centroids <- data.frame(centroid = ks_initial@centroids) %>%
  mutate(month = row_number()) %>%
  pivot_longer(
    cols = 1:clust_initial,
    names_to = "centroids",
    values_to = "energy_centroid"
  ) %>%
  mutate(centroid = rep(1:clust_initial, length(unique(df_ks_initial$month)))) %>%
  select(-centroids)


### Combine data with centroids information
df_ks_initial_comb <- df_ks_initial %>% left_join(df_ks_centroids)

ggplot_ks_initial <- ggplot(df_ks_initial_comb, aes(x = month, y = energy_standardized, group = postcodes)) +
  geom_line(color = "black", alpha = 0.3, linewidth = 1) +
  geom_line(data = df_ks_initial_comb, aes(x = month, y = energy_centroid), color = "red", linewidth = 3) +
  facet_wrap(. ~ centroid) +
  theme_bw() +
  labs(
    y = "Standardized energy [-]",
    x = "Month"
  ) +
  scale_x_continuous(breaks = 1:12, limits = c(1, 12))

ggplot_ks_initial

ggsave(filename = "figures/ggplot_ks_initial.png", plot = ggplot_ks_initial, width = 8, height = 6, dpi = 300)



### ============================================================================
# FILTER OUTLIERS BASED ON SBD DISTANCE ----
### ============================================================================

# Description:
# Filter time-series where their SBD with the centroid is
# higher than a specific limit.

beta_val_calikus <- 3
sbd_limit_calikus <- mean(df_sbd$sbd_distance) + beta_val_calikus * sd(df_sbd$sbd_distance)
sbd_limit_used <- 0.15


df_ks_condition <- df_ks_initial_comb %>%
  left_join(df_sbd) %>%
  mutate(sbd_condition = ifelse(sbd_distance < sbd_limit_used,
                                "Normal",
                                "Outlier"))

lines_df <- data.frame(
  sbd_distance = c(sbd_limit_calikus, sbd_limit_used),
  color_label = c("Calikus et al. (2019)", "Used"))

ggplot_sbd <- ggplot(
  df_ks_condition %>%
    group_by(postcodes) %>%
    summarise(sbd_distance = mean(sbd_distance)),
  aes(x = sbd_distance)
) +
  geom_histogram(
    fill = "lightblue",
    color = "royalblue4"
  ) +
  geom_vline(
    data = lines_df,
    aes(xintercept = sbd_distance, color = color_label),
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  scale_color_manual(
    name = "Threshold:",
    values = c("Calikus et al. (2019)" = "black", "Used" = "red")
  ) +
  theme_bw() +
  labs(
    x = "Shape-based distance (SBD)",
    y = "Number of cases"
  ) +
  theme(
    legend.position.inside = c(0.865, 0.865),
    legend.background = element_rect(
      colour = "black",
      fill = "white"
    )
  )

ggplot_sbd

ggsave(filename = "figures/ggplot_sbd.png", plot = ggplot_sbd, width = 8, height = 6, dpi = 300)


df_ks_condition %>%
  count(postcodes, sbd_condition) %>%
  count(sbd_condition) %>%
  mutate(prcent = 100 * n / 471)




### ============================================================================
# PREPARE DATAFRAME FOR THE CLUSTERING ALGORITHM ----
### ============================================================================

# Description:
# The dataframe must be converted into a matrix where the rownames is poscodes.
# The columns must be the month numbers (1-12).
# The values inside the matrix must z-score standardized energy measurements.

matrix_elec_2 <- reshape2::dcast(df_ks_condition %>%
                                   filter(sbd_condition == "Normal"), # Convert the dataframe into a matrix.
                                 postcodes ~ month,
                                 value.var = "energy_standardized"
)

rownames(matrix_elec_2) <- matrix_elec_2$postcodes # Set row names and remove the postcodes column.

matrix_elec_2$postcodes <- NULL # Remove postcodes column.


### ============================================================================
# OUTLIERS DATASET ----
### ============================================================================

# Description:
# Plot the time-series identified as outliers.

ggplot_ks_outlier <- ggplot(
  data = df_ks_condition %>% filter(sbd_condition == "Outlier"),
  aes(
    x = month,
    y = energy_standardized,
    group = postcodes
  )
) +
  geom_line(color = "black", alpha = 0.3, linewidth = 1) +
  geom_line(data = df_ks_condition %>% filter(sbd_condition == "Outlier"), aes(x = month, y = energy_centroid), color = "red", linewidth = 3) +
  facet_wrap(. ~ centroid) +
  theme_bw() +
  labs(
    y = "Standardized energy [-]",
    x = "Month"
  ) +
  scale_x_continuous(breaks = 1:12, limits = c(1, 12))


ggplot_ks_outlier

ggsave(filename = "figures/ggplot_ks_outlier.png", plot = ggplot_ks_outlier, width = 8, height = 6, dpi = 300)




### ============================================================================
# SILHOUETTE PLOTS - FILTERED DATA ----
### ============================================================================

# Description:
# Applying cvi methods into matrix, in order to find optimal number of clusters.
# In this work, I selected to use inertia and silhouette methods.
# Plot and save the results.

seed_value <- 314
set.seed(seed_value)

pc_k <- tsclust(matrix_elec_2,
                seed = seed_value,
                k = 2L:20L, # Test optimal number of clusters.
                distance = "sbd",
                centroid = "shape"
)

names(pc_k) <- paste0("k_", 2L:20L) # Rename list names.

cvi_results <- as.data.frame(sapply(pc_k, cvi, type = "internal")) # Merge list into matrix.

cvi_results$cvi_method <- rownames(cvi_results) # Convert rownames to a column before using pivot_longer.


sil_results <- pivot_longer(cvi_results,
                            cols = starts_with("k"),
                            names_to = "k",
                            values_to = "value"
) %>%
  mutate(nr_cluster = as.numeric(gsub("k_", "", k))) %>%
  filter(cvi_method == "Sil")


### Plot silhouette plot
segment_df <- data.frame(
  nr_cluster_max = sil_results$nr_cluster[which.max(sil_results$value)],
  sil_max = max(sil_results$value)
)


ggplot_silh_final <- ggplot(sil_results %>% filter(cvi_method == "Sil"), aes(x = nr_cluster, y = value)) +
  geom_point(color = "royalblue3", size = 3) +
  geom_line(color = "royalblue3", size = 1) +
  theme_classic() +
  labs(
    x = "Number of clusters",
    y = "Silhouette score"
  ) +
  scale_x_continuous(breaks = sil_results$nr_cluster) +
  ylim(0, segment_df$sil_max + 0.06) +
  geom_segment(
    data = segment_df, aes(
      x = nr_cluster_max,
      xend = nr_cluster_max,
      y = -Inf,
      yend = sil_max
    ),
    color = "red",
    size = 1,
    linetype = "dashed"
  ) +
  annotate("text",
           x = segment_df$nr_cluster_max, y = segment_df$sil_max, label = paste("Score:", round(segment_df$sil_max, digits = 3)),
           vjust = -1.2, hjust = 0.2, color = "red"
  ) +
  geom_point(data = segment_df, aes(x = nr_cluster_max, y = sil_max), color = "red", size = 4)

ggplot_silh_final

ggsave(filename = "figures/ggplot_silh_final.png", plot = ggplot_silh_final, width = 8, height = 6, dpi = 300)




### ============================================================================
# APPLY K-SHAPE ALGORITHM IN THE FILTERED DATA ----
### ============================================================================

# Description:
# Apply k-Shape clustering technique using 4 clusters.
# Initial clustering (before outlier filtering).

ks_dataset_filter <- nrow(matrix_elec_2)
print(paste("Total number of buildings clustered:", ks_dataset_filter))
clust_filter <- 4L # Optimal number of clusters

### Apply k-Shape

ks_filter <- tsclust(matrix_elec_2,
                     # seed = seed_value,
                     k = clust_filter,
                     type = "p",
                     distance = "sbd",
                     centroid = "shape"
)

plot(ks_filter, type = "series")

plot(ks_filter, type = "centroids")


### Extract data

df_ks_filter <- data.frame(data = ks_filter@datalist) %>%
  mutate(month = row_number()) %>% # Add time_months column
  pivot_longer(
    cols = starts_with("data"), # Pivot columns
    names_to = "postcodes",
    values_to = "energy_standardized"
  ) %>%
  mutate(postcodes = as.numeric(str_extract(postcodes, "\\d+"))) %>%
  arrange(postcodes, month) %>%
  mutate(centroid = rep(ks_filter@cluster, each = 12))


df_sbd <- data.frame(
  sbd_distance = ks_filter@cldist,
  postcodes = unique(df_ks_condition$postcodes[df_ks_condition$sbd_condition == "Normal"])
)

### Extract centroids


df_ks_centroids <- data.frame(centroid = ks_filter@centroids) %>%
  mutate(month = row_number()) %>%
  pivot_longer(
    cols = 1:clust_filter,
    names_to = "centroids",
    values_to = "energy_centroid"
  ) %>%
  mutate(centroid = rep(1:clust_filter, length(unique(df_ks_filter$month)))) %>%
  select(-centroids)


### Combine data with centroids information

df_ks_filter_comb <- df_ks_filter %>% left_join(df_ks_centroids)

ggplot_ks_filter <- ggplot(df_ks_filter_comb, aes(x = month, y = energy_standardized, group = postcodes)) +
  geom_line(color = "black", alpha = 0.3, linewidth = 1) +
  geom_line(data = df_ks_filter_comb, aes(x = month, y = energy_centroid), color = "red", linewidth = 3) +
  facet_wrap(. ~ centroid) +
  theme_bw() +
  labs(
    y = "Standardized energy [-]",
    x = "Month"
  ) +
  scale_x_continuous(breaks = 1:12, limits = c(1, 12))

ggplot_ks_filter

ggsave(filename = "figures/ggplot_ks_filter.png", plot = ggplot_ks_filter, width = 8, height = 6, dpi = 300)



### ============================================================================
# SAVE FINAL DATASET ----
### ============================================================================

# Description:
# Save dataset with the clustering results.


df_outlier <- df_ks_condition %>%
  filter(sbd_condition == "Outlier") %>%
  select(-sbd_distance)

df_final <- df_ks_filter_comb %>%
  mutate(sbd_condition = "Normal") %>%
  rbind(df_outlier) %>%
  arrange(postcodes, month)


write.csv(df_final, "data/03-output-clusters/electricity-kshape-clustered.csv", row.names = FALSE)