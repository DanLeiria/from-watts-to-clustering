### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HEADER ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Author:       Daniel Leiria
# Copyright     Copyright 2025 - Daniel Leiria
# Email:        daniel.h.leiria@gmail.com

# Date:         2025-01-19

# Script name:  02-kshape-clustering.R

# Script Description:
# Apply k-Shape clustering of the monthly time series measurements
# Store the clusters group associated per geocode in the folder data/03-output-clusters


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SOURCE ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Call functions
source("R/src/func01-silhouette-score.R")
source("R/src/func02-kshape.R")

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
# In this work, I applied the silhouette score
# Plot and save the results

ggplot_silh_initial <- silh_score(matrix_input = matrix_elec,
                                  clusters_interval = clusters_interval,
                                  seed_value = seed_value,
                                  figure_name = "silh-score-initial.png")

ggplot_silh_initial


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# APPLY INITIAL K-SHAPE ALGORITHM IN THE DATA ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description:
# Apply k-Shape clustering technique using 4 clusters.
# Initial clustering (before outlier filtering).

# Optimal number of clusters
clust_initial <- 4L

# Initial k-Shape results
ks_initial_results <- kshape_clustering(matrix_elec,
                                        nr_clusters = clust_initial,
                                        seed_value = seed_value,
                                        df_postcodes = unique(df_electricity$postcodes),
                                        figure_name = "initial-clusters.png")

# SBD calculated
df_sbd <- ks_initial_results$sbd_values

# k-Shape: initial results
df_ks_initial_comb <- ks_initial_results$kshape_results

# Plot
ks_initial_results$kshape_plot


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FILTER OUTLIERS BASED ON SBD DISTANCE ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description:
# Filter time-series where their SBD with the centroid is
# higher than a specific limit.

sbd_limit_calikus <- mean(df_sbd$sbd_distance) + beta_val_calikus * sd(df_sbd$sbd_distance)

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

ggsave(filename = "figures/sbd-filtering.png", plot = ggplot_sbd, width = 8, height = 6, dpi = 300)


df_ks_condition %>%
  count(postcodes, sbd_condition) %>%
  count(sbd_condition) %>%
  mutate(prcent = 100 * n / 471)



### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PREPARE DATAFRAME FOR THE CLUSTERING ALGORITHM ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description:
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


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OUTLIERS DATASET ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description: 
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

ggsave(filename = "figures/ks-outlier.png", plot = ggplot_ks_outlier, width = 8, height = 6, dpi = 300)



### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SILHOUETTE PLOTS - FILTERED DATA ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description: 
# Applying cvi methods into matrix, in order to find optimal number of clusters.
# In this work, I selected to use inertia and silhouette methods.
# Plot and save the results.

ggplot_silh_filter <- silh_score(matrix_input = matrix_elec_2,
                                 clusters_interval = clusters_interval,
                                 seed_value = seed_value,
                                 figure_name = "silh-score-filter.png")

ggplot_silh_filter


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# APPLY K-SHAPE ALGORITHM IN THE FILTERED DATA ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description:
# Apply k-Shape clustering technique using 4 clusters.
# Initial clustering (before outlier filtering).

# Optimal number of clusters
clust_filter <- 4L

# Initial k-Shape results
ks_filter_results <- kshape_clustering(matrix_elec_2,
                                        nr_clusters = clust_filter,
                                        seed_value = seed_value,
                                        df_postcodes = unique(df_ks_condition$postcodes[df_ks_condition$sbd_condition == "Normal"]),
                                        figure_name = "filtered-clusters.png")

# SBD calculated
df_sbd <- ks_filter_results$sbd_values

# k-Shape: initial results
df_ks_filter_comb <- ks_filter_results$kshape_results

# Plot
ks_filter_results$kshape_plot

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SAVE FINAL DATASET ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description: 
# Save dataset with the clustering results.


df_outlier <- df_ks_condition %>%
  filter(sbd_condition == "Outlier") %>%
  select(-sbd_distance)

df_final <- df_ks_filter_comb %>%
  mutate(sbd_condition = "Normal") %>%
  rbind(df_outlier) %>%
  arrange(postcodes, month)


write.csv(df_final, "data/03-output-clusters/electricity-kshape-clustered.csv", row.names = FALSE)