### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HEADER ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Author:       Daniel Leiria
# Copyright     Copyright 2025 - Daniel Leiria
# Email:        daniel.h.leiria@gmail.com

# Date:         2025-01-19

# Script name:  func02-kshape.R

# Script Description:
# Apply the clustering methodology into a specific matrix

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

kshape_clustering <- function(matrix_input,
                              nr_clusters,
                              seed_value,
                              df_postcodes,
                              figure_name) {
  
  ### Inputs:
  #           matrix_input: Dataset to be clustered as a matrix
  #           nr_clusters: Number of selected clusters
  #           seed_value: Seed number for replication
  #           df_postcodes:
  #           figure_name: Name to save the final plot
  #
  ### Output:
  #           sbd_values: SBD distance (df_sbd)
  #           kshape_results: Clustering results (df_ks_cluster_comb)
  #           kshape_plot: Clustering plot (ggplot_ks_cluster)


# To see total number of postcodes to be clustered
print(paste("Total number of postcodes to be initially clustered:", nrow(matrix_elec)))

# Apply k-Shape
ks_cluster <- tsclust(matrix_input,
                      seed = seed_value,
                      k = nr_clusters,
                      type = "p",
                      distance = "sbd",
                      centroid = "shape")

# Plot results (just for a quick visualization)
plot(ks_cluster, type = "series")
plot(ks_cluster, type = "centroids")


# Extract clustering results
# Make it as a dataframe
df_ks_cluster <- data.frame(data = ks_cluster@datalist) %>%
  mutate(month = row_number()) %>% # Add time_months column
  pivot_longer(
    cols = starts_with("data"), # Pivot columns
    names_to = "postcodes",
    values_to = "energy_standardized"
  ) %>%
  mutate(postcodes = as.numeric(str_extract(postcodes, "\\d+"))) %>%
  arrange(postcodes, month) %>%
  mutate(centroid = rep(ks_cluster@cluster, each = 12))

# Extract sbd distance between postcode time series (measurements) and its cluster centroid
df_sbd <- data.frame(
  sbd_distance = ks_cluster@cldist,
  postcodes = df_postcodes
)


# Extract centroids
# Make it as a good dataframe to be used
df_ks_centroids <- data.frame(centroid = ks_cluster@centroids) %>%
  mutate(month = row_number()) %>%
  pivot_longer(
    cols = 1:nr_clusters,
    names_to = "centroids",
    values_to = "energy_centroid"
  ) %>%
  mutate(centroid = rep(1:nr_clusters, length(unique(df_ks_cluster$month)))) %>%
  select(-centroids)


# Combine data with centroids information
df_ks_cluster_comb <- df_ks_cluster %>% left_join(df_ks_centroids)

# Plot of the main clustering results
ggplot_ks_cluster <- ggplot(df_ks_cluster_comb, aes(x = month, y = energy_standardized, group = postcodes)) +
  geom_line(color = "black", alpha = 0.3, linewidth = 1) +
  geom_line(data = df_ks_cluster_comb, aes(x = month, y = energy_centroid), color = "red", linewidth = 3) +
  facet_wrap(. ~ centroid) +
  theme_bw() +
  labs(
    y = "Standardized energy [-]",
    x = "Month"
  ) +
  scale_x_continuous(breaks = 1:12, limits = c(1, 12))

ggsave(filename = paste0("figures/", figure_name), plot = ggplot_ks_cluster, width = 8, height = 6, dpi = 300)

return(list(sbd_values = df_sbd,
            kshape_results = df_ks_cluster_comb,
            kshape_plot = ggplot_ks_cluster))

}
