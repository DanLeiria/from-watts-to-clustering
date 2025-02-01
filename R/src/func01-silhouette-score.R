### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HEADER ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Author:       Daniel Leiria
# Copyright     Copyright 2025 - Daniel Leiria
# Email:        daniel.h.leiria@gmail.com

# Date:         2025-01-19

# Script name:  func01-silhouette-score.R
# R version:    4.4.1
 
# Script Description:
# Function to assess and plot the silhouette scores
# Applying cvi methods into matrix, in order to find optimal number of clusters
# In this work, I selected to use the silhouette score method
# Plot and save the results

silh_score <- function(matrix_input,
                       clusters_interval,
                       seed_value,
                       figure_name) {
  
  ### Inputs:
  #           matrix_input: Dataset to be clustered as a matrix
  #           clusters_interval: Interval to test the silhouette score
  #           seed_value: Seed number for replication
  #           figure_name: Name to save the final plot
  #
  ### Output:
  #           ggplot_silh: Silhouette score plot
  
  set.seed(seed_value)
  
  # Test optimal number of clusters
  pc_k <- tsclust(matrix_input,
                  seed = seed_value,
                  k = clusters_interval,
                  distance = "sbd",
                  centroid = "shape")
  
  # Rename list names according to clusters
  names(pc_k) <- paste0("k_", clusters_interval)
  
  # Merge list into matrix
  cvi_results <- as.data.frame(sapply(pc_k, cvi, type = "internal"))
  
  # Convert rownames to a column before using pivot_longer
  cvi_results$cvi_method <- rownames(cvi_results)
  
  # Apply pivot_longer
  # Create column with a numeric value of the number of clusters
  # Extract only silhouette scores
  silh_results <- pivot_longer(cvi_results,
                              cols = starts_with("k"),
                              names_to = "k",
                              values_to = "value") %>%
    mutate(nr_cluster = as.numeric(gsub("k_", "", k))) %>%
    filter(cvi_method == "Sil")
  
  # Create best silhouette score dataframe
  # Get the maximum silhouette score (sil_max)
  # Get the associated number of clusters for the best silh. score
  segment_df <- data.frame(
    nr_cluster_max = silh_results$nr_cluster[which.max(silh_results$value)],
    sil_max = max(silh_results$value))
  
  # Create silhouette score plot
  ggplot_silh <- ggplot(silh_results, aes(x = nr_cluster, y = value)) +
    geom_point(color = "royalblue3", size = 3) +
    geom_line(color = "royalblue3", linewidth = 1) +
    theme_classic() +
    labs(x = "Number of clusters",
         y = "Silhouette score") +
    scale_x_continuous(breaks = silh_results$nr_cluster) +
    ylim(0, segment_df$sil_max + 0.06) +
    geom_segment(data = segment_df, aes(x = nr_cluster_max,
                                        xend = nr_cluster_max,
                                        y = -Inf,
                                        yend = sil_max),
                 color = "red",
                 linewidth = 1,
                 linetype = "dashed") +
    annotate("text", x = segment_df$nr_cluster_max, y = segment_df$sil_max, label = paste("Score:", round(segment_df$sil_max, digits = 3)),
             vjust = -1.2, hjust = 0.2, color = "red") +
    geom_point(data = segment_df, aes(x = nr_cluster_max, y = sil_max), color = "red", size = 4)
  
  ggsave(filename = paste0("figures/", figure_name), plot = ggplot_silh, width = 8, height = 6, dpi = 300)
  
  return(ggplot_silh)
  
}
