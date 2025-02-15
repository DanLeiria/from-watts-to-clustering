### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HEADER ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Author:       Daniel Leiria
# Copyright     Copyright 2025 - Daniel Leiria
# Email:        daniel.h.leiria@gmail.com

# Date:         2025-01-26

# Script name:  03-analysis-and-visualization.R

# Script Description:
# Plot and analysis of the clustering results
# while comparing with Portugal characteristics


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD DATA ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description:
# Load the different files with their specifications.


### Electricity dataset

df_elect <- read.csv("data/02-preprocessed/electricity-preprocessed.csv")

### Clustering dataset

df_clust <- read.csv("data/03-output-clusters/electricity-kshape-clustered.csv") %>%
  select(-energy_standardized)

df_clust <- df_clust %>%
  mutate(
    cluster_name = ifelse(
      centroid == 1, "Winter peak (1)",
      ifelse(
        centroid == 2, "Summer peak (2)",
        ifelse(
          centroid == 3, "Summer break (3)",
          "Winter/Summer peaks (4)"
        )
      )
    )
  )

df_clust$cluster_name[df_clust$sbd_condition == "Outlier"] <- "Outlier (5)"
df_clust$centroid[df_clust$sbd_condition == "Outlier"] <- 5


desired_order <- c(
  "Winter peak (1)",
  "Summer peak (2)",
  "Summer break (3)",
  "Winter/Summer peaks (4)",
  "Outlier (5)"
)

df_clust$cluster_name <- factor(df_clust$cluster_name, levels = desired_order)


### Portugal characteristics dataset

df_postcodes <- read.csv("data/02-preprocessed/postcodes-preprocessed.csv")

df_county_consumption_normalized <- read.csv("data/02-preprocessed/county-consumption-normalized-preprocessed.csv")

df_county_purchasing_power <- read.csv("data/02-preprocessed/county-purchasing-power-preprocessed.csv")

df_county_population <- read.csv("data/02-preprocessed/county-population-preprocessed.csv")


df_portugal_info <- df_postcodes %>%
  left_join(df_county_population) %>%
  left_join(df_county_purchasing_power) %>% 
  left_join(df_county_consumption_normalized)


# Combine smart electricity meters data with the postcodes dataset
df_electricity <- df_elect %>%
  inner_join(df_portugal_info, by = "postcodes") %>%
  inner_join(df_clust, by = c("postcodes", "month"))

### GeoJSON file - Portugal
# District
geojson_data <- st_read("data/01-input/portugal-with-regions.geojson")
# County
geojson_county <- st_read("data/01-input/portugal-with-counties.geojson")


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT MAPPING OF PORTUGAL POPULATION DENSITY ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description: 
# Bubble plot of Portugal population density

# Data: world.cities {maps}
data_pop <- world.cities %>% filter(country.etc == "Portugal")

# Legend of population numbers
my_breaks <- c(5000, 25000, 50000, 100000, 250000, 500000)

# Define custom labels
# Add comma of the population numbers
custom_labels <- function(x) {
  format(x, scientific = FALSE, big.mark = ",")
}

ggplot_population_distribution <- ggplot() +
  geom_sf(data = geojson_data, fill = "grey", alpha = 0.2) +
  geom_point(
    data = data_pop,
    aes(x = long, y = lat, size = pop, fill = pop),
    shape = 21,
    alpha = 0.6
  ) +
  scale_size_continuous(
    range = c(0.7, 15),
    breaks = my_breaks,
    labels = custom_labels
  ) +
  scale_fill_continuous(
    type = "viridis",
    trans = "log",
    breaks = my_breaks,
    labels = custom_labels,
    direction = -1
  ) +
  xlim(-10.5, -6) +
  ylim(36.7, 42) +
  labs(
    x = "Longitude",
    y = "Latitude",
    size = "Population \n(log):",
    fill = "Population \n(log):",
    caption = "Dataset source: https://search.r-project.org/CRAN/refmans/maps/html/world.cities.html"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.background = element_rect(colour = "black")
  ) +
  guides(fill = guide_legend(), size = guide_legend()) +
  ### Lisbon (capital)
  annotate("text",
           x = c(-9.95), y = c(38.72),
           label = c("Lisboa"), color = "black",
           size = 4, hjust = 0.5, fontface = "bold"
  ) +
  ### Porto
  annotate("text",
           x = c(-9.25), y = c(41.15),
           label = c("Porto"), color = "black",
           size = 4, hjust = 0.5, fontface = "bold"
  ) +
  ### Faro
  annotate("text",
           x = c(-7.94), y = c(36.85),
           label = c("Faro"), color = "black",
           size = 4, hjust = 0.5, fontface = "bold"
  )

ggplot_population_distribution

ggsave(filename = "figures/population-distribution.png", plot = ggplot_population_distribution, width = 8, height = 6, dpi = 300)



### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT CLUSTERS FREQUENCY ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description: 
# Frequency of the clusters of the counties

df_clust_grouped <- df_clust %>%
  group_by(postcodes) %>%
  reframe(centroid = mean(centroid),
          sbd_condition = unique(sbd_condition),
          cluster_name = unique(cluster_name))

ggplot_cluster_freq <- ggplot(df_clust_grouped,
                              aes(x = as.factor(centroid),
                                  fill = cluster_name)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  scale_fill_viridis(discrete = TRUE) +
  xlab("Cluster group") +
  ylab("Frequency") +
  labs(fill = "Type of pattern:")

ggplot_cluster_freq

ggsave(filename = "figures/cluster-freq.png", plot = ggplot_cluster_freq, width = 12, height = 10, dpi = 300)


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT MAPPING TOTAL ENERGY USAGE ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description: 
# Total consumption per district


df_total_energy <- df_electricity %>%
  group_by(district) %>%
  summarise(total_energy_kwh = sum(energy_kwh))

sf_district_energy <- geojson_data %>%
  left_join(df_total_energy, by = c("name" = "district"))

plt_total_energy_map <- ggplot(sf_district_energy) +
  geom_sf(aes(fill = (total_energy_kwh / 1000000000))) +
  xlim(-10.5, -6) +
  ylim(36.7, 42) +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Yearly energy \nusage [TW]:") +
  theme_classic() +
  scale_fill_distiller(palette = "RdBu", direction = -1) +
  theme(legend.position = "bottom",
        legend.background = element_rect(colour = "black")) +
  ggrepel::geom_label_repel(data = sf_district_energy,
                            aes(label = name,
                                geometry = geometry),
                            stat = "sf_coordinates",
                            min.segment.length = 0)


plt_total_energy_map

plt_total_energy_district <- sf_district_energy %>%
  mutate(name = fct_reorder(name, total_energy_kwh)) %>%
  ggplot(aes(x = name,
             y = total_energy_kwh / 1000000000,
             fill = total_energy_kwh / 1000000000)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic() +
  scale_fill_distiller(palette = "RdBu",
                       direction = -1) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_line(color = "grey", linewidth = 0.8, linetype = "dashed"), # Adding grid lines only for the y-axis
    panel.grid.major.y = element_blank(), # Removing grid lines for the x-axis
    panel.grid.minor.x = element_line(color = "grey", linewidth = 0.2, linetype = "dashed"),
    panel.grid.minor.y = element_blank() # Removing minor grid lines for the y-axis
  ) +
  labs(
    x = "District",
    y = "Yearly energy usage [TW]"
  )

plt_total_energy_district

combined_ggplot_total_energy <- grid.arrange(plt_total_energy_district, plt_total_energy_map, ncol = 2)

ggsave(filename = "figures/combined-total-energy.png", plot = combined_ggplot_total_energy, width = 8, height = 6, dpi = 300)



### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT CORRELATION BETWEEN POWER PURCHASE AND ENERGY USAGE (DISTRICT) ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


my_breaks <- c(150000, 675000, 1200000, 1725000, 2250000)


df_district_info <- df_portugal_info %>%
  dplyr::select(-postcodes) %>% 
  distinct() %>% 
  group_by(district) %>%
  reframe(population_numbers = sum(population_numbers),
          purchasing_power_per_capita = mean(purchasing_power_per_capita))

population_total <- sum(df_district_info$population_numbers)

df_data <- df_electricity %>%
  group_by(district) %>%
  reframe(total_energy_kwh = sum(energy_kwh)) %>%
  left_join(df_district_info)

ggplot_corr_energy_purchase <- ggplot(df_data,
                                      aes(x = log10(purchasing_power_per_capita),
                                          y = log10(total_energy_kwh / population_total))) +
  geom_point(data = df_data,
             aes(fill = total_energy_kwh / 1000000000,
                 size = population_numbers),
             shape = 21) +
  geom_smooth(method = "gam",
              se = FALSE,
              color = "lightblue4",
              linewidth = 1.5) +
  geom_label_repel(aes(label = district),
                   size = 3.5,
                   max.overlaps = nrow(df_data)) +
  scale_size_continuous(range = c(0.7, 15),
                        breaks = my_breaks,
                        labels = custom_labels) +
  theme_classic() +
  scale_fill_distiller(palette = "RdBu", direction = -1) +
  theme(legend.position = "right",
        legend.background = element_rect(colour = "black"),
        panel.grid.major.x = element_line(color = "grey", linewidth = 0.2), # Adding grid lines only for the y-axis
        panel.grid.major.y = element_line(color = "grey", linewidth = 0.2), # Adding grid lines only for the y-axis
        panel.grid.minor.x = element_line(color = "grey", linewidth = 0.2, linetype = "dashed"),
        panel.grid.minor.y = element_line(color = "grey", linewidth = 0.2, linetype = "dashed")) +
  labs(x = "Total purchasing power per capita (log)",
       y = "Total energy usage in kWh per capita (log)",
       fill = "Yearly energy \nusage [TW]:",
       size = "District \npopulation:")

ggplot_corr_energy_purchase

ggsave(filename = "figures/corr-energy-purchase.png", plot = ggplot_corr_energy_purchase, width = 8, height = 6, dpi = 300)



### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT CORRELATION BETWEEN POWER PURCHASE AND ENERGY USAGE PER POPULATION DISTRICT ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

my_breaks <- c(150000, 675000, 1200000, 1725000, 2250000)

df_district_info <- df_portugal_info %>%
  dplyr::select(-postcodes) %>% 
  distinct() %>% 
  group_by(district) %>%
  summarise(population_numbers = sum(population_numbers),
            purchasing_power_per_capita = mean(purchasing_power_per_capita))

df_data <- df_electricity %>%
  group_by(district) %>%
  summarise(total_energy_kwh = sum(energy_kwh)) %>%
  left_join(df_district_info)

ggplot_corr_district_energy_purchase <- ggplot(df_data,
                                               aes(x = log10(purchasing_power_per_capita),
                                                   y = log10(total_energy_kwh / population_numbers))) +
  geom_point(data = df_data,
             aes(fill = total_energy_kwh / 1000000000,
                 size = population_numbers),
             shape = 21) +
  geom_smooth(method = "gam", se = FALSE, color = "lightblue4", linewidth = 1.5) +
  geom_label_repel(aes(label = district),
                   size = 3.5,
                   max.overlaps = nrow(df_data)) +
  scale_size_continuous(range = c(0.7, 15),
                        breaks = my_breaks,
                        labels = custom_labels) +
  theme_classic() +
  scale_fill_distiller(palette = "RdBu", direction = -1) +
  theme(legend.position = "right",
        legend.background = element_rect(colour = "black"),
        panel.grid.major.x = element_line(color = "grey", linewidth = 0.2), # Adding grid lines only for the y-axis
        panel.grid.major.y = element_line(color = "grey", linewidth = 0.2), # Adding grid lines only for the y-axis
        panel.grid.minor.x = element_line(color = "grey", linewidth = 0.2, linetype = "dashed"),
        panel.grid.minor.y = element_line(color = "grey", linewidth = 0.2, linetype = "dashed")) +
  labs(x = "Total purchasing power per capita (log)",
       y = "Total energy usage in kWh per capita of the district (log)",
       fill = "Yearly energy \nusage [TW]:",
       size = "District \npopulation:")

ggplot_corr_district_energy_purchase

ggsave(filename = "figures/corr-district-energy-purchase.png", plot = ggplot_corr_district_energy_purchase, width = 8, height = 6, dpi = 300)



### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT ENERGY USAGE PER COUNTY ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

my_breaks <- c(150000, 675000, 1200000, 1725000, 2250000)

df_county_info <- df_portugal_info %>%
  dplyr::select(-postcodes) %>% 
  distinct() %>% 
  group_by(district, county) %>%
  reframe(population_numbers = sum(population_numbers),
          purchasing_power_per_capita = mean(purchasing_power_per_capita))

df_data <- df_electricity %>%
  group_by(district, county) %>%
  reframe(total_energy_kwh = sum(energy_kwh),
          centroid = trunc(median(centroid))) %>%
  left_join(df_county_info)


plt_energy_per_county <- ggplot(df_data,
                                aes(x = district,
                                    y = log10(total_energy_kwh/population_numbers))) +
  geom_point(size = 2) +
  geom_hline(yintercept = 3.3,
             color = "red",
             linetype = "dashed") +
  geom_hline(yintercept = 4,
             color = "red",
             linetype = "dashed") +
  geom_label_repel(data = df_data %>%
                     filter(log10(total_energy_kwh/population_numbers) >= 4 |
                              log10(total_energy_kwh/population_numbers) <= 3.3),
                   aes(label = county),
                   size = 3.5,
                   max.overlaps = nrow(df_data)) +
  theme_classic() +
  theme(legend.position = "right",
        legend.background = element_rect(colour = "black"),
        panel.grid.major.x = element_line(color = "grey", linewidth = 0.2), # Adding grid lines only for the y-axis
        panel.grid.major.y = element_line(color = "grey", linewidth = 0.2), # Adding grid lines only for the y-axis
        panel.grid.minor.x = element_line(color = "grey", linewidth = 0.2, linetype = "dashed"),
        panel.grid.minor.y = element_line(color = "grey", linewidth = 0.2, linetype = "dashed")) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  labs(x = "District",
       y = "Total energy usage in kWh per capita of the county (log)")

plt_energy_per_county

ggsave(filename = "figures/energy_per_county.png",
       plot = plt_energy_per_county,
       width = 8,
       height = 6,
       dpi = 300)


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT MAPPING ENERGY CONSUMPTION PER END-USER ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_non_residential_energy <- df_portugal_info %>%
  group_by(county) %>%
  summarise(mean_domestic = mean(domestic),
            mean_not_domestic = mean(not_domestic),
            mean_agriculture = mean(agriculture),
            mean_industry = mean(industry))

geojson_county$Concelho <- stri_trans_general(stri_trans_tolower(geojson_county$Concelho), "Latin-ASCII")
df_non_residential_energy$county <- stri_trans_general(stri_trans_tolower(df_non_residential_energy$county), "Latin-ASCII")

my_sf_merged <- geojson_county %>%
  left_join(df_non_residential_energy, by = c("Concelho" = "county"))

ggplot_domestic_energy_map <- ggplot(my_sf_merged) +
  geom_sf(aes(fill = round(mean_domestic * 100, digits = 1))) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Domestic",
       fill = "Pct [%]:") +
  theme_void() +
  scale_fill_distiller(palette = "RdBu", direction = -1) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  ggrepel::geom_label_repel(data = my_sf_merged %>%
                              arrange(desc(mean_domestic)) %>%  # Sort in descending order
                              slice_head(n = 5),  # Select the top 5 rows
                            aes(label = Concelho,
                                geometry = geometry),
                            stat = "sf_coordinates",
                            min.segment.length = 0)

ggplot_domestic_energy_map

ggplot_not_domestic_energy_map <- ggplot(my_sf_merged) +
  geom_sf(aes(fill = round(mean_not_domestic * 100, digits = 1))) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Not domestic",
       fill = "Pct [%]:") +
  theme_void() +
  scale_fill_distiller(palette = "RdBu", direction = -1) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  ggrepel::geom_label_repel(data = my_sf_merged %>%
                              arrange(desc(mean_not_domestic)) %>%  # Sort in descending order
                              slice_head(n = 5),  # Select the top 5 rows
                            aes(label = Concelho,
                                geometry = geometry),
                            stat = "sf_coordinates",
                            min.segment.length = 0)

ggplot_not_domestic_energy_map

ggplot_industry_energy_map <- ggplot(my_sf_merged) +
  geom_sf(aes(fill = round(mean_industry * 100, digits = 1))) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Industry",
       fill = "Pct [%]:") +
  theme_void() +
  scale_fill_distiller(palette = "RdBu", direction = -1) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  ggrepel::geom_label_repel(data = my_sf_merged %>%
                              arrange(desc(mean_industry)) %>%  # Sort in descending order
                              slice_head(n = 5),  # Select the top 5 rows
                            aes(label = Concelho,
                                geometry = geometry),
                            stat = "sf_coordinates",
                            min.segment.length = 0)

ggplot_industry_energy_map

ggplot_agriculture_energy_map <- ggplot(my_sf_merged) +
  geom_sf(aes(fill = round(mean_agriculture * 100, digits = 1))) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Agriculture",
       fill = "Pct [%]:") +
  theme_void() +
  scale_fill_distiller(palette = "RdBu", direction = -1) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  ggrepel::geom_label_repel(data = my_sf_merged %>%
                              arrange(desc(mean_agriculture)) %>%  # Sort in descending order
                              slice_head(n = 5),  # Select the top 5 rows
                            aes(label = Concelho,
                                geometry = geometry),
                            stat = "sf_coordinates",
                            min.segment.length = 0)

ggplot_agriculture_energy_map

combined_ggplot_shares_energy <- grid.arrange(ggplot_domestic_energy_map,
                                              ggplot_not_domestic_energy_map,
                                              ggplot_industry_energy_map,
                                              ggplot_agriculture_energy_map,
                                              ncol = 4)
combined_ggplot_shares_energy

ggsave(filename = "figures/combined-shares-energy.png", plot = combined_ggplot_shares_energy, width = 8, height = 6, dpi = 300)





### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT CLUSTERS MAPPING ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_cluster_county <- df_electricity %>%
  group_by(county) %>%
  reframe(total_energy_kwh = sum(energy_kwh),
          centroid = trunc(median(centroid)))

df_cluster_county$county <- stri_trans_general(stri_trans_tolower(df_cluster_county$county), "Latin-ASCII")

my_sf_merged <- geojson_county %>%
  left_join(df_cluster_county, by = c("Concelho" = "county"))



plt_cluster1_map <- ggplot(my_sf_merged) +
  geom_sf(aes(fill = centroid == 1),
          color = "#696969") +
  scale_fill_manual(values = c("TRUE" = "#440154", "FALSE" = "#DCDCDC")) +
  labs(title = "Cluster 1") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

plt_cluster1_map


plt_cluster2_map <- ggplot(my_sf_merged) +
  geom_sf(aes(fill = centroid == 2), 
          color = "#696969") +
  scale_fill_manual(values = c("TRUE" = "#3b528b", "FALSE" = "#DCDCDC")) +
  labs(title = "Cluster 2") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

plt_cluster2_map


plt_cluster3_map <- ggplot(my_sf_merged) +
  geom_sf(aes(fill = centroid == 3), 
          color = "#696969") +
  scale_fill_manual(values = c("TRUE" = "#21918c", "FALSE" = "#DCDCDC")) +
  labs(title = "Cluster 3") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

plt_cluster3_map


plt_cluster4_map <- ggplot(my_sf_merged) +
  geom_sf(aes(fill = centroid == 4),
          color = "#696969") +
  scale_fill_manual(values = c("TRUE" = "#5ec962", "FALSE" = "#DCDCDC")) +
  labs(title = "Cluster 4") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

plt_cluster4_map


plt_cluster5_map <- ggplot(my_sf_merged) +
  geom_sf(aes(fill = centroid == 5),
          color = "#696969") +
  scale_fill_manual(values = c("TRUE" = "#fde725", "FALSE" = "#DCDCDC")) +
  labs(title = "Cluster 5") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

plt_cluster5_map


combined_clusters_map <- grid.arrange(plt_cluster1_map,
                                      plt_cluster2_map,
                                      plt_cluster3_map,
                                      plt_cluster4_map,
                                      plt_cluster5_map,
                                      ncol = 5)
combined_clusters_map

ggsave(filename = "figures/combined_clusters_map.png", plot = combined_clusters_map, width = 8, height = 6, dpi = 300)



### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT CLUSTERS PERCENTAGE PER DISTRICT ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_cluster_district <- df_electricity %>%
  group_by(district, cluster_name) %>%
  reframe(n_counties = n()/12)

# Compute percentage per district
df_cluster_district <- df_cluster_district %>%
  group_by(district) %>%
  mutate(percent = (n_counties / sum(n_counties)) * 100)

# Create the plot
plt_cluster_district <- df_cluster_district %>%
  ggplot(aes(fill = cluster_name, y = percent, x = district)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  coord_flip() +
  labs(x = "District",
       y = "Percentage [%]",
       fill = "Cluster:") +
  # facet_wrap(. ~ cluster_name, ncol = 1) +
  scale_y_continuous(labels = percent_format(scale = 1)) # Ensures y-axis is formatted as percentage

plt_cluster_district

ggsave(filename = "figures/plt_cluster_district.png", plot = plt_cluster_district, width = 8, height = 6, dpi = 300)





### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CLUSTERS AND THERE REASONS ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


df_cluster_type <- df_electricity %>% 
  group_by(postcodes) %>% 
  reframe(centroid = factor(mean(centroid), levels = c(1,2,3,4,5)),
          population_numbers = mean(population_numbers),
          agriculture = mean(agriculture),
          industry = mean(industry),
          not_domestic = mean(not_domestic),
          domestic = mean(domestic)) %>%
  # Remove columns you do not want to reshape (e.g., postcodes, population_numbers)
  select(-postcodes) %>%
  # Convert from wide to long
  pivot_longer(cols = c(population_numbers,
                        agriculture, 
                        industry,
                        not_domestic,
                        domestic),
               names_to = "sector",
               values_to = "value")

df_cluster_type$sector[df_cluster_type$sector == "population_numbers"] <- "Population"
df_cluster_type$sector[df_cluster_type$sector == "agriculture"] <- "Agriculture"
df_cluster_type$sector[df_cluster_type$sector == "industry"] <- "Industry"
df_cluster_type$sector[df_cluster_type$sector == "not_domestic"] <- "Not domestic"
df_cluster_type$sector[df_cluster_type$sector == "domestic"] <- "Domestic"


plt_cluster_end_user <- ggplot(df_cluster_type, aes(x = value, y = centroid, fill = centroid)) +
  geom_violin() +
  geom_jitter(size = 0.9, height = 0.1) +
  stat_summary(fun = median,
               geom = "point",
               color = "red",
               size = 3,
               shape = 16,
               position = position_dodge(width = 0.9)) +
  scale_fill_viridis_d() +
  facet_wrap(~ sector, scales = "free_x", ncol = 2) +
  labs(x = "Values",
       y = "Cluster") +
  theme_bw() +
  theme(legend.position = "none")

ggsave(filename = "figures/plt_cluster_end_user.png", plot = plt_cluster_end_user, width = 8, height = 6, dpi = 300)


