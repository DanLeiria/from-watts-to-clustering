### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HEADER ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Author:       Daniel Leiria
# Copyright     Copyright 2025 - Daniel Leiria
# Email:        daniel.h.leiria@gmail.com

# Date:         2025-01-26

# Script name:  03-analysis-and-visualization.R
# R version:    4.4.1
 
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
      centroid == 1, "Summer peak (1)",
      ifelse(
        centroid == 2, "Winter/Summer peaks (2)",
        ifelse(
          centroid == 3, "Summer break (3)",
          "Winter peak (4)"
        )
      )
    )
  )

df_clust$cluster_name[df_clust$sbd_condition == "Outlier"] <- "Outlier (5)"
df_clust$centroid[df_clust$sbd_condition == "Outlier"] <- 5


desired_order <- c(
  "Summer peak (1)",
  "Winter/Summer peaks (2)",
  "Summer break (3)",
  "Winter peak (4)",
  "Outlier (5)"
)

df_clust$cluster_name <- factor(df_clust$cluster_name, levels = desired_order)


### Portugal characteristics dataset
filename_postcodes <- "data/02-preprocessed/postcodes-preprocessed.csv"
df_postcodes <- read.csv(filename_postcodes)

filename_county_consumption_normalized <- "data/02-preprocessed/county-consumption-normalized-preprocessed.csv"
df_county_consumption_normalized <- read.csv(filename_county_consumption_normalized)

filename_county_industries_normalized <- "data/02-preprocessed/county-industries-normalized-preprocessed.csv"
df_county_industries_normalized <- read.csv(filename_county_industries_normalized)

filename_purchasing_power <- "data/02-preprocessed/county-purchasing-power-preprocessed.csv"
df_county_purchasing_power <- read.csv(filename_purchasing_power)

filename_population <- "data/02-preprocessed/county-population-preprocessed.csv"
df_county_population <- read.csv(filename_population)


df_portugal_info <- df_postcodes %>%
  left_join(df_county_population) %>%
  left_join(df_county_purchasing_power) %>% 
  left_join(df_county_consumption_normalized) %>%
  left_join(df_county_industries_normalized)


# Combine smart electricity meters data with the postcodes dataset
df_electricity <- df_elect %>%
  inner_join(df_portugal_info, by = "postcodes") %>%
  inner_join(df_clust, by = c("postcodes", "month"))

### GeoJSON file - Portugal
geojson_data <- st_read("data/01-input/portugal-with-regions.geojson")


### Load weather data

df_weather <- read.csv("data/02-preprocessed/weather-preprocessed.csv")


### ============================================================================
# PLOT MAPPING OF PORTUGAL POPULATION DENSITY ----
### ============================================================================


data_pop <- world.cities %>% filter(country.etc == "Portugal")

my_breaks <- c(5000, 25000, 50000, 100000, 250000, 500000)

# Define custom labels
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




### ============================================================================
# PLOT CLUSTERS FREQUENCY ----
### ============================================================================

df_clust_grouped <- df_clust %>%
  group_by(postcodes) %>%
  summarise(
    centroid = mean(centroid),
    sbd_condition = unique(sbd_condition),
    cluster_name = unique(cluster_name)
  ) %>%
  ungroup()


ggplot_cluster_freq <- ggplot(
  df_clust_grouped,
  aes(
    x = as.factor(centroid),
    fill = cluster_name
  )
) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  xlab("Cluster group") +
  ylab("Frequency") +
  labs(fill = "Type of pattern:")

ggplot_cluster_freq

ggsave(filename = "figures/cluster-freq.png", plot = ggplot_cluster_freq, width = 12, height = 10, dpi = 300)



### ============================================================================
# PLOT MAPPING TOTAL ENERGY USAGE ----
### ============================================================================


df_total_energy <- df_electricity %>%
  group_by(district) %>%
  summarise(total_energy_kwh = sum(energy_kwh))

my_sf_merged <- geojson_data %>%
  left_join(df_total_energy, by = c("name" = "district"))

plt_total_energy_map <- ggplot(my_sf_merged) +
  geom_sf(aes(fill = (total_energy_kwh / 1000000000))) +
  xlim(-10.5, -6) +
  ylim(36.7, 42) +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Yearly energy \nusage [TW]:"
  ) +
  theme_classic() +
  scale_fill_distiller(palette = "RdBu", direction = -1) +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(colour = "black")
  ) +
  ggrepel::geom_label_repel(
    data = my_sf_merged,
    aes(label = name,
        geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0
  )


plt_total_energy_map

plt_total_energy_district <- my_sf_merged %>%
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




### ============================================================================
# PLOT CORRELATION BETWEEN POWER PURCHASE AND ENERGY USAGE ----
### ============================================================================


my_breaks <- c(170000, 2400000, 4700000, 9400000, 44500000)


df_district_info <- df_portugal_info %>%
  group_by(district) %>%
  summarise(
    population_numbers = sum(population_numbers),
    purchasing_power_per_capita = mean(purchasing_power_per_capita)
  )

population_total <- sum(df_district_info$population_numbers)

df_data <- df_electricity %>%
  group_by(district) %>%
  summarise(total_energy_kwh = sum(energy_kwh)) %>%
  left_join(df_district_info)

ggplot_corr_energy_purchase <- ggplot(
  df_data,
  aes(
    x = log10(purchasing_power_per_capita),
    y = log10(total_energy_kwh / population_total)
  )
) +
  geom_point(
    data = df_data,
    aes(
      fill = total_energy_kwh / 1000000000,
      size = population_numbers
    ),
    shape = 21
  ) +
  geom_smooth(method = "gam", se = FALSE, color = "lightblue4", linewidth = 1.5) +
  geom_label_repel(aes(label = district),
                   size = 3.5,
                   max.overlaps = nrow(df_data)
  ) +
  scale_size_continuous(
    range = c(0.7, 15),
    breaks = my_breaks,
    labels = custom_labels
  ) +
  theme_classic() +
  scale_fill_distiller(palette = "RdBu", direction = -1) +
  theme(
    legend.position = "right",
    legend.background = element_rect(colour = "black"),
    panel.grid.major.x = element_line(color = "grey", linewidth = 0.2), # Adding grid lines only for the y-axis
    panel.grid.major.y = element_line(color = "grey", linewidth = 0.2), # Adding grid lines only for the y-axis
    panel.grid.minor.x = element_line(color = "grey", linewidth = 0.2, linetype = "dashed"),
    panel.grid.minor.y = element_line(color = "grey", linewidth = 0.2, linetype = "dashed"),
  ) +
  labs(
    x = "Total purchasing power per capita (log)",
    y = "Total energy usage in kWh per capita (log)",
    fill = "Yearly energy \nusage [TW]:",
    size = "District \npopulation:"
  )

ggplot_corr_energy_purchase

ggsave(filename = "figures/corr-energy-purchase.png", plot = ggplot_corr_energy_purchase, width = 8, height = 6, dpi = 300)



### ============================================================================
# PLOT MAPPING NON-RESIDENTIAL ENERGY CONSUMPTION ----
### ============================================================================


df_non_residential_energy <- df_portugal_info %>%
  group_by(district) %>%
  summarise(
    mean_domestic = mean(domestic),
    mean_not_domestic = mean(not_domestic),
    mean_agriculture = mean(agriculture),
    mean_industry = mean(industry)
  )

my_sf_merged <- geojson_data %>%
  left_join(df_non_residential_energy, by = c("name" = "district"))

ggplot_domestic_energy_map <- ggplot(my_sf_merged) +
  geom_sf(aes(fill = round(mean_domestic * 100, digits = 1))) +
  xlim(-10.5, -6) +
  ylim(36.7, 42) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Domestic",
    fill = "Percentage [%]:"
  ) +
  theme_void() +
  scale_fill_distiller(palette = "RdBu", direction = -1) +
  theme(
    legend.position = "right"
  )

ggplot_domestic_energy_map

ggplot_not_domestic_energy_map <- ggplot(my_sf_merged) +
  geom_sf(aes(fill = round(mean_not_domestic * 100, digits = 1))) +
  xlim(-10.5, -6) +
  ylim(36.7, 42) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Not domestic",
    fill = "Percentage [%]:"
  ) +
  theme_void() +
  scale_fill_distiller(palette = "RdBu", direction = -1) +
  theme(
    legend.position = "right"
  )


ggplot_industry_energy_map <- ggplot(my_sf_merged) +
  geom_sf(aes(fill = round(mean_industry * 100, digits = 1))) +
  xlim(-10.5, -6) +
  ylim(36.7, 42) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Industry",
    fill = "Percentage [%]:"
  ) +
  theme_void() +
  scale_fill_distiller(palette = "RdBu", direction = -1) +
  theme(
    legend.position = "right"
  )

ggplot_industry_energy_map

ggplot_agriculture_energy_map <- ggplot(my_sf_merged) +
  geom_sf(aes(fill = round(mean_agriculture * 100, digits = 1))) +
  xlim(-10.5, -6) +
  ylim(36.7, 42) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Agriculture",
    fill = "Percentage [%]:"
  ) +
  theme_void() +
  scale_fill_distiller(palette = "RdBu", direction = -1) +
  theme(
    legend.position = "right"
  )

ggplot_agriculture_energy_map

combined_ggplot_shares_energy <- grid.arrange(ggplot_domestic_energy_map,
                                              ggplot_not_domestic_energy_map,
                                              ggplot_industry_energy_map,
                                              ggplot_agriculture_energy_map,
                                              ncol = 2
)
combined_ggplot_shares_energy

ggsave(filename = "figures/combined-shares-energy.png", plot = combined_ggplot_shares_energy, width = 8, height = 6, dpi = 300)





### ------------------------------------------------------------------------
# PLOT STANDARDIZED TIME SERIES (CLUSTERS)
### ------------------------------------------------------------------------
#
# plt_cluster_ts <- ggplot(df_electricity, aes(x = month, y = energy_standardized)) +
#   geom_line(data = df_electricity, aes(group = postcodes), color = 'grey', alpha = 0.5, size = 0.7) +
#   geom_smooth(color = 'blue', size = 2, se = FALSE) +
#   facet_wrap(~cluster_name) +
#   theme_bw() +
#   theme(legend.position="bottom",
#         axis.title.x = element_text(size=14),
#         axis.title.y = element_text(size=14),
#         axis.text.x = element_text(size=12),
#         axis.text.y = element_text(size=12),
#         legend.title = element_text(size=14),
#         legend.text = element_text(size=12),
#         strip.text = element_text(size=14)) +
#   scale_x_continuous(breaks = seq(1, 12, by = 1)) +
#   ylab('Standardized energy [-]') +
#   xlab('Month of the year')
#
# plt_cluster_ts
#
# ggsave(filename = "figures/plt-cluster-ts.png", plot = gg_cluster_ts, width = 8, height = 6, dpi = 300)




### ============================================================================
# PLOT CLUSTERS PER DISTRICT ----
### ============================================================================
#
# Description:
# BLA
# BLA
# BLA



df_cluster_district <- df_electricity %>%
  group_by(district, cluster_name) %>%
  summarise(n_counties = n())


plt_cluster_district <- df_cluster_district %>%
  ggplot(aes(fill = cluster_name, y = n_counties, x = district)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = T) +
  coord_flip() +
  theme_bw() +
  labs(
    x = "District",
    y = "Percentage"
  ) +
  facet_wrap(. ~ cluster_name)

plt_cluster_district



### ------------------------------------------------------------------------
# COMBINE CLUSTERS WITH NORMALIZED COUNTY CONSUMPTION
### ------------------------------------------------------------------------
#
# df_merged_2 <- df_county_consumption_normalized %>% inner_join(df_clust, by = 'postcodes') %>%
#   select(-district, -county, -country, -district_county, -centroid) %>%
#   pivot_longer(
#     cols = !postcodes & !cluster_name,
#     names_to = "elect_type",
#     values_to = "percentage_value"
#   )
#
# ggplot(df_merged_2, aes(x = (percentage_value)*100, y = cluster_name, fill = cluster_name)) +
#   geom_boxplot() +
#   facet_wrap(~elect_type, scales = "free_x") +
#   theme_bw() +
#   theme(legend.position="bottom",
#         axis.title.x = element_text(size=14),
#         axis.title.y = element_text(size=14),
#         axis.text.x = element_text(size=12),
#         axis.text.y = element_text(size=12),
#         legend.title = element_text(size=14),
#         legend.text = element_text(size=12),
#         strip.text = element_text(size=14)) +
#   ylab('Cluster group') +
#   xlab('Percentage [%]')




### ------------------------------------------------------------------------
# COMBINE CLUSTERS WITH NORMALIZED COUNTY INDUSTRY PROFILE
### ------------------------------------------------------------------------
#
# df_merged_3 <- df_county_industries_normalized %>% inner_join(df_clust, by = 'postcodes') %>%
#   select(-district, -county, -country, -district_county, -cluster_group) %>%
#   pivot_longer(
#     cols = !postcodes & !cluster_name,
#     names_to = "elect_type",
#     values_to = "percentage_value"
#   )
#
# ggplot(df_merged_3, aes(x = (percentage_value)*100, y = cluster_name, fill = cluster_name)) +
#   geom_boxplot() +
#   facet_wrap(~elect_type, scales = "free_x") +
#   theme_bw() +
#   theme(legend.position="bottom",
#         axis.title.x = element_text(size=14),
#         axis.title.y = element_text(size=14),
#         axis.text.x = element_text(size=12),
#         axis.text.y = element_text(size=12),
#         legend.title = element_text(size=14),
#         legend.text = element_text(size=12),
#         strip.text = element_text(size=14)) +
#   ylab('Cluster group') +
#   xlab('Percentage [%]')


### ------------------------------------------------------------------------
# CHECK TOP 25 LARGEST ELECTRICITY CONSUMERS AND ITS CLUSTERS
### ------------------------------------------------------------------------

df_electricity_county <- df_electricity %>%
  group_by(district_county) %>%
  summarise(sum_electricity = sum(energy_kwh)) %>%
  arrange(desc(sum_electricity))




### ------------------------------------------------------------------------
# STUFF
### ------------------------------------------------------------------------

ggplot(df_electricity, aes(x = long, y = lat, colour = cluster_name)) +
  geom_jitter()

