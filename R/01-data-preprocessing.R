### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HEADER ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Author:       Daniel Leiria
# Copyright     Copyright 2025 - Daniel Leiria
# Email:        daniel.h.leiria@gmail.com

# Date:         2025-01-19

# Script name:  01-data-preprocessing
# R version:    4.4.1
 
# Script Description:
# Setup the packages needed for the specific project


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SETUP ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description:
# Clears the console
# Remove all variables of the work space
cat("\014")
rm(list = ls())

# Call project libraries and settings
source("R/00-library.R")
source("R/00-settings.R")


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD DATA ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description: 
# Load the different files with their specifications


### Fetch the electricity data, as csv, from local computer

filename_elec <- "data/01-input/smart-electricity-meter-portugal-2023.csv"
df_original <- read.csv(filename_elec, sep = ";")

# Change the name of the columns
names(df_original)[1] <- "year"
names(df_original)[2] <- "month"
names(df_original)[3] <- "date"
names(df_original)[4] <- "postcodes"
names(df_original)[5] <- "energy_kwh"


### Fetch the postcodes data from local computer

# Read file as csv
# Rename the postcode numbers
# Add another column "country"
filename_post <- "data/01-input/cod-post-freg-matched.csv"
df_postcodes <- read.csv(filename_post, encoding = "UTF-8") %>%
  select(CodigoPostal, Distrito, Concelho) %>%
  rename(
    postcodes = CodigoPostal,
    district = Distrito,
    county = Concelho
  ) %>%
  mutate(
    country = "Portugal"
  )


### Fetch the Portuguese industrial data from local computer

filename_industry <- "data/01-input/industry-type-portugal-2022.xlsx"
df_original_industry <- read_excel(filename_industry, sheet = "industry_type")
# df_original_industry_normalized <- read_excel(filename_industry, sheet = "industry_type_normalized")


### Fetch the Portuguese energy usage by sector from local computer

filename_consumption_type <- "data/01-input/elec-energy-by-type-portugal-2022.xlsx"
df_original_consumption_type <- read_excel(filename_consumption_type, sheet = "consumption_type")
# df_original_consumption_type_normalized <- read_excel(filename_consumption_type, sheet = "consumption_type_normalized")


### Fetch the Portuguese purchasing power per capita from local computer

filename_purchasing_power <- "data/01-input/purchasing-power-per-capita-portugal-2021.xlsx"
df_original_purchasing_power <- read_excel(filename_purchasing_power)


### Fetch the Portuguese population numbers from local computer

filename_population <- "data/01-input/population-number-portugal-2023.xlsx"
df_original_population <- read_excel(filename_population)



### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PREPROCESS POSCODES DATASET ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description: 
# Preprocess the geocodes dataset

# Function to remove the last 3 digits of the postcodes
remove_last_3_digits <- function(number) {
  as.integer(substr(number, 1, nchar(number) - 3))
}

# Apply the function "remove_last_3_digits" to the 'postcodes' column
# Add column of district and county  names
# Filter the dataset to remove the similar rows
df_postcodes <- df_postcodes %>%
  mutate(
    postcodes = sapply(postcodes, remove_last_3_digits),
    district_county = paste0(district, "_", county)) %>%
  distinct()


### These names are repetitive as municipality names

df_postcodes$county[df_postcodes$postcodes == 9560] <- "Lagoa_Azores"

df_postcodes$county[df_postcodes$postcodes == 9370] <- "Calheta_Madeira"
df_postcodes$county[df_postcodes$postcodes == 9374] <- "Calheta_Madeira"
df_postcodes$county[df_postcodes$postcodes == 9385] <- "Calheta_Madeira"

df_postcodes$county[df_postcodes$postcodes == 9850] <- "Calheta_Azores"
df_postcodes$county[df_postcodes$postcodes == 9875] <- "Calheta_Azores"


### Small dataset adjustments before combining with another dataset

df_postcodes$county[df_postcodes$postcodes == 6430] <- "Mêda"
Encoding(df_postcodes$county[df_postcodes$postcodes == 6430]) <- "UTF-8"

df_postcodes$county[df_postcodes$county == "Lagoa "] <- "Lagoa"
df_postcodes$county[df_postcodes$postcodes == 9760] <- "Vila da Praia da Vitória"
Encoding(df_postcodes$county[df_postcodes$postcodes == 9760]) <- "UTF-8"


### Remove duplicate postcodes

df_postcodes$county <- enc2utf8(df_postcodes$county)

df_postcodes_cleaned <- df_postcodes %>%
  filter(
    !(postcodes == 7200 & county == "Alandroal"), # Kept: Reguengos de Monsaraz
    !(postcodes == 7200 & county == "Redondo"),
    !(postcodes == 7200 & county == "Évora"),
    !(postcodes == 2495 & county == "Batalha"), # Kept: Leiria
    !(postcodes == 2495 & county == "Ourém"),
    !(postcodes == 3700 & county == "Oliveira de Azeméis"), # Kept: Santa Maria da Feira
    !(postcodes == 3700 & county == "São João da Madeira"),
    !(postcodes == 4615 & county == "Celorico de Basto"), # Kept: Amarante
    !(postcodes == 4615 & county == "Felgueiras"),
    !(postcodes == 4815 & county == "Vizela"), # Kept: Guimarães
    !(postcodes == 4815 & county == "Felgueiras"),
    !(postcodes == 5040 & county == "Baião"), # Kept: Mesão Frio
    !(postcodes == 5040 & county == "Peso da Régua"),
    !(postcodes == 2100 & county == "Montijo"), # Kept: Coruche
    
    !(postcodes == 2230 & county == "Abrantes"), # Kept: Sardoal
    
    !(postcodes == 2445 & county == "Marinha Grande"), # Kept: Alcobaça
    
    !(postcodes == 2835 & county == "Moita"), # Kept: Barreiro
    
    !(postcodes == 2890 & county == "Alcochete"), # Kept: Benavente
    
    !(postcodes == 2965 & county == "Vendas Novas"), # Kept: Palmela
    
    !(postcodes == 3020 & county == "Mealhada"), # Kept: Coimbra
    
    !(postcodes == 3260 & county == "Alvaiázere"), # Kept: Figueiró dos Vinhos
    
    !(postcodes == 3475 & county == "Oliveira de Frades"), # Kept: Tondela
    
    !(postcodes == 3620 & county == "Sernancelhe"), # Kept: Moimenta da Beira
    
    !(postcodes == 3640 & county == "Trancoso"), # Kept: Sernancelhe
    
    !(postcodes == 3650 & county == "Vila Nova de Paiva"), # Kept: Sátão
    
    !(postcodes == 4500 & county == "Espinho"), # Kept: Santa Maria da Feira
    
    !(postcodes == 4575 & county == "Marco de Canaveses"), # Kept: Penafiel
    
    !(postcodes == 4620 & county == "Lousada"), # Kept: Vizela
    
    !(postcodes == 4650 & county == "Lousada"), # Kept: Felgueiras
    
    !(postcodes == 4740 & county == "Barcelos"), # Kept: Esposende
    
    !(postcodes == 4765 & county == "Guimarães"), # Kept: Vila Nova de Famalicão
    
    !(postcodes == 4775 & county == "Vila Nova de Famalicão"), # Kept: Barcelos
    
    !(postcodes == 4820 & county == "Celorico de Basto"), # Kept: Fafe
    
    !(postcodes == 4905 & county == "Barcelos"), # Kept: Viana do Castelo
    
    !(postcodes == 5085 & county == "Sabrosa"), # Kept: Alijó
    
    !(postcodes == 5350 & county == "Mogadouro"), # Kept: Alfândega da Fé
    
    !(postcodes == 6005 & county == "Fundão"), # Kept: Castelo Branco
    
    !(postcodes == 6185 & county == "Oleiros"), # Kept: Fundão
    
    !(postcodes == 6230 & county == "Fundão"), # Kept: Covilhã
    
    !(postcodes == 6250 & county == "Sabugal"), # Kept: Belmonte
    
    !(postcodes == 6320 & county == "Sabugal"), # Kept: Penamacor
    
    !(postcodes == 7555 & county == "Sines") # Kept: Santiago do Cacém
  )


# Print number per postcodes
print(paste("The total number of rows of postcodes on the Portuguese location dataset is:", nrow(df_postcodes)))
print(paste("The total number of rows of postcodes on the Portuguese location dataset (cleaned) is:", nrow(df_postcodes_cleaned)))


### Check number counties per postcode

df_postcodes_group <- df_postcodes_cleaned %>% # It must be one (1) county per postcode.
  group_by(postcodes) %>%
  summarise(number_cases = n())


### Get geocodes of Portuguese counties

df_postcodes_cleaned <- df_postcodes_cleaned %>%
  tidygeocoder::geocode(
    county = county,
    country = country,
    method = "osm"
  )


# "Mafra" county has slight problem when identified by the API
# Therefore, this county geocodes was added manually
df_postcodes_cleaned$lat[df_postcodes_cleaned$county == "Mafra"] <- 38.9369782
df_postcodes_cleaned$long[df_postcodes_cleaned$county == "Mafra"] <- -9.3282374


# Delete counties that were not identified by the API
# This is observed by NA values in latitude and longitude
df_postcodes_cleaned <- df_postcodes_cleaned %>% filter(!(is.na(lat) | is.na(long)))

print(paste("The total number of rows of postcodes on the Portuguese location dataset (cleaned - final) is:", nrow(df_postcodes_cleaned)))


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PREPROCESS ELECTRICITY DATASET ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description: 
# Cleaning the monthly electricity measurements.
# All counties with measurements lower than 12 months are removed.
# I standardized (z-score) the dataset measurements (to be used in the clustering algorithm).

# Obtain the postcodes with 12 months measurements - lower values should be removed
df_original_group <- df_original %>%
  group_by(postcodes, ) %>%
  summarise(number_months = n()) %>%
  filter(number_months == 12)


print(paste("The total number of rows of postcodes on the Portuguese electricity dataset is:", nrow(df_original_group)))


# Filter out all postcodes with less than 12 months measurements.
df_electricity <- df_original %>%
  filter(postcodes %in% df_original_group$postcodes) %>%
  arrange(postcodes, month)


# Standardize electricity
df_electricity <- df_electricity %>%
  group_by(postcodes) %>%
  mutate(energy_standardized = (energy_kwh - mean(energy_kwh)) / (sd(energy_kwh))) %>%
  ungroup()


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# COMBINE DATASETS WITH THEIR ASSOCIATED LOCATION AND POSTCODE ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description: 
# Combined the different datasets (county characteristics) with the postcodes dataset
# I used the county name as the matching column


df_industry <- df_postcodes_cleaned %>% left_join(df_original_industry, by = "county")
df_industry_normalized <- df_postcodes %>% left_join(df_original_industry_normalized, by = "county")

df_consumption_type <- df_postcodes %>% left_join(df_original_consumption_type, by = "county")
df_consumption_type_normalized <- df_postcodes %>% left_join(df_original_consumption_type_normalized, by = "county")

df_purchasing_power <- df_postcodes %>% left_join(df_original_purchasing_power, by = "county")

df_population <- df_postcodes %>% left_join(df_original_population, by = "county")


### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SAVE DATASETS ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Description: 
# Save preprocessed datasets into .csv files


# Electricity dataset
filename_1 <- "data/02-preprocessed/electricity-preprocessed.csv"
write_csv(df_electricity, filename_1)

# Postcodes dataset
filename_2 <- "data/02-preprocessed/postcodes-preprocessed.csv"
write_csv(df_postcodes_cleaned, filename_2)


### Industries dataset

# Industries by counties dataset
filename_3 <- "data/02-preprocessed/county-industries-preprocessed.csv"
write_csv(df_industry, filename_3)

# Industries by counties (normalized) dataset
# filename_4 <- "data/02-preprocessed/county-industries-normalized-preprocessed.csv"
# write_csv(df_industry_normalized, filename_4)


### Electricity usage by municipalities

# Electricity usage by counties dataset
filename_5 <- "data/02-preprocessed/county-consumption-preprocessed.csv"
write_csv(df_consumption_type, filename_5)

# Electricity usage by counties (normalized) dataset
# filename_6 <- "data/02-preprocessed/county-consumption-normalized-preprocessed.csv"
# write_csv(df_consumption_type_normalized, filename_6)


### Puchasing power per capita

filename_7 <- "data/02-preprocessed/county-purchasing-power-preprocessed.csv"
write_csv(df_purchasing_power, filename_7)


### Portuguese population per county

filename_8 <- "data/02-preprocessed/county-population-preprocessed.csv"
write_csv(df_population, filename_8)



