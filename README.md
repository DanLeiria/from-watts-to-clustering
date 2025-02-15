# Clustering Electricity Usage in Portugal

Welcome to the **Clustering Electricity Usage in Portugal** project! This project focuses on clustering monthly electricity usage data across various regions of Portugal. By leveraging advanced data analytics and clustering techniques, I aim to uncover patterns and insights at the country level that can help in understanding regional electricity usage behaviors, identifying trends, and supporting decision-making for energy distribution and management.

This repository contains a complete workflow for analyzing electricity usage in Portugal. It covers everything from data preprocessing and visualization to the application of clustering algorithms and interpretation of results. Whether you are an energy analyst, data scientist, or simply interested in the dynamics of electricity consumption in Portugal, this project offers a thorough and insightful exploration of regional usage patterns. Additionally, I have attempted to correlate the identified clusters with the Portuguese electricity end-users in each region.

#### Keywords:
Energy distribution management; Clustering algorithms; Customers segmentation; Electricity consumption; Machine learning; Regional consumption patterns.

## Medium article:
The full Medium article can be accessed in here: **LINK**

## Directory tree:
This project has the following directory structure and the next sections attempt to explain them. 

```
.
├── \data
│   ├── 01-input
│   ├── 02-preprocessed
│   └── 03-output-clusters
├── \figures
├── from-watts-to-clustering.Rproj
├── main.log
├── main.R
├── R
│   ├── 00-library.R
│   ├── 00-settings.R
│   ├── 01-data-preprocessing.R
│   ├── 02-kshape-clustering.R
│   ├── 03-analysis-and-visualization.R
│   └── \src
├── README.md
├── \renv
└── renv.lock

```

## Main script: ``main.R``
Start here! This script loads project settings, sources helper functions, and runs the clustering pipeline. Usage: ``source("main.R")``.

The file ``main.log`` is a .txt file with the logs of the algorithm.

## Folder: data
### 01-input
- Monthly electricity data (2023): [E-REDES](https://e-redes.opendatasoft.com/explore/dataset/02-consumos-faturados-por-codigo-postal-ultimos-5-anos/export/?sort=-date&refine.date=2023) (file: ``smart_electricity_meter_portugal_2023.csv``)
- Portuguese postcode data: [Data Science for Social Good Portugal](https://www.dssg.pt/projects/mapeamento-de-codigos-postais-para-localizacoes-em-portugal/) (file: ``cod_post_freg_matched.csv``)
- Electricity end-users per county (2022): [PORDATA](https://www.pordata.pt/municipios/consumidores+de+energia+eletrica+total+e+por+tipo+de+consumo-18) (file: ``elec_energy_by_type_portugal_2022.xlsx``)
- Portuguese purchasing power per county (2021): PORDATA (file: ``purchasing-power-per-capita-portugal-2021.xlxs``)
- Portugal GeoJSON data: [GitHub](https://github.com/nmota/caop_GeoJSON)(files: ``portugal-with-counties.geojson`` and ``portugal-with-regions.geojson``)

### 02-preprocessed

- ``county_consumption_normalized_preprocessed.csv``:
- ``county_consumption_preprocessed.csv``:

- ``county_industries_normalized_preprocessed.csv``:
- ``county_industries_preprocessed.csv``:

- ``electricity_preprocessed.csv``:
- ``postcodes_preprocessed.csv``:

### 03-output-clusters

- ``clustered_electricity.csv``: Electricity dataset with the time series and their clusters groups (1-4) and outliers (5)

## Folder: figures
All figures are created by scripts ``02-kshape-clustering.R`` and ``03-analysis-and-visualization.R``. Figures are saved as PNG files by default.

## Folder: R

- ``00-library.R``: Load packages used in this project.
- ``00-settings.R``: Settings and constants.
- ``01-data-preprocessing.R``: Cleans and normalizes the data.
- ``02-kshape-clustering.R``: Electricity measurements clustering using **K-shape** algorithm.
- ``03-analysis-and-visualization.R``: Plots generation and outputs.

### Folder: utils
Functions created for this project and source by the scripts in R/.
- ``func01-silhouette-score.R``: Calculate and plot the silhouette score for a sequence of clustering numbers. 
- ``func02-kshape.R``: Perform the k-shape clustering.

## Folder: renv
Project dependency management using the **renv** package. Contains:
- renv.lock file listing exact package versions used in the project.
- The local renv library with project-specific installed packages.



