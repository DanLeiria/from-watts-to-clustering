# Clustering Electricity Usage in Portugal - 🚧 IN PROGRESS

Welcome to the **Clustering Electricity Usage in Portugal** project! This project focuses on clustering monthly electricity usage data across various regions of Portugal. By leveraging advanced data analytics and clustering techniques, I aim to uncover patterns and insights at the country level that can help in understanding regional electricity usage behaviors, identifying trends, and supporting decision-making for energy distribution and management.

This repository contains a complete workflow for analyzing electricity usage in Portugal. It covers everything from data preprocessing and visualization to the application of clustering algorithms and interpretation of results. Whether you are an energy analyst, data scientist, or simply interested in the dynamics of electricity consumption in Portugal, this project offers a thorough and insightful exploration of regional usage patterns. Additionally, I have attempted to correlate the identified clusters with the Portuguese industrial sectors and the electricity end-users in each region.

#### Keywords:
Energy distribution management; Clustering algorithms; Customers segmentation; Electricity consumption; Machine learning; Regional consumption patterns

## Input data:
#### In the folder: _input_data_
- Monthly electricity data (2023): [E-REDES](https://e-redes.opendatasoft.com/explore/dataset/02-consumos-faturados-por-codigo-postal-ultimos-5-anos/export/?sort=-date&refine.date=2023) (file: _smart_electricity_meter_portugal_2023.csv_)
- Portuguese postcode data: [Data Science for Social Good Portugal](https://www.dssg.pt/projects/mapeamento-de-codigos-postais-para-localizacoes-em-portugal/) (file: _cod_post_freg_matched.csv_)
- Industry sectors per region (2022): [PORDATA](https://www.pordata.pt/municipios/empresas+nao+financeiras+total+e+por+setor+de+atividade+economica-346) (file: _industry_type_portugal_2022.xlsx_)
- Electricity end-users per region (2022): [PORDATA](https://www.pordata.pt/municipios/consumidores+de+energia+eletrica+total+e+por+tipo+de+consumo-18) (file: _elec_energy_by_type_portugal_2022.xlsx_)

Weather data of 2023:
- Mean air temperature in $\degree\text{C} (altitude 2 meters)$: https://power.larc.nasa.gov/data-access-viewer/

## Methodology:
FIGURE XXX

### Scripts list:
#### In the folder: _scripts_
- Data preprocessing in _R_ (file: _data_preprocessing_electricity_portugal.R_)
- Electricity measurements clustering using **K-shape** algorithm in _Python_ (file: _k_shape_clustering_electricity.py_)
- Portuguese counties (municipalities) characteristics segmentation using **Self-Organizing Maps (SOM)** in _R_ (file: _county_profile_som_clustering.R_)
- Data visualization in:
  - _R_ (file: _data_analysis_clustering_results.R_) - Plots generation and analysis
  - _PowerBI_ (file: _xxx_) - Results' summary and maps

## Output data:
#### In the folder: _output_preprocessed_
- _county_consumption_normalized_preprocessed.csv:_
- _county_consumption_preprocessed.csv:_

- _county_industries_normalized_preprocessed.csv:_
- _county_industries_preprocessed.csv:_

- _electricity_preprocessed.csv:_
- _postcodes_preprocessed.csv:_

#### In the folder: _output_clustering_
- _clustered_electricity.csv:_ Electricity dataset with the time series and their clusters groups (1-5) and outliers (0)

## References:
1. Ece Calikus, Sławomir Nowaczyk, Anita Sant'Anna, Henrik Gadd, Sven Werner, "A data-driven approach for discovering heat load patterns in district heating", _Applied Energy_, Volume 252, 2019, 113409, https://doi.org/10.1016/j.apenergy.2019.113409
2. J. Paparrizos and L. Gravano, “k-Shape,” ACM SIGMOD Record, vol. 45, no. 1, pp. 69–76, Jun. 2016, doi: https://doi.org/10.1145/2949741.2949758
