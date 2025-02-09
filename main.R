### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HEADER ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Author:       Daniel Leiria
# Copyright     Copyright 2025 - Daniel Leiria
# Email:        daniel.h.leiria@gmail.com

# Date:         2025-01-28

# Script name:  main.R

# Script Description:
# Main entry point for the ML project.
# This script runs the full data pipeline:
# - Loads raw data
# - Preprocess data
# - Performs clustering
# - Creates the plots

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clears the console
# Remove all variables of the work space
cat("\014")
rm(list = ls())


# Call project libraries and settings
source("R/00-library.R")
source("R/00-settings.R")
flog.info("Loaded settings.")

flog.info("Preprocessing started.")
source("R/01-data-preprocessing.R")
flog.info("Preprocessing complete.")

flog.info("Clustering started.")
source("R/02-kshape-clustering.R")
flog.info("Clustering complete.")

flog.info("Analysis and plotting started.")
source("R/03-analysis-and-visualization.R")
flog.info("Analysis and plotting completed.")

flog.info("Algorithm complete.")


