### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HEADER ----
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Author:       Daniel Leiria
# Copyright     Copyright 2025 - Daniel Leiria
# Email:        daniel.h.leiria@gmail.com

# Date:         2025-01-28

# Script name:  main.R
# R version:    4.4.1
 
# Script Description:
# Main entry point for the ML project.
# This script runs the full data pipeline:
# - Loads raw data
# - Preprocesses data
# - Performes the clustering
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

#source("R/01-data-preprocessing.R")
# source("R/02-kshape-clustering.R")
# source("R/03-analysis-and-visualization.R")


