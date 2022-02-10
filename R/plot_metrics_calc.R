# ==============================================================================
#
# Plot level point cloud metrics
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 31 Jan 2022
# Last commit:
#
# Status: Needs documentation
#
# Created as part of 2021 UAS biomass study.
#
# ==============================================================================
#
# Description:
#
# Produces a series of summary metrics as defined in point cloud metrics function
# for UAS point clouds that have been height normalized and clipped to plot
# boundaries.
#
# Output is a data frame containing all metrics for each plot
#
# ==============================================================================
#
# User inputs:
#
# ==============================================================================
#
# Package dependencies:
#
# lidR, tidyverse
#
# ==============================================================================
#
# Known problems:
#
# ==============================================================================

library(lidR)
library(tidyverse)
library(doParallel)

# ================================= User inputs ================================

las_folder <- 'data/las/plots'
uas_output <- 'data/las/metrics/uas_plot_metrics.csv'
als_output <- 'data/las/metrics/als_plot_metrics.csv'

# =========================== UAS Point cloud metrics ==========================

uas_files <- list.files(path = las_folder,
                        pattern = '_uas',
                        full.names = TRUE)

cl <- makeCluster(10)
registerDoParallel(cl)

uas_metrics <- foreach (
  uas = uas_files,
  .combine = 'rbind',
  .packages = c('lidR', 'tidyverse', 'raster')
) %dopar% {
  source('R/uas_metric_function.R')
  
  c <- str_extract(uas, '(?<=_c)[:digit:]+')
  p <- str_extract(uas, '(?<=_p)[:digit:]+')
  
  uas_metrics <- readLAS(uas) %>%
    cloud_metrics(
      ~ uas_cld_metrics(
        z = Z,
        r = red,
        g = green,
        b = blue,
        re = re,
        nir = nir,
        ndvi = ndvi,
        ndre = ndre,
        gndvi = gndvi
      )
    ) %>%
    as_tibble() %>%
    add_column(
      campaign = c,
      plot = p,
      method = 'uas',
      .before = 1
    )
  
}

write_csv(uas_metrics, uas_output)

# =========================== als Point cloud metrics ==========================

als_files <- list.files(path = las_folder,
                        pattern = '_als',
                        full.names = TRUE)

cl <- makeCluster(10)
registerDoParallel(cl)

als_metrics <- foreach (
  als = als_files,
  .combine = 'rbind',
  .packages = c('lidR', 'tidyverse', 'raster')
) %dopar% {
  source('R/als_metric_function.R')
  
  c <- str_extract(als, '(?<=_c)[:digit:]+')
  p <- str_extract(als, '(?<=_p)[:digit:]+')
  
  als_metrics <- readLAS(als) %>%
    cloud_metrics(~ als_cld_metrics(z = Z)) %>%
    as_tibble() %>%
    add_column(
      campaign = c,
      plot = p,
      method = 'als',
      .before = 1
    )
  
}

write_csv(als_metrics, als_output)

# ==============================================================================
