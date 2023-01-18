# ==============================================================================
#
# UAS plot level point cloud metrics
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
library(caret)

# ================================= User inputs ================================

uas_las_folder <- 'data/las/uas'
output_file <- 'data/temp'

# ============================= Point cloud metrics ============================

uas_files <- list.files(path = uas_las_folder,
                        pattern = '_hnrm',
                        full.names = TRUE) %>%
  str_subset('ppwd')

cl <- makeCluster(3)
registerDoParallel(cl)

foreach (
  uas = uas_files,
  .packages = c('lidR', 'tidyverse', 'terra', 'caret')
) %dopar% {
  
  source('R/uas_metric_function.R')
  mdl <- readRDS('data/ml_output/rf_svm_testing_model_20220323_1124.RData')
  
  c <- str_extract(uas, '(?<=_c)[:digit:]+')
  p <- str_extract(uas, '(?<=_p)[:digit:]+')
  
  uas_metrics <- readLAS(uas, select = '* -5') %>%
    pixel_metrics(
      ~ uas_cld_metrics(
        z = Z,
        r = red,
        g = green,
        b = blue,
        re = re,
        nir = re,
        ndvi = ndvi,
        ndre = ndre,
        gndvi = gndvi
      )
    ) 
  
  x <- uas_metrics %>%
    as.data.frame(xy = TRUE) %>%
    add_column(
      campaign = c,
      plot = p,
      method = 'uas',
      .before = 1
    ) 
  
  x[is.na(x)] = -9999
  
  pred <- predict(mdl$h_mean_rf, x)
  
  out <- x %>%
    select(x, y) %>%
    add_column(pred)
  
  rs <- rast(out)
  
  f_name <- uas %>%
    str_replace(uas_las_folder, output_file) %>%
    str_replace('_uas_hnrm.las', '_hmean_prediction.tif')
  
  writeRaster(rs, f_name)
  
}
  