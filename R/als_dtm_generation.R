# ==============================================================================
#
# LAS Ground Classification
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 10 Aug 2021
# Last commit: 10 Aug 2021
#
# Status: Completed
#
# Created as part of 2021-2022 UAS biomass study.
#
# ==============================================================================
#
# Description:
#
# Generates DTM from ALS las files and converts the DTM to a LAS file format for
# use in ground icp registration of UAS files.
#
# Setup for parallel batch processing of all las files in one parent folder
#
# ==============================================================================
#
# User inputs:
#
# las_folder = Folder containing las files with classified ground points
# raster_output_folder = Location to which raster dtms should be exported
# las_output_folder = Location to which las dtms should be exported
#
# ==============================================================================
#
# Package dependencies:
#
# lidr, tidyverse, glue, sf, doParallel
#
# ==============================================================================
#
# Known problems:
#
# ==============================================================================

library(lidR)
library(tidyverse)
library(glue)
library(sf)
library(doParallel)

# ================================= User inputs ================================

las_folder <- 'data/las/als'

raster_output_folder <- 'data/dtm/als'

las_output_folder <- 'data/las/icp_registration/als_dtm_las'


# ==============================================================================
# ============================ Generate DTM formats ============================ 
# ==============================================================================

las_files <- list.files(las_folder, full.names = TRUE) %>%
  str_subset('\\.las$') %>%
  str_subset('c1')

cl <- makeCluster(11)
registerDoParallel(cl)

foreach (
  lf = las_files,
  .packages = c('lidR', 'tidyverse', 'glue', 'sf')
) %dopar% {
  
  # --------------------------- Generate raster DTM ---------------------------- 
  
  dtm <- readLAS(lf, select = 'c') %>%
    filter_ground() %>%
    grid_terrain(res = 0.5, algorithm = tin())
  
  dtm_name <- lf %>%
    str_replace('_als', '_als_dtm') %>%
    str_replace('data/las/als', raster_output_folder)
  
  writeRaster(
    x = dtm,
    filename = dtm_name,
    datatype='FLT4S',
    format="GTiff",
    overwrite=TRUE)
  
  # ----------------------------- Generate LAS DTM -----------------------------
  
  dtm_las <- dtm %>%
    as.data.frame(xy = TRUE) %>%
    filter(!is.na(Z),
           Z > -100) %>%
    rename(
      X = x, 
      Y = y
    ) %>%
    LAS()
  
  projection(dtm_las) <- crs(dtm)
  
  dtm_las_name <- lf %>%
    str_replace('_als', '_als_dtm') %>%
    str_replace('data/las/als', las_output_folder)
  
  writeLAS(dtm_las, dtm_las_name)
  
}
  
stopCluster(cl)

# ==============================================================================