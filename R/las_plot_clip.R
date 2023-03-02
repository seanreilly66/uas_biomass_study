# ==============================================================================
#
# LAS plot clipping
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 19 Jan 2022
# Last commit: 
#
# Status: Needs documentation
#
# Part of 2022 UAS biomass study.
#
# ==============================================================================
#
# Description:
#
# Reads in height normalized uas and als las files and clip them to plot extent.
#
# ==============================================================================
#
# User inputs:
#
# ==============================================================================
#
# Package dependencies:
#
# ==============================================================================
#
# Known problems:
#
# ==============================================================================

library(lidR)
library(sf)
library(tidyverse)
library(glue)
library(doParallel)

# ================================= User inputs ================================

geodatabase <- 'data/boundaries/ssu_3dforests.gdb'

plot_layer <- 'field_plots'

merged_table_file <- 'data/boundaries/plot_list.csv'

uas_folder <- 'data/las/uas'

als_folder <- 'data/las/als'

output_folder <- 'data/las/plots'

# ==============================================================================

merged_table <- read_csv(merged_table_file) %>%
  mutate(id = glue('c{campaign}_p{plot}'))

plots <- st_read(geodatabase, plot_layer)

uas_files <- list.files(uas_folder, pattern = 'hnrm', full.names = TRUE)
als_files <- list.files(als_folder, pattern = 'hnrm', full.names = TRUE)

cl <- makeCluster(10)
registerDoParallel(cl)

foreach (
  i = merged_table$id,
  .packages = c('lidR', 'sf', 'tidyverse', 'glue')
) %dopar% {
  
  info <- merged_table %>%
    filter(id == i)
  
  plot_i <- plots %>%
    filter(campaign == info$campaign,
           plot == info$plot)
  
  uas_file_name <- uas_files %>%
    str_subset(glue('c{info$campaign}_z{info$zone}_'))
  
  uas <-  uas_file_name %>%
    readLAS() 
  
  x = uas %>%
    clip_roi(plot_i)
  
  writeLAS(
    las = uas,
    file = uas_file_name %>%
      str_replace(uas_folder, output_folder) %>%
      str_replace('z[:digit:]+', glue('p{info$plot}'))
  )
  
  als_file_name <- als_files %>%
    str_subset(glue('c{info$campaign}_z{info$zone}_'))
  
  als <- als_file_name %>%
    readLAS() %>%
    clip_roi(plot_i)
  
  writeLAS(
    las = als,
    file = als_file_name %>%
      str_replace(als_folder, output_folder) %>%
      str_replace('z[:digit:]+', glue('p{info$plot}'))
  )
  
}



