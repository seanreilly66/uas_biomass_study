# ==============================================================================
#
# UAS LAS height normalization and registration using ground ICP matrices
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
# Originated with 2019 Pepperwood UAS study. Finalized for 2021 UAS biomass study.
#
# ==============================================================================
#
# Description:
#
# Reads in uas las files that have been classified. Must contain uas_clsfd.las at
# end of name in accordance with project naming convention to be identified.
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
library(tidyverse)
library(glue)
library(doParallel)
library(readxl)

# ================================= User inputs ================================

icp_matrix_folder <- 'data/las/icp_registration/icp_matrices'

uas_las_folder <- 'data/las/uas'

als_dtm_folder <- 'data/dtm/als'

grndpts_output <- 'data/las/icp_registration/registered_grndpts.csv'

las_transform_path <- 'R/las_transformation.R'

# ==============================================================================
# =================== Registration and height normalization ==================== 
# ==============================================================================

# -------------- Setup parallel processing and load matching files ------------- 

uas_files <- list.files(path = uas_las_folder,
                        pattern = 'uas_clsfd.las$',
                        full.names = TRUE)

cl <- makeCluster(13)
registerDoParallel(cl)

grndpts <- foreach (
  uas = uas_files,
  .combine = 'rbind',
  .packages = c('lidR', 'tidyverse', 'glue', 'sf', 'raster', 'readxl')
) %dopar% {
  
  source(las_transform_path)
  
  c <- str_extract(uas, '(?<=c)[:digit:]+')
  z <- str_extract(uas, '(?<=z)[:digit:]+')
  
  als_dtm <- list.files(
    path = als_dtm_folder,
    pattern = glue('c{c}_z{z}_als'),
    full.names = TRUE
  ) %>% 
    raster()
  
  icp_matrix <- list.files(path = icp_matrix_folder,
                           pattern = glue('c{c}'),
                           full.names = TRUE)
  
  # Identify matrix for sites that required canopy alignment at some plots
  if (length(icp_matrix > 1)) {
    for (x in icp_matrix) {
      sheets = excel_sheets(x)
      if (glue('z{z}') %in% sheets) {
        icp_matrix <- x
      }
    }
  }
  
  matrix_type <- str_extract(icp_matrix, '(?<=icpmat_)[:alpha:]+')
  
  icp_matrix <- read_xlsx(
    path = icp_matrix,
    sheet = glue('z{z}'),
    col_names = FALSE,
    range = 'A1:D4'
  ) %>%
    as.matrix()
  
  
  # ------------------------- ICP matrix registration -------------------------- 
  
  las <- lastransformation(las_file = uas,
                           t_matrix = icp_matrix)
  
  writeLAS(las,
           file = str_replace(uas, 'clsfd', glue('reg_{matrix_type}')))
  
  # ---------- Extract ground points and dtm Z for offset confirmation ---------
  
  grnd <- las %>%
    filter_ground() %>%
    merge_spatial(source = als_dtm,
                  attribute = 'dtm_z')
  
  # --------------------- DTM based height normalization ---------------------
  
  las <- normalize_height(las, als_dtm, na.rm = TRUE)
  
  writeLAS(las, 
           file = str_replace(uas, 'clsfd', 'hnrm'))
  
  # ---------------------- Output ground points dataset ----------------------
  
  if (matrix_type == 'grnd') {
    grnd <- grnd@data %>%
      rename(uas_z = Z) %>%
      select(uas_z, dtm_z) %>%
      add_column(zone = z,
                 campaign = c) 
  } else {
    grnd <- data.frame(
      uas_z = NA,
      dtm_z = NA,
      zone = z, 
      campaign = c
    )
  }
  
}

write_csv(grndpts, grndpts_output)

stopCluster(cl)

# ==============================================================================