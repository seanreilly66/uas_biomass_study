# Spectral raster quality control

# =================================== Libraries ================================

library(terra)
library(tidyverse)
library(glue)
library(foreach)
library(doParallel)

# ============================= Composite generation ===========================

spec_file <-
  list.files('data/temp/composite_rasters', pattern = '.tif$', full.names = TRUE)

n_cores <- 5

cl <- makeCluster(n_cores)
registerDoParallel(cl)

spec_range <- foreach (
  i_file = spec_file,
  .combine = 'rbind',
  .packages = c('tidyverse', 'terra', 'glue')
) %dopar% {
  
  r <- rast(i_file) %>%
    as_tibble() %>%
    rename_with(~ str_extract(., '[:lower:]+$')) %>%
    summarise(
      red_min = min(red, na.rm = TRUE),
      red_max = max(red, na.rm = TRUE),
      green_min = min(green, na.rm = TRUE),
      green_max = max(green, na.rm = TRUE),
      blue_min = min(blue, na.rm = TRUE),
      blue_max = max(blue, na.rm = TRUE),
      rededge_min = min(rededge, na.rm = TRUE),
      rededge_max = max(rededge, na.rm = TRUE),
      nir_min = min(nir, na.rm = TRUE),
      nir_max = max(nir, na.rm = TRUE)
    ) %>%
    add_column(c = str_extract(i_file, pattern = '(?<=_c)[:digit:]+'),
               z = str_extract(i_file, pattern = '(?<=_z)[:digit:]+'),
               .before = 1)
  
}

stopCluster(cl)

write_csv(spec_range, 'data/temp/spec_qc.csv')

# ==============================================================================