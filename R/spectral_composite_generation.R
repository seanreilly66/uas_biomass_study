# Composite spectral raster generation

# =================================== Libraries ================================

library(terra)
library(tidyverse)
library(glue)
library(foreach)
library(doParallel)

# ============================= Composite generation ===========================

spec_file <-
  list.files('data/spectral', pattern = '.tif$', full.names = TRUE)

cz <- str_extract(spec_file, pattern = 'c[:digit:]+_z[:digit:]+') %>%
  unique()

n_cores <- detectCores() - 10

cl <- makeCluster(n_cores)
registerDoParallel(cl)

foreach (
  i_cz = cz,
  .packages = c('tidyverse', 'terra', 'glue')
) %dopar% {
  
  i_files <- spec_file %>%
    str_subset(pattern = glue('{i_cz}_'))
  
  output_name <-
    str_replace(i_files[1], glue('(?<={i_cz}).+'), '_composite.tif') %>%
    str_replace('spectral', 'temp/composite_rasters')
  
  i_files <- c(
    str_subset(i_files, 'red.tif'),
    str_subset(i_files, 'green'),
    str_subset(i_files, 'blue'),
    str_subset(i_files, 'rededge'),
    str_subset(i_files, 'nir')
  )
  
  r <- rast(i_files)

  writeRaster(r, filename = output_name, overwrite = TRUE)

}

stopCluster()

# ==============================================================================