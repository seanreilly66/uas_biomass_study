library(lidR)
library(tidyverse)
library(glue)
library(sf)
library(doParallel)

las_files <- list.files('data/las/als', full.names = TRUE) %>%
  str_subset('\\.las$')

cl <- makeCluster(9)
registerDoParallel(cl)

foreach (
  lf = las_files,
  .packages = c('lidR', 'tidyverse', 'glue', 'sf')
) %dopar% {
  
  dtm <- readLAS(lf, select = 'c') %>%
    filter_ground() %>%
    grid_terrain(res = 0.5, algorithm = tin())
  
  dtm_name <- lf %>%
    str_replace('_als', '_als_dtm') %>%
    str_replace('data/las/als', 'data/dtm/als')
  
  writeRaster(
    x = dtm,
    filename = dtm_name,
    datatype='FLT4S',
    format="GTiff",
    overwrite=TRUE)
  
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
    str_replace('data/las/als', 'data/las/icp_registration')
  
  writeLAS(dtm_las, dtm_las_name)
  
}
  
  
