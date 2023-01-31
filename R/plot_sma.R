# Spectral unmixing

# =================================== Libraries ================================

library(tidyverse)
library(terra)
library(sf)
library(foreach)
library(glue)
library(doParallel)
library(raster)


# ==============================================================================
# ================================= User inputs ================================
# ==============================================================================

em_shp_file <- 'data/temp/endmember/end_member_library.shp'

plot_shp_gdb <- 'data/boundaries/ssu_3dforests.gdb'
plot_shp_layer <- 'field_plots'

composite_folder <- 'data/temp/composite_rasters'

em_library_output <- 'data/temp/endmember/em_library_values.csv'
sma_output <- 'data/las/metrics/sma_plot_metrics.csv'

n_cores <- detectCores() - 5

# ==============================================================================
# ============================== End member library ============================
# ==============================================================================

em_shp <- read_sf(em_shp_file)

c = unique(em_shp$campaign)

band_files <- list.files(path = composite_folder,
                         pattern = glue('.tif$'),
                         full.names = TRUE)

#testing
# i = 1:2
# i_c = c[i]

em_val <- foreach (
  i_c = c,
  .combine = 'rbind'
) %do% {
  
  i_band_files <- str_subset(band_files, pattern = glue('_c{i_c}'))
  
  z <- str_extract(i_band_files, pattern = '(?<=_z)[:digit:]+') %>%
    as.numeric() %>%
    unique()
  
  i_shp <- em_shp %>%
    filter(campaign == i_c)
  
  #testing
  # j = 1:2
  # j_z = z[j]

  # zone loop
  
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  c_em_val <- foreach (
    j_z = z,
    .packages = c('tidyverse', 'terra', 'sf', 'glue'),
    .combine = 'rbind'
  ) %dopar% {
    
    r = i_band_files %>%
      str_subset(pattern = glue('_z{j_z}_')) %>%
      rast()
    
    names(r) <- str_extract(names(r), pattern = '[:alpha:]+$')
    
    cz_em_val <-  extract(r, vect(i_shp)) %>%
      add_column(st_drop_geometry(i_shp),
                 .before = 1) %>%
      drop_na() %>%
      dplyr::select(-ID)
    
    return(cz_em_val)

  }
  
  stopCluster(cl)
  
  return(c_em_val)
  
}

em_val <- em_val %>%
  group_by(campaign, type) %>%
  summarize(
    red = mean(red),
    green = mean(green),
    blue = mean(blue),
    rededge = mean(rededge),
    nir = mean(nir),
    n = n()
  )

write_csv(em_val, em_library_output)

# ==============================================================================
# ===================================== SMA ====================================
# ==============================================================================

plot_shp <- read_sf(plot_shp_gdb, plot_shp_layer)
c = unique(plot_shp$campaign)

#testing
# i = 1:2
# c = c[i]

sma_val <- foreach (
  i_c = c,
  .combine = 'rbind'
) %do% {
  
  i_band_files <- str_subset(band_files, pattern = glue('_c{i_c}'))
  
  z <- str_extract(i_band_files, pattern = '(?<=_z)[:digit:]+') %>%
    as.numeric() %>%
    unique()
  
  i_shp <- plot_shp %>%
    filter(campaign == i_c)
  
  i_em <- em_val %>%
    ungroup() %>%
    filter(campaign == i_c) %>%
    dplyr::select(-campaign, -n) %>%
    as.data.frame()
  
  rownames(i_em) <- i_em$type
  
  i_em <- i_em[-1]

  #testing
  # j = 1:2
  # z = z[j]
  
  # zone loop
  
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  c_sma_val <- foreach (
    j_z = z,
    .packages = c('tidyverse', 'terra', 'sf', 'glue'),
    .combine = 'rbind'
  ) %dopar% {
    
    r = i_band_files %>%
      str_subset(pattern = glue('_z{j_z}_')) %>%
      rast()
    
    names(r) <- str_extract(names(r), pattern = '[:alpha:]+$')
    
    sma <- RStoolbox::mesma(r, i_em) %>%
      rast()
    
    cz_sma_val <-  extract(sma, vect(i_shp), fun = mean) %>%
      add_column(st_drop_geometry(i_shp) %>%
                   dplyr::select(campaign, plot),
                 .before = 1) %>%
      drop_na() %>%
      dplyr::select(-ID)
    
    return(cz_sma_val)
    
  }
  
  write_csv(c_sma_val, glue('data/temp/sma_c{i_c}.csv'))
  
  stopCluster(cl)
  
  return(c_sma_val)
  
}

write_csv(sma_val, sma_output)
